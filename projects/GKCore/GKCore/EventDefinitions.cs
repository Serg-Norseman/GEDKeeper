/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.IO;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GKCore
{
    /// <summary>
    /// Event Definition.
    /// </summary>
    public class EventDef : IGDMObject
    {
        public string DisplayName { get; set; }

        public string Tag { get; set; }

        public string Type { get; set; }

        public bool Enabled { get; set; }

        public string Description { get; set; }


        internal EventTarget Target { get; set; }

        internal EventKind Kind { get; set; }

        internal bool Protected { get; set; }

        internal bool AcceptableEmpty { get; set; }


        public EventDef()
        {
        }

        internal EventDef(PredefinedEvent predefEvent)
        {
            string tagOrigin = predefEvent.Tag.StartsWith("_") ? "GEDKeeper" : "GEDCOM";
            string typeOrigin = predefEvent.Kind == EventKind.ekEvent ? "event" : "fact";
            string desc = string.Format("predefined {0} {1} tag", tagOrigin, typeOrigin);

            DisplayName = LangMan.LS(predefEvent.Name);
            Description = desc;
            Tag = predefEvent.Tag;
            Type = "";
            Enabled = true;
            Target = predefEvent.Target;
            Kind = predefEvent.Kind;
            Protected = true;
            AcceptableEmpty = predefEvent.AcceptableEmpty;
        }

        public EventDef(string displayName, string description, string tag, string type, bool enabled, EventTarget target)
        {
            DisplayName = displayName;
            Description = description;
            Tag = tag;
            Type = type;
            Enabled = enabled;
            Target = target;
            Kind = (tag == GEDCOMTagName.EVEN) ? EventKind.ekEvent : EventKind.ekFact;
            Protected = false;
            AcceptableEmpty = false;
        }
    }


    public class EventDefList : List<EventDef>, IGDMList<EventDef>
    {
        public void Dispose()
        {
        }

        public new EventDef Add(EventDef item)
        {
            base.Add(item);
            return item;
        }

        public new void Remove(EventDef item)
        {
            base.Remove(item);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class EventDefinitions : IGDMObject
    {
        private EventDefList fEventDefs;
        private Dictionary<string, EventDef> fIndex;

        internal EventDefList List
        {
            get { return fEventDefs; }
        }

        public EventDefinitions()
        {
            fEventDefs = new EventDefList();
            fIndex = new Dictionary<string, EventDef>();
        }

        private void Add(EventDef eventDef)
        {
            fEventDefs.Add(eventDef);

            string key = eventDef.Tag + ":" + eventDef.Type;
            fIndex.Add(key, eventDef);
        }

        public EventDef Find(string tag, string type)
        {
            string key = tag + ":" + type;
            EventDef result;
            return fIndex.TryGetValue(key, out result) ? result : null;
        }

        public EventDef Find(GDMCustomEvent evt)
        {
            if (evt == null)
                return null;

            string tag = evt.GetTagName();
            string type = evt.Classification;
            return Find(tag, type);
        }

        public void Collect(GDMCustomEvent evt)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            try {
                string tag = evt.GetTagName();
                string type = evt.Classification;

                if (!string.IsNullOrEmpty(type)) {
                    var eventDef = Find(tag, type);
                    if (eventDef == null) {
                        EventTarget target = (evt is GDMFamilyEvent) ? EventTarget.etFamily : EventTarget.etIndividual;
                        eventDef = new EventDef(type, "custom event/fact tag", tag, type, true, target);
                        Add(eventDef);
                    } else {
                        eventDef.Enabled = true;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("EventDefinitions.Collect()", ex);
            }
        }

        public void InitPredefined()
        {
            // clear all data
            fEventDefs.Clear();
            fIndex.Clear();

            // load predefined event types (constant properties)
            var predefList = GKData.PredefinedEvents;
            for (int i = 0; i < predefList.Length; i++) {
                var predefEvent = predefList[i];

                var eventDef = new EventDef(predefEvent);
                Add(eventDef);
            }
        }

        public void Load(string fileName)
        {
            InitPredefined();

            if (!File.Exists(fileName)) return;

            // load variable customizable event types
            try {
                EventDef[] loadedDefs;
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    loadedDefs = YamlHelper.Deserialize<EventDef[]>(content);
                }

                if (loadedDefs != null && loadedDefs.Length > 0) {
                    for (int i = 0; i < loadedDefs.Length; i++) {
                        var loadedDef = loadedDefs[i];

                        var eventDef = Find(loadedDef.Tag, loadedDef.Type);
                        if (eventDef != null) {
                            eventDef.Enabled = loadedDef.Enabled;
                            eventDef.DisplayName = loadedDef.DisplayName;
                            // other properties don't change
                        } else {
                            eventDef = new EventDef(loadedDef.DisplayName, loadedDef.Description, loadedDef.Tag, loadedDef.Type, loadedDef.Enabled, EventTarget.etIndividual);
                            Add(eventDef);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("EventDefinitions.Load()", ex);
            }
        }

        public void Save(string fileName)
        {
            try {
                using (var writer = new StreamWriter(fileName)) {
                    string content = YamlHelper.Serialize(fEventDefs);
                    writer.Write(content);
                    writer.Flush();
                }
            } catch (Exception ex) {
                Logger.WriteError("EventDefinitions.Save()", ex);
            }
        }

        /*private static int DefsCompare(EventDef e1, EventDef e2)
        {
            return e1.DisplayName.CompareTo(e2.DisplayName);
        }

        public void Sort()
        {
            fEventDefs.Sort(DefsCompare);
        }*/
    }
}
