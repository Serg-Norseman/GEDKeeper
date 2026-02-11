/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Locales;

namespace GKCore.Events
{
    internal class EventUtils
    {
        public static bool CreateCalculatedBirthEvent(BaseContext baseContext, GDMIndividualRecord indiRec, GDMIndividualEventDetail evt)
        {
            if (indiRec != null && evt != null && evt.Date.Value is GDMDate date) {
                if (!evt.HasAge) {
                    AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.NoAgeOnEventDate));
                    return false;
                }

                var newDate = GDMDate.Subtract(date, evt.Age);

                if (!newDate.IsEmpty()) {
                    var newEvent = GKUtils.CreateIndividualEvent(indiRec, newDate, GEDCOMTagName.BIRT, true);
                    newEvent.AssignDerivative(evt);
                    return true;
                }
            }

            return false;
        }

        private static void ExtractEvents(IGDMRecordWithEvents record, GEDCOMTagType eventType, IList<SpecEvent> list, bool first, bool parent)
        {
            if (!record.HasEvents) return;

            var subList = new List<GDMCustomDate>();

            int evtType = (int)eventType;
            int num = record.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = record.Events[i];
                if (((!parent && evt.Id == evtType) || (parent && evt.Id != evtType)) && evt.GetChronologicalYear() != 0) {
                    subList.Add(evt.Date.Value);
                }
            }

            if (subList.Count > 0) {
                subList.Sort();

                // From the children's records we extract only one (first or last) births,
                // from the parent's record - the first and last significant event
                if (parent) {
                    list.Add(new SpecEvent(subList[0], SEDefinition.LifeEvent));
                    list.Add(new SpecEvent(subList[subList.Count - 1], SEDefinition.LifeEvent));
                } else {
                    var dtx = first ? subList[0] : subList[subList.Count - 1];
                    list.Add(new SpecEvent(dtx, SEDefinition.ChildBirth));
                }
            }
        }

        private static List<SpecEvent> GetIndiEvents(GDMTree tree, GDMIndividualRecord indiRec, GEDCOMTagType targetEventType)
        {
            var children = new List<GDMIndividualRecord>();
            for (int j = 0, jNum = indiRec.SpouseToFamilyLinks.Count; j < jNum; j++) {
                GDMFamilyRecord famRec = tree.GetPtrValue(indiRec.SpouseToFamilyLinks[j]);
                for (int i = 0, iNum = famRec.Children.Count; i < iNum; i++) {
                    GDMIndividualRecord child = tree.GetPtrValue(famRec.Children[i]);
                    children.Add(child);
                }
            }

            var events = new List<SpecEvent>();

            for (int i = 0, iNum = children.Count; i < iNum; i++) {
                var child = children[i];
                bool last = (i != 0 && i == iNum - 1);
                ExtractEvents(child, GEDCOMTagType.BIRT, events, !last, false);
            }

            ExtractEvents(indiRec, targetEventType, events, false, true);

            events.Sort((a, b) => a.Date.CompareTo(b.Date));

            return events;
        }

        private enum SEDefinition { LifeEvent, ChildBirth }

        private class SpecEvent
        {
            public GDMCustomDate Date;
            public SEDefinition Definition;

            public SpecEvent(GDMCustomDate date, SEDefinition definition)
            {
                Date = date;
                Definition = definition;
            }
        }

        public static bool CreateEstimatedBirthEvent(BaseContext baseContext, GDMIndividualRecord indiRec)
        {
            var events = GetIndiEvents(baseContext.Tree, indiRec, GEDCOMTagType.BIRT);
            if (events.Count < 1) return false;

            var firstEvt = events[0];

            GDMCustomDate newDate = null;
            if (firstEvt.Definition == SEDefinition.ChildBirth) {
                var age = new GDMAge();
                age.ParseString($"{GKData.MIN_PARENT_AGE}y");
                newDate = GDMDate.Subtract(firstEvt.Date as GDMDate, age);
            } else {
                newDate = firstEvt.Date;
            }

            if (newDate != null) {
                newDate = GDMCustomDate.CreateRange(null, newDate as GDMDate); // before
                var newEvent = GKUtils.CreateIndividualEvent(indiRec, newDate, GEDCOMTagName.BIRT);
                newEvent.Agency = "Estimated using some other event dates";
                return true;
            }

            return false;
        }

        public static bool CreateEstimatedDeathEvent(BaseContext baseContext, GDMIndividualRecord indiRec)
        {
            var events = GetIndiEvents(baseContext.Tree, indiRec, GEDCOMTagType.DEAT);
            if (events.Count < 1) return false;

            var lastEvt = events[events.Count - 1];

            GDMCustomDate newDate = null;
            if (indiRec.Sex == GDMSex.svMale) {
                if (lastEvt.Definition == SEDefinition.ChildBirth) {
                    var age = new GDMAge();
                    age.ParseString("9m");
                    newDate = GDMDate.Subtract(lastEvt.Date as GDMDate, age);
                } else {
                    newDate = lastEvt.Date;
                }
            } else if (indiRec.Sex == GDMSex.svFemale) {
                newDate = lastEvt.Date;
            }

            if (newDate != null) {
                newDate = GDMCustomDate.CreateRange(newDate as GDMDate, null); // after
                var newEvent = GKUtils.CreateIndividualEvent(indiRec, newDate, GEDCOMTagName.DEAT);
                newEvent.Agency = "Estimated using some other event dates";
                return true;
            }

            return false;
        }
    }
}
