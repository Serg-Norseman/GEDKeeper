/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;

namespace GDModel
{
    public enum GDMRestriction
    {
        rnNone,
        rnLocked,
        rnConfidential,
        rnPrivacy,

        rnLast = rnPrivacy
    }


    public abstract class GDMRecordWithEvents : GDMRecord, IGDMRecordWithEvents
    {
        private GDMList<GDMCustomEvent> fEvents;
        private GDMRestriction fRestriction;


        public bool HasEvents
        {
            get { return fEvents != null && fEvents.Count != 0; }
        }

        public GDMList<GDMCustomEvent> Events
        {
            get {
                if (fEvents == null) {
                    fEvents = new GDMList<GDMCustomEvent>();
                }

                return fEvents;
            }
        }

        public GDMRestriction Restriction
        {
            get { return fRestriction; }
            set { fRestriction = value; }
        }


        protected GDMRecordWithEvents(GDMTree tree) : base(tree)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fEvents != null) fEvents.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fEvents != null) fEvents.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            if (fEvents != null) fEvents.Clear();
            fRestriction = GDMRestriction.rnNone;
        }

        public override bool IsEmpty()
        {
            // Restrictions are not checked because they are not important if other fields are empty.
            return base.IsEmpty() && (fEvents == null || fEvents.Count == 0);
        }

        public override void Assign(GDMTag source)
        {
            GDMRecordWithEvents sourceRec = source as GDMRecordWithEvents;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            if (sourceRec.fEvents != null) {
                for (int i = 0, count = sourceRec.fEvents.Count; i < count; i++) {
                    GDMCustomEvent sourceEvent = sourceRec.fEvents[i];
                    GDMCustomEvent copy = (GDMCustomEvent)Activator.CreateInstance(sourceEvent.GetType());
                    copy.Assign(sourceEvent);
                    AddEvent(copy);
                }
            }

            fRestriction = sourceRec.Restriction;
        }

        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMRecordWithEvents target = targetRecord as GDMRecordWithEvents;
            if (target == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord);

            while (fEvents != null && fEvents.Count > 0) {
                GDMCustomEvent obj = fEvents.Extract(0);
                target.AddEvent(obj);
            }

            target.Restriction = fRestriction;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (fEvents != null) fEvents.ReplaceXRefs(map);
        }

        public GDMCustomEvent FindEvent(string eventName)
        {
            GDMCustomEvent result = null;
            if (fEvents == null) return result;

            int num = fEvents.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = fEvents[i];

                if (evt.GetTagName() == eventName) {
                    result = evt;
                    break;
                }
            }

            return result;
        }

        public GDMCustomEvent FindEvent(GEDCOMTagType eventType)
        {
            GDMCustomEvent result = null;
            if (fEvents == null) return result;

            int evtType = (int)eventType;
            int num = fEvents.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = fEvents[i];

                if (evt.Id == evtType) {
                    result = evt;
                    break;
                }
            }

            return result;
        }

        public abstract GDMCustomEvent AddEvent(GDMCustomEvent evt);

        public UDN GetUDN(string eventSign)
        {
            GDMCustomEvent evt = FindEvent(eventSign);
            return (evt == null) ? UDN.Unknown : evt.Date.GetUDN();
        }

        public UDN GetUDN(GEDCOMTagType eventType)
        {
            GDMCustomEvent evt = FindEvent(eventType);
            return (evt == null) ? UDN.Unknown : evt.Date.GetUDN();
        }

        /// <summary>
        /// In the historical chronology of the year 0 does not exist.
        /// Therefore, the digit 0 in the year value can be used as a sign of lack or error.
        /// ChronologicalYear - introduced for the purposes of uniform chronology years in the Gregorian calendar.
        /// Is estimated from -4714 BC to 3268 AD.
        /// </summary>
        /// <returns>chronological year</returns>
        public int GetChronologicalYear(string eventSign)
        {
            GDMCustomEvent evt = FindEvent(eventSign);
            return (evt == null) ? 0 : evt.GetChronologicalYear();
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            ProcessHashes(ref hashCode, fEvents);
            hashCode.Add(fRestriction);
        }
    }
}
