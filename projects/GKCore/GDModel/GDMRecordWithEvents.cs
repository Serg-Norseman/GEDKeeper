/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using BSLib.Calendar;
using GDModel.Providers.GEDCOM;

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


        public GDMList<GDMCustomEvent> Events
        {
            get { return fEvents; }
        }

        public GDMRestriction Restriction
        {
            get { return fRestriction; }
            set { fRestriction = value; }
        }


        protected GDMRecordWithEvents(GDMTree tree) : base(tree)
        {
            fEvents = new GDMList<GDMCustomEvent>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fEvents.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fEvents.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            fEvents.Clear();
            fRestriction = GDMRestriction.rnNone;
        }

        public override bool IsEmpty()
        {
            // Restrictions are not checked because they are not important if other fields are empty.
            return base.IsEmpty() && (fEvents.Count == 0);
        }

        public override void Assign(GDMTag source)
        {
            GDMRecordWithEvents sourceRec = source as GDMRecordWithEvents;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            for (int i = 0, count = sourceRec.fEvents.Count; i < count; i++) {
                GDMCustomEvent sourceEvent = sourceRec.fEvents[i];
                GDMCustomEvent copy = (GDMCustomEvent)Activator.CreateInstance(sourceEvent.GetType(), new object[] { this });
                copy.Assign(sourceEvent);
                AddEvent(copy);
            }

            fRestriction = sourceRec.Restriction;
        }

        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMRecordWithEvents target = targetRecord as GDMRecordWithEvents;
            if (target == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord);

            while (fEvents.Count > 0) {
                GDMCustomEvent obj = fEvents.Extract(0);
                target.AddEvent(obj);
            }

            target.Restriction = fRestriction;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fEvents.ReplaceXRefs(map);
        }

        public GDMCustomEvent FindEvent(string eventName)
        {
            GDMCustomEvent result = null;

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

        private static readonly float[] CA_VALUES = new float[] { 0.25f, 0.5f, 0.75f, 1.0f };

        public float GetCertaintyAssessment()
        {
            float result = 0;
            float wsum = 0;

            int num1 = fEvents.Count;
            for (int i = 0; i < num1; i++) {
                GDMCustomEvent evt = fEvents[i];

                int num2 = evt.SourceCitations.Count;
                for (int k = 0; k < num2; k++) {
                    GDMSourceCitation cit = evt.SourceCitations[k];

                    int ca = cit.GetValidCertaintyAssessment();
                    int weight = (ca + 1);

                    result += (CA_VALUES[ca] * weight);
                    wsum += weight;
                }
            }

            int num3 = SourceCitations.Count;
            for (int i = 0; i < num3; i++) {
                GDMSourceCitation cit = SourceCitations[i];

                int ca = cit.GetValidCertaintyAssessment();
                int weight = (ca + 1);

                result += (CA_VALUES[ca] * weight);
                wsum += weight;
            }

            if (wsum != 0.0f) {
                result /= wsum;
            } else {
                result = 0.0f;
            }

            return result;
        }

        public UDN GetUDN(string eventSign)
        {
            GDMCustomEvent evt = FindEvent(eventSign);
            return (evt == null) ? UDN.CreateEmpty() : evt.GetUDN();
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
    }
}
