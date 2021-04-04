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
using GKCore.Types;

namespace GDModel
{
    public abstract class GDMCustomEvent : GDMValueTag, IGDMStructWithLists
    {
        private GDMAddress fAddress;
        private string fAgency;
        private string fCause;
        private string fClassification;
        private GDMDateValue fDate;
        private GDMPlace fPlace;
        private string fReligiousAffilation;
        private GDMRestriction fRestriction;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;
        private GDMList<GDMMultimediaLink> fMultimediaLinks;


        public GDMAddress Address
        {
            get { return fAddress; }
        }

        public string Agency
        {
            get { return fAgency; }
            set { fAgency = value; }
        }

        public string Cause
        {
            get { return fCause; }
            set { fCause = value; }
        }

        public string Classification
        {
            get { return fClassification; }
            set { fClassification = value; }
        }

        public GDMDateValue Date
        {
            get { return fDate; }
        }

        public GDMPlace Place
        {
            get { return fPlace; }
        }

        public string ReligiousAffilation
        {
            get { return fReligiousAffilation; }
            set { fReligiousAffilation = value; }
        }

        public GDMRestriction Restriction
        {
            get { return fRestriction; }
            set { fRestriction = value; }
        }

        public GDMList<GDMNotes> Notes
        {
            get { return fNotes; }
        }

        public GDMList<GDMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }

        public GDMList<GDMMultimediaLink> MultimediaLinks
        {
            get { return fMultimediaLinks; }
        }


        protected GDMCustomEvent()
        {
            fAddress = new GDMAddress();
            fDate = new GDMDateValue();
            fPlace = new GDMPlace();
            fNotes = new GDMList<GDMNotes>();
            fSourceCitations = new GDMList<GDMSourceCitation>();
            fMultimediaLinks = new GDMList<GDMMultimediaLink>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fNotes.Dispose();
                fSourceCitations.Dispose();
                fMultimediaLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fAddress.TrimExcess();
            fDate.TrimExcess();
            fPlace.TrimExcess();
            fNotes.TrimExcess();
            fSourceCitations.TrimExcess();
            fMultimediaLinks.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMCustomEvent sourceObj = (source as GDMCustomEvent);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fAddress.Assign(sourceObj.fAddress);
            fAgency = sourceObj.fAgency;
            fCause = sourceObj.fCause;
            fClassification = sourceObj.fClassification;
            fDate.Assign(sourceObj.fDate);
            fPlace.Assign(sourceObj.fPlace);
            fReligiousAffilation = sourceObj.fReligiousAffilation;
            fRestriction = sourceObj.fRestriction;
            AssignList(sourceObj.Notes, fNotes);
            AssignList(sourceObj.SourceCitations, fSourceCitations);
            AssignList(sourceObj.MultimediaLinks, fMultimediaLinks);
        }

        public override void Clear()
        {
            base.Clear();

            fAddress.Clear();
            fAgency = string.Empty;
            fCause = string.Empty;
            fClassification = string.Empty;
            fDate.Clear();
            fPlace.Clear();
            fReligiousAffilation = string.Empty;
            fRestriction = GDMRestriction.rnNone;
            fNotes.Clear();
            fSourceCitations.Clear();
            fMultimediaLinks.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fAddress.IsEmpty() && string.IsNullOrEmpty(fAgency) && string.IsNullOrEmpty(fCause)
                && string.IsNullOrEmpty(fClassification) && fDate.IsEmpty() && fPlace.IsEmpty()
                && string.IsNullOrEmpty(fReligiousAffilation) && (fRestriction == GDMRestriction.rnNone)
                && (fNotes.Count == 0) && (fSourceCitations.Count == 0) && (fMultimediaLinks.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fAddress.ReplaceXRefs(map);
            fDate.ReplaceXRefs(map);
            fPlace.ReplaceXRefs(map);
            fNotes.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
            fMultimediaLinks.ReplaceXRefs(map);
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            if (tag == null) return 0.0f;
            GDMCustomEvent ev = (GDMCustomEvent)tag;

            // match date
            float dateMatch = 0.0f;
            float locMatch = 0.0f;
            int matches = 0;

            GDMDateValue dtVal = this.Date;
            GDMDateValue dtVal2 = ev.Date;

            matches += 1;
            if (dtVal != null && dtVal2 != null) {
                dateMatch = dtVal.IsMatch(dtVal2, matchParams);
            }

            // match location - late code-on by option implementation
            if (matchParams.CheckEventPlaces) {
                matches += 1;

                if (this.Place == null && ev.Place == null) {
                    locMatch = 100.0f;
                } else if (this.Place != null && ev.Place != null && this.Place.StringValue == ev.Place.StringValue) {
                    locMatch = 100.0f;
                }
            }

            float match = (dateMatch + locMatch) / matches;
            return match;
        }

        public UDN GetUDN()
        {
            return Date.GetUDN();
        }

        /// <summary>
        /// In the historical chronology of the year 0 does not exist.
        /// Therefore, the digit 0 in the year value can be used as a sign of lack or error.
        /// ChronologicalYear - introduced for the purposes of uniform chronology years in the Gregorian calendar.
        /// Is estimated from -4714 BC to 3268 AD.
        /// </summary>
        /// <returns>chronological year</returns>
        public int GetChronologicalYear()
        {
            return Date.GetChronologicalYear();
        }

        public void SetName(string tagName)
        {
            int tagId = GEDCOMTagsTable.Lookup(tagName);
            SetName(tagId);
        }
    }
}
