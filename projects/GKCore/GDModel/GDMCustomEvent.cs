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
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public abstract class GDMCustomEvent : GDMValueTag, IGDMEvent
    {
        private GDMAddress fAddress;
        private string fAgency;
        private string fCause;
        private string fClassification;
        private readonly GDMDateValue fDate;
        private GDMPlace fPlace;
        private string fReligiousAffilation;
        private GDMRestriction fRestriction;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;
        private GDMList<GDMMultimediaLink> fMultimediaLinks;


        public bool HasAddress
        {
            get { return fAddress != null && !fAddress.IsEmpty(); }
        }

        public GDMAddress Address
        {
            get {
                if (fAddress == null) {
                    fAddress = new GDMAddress();
                }
                return fAddress;
            }
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

        /// <summary>
        /// Tag "TYPE" (GEDCOM 5.5.1 Specification).
        /// </summary>
        public string Classification
        {
            get { return fClassification; }
            set { fClassification = value; }
        }

        public GDMDateValue Date
        {
            get { return fDate; }
        }

        public bool HasPlace
        {
            get { return fPlace != null && !fPlace.IsEmpty(); }
        }

        public GDMPlace Place
        {
            get {
                if (fPlace == null) {
                    fPlace = new GDMPlace();
                }

                return fPlace;
            }
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

        public bool HasNotes
        {
            get { return fNotes != null && fNotes.Count != 0; }
        }

        public GDMList<GDMNotes> Notes
        {
            get {
                if (fNotes == null) {
                    fNotes = new GDMList<GDMNotes>();
                }

                return fNotes;
            }
        }

        public bool HasSourceCitations
        {
            get { return fSourceCitations != null && fSourceCitations.Count != 0; }
        }

        public GDMList<GDMSourceCitation> SourceCitations
        {
            get {
                if (fSourceCitations == null) {
                    fSourceCitations = new GDMList<GDMSourceCitation>();
                }

                return fSourceCitations;
            }
        }

        public bool HasMultimediaLinks
        {
            get { return fMultimediaLinks != null && fMultimediaLinks.Count != 0; }
        }

        public GDMList<GDMMultimediaLink> MultimediaLinks
        {
            get {
                if (fMultimediaLinks == null) {
                    fMultimediaLinks = new GDMList<GDMMultimediaLink>();
                }

                return fMultimediaLinks;
            }
        }


        protected GDMCustomEvent()
        {
            fDate = new GDMDateValue();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fAddress != null) fAddress.Dispose();
                fDate.Dispose();
                if (fPlace != null) fPlace.Dispose();
                if (fNotes != null) fNotes.Dispose();
                if (fSourceCitations != null) fSourceCitations.Dispose();
                if (fMultimediaLinks != null) fMultimediaLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetName(string tagName)
        {
            int tagId = GEDCOMTagsTable.Lookup(tagName);
            SetName(tagId);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fAddress != null) fAddress.TrimExcess();
            fDate.TrimExcess();
            if (fPlace != null) fPlace.TrimExcess();
            if (fNotes != null) fNotes.TrimExcess();
            if (fSourceCitations != null) fSourceCitations.TrimExcess();
            if (fMultimediaLinks != null) fMultimediaLinks.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMCustomEvent sourceObj = (source as GDMCustomEvent);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            if (sourceObj.fAddress != null) Address.Assign(sourceObj.fAddress);
            fAgency = sourceObj.fAgency;
            fCause = sourceObj.fCause;
            fClassification = sourceObj.fClassification;
            fDate.Assign(sourceObj.fDate);
            if (sourceObj.fPlace != null) Place.Assign(sourceObj.fPlace);
            fReligiousAffilation = sourceObj.fReligiousAffilation;
            fRestriction = sourceObj.fRestriction;
            if (sourceObj.fNotes != null) AssignList(sourceObj.fNotes, Notes);
            if (sourceObj.fSourceCitations != null) AssignList(sourceObj.fSourceCitations, SourceCitations);
            if (sourceObj.fMultimediaLinks != null) AssignList(sourceObj.fMultimediaLinks, MultimediaLinks);
        }

        public override void Clear()
        {
            base.Clear();

            if (fAddress != null) fAddress.Clear();
            fAgency = string.Empty;
            fCause = string.Empty;
            fClassification = string.Empty;
            fDate.Clear();
            if (fPlace != null) fPlace.Clear();
            fReligiousAffilation = string.Empty;
            fRestriction = GDMRestriction.rnNone;
            if (fNotes != null) fNotes.Clear();
            if (fSourceCitations != null) fSourceCitations.Clear();
            if (fMultimediaLinks != null) fMultimediaLinks.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty()
                && string.IsNullOrEmpty(fClassification) && string.IsNullOrEmpty(fCause) && fDate.IsEmpty()
                && string.IsNullOrEmpty(fAgency) && (fRestriction == GDMRestriction.rnNone) && string.IsNullOrEmpty(fReligiousAffilation)
                && (fPlace == null || fPlace.IsEmpty())
                && (fAddress == null || fAddress.IsEmpty())
                && (fNotes == null || fNotes.Count == 0)
                && (fSourceCitations == null || fSourceCitations.Count == 0)
                && (fMultimediaLinks == null || fMultimediaLinks.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (fAddress != null) fAddress.ReplaceXRefs(map);
            fDate.ReplaceXRefs(map);
            if (fPlace != null) fPlace.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
            if (fSourceCitations != null) fSourceCitations.ReplaceXRefs(map);
            if (fMultimediaLinks != null) fMultimediaLinks.ReplaceXRefs(map);
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

                if (!this.HasPlace && !ev.HasPlace) {
                    locMatch = 100.0f;
                } else if (this.HasPlace && ev.HasPlace && this.Place.StringValue == ev.Place.StringValue) {
                    locMatch = 100.0f;
                }
            }

            float match = (dateMatch + locMatch) / matches;
            return match;
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

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAddress);
            hashCode.Add(fAgency);
            hashCode.Add(fCause);
            hashCode.Add(fClassification);
            hashCode.Add(fDate);
            hashCode.Add(fPlace);
            hashCode.Add(fReligiousAffilation);
            hashCode.Add(fRestriction);
            ProcessHashes(ref hashCode, fNotes);
            ProcessHashes(ref hashCode, fSourceCitations);
            ProcessHashes(ref hashCode, fMultimediaLinks);
        }
    }
}
