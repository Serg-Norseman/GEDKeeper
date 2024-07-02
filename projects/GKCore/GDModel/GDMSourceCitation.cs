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

namespace GDModel
{
    public sealed class GDMSourceCitation : GDMPointer, IGDMTextObject, IGDMStructWithNotes, IGDMStructWithMultimediaLinks
    {
        private int fCertaintyAssessment;
        private GDMSourceCitationData fData;
        private string fPage;

        // converted to SourceRecord
        private GDMLines fDescription;
        private GDMList<GDMMultimediaLink> fMultimediaLinks;
        private GDMList<GDMNotes> fNotes;
        private GDMTextTag fText;


        public int CertaintyAssessment
        {
            get { return fCertaintyAssessment; }
            set { fCertaintyAssessment = value; }
        }

        public GDMSourceCitationData Data
        {
            get {
                if (fData == null) {
                    fData = new GDMSourceCitationData();
                }

                return fData;
            }
        }

        public GDMLines Description
        {
            get {
                if (IsPointer) {
                    throw new InvalidOperationException("GDMSourceCitation is a pointer, please dereference");
                }

                return fDescription;
            }
        }

        GDMLines IGDMTextObject.Lines
        {
            get { return fDescription; }
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

        public string Page
        {
            get { return fPage; }
            set { fPage = value; }
        }

        public GDMTextTag Text
        {
            get {
                if (fText == null) {
                    fText = new GDMTextTag((int)GEDCOMTagType.TEXT);
                }

                return fText;
            }
        }


        public GDMSourceCitation()
        {
            SetName(GEDCOMTagType.SOUR);

            fCertaintyAssessment = -1;
            fDescription = new GDMLines();
            fPage = string.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fData != null) fData.Dispose();
                if (fText != null) fText.Dispose();
                if (fNotes != null) fNotes.Dispose();
                if (fMultimediaLinks != null) fMultimediaLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fData != null) fData.TrimExcess();
            fDescription.TrimExcess();
            if (fText != null) fText.TrimExcess();
            if (fNotes != null) fNotes.TrimExcess();
            if (fMultimediaLinks != null) fMultimediaLinks.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMSourceCitation sourceObj = (source as GDMSourceCitation);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fCertaintyAssessment = sourceObj.fCertaintyAssessment;
            fPage = sourceObj.fPage;
            if (sourceObj.fData != null) Data.Assign(sourceObj.fData);
            fDescription.Assign(sourceObj.fDescription);
            if (sourceObj.fText != null) Text.Assign(sourceObj.fText);
            if (sourceObj.fNotes != null) AssignList(sourceObj.fNotes, Notes);
            if (sourceObj.fMultimediaLinks != null) AssignList(sourceObj.fMultimediaLinks, MultimediaLinks);
        }

        public override void Clear()
        {
            base.Clear();

            fCertaintyAssessment = -1;
            if (fData != null) fData.Clear();
            fDescription.Clear();
            fPage = string.Empty;
            if (fText != null) fText.Clear();
            if (fNotes != null) fNotes.Clear();
            if (fMultimediaLinks != null) fMultimediaLinks.Clear();
        }

        public override bool IsEmpty()
        {
            bool result;

            bool isCommonEmpty = string.IsNullOrEmpty(fPage)
                && (fNotes == null || fNotes.Count == 0)
                && (fMultimediaLinks == null || fMultimediaLinks.Count == 0);

            if (IsPointer) {
                result = base.IsEmpty() && isCommonEmpty && (fData == null || fData.IsEmpty());
            } else {
                result = fDescription.IsEmpty() && (SubTags.Count == 0) && (fText == null || fText.IsEmpty()) && isCommonEmpty;
            }

            return result;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (fNotes != null) fNotes.ReplaceXRefs(map);
            if (fMultimediaLinks != null) fMultimediaLinks.ReplaceXRefs(map);
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : ((fDescription.Count > 0) ? fDescription[0] : string.Empty);
            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fDescription.Clear();
                if (!string.IsNullOrEmpty(result)) {
                    fDescription.Add(result);
                }
                result = string.Empty;
            }
            return result;
        }

        /// <summary>
        /// Strange values were found, possibly from other genealogical programs.
        /// </summary>
        /// <returns>Checked value of CertaintyAssessment</returns>
        public int GetValidCertaintyAssessment()
        {
            int val = fCertaintyAssessment;
            return (val >= 0 && val <= 3) ? val : 0;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fCertaintyAssessment);
            hashCode.Add(fData);
            hashCode.Add(fPage);
            hashCode.Add(fDescription);
            ProcessHashes(ref hashCode, fMultimediaLinks);
            ProcessHashes(ref hashCode, fNotes);
            hashCode.Add(fText);
        }
    }
}
