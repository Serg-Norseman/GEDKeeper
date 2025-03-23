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

namespace GDModel
{
    public enum GDMDNADataType
    {
        None = 0,
        SNP,
        STR
    }


    public class GDMDNATest : GDMValueTag, IGDMStructWithNotes, IGDMStructWithMultimediaLinks
    {
        private string fAgency;
        private GDMDNADataType fDataType;
        private readonly GDMDateValue fDate;
        private string fFileReference;
        private string fPersonName;
        private string fMHaplogroup;
        private string fYHaplogroup;
        private GDMRestriction fRestriction;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMMultimediaLink> fMultimediaLinks;


        public string Agency
        {
            get { return fAgency; }
            set { fAgency = value; }
        }

        public GDMDateValue Date
        {
            get { return fDate; }
        }

        /// <summary>
        /// When ordering and performing a test, the following may be specified:
        /// full or partial name of the person, nickname.
        /// The test may also have its own identifier.
        /// </summary>
        public string PersonName
        {
            get { return fPersonName; }
            set { fPersonName = value; }
        }

        /// <summary>
        /// Description can be entered via regular notes.
        /// </summary>
        //public string Description { get; set; }

        public GDMDNADataType DataType
        {
            get { return fDataType; }
            set { fDataType = value; }
        }

        public string FileReference
        {
            get { return fFileReference; }
            set { fFileReference = value; }
        }

        public string MHaplogroup
        {
            get { return fMHaplogroup; }
            set { fMHaplogroup = value; }
        }

        public string YHaplogroup
        {
            get { return fYHaplogroup; }
            set { fYHaplogroup = value; }
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


        public GDMDNATest()
        {
            SetName(GEDCOMTagType._DNA);

            fDate = new GDMDateValue();

            // FileReference / FILE
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fDate.Dispose();
                if (fNotes != null) fNotes.Dispose();
                if (fMultimediaLinks != null) fMultimediaLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fDate.TrimExcess();
            if (fNotes != null) fNotes.TrimExcess();
            if (fMultimediaLinks != null) fMultimediaLinks.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMDNATest sourceObj = (source as GDMDNATest);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fAgency = sourceObj.fAgency;
            fDate.Assign(sourceObj.fDate);
            fRestriction = sourceObj.fRestriction;
            if (sourceObj.fNotes != null) AssignList(sourceObj.fNotes, Notes);
            if (sourceObj.fMultimediaLinks != null) AssignList(sourceObj.fMultimediaLinks, MultimediaLinks);

            fPersonName = sourceObj.fPersonName;
            fDataType = sourceObj.fDataType;
            fMHaplogroup = sourceObj.fMHaplogroup;
            fYHaplogroup = sourceObj.fYHaplogroup;
            fFileReference = sourceObj.fFileReference;
        }

        public override void Clear()
        {
            base.Clear();

            fAgency = string.Empty;
            fDate.Clear();
            fRestriction = GDMRestriction.rnNone;
            if (fNotes != null) fNotes.Clear();
            if (fMultimediaLinks != null) fMultimediaLinks.Clear();

            fPersonName = string.Empty;
            fDataType = GDMDNADataType.None;
            fMHaplogroup = string.Empty;
            fYHaplogroup = string.Empty;
            fFileReference = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty()
                && fDate.IsEmpty() && string.IsNullOrEmpty(fAgency) && (fRestriction == GDMRestriction.rnNone)
                && string.IsNullOrEmpty(fPersonName) && (fDataType == GDMDNADataType.None) && string.IsNullOrEmpty(fFileReference)
                && string.IsNullOrEmpty(fMHaplogroup) && string.IsNullOrEmpty(fYHaplogroup)
                && (fNotes == null || fNotes.Count == 0)
                && (fMultimediaLinks == null || fMultimediaLinks.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fDate.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
            if (fMultimediaLinks != null) fMultimediaLinks.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAgency);
            hashCode.Add(fDate);
            hashCode.Add(fRestriction);
            ProcessHashes(ref hashCode, fNotes);
            ProcessHashes(ref hashCode, fMultimediaLinks);

            hashCode.Add(fPersonName);
            hashCode.Add(fDataType);
            hashCode.Add(fMHaplogroup);
            hashCode.Add(fYHaplogroup);
            hashCode.Add(fFileReference);
        }
    }
}
