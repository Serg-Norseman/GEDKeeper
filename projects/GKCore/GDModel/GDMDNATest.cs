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
    public interface IGDMStructWithDNA : IGDMObject
    {
        bool HasDNATests { get; }
        GDMList<GDMDNATest> DNATests { get; }
    }


    public enum GDMDNAFileFormat
    {
        None = 0,
        SNP,
        STR
    }


    /// <summary>
    /// Data structure for describing DNA tests in GEDCOM format (GEDKeeper's extension).
    /// Future tags: ETHNIC_PERCENTAGE, MATCHES, CONFIDENCE_LEVEL, ANALYSIS_STATUS (completed, in progress, pending).
    /// </summary>
    public class GDMDNATest : GDMValueTag, IGDMStructWithNotes, IGDMStructWithMultimediaLinks, IGDMStructWithRestriction
    {
        private string fAgency;
        private readonly GDMDate fDate;
        private GDMDNAFileFormat fFileFormat;
        private string fFileReference;
        private string fTestName;
        private string fMHaplogroup;
        private string fYHaplogroup;
        private GDMRestriction fRestriction;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMMultimediaLink> fMultimediaLinks;


        /// <summary>
        /// Name of the laboratory, company (AGNC).
        /// </summary>
        public string Agency
        {
            get { return fAgency; }
            set { fAgency = value; }
        }

        /// <summary>
        /// Date of the test (DATE).
        /// </summary>
        public GDMDate Date
        {
            get { return fDate; }
        }

        /// <summary>
        /// When ordering and performing a test, the following may be specified:
        /// full or partial name of the person, nickname.
        /// The test may also have its own identifier (NAME).
        /// </summary>
        public string TestName
        {
            get { return fTestName; }
            set { fTestName = value; }
        }

        /// <summary>
        /// Raw data file format (FORM).
        /// </summary>
        public GDMDNAFileFormat FileFormat
        {
            get { return fFileFormat; }
            set { fFileFormat = value; }
        }

        /// <summary>
        /// Path to raw data file (FILE).
        /// </summary>
        public string FileReference
        {
            get { return fFileReference; }
            set { fFileReference = value; }
        }

        /// <summary>
        /// mtDNA Haplogroup (_MHAP).
        /// </summary>
        public string MHaplogroup
        {
            get { return fMHaplogroup; }
            set { fMHaplogroup = value; }
        }

        /// <summary>
        /// Y-DNA Haplogroup (_YHAP).
        /// </summary>
        public string YHaplogroup
        {
            get { return fYHaplogroup; }
            set { fYHaplogroup = value; }
        }

        /// <summary>
        /// Restriction notice (RESN).
        /// </summary>
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

            fDate = new GDMDate();
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

            fTestName = sourceObj.fTestName;
            fFileFormat = sourceObj.fFileFormat;
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

            fTestName = string.Empty;
            fFileFormat = GDMDNAFileFormat.None;
            fMHaplogroup = string.Empty;
            fYHaplogroup = string.Empty;
            fFileReference = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty()
                && fDate.IsEmpty() && string.IsNullOrEmpty(fAgency) && (fRestriction == GDMRestriction.rnNone)
                && string.IsNullOrEmpty(fTestName) && (fFileFormat == GDMDNAFileFormat.None) && string.IsNullOrEmpty(fFileReference)
                && string.IsNullOrEmpty(fMHaplogroup) && string.IsNullOrEmpty(fYHaplogroup)
                && fNotes.IsEmpty() && fMultimediaLinks.IsEmpty();
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

            hashCode.Add(fTestName);
            hashCode.Add(fFileFormat);
            hashCode.Add(fMHaplogroup);
            hashCode.Add(fYHaplogroup);
            hashCode.Add(fFileReference);
        }
    }
}
