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
    public enum GDMRecordType
    {
        rtNone, // may be rename to Unknown?

        rtIndividual,
        rtFamily,
        rtNote,
        rtMultimedia,
        rtSource,
        rtRepository,
        rtGroup,
        rtResearch,
        rtTask,
        rtCommunication,
        rtLocation,
        rtSubmission,
        rtSubmitter,

        rtLast/* = rtSubmitter*/
    }


    /// <summary>
    /// 
    /// </summary>
    public class GDMRecord : GDMTag, IGDMRecord
    {
        protected GDMTree fTree;
        private string fAutomatedRecordID;
        private readonly GDMChangeDate fChangeDate;
        private string fUID;
        private string fXRef;

        private GDMList<GDMMultimediaLink> fMultimediaLinks;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;
        private GDMList<GDMUserReference> fUserReferences;


        public string AutomatedRecordID
        {
            get { return fAutomatedRecordID; }
            set { fAutomatedRecordID = value; }
        }

        public GDMChangeDate ChangeDate
        {
            get { return fChangeDate; }
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

        public GDMRecordType RecordType
        {
            get { return (GDMRecordType)base.Id; }
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

        public GDMTree Tree
        {
            get { return fTree; }
        }

        public string UID
        {
            get {
                if (string.IsNullOrEmpty(fUID)) {
                    fUID = GEDCOMUtils.CreateUID();
                }
                return fUID;
            }
            set { fUID = value; }
        }

        public bool HasUserReferences
        {
            get { return fUserReferences != null && fUserReferences.Count != 0; }
        }

        public GDMList<GDMUserReference> UserReferences
        {
            get {
                if (fUserReferences == null) {
                    fUserReferences = new GDMList<GDMUserReference>();
                }

                return fUserReferences;
            }
        }

        public string XRef
        {
            get { return fXRef; }
        }


        public GDMRecord(GDMTree tree)
        {
            fTree = tree;
            fXRef = string.Empty;
            fAutomatedRecordID = string.Empty;
            fChangeDate = new GDMChangeDate();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fNotes != null) fNotes.Dispose();
                if (fSourceCitations != null) fSourceCitations.Dispose();
                if (fMultimediaLinks != null) fMultimediaLinks.Dispose();
                if (fUserReferences != null) fUserReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        public void ResetTree(GDMTree tree)
        {
            fTree = tree;
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fChangeDate.TrimExcess();
            if (fNotes != null) fNotes.TrimExcess();
            if (fSourceCitations != null) fSourceCitations.TrimExcess();
            if (fMultimediaLinks != null) fMultimediaLinks.TrimExcess();
            if (fUserReferences != null) fUserReferences.TrimExcess();
        }

        public int IndexOfSource(GDMSourceRecord sourceRec)
        {
            if (sourceRec != null && fSourceCitations != null) {
                int num = fSourceCitations.Count;
                for (int i = 0; i < num; i++) {
                    if (fSourceCitations[i].XRef == sourceRec.XRef) {
                        return i;
                    }
                }
            }

            return -1;
        }

        public override void Assign(GDMTag source)
        {
            GDMRecord sourceRec = source as GDMRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            if (sourceRec.fNotes != null) AssignList(sourceRec.fNotes, Notes);
            if (sourceRec.fMultimediaLinks != null) AssignList(sourceRec.fMultimediaLinks, MultimediaLinks);
            if (sourceRec.fSourceCitations != null) AssignList(sourceRec.fSourceCitations, SourceCitations);
            if (sourceRec.fUserReferences != null) AssignList(sourceRec.fUserReferences, UserReferences);
        }

        public virtual void MoveTo(GDMRecord targetRecord)
        {
            var subTags = SubTags;
            while (subTags.Count > 0) {
                GDMTag tag = subTags.Extract(0);
                if (tag.GetTagType() == GEDCOMTagType.CHAN) {
                    tag.Dispose();
                } else {
                    targetRecord.AddTag(tag);
                }
            }

            while (fNotes != null && fNotes.Count > 0) {
                GDMTag tag = fNotes.Extract(0);
                targetRecord.Notes.Add((GDMNotes)tag);
            }

            while (fMultimediaLinks != null && fMultimediaLinks.Count > 0) {
                GDMTag tag = fMultimediaLinks.Extract(0);
                targetRecord.MultimediaLinks.Add((GDMMultimediaLink)tag);
            }

            while (fSourceCitations != null && fSourceCitations.Count > 0) {
                GDMTag tag = fSourceCitations.Extract(0);
                targetRecord.SourceCitations.Add((GDMSourceCitation)tag);
            }

            while (fUserReferences != null && fUserReferences.Count > 0) {
                GDMTag tag = fUserReferences.Extract(0);
                targetRecord.UserReferences.Add((GDMUserReference)tag);
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (fNotes != null) fNotes.ReplaceXRefs(map);
            if (fSourceCitations != null) fSourceCitations.ReplaceXRefs(map);
            if (fMultimediaLinks != null) fMultimediaLinks.ReplaceXRefs(map);
            if (fUserReferences != null) fUserReferences.ReplaceXRefs(map);
        }

        public override void Clear()
        {
            base.Clear();

            fAutomatedRecordID = string.Empty;
            fChangeDate.Clear();
            if (fNotes != null) fNotes.Clear();
            if (fSourceCitations != null) fSourceCitations.Clear();
            if (fMultimediaLinks != null) fMultimediaLinks.Clear();
            if (fUserReferences != null) fUserReferences.Clear();
            fUID = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fAutomatedRecordID) && fChangeDate.IsEmpty() &&
                (fNotes == null || fNotes.Count == 0) &&
                (fSourceCitations == null || fSourceCitations.Count == 0) && 
                (fMultimediaLinks == null || fMultimediaLinks.Count == 0) &&
                (fUserReferences == null || fUserReferences.Count == 0);
        }

        public void SetXRef(GDMTree tree, string newXRef, bool removeOldXRef)
        {
            string oldXRef = fXRef;
            fXRef = newXRef;

            if (tree != null) {
                tree.SetXRef(oldXRef, this, removeOldXRef);
            }
        }

        public string GetXRefNum()
        {
            string xref = XRef;

            int i = 0;
            int last = xref.Length;
            while (i < last && (xref[i] < '0' || xref[i] > '9')) i++;
            xref = ((i < last) ? xref.Substring(i) : string.Empty);
            return xref;
        }

        public long GetId()
        {
            return GEDCOMUtils.GetXRefNumber(XRef);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAutomatedRecordID);
            // without fChangeDate and fUID!
            hashCode.Add(fXRef);
            ProcessHashes(ref hashCode, fMultimediaLinks);
            ProcessHashes(ref hashCode, fNotes);
            ProcessHashes(ref hashCode, fSourceCitations);
            ProcessHashes(ref hashCode, fUserReferences);
        }
    }
}
