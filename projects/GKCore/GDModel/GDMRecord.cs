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
    public class GDMRecord : GDMTag, IGDMStructWithLists, IGDMStructWithUserReferences
    {
        private string fAutomatedRecordID;
        private GDMChangeDate fChangeDate;
        private object fExtData;
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

        // TODO: need to remove from here, replace with other methods
        public object ExtData
        {
            get { return fExtData; }
            set { fExtData = value; }
        }

        public GDMList<GDMMultimediaLink> MultimediaLinks
        {
            get	{ return fMultimediaLinks; }
        }

        public GDMList<GDMNotes> Notes
        {
            get { return fNotes; }
        }

        public GDMRecordType RecordType
        {
            get { return (GDMRecordType)base.Id; }
        }

        public GDMList<GDMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
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

        public GDMList<GDMUserReference> UserReferences
        {
            get { return fUserReferences; }
        }

        public string XRef
        {
            get { return fXRef; }
        }

        public GDMRecord(GDMObject owner) : base(owner)
        {
            fXRef = string.Empty;
            fAutomatedRecordID = string.Empty;
            fChangeDate = new GDMChangeDate(this);
            fNotes = new GDMList<GDMNotes>(this);
            fSourceCitations = new GDMList<GDMSourceCitation>(this);
            fMultimediaLinks = new GDMList<GDMMultimediaLink>(this);
            fUserReferences = new GDMList<GDMUserReference>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fNotes.Dispose();
                fSourceCitations.Dispose();
                fMultimediaLinks.Dispose();
                fUserReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fChangeDate.TrimExcess();
            fNotes.TrimExcess();
            fSourceCitations.TrimExcess();
            fMultimediaLinks.TrimExcess();
            fUserReferences.TrimExcess();
        }

        internal override GDMTree GetTree()
        {
            return (Owner as GDMTree);
        }

        public int IndexOfSource(GDMSourceRecord sourceRec)
        {
            if (sourceRec != null) {
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

            AssignList(sourceRec.fNotes, fNotes);
            AssignList(sourceRec.fMultimediaLinks, fMultimediaLinks);
            AssignList(sourceRec.fSourceCitations, fSourceCitations);
            AssignList(sourceRec.fUserReferences, fUserReferences);
        }

        public virtual void MoveTo(GDMRecord targetRecord, bool clearDest)
        {
            if (clearDest) {
                targetRecord.Clear();
            }

            var subTags = SubTags;
            while (subTags.Count > 0) {
                GDMTag tag = subTags.Extract(0);
                if (tag.GetTagType() == GEDCOMTagType.CHAN && !clearDest) {
                    tag.Dispose();
                } else {
                    tag.ResetOwner(targetRecord);
                    targetRecord.AddTag(tag);
                }
            }

            while (fNotes.Count > 0) {
                GDMTag tag = fNotes.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.Notes.Add((GDMNotes)tag);
            }

            while (fMultimediaLinks.Count > 0) {
                GDMTag tag = fMultimediaLinks.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.MultimediaLinks.Add((GDMMultimediaLink)tag);
            }

            while (fSourceCitations.Count > 0) {
                GDMTag tag = fSourceCitations.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.SourceCitations.Add((GDMSourceCitation)tag);
            }

            while (fUserReferences.Count > 0) {
                GDMTag tag = fUserReferences.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.UserReferences.Add((GDMUserReference)tag);
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fNotes.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
            fMultimediaLinks.ReplaceXRefs(map);
            fUserReferences.ReplaceXRefs(map);
        }

        public override void Clear()
        {
            base.Clear();

            fAutomatedRecordID = string.Empty;
            fChangeDate.Clear();
            fNotes.Clear();
            fSourceCitations.Clear();
            fMultimediaLinks.Clear();
            fUserReferences.Clear();
            fUID = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fAutomatedRecordID) && fChangeDate.IsEmpty() &&
                (fNotes.Count == 0) && (fSourceCitations.Count == 0) && 
                (fMultimediaLinks.Count == 0) && (fUserReferences.Count == 0);
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
    }
}
