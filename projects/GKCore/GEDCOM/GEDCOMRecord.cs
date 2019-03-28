/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.IO;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class GEDCOMRecord : GEDCOMCustomRecord, IGEDCOMStructWithLists
    {
        private object fExtData;
        private GEDCOMRecordType fRecordType;
        private string fUID;

        private GEDCOMList<GEDCOMMultimediaLink> fMultimediaLinks;
        private GEDCOMList<GEDCOMNotes> fNotes;
        private GEDCOMList<GEDCOMSourceCitation> fSourceCitations;
        private GEDCOMList<GEDCOMUserReference> fUserReferences;


        public string AutomatedRecordID
        {
            get { return GetTagStringValue(GEDCOMTagType.RIN); }
            set { SetTagStringValue(GEDCOMTagType.RIN, value); }
        }

        public GEDCOMChangeDate ChangeDate
        {
            get { return TagClass(GEDCOMTagType.CHAN, GEDCOMChangeDate.Create) as GEDCOMChangeDate; }
        }

        public object ExtData
        {
            get { return fExtData; }
            set { fExtData = value; }
        }

        public GEDCOMList<GEDCOMMultimediaLink> MultimediaLinks
        {
            get	{ return fMultimediaLinks; }
        }

        public GEDCOMList<GEDCOMNotes> Notes
        {
            get { return fNotes; }
        }

        public GEDCOMRecordType RecordType
        {
            get { return fRecordType; }
        }

        public GEDCOMList<GEDCOMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }

        public string UID
        {
            get { return fUID; }
            set { fUID = value; }
        }

        public GEDCOMList<GEDCOMUserReference> UserReferences
        {
            get { return fUserReferences; }
        }


        protected GEDCOMRecord(GEDCOMObject owner) : base(owner)
        {
            fNotes = new GEDCOMList<GEDCOMNotes>(this);
            fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
            fMultimediaLinks = new GEDCOMList<GEDCOMMultimediaLink>(this);
            fUserReferences = new GEDCOMList<GEDCOMUserReference>(this);
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

        protected void SetRecordType(GEDCOMRecordType type)
        {
            fRecordType = type;
        }

        public int IndexOfSource(GEDCOMSourceRecord sourceRec)
        {
            if (sourceRec != null) {
                int num = fSourceCitations.Count;
                for (int i = 0; i < num; i++)
                {
                    if (fSourceCitations[i].XRef == sourceRec.XRef) {
                        return i;
                    }
                }
            }

            return -1;
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMRecord sourceRec = source as GEDCOMRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            foreach (GEDCOMNotes sourceNote in sourceRec.fNotes) {
                GEDCOMNotes copy = new GEDCOMNotes(this);
                copy.Assign(sourceNote);
                Notes.Add(copy);
            }

            foreach (GEDCOMMultimediaLink sourceMediaLink in sourceRec.fMultimediaLinks) {
                GEDCOMMultimediaLink copy = new GEDCOMMultimediaLink(this);
                copy.Assign(sourceMediaLink);
                MultimediaLinks.Add(copy);
            }

            foreach (GEDCOMSourceCitation sourceSrcCit in sourceRec.fSourceCitations) {
                GEDCOMSourceCitation copy = new GEDCOMSourceCitation(this);
                copy.Assign(sourceSrcCit);
                SourceCitations.Add(copy);
            }

            foreach (GEDCOMUserReference sourceUserRef in sourceRec.fUserReferences) {
                GEDCOMUserReference copy = new GEDCOMUserReference(this);
                copy.Assign(sourceUserRef);
                UserReferences.Add(copy);
            }
        }

        public virtual void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            if (clearDest) {
                targetRecord.Clear();
            }

            var subtags = GetTagList();
            while (subtags.Count > 0) {
                GEDCOMTag tag = subtags.Extract(0);
                if (tag.Name == GEDCOMTagType.CHAN && !clearDest) {
                    tag.Dispose();
                } else {
                    tag.ResetOwner(targetRecord);
                    targetRecord.InsertTag(tag);
                }
            }

            while (fNotes.Count > 0) {
                GEDCOMTag tag = fNotes.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.Notes.Add((GEDCOMNotes)tag);
            }

            while (fMultimediaLinks.Count > 0) {
                GEDCOMTag tag = fMultimediaLinks.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.MultimediaLinks.Add((GEDCOMMultimediaLink)tag);
            }

            while (fSourceCitations.Count > 0) {
                GEDCOMTag tag = fSourceCitations.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.SourceCitations.Add((GEDCOMSourceCitation)tag);
            }

            while (fUserReferences.Count > 0) {
                GEDCOMTag tag = fUserReferences.Extract(0);
                tag.ResetOwner(targetRecord);
                targetRecord.UserReferences.Add((GEDCOMUserReference)tag);
            }
        }

        public override void Pack()
        {
            base.Pack();

            fNotes.Pack();
            fSourceCitations.Pack();
            fMultimediaLinks.Pack();
            fUserReferences.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fNotes.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
            fMultimediaLinks.ReplaceXRefs(map);
            fUserReferences.ReplaceXRefs(map);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            WriteTagLine(stream, Level + 1, GEDCOMTagType._UID, fUID, true);

            fNotes.SaveToStream(stream);
            fSourceCitations.SaveToStream(stream);
            fMultimediaLinks.SaveToStream(stream);
            fUserReferences.SaveToStream(stream);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.NOTE) {
                result = fNotes.Add(new GEDCOMNotes(this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SOUR) {
                result = fSourceCitations.Add(new GEDCOMSourceCitation(this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.OBJE) {
                result = fMultimediaLinks.Add(new GEDCOMMultimediaLink(this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.REFN) {
                result = fUserReferences.Add(new GEDCOMUserReference(this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._UID) {
                fUID = tagValue;
                result = null;
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();

            fNotes.Clear();
            fSourceCitations.Clear();
            fMultimediaLinks.Clear();
            fUserReferences.Clear();
            fUID = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fNotes.Count == 0 && fSourceCitations.Count == 0 && fMultimediaLinks.Count == 0 && fUserReferences.Count == 0;
        }

        public string NewXRef()
        {
            var owner = GetTree();
            if (owner != null) {
                string newXRef = owner.XRefIndex_NewXRef(this);
                XRef = newXRef;
            }
            return XRef;
        }

        public void RequireUID()
        {
            if (string.IsNullOrEmpty(fUID)) {
                fUID = GEDCOMUtils.CreateUID();
            }
        }

        public void InitNew()
        {
            NewXRef();
            RequireUID();
        }

        #region Auxiliary

        public string GetXRefNum()
        {
            string xref = XRef;

            int i = 0;
            int last = xref.Length;
            while (i < last && (xref[i] < '0' || xref[i] > '9')) i++;
            xref = ((i < last) ? xref.Substring(i) : string.Empty);
            return xref;
        }

        public int GetId()
        {
            return GEDCOMUtils.GetXRefNumber(XRef);
        }

        public GEDCOMNotes AddNote(GEDCOMNoteRecord noteRec)
        {
            GEDCOMNotes note = null;

            if (noteRec != null) {
                note = new GEDCOMNotes(this);
                note.Value = noteRec;
                Notes.Add(note);
            }

            return note;
        }

        public GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality)
        {
            GEDCOMSourceCitation cit = null;

            if (sourceRec != null) {
                cit = new GEDCOMSourceCitation(this);
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                SourceCitations.Add(cit);
            }

            return cit;
        }

        public GEDCOMMultimediaLink AddMultimedia(GEDCOMMultimediaRecord mediaRec)
        {
            GEDCOMMultimediaLink mmLink = null;

            if (mediaRec != null) {
                mmLink = new GEDCOMMultimediaLink(this);
                mmLink.Value = mediaRec;
                MultimediaLinks.Add(mmLink);
            }

            return mmLink;
        }

        public void AddUserRef(string reference)
        {
            GEDCOMUserReference uRef = new GEDCOMUserReference(this);
            uRef.StringValue = reference;
            UserReferences.Add(uRef);
        }

        public string GetFolder()
        {
            var folderTag = FindTag(GEDCOMTagType._FOLDER, 0);
            return (folderTag == null) ? "" : folderTag.StringValue;
        }

        public void SetFolder(string value)
        {
            if (!HasFolderSupport()) {
                return;
            }

            var folderTag = FindTag(GEDCOMTagType._FOLDER, 0);
            if (!string.IsNullOrEmpty(value)) {
                if (folderTag == null) {
                    AddTag(GEDCOMTagType._FOLDER, value, null);
                } else {
                    folderTag.StringValue = value;
                }
            } else {
                if (folderTag != null) {
                    DeleteTag(GEDCOMTagType._FOLDER);
                }
            }
        }

        public bool HasFolderSupport()
        {
            bool result = false;

            switch (fRecordType) {
                case GEDCOMRecordType.rtNone:
                case GEDCOMRecordType.rtNote:
                case GEDCOMRecordType.rtMultimedia:
                case GEDCOMRecordType.rtSource:
                case GEDCOMRecordType.rtRepository:
                case GEDCOMRecordType.rtLocation:
                case GEDCOMRecordType.rtSubmission:
                case GEDCOMRecordType.rtSubmitter:
                    break;

                case GEDCOMRecordType.rtIndividual:
                case GEDCOMRecordType.rtFamily:
                case GEDCOMRecordType.rtGroup:
                case GEDCOMRecordType.rtResearch:
                case GEDCOMRecordType.rtTask:
                case GEDCOMRecordType.rtCommunication:
                    result = true;
                    break;
            }

            return result;
        }

        #endregion
    }
}
