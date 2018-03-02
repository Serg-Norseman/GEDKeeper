/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class GEDCOMRecord : GEDCOMCustomRecord, IGEDCOMStructWithLists
    {
        private GEDCOMRecordType fRecordType;

        private GEDCOMList<GEDCOMMultimediaLink> fMultimediaLinks;
        private GEDCOMList<GEDCOMNotes> fNotes;
        private GEDCOMList<GEDCOMSourceCitation> fSourceCitations;
        private GEDCOMList<GEDCOMUserReference> fUserReferences;

        public string AutomatedRecordID
        {
            get { return GetTagStringValue("RIN"); }
            set { SetTagStringValue("RIN", value); }
        }

        public GEDCOMChangeDate ChangeDate
        {
            get { return TagClass("CHAN", GEDCOMChangeDate.Create) as GEDCOMChangeDate; }
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
            get { return GetTagStringValue("_UID"); }
            set { SetTagStringValue("_UID", value); }
        }

        public GEDCOMList<GEDCOMUserReference> UserReferences
        {
            get { return fUserReferences; }
        }

        private static string CreateUID()
        {
            byte[] binary = Guid.NewGuid().ToByteArray();
            string result = SysUtils.EncodeUID(binary);
            return result;
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            fRecordType = GEDCOMRecordType.rtNone;

            fNotes = new GEDCOMList<GEDCOMNotes>(this);
            fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
            fMultimediaLinks = new GEDCOMList<GEDCOMMultimediaLink>(this);
            fUserReferences = new GEDCOMList<GEDCOMUserReference>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
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

            foreach (GEDCOMNotes sourceNote in sourceRec.fNotes)
            {
                GEDCOMNotes copy = (GEDCOMNotes)GEDCOMNotes.Create(Owner, this, "", "");
                copy.Assign(sourceNote);
                Notes.Add(copy);
            }

            foreach (GEDCOMMultimediaLink sourceMediaLink in sourceRec.fMultimediaLinks)
            {
                GEDCOMMultimediaLink copy = (GEDCOMMultimediaLink)GEDCOMMultimediaLink.Create(Owner, this, "", "");
                copy.Assign(sourceMediaLink);
                MultimediaLinks.Add(copy);
            }

            foreach (GEDCOMSourceCitation sourceSrcCit in sourceRec.fSourceCitations)
            {
                GEDCOMSourceCitation copy = (GEDCOMSourceCitation)GEDCOMSourceCitation.Create(Owner, this, "", "");
                copy.Assign(sourceSrcCit);
                SourceCitations.Add(copy);
            }

            foreach (GEDCOMUserReference sourceUserRef in sourceRec.fUserReferences)
            {
                GEDCOMUserReference copy = (GEDCOMUserReference)GEDCOMUserReference.Create(Owner, this, "", "");
                copy.Assign(sourceUserRef);
                UserReferences.Add(copy);
            }
        }

        public virtual void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            if (clearDest)
            {
                targetRecord.Clear();
            }

            if (fTags != null)
            {
                while (fTags.Count > 0)
                {
                    GEDCOMTag tag = fTags.Extract(0);
                    if (tag.Name == "CHAN" && !clearDest)
                    {
                        tag.Dispose();
                    }
                    else
                    {
                        tag.ResetParent(targetRecord);
                        targetRecord.InsertTag(tag);
                    }
                }
            }

            while (fNotes.Count > 0)
            {
                GEDCOMTag tag = fNotes.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.Notes.Add((GEDCOMNotes) tag);
            }

            while (fMultimediaLinks.Count > 0)
            {
                GEDCOMTag tag = fMultimediaLinks.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.MultimediaLinks.Add((GEDCOMMultimediaLink) tag);
            }

            while (fSourceCitations.Count > 0)
            {
                GEDCOMTag tag = fSourceCitations.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.SourceCitations.Add((GEDCOMSourceCitation) tag);
            }

            while (fUserReferences.Count > 0)
            {
                GEDCOMTag tag = fUserReferences.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.UserReferences.Add((GEDCOMUserReference) tag);
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

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);

            fNotes.ResetOwner(newOwner);
            fSourceCitations.ResetOwner(newOwner);
            fMultimediaLinks.ResetOwner(newOwner);
            fUserReferences.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            fNotes.SaveToStream(stream);
            fSourceCitations.SaveToStream(stream);
            fMultimediaLinks.SaveToStream(stream);
            fUserReferences.SaveToStream(stream);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NOTE") {
                result = fNotes.Add(new GEDCOMNotes(Owner, this, tagName, tagValue));
            } else if (tagName == "SOUR") {
                result = fSourceCitations.Add(new GEDCOMSourceCitation(Owner, this, tagName, tagValue));
            } else if (tagName == "OBJE") {
                result = fMultimediaLinks.Add(new GEDCOMMultimediaLink(Owner, this, tagName, tagValue));
            } else if (tagName == "REFN") {
                result = fUserReferences.Add(new GEDCOMUserReference(Owner, this, tagName, tagValue));
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
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fNotes.Count == 0 && fSourceCitations.Count == 0 && fMultimediaLinks.Count == 0 && fUserReferences.Count == 0;
        }

        public string NewXRef()
        {
            if (Owner != null)
            {
                string newXRef = Owner.XRefIndex_NewXRef(this);
                XRef = newXRef;
            }
            return XRef;
        }

        public void RequireUID()
        {
            if (string.IsNullOrEmpty(UID))
            {
                UID = CreateUID();
            }
        }

        public void InitNew()
        {
            NewXRef();
            RequireUID();
        }

        protected GEDCOMRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        #region Auxiliary

        public string GetXRefNum()
        {
            string xref = XRef;

            int i = 0;
            int last = xref.Length;
            while (i < last && (xref[i] < '0' || xref[i] > '9')) i++;
            xref = ((i < last) ? xref.Substring(i) : "");
            return xref;
        }

        public int GetId()
        {
            int result;
            try
            {
                string xref = GetXRefNum();
                result = ConvertHelper.ParseInt(xref, 0);
            }
            catch (Exception)
            {
                result = -1;
            }
            return result;
        }

        public GEDCOMNotes AddNote(GEDCOMNoteRecord noteRec)
        {
            GEDCOMNotes note = null;

            if (noteRec != null)
            {
                note = new GEDCOMNotes(Owner, this, "", "");
                note.Value = noteRec;
                Notes.Add(note);
            }

            return note;
        }

        public GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality)
        {
            GEDCOMSourceCitation cit = null;

            if (sourceRec != null) {
                cit = new GEDCOMSourceCitation(Owner, this, "", "");
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
                mmLink = new GEDCOMMultimediaLink(Owner, this, "", "");
                mmLink.Value = mediaRec;
                MultimediaLinks.Add(mmLink);
            }

            return mmLink;
        }

        public void AddUserRef(string reference)
        {
            GEDCOMUserReference uRef = new GEDCOMUserReference(Owner, this, "", "");
            uRef.StringValue = reference;
            UserReferences.Add(uRef);
        }

        #endregion
    }
}
