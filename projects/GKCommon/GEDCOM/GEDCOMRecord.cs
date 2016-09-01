/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
        private GEDCOMRecordType fRecordType;

        private GEDCOMList<GEDCOMMultimediaLink> fMultimediaLinks;
        private GEDCOMList<GEDCOMNotes> fNotes;
        private GEDCOMList<GEDCOMSourceCitation> fSourceCitations;
        private GEDCOMList<GEDCOMUserReference> fUserReferences;

        public string AutomatedRecordID
        {
            get { return base.GetTagStringValue("RIN"); }
            set { base.SetTagStringValue("RIN", value); }
        }

        public GEDCOMChangeDate ChangeDate
        {
            get { return base.TagClass("CHAN", GEDCOMChangeDate.Create) as GEDCOMChangeDate; }
        }

        public GEDCOMList<GEDCOMMultimediaLink> MultimediaLinks
        {
            get	{ return this.fMultimediaLinks; }
        }

        public GEDCOMList<GEDCOMNotes> Notes
        {
            get { return this.fNotes; }
        }

        public GEDCOMRecordType RecordType
        {
            get { return this.fRecordType; }
        }

        public GEDCOMList<GEDCOMSourceCitation> SourceCitations
        {
            get { return this.fSourceCitations; }
        }

        public string UID
        {
            get { return base.GetTagStringValue("_UID"); }
            set { base.SetTagStringValue("_UID", value); }
        }

        public GEDCOMList<GEDCOMUserReference> UserReferences
        {
            get { return this.fUserReferences; }
        }

        private static string CreateUID()
        {
            string result = "";
            byte checkA = 0;
            byte checkB = 0;

            byte[] binary = Guid.NewGuid().ToByteArray();
            int num = binary.Length;
            for (int i = 0; i < num; i++)
            {
                byte val = binary[i];
                checkA = unchecked((byte)((uint)checkA + (uint)val));
                checkB = unchecked((byte)((uint)checkB + (uint)checkA));
                result += string.Format("{0:X2}", val);
            }

            result += string.Format("{0:X2}", checkA);
            result += string.Format("{0:X2}", checkB);
            return result;
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.fRecordType = GEDCOMRecordType.rtNone;

            this.fNotes = new GEDCOMList<GEDCOMNotes>(this);
            this.fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
            this.fMultimediaLinks = new GEDCOMList<GEDCOMMultimediaLink>(this);
            this.fUserReferences = new GEDCOMList<GEDCOMUserReference>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fNotes.Dispose();
                this.fSourceCitations.Dispose();
                this.fMultimediaLinks.Dispose();
                this.fUserReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        protected void SetRecordType(GEDCOMRecordType type)
        {
            this.fRecordType = type;
        }

        public int IndexOfSource(GEDCOMSourceRecord sourceRec)
        {
            if (sourceRec != null) {
                int num = this.fSourceCitations.Count;
                for (int i = 0; i < num; i++)
                {
                    if (this.fSourceCitations[i].XRef == sourceRec.XRef) {
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
                GEDCOMNotes copy = (GEDCOMNotes)GEDCOMNotes.Create(this.Owner, this, "", "");
                copy.Assign(sourceNote);
                this.Notes.Add(copy);
            }

            foreach (GEDCOMMultimediaLink sourceMediaLink in sourceRec.fMultimediaLinks)
            {
                GEDCOMMultimediaLink copy = (GEDCOMMultimediaLink)GEDCOMMultimediaLink.Create(this.Owner, this, "", "");
                copy.Assign(sourceMediaLink);
                this.MultimediaLinks.Add(copy);
            }

            foreach (GEDCOMSourceCitation sourceSrcCit in sourceRec.fSourceCitations)
            {
                GEDCOMSourceCitation copy = (GEDCOMSourceCitation)GEDCOMSourceCitation.Create(this.Owner, this, "", "");
                copy.Assign(sourceSrcCit);
                this.SourceCitations.Add(copy);
            }

            foreach (GEDCOMUserReference sourceUserRef in sourceRec.fUserReferences)
            {
                GEDCOMUserReference copy = (GEDCOMUserReference)GEDCOMUserReference.Create(this.Owner, this, "", "");
                copy.Assign(sourceUserRef);
                this.UserReferences.Add(copy);
            }
        }

        public virtual void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            if (clearDest)
            {
                targetRecord.Clear();
            }

            if (this.fTags != null)
            {
                while (this.fTags.Count > 0)
                {
                    GEDCOMTag tag = this.fTags.Extract(0);
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

            while (this.fNotes.Count > 0)
            {
                GEDCOMTag tag = this.fNotes.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.Notes.Add(tag as GEDCOMNotes);
            }

            while (this.fMultimediaLinks.Count > 0)
            {
                GEDCOMTag tag = this.fMultimediaLinks.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.MultimediaLinks.Add(tag as GEDCOMMultimediaLink);
            }

            while (this.fSourceCitations.Count > 0)
            {
                GEDCOMTag tag = this.fSourceCitations.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.SourceCitations.Add(tag as GEDCOMSourceCitation);
            }

            while (this.fUserReferences.Count > 0)
            {
                GEDCOMTag tag = this.fUserReferences.Extract(0);
                tag.ResetParent(targetRecord);
                targetRecord.UserReferences.Add(tag as GEDCOMUserReference);
            }
        }

        public override void Pack()
        {
            base.Pack();

            this.fNotes.Pack();
            this.fSourceCitations.Pack();
            this.fMultimediaLinks.Pack();
            this.fUserReferences.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            this.fNotes.ReplaceXRefs(map);
            this.fSourceCitations.ReplaceXRefs(map);
            this.fMultimediaLinks.ReplaceXRefs(map);
            this.fUserReferences.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);

            this.fNotes.ResetOwner(newOwner);
            this.fSourceCitations.ResetOwner(newOwner);
            this.fMultimediaLinks.ResetOwner(newOwner);
            this.fUserReferences.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            this.fNotes.SaveToStream(stream);
            this.fSourceCitations.SaveToStream(stream);
            this.fMultimediaLinks.SaveToStream(stream);
            this.fUserReferences.SaveToStream(stream);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "CHAN")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMChangeDate.Create);
            }
            else if (tagName == "NOTE")
            {
                result = this.fNotes.Add(new GEDCOMNotes(base.Owner, this, tagName, tagValue));
            }
            else if (tagName == "SOUR")
            {
                result = this.fSourceCitations.Add(new GEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
            }
            else if (tagName == "OBJE")
            {
                result = this.fMultimediaLinks.Add(new GEDCOMMultimediaLink(base.Owner, this, tagName, tagValue));
            }
            else if (tagName == "REFN")
            {
                result = this.fUserReferences.Add(new GEDCOMUserReference(base.Owner, this, tagName, tagValue));
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();

            this.fNotes.Clear();
            this.fSourceCitations.Clear();
            this.fMultimediaLinks.Clear();
            this.fUserReferences.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fNotes.Count == 0 && this.fSourceCitations.Count == 0 && this.fMultimediaLinks.Count == 0 && this.fUserReferences.Count == 0;
        }

        public string NewXRef()
        {
            if (this.Owner != null)
            {
                string newXRef = this.Owner.XRefIndex_NewXRef(this);
                this.XRef = newXRef;
            }
            return this.XRef;
        }

        public void RequireUID()
        {
            if (string.IsNullOrEmpty(this.UID))
            {
                this.UID = CreateUID();
            }
        }

        public void InitNew()
        {
            this.NewXRef();
            this.RequireUID();
        }

        protected GEDCOMRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        #region Auxiliary

        public string GetXRefNum()
        {
            string xref = this.XRef;

            int i = 0;
            int last = xref.Length - 1;
            while (i <= last && (xref[i] < '0' || xref[i] > '9')) i++;
            xref = ((i <= last) ? xref.Substring(i) : "");
            return xref;
        }

        public int GetId()
        {
            int result;
            try
            {
                string xref = this.GetXRefNum();
                result = ConvHelper.ParseInt(xref, 0);
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
                note = new GEDCOMNotes(this.Owner, this, "", "");
                note.Value = noteRec;
                this.Notes.Add(note);
            }

            return note;
        }

        public GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality)
        {
            GEDCOMSourceCitation cit = null;

            if (sourceRec != null) {
                cit = new GEDCOMSourceCitation(this.Owner, this, "", "");
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                this.SourceCitations.Add(cit);
            }

            return cit;
        }

        public GEDCOMMultimediaLink AddMultimedia(GEDCOMMultimediaRecord mediaRec)
        {
            GEDCOMMultimediaLink mmLink = null;

            if (mediaRec != null) {
                mmLink = new GEDCOMMultimediaLink(this.Owner, this, "", "");
                mmLink.Value = mediaRec;
                this.MultimediaLinks.Add(mmLink);
            }

            return mmLink;
        }

        public void AddUserRef(string reference)
        {
            GEDCOMUserReference uRef = new GEDCOMUserReference(this.Owner, this, "", "");
            uRef.StringValue = reference;
            this.UserReferences.Add(uRef);
        }

        #endregion
    }
}
