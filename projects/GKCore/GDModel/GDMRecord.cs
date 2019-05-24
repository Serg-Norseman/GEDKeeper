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
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMRecordType
    {
        rtNone,
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
    public class GDMRecord : GDMCustomRecord, IGEDCOMStructWithLists
    {
        private object fExtData;
        private GDMRecordType fRecordType;
        private string fUID;

        private GDMList<GDMMultimediaLink> fMultimediaLinks;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;
        private GDMList<GDMUserReference> fUserReferences;


        public string AutomatedRecordID
        {
            get { return GetTagStringValue(GEDCOMTagType.RIN); }
            set { SetTagStringValue(GEDCOMTagType.RIN, value); }
        }

        public GDMChangeDate ChangeDate
        {
            get { return GetTag<GDMChangeDate>(GEDCOMTagType.CHAN, GDMChangeDate.Create); }
        }

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
            get { return fRecordType; }
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


        public GDMRecord(GDMObject owner) : base(owner)
        {
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

        protected void SetRecordType(GDMRecordType type)
        {
            fRecordType = type;
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

            foreach (GDMNotes sourceNote in sourceRec.fNotes) {
                GDMNotes copy = new GDMNotes(this);
                copy.Assign(sourceNote);
                Notes.Add(copy);
            }

            foreach (GDMMultimediaLink sourceMediaLink in sourceRec.fMultimediaLinks) {
                GDMMultimediaLink copy = new GDMMultimediaLink(this);
                copy.Assign(sourceMediaLink);
                MultimediaLinks.Add(copy);
            }

            foreach (GDMSourceCitation sourceSrcCit in sourceRec.fSourceCitations) {
                GDMSourceCitation copy = new GDMSourceCitation(this);
                copy.Assign(sourceSrcCit);
                SourceCitations.Add(copy);
            }

            foreach (GDMUserReference sourceUserRef in sourceRec.fUserReferences) {
                GDMUserReference copy = new GDMUserReference(this);
                copy.Assign(sourceUserRef);
                UserReferences.Add(copy);
            }
        }

        public virtual void MoveTo(GDMRecord targetRecord, bool clearDest)
        {
            if (clearDest) {
                targetRecord.Clear();
            }

            var subtags = GetTagList();
            while (subtags.Count > 0) {
                GDMTag tag = subtags.Extract(0);
                if (tag.Name == GEDCOMTagType.CHAN && !clearDest) {
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

        public override void Pack()
        {
            base.Pack();

            fNotes.Pack();
            fSourceCitations.Pack();
            fMultimediaLinks.Pack();
            fUserReferences.Pack();
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

        public void InitNew()
        {
            NewXRef();
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

        public int GetId()
        {
            return GEDCOMUtils.GetXRefNumber(XRef);
        }

        public GDMNotes AddNote(GDMNoteRecord noteRec)
        {
            GDMNotes note = null;

            if (noteRec != null) {
                note = new GDMNotes(this);
                note.Value = noteRec;
                Notes.Add(note);
            }

            return note;
        }

        public GDMSourceCitation AddSource(GDMSourceRecord sourceRec, string page, int quality)
        {
            GDMSourceCitation cit = null;

            if (sourceRec != null) {
                cit = new GDMSourceCitation(this);
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                SourceCitations.Add(cit);
            }

            return cit;
        }

        public GDMMultimediaLink AddMultimedia(GDMMultimediaRecord mediaRec)
        {
            GDMMultimediaLink mmLink = null;

            if (mediaRec != null) {
                mmLink = new GDMMultimediaLink(this);
                mmLink.Value = mediaRec;
                MultimediaLinks.Add(mmLink);
            }

            return mmLink;
        }

        public void AddUserRef(string reference)
        {
            GDMUserReference uRef = new GDMUserReference(this);
            uRef.StringValue = reference;
            UserReferences.Add(uRef);
        }
    }
}
