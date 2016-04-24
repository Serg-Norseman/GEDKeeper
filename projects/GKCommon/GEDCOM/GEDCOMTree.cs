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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMTree : GEDCOMObject
    {
        #region Tree Enumerator

        private struct TreeEnumerator : IGEDCOMTreeEnumerator
        {
            private readonly GEDCOMTree fTree;
            private readonly GEDCOMRecordType fRecType;
            private readonly int fEndIndex;
            private int fIndex;

            public TreeEnumerator(GEDCOMTree tree, GEDCOMRecordType recType)
            {
                this.fTree = tree;
                this.fIndex = -1;
                this.fEndIndex = tree.RecordsCount - 1;
                this.fRecType = recType;
            }

            public bool MoveNext(out GEDCOMRecord current)
            {
                if (this.fRecType == GEDCOMRecordType.rtNone)
                {
                    if (this.fIndex < this.fEndIndex)
                    {
                        this.fIndex++;
                        current = this.fTree[this.fIndex];
                        return true;
                    }
                } else {
                    while (this.fIndex < this.fEndIndex)
                    {
                        this.fIndex++;
                        GEDCOMRecord rec = this.fTree[this.fIndex];
                        if (rec.RecordType == this.fRecType) {
                            current = rec;
                            return true;
                        }
                    }
                }

                this.fIndex = this.fEndIndex + 1;
                current = null;
                return false;
            }

            public void Reset()
            {
                this.fIndex = -1;
            }
        }

        #endregion


        private readonly GEDCOMHeader fHeader;
        private readonly GEDCOMList<GEDCOMRecord> fRecords;
        private readonly Hashtable fXRefIndex;
        
        private string fFileName;
        private ProgressEventHandler fOnProgressEvent;
        private GEDCOMState fState;


        public string FileName
        {
            get { return this.fFileName; }
            //set { this.fFileName = value; }
        }

        public event ProgressEventHandler OnProgress
        {
            add {
                this.fOnProgressEvent = value;
            }
            remove {
                if (this.fOnProgressEvent == value) {
                    this.fOnProgressEvent = null;
                }
            }
        }

        public int RecordsCount
        {
            get { return this.fRecords.Count; }
        }

        public GEDCOMRecord this[int index]
        {
            get { return this.fRecords[index]; }
        }

        public GEDCOMHeader Header
        {
            get { return this.fHeader; }
        }

        public GEDCOMState State
        {
            get { return this.fState; }
            set { this.fState = value; }
        }

        public GEDCOMTree()
        {
            this.fRecords = new GEDCOMList<GEDCOMRecord>(this);
            this.fHeader = new GEDCOMHeader(this, this, "", "");
            this.fXRefIndex = new Hashtable();
            this.fFileName = "";
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //this.fXRefIndex.Dispose();
                this.fHeader.Dispose();
                this.fRecords.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Internal

        static GEDCOMTree()
        {
            GEDCOMFactory f = GEDCOMFactory.GetInstance();

            f.RegisterTag("DATE", GEDCOMDateValue.Create);
            f.RegisterTag("TIME", GEDCOMTime.Create);
            f.RegisterTag("ADDR", GEDCOMAddress.Create);
            f.RegisterTag("PLAC", GEDCOMPlace.Create);
            f.RegisterTag("MAP", GEDCOMMap.Create);
            f.RegisterTag("_LOC", GEDCOMPointer.Create);

            //f.RegisterTag("xxxx", xxxx.Create);
        }

        private static string StrToUtf8(string str)
        {
            byte[] src = Encoding.GetEncoding(1251).GetBytes(str);
            return Encoding.UTF8.GetString(src);
        }

        private static string GetSignByRecord(GEDCOMRecord record)
        {
            string result = "";

            if (record != null)
            {
                switch (record.RecordType) {
                    case GEDCOMRecordType.rtIndividual:
                        result = "I";
                        break;
                    case GEDCOMRecordType.rtFamily:
                        result = "F";
                        break;
                    case GEDCOMRecordType.rtNote:
                        result = "N";
                        break;
                    case GEDCOMRecordType.rtMultimedia:
                        result = "O";
                        break;
                    case GEDCOMRecordType.rtSource:
                        result = "S";
                        break;
                    case GEDCOMRecordType.rtRepository:
                        result = "R";
                        break;
                    case GEDCOMRecordType.rtGroup:
                        result = "G";
                        break;
                    case GEDCOMRecordType.rtResearch:
                        result = "RS";
                        break;
                    case GEDCOMRecordType.rtTask:
                        result = "TK";
                        break;
                    case GEDCOMRecordType.rtCommunication:
                        result = "CM";
                        break;
                    case GEDCOMRecordType.rtLocation:
                        result = "L";
                        break;
                    case GEDCOMRecordType.rtSubmission:
                        result = "????";
                        break;
                    case GEDCOMRecordType.rtSubmitter:
                        result = "SUB";
                        break;
                }
            }

            return result;
        }

        #endregion

        #region XRef Search

        private void XRefIndex_Clear()
        {
            this.fXRefIndex.Clear();
        }

        private void XRefIndex_AddRecord(GEDCOMCustomRecord record)
        {
            if (record != null && !string.IsNullOrEmpty(record.XRef))
            {
                bool exists = this.fXRefIndex.ContainsKey(record.XRef);
                if (!exists) this.fXRefIndex.Add(record.XRef, record);
            }
        }

        public void SetXRef(string oldXRef, GEDCOMCustomRecord sender)
        {
            if (!string.IsNullOrEmpty(oldXRef))
            {
                bool exists = this.fXRefIndex.ContainsKey(oldXRef);
                if (exists) this.fXRefIndex.Remove(oldXRef);
            }

            this.XRefIndex_AddRecord(sender);
        }

        private void XRefIndex_DeleteRecord(GEDCOMRecord record)
        {
            bool exists = this.fXRefIndex.ContainsKey(record.XRef);
            if (exists) this.fXRefIndex.Remove(record.XRef);
        }

        public GEDCOMRecord XRefIndex_Find(string xref)
        {
            return (this.fXRefIndex[xref] as GEDCOMRecord);
        }

        public string XRefIndex_NewXRef(GEDCOMRecord sender)
        {
            string sign = GetSignByRecord(sender);
            int I = 1;
            while (this.fXRefIndex.ContainsKey(sign + I.ToString()))
            {
                I++;
            }
            return sign + I.ToString();
        }

        #endregion

        #region Main functionality

        public IGEDCOMTreeEnumerator GetEnumerator(GEDCOMRecordType recType)
        {
            return new TreeEnumerator(this, recType);
        }

        public GEDCOMRecord AddRecord(GEDCOMRecord record)
        {
            this.fRecords.Add(record);
            this.XRefIndex_AddRecord(record);
            return record;
        }

        public void Clear()
        {
            this.fRecords.Clear();
            this.fHeader.Clear();
            this.XRefIndex_Clear();
        }

        public void Delete(int index)
        {
            this.XRefIndex_DeleteRecord(this.fRecords[index]);
            this.fRecords.DeleteAt(index);
        }

        public void DeleteRecord(GEDCOMRecord sender)
        {
            this.XRefIndex_DeleteRecord(sender);
            this.fRecords.Delete(sender);
        }

        public GEDCOMRecord Extract(int index)
        {
            this.XRefIndex_DeleteRecord(this.fRecords[index]);
            return this.fRecords.Extract(index);
        }

        public int IndexOfRecord(GEDCOMRecord record)
        {
            return this.fRecords.IndexOf(record);
        }

        public void Pack()
        {
            int num = this.fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                this.fRecords[i].Pack();
            }
        }

        public GEDCOMRecord FindUID(string uid)
        {
            int num = this.fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this.fRecords[i];
                if (rec.UID == uid) {
                    return rec;
                }
            }

            return null;
        }

        #endregion

        #region Load/Save

        public void SetFileName(string fileName)
        {
            this.fFileName = fileName;
        }

        public void LoadFromFile(string fileName)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                this.LoadFromStreamExt(fileStream, fileStream, fileName);
            }
        }

        public void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write))
            {
                this.SaveToStreamExt(fileStream, fileName, charSet);
            }
        }

        public void LoadFromStreamExt(Stream fileStream, Stream inputStream, string fileName)
        {
            this.fFileName = fileName;

            using (StreamReader reader = new StreamReader(inputStream, Encoding.GetEncoding(1251))) {
                this.Clear();
                this.LoadFromStream(fileStream, reader);
                this.fHeader.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }

        public void SaveToStreamExt(Stream outputStream, string fileName, GEDCOMCharacterSet charSet)
        {
            this.fFileName = fileName;

            string subm = this.fHeader.GetTagStringValue("SUBM");
            int rev = this.fHeader.FileRevision;

            this.fHeader.Clear();
            this.fHeader.Source = "GEDKeeper";
            this.fHeader.ReceivingSystemName = "GEDKeeper";
            this.fHeader.CharacterSet = charSet;
            this.fHeader.Language = "Russian";
            this.fHeader.GEDCOMVersion = "5.5";
            this.fHeader.GEDCOMForm = "LINEAGE-LINKED";
            this.fHeader.FileName = Path.GetFileName(fileName);
            this.fHeader.TransmissionDateTime = DateTime.Now;
            this.fHeader.FileRevision = rev + 1;

            if (subm != "") {
                this.fHeader.SetTagStringValue("SUBM", subm);
            }

            this.Pack();

            using (StreamWriter writer = new StreamWriter(outputStream, GEDCOMUtils.GetEncodingByCharacterSet(charSet))) {
                this.SaveToStream(writer);
                writer.Flush();
            }

            this.fHeader.CharacterSet = GEDCOMCharacterSet.csASCII;
        }

        private static void CorrectLine(GEDCOMCustomRecord curRecord, GEDCOMTag curTag, int lineNum, string str)
        {
            try
            {
                if (curTag is GEDCOMNotes) {
                    curTag.AddTag("CONT", str, null);
                } else {
                    if (curRecord != null) {
                        curRecord.AddTag("NOTE", str, null);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GEDCOMTree.CorrectLine(): Line " + lineNum.ToString() + " failed correct: " + ex.Message);
            }
        }

        private void LoadFromStream(Stream fileStream, StreamReader reader)
        {
            long fileSize = fileStream.Length;
            int progress = 0;

            this.fState = GEDCOMState.osLoading;
            try
            {
                GEDCOMCustomRecord curRecord = null;
                GEDCOMTag curTag = null;
                GEDCOMCharacterSet charSet = GEDCOMCharacterSet.csASCII;

                int lineNum = 0;
                while (reader.Peek() != -1)
                {
                    lineNum++;
                    string srcLine = reader.ReadLine();
                    string str = GEDCOMUtils.TrimLeft(srcLine);

                    if (str.Length != 0)
                    {
                        if (!GEDCOMUtils.IsDigit(str[0]))
                        {
                            CorrectLine(curRecord, curTag, lineNum, str);
                        }
                        else
                        {
                            int tagLevel;
                            string tagXRef;
                            string tagName;
                            string tagValue;

                            try
                            {
                                str = GEDCOMUtils.ExtractNumber(str, out tagLevel, false, 0);
                                str = GEDCOMUtils.ExtractDelimiter(str, 0);
                                str = GEDCOMUtils.ExtractXRef(str, out tagXRef, true, "");
                                str = GEDCOMUtils.ExtractDelimiter(str, 0);
                                str = GEDCOMUtils.ExtractString(str, out tagName, "");
                                tagName = tagName.ToUpperInvariant();
                                str = GEDCOMUtils.ExtractDelimiter(str, 1);
                                tagValue = str;
                            }
                            catch (EGEDCOMException ex)
                            {
                                throw new EGEDCOMException("Syntax error in line " + Convert.ToString(lineNum) + ".\r" + ex.Message);
                            }

                            // temp hack
                            if (!string.IsNullOrEmpty(tagValue) && charSet == GEDCOMCharacterSet.csUTF8)
                            {
                                if (!Equals(reader.CurrentEncoding, Encoding.UTF8)) {
                                    tagValue = StrToUtf8(tagValue);
                                }
                            }
                            // end

                            if (tagLevel == 0)
                            {
                                if (tagName == "INDI")
                                {
                                    curRecord = this.AddRecord(new GEDCOMIndividualRecord(this, this, "", ""));
                                }
                                else if (tagName == "FAM")
                                {
                                    curRecord = this.AddRecord(new GEDCOMFamilyRecord(this, this, "", ""));
                                }
                                else if (tagName == "OBJE")
                                {
                                    curRecord = this.AddRecord(new GEDCOMMultimediaRecord(this, this, "", ""));
                                }
                                else if (tagName == "NOTE")
                                {
                                    curRecord = this.AddRecord(new GEDCOMNoteRecord(this, this, "", ""));
                                }
                                else if (tagName == "REPO")
                                {
                                    curRecord = this.AddRecord(new GEDCOMRepositoryRecord(this, this, "", ""));
                                }
                                else if (tagName == "SOUR")
                                {
                                    curRecord = this.AddRecord(new GEDCOMSourceRecord(this, this, "", ""));
                                }
                                else if (tagName == "SUBN")
                                {
                                    curRecord = this.AddRecord(new GEDCOMSubmissionRecord(this, this, "", ""));
                                }
                                else if (tagName == "SUBM")
                                {
                                    curRecord = this.AddRecord(new GEDCOMSubmitterRecord(this, this, "", ""));
                                }
                                else if (tagName == "_GROUP")
                                {
                                    curRecord = this.AddRecord(new GEDCOMGroupRecord(this, this, "", ""));
                                }
                                else if (tagName == "_RESEARCH")
                                {
                                    curRecord = this.AddRecord(new GEDCOMResearchRecord(this, this, "", ""));
                                }
                                else if (tagName == "_TASK")
                                {
                                    curRecord = this.AddRecord(new GEDCOMTaskRecord(this, this, "", ""));
                                }
                                else if (tagName == "_COMM")
                                {
                                    curRecord = this.AddRecord(new GEDCOMCommunicationRecord(this, this, "", ""));
                                }
                                else if (tagName == "_LOC")
                                {
                                    curRecord = this.AddRecord(new GEDCOMLocationRecord(this, this, "", ""));
                                }
                                else if (tagName == "HEAD")
                                {
                                    curRecord = this.fHeader;
                                }
                                else if (tagName == "TRLR")
                                {
                                    break;
                                }
                                else
                                {
                                    curRecord = null;
                                }

                                if (curRecord != null && tagXRef != "")
                                {
                                    curRecord.XRef = tagXRef;
                                }
                                curTag = null;
                            }
                            else
                            {
                                // temp hack
                                if (tagName == "CHAR") {
                                    charSet = GEDCOMUtils.GetCharacterSetVal(tagValue);
                                }
                                // end

                                if (curRecord != null)
                                {
                                    if (curTag == null || tagLevel == 1)
                                    {
                                        curTag = curRecord.AddTag(tagName, tagValue, null);
                                    }
                                    else
                                    {
                                        while (tagLevel <= curTag.Level)
                                        {
                                            curTag = (curTag.Parent as GEDCOMTag);
                                        }
                                        curTag = curTag.AddTag(tagName, tagValue, null);
                                    }
                                }
                            }
                        }
                    }

                    if (fOnProgressEvent != null) {
                        int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);

                        if (progress != newProgress) {
                            progress = newProgress;
                            fOnProgressEvent(this, progress);
                        }
                    }
                }
            }
            finally
            {
                this.fState = GEDCOMState.osReady;
            }
        }

        private void SaveToStream(StreamWriter writer)
        {
            this.SaveHeaderToStream(writer);

            int num = this.fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                this.fRecords[i].SaveToStream(writer);
            }

            SaveFooterToStream(writer);
        }

        public void SaveToStream(StreamWriter writer, List<GEDCOMRecord> list)
        {
            this.SaveHeaderToStream(writer);

            if (list != null)
            {
                int num = list.Count;
                for (int i = 0; i < num; i++)
                {
                    list[i].SaveToStream(writer);
                }
            }

            SaveFooterToStream(writer);
        }

        private void SaveHeaderToStream(StreamWriter stream)
        {
            this.fHeader.SaveToStream(stream);
        }

        private static void SaveFooterToStream(StreamWriter stream)
        {
            const string str = "0 TRLR";
            stream.WriteLine(str);
        }

        #endregion

        #region Auxiliary

        public GEDCOMFormat GetGEDCOMFormat()
        {
            string sour = this.fHeader.Source;

            for (GEDCOMFormat gf = GEDCOMFormat.gf_Native; gf <= GEDCOMFormat.gf_Last; gf++)
            {
                if (GEDCOMConsts.GEDCOMFormats[(int)gf].Sign == sour)
                {
                    return gf;
                }
            }
            
            return GEDCOMFormat.gf_Unknown;
        }

        public GEDCOMSubmitterRecord GetSubmitter()
        {
            GEDCOMSubmitterRecord submitter = this.fHeader.Submitter.Value as GEDCOMSubmitterRecord;
            if (submitter == null)
            {
                submitter = new GEDCOMSubmitterRecord(this, this, "", "");
                submitter.InitNew();
                this.AddRecord(submitter);
                this.fHeader.SetTagStringValue("SUBM", "@" + submitter.XRef + "@");
            }
            return submitter;
        }

        public GEDCOMIndividualRecord CreateIndividual()
        {
            GEDCOMIndividualRecord result = new GEDCOMIndividualRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            this.AddRecord(result);

            return result;
        }

        public GEDCOMIndividualRecord CreateIndividual(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex)
        {
            GEDCOMIndividualRecord result = this.CreateIndividual();

            result.Sex = iSex;
            GEDCOMPersonalName pn = new GEDCOMPersonalName(this, result, "", "");
            pn.StringValue = iName.Trim() + " " + iPatronymic.Trim() + " /" + iSurname.Trim() + "/";
            result.AddPersonalName(pn);

            return result;
        }

        public GEDCOMFamilyRecord CreateFamily()
        {
            GEDCOMFamilyRecord result = new GEDCOMFamilyRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            this.AddRecord(result);

            return result;
        }

        public GEDCOMNoteRecord CreateNote()
        {
            GEDCOMNoteRecord result = new GEDCOMNoteRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            this.AddRecord(result);

            return result;
        }

        public GEDCOMNoteRecord CreateNoteEx(GEDCOMRecord toRecord, string text)
        {
            GEDCOMNoteRecord result = null;

            if (toRecord != null && !string.IsNullOrEmpty(text)) {
                result = this.CreateNote();
                result.AddNoteText(text);
                toRecord.AddNote(result);
            }

            return result;
        }

        public GEDCOMNoteRecord CreateNoteEx(GEDCOMRecord toRecord, StringList text)
        {
            GEDCOMNoteRecord result = null;

            if (text != null) {
                result = this.CreateNote();
                result.Note = text;
            }

            if (toRecord != null && result != null) {
                toRecord.AddNote(result);
            }
            
            return result;
        }

        public GEDCOMSourceRecord CreateSource()
        {
            GEDCOMSourceRecord result = new GEDCOMSourceRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            this.AddRecord(result);

            return result;
        }

        public GEDCOMGroupRecord CreateGroup()
        {
            GEDCOMGroupRecord result = new GEDCOMGroupRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            this.AddRecord(result);

            return result;
        }

        //

        public bool DeleteIndividualRecord(GEDCOMIndividualRecord iRec)
        {
            bool result = false;
            if (iRec != null)
            {
                GEDCOMUtils.CleanIndividual(iRec);

                this.Delete(this.IndexOfRecord(iRec));
                result = true;
            }
            return result;
        }

        public bool DeleteFamilyRecord(GEDCOMFamilyRecord famRec)
        {
            bool result = false;
            if (famRec != null)
            {
                GEDCOMUtils.CleanFamily(famRec);

                this.Delete(this.IndexOfRecord(famRec));
                result = true;
            }
            return result;
        }

        public bool DeleteGroupRecord(GEDCOMGroupRecord groupRec)
        {
            bool result = false;
            if (groupRec != null)
            {
                for (int i = groupRec.Members.Count - 1; i >= 0; i--)
                {
                    GEDCOMIndividualRecord member = groupRec.Members[i].Value as GEDCOMIndividualRecord;
                    groupRec.RemoveMember(member);
                }

                this.Delete(this.IndexOfRecord(groupRec));
                result = true;
            }
            return result;
        }

        public bool DeleteMediaRecord(GEDCOMMultimediaRecord mRec)
        {
            bool result = false;
            if (mRec != null)
            {
                int num = this.RecordsCount - 1;
                for (int i = 0; i <= num; i++)
                {
                    GEDCOMRecord rec = this[i];
                    for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--)
                    {
                        if (rec.MultimediaLinks[j].Value == mRec)
                        {
                            rec.MultimediaLinks.DeleteAt(j);
                        }
                    }
                }

                this.Delete(this.IndexOfRecord(mRec));
                result = true;
            }
            return result;
        }

        public bool DeleteNoteRecord(GEDCOMNoteRecord nRec)
        {
            bool result = false;
            if (nRec != null)
            {
                int num = this.RecordsCount - 1;
                for (int i = 0; i <= num; i++)
                {
                    GEDCOMRecord rec = this[i];
                    for (int j = rec.Notes.Count - 1; j >= 0; j--)
                    {
                        if (rec.Notes[j].Value == nRec)
                            rec.Notes.DeleteAt(j);
                    }
                }

                this.Delete(this.IndexOfRecord(nRec));
                result = true;
            }
            return result;
        }

        public bool DeleteRepositoryRecord(GEDCOMRepositoryRecord repRec)
        {
            bool result = false;
            if (repRec != null)
            {
                int num = this.RecordsCount - 1;
                for (int i = 0; i <= num; i++)
                {
                    GEDCOMRecord rec = this[i];
                    if (rec is GEDCOMSourceRecord)
                    {
                        GEDCOMSourceRecord srcRec = rec as GEDCOMSourceRecord;
                        for (int j = srcRec.RepositoryCitations.Count - 1; j >= 0; j--)
                        {
                            if (srcRec.RepositoryCitations[j].Value == repRec)
                            {
                                srcRec.RepositoryCitations.Delete(srcRec.RepositoryCitations[j]);
                            }
                        }
                    }
                }

                this.Delete(this.IndexOfRecord(repRec));
                result = true;
            }
            return result;
        }

        public bool DeleteResearchRecord(GEDCOMResearchRecord resRec)
        {
            bool result = false;
            if (resRec != null)
            {
                this.Delete(this.IndexOfRecord(resRec));
                result = true;
            }
            return result;
        }

        public bool DeleteSourceRecord(GEDCOMSourceRecord srcRec)
        {
            bool result = false;
            if (srcRec != null)
            {
                int num = this.RecordsCount - 1;
                for (int i = 0; i <= num; i++)
                {
                    GEDCOMRecord rec = this[i];
                    for (int j = rec.SourceCitations.Count - 1; j >= 0; j--)
                    {
                        if (rec.SourceCitations[j].Value == srcRec)
                        {
                            rec.SourceCitations.DeleteAt(j);
                        }
                    }
                }
                
                this.Delete(this.IndexOfRecord(srcRec));
                result = true;
            }
            return result;
        }

        public bool DeleteTaskRecord(GEDCOMTaskRecord taskRec)
        {
            bool result = false;
            if (taskRec != null)
            {
                int num = this.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = this[i];
                    if (rec is GEDCOMResearchRecord)
                    {
                        GEDCOMResearchRecord resRec = rec as GEDCOMResearchRecord;
                        for (int j = resRec.Tasks.Count - 1; j >= 0; j--)
                        {
                            if (resRec.Tasks[j].Value == taskRec)
                            {
                                resRec.Tasks.DeleteAt(j);
                            }
                        }
                    }
                }

                this.Delete(this.IndexOfRecord(taskRec));
                result = true;
            }
            return result;
        }

        public bool DeleteCommunicationRecord(GEDCOMCommunicationRecord commRec)
        {
            bool result = false;
            if (commRec != null)
            {
                int num = this.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = this[i];
                    if (rec is GEDCOMResearchRecord)
                    {
                        GEDCOMResearchRecord resRec = rec as GEDCOMResearchRecord;
                        for (int j = resRec.Communications.Count - 1; j >= 0; j--)
                        {
                            if (resRec.Communications[j].Value == commRec)
                            {
                                resRec.Communications.DeleteAt(j);
                            }
                        }
                    }
                }

                this.Delete(this.IndexOfRecord(commRec));
                result = true;
            }
            return result;
        }

        public bool DeleteLocationRecord(GEDCOMLocationRecord locRec)
        {
            bool result = false;
            if (locRec != null)
            {
                int num = this.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = this[i];

                    if (rec is GEDCOMRecordWithEvents)
                    {
                        GEDCOMRecordWithEvents evsRec = rec as GEDCOMRecordWithEvents;

                        for (int j = evsRec.Events.Count - 1; j >= 0; j--)
                        {
                            GEDCOMCustomEvent ev = evsRec.Events[j];

                            if (ev.Detail.Place.Location.Value == locRec) {
                                ev.Detail.Place.DeleteTag("_LOC");
                            }
                        }
                    }
                }

                this.Delete(this.IndexOfRecord(locRec));
                result = true;
            }
            return result;
        }

        #endregion
    }
}
