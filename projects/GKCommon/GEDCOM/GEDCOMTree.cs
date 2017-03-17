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
                fTree = tree;
                fIndex = -1;
                fEndIndex = tree.RecordsCount - 1;
                fRecType = recType;
            }

            public bool MoveNext(out GEDCOMRecord current)
            {
                if (fRecType == GEDCOMRecordType.rtNone)
                {
                    if (fIndex < fEndIndex)
                    {
                        fIndex++;
                        current = fTree[fIndex];
                        return true;
                    }
                } else {
                    while (fIndex < fEndIndex)
                    {
                        fIndex++;
                        GEDCOMRecord rec = fTree[fIndex];
                        if (rec.RecordType == fRecType) {
                            current = rec;
                            return true;
                        }
                    }
                }

                fIndex = fEndIndex + 1;
                current = null;
                return false;
            }

            public void Reset()
            {
                fIndex = -1;
            }
        }

        #endregion


        private readonly GEDCOMHeader fHeader;
        private readonly GEDCOMList<GEDCOMRecord> fRecords;
        private readonly Dictionary<string, GEDCOMCustomRecord> fXRefIndex;
        
        private string fFileName;
        private EventHandler fOnChange;
        private EventHandler fOnChanging;
        private ProgressEventHandler fOnProgressEvent;
        private GEDCOMState fState;
        private int fUpdateCount;
        private GEDCOMFormat fFormat;


        public string FileName
        {
            get { return fFileName; }
        }

        public event ProgressEventHandler OnProgress
        {
            add {
                fOnProgressEvent = value;
            }
            remove {
                if (fOnProgressEvent == value) {
                    fOnProgressEvent = null;
                }
            }
        }

        public int RecordsCount
        {
            get { return fRecords.Count; }
        }

        public GEDCOMRecord this[int index]
        {
            get { return fRecords[index]; }
        }

        public GEDCOMHeader Header
        {
            get { return fHeader; }
        }

        public GEDCOMState State
        {
            get { return fState; }
            set { fState = value; }
        }

        public event EventHandler OnChange
        {
            add { fOnChange = value; }
            remove { if (fOnChange == value) fOnChange = null; }
        }

        public event EventHandler OnChanging
        {
            add { fOnChanging = value; }
            remove { if (fOnChanging == value) fOnChanging = null; }
        }


        public GEDCOMTree()
        {
            fRecords = new GEDCOMList<GEDCOMRecord>(this);
            fXRefIndex = new Dictionary<string, GEDCOMCustomRecord>();
            fHeader = new GEDCOMHeader(this, this, "", "");
            fFileName = "";
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //fXRefIndex.Dispose();
                fHeader.Dispose();
                fRecords.Dispose();
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
            f.RegisterTag("_POSITION", GEDCOMCutoutPosition.Create);
            f.RegisterTag("LANG", GEDCOMLanguage.Create);

            //f.RegisterTag("xxxx", xxxx.Create);
        }

        private static string GetSignByRecord(GEDCOMRecord record)
        {
            string result = "";
            if (record == null) return result;

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

            return result;
        }

        #endregion

        #region XRef Search

        private void XRefIndex_AddRecord(GEDCOMCustomRecord record)
        {
            if (record == null || string.IsNullOrEmpty(record.XRef)) return;

            bool exists = fXRefIndex.ContainsKey(record.XRef);
            if (!exists) fXRefIndex.Add(record.XRef, record);
        }

        private void XRefIndex_DeleteRecord(GEDCOMRecord record)
        {
            bool exists = fXRefIndex.ContainsKey(record.XRef);
            if (exists) fXRefIndex.Remove(record.XRef);
        }

        public GEDCOMRecord XRefIndex_Find(string xref)
        {
            if (string.IsNullOrEmpty(xref)) return null;

            GEDCOMCustomRecord record;
            if (fXRefIndex.TryGetValue(xref, out record)) {
                return (record as GEDCOMRecord);
            } else {
                return null;
            }
        }

        public string XRefIndex_NewXRef(GEDCOMRecord record)
        {
            string sign = GetSignByRecord(record);
            int I = 1;
            while (fXRefIndex.ContainsKey(sign + I.ToString()))
            {
                I++;
            }
            return sign + I.ToString();
        }

        public void SetXRef(string oldXRef, GEDCOMCustomRecord record)
        {
            if (!string.IsNullOrEmpty(oldXRef))
            {
                bool exists = fXRefIndex.ContainsKey(oldXRef);
                if (exists) fXRefIndex.Remove(oldXRef);
            }

            XRefIndex_AddRecord(record);
        }

        #endregion

        #region Main functionality

        public IGEDCOMTreeEnumerator GetEnumerator(GEDCOMRecordType recType)
        {
            return new TreeEnumerator(this, recType);
        }

        public void Clear()
        {
            fHeader.Clear();
            fRecords.Clear();
            fXRefIndex.Clear();
        }

        public GEDCOMRecord AddRecord(GEDCOMRecord record)
        {
            fRecords.Add(record);
            XRefIndex_AddRecord(record);
            return record;
        }

        /*public void Delete(int index)
        {
            XRefIndex_DeleteRecord(fRecords[index]);
            fRecords.DeleteAt(index);
        }*/

        public void DeleteRecord(GEDCOMRecord record)
        {
            XRefIndex_DeleteRecord(record);
            fRecords.Delete(record);
        }

        public GEDCOMRecord Extract(int index)
        {
            XRefIndex_DeleteRecord(fRecords[index]);
            return fRecords.Extract(index);
        }

        public int IndexOf(GEDCOMRecord record)
        {
            return fRecords.IndexOf(record);
        }

        public GEDCOMRecord FindUID(string uid)
        {
            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fRecords[i];
                if (rec.UID == uid) {
                    return rec;
                }
            }

            return null;
        }

        public void Pack()
        {
            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                fRecords[i].Pack();
            }
        }

        #endregion

        #region Load/Save

        public void SetFileName(string fileName)
        {
            fFileName = fileName;
        }

        public void LoadFromFile(string fileName)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromStreamExt(fileStream, fileStream, fileName);
            }
        }

        public void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            using (FileStream fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write))
            {
                SaveToStreamExt(fileStream, fileName, charSet);
            }
        }

        public void LoadFromStreamExt(Stream fileStream, Stream inputStream, string fileName)
        {
            fFileName = fileName;

            using (StreamReader reader = GEDCOMUtils.OpenStreamReader(inputStream, DEFAULT_ENCODING)) {
                Clear();
                LoadFromStream(fileStream, reader);
                fHeader.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }

        public void SaveToStreamExt(Stream outputStream, string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            fFileName = fileName;

            Pack();

            using (StreamWriter writer = new StreamWriter(outputStream, GEDCOMUtils.GetEncodingByCharacterSet(charSet))) {
                SaveToStream(writer);
                writer.Flush();
            }

            fHeader.CharacterSet = GEDCOMCharacterSet.csASCII;
        }

        #region Encoding hack

        private enum EncodingState { esUnchecked, esUnchanged, esChanged }

        private const int DEF_CODEPAGE = 1251;
        private static readonly Encoding DEFAULT_ENCODING = Encoding.GetEncoding(DEF_CODEPAGE);

        private static string ConvertStr(Encoding encoding, string str)
        {
            byte[] src = DEFAULT_ENCODING.GetBytes(str);
            str = encoding.GetString(src);
            return str;
        }

        private void DefineEncoding(StreamReader reader, ref Encoding sourceEncoding, ref EncodingState encodingState)
        {
            GEDCOMCharacterSet charSet = fHeader.CharacterSet;
            switch (charSet)
            {
                case GEDCOMCharacterSet.csUTF8:
                    if (!GEDCOMUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        sourceEncoding = Encoding.UTF8;
                        encodingState = EncodingState.esChanged; // file without BOM
                    } else {
                        encodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    if (!GEDCOMUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        sourceEncoding = Encoding.Unicode;
                        encodingState = EncodingState.esChanged; // file without BOM
                    } else {
                        encodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csASCII:
                    string cpVers = fHeader.CharacterSetVersion;
                    if (!string.IsNullOrEmpty(cpVers)) {
                        int sourceCodepage = SysUtils.ParseInt(cpVers, DEF_CODEPAGE);
                        sourceEncoding = Encoding.GetEncoding(sourceCodepage);
                        encodingState = EncodingState.esChanged;
                    } else {
                        sourceEncoding = Encoding.GetEncoding(DEF_CODEPAGE);
                        encodingState = EncodingState.esChanged;
                    }
                    break;
            }
        }

        #endregion

        private void LoadFromStream(Stream fileStream, StreamReader reader)
        {
            fState = GEDCOMState.osLoading;
            try
            {
                Encoding sourceEncoding = DEFAULT_ENCODING;
                EncodingState encodingState = EncodingState.esUnchecked;
                long fileSize = fileStream.Length;
                int progress = 0;

                GEDCOMCustomRecord curRecord = null;
                GEDCOMTag curTag = null;

                int lineNum = 0;
                while (reader.Peek() != -1)
                {
                    lineNum++;
                    string str = reader.ReadLine();
                    str = GEDCOMUtils.TrimLeft(str);
                    if (str.Length == 0) continue;

                    if (!GEDCOMUtils.IsDigit(str[0]))
                    {
                        GEDCOMUtils.FixFTBLine(curRecord, curTag, lineNum, str);
                    }
                    else
                    {
                        int tagLevel;
                        string tagXRef, tagName, tagValue;

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

                        // convert codepages
                        if (!string.IsNullOrEmpty(tagValue) && encodingState == EncodingState.esChanged)
                        {
                            tagValue = ConvertStr(sourceEncoding, tagValue);
                        }

                        if (tagLevel == 0)
                        {
                            if (curRecord == fHeader && encodingState == EncodingState.esUnchecked) {
                                // beginning recognition of the first is not header record
                                // to check for additional versions of the code page
                                DefineEncoding(reader, ref sourceEncoding, ref encodingState);
                            }

                            if (tagName == "INDI")
                            {
                                curRecord = AddRecord(new GEDCOMIndividualRecord(this, this, "", ""));
                            }
                            else if (tagName == "FAM")
                            {
                                curRecord = AddRecord(new GEDCOMFamilyRecord(this, this, "", ""));
                            }
                            else if (tagName == "OBJE")
                            {
                                curRecord = AddRecord(new GEDCOMMultimediaRecord(this, this, "", ""));
                            }
                            else if (tagName == "NOTE")
                            {
                                curRecord = AddRecord(new GEDCOMNoteRecord(this, this, "", ""));
                            }
                            else if (tagName == "REPO")
                            {
                                curRecord = AddRecord(new GEDCOMRepositoryRecord(this, this, "", ""));
                            }
                            else if (tagName == "SOUR")
                            {
                                curRecord = AddRecord(new GEDCOMSourceRecord(this, this, "", ""));
                            }
                            else if (tagName == "SUBN")
                            {
                                curRecord = AddRecord(new GEDCOMSubmissionRecord(this, this, "", ""));
                            }
                            else if (tagName == "SUBM")
                            {
                                curRecord = AddRecord(new GEDCOMSubmitterRecord(this, this, "", ""));
                            }
                            else if (tagName == "_GROUP")
                            {
                                curRecord = AddRecord(new GEDCOMGroupRecord(this, this, "", ""));
                            }
                            else if (tagName == "_RESEARCH")
                            {
                                curRecord = AddRecord(new GEDCOMResearchRecord(this, this, "", ""));
                            }
                            else if (tagName == "_TASK")
                            {
                                curRecord = AddRecord(new GEDCOMTaskRecord(this, this, "", ""));
                            }
                            else if (tagName == "_COMM")
                            {
                                curRecord = AddRecord(new GEDCOMCommunicationRecord(this, this, "", ""));
                            }
                            else if (tagName == "_LOC")
                            {
                                curRecord = AddRecord(new GEDCOMLocationRecord(this, this, "", ""));
                            }
                            else if (tagName == "HEAD")
                            {
                                curRecord = fHeader;
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
                fState = GEDCOMState.osReady;
            }
        }

        private void SaveToStream(StreamWriter writer)
        {
            SaveHeaderToStream(writer);

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                fRecords[i].SaveToStream(writer);
            }

            SaveFooterToStream(writer);
        }

        public void SaveToStream(StreamWriter writer, List<GEDCOMRecord> list)
        {
            SaveHeaderToStream(writer);

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
            fHeader.SaveToStream(stream);
        }

        private static void SaveFooterToStream(StreamWriter stream)
        {
            const string str = "0 TRLR";
            stream.Write(str + GEDCOM_NEWLINE);
        }

        #endregion

        #region Auxiliary

        public int[] GetRecordStats()
        {
            int[] stats = new int[((int)GEDCOMRecordType.rtLast)];

            int num = RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this[i];
                int index = (int)rec.RecordType;
                stats[index] += 1;
            }

            return stats;
        }

        public GEDCOMFormat GetGEDCOMFormat()
        {
            if (fFormat != GEDCOMFormat.gf_Unknown) return fFormat;

            string sour = fHeader.Source;

            for (GEDCOMFormat gf = GEDCOMFormat.gf_Native; gf <= GEDCOMFormat.gf_Last; gf++)
            {
                if (GEDCOMConsts.GEDCOMFormats[(int)gf].Sign == sour)
                {
                    fFormat = gf;
                    return gf;
                }
            }

            fFormat = GEDCOMFormat.gf_Unknown;
            return GEDCOMFormat.gf_Unknown;
        }

        public GEDCOMSubmitterRecord GetSubmitter()
        {
            GEDCOMSubmitterRecord submitter = fHeader.Submitter.Value as GEDCOMSubmitterRecord;
            if (submitter == null)
            {
                submitter = new GEDCOMSubmitterRecord(this, this, "", "");
                submitter.InitNew();
                AddRecord(submitter);
                fHeader.SetTagStringValue("SUBM", "@" + submitter.XRef + "@");
            }
            return submitter;
        }

        public GEDCOMIndividualRecord CreateIndividual()
        {
            GEDCOMIndividualRecord result = new GEDCOMIndividualRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMFamilyRecord CreateFamily()
        {
            GEDCOMFamilyRecord result = new GEDCOMFamilyRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMNoteRecord CreateNote()
        {
            GEDCOMNoteRecord result = new GEDCOMNoteRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMSourceRecord CreateSource()
        {
            GEDCOMSourceRecord result = new GEDCOMSourceRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMRepositoryRecord CreateRepository()
        {
            GEDCOMRepositoryRecord result = new GEDCOMRepositoryRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMResearchRecord CreateResearch()
        {
            GEDCOMResearchRecord result = new GEDCOMResearchRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMCommunicationRecord CreateCommunication()
        {
            GEDCOMCommunicationRecord result = new GEDCOMCommunicationRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMTaskRecord CreateTask()
        {
            GEDCOMTaskRecord result = new GEDCOMTaskRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMMultimediaRecord CreateMultimedia()
        {
            GEDCOMMultimediaRecord result = new GEDCOMMultimediaRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMLocationRecord CreateLocation()
        {
            GEDCOMLocationRecord result = new GEDCOMLocationRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        public GEDCOMGroupRecord CreateGroup()
        {
            GEDCOMGroupRecord result = new GEDCOMGroupRecord(this, this, "", "");
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);

            return result;
        }

        //

        public bool DeleteIndividualRecord(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return false;

            GEDCOMUtils.CleanIndividual(iRec);

            DeleteRecord(iRec);
            return true;
        }

        public bool DeleteFamilyRecord(GEDCOMFamilyRecord famRec)
        {
            if (famRec == null) return false;

            GEDCOMUtils.CleanFamily(famRec);

            DeleteRecord(famRec);
            return true;
        }

        public bool DeleteGroupRecord(GEDCOMGroupRecord groupRec)
        {
            if (groupRec == null) return false;

            for (int i = groupRec.Members.Count - 1; i >= 0; i--)
            {
                GEDCOMIndividualRecord member = groupRec.Members[i].Value as GEDCOMIndividualRecord;
                groupRec.RemoveMember(member);
            }

            DeleteRecord(groupRec);
            return true;
        }

        public bool DeleteMediaRecord(GEDCOMMultimediaRecord mRec)
        {
            if (mRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
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

            DeleteRecord(mRec);
            return true;
        }

        public bool DeleteNoteRecord(GEDCOMNoteRecord nRec)
        {
            if (nRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this[i];
                for (int j = rec.Notes.Count - 1; j >= 0; j--)
                {
                    if (rec.Notes[j].Value == nRec)
                        rec.Notes.DeleteAt(j);
                }
            }

            DeleteRecord(nRec);
            return true;
        }

        public bool DeleteRepositoryRecord(GEDCOMRepositoryRecord repRec)
        {
            if (repRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this[i];
                if (rec.RecordType == GEDCOMRecordType.rtSource)
                {
                    GEDCOMSourceRecord srcRec = (GEDCOMSourceRecord) rec;
                    for (int j = srcRec.RepositoryCitations.Count - 1; j >= 0; j--)
                    {
                        if (srcRec.RepositoryCitations[j].Value == repRec)
                        {
                            srcRec.RepositoryCitations.Delete(srcRec.RepositoryCitations[j]);
                        }
                    }
                }
            }

            DeleteRecord(repRec);
            return true;
        }

        public bool DeleteResearchRecord(GEDCOMResearchRecord resRec)
        {
            if (resRec == null) return false;

            DeleteRecord(resRec);
            return true;
        }

        public bool DeleteSourceRecord(GEDCOMSourceRecord srcRec)
        {
            if (srcRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
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

            DeleteRecord(srcRec);
            return true;
        }

        public bool DeleteTaskRecord(GEDCOMTaskRecord taskRec)
        {
            if (taskRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this[i];
                if (rec.RecordType == GEDCOMRecordType.rtResearch)
                {
                    GEDCOMResearchRecord resRec = (GEDCOMResearchRecord) rec;
                    for (int j = resRec.Tasks.Count - 1; j >= 0; j--)
                    {
                        if (resRec.Tasks[j].Value == taskRec)
                        {
                            resRec.Tasks.DeleteAt(j);
                        }
                    }
                }
            }

            DeleteRecord(taskRec);
            return true;
        }

        public bool DeleteCommunicationRecord(GEDCOMCommunicationRecord commRec)
        {
            if (commRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this[i];
                if (rec.RecordType == GEDCOMRecordType.rtResearch)
                {
                    GEDCOMResearchRecord resRec = (GEDCOMResearchRecord) rec;
                    for (int j = resRec.Communications.Count - 1; j >= 0; j--)
                    {
                        if (resRec.Communications[j].Value == commRec)
                        {
                            resRec.Communications.DeleteAt(j);
                        }
                    }
                }
            }

            DeleteRecord(commRec);
            return true;
        }

        public bool DeleteLocationRecord(GEDCOMLocationRecord locRec)
        {
            if (locRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this[i];

                if (rec is GEDCOMRecordWithEvents)
                {
                    GEDCOMRecordWithEvents evsRec = (GEDCOMRecordWithEvents) rec;

                    for (int j = evsRec.Events.Count - 1; j >= 0; j--)
                    {
                        GEDCOMCustomEvent ev = evsRec.Events[j];

                        if (ev.Detail.Place.Location.Value == locRec) {
                            ev.Detail.Place.DeleteTag("_LOC");
                        }
                    }
                }
            }

            DeleteRecord(locRec);
            return true;
        }

        #endregion

        #region Updating

        public bool IsUpdated()
        {
            return (fUpdateCount != 0);
        }

        public void BeginUpdate()
        {
            if (fUpdateCount == 0)
            {
                SetUpdateState(true);
            }
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;
            if (fUpdateCount == 0)
            {
                SetUpdateState(false);
            }
        }

        private void SetUpdateState(bool updating)
        {
            if (updating)
            {
                Changing();
            }
            else
            {
                Changed();
            }
        }

        private void Changed()
        {
            if (fUpdateCount == 0 && fOnChange != null)
            {
                fOnChange(this, new EventArgs());
            }
        }

        private void Changing()
        {
            if (fUpdateCount == 0 && fOnChanging != null)
            {
                fOnChanging(this, new EventArgs());
            }
        }

        #endregion
    }
}
