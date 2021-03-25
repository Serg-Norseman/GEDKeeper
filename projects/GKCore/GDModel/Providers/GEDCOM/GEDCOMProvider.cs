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
using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using GKCore;

namespace GDModel.Providers.GEDCOM
{
    public sealed class GEDCOMAppFormat
    {
        public readonly GEDCOMFormat Format;
        public readonly string Sign;
        public readonly string Name;
        public readonly int PredefCharset;

        public GEDCOMAppFormat(GEDCOMFormat format, string sign, string name, int predefCharset)
        {
            Format = format;
            Sign = sign;
            Name = name;
            PredefCharset = predefCharset;
        }
    }


    public delegate GDMTag TagConstructor(GDMObject owner, int tagId, string tagValue);

    public delegate StackTuple AddTagHandler(GDMObject owner, int tagLevel, int tagId, string tagValue);

    public delegate bool SaveTagHandler(StreamWriter stream, int level, GDMTag tag);

    public sealed class StackTuple
    {
        public int Level;
        public GDMTag Tag;
        public AddTagHandler AddHandler;

        public StackTuple(int level, GDMTag tag, AddTagHandler addHandler)
        {
            Level = level;
            Tag = tag;
            AddHandler = (addHandler != null) ? addHandler : GEDCOMProvider.GetAddHandler(tag.Id);
        }
    }


    /// <summary>
    /// Processing the GEDCOM format is one part of the Genealogical Data Model (GDM).
    /// </summary>
    /// <remarks>
    /// This class has been heavily refactored under profiling. Any alterations must take into account the factor 
    /// of performance degradation when changing the approach, even in small things.
    /// </remarks>
    public class GEDCOMProvider : FileProvider
    {
        public static readonly GEDCOMAppFormat[] GEDCOMFormats;


        public GEDCOMProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return LangMan.LS(LSID.LSID_GEDCOMFilter);
        }

        #region Encoding routines

        private enum EncodingState { esUnchecked, esUnchanged, esChanged }

        private const int DEF_CODEPAGE = 437;
        private Encoding fDefaultEncoding;
        private Encoding fSourceEncoding;
        private EncodingState fEncodingState;

        private void SetEncoding(Encoding encoding)
        {
            fSourceEncoding = encoding;
            fEncodingState = (fDefaultEncoding.Equals(fSourceEncoding)) ? EncodingState.esUnchanged : EncodingState.esChanged;
        }

        private void DefineEncoding(StreamReader reader, GEDCOMFormat format, string streamCharset)
        {
            GEDCOMCharacterSet charSet = fTree.Header.CharacterSet.Value;
            switch (charSet)
            {
                case GEDCOMCharacterSet.csUTF8:
                    if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        SetEncoding(Encoding.UTF8); // file without BOM
                    } else {
                        fEncodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    if (format == GEDCOMFormat.gf_Geni) {
                        SetEncoding(Encoding.UTF8);
                    } else if (format == GEDCOMFormat.gf_GENJ) {
                        SetEncoding(Encoding.UTF8);
                    } else {
                        if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                            SetEncoding(Encoding.Unicode); // file without BOM
                        } else {
                            fEncodingState = EncodingState.esUnchanged;
                        }
                    }
                    break;

                case GEDCOMCharacterSet.csANSEL:
                    if (format == GEDCOMFormat.gf_ALTREE) {
                        // Agelong Tree 4.0 with ANSEL is actually characteristic 
                        // for the Russian-language data export
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else if (format == GEDCOMFormat.gf_Geni) {
                        SetEncoding(Encoding.UTF8);
                    } else {
                        SetEncoding(new AnselEncoding());
                    }
                    break;

                case GEDCOMCharacterSet.csASCII:
                    if (format == GEDCOMFormat.gf_Native) {
                        // GEDKeeper native format (old) and ASCII charset
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else {
                        var fmtProps = GetGEDCOMFormatProps(format);
                        if (fmtProps.PredefCharset > -1) {
                            SetEncoding(Encoding.GetEncoding(fmtProps.PredefCharset));
                        } else {
                            string cpVers = fTree.Header.CharacterSet.Version;
                            if (!string.IsNullOrEmpty(cpVers)) {
                                int sourceCodepage = ConvertHelper.ParseInt(cpVers, DEF_CODEPAGE);
                                SetEncoding(Encoding.GetEncoding(sourceCodepage));
                            } else {
                                if (fTree.Header.Language == GDMLanguageID.Russian) {
                                    SetEncoding(Encoding.GetEncoding(1251));
                                } else {
                                    if (streamCharset == null) {
                                        SetEncoding(Encoding.GetEncoding(DEF_CODEPAGE));
                                    } else {
                                        SetEncoding(Encoding.GetEncoding(streamCharset));
                                    }
                                }
                            }
                        }
                    }
                    break;
            }
        }

        protected override Encoding GetDefaultEncoding()
        {
            return Encoding.GetEncoding(DEF_CODEPAGE);
        }

        protected override string DetectCharset(Stream inputStream, bool charsetDetection)
        {
            string streamCharset = null;
            if (charsetDetection) {
                var charsetRes = GKUtils.DetectCharset(inputStream);
                if (charsetRes.Confidence >= 0.7f) {
                    streamCharset = charsetRes.Charset;
                }
            }
            return streamCharset;
        }

        #endregion

        #region Buffered read without excessive allocating memory

        private const int SB_SIZE = 32 * 1024;
        private const int LB_SIZE = 32 * 1024; // Fix for files from WikiTree, with lines above 3000 chars!

        private char[] fStreamBuffer, fLineBuffer;
        private int fStmBufLen, fStmBufPos, fLineBufPos;

        private void InitBuffers()
        {
            fStreamBuffer = new char[SB_SIZE];
            fLineBuffer = new char[LB_SIZE];
            fStmBufLen = 0;
            fStmBufPos = 0;
            fLineBufPos = 0;
        }

        private int ReadLine(StreamReader reader)
        {
            while (true) {
                if (fStmBufPos >= fStmBufLen) {
                    fStmBufLen = reader.Read(fStreamBuffer, 0, SB_SIZE);
                    if (fStmBufLen <= 0 && fLineBufPos <= 0) {
                        return -1; // eof, no more lines and no line's buffer
                    }
                    fStmBufPos = 0;
                }

                // here '\r' - it's replace for \0, to reduce checks
                char ch = (fStmBufPos >= fStmBufLen) ? '\r' : fStreamBuffer[fStmBufPos];
                fStmBufPos += 1;

                if (ch == '\r' || ch == '\n') {
                    int linePos = fLineBufPos;
                    fLineBufPos = 0;
                    if (linePos > 0) {
                        if (fEncodingState == EncodingState.esChanged) {
                            byte[] src = fDefaultEncoding.GetBytes(fLineBuffer, 0, linePos);
                            linePos = fSourceEncoding.GetChars(src, 0, src.Length, fLineBuffer, 0);
                        }

                        return linePos;
                    }
                } else {
                    fLineBuffer[fLineBufPos] = ch;
                    fLineBufPos += 1;
                }
            }
        }

        #endregion

        #region Loading functions

        protected override void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                fDefaultEncoding = GetDefaultEncoding();
                fSourceEncoding = fDefaultEncoding;
                fEncodingState = EncodingState.esUnchecked;

                long fileSize = fileStream.Length;
                int progress = 0;
                var invariantText = GEDCOMUtils.InvariantTextInfo;

                InitBuffers();
                var strTok = new GEDCOMParser(false);
                GDMTag curRecord = null;
                GDMTag curTag = null;
                var stack = new Stack<StackTuple>(9);

                int lineNum = 0;
                int lineLen;
                while ((lineLen = ReadLine(reader)) != -1) {
                    lineNum++;

                    int tagLevel;
                    string tagXRef, tagName, tagValue;
                    int tagId;

                    try {
                        strTok.Reset(fLineBuffer, 0, lineLen);
                        int lineRes = GEDCOMUtils.ParseTag(strTok, out tagLevel, out tagXRef, out tagName, out tagValue);

                        // empty line
                        if (lineRes == -2) continue;

                        // line with text but not in standard tag format
                        if (lineRes == -1) {
                            if (fTree.Format == GEDCOMFormat.gf_FTB || 
                                fTree.Format == GEDCOMFormat.gf_WikiTree ||
                                fTree.Format == GEDCOMFormat.gf_Geni ||
                                fTree.Format == GEDCOMFormat.gf_Ancestry) {
                                FixFTBLine(curRecord, curTag, lineNum, tagValue);
                                continue;
                            } else {
                                throw new GEDCOMInvalidFormatException(string.Format("The string {0} doesn't start with a valid number", lineNum));
                            }
                        }

                        tagName = invariantText.ToUpper(tagName);
                        tagId = GEDCOMTagsTable.Lookup(tagName);
                    } catch (GEDCOMInvalidFormatException) {
                        throw;
                    } catch (Exception ex) {
                        throw new GEDCOMInvalidFormatException(string.Format("Syntax error in line {0}", lineNum), ex);
                    }

                    if (tagLevel == 0) {
                        // beginning new record
                        if (curRecord != null) {
                            // previous record needs to be trimmed
                            curRecord.TrimExcess();
                        }

                        if (curRecord == fTree.Header && fEncodingState == EncodingState.esUnchecked) {
                            // beginning recognition of the first is not header record
                            // to check for additional versions of the code page
                            var format = GetGEDCOMFormat(fTree);
                            fTree.Format = format;
                            DefineEncoding(reader, format, streamCharset);
                        }

                        StackTuple stackTuple = AddTreeTag(fTree, tagLevel, tagId, tagValue);
                        if (stackTuple != null) {
                            stack.Clear();
                            stack.Push(stackTuple);

                            curRecord = stackTuple.Tag;
                            if (!string.IsNullOrEmpty(tagXRef)) {
                                ((GDMRecord)curRecord).SetXRef(fTree, tagXRef, false);
                            }
                            curTag = null;
                        } else {
                            // only TRLR
                            break;
                        }
                    } else {
                        if (curRecord != null) {
                            curTag = ProcessTag(stack, tagLevel, tagId, tagValue);
                        }
                    }

                    if (progressHandler != null) {
                        int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);
                        if (progress != newProgress) {
                            progress = newProgress;
                            progressHandler(fTree, progress);
                        }
                    }
                }

                stack.Clear();

                if (lineNum == 0) {
                    throw new GEDCOMEmptyFileException();
                }
            } finally {
                fTree.State = GDMTreeState.osReady;
            }
        }

        internal static GDMTag ProcessTag(Stack<StackTuple> stack, int tagLevel, int tagId, string tagValue)
        {
            GDMTag curTag = null;

            GDMTag parentTag = null;
            AddTagHandler addTagHandler = null;
            while (stack.Count > 0) {
                var tuple = stack.Peek();
                if (tagLevel > tuple.Level) {
                    parentTag = tuple.Tag;
                    addTagHandler = tuple.AddHandler;
                    break;
                }
                stack.Pop();
            }

            if (parentTag != null) {
                StackTuple tuple = null;

                if (addTagHandler != null) {
                    tuple = addTagHandler(parentTag, tagLevel, tagId, tagValue);
                } else {
                    tuple = AddBaseTag(parentTag, tagLevel, tagId, tagValue);
                }

                if (tuple != null) {
                    stack.Push(tuple);
                    curTag = tuple.Tag;
                }
            }

            if (curTag == null) {
                curTag = parentTag;
            }

            return curTag;
        }

        internal static StackTuple AddTreeTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTree tree = (GDMTree)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;
            GEDCOMTagType tagType = (GEDCOMTagType)tagId;

            if (tagType == GEDCOMTagType.INDI) {
                curTag = tree.AddRecord(new GDMIndividualRecord(tree));
                addHandler = AddIndividualRecordTag;

            } else if (tagType == GEDCOMTagType.FAM) {
                curTag = tree.AddRecord(new GDMFamilyRecord(tree));
                addHandler = AddFamilyRecordTag;

            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = tree.AddRecord(new GDMMultimediaRecord(tree));
                addHandler = AddMultimediaRecordTag;

            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = tree.AddRecord(new GDMNoteRecord(tree));
                curTag.ParseString(tagValue);
                addHandler = AddNoteRecordTag;

            } else if (tagType == GEDCOMTagType.REPO) {
                curTag = tree.AddRecord(new GDMRepositoryRecord(tree));
                addHandler = AddRepositoryRecordTag;

            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = tree.AddRecord(new GDMSourceRecord(tree));
                addHandler = AddSourceRecordTag;

            } else if (tagType == GEDCOMTagType.SUBN) {
                curTag = tree.AddRecord(new GDMSubmissionRecord(tree));
                addHandler = AddSubmissionRecordTag;

            } else if (tagType == GEDCOMTagType.SUBM) {
                curTag = tree.AddRecord(new GDMSubmitterRecord(tree));
                addHandler = AddSubmitterRecordTag;

            } else if (tagType == GEDCOMTagType._GROUP) {
                curTag = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = AddGroupRecordTag;

            } else if ((tagType == GEDCOMTagType._GRP) && (tree.Format == GEDCOMFormat.gf_Genney)) {
                curTag = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = AddGroupRecordTag;

            } else if (tagType == GEDCOMTagType._RESEARCH) {
                curTag = tree.AddRecord(new GDMResearchRecord(tree));
                addHandler = AddResearchRecordTag;

            } else if (tagType == GEDCOMTagType._TASK) {
                curTag = tree.AddRecord(new GDMTaskRecord(tree));
                addHandler = AddTaskRecordTag;

            } else if (tagType == GEDCOMTagType._COMM) {
                curTag = tree.AddRecord(new GDMCommunicationRecord(tree));
                addHandler = AddCommunicationRecordTag;

            } else if (tagType == GEDCOMTagType._LOC) {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = AddLocationRecordTag;

            } else if ((tagType == GEDCOMTagType._PLAC) && (tree.Format == GEDCOMFormat.gf_FamilyHistorian)) {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                ((GDMLocationRecord)curTag).LocationName = tagValue;
                addHandler = AddLocationRecordTag;

            } else if ((tagType == GEDCOMTagType._PLC) && (tree.Format == GEDCOMFormat.gf_Genney)) {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = AddLocationRecordTag;

            } else if (tagType == GEDCOMTagType.HEAD) {
                curTag = tree.Header;
                addHandler = AddHeaderTag;

            } else if (tagType == GEDCOMTagType.TRLR) {
                curTag = null;

            } else {
                curTag = tree.AddRecord(new GDMRecord(tree));
                addHandler = AddRecordTag;
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        #endregion

        #region Saving functions

        public void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            using (FileStream fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write)) {
                SaveToStreamExt(fileStream, charSet);
            }
        }

        public void SaveToStreamExt(Stream outputStream, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            StreamWriter writer = new StreamWriter(outputStream, GEDCOMUtils.GetEncodingByCharacterSet(charSet));
            IList<GDMRecord> records = fTree.GetRecords().GetList();
            SaveToStream(writer, records);
            writer.Flush();
        }

        public void SaveToStream(StreamWriter writer, IList<GDMRecord> list)
        {
            // write header
            WriteHeader(writer, 0, fTree.Header);

            if (list != null) {
                int num = list.Count;
                for (int i = 0; i < num; i++) {
                    GDMRecord record = list[i];
                    WriteRecordEx(writer, record);
                }
            }

            // write footer
            WriteTagLine(writer, 0, GEDCOMTagName.TRLR, string.Empty);
        }

        public static void WriteRecordEx(StreamWriter writer, GDMRecord record)
        {
            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    WriteIndividualRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtFamily:
                    WriteFamilyRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtNote:
                    WriteNoteRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtMultimedia:
                    WriteMultimediaRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtSource:
                    WriteSourceRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtRepository:
                    WriteRepositoryRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtGroup:
                    WriteGroupRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtResearch:
                    WriteResearchRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtTask:
                    WriteTaskRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtCommunication:
                    WriteCommunicationRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtLocation:
                    WriteLocationRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtSubmission:
                    WriteSubmissionRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtSubmitter:
                    WriteSubmitterRecord(writer, 0, record);
                    break;

                default:
                    WriteRecord(writer, 0, record);
                    break;
            }
        }

        #endregion

        #region Unified read/write functions

        private static StackTuple AddIndividualRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMIndividualRecord indiRec = (GDMIndividualRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FAMC) {
                curTag = indiRec.ChildToFamilyLinks.Add(new GDMChildToFamilyLink(indiRec));
                curTag.ParseString(tagValue);
                addHandler = AddChildToFamilyLinkTag;
            } else if (tagType == GEDCOMTagType.FAMS) {
                curTag = indiRec.SpouseToFamilyLinks.Add(new GDMSpouseToFamilyLink(indiRec));
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.NAME) {
                curTag = indiRec.AddPersonalName(new GDMPersonalName(indiRec));
                curTag.ParseString(tagValue);
                addHandler = AddPersonalNameTag;
            } else if (tagType == GEDCOMTagType.ASSO) {
                curTag = indiRec.Associations.Add(new GDMAssociation(indiRec));
                curTag.ParseString(tagValue);
                addHandler = AddAssociationTag;
            } else if (tagType == GEDCOMTagType.ALIA) {
                curTag = indiRec.Aliases.Add(new GDMAlias(indiRec));
                curTag.ParseString(tagValue);
            } else if (GEDCOMUtils.IsIndiEvent(tagType)) {
                curTag = indiRec.AddEvent(new GDMIndividualEvent(indiRec, tagId, tagValue));
                addHandler = AddCustomEventTag;
            } else if (GEDCOMUtils.IsIndiAttr(tagType)) {
                curTag = indiRec.AddEvent(new GDMIndividualAttribute(indiRec, tagId, tagValue));
                addHandler = AddCustomEventTag;
            } else if (tagType == GEDCOMTagType._GROUP) {
                curTag = indiRec.Groups.Add(new GDMPointer(indiRec, tagId, tagValue));
            } else if (tagType == GEDCOMTagType.SEX) {
                indiRec.Sex = GEDCOMUtils.GetSexVal(tagValue);
            } else {
                return AddRecordWithEventsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteIndividualRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMIndividualRecord indiRec = (GDMIndividualRecord)tag;

            WriteRecordWithEvents(stream, level, indiRec);

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.SEX, GEDCOMUtils.GetSexStr(indiRec.Sex), true);

            WriteList(stream, level, indiRec.PersonalNames, WritePersonalName);
            WriteList(stream, level, indiRec.ChildToFamilyLinks, WriteChildToFamilyLink);
            WriteList(stream, level, indiRec.SpouseToFamilyLinks, WriteTagEx);
            WriteList(stream, level, indiRec.Events, WriteCustomEvent);
            WriteList(stream, level, indiRec.Associations, WriteAssociation);
            WriteList(stream, level, indiRec.Aliases, WriteTagEx);
            WriteList(stream, level, indiRec.Groups, WriteTagEx);
        }


        private static StackTuple AddFamilyRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMFamilyRecord famRec = (GDMFamilyRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.HUSB) {
                curTag = famRec.Husband;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.WIFE) {
                curTag = famRec.Wife;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.CHIL) {
                curTag = famRec.Children.Add(new GDMIndividualLink(famRec, tagId, tagValue));
            } else if (tagType == GEDCOMTagType._STAT) {
                famRec.Status = GEDCOMUtils.GetMarriageStatusVal(tagValue);
            } else if (GEDCOMUtils.IsFamEvent(tagType)) {
                curTag = famRec.AddEvent(new GDMFamilyEvent(famRec, tagId, tagValue));
                addHandler = AddCustomEventTag;
            } else {
                return AddRecordWithEventsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteFamilyRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFamilyRecord famRec = (GDMFamilyRecord)tag;

            WriteRecordWithEvents(stream, level, famRec);

            level += 1;
            WriteBaseTag(stream, level, famRec.Husband);
            WriteBaseTag(stream, level, famRec.Wife);
            WriteTagLine(stream, level, GEDCOMTagName._STAT, GEDCOMUtils.GetMarriageStatusStr(famRec.Status), true);

            WriteList(stream, level, famRec.Children, WriteTagEx);
            WriteList(stream, level, famRec.Events, WriteCustomEvent);
        }


        private static StackTuple AddRecordWithEventsTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMRecordWithEvents evtRec = (GDMRecordWithEvents)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.RESN) {
                evtRec.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteRecordWithEvents(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRecordWithEvents recWE = (GDMRecordWithEvents)tag;

            WriteRecord(stream, level, recWE);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.RESN, GEDCOMUtils.GetRestrictionStr(recWE.Restriction), true);
        }


        private static StackTuple AddGroupRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                groupRec.GroupName = tagValue;
            } else if (tagType == GEDCOMTagType._MEMBER) {
                curTag = groupRec.Members.Add(new GDMIndividualLink(groupRec, tagId, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteGroupRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)tag;

            WriteRecord(stream, level, groupRec);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, groupRec.GroupName, true);
            WriteList(stream, level, groupRec.Members, WriteTagEx);
        }


        private static StackTuple AddMultimediaRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FILE) {
                curTag = mmRec.FileReferences.Add(new GDMFileReferenceWithTitle(mmRec));
                curTag.ParseString(tagValue);
                addHandler = AddFileReferenceWithTitleTag;
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteMultimediaRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)tag;

            WriteRecord(stream, level, mmRec);
            WriteList(stream, ++level, mmRec.FileReferences, WriteFileReferenceWithTitle);
        }


        private static StackTuple AddSourceRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceRecord sourRec = (GDMSourceRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.REPO) {
                curTag = sourRec.RepositoryCitations.Add(new GDMRepositoryCitation(sourRec));
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.DATA) {
                curTag = sourRec.Data;
                addHandler = AddSourceDataTag;
            } else if (tagType == GEDCOMTagType.AUTH) {
                curTag = sourRec.Originator;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            } else if (tagType == GEDCOMTagType.PUBL) {
                curTag = sourRec.Publication;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            } else if (tagType == GEDCOMTagType.ABBR) {
                sourRec.ShortTitle = tagValue;
            } else if (tagType == GEDCOMTagType.TEXT) {
                curTag = sourRec.Text;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            } else if (tagType == GEDCOMTagType.TITL) {
                curTag = sourRec.Title;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteSourceRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceRecord sourRec = (GDMSourceRecord)tag;

            WriteRecord(stream, level, sourRec);

            level += 1;
            WriteText(stream, level, sourRec.Title);
            WriteText(stream, level, sourRec.Publication);
            WriteTagLine(stream, level, GEDCOMTagName.ABBR, sourRec.ShortTitle, true);
            WriteList(stream, level, sourRec.RepositoryCitations, WriteTagEx);

            WriteSourceData(stream, level, sourRec.Data);
            WriteText(stream, level, sourRec.Originator);
            WriteText(stream, level, sourRec.Text);
        }


        private static StackTuple AddResearchRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMResearchRecord resRec = (GDMResearchRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                resRec.ResearchName = tagValue;
            } else if (tagType == GEDCOMTagType._PRIORITY) {
                resRec.Priority = GEDCOMUtils.GetPriorityVal(tagValue);
            } else if (tagType == GEDCOMTagType._STATUS) {
                resRec.Status = GEDCOMUtils.GetStatusVal(tagValue);
            } else if (tagType == GEDCOMTagType._PERCENT) {
                resRec.Percent = GEDCOMUtils.GetIntVal(tagValue);
            } else if (tagType == GEDCOMTagType._STARTDATE) {
                curTag = resRec.StartDate;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType._STOPDATE) {
                curTag = resRec.StopDate;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType._TASK) {
                curTag = resRec.Tasks.Add(new GDMPointer(resRec, tagId, tagValue));
            } else if (tagType == GEDCOMTagType._COMM) {
                curTag = resRec.Communications.Add(new GDMPointer(resRec, tagId, tagValue));
            } else if (tagType == GEDCOMTagType._GROUP) {
                curTag = resRec.Groups.Add(new GDMPointer(resRec, tagId, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteResearchRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMResearchRecord resRec = (GDMResearchRecord)tag;

            WriteRecord(stream, level, resRec);

            level += 1;

            WriteBaseTag(stream, level, resRec.StartDate);
            WriteBaseTag(stream, level, resRec.StopDate);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, resRec.ResearchName, true);
            WriteTagLine(stream, level, GEDCOMTagName._PRIORITY, GEDCOMUtils.GetPriorityStr(resRec.Priority), true);
            WriteTagLine(stream, level, GEDCOMTagName._STATUS, GEDCOMUtils.GetStatusStr(resRec.Status), true);
            WriteTagLine(stream, level, GEDCOMTagName._PERCENT, GEDCOMUtils.GetIntStr(resRec.Percent), true);

            WriteList(stream, level, resRec.Tasks, WriteTagEx);
            WriteList(stream, level, resRec.Communications, WriteTagEx);
            WriteList(stream, level, resRec.Groups, WriteTagEx);
        }


        private static StackTuple AddNoteRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMNoteRecord noteRec = (GDMNoteRecord)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType)) {
                return AddTextTag(noteRec, tagLevel, tagId, tagValue);
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }
        }

        private static void WriteNoteRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMNoteRecord noteRec = (GDMNoteRecord)tag;

            WriteRecordValue(stream, level, noteRec);
            level += 1;
            WriteText(stream, level, noteRec, true);
            WriteSubTags(stream, level, tag);
            if (!DebugWrite) {
                WriteTagLine(stream, level, GEDCOMTagName._UID, noteRec.UID, true);
                WriteChangeDate(stream, level, noteRec.ChangeDate);
            }
            WriteList(stream, level, noteRec.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, noteRec.UserReferences, WriteUserReference);
        }


        private static StackTuple AddRepositoryRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMRepositoryRecord repoRec = (GDMRepositoryRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                repoRec.RepositoryName = tagValue;
            } else if (tagType == GEDCOMTagType.ADDR) {
                curTag = repoRec.Address;
                curTag.ParseString(tagValue);
                addHandler = AddAddressTag;
            } else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW) {
                return AddAddressTag(repoRec.Address, tagLevel, tagId, tagValue);
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteRepositoryRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRepositoryRecord repoRec = (GDMRepositoryRecord)tag;

            WriteRecord(stream, level, repoRec);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, repoRec.RepositoryName, true);
            WriteAddress(stream, level, repoRec.Address);
        }


        private static StackTuple AddTaskRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTaskRecord taskRec = (GDMTaskRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._GOAL) {
                taskRec.Goal = tagValue;
            } else if (tagType == GEDCOMTagType._PRIORITY) {
                taskRec.Priority = GEDCOMUtils.GetPriorityVal(tagValue);
            } else if (tagType == GEDCOMTagType._STARTDATE) {
                curTag = taskRec.StartDate;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType._STOPDATE) {
                curTag = taskRec.StopDate;
                curTag.ParseString(tagValue);
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteTaskRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMTaskRecord taskRec = (GDMTaskRecord)tag;

            WriteRecord(stream, level, taskRec);

            level += 1;
            WriteBaseTag(stream, level, taskRec.StartDate);
            WriteBaseTag(stream, level, taskRec.StopDate);
            WriteTagLine(stream, level, GEDCOMTagName._PRIORITY, GEDCOMUtils.GetPriorityStr(taskRec.Priority), true);
            WriteTagLine(stream, level, GEDCOMTagName._GOAL, taskRec.Goal, true);
        }


        private static StackTuple AddCommunicationRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMCommunicationRecord commRec = (GDMCommunicationRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                commRec.CommName = tagValue;
            } else if (tagType == GEDCOMTagType.TYPE) {
                commRec.CommunicationType = GEDCOMUtils.GetCommunicationTypeVal(tagValue);
            } else if (tagType == GEDCOMTagType.FROM) {
                commRec.CommDirection = GDMCommunicationDir.cdFrom;
                commRec.Corresponder.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.TO) {
                commRec.CommDirection = GDMCommunicationDir.cdTo;
                commRec.Corresponder.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.DATE) {
                curTag = commRec.Date;
                curTag.ParseString(tagValue);
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteCommunicationRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMCommunicationRecord commRec = (GDMCommunicationRecord)tag;

            WriteRecord(stream, level, commRec);

            level += 1;
            WriteBaseTag(stream, level, commRec.Date);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, commRec.CommName, true);
            WriteTagLine(stream, level, GEDCOMTagName.TYPE, GEDCOMUtils.GetCommunicationTypeStr(commRec.CommunicationType), true);
            WriteBaseTag(stream, level, commRec.Corresponder);
        }


        private static StackTuple AddSubmissionRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSubmissionRecord submnRec = (GDMSubmissionRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.SUBM) {
                curTag = submnRec.Submitter;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.FAMF) {
                submnRec.FamilyFileName = tagValue;
            } else if (tagType == GEDCOMTagType.TEMP) {
                submnRec.TempleCode = tagValue;
            } else if (tagType == GEDCOMTagType.ANCE) {
                submnRec.GenerationsOfAncestors = GEDCOMUtils.GetIntVal(tagValue);
            } else if (tagType == GEDCOMTagType.DESC) {
                submnRec.GenerationsOfDescendants = GEDCOMUtils.GetIntVal(tagValue);
            } else if (tagType == GEDCOMTagType.ORDI) {
                submnRec.OrdinanceProcessFlag = GEDCOMUtils.GetOrdinanceProcessFlagVal(tagValue);
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteSubmissionRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSubmissionRecord submnRec = (GDMSubmissionRecord)tag;

            WriteRecord(stream, level, submnRec);

            level += 1;
            WriteBaseTag(stream, level, submnRec.Submitter);
            WriteTagLine(stream, level, GEDCOMTagName.FAMF, submnRec.FamilyFileName, true);
            WriteTagLine(stream, level, GEDCOMTagName.TEMP, submnRec.TempleCode, true);
            WriteTagLine(stream, level, GEDCOMTagName.ANCE, GEDCOMUtils.GetIntStr(submnRec.GenerationsOfAncestors), true);
            WriteTagLine(stream, level, GEDCOMTagName.DESC, GEDCOMUtils.GetIntStr(submnRec.GenerationsOfDescendants), true);
            WriteTagLine(stream, level, GEDCOMTagName.ORDI, GEDCOMUtils.GetOrdinanceProcessFlagStr(submnRec.OrdinanceProcessFlag), true);
        }


        private static StackTuple AddSubmitterRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                curTag = submrRec.Name;
                curTag.ParseString(tagValue);
                addHandler = AddPersonalNameTag;
            } else if (tagType == GEDCOMTagType.ADDR) {
                curTag = submrRec.Address;
                curTag.ParseString(tagValue);
                addHandler = AddAddressTag;
            } else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW) {
                return AddAddressTag(submrRec.Address, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.LANG) {
                curTag = submrRec.AddLanguage(new GDMLanguage(submrRec, tagId, tagValue));
            } else if (tagType == GEDCOMTagType.RFN) {
                submrRec.RegisteredReference = tagValue;
            } else {
                // 'ADDR' defines by default
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteSubmitterRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)tag;

            WriteRecord(stream, level, submrRec);

            level += 1;
            WritePersonalName(stream, level, submrRec.Name);
            WriteList(stream, level, submrRec.Languages, WriteTagEx);
            WriteAddress(stream, level, submrRec.Address);
            WriteTagLine(stream, level, GEDCOMTagName.RFN, submrRec.RegisteredReference, true);
        }


        private static StackTuple AddRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMRecord record = (GDMRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE) {
                curTag = record.Notes.Add(new GDMNotes(record));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = record.SourceCitations.Add(new GDMSourceCitation(record));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = record.MultimediaLinks.Add(new GDMMultimediaLink(record));
                curTag.ParseString(tagValue);
                addHandler = AddMultimediaLinkTag;
            } else if (tagType == GEDCOMTagType.REFN) {
                curTag = record.UserReferences.Add(new GDMUserReference(record));
                curTag.ParseString(tagValue);
                addHandler = AddUserReferenceTag;
            } else if (tagType == GEDCOMTagType._UID) {
                record.UID = tagValue;
            } else if (tagType == GEDCOMTagType.RIN) {
                record.AutomatedRecordID = tagValue;
            } else if (tagType == GEDCOMTagType.CHAN) {
                curTag = record.ChangeDate;
                addHandler = AddChangeDateTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRecord record = (GDMRecord)tag;

            WriteRecordValue(stream, level, record);

            level += 1;
            if (!DebugWrite) {
                WriteTagLine(stream, level, GEDCOMTagName._UID, record.UID, true);
                WriteChangeDate(stream, level, record.ChangeDate);
            }
            WriteSubTags(stream, level, tag);

            WriteTagLine(stream, level, GEDCOMTagName.RIN, record.AutomatedRecordID, true);
            WriteList(stream, level, record.Notes, WriteNote);
            WriteList(stream, level, record.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, record.MultimediaLinks, WriteMultimediaLink);
            WriteList(stream, level, record.UserReferences, WriteUserReference);
        }


        private static StackTuple AddHeaderTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeader header = (GDMHeader)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.CHAR) {
                curTag = header.CharacterSet;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderCharSetTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = header.Source;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderSourceTag;
            } else if (tagType == GEDCOMTagType.GEDC) {
                curTag = header.GEDCOM;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderGEDCOMTag;
            } else if (tagType == GEDCOMTagType.LANG) {
                header.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            } else if (tagType == GEDCOMTagType.COPR) {
                header.Copyright = tagValue;
            } else if (tagType == GEDCOMTagType.DEST) {
                header.ReceivingSystemName = tagValue;
            } else if (tagType == GEDCOMTagType.PLAC) {
                curTag = header.Place;
                curTag.ParseString(tagValue);
                addHandler = AddPlaceTag;
            } else if (tagType == GEDCOMTagType.SUBM) {
                curTag = header.Submitter;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.SUBN) {
                curTag = header.Submission;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = header.Note;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            } else if (tagType == GEDCOMTagType.DATE) {
                DateTime date;
                GEDCOMUtils.ParseDate(header.GetTree(), tagValue, out date);
                header.TransmissionDateTime = date;
                curTag = header;
                addHandler = AddHeaderTag;
            } else if (tagType == GEDCOMTagType.TIME) {
                TimeSpan time;
                GEDCOMUtils.ParseTime(tagValue, out time);
                DateTime date = header.TransmissionDateTime;
                header.TransmissionDateTime = date.Add(time);
            } else if (tagType == GEDCOMTagType.FILE) {
                curTag = header.File;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderFileTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteHeader(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeader header = (GDMHeader)tag;

            if (!WriteBaseTag(stream, level, header)) return false;

            level += 1;
            WriteHeaderSource(stream, level, header.Source);
            WriteTagLine(stream, level, GEDCOMTagName.DEST, header.ReceivingSystemName, true);
            WriteHeaderCharSet(stream, level, header.CharacterSet);
            WriteTagLine(stream, level, GEDCOMTagName.LANG, GEDCOMUtils.GetLanguageStr(header.Language), true);
            WriteHeaderGEDCOM(stream, level, header.GEDCOM);
            WriteHeaderFile(stream, level, header.File);
            WriteDateTime(stream, level, header.TransmissionDateTime);
            WriteTagLine(stream, level, GEDCOMTagName.COPR, header.Copyright, true);
            WritePlace(stream, level, header.Place);
            WriteBaseTag(stream, level, header.Submitter);
            WriteBaseTag(stream, level, header.Submission);
            WriteText(stream, level, header.Note);
            return true;
        }


        private static StackTuple AddHeaderSourceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderSource headerSource = (GDMHeaderSource)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS) {
                headerSource.Version = tagValue;
            } else if (tagType == GEDCOMTagType.NAME) {
                headerSource.ProductName = tagValue;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteHeaderSource(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderSource headerSource = (GDMHeaderSource)tag;

            if (!WriteBaseTag(stream, level, headerSource)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.VERS, headerSource.Version, true);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, headerSource.ProductName, true);
            return true;
        }


        private static StackTuple AddHeaderGEDCOMTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderGEDCOM headerGEDCOM = (GDMHeaderGEDCOM)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS) {
                headerGEDCOM.Version = tagValue;
            } else if (tagType == GEDCOMTagType.FORM) {
                headerGEDCOM.Form = tagValue;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteHeaderGEDCOM(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderGEDCOM headerGEDCOM = (GDMHeaderGEDCOM)tag;

            if (!WriteBaseTag(stream, level, headerGEDCOM)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.VERS, headerGEDCOM.Version, true);
            WriteTagLine(stream, level, GEDCOMTagName.FORM, headerGEDCOM.Form, true);
            return true;
        }


        private static StackTuple AddHeaderCharSetTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderCharSet headerCharSet = (GDMHeaderCharSet)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS) {
                headerCharSet.Version = tagValue;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteHeaderCharSet(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderCharSet headerCharSet = (GDMHeaderCharSet)tag;

            if (!WriteBaseTag(stream, level, headerCharSet)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.VERS, headerCharSet.Version, true);
            return true;
        }


        private static StackTuple AddHeaderFileTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderFile headerFile = (GDMHeaderFile)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._REV) {
                headerFile.Revision = GEDCOMUtils.GetIntVal(tagValue);
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteHeaderFile(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderFile headerFile = (GDMHeaderFile)tag;

            if (!WriteBaseTag(stream, level, headerFile)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName._REV, GEDCOMUtils.GetIntStr(headerFile.Revision), true);
            return true;
        }


        private static StackTuple AddChangeDateTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMChangeDate changeDate = (GDMChangeDate)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                DateTime date;
                GEDCOMUtils.ParseDate(changeDate.GetTree(), tagValue, out date);
                changeDate.ChangeDateTime = date;
                curTag = changeDate;
            } else if (tagType == GEDCOMTagType.TIME) {
                TimeSpan time;
                GEDCOMUtils.ParseTime(tagValue, out time);
                DateTime date = changeDate.ChangeDateTime;
                changeDate.ChangeDateTime = date.Add(time);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = changeDate.AddTag(new GDMNotes(changeDate));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteChangeDate(StreamWriter stream, int level, GDMTag tag)
        {
            GDMChangeDate changeDate = (GDMChangeDate)tag;

            if (!WriteBaseTag(stream, level, changeDate)) return false;

            level += 1;
            WriteDateTime(stream, level, changeDate.ChangeDateTime);
            return true;
        }

        private static void WriteDateTime(StreamWriter stream, int level, DateTime dtx)
        {
            if (!dtx.Equals(GDMChangeDate.ZeroDateTime)) {
                WriteTagLine(stream, level, GEDCOMTagName.DATE, GEDCOMUtils.GetDateStr(dtx), true);
                WriteTagLine(stream, ++level, GEDCOMTagName.TIME, GEDCOMUtils.GetTimeStr(dtx.TimeOfDay), true);
            }
        }


        private static StackTuple AddCustomEventTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.ADDR) {
                curTag = custEvent.Address;
                curTag.ParseString(tagValue);
                addHandler = AddAddressTag;
            } else if (tagType == GEDCOMTagType.AGNC) {
                custEvent.Agency = tagValue;
            } else if (tagType == GEDCOMTagType.CAUS) {
                custEvent.Cause = tagValue;
            } else if (tagType == GEDCOMTagType.TYPE) {
                custEvent.Classification = tagValue;
            } else if (tagType == GEDCOMTagType.DATE) {
                curTag = custEvent.Date;
                GEDCOMUtils.ParseDateValue(custEvent.GetTree(), custEvent.Date, tagValue);
            } else if (tagType == GEDCOMTagType.PLAC) {
                curTag = custEvent.Place;
                curTag.ParseString(tagValue);
                addHandler = AddPlaceTag;
            } else if (tagType == GEDCOMTagType.RELI) {
                custEvent.ReligiousAffilation = tagValue;
            } else if (tagType == GEDCOMTagType.RESN) {
                custEvent.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            } else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW) {
                return AddAddressTag(custEvent.Address, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = custEvent.Notes.Add(new GDMNotes(custEvent));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = custEvent.SourceCitations.Add(new GDMSourceCitation(custEvent));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = custEvent.MultimediaLinks.Add(new GDMMultimediaLink(custEvent));
                curTag.ParseString(tagValue);
                addHandler = AddMultimediaLinkTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteCustomEvent(StreamWriter stream, int level, GDMTag tag)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)tag;

            if (!WriteBaseTag(stream, level, custEvent)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.TYPE, custEvent.Classification, true);
            WriteBaseTag(stream, level, custEvent.Date);
            WritePlace(stream, level, custEvent.Place);
            WriteAddress(stream, level, custEvent.Address);
            WriteTagLine(stream, level, GEDCOMTagName.CAUS, custEvent.Cause, true);
            WriteTagLine(stream, level, GEDCOMTagName.AGNC, custEvent.Agency, true);
            WriteTagLine(stream, level, GEDCOMTagName.RELI, custEvent.ReligiousAffilation, true);
            WriteTagLine(stream, level, GEDCOMTagName.RESN, GEDCOMUtils.GetRestrictionStr(custEvent.Restriction), true);

            WriteList(stream, level, custEvent.Notes, WriteNote);
            WriteList(stream, level, custEvent.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, custEvent.MultimediaLinks, WriteMultimediaLink);
            return true;
        }


        private static StackTuple AddSourceDataTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceData sourData = (GDMSourceData)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.EVEN) {
                curTag = sourData.Events.Add(new GDMSourceEvent(sourData));
                curTag.ParseString(tagValue);
                addHandler = AddSourceDataEventTag;
            } else if (tagType == GEDCOMTagType.AGNC) {
                sourData.Agency = tagValue;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = sourData.Notes.Add(new GDMNotes(sourData));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        public static bool WriteSourceData(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceData sourData = (GDMSourceData)tag;

            if (!WriteBaseTag(stream, level, tag)) return false;

            level += 1;
            WriteList(stream, level, sourData.Notes, WriteNote);
            WriteTagLine(stream, level, GEDCOMTagName.AGNC, sourData.Agency, true);
            WriteList(stream, level, sourData.Events, WriteSourceDataEvent);
            return true;
        }


        private static StackTuple AddSourceDataEventTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                curTag = dataEvent.Date;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.PLAC) {
                curTag = dataEvent.Place;
                curTag.ParseString(tagValue);
                addHandler = AddPlaceTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteSourceDataEvent(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)tag;

            if (!WriteBaseTag(stream, level, dataEvent)) return false;

            level += 1;
            WriteBaseTag(stream, level, dataEvent.Date);
            WritePlace(stream, level, dataEvent.Place);
            return true;
        }

        private static StackTuple AddPointerWithNotesTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPointerWithNotes ptrWN = (GDMPointerWithNotes)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE) {
                curTag = ptrWN.Notes.Add(new GDMNotes(ptrWN));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }


        private static bool IsTextTag(string tagName)
        {
            return (tagName == GEDCOMTagName.CONT || tagName == GEDCOMTagName.CONC);
        }

        private static bool IsTextTag(GEDCOMTagType tag)
        {
            return (tag == GEDCOMTagType.CONT || tag == GEDCOMTagType.CONC);
        }

        private static StackTuple AddTextTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            IGDMTextObject textTag = (IGDMTextObject)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            var strings = textTag.Lines;
            if (tagType == GEDCOMTagType.CONC) {
                int strCount = strings.Count;
                if (strCount > 0) {
                    strings[strCount - 1] = strings[strCount - 1] + tagValue;
                } else {
                    strings.Add(tagValue);
                }
            } else if (tagType == GEDCOMTagType.CONT || tagType == GEDCOMTagType.TEXT) {
                strings.Add(tagValue);
            }

            return CreateReaderStackTuple(tagLevel, null, null);
        }

        private static bool WriteText(StreamWriter stream, int level, IGDMTextObject textTag, bool skipTag = false)
        {
            if (textTag.IsEmpty()) return false;

            var strings = textTag.Lines;
            int strCount = strings.Count;
            for (int i = 0; i < strCount; i++) {
                string str = strings[i];

                int len = Math.Min(str.Length, GEDCOMConsts.MaxLineLength);
                string sub = str.Substring(0, len);
                str = str.Remove(0, len);

                if (i == 0 && !skipTag) {
                    WriteTagLine(stream, level, ((GDMTag)textTag).GetTagName(), sub);
                    level += 1;
                } else {
                    WriteTagLine(stream, level, GEDCOMTagName.CONT, sub);
                }

                while (str.Length > 0) {
                    len = Math.Min(str.Length, GEDCOMConsts.MaxLineLength);

                    WriteTagLine(stream, level, GEDCOMTagName.CONC, str.Substring(0, len));

                    str = str.Remove(0, len);
                }
            }

            return true;
        }


        private static void WriteSubTags(StreamWriter stream, int level, GDMTag tag)
        {
            var subTags = tag.SubTags;

            int subtagsCount = subTags.Count;
            if (subtagsCount > 0) {
                for (int i = 0; i < subtagsCount; i++) {
                    GDMTag subtag = subTags[i];
                    var tagType = subtag.GetTagType();

                    if (tagType == GEDCOMTagType.CONC || tagType == GEDCOMTagType.CONT) {
                        WriteBaseTag(stream, level, subtag);
                    }
                }

                for (int i = 0; i < subtagsCount; i++) {
                    GDMTag subtag = subTags[i];
                    var tagType = subtag.GetTagType();

                    if (tagType != GEDCOMTagType.CONT && tagType != GEDCOMTagType.CONC) {
                        WriteTagEx(stream, level, subtag);
                    }
                }
            }
        }

        private static void WriteRecordValue(StreamWriter stream, int level, GDMRecord record)
        {
            string str = level.ToString();

            if (!string.IsNullOrEmpty(record.XRef)) {
                str = str + " @" + record.XRef + "@";
            }

            str = str + " " + record.GetTagName();

            string strValue = record.StringValue;
            if (!string.IsNullOrEmpty(strValue)) {
                str = str + " " + strValue;
            }

            stream.Write(str + GEDCOMConsts.NewLine);
        }

        private static void WriteTagValue(StreamWriter stream, int level, GDMTag tag)
        {
            WriteTagLine(stream, level, tag.GetTagName(), tag.StringValue);
        }

        public static bool WriteTagEx(StreamWriter stream, int level, GDMTag tag)
        {
            SaveTagHandler saveHandler = null;
            GEDCOMTagProps tagInfo = GEDCOMTagsTable.GetTagProps(tag.Id);
            if (tagInfo != null) {
                saveHandler = tagInfo.SaveHandler;
            }
            if (saveHandler == null) {
                saveHandler = WriteBaseTag;
            }

            return saveHandler(stream, level, tag);
        }

        internal static StackTuple AddBaseTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTag ownerTag = (GDMTag)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            curTag = GEDCOMProvider.CreateTag(ownerTag, tagId, tagValue);
            curTag = ownerTag.AddTag(curTag);

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteBaseTag(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            WriteTagValue(stream, level, tag);
            WriteSubTags(stream, ++level, tag);
            return true;
        }

        private static void WriteList<T>(StreamWriter stream, int level, GDMList<T> list, SaveTagHandler tagHandler) where T : GDMTag
        {
            IList<T> internalList = list.GetList();
            if (internalList == null) return;

            int num = internalList.Count;
            for (int i = 0; i < num; i++) {
                var item = internalList[i];
                if (item != null) {
                    tagHandler(stream, level, item);
                }
            }
        }

        // debug field
        public static bool DebugWrite = false;

        private static void WriteTagLine(StreamWriter stream, int level, string tagName, string tagValue, bool skipEmpty = false)
        {
            bool isEmpty = string.IsNullOrEmpty(tagValue);
            if (string.IsNullOrEmpty(tagName) || (isEmpty && skipEmpty)) return;

            string str = level + " " + tagName;
            if (!string.IsNullOrEmpty(tagValue)) {
                str = str + " " + tagValue;
            }
            stream.Write(str + GEDCOMConsts.NewLine);
        }

        private static StackTuple CreateReaderStackTuple(int level, GDMTag tag, AddTagHandler addHandler)
        {
            if (tag == null) {
                return null;
            } else {
                if (addHandler == null) {
                    addHandler = GEDCOMProvider.GetAddHandler(tag.Id);
                }
                return new StackTuple(level, tag, addHandler);
            }
        }


        private static StackTuple AddLocationRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMLocationRecord locRec = (GDMLocationRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                locRec.LocationName = tagValue;
            } else if (tagType == GEDCOMTagType.MAP) {
                curTag = locRec.Map;
                addHandler = AddMapTag;
            } else {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteLocationRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMLocationRecord locRec = (GDMLocationRecord)tag;

            WriteRecord(stream, level, locRec);

            level += 1;
            WriteMap(stream, level, locRec.Map);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, locRec.LocationName, true);
        }


        private static StackTuple AddPlaceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPlace place = (GDMPlace)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FORM) {
                place.Form = tagValue;
            } else if (tagType == GEDCOMTagType.MAP) {
                curTag = place.Map;
                addHandler = AddMapTag;
            } else if (tagType == GEDCOMTagType._LOC) {
                curTag = place.Location;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = place.Notes.Add(new GDMNotes(place));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WritePlace(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPlace place = (GDMPlace)tag;

            if (!WriteBaseTag(stream, level, tag)) return false;

            level += 1;
            WriteList(stream, level, place.Notes, WriteNote);
            WriteBaseTag(stream, level, place.Location);
            WriteMap(stream, level, place.Map);
            WriteTagLine(stream, level, GEDCOMTagName.FORM, place.Form, true);
            return true;
        }


        private static StackTuple AddMapTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMMap map = (GDMMap)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.LATI) {
                map.Lati = GEDCOMUtils.GetGeoCoord(tagValue, GEDCOMGeoCoord.Lati);
            } else if (tagType == GEDCOMTagType.LONG) {
                map.Long = GEDCOMUtils.GetGeoCoord(tagValue, GEDCOMGeoCoord.Long);
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteMap(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMap map = (GDMMap)tag;
            if (map.IsEmpty() && GEDCOMProvider.SkipEmptyTag(map.Id)) return false;

            WriteTagValue(stream, level, tag);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.LATI, GEDCOMUtils.GetFloatStr(map.Lati), true);
            WriteTagLine(stream, level, GEDCOMTagName.LONG, GEDCOMUtils.GetFloatStr(map.Long), true);
            return true;
        }


        private static StackTuple AddUserReferenceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMUserReference userRef = (GDMUserReference)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE) {
                userRef.ReferenceType = tagValue;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteUserReference(StreamWriter stream, int level, GDMTag tag)
        {
            GDMUserReference userRef = (GDMUserReference)tag;

            if (!WriteBaseTag(stream, level, userRef)) return false;

            GEDCOMProvider.WriteTagLine(stream, ++level, GEDCOMTagName.TYPE, userRef.ReferenceType, true);
            return true;
        }


        // Format: FORM\TYPE
        private static StackTuple AddFileReferenceWithTitleTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMFileReferenceWithTitle fileRef = (GDMFileReferenceWithTitle)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TITL) {
                fileRef.Title = tagValue;
            } else if (tagType == GEDCOMTagType.FORM) {
                fileRef.MultimediaFormat = GEDCOMUtils.GetMultimediaFormatVal(tagValue);
                curTag = fileRef;
                addHandler = AddFileReferenceWithTitleTag;
            } else if (tagType == GEDCOMTagType.TYPE) {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        // Format: FORM\TYPE
        private static bool WriteFileReferenceWithTitle(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFileReferenceWithTitle fileRef = (GDMFileReferenceWithTitle)tag;

            if (!WriteBaseTag(stream, level, fileRef)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.FORM, GEDCOMUtils.GetMultimediaFormatStr(fileRef.MultimediaFormat), true);
            GEDCOMProvider.WriteTagLine(stream, (level+1), GEDCOMTagName.TYPE, GEDCOMUtils.GetMediaTypeStr(fileRef.MediaType), true);
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.TITL, fileRef.Title, true);
            return true;
        }


        // Format: FORM\MEDI
        private static StackTuple AddFileReferenceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMFileReference fileRef = (GDMFileReference)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FORM) {
                fileRef.MultimediaFormat = GEDCOMUtils.GetMultimediaFormatVal(tagValue);
                curTag = fileRef;
                addHandler = AddFileReferenceTag;
            } else if (tagType == GEDCOMTagType.MEDI) {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        // Format: FORM\MEDI
        private static bool WriteFileReference(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFileReference fileRef = (GDMFileReference)tag;

            if (!WriteBaseTag(stream, level, fileRef)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.FORM, GEDCOMUtils.GetMultimediaFormatStr(fileRef.MultimediaFormat), true);
            GEDCOMProvider.WriteTagLine(stream, ++level, GEDCOMTagName.MEDI, GEDCOMUtils.GetMediaTypeStr(fileRef.MediaType), true);
            return true;
        }


        private static StackTuple AddMultimediaLinkTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TITL) {
                mmLink.Title = tagValue;
            } else if (tagType == GEDCOMTagType._PRIM) {
                mmLink.IsPrimary = GEDCOMUtils.GetBoolVal(tagValue);
            } else if (tagType == GEDCOMTagType._PRIM_CUTOUT) {
                mmLink.IsPrimaryCutout = GEDCOMUtils.GetBoolVal(tagValue);
            } else if (tagType == GEDCOMTagType._POSITION) {
                curTag = mmLink.CutoutPosition;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.FILE) {
                curTag = mmLink.FileReferences.Add(new GDMFileReference(mmLink));
                curTag.ParseString(tagValue);
                addHandler = AddFileReferenceTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        public static bool WriteMultimediaLink(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)tag;

            if (!WriteBaseTag(stream, level, mmLink)) return false;

            level += 1;
            WriteList(stream, level, mmLink.FileReferences, WriteFileReference);
            WriteTagLine(stream, level, GEDCOMTagName.TITL, mmLink.Title, true);
            if (mmLink.IsPrimary) WriteTagLine(stream, level, GEDCOMTagName._PRIM, GEDCOMUtils.GetBoolStr(mmLink.IsPrimary), true);
            if (mmLink.IsPrimaryCutout) WriteTagLine(stream, level, GEDCOMTagName._PRIM_CUTOUT, GEDCOMUtils.GetBoolStr(mmLink.IsPrimaryCutout), true);
            WriteBaseTag(stream, level, mmLink.CutoutPosition);
            return true;
        }


        private static StackTuple AddNoteTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMNotes note = (GDMNotes)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType)) {
                return AddTextTag(note, tagLevel, tagId, tagValue);
            } else {
                return AddBaseTag(note, tagLevel, tagId, tagValue);
            }
        }

        private static bool WriteNote(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            GDMNotes note = (GDMNotes)tag;
            if (note.IsPointer) {
                WriteTagValue(stream, level, note);
            } else {
                WriteText(stream, level, note);
            }
            WriteSubTags(stream, ++level, note);

            return true;
        }


        private static StackTuple AddSourceCitationTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceCitation sourCit = (GDMSourceCitation)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType)) {
                return AddTextTag(sourCit, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.QUAY) {
                sourCit.CertaintyAssessment = GEDCOMUtils.GetIntVal(tagValue);
            } else if (tagType == GEDCOMTagType.PAGE) {
                sourCit.Page = tagValue;
            } else if (tagType == GEDCOMTagType.TEXT) {
                curTag = sourCit.Text;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = sourCit.Notes.Add(new GDMNotes(sourCit));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = sourCit.MultimediaLinks.Add(new GDMMultimediaLink(sourCit));
                curTag.ParseString(tagValue);
                addHandler = AddMultimediaLinkTag;
            } else if (tagType == GEDCOMTagType.DATA) {
                curTag = sourCit.Data;
                addHandler = AddSourceCitationDataTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        public static bool WriteSourceCitation(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            GDMSourceCitation sourCit = (GDMSourceCitation)tag;
            if (sourCit.IsPointer) {
                WriteTagValue(stream, level, sourCit);
            } else {
                WriteText(stream, level, sourCit);
            }
            level += 1;
            WriteSubTags(stream, level, sourCit);

            WriteTagLine(stream, level, GEDCOMTagName.PAGE, sourCit.Page, true);
            WriteTagLine(stream, level, GEDCOMTagName.QUAY, GEDCOMUtils.GetIntStr(sourCit.CertaintyAssessment), true);
            WriteSourceCitationData(stream, level, sourCit.Data);
            WriteText(stream, level, sourCit.Text);
            WriteList(stream, level, sourCit.Notes, WriteNote);
            WriteList(stream, level, sourCit.MultimediaLinks, WriteMultimediaLink);
            return true;
        }


        private static StackTuple AddSourceCitationDataTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceCitationData srcitData = (GDMSourceCitationData)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                curTag = srcitData.Date;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.TEXT) {
                curTag = srcitData.Text;
                ((GDMTextTag)curTag).Lines.Add(tagValue);
                // without curTag.ParseString(tagValue) because next TEXT
                // will be rewrite all previous lines
                // GK will be without support for a list of text chunks
                addHandler = AddTextTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteSourceCitationData(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceCitationData srcitData = (GDMSourceCitationData)tag;

            if (!WriteBaseTag(stream, level, srcitData)) return false;

            level += 1;
            WriteBaseTag(stream, level, srcitData.Date);
            WriteText(stream, level, srcitData.Text);
            return true;
        }


        private static StackTuple AddAssociationTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMAssociation asso = (GDMAssociation)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.RELA) {
                asso.Relation = tagValue;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = asso.SourceCitations.Add(new GDMSourceCitation(asso));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            } else {
                return AddPointerWithNotesTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteAssociation(StreamWriter stream, int level, GDMTag tag)
        {
            GDMAssociation asso = (GDMAssociation)tag;

            if (!WriteBaseTag(stream, level, asso)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.RELA, asso.Relation, true);
            WriteList(stream, level, asso.SourceCitations, WriteSourceCitation);
            return true;
        }


        private static StackTuple AddAddressTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMAddress addr = (GDMAddress)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.CONT) {
                addr.Lines.Add(tagValue);
            } else if (tagType == GEDCOMTagType.ADR1) {
                addr.AddressLine1 = tagValue;
            } else if (tagType == GEDCOMTagType.ADR2) {
                addr.AddressLine2 = tagValue;
            } else if (tagType == GEDCOMTagType.ADR3) {
                addr.AddressLine3 = tagValue;
            } else if (tagType == GEDCOMTagType.CITY) {
                addr.AddressCity = tagValue;
            } else if (tagType == GEDCOMTagType.STAE) {
                addr.AddressState = tagValue;
            } else if (tagType == GEDCOMTagType.POST) {
                addr.AddressPostalCode = tagValue;
            } else if (tagType == GEDCOMTagType.CTRY) {
                addr.AddressCountry = tagValue;
            } else if (tagType == GEDCOMTagType.PHON) {
                curTag = addr.AddPhoneNumber(tagValue);
            } else if (tagType == GEDCOMTagType.EMAIL) {
                curTag = addr.AddEmailAddress(tagValue);
            } else if (tagType == GEDCOMTagType.FAX) {
                curTag = addr.AddFaxNumber(tagValue);
            } else if (tagType == GEDCOMTagType.WWW) {
                curTag = addr.AddWebPage(tagValue);
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteAddress(StreamWriter stream, int level, GDMTag tag)
        {
            GDMAddress addr = (GDMAddress)tag;
            if (addr.IsEmpty() && GEDCOMProvider.SkipEmptyTag(addr.Id)) return false;

            WriteTagValue(stream, level, addr);

            int lev = level + 1;

            var strings = addr.Lines;
            int strCount = strings.Count;
            for (int i = 1; i < strCount; i++) {
                WriteTagLine(stream, lev, GEDCOMTagName.CONT, strings[i]);
            }

            WriteTagLine(stream, lev, GEDCOMTagName.ADR1, addr.AddressLine1, true);
            WriteTagLine(stream, lev, GEDCOMTagName.ADR2, addr.AddressLine2, true);
            WriteTagLine(stream, lev, GEDCOMTagName.ADR3, addr.AddressLine3, true);
            WriteTagLine(stream, lev, GEDCOMTagName.CITY, addr.AddressCity, true);
            WriteTagLine(stream, lev, GEDCOMTagName.STAE, addr.AddressState, true);
            WriteTagLine(stream, lev, GEDCOMTagName.CTRY, addr.AddressCountry, true);
            WriteTagLine(stream, lev, GEDCOMTagName.POST, addr.AddressPostalCode, true);

            WriteList(stream, level, addr.PhoneNumbers, WriteTagEx);
            WriteList(stream, level, addr.EmailAddresses, WriteTagEx);
            WriteList(stream, level, addr.FaxNumbers, WriteTagEx);
            WriteList(stream, level, addr.WebPages, WriteTagEx);
            return true;
        }


        private static StackTuple AddChildToFamilyLinkTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMChildToFamilyLink cfl = (GDMChildToFamilyLink)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.STAT) {
                cfl.ChildLinkageStatus = GEDCOMUtils.GetChildLinkageStatusVal(tagValue);
            } else if (tagType == GEDCOMTagType.PEDI) {
                cfl.PedigreeLinkageType = GEDCOMUtils.GetPedigreeLinkageTypeVal(tagValue);
            } else {
                return AddPointerWithNotesTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteChildToFamilyLink(StreamWriter stream, int level, GDMTag tag)
        {
            GDMChildToFamilyLink cfl = (GDMChildToFamilyLink)tag;

            if (!WriteBaseTag(stream, level, cfl)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.STAT, GEDCOMUtils.GetChildLinkageStatusStr(cfl.ChildLinkageStatus), true);
            WriteTagLine(stream, level, GEDCOMTagName.PEDI, GEDCOMUtils.GetPedigreeLinkageTypeStr(cfl.PedigreeLinkageType), true);
            return true;
        }


        private static StackTuple AddPersonalNameTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPersonalName persName = (GDMPersonalName)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE) {
                persName.NameType = GEDCOMUtils.GetNameTypeVal(tagValue);
            } else if (tagType == GEDCOMTagType._LANG || tagType == GEDCOMTagType.LANG) {
                persName.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            } else if (tagType == GEDCOMTagType.FONE || tagType == GEDCOMTagType.ROMN) {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = persName.Notes.Add(new GDMNotes(persName));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = persName.SourceCitations.Add(new GDMSourceCitation(persName));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            } else {
                return AddPersonalNamePiecesTag(persName.Pieces, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        public static bool WritePersonalName(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPersonalName persName = (GDMPersonalName)tag;

            if (!WriteBaseTag(stream, level, persName)) return false;

            int lev = level + 1;
            WriteTagLine(stream, lev, GEDCOMTagName.LANG, GEDCOMUtils.GetLanguageStr(persName.Language), true);
            WriteTagLine(stream, lev, GEDCOMTagName.TYPE, GEDCOMUtils.GetNameTypeStr(persName.NameType), true);
            WriteList(stream, lev, persName.Notes, WriteNote);
            WriteList(stream, lev, persName.SourceCitations, WriteSourceCitation);
            WritePersonalNamePieces(stream, level, persName.Pieces); // same level
            return true;
        }


        private static StackTuple AddPersonalNamePiecesTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPersonalNamePieces persNamePieces = (GDMPersonalNamePieces)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NPFX) {
                persNamePieces.Prefix = tagValue;
            } else if (tagType == GEDCOMTagType.GIVN) {
                persNamePieces.Given = tagValue;
            } else if (tagType == GEDCOMTagType.NICK) {
                persNamePieces.Nickname = tagValue;
            } else if (tagType == GEDCOMTagType.SPFX) {
                persNamePieces.SurnamePrefix = tagValue;
            } else if (tagType == GEDCOMTagType.SURN) {
                persNamePieces.Surname = tagValue;
            } else if (tagType == GEDCOMTagType.NSFX) {
                persNamePieces.Suffix = tagValue;
            } else if (tagType == GEDCOMTagType._PATN || tagType == GEDCOMTagType._MIDN) {
                persNamePieces.PatronymicName = tagValue;
            } else if (tagType == GEDCOMTagType._MARN || tagType == GEDCOMTagType._MARNM) {
                persNamePieces.MarriedName = tagValue;
            } else if (tagType == GEDCOMTagType._RELN) {
                persNamePieces.ReligiousName = tagValue;
            } else if (tagType == GEDCOMTagType._CENN) {
                persNamePieces.CensusName = tagValue;
            } else {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WritePersonalNamePieces(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPersonalNamePieces persNamePieces = (GDMPersonalNamePieces)tag;

            int lev = level + 1;
            WriteSubTags(stream, lev, persNamePieces);

            WriteTagLine(stream, lev, GEDCOMTagName.SURN, persNamePieces.Surname, true);
            WriteTagLine(stream, lev, GEDCOMTagName.GIVN, persNamePieces.Given, true);
            WriteTagLine(stream, lev, GEDCOMTagName._PATN, persNamePieces.PatronymicName, true);
            WriteTagLine(stream, lev, GEDCOMTagName.NPFX, persNamePieces.Prefix, true);
            WriteTagLine(stream, lev, GEDCOMTagName.NICK, persNamePieces.Nickname, true);
            WriteTagLine(stream, lev, GEDCOMTagName.SPFX, persNamePieces.SurnamePrefix, true);
            WriteTagLine(stream, lev, GEDCOMTagName.NSFX, persNamePieces.Suffix, true);
            WriteTagLine(stream, lev, GEDCOMTagName._MARN, persNamePieces.MarriedName, true);
            WriteTagLine(stream, lev, GEDCOMTagName._RELN, persNamePieces.ReligiousName, true);
            WriteTagLine(stream, lev, GEDCOMTagName._CENN, persNamePieces.CensusName, true);
        }

        #endregion

        #region Format variations

        /// <summary>
        /// Fix of errors that are in the dates of FamilyTreeBuilder.
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        public static string FixFTB(string str)
        {
            string result = str;
            string su = result.Substring(0, 3).ToUpperInvariant();

            if (su == GEDCOMConsts.GEDCOMDateRangeArray[0] ||
                su == GEDCOMConsts.GEDCOMDateRangeArray[1] ||
                su == GEDCOMConsts.GEDCOMDateApproximatedArray[1] ||
                su == GEDCOMConsts.GEDCOMDateApproximatedArray[2] ||
                su == GEDCOMConsts.GEDCOMDateApproximatedArray[3])
            {
                result = result.Remove(0, 4);
            }
            return result;
        }

        /// <summary>
        /// Fix of line errors that are in the files of FamilyTreeBuilder.
        /// </summary>
        private static void FixFTBLine(GDMTag curRecord, GDMTag curTag, int lineNum, string str)
        {
            try {
                if (curTag != null) {
                    if (curTag is IGDMTextObject) {
                        AddTextTag(curTag, 0, (int)GEDCOMTagType.CONT, str);
                    } else {
                        var tagType = curTag.GetTagType();

                        if (tagType == GEDCOMTagType.CONT || tagType == GEDCOMTagType.CONC) {
                            curTag.StringValue += str;
                        } else {
                            AddBaseTag(curTag, 0, (int)GEDCOMTagType.NOTE, str);
                        }
                    }
                } else if (curRecord != null) {
                    if (curRecord is IGDMTextObject) {
                        AddTextTag(curRecord, 0, (int)GEDCOMTagType.CONT, str);
                    } else {
                        AddRecordTag(curRecord, 0, (int)GEDCOMTagType.NOTE, str);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDCOMProvider.FixFTBLine(): Line " + lineNum.ToString() + " failed correct", ex);
            }
        }

        public static GEDCOMFormat GetGEDCOMFormat(GDMTree tree)
        {
            if (tree != null) {
                string sour = tree.Header.Source.StringValue.Trim();

                int num = GEDCOMProvider.GEDCOMFormats.Length;
                for (int i = 1; i < num; i++) {
                    var appFmt = GEDCOMProvider.GEDCOMFormats[i];
                    if (string.Equals(appFmt.Sign, sour, StringComparison.Ordinal)) {
                        return appFmt.Format;
                    }
                }
            }

            return GEDCOMFormat.gf_Unknown;
        }

        public static GEDCOMAppFormat GetGEDCOMFormatProps(GEDCOMFormat format)
        {
            int num = GEDCOMProvider.GEDCOMFormats.Length;
            for (int i = 1; i < num; i++) {
                var appFmt = GEDCOMProvider.GEDCOMFormats[i];
                if (appFmt.Format == format) {
                    return appFmt;
                }
            }

            return GEDCOMProvider.GEDCOMFormats[0];
        }

        #endregion

        #region Tag properties

        static GEDCOMProvider()
        {
            GEDCOMFormats = new GEDCOMAppFormat[] {
                new GEDCOMAppFormat(GEDCOMFormat.gf_Unknown, "", "", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Native, "GEDKeeper", "", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Genealogy_RusOld, "├σφσαδεπΦ", "Genealogy (Rus, old)", 1251), // signature in CP437

                new GEDCOMAppFormat(GEDCOMFormat.gf_AGES, "AGES", "Ages!", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_ALTREE, "ALTREE", "Agelong Tree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Ahnenblatt, "AHN", "Ahnenblatt", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_AncestQuest, "AncestQuest", "Ancestral Quest", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Ancestry, "Ancestry.com Family Trees", "WikiTree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_EasyTree, "EasyTree", "EasyTree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyHistorian, "FAMILY_HISTORIAN", "Family Historian", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyTreeMaker, "FTM", "Family Tree Maker", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyTreeMaker, "FTW", "Family Tree Maker", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FTB, "MYHERITAGE", "MyHeritage Family Tree Builder", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GENBOX, "GENBOX", "Genbox Family History", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GENJ, "GENJ", "GENJ", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Geni, "Geni.com", "Geni", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GeneWeb, "GeneWeb", "GeneWeb", 1252),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Genney, "Genney", "Genney", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GenoPro, "GenoPro", "GenoPro", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Gramps, "Gramps", "Gramps", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Heredis, "HEREDIS 12 PC", "Heredis", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Legacy, "Legacy", "Legacy", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Lifelines, "Lifelines", "Lifelines", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_PAF, "PAF", "Personal Ancestral File", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Reunion, "Reunion", "Reunion", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_RootsMagic, "RootsMagic", "RootsMagic", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_WikiTree, "WikiTree.com", "WikiTree", -1),
            };


            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ABBR, GEDCOMTagName.ABBR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADDR, GEDCOMTagName.ADDR, GDMAddress.Create, AddAddressTag, WriteAddress, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADOP, GEDCOMTagName.ADOP, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR1, GEDCOMTagName.ADR1);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR2, GEDCOMTagName.ADR2);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR3, GEDCOMTagName.ADR3);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AFN, GEDCOMTagName.AFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AGNC, GEDCOMTagName.AGNC, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ALIA, GEDCOMTagName.ALIA, null, AddBaseTag, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANCE, GEDCOMTagName.ANCE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANCI, GEDCOMTagName.ANCI, GDMPointer.Create, AddBaseTag);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANUL, GEDCOMTagName.ANUL, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ASSO, GEDCOMTagName.ASSO, null, AddAssociationTag, WriteAssociation, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AUTH, GEDCOMTagName.AUTH, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BAPL, GEDCOMTagName.BAPL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BAPM, GEDCOMTagName.BAPM, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BARM, GEDCOMTagName.BARM, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BASM, GEDCOMTagName.BASM, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BIRT, GEDCOMTagName.BIRT, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BLES, GEDCOMTagName.BLES, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BURI, GEDCOMTagName.BURI, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CAST, GEDCOMTagName.CAST, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CAUS, GEDCOMTagName.CAUS, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CENS, GEDCOMTagName.CENS, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHAN, GEDCOMTagName.CHAN, null, AddChangeDateTag, WriteChangeDate, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHAR, GEDCOMTagName.CHAR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHIL, GEDCOMTagName.CHIL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHR, GEDCOMTagName.CHR, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHRA, GEDCOMTagName.CHRA, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CITY, GEDCOMTagName.CITY, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONC, GEDCOMTagName.CONC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONF, GEDCOMTagName.CONF, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONL, GEDCOMTagName.CONL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONT, GEDCOMTagName.CONT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.COPR, GEDCOMTagName.COPR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CORP, GEDCOMTagName.CORP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CREM, GEDCOMTagName.CREM, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CTRY, GEDCOMTagName.CTRY, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DATA, GEDCOMTagName.DATA, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DATE, GEDCOMTagName.DATE, GDMDateValue.Create, AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DEAT, GEDCOMTagName.DEAT, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DESC, GEDCOMTagName.DESC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DESI, GEDCOMTagName.DESI, GDMPointer.Create, AddBaseTag);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DEST, GEDCOMTagName.DEST);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DIV, GEDCOMTagName.DIV, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DIVF, GEDCOMTagName.DIVF, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DSCR, GEDCOMTagName.DSCR, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EDUC, GEDCOMTagName.EDUC, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ENDL, GEDCOMTagName.ENDL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EVEN, GEDCOMTagName.EVEN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EMAIL, GEDCOMTagName.EMAIL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EMIG, GEDCOMTagName.EMIG, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ENGA, GEDCOMTagName.ENGA, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FACT, GEDCOMTagName.FACT, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAM, GEDCOMTagName.FAM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMC, GEDCOMTagName.FAMC, GDMChildToFamilyLink.Create, AddChildToFamilyLinkTag, WriteChildToFamilyLink);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMF, GEDCOMTagName.FAMF);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMS, GEDCOMTagName.FAMS, null, AddPointerWithNotesTag);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAX, GEDCOMTagName.FAX);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FCOM, GEDCOMTagName.FCOM, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FILE, GEDCOMTagName.FILE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FONE, GEDCOMTagName.FONE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FORM, GEDCOMTagName.FORM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FROM, GEDCOMTagName.FROM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GEDC, GEDCOMTagName.GEDC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GIVN, GEDCOMTagName.GIVN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GRAD, GEDCOMTagName.GRAD, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.HEAD, GEDCOMTagName.HEAD);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.HUSB, GEDCOMTagName.HUSB, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.IDNO, GEDCOMTagName.IDNO, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.IMMI, GEDCOMTagName.IMMI, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.INDI, GEDCOMTagName.INDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.INT, GEDCOMTagName.INT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LANG, GEDCOMTagName.LANG, GDMLanguage.Create, AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LATI, GEDCOMTagName.LATI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LONG, GEDCOMTagName.LONG);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MAP, GEDCOMTagName.MAP, GDMMap.Create, AddMapTag, WriteMap, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARB, GEDCOMTagName.MARB, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARC, GEDCOMTagName.MARC, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARL, GEDCOMTagName.MARL, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARS, GEDCOMTagName.MARS, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARR, GEDCOMTagName.MARR, GDMFamilyEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MEDI, GEDCOMTagName.MEDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NAME, GEDCOMTagName.NAME, null, AddPersonalNameTag, WritePersonalName);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NATI, GEDCOMTagName.NATI, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NATU, GEDCOMTagName.NATU, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NCHI, GEDCOMTagName.NCHI, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NICK, GEDCOMTagName.NICK, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NMR, GEDCOMTagName.NMR, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NOTE, GEDCOMTagName.NOTE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NPFX, GEDCOMTagName.NPFX, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NSFX, GEDCOMTagName.NSFX, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.OBJE, GEDCOMTagName.OBJE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.OCCU, GEDCOMTagName.OCCU, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ORDI, GEDCOMTagName.ORDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ORDN, GEDCOMTagName.ORDN, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PAGE, GEDCOMTagName.PAGE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PEDI, GEDCOMTagName.PEDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PHON, GEDCOMTagName.PHON, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PLAC, GEDCOMTagName.PLAC, GDMPlace.Create, AddPlaceTag, WritePlace, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.POST, GEDCOMTagName.POST, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PROB, GEDCOMTagName.PROB, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PROP, GEDCOMTagName.PROP, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PUBL, GEDCOMTagName.PUBL, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.QUAY, GEDCOMTagName.QUAY, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.REFN, GEDCOMTagName.REFN, null, AddUserReferenceTag, WriteUserReference);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RELA, GEDCOMTagName.RELA);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RELI, GEDCOMTagName.RELI, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.REPO, GEDCOMTagName.REPO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RESI, GEDCOMTagName.RESI, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RESN, GEDCOMTagName.RESN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RETI, GEDCOMTagName.RETI, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RFN, GEDCOMTagName.RFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RIN, GEDCOMTagName.RIN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ROMN, GEDCOMTagName.ROMN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SEX, GEDCOMTagName.SEX);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SLGC, GEDCOMTagName.SLGC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SLGS, GEDCOMTagName.SLGS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SOUR, GEDCOMTagName.SOUR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SPFX, GEDCOMTagName.SPFX, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SSN, GEDCOMTagName.SSN, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.STAE, GEDCOMTagName.STAE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.STAT, GEDCOMTagName.STAT, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SUBM, GEDCOMTagName.SUBM, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SUBN, GEDCOMTagName.SUBN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SURN, GEDCOMTagName.SURN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TEMP, GEDCOMTagName.TEMP, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TEXT, GEDCOMTagName.TEXT, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TIME, GEDCOMTagName.TIME, GDMTime.Create, AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TITL, GEDCOMTagName.TITL, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TO, GEDCOMTagName.TO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TRLR, GEDCOMTagName.TRLR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TYPE, GEDCOMTagName.TYPE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.VERS, GEDCOMTagName.VERS, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WIFE, GEDCOMTagName.WIFE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WILL, GEDCOMTagName.WILL, GDMIndividualEvent.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WWW, GEDCOMTagName.WWW, null, null, null, true);

            // non-standard extended tags (other applications)
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._AWARD, GEDCOMTagName._AWARD, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._BGRO, GEDCOMTagName._BGRO, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._CENN, GEDCOMTagName._CENN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._ELEC, GEDCOMTagName._ELEC, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._EXCM, GEDCOMTagName._EXCM, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._EYES, GEDCOMTagName._EYES, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GRP, GEDCOMTagName._GRP, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._HAIR, GEDCOMTagName._HAIR, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._HOBBY, GEDCOMTagName._HOBBY, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._LOC, GEDCOMTagName._LOC, GDMPointer.Create, AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MARN, GEDCOMTagName._MARN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MDCL, GEDCOMTagName._MDCL, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MDNA, GEDCOMTagName._MDNA, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._OBIT, GEDCOMTagName._OBIT, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PATN, GEDCOMTagName._PATN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PLAC, GEDCOMTagName._PLAC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PLC, GEDCOMTagName._PLC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._POSITION, GEDCOMTagName._POSITION, GDMCutoutPosition.Create, AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIM, GEDCOMTagName._PRIM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIM_CUTOUT, GEDCOMTagName._PRIM_CUTOUT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._RELN, GEDCOMTagName._RELN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STAT, GEDCOMTagName._STAT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._TRAVEL, GEDCOMTagName._TRAVEL, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._UID, GEDCOMTagName._UID);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._YDNA, GEDCOMTagName._YDNA, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent, true);

            // import only
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MARNM, GEDCOMTagName._MARNM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MIDN, GEDCOMTagName._MIDN);

            // non-standard extended tags (GEDKeeper)
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._BOOKMARK, GEDCOMTagName._BOOKMARK);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._COMM, GEDCOMTagName._COMM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._FOLDER, GEDCOMTagName._FOLDER);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GOAL, GEDCOMTagName._GOAL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GROUP, GEDCOMTagName._GROUP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._LANG, GEDCOMTagName._LANG, GDMLanguage.Create, AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MEMBER, GEDCOMTagName._MEMBER);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI, GEDCOMTagName._MILI, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_DIS, GEDCOMTagName._MILI_DIS, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_IND, GEDCOMTagName._MILI_IND, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_RANK, GEDCOMTagName._MILI_RANK, GDMIndividualAttribute.Create, AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PATRIARCH, GEDCOMTagName._PATRIARCH);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PERCENT, GEDCOMTagName._PERCENT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIORITY, GEDCOMTagName._PRIORITY);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._RESEARCH, GEDCOMTagName._RESEARCH);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._REV, GEDCOMTagName._REV);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STARTDATE, GEDCOMTagName._STARTDATE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STOPDATE, GEDCOMTagName._STOPDATE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STATUS, GEDCOMTagName._STATUS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._TASK, GEDCOMTagName._TASK);
        }

        public static GDMTag CreateTag(GDMObject owner, int tagId, string tagValue)
        {
            TagConstructor ctor = null;

            GEDCOMTagProps tagInfo = GEDCOMTagsTable.GetTagProps(tagId);
            if (tagInfo != null) {
                ctor = tagInfo.Constructor;
            }

            if (ctor == null) {
                ctor = GDMTag.Create;
            }

            return ctor(owner, tagId, tagValue);
        }

        public static AddTagHandler GetAddHandler(int tagId)
        {
            GEDCOMTagProps tagInfo = GEDCOMTagsTable.GetTagProps(tagId);
            return (tagInfo != null) ? tagInfo.AddHandler : GEDCOMProvider.AddBaseTag;
        }

        public static bool SkipEmptyTag(int tagId)
        {
            GEDCOMTagProps props = GEDCOMTagsTable.GetTagProps(tagId);
            return (props != null && props.SkipEmpty);
        }

        #endregion
    }
}
