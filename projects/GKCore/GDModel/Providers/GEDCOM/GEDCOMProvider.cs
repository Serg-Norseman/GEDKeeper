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

#pragma warning disable IDE0060 // Remove unused parameter

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
        public readonly bool BadLines;

        public GEDCOMAppFormat(GEDCOMFormat format, string sign, string name, int predefCharset, bool badLines = false)
        {
            Format = format;
            Sign = sign;
            Name = name;
            PredefCharset = predefCharset;
            BadLines = badLines;
        }
    }

    internal delegate StackTuple AddTagHandler(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue);

    internal delegate bool SaveTagHandler(StreamWriter stream, int level, GDMTag tag);

    public enum TagHandler
    {
        Null,
        IndividualRecordTag,
        FamilyRecordTag,
        GroupRecordTag,
        MultimediaRecordTag,
        SourceRecordTag,
        ResearchRecordTag,
        NoteRecordTag,
        RepositoryRecordTag,
        TaskRecordTag,
        CommunicationRecordTag,
        SubmissionRecordTag,
        SubmitterRecordTag,
        RecordTag,
        HeaderTag,
        HeaderSourceTag,
        HeaderGEDCOMTag,
        HeaderCharSetTag,
        HeaderFileTag,
        ChangeDateTag,
        CustomEventTag,
        SourceDataTag,
        SourceDataEventTag,
        TextTag,
        BaseTag,
        LocationRecordTag,
        LocationNameTag,
        LocationLinkTag,
        PlaceTag,
        MapTag,
        UserReferenceTag,
        FileReferenceWithTitleTag,
        FileReferenceTag,
        MultimediaLinkTag,
        NoteTag,
        SourceCitationTag,
        SourceCitationDataTag,
        AssociationTag,
        AddressTag,
        ChildToFamilyLinkTag,
        PersonalNameTag,
        RepositoryCitationTag,
        SourceCallNumberTag,
        SkipTag,
        IndividualEventTag,
        FamilyEventTag,
        AgeTag,
        DNATestTag,
    }

    public struct StackTuple
    {
        public int Level;
        public GDMTag Tag;
        public TagHandler AddHandler;

        public StackTuple(int level, GDMTag tag, TagHandler addHandler)
        {
            Level = level;
            Tag = tag;
            AddHandler = addHandler;
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

        public static bool KeepRichNames = true;
        public static bool Strict = false;


        public GEDCOMProvider(GDMTree tree) : base(tree)
        {
        }

        public GEDCOMProvider(GDMTree tree, bool keepRichNames, bool strict) : base(tree)
        {
            KeepRichNames = keepRichNames;
            Strict = strict;
        }

        public override string GetFilesFilter()
        {
            return LangMan.LS(LSID.GEDCOMFilter);
        }

        #region Encoding routines

        private enum EncodingState { esUnchecked, esUnchanged, esChanged }

        private const int DEF_CODEPAGE = 437;
        private Encoding fEncoding;
        private EncodingState fEncodingState;

        private void SetEncoding(Encoding encoding)
        {
            fEncodingState = fEncoding.Equals(encoding) ? EncodingState.esUnchanged : EncodingState.esChanged;
            fEncoding = encoding;
        }

        private void DefineEncoding(Stream inputStream, GEDCOMFormat format, string streamCharset)
        {
            GEDCOMCharacterSet charSet = fTree.Header.CharacterSet.Value;
            switch (charSet)
            {
                case GEDCOMCharacterSet.csUTF8:
                    if (!SysUtils.IsUnicodeEncoding(fEncoding)) {
                        SetEncoding(Encoding.UTF8); // file without BOM
                    } else {
                        fEncodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    if (format == GEDCOMFormat.Geni || format == GEDCOMFormat.GENJ) {
                        SetEncoding(Encoding.UTF8);
                    } else {
                        if (!SysUtils.IsUnicodeEncoding(fEncoding)) {
                            SetEncoding(Encoding.Unicode); // file without BOM
                        } else {
                            fEncodingState = EncodingState.esUnchanged;
                        }
                    }
                    break;

                case GEDCOMCharacterSet.csANSEL:
                    if (format == GEDCOMFormat.ALTREE) {
                        // Agelong Tree 4.0 with ANSEL is actually characteristic
                        // for the Russian-language data export
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else if (format == GEDCOMFormat.Geni) {
                        SetEncoding(Encoding.UTF8);
                    } else {
                        SetEncoding(new AnselEncoding());
                    }
                    break;

                case GEDCOMCharacterSet.csASCII:
                    if (format == GEDCOMFormat.Native) {
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

        protected override Encoding GetDefaultEncoding(Stream inputStream)
        {
            // Dirty code: external encodings are not supported on mobile (Xamarin)
            if (AppHost.Instance.HasFeatureSupport(GKCore.Types.Feature.Mobile)) return Encoding.UTF8;

            var result = Encoding.GetEncoding(DEF_CODEPAGE);
            if (inputStream.CanSeek) {
                byte[] array = new byte[4];
                int num = inputStream.Read(array, 0, 4);
                if (num >= 3 && array[0] == 239 && array[1] == 187 && array[2] == 191) {
                    result = Encoding.UTF8;
                    num -= 3;
                } else if (num == 4 && array[0] == 0 && array[1] == 0 && array[2] == 254 && array[3] == byte.MaxValue) {
                    result = Encoding.GetEncoding("utf-32");
                    num -= 4;
                } else if (num == 4 && array[0] == byte.MaxValue && array[1] == 254 && array[2] == 0 && array[3] == 0) {
                    result = Encoding.GetEncoding("utf-32");
                    num -= 4;
                } else if (num >= 2 && array[0] == 254 && array[1] == byte.MaxValue) {
                    result = Encoding.BigEndianUnicode;
                    num -= 2;
                } else if (num >= 2 && array[0] == byte.MaxValue && array[1] == 254) {
                    result = Encoding.Unicode;
                    num -= 2;
                }
                inputStream.Seek(-num, SeekOrigin.Current);
            }
            return result;
        }

        private string DetectCharset(Stream inputStream, bool charsetDetection)
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

        private byte[] fStreamBuffer, fLineBuffer;
        private int fStmBufLen, fStmBufPos, fLineBufPos;

        private void InitBuffers()
        {
            fStreamBuffer = new byte[SB_SIZE];
            fLineBuffer = new byte[LB_SIZE];
            fStmBufLen = 0;
            fStmBufPos = 0;
            fLineBufPos = 0;
        }

        private const byte LF = 0x0A; // \n
        private const byte CR = 0x0D; // \r

        private int ReadLine(Stream reader)
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
                byte ch = (fStmBufPos >= fStmBufLen) ? CR : fStreamBuffer[fStmBufPos];
                fStmBufPos += 1;

                if (ch == CR || ch == LF) {
                    int linePos = fLineBufPos;
                    fLineBufPos = 0;
                    if (linePos > 0) {
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

        protected override void ReadStream(Stream fileStream, Stream inputStream, bool charsetDetection = false)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                // encoding variables
                string streamCharset = DetectCharset(inputStream, charsetDetection);
                fEncoding = GetDefaultEncoding(inputStream);
                fEncodingState = EncodingState.esUnchecked;

                // reading variables
                var progressCallback = fTree.ProgressCallback;
                long fileSize = fileStream.Length;
                int progress = 0;
                InitBuffers();

                // parsing variables
                var invariantText = GEDCOMUtils.InvariantTextInfo;
                var strTok = new GEDCOMParser(false);
                GDMTag curRecord = null;
                GDMTag curTag = null;
                var stack = new Stack<StackTuple>(9);
                var header = fTree.Header;
                bool ilb = false;

                // check the integrity of utf8 the lines
                bool checkLI = false;
                byte dmgByte = 0, dmgBytePrev = 0;
                byte[] dmgBuf = new byte[2];
                char[] dmgChar = new char[2];

                int lineNum = 0, lineLen;
                char[] line = new char[LB_SIZE];
                while ((lineLen = ReadLine(inputStream)) != -1) {
                    lineNum++;

                    int tagLevel, tagId;
                    string tagXRef, tagName;
                    StringSpan tagValue;

                    try {
                        // damaged utf8 characters (FTB)
                        if (dmgByte != 0) {
                            dmgBytePrev = dmgByte;
                            dmgByte = 0;
                        }
                        if (lineLen > 0 && checkLI) {
                            byte lb = fLineBuffer[lineLen - 1];
                            if (SysUtils.IsDamagedUtf8Sequence(lb, true)) {
                                dmgByte = lb;
                                lineLen -= 1;
                            }
                        }

                        int charsLen = fEncoding.GetChars(fLineBuffer, 0, lineLen, line, 0);
                        strTok.Reset(line, 0, charsLen);
                        int lineRes = GEDCOMUtils.ParseTag(strTok, out tagLevel, out tagXRef, out tagName, out tagValue);

                        // empty line
                        if (lineRes == -2) continue;

                        // damaged utf8 characters (FTB)
                        if (dmgBytePrev != 0) {
                            // only ASCII characters (1b) can be before tagValue,
                            // so their length in characters is the same as their length in bytes
                            int pos = tagValue.Pos;
                            dmgBuf[0] = dmgBytePrev;
                            dmgBuf[1] = fLineBuffer[pos];
                            if (fEncoding.GetChars(dmgBuf, 0, 2, dmgChar, 0) > 0) {
                                tagValue.Data[pos] = dmgChar[0];
                            }
                            dmgBytePrev = 0;
                        }

                        // line with text but not in standard tag format
                        if (lineRes == -1) {
                            if (ilb) {
                                FixBreakedLine(fTree, curRecord, curTag, lineNum, tagValue);
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

                        if (curRecord == header && fEncodingState == EncodingState.esUnchecked) {
                            // beginning recognition of the first is not header record
                            // to check for additional versions of the code page
                            var format = GetGEDCOMFormat(fTree, out ilb);
                            fTree.Format = format;
                            DefineEncoding(inputStream, format, streamCharset);
                            checkLI = (format == GEDCOMFormat.FTB && Encoding.UTF8.Equals(fEncoding));
                        }

                        StackTuple stackTuple = AddTreeTag(fTree, tagLevel, tagId, tagValue);
                        if (stackTuple.Level >= 0) {
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
                            curTag = ProcessTag(fTree, stack, tagLevel, tagId, tagValue);
                        }
                    }

                    if (progressCallback != null) {
                        int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);
                        if (progress != newProgress) {
                            progress = newProgress;
                            progressCallback.StepTo(progress);
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

        internal static GDMTag ProcessTag(GDMTree tree, Stack<StackTuple> stack, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMTag curTag = null;

            GDMTag parentTag = null;
            TagHandler tagHandler = TagHandler.Null;
            while (stack.Count > 0) {
                var tuple = stack.Peek();
                if (tagLevel > tuple.Level) {
                    parentTag = tuple.Tag;
                    tagHandler = tuple.AddHandler;
                    break;
                }
                stack.Pop();
            }

            if (parentTag != null) {
                StackTuple tuple;
                if (tagHandler != TagHandler.Null) {
                    var handler = HandlersIndex[(int)tagHandler];
                    tuple = handler(tree, parentTag, tagLevel, tagId, tagValue);
                } else {
                    tuple = AddBaseTag(tree, parentTag, tagLevel, tagId, tagValue);
                }

                if (tuple.Level >= 0) {
                    stack.Push(tuple);
                    curTag = tuple.Tag;
                }
            }

            if (curTag == null) {
                curTag = parentTag;
            }

            return curTag;
        }

        protected static StackTuple AddTreeTag(GDMTree tree, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;
            GEDCOMTagType tagType = (GEDCOMTagType)tagId;

            if (tagType == GEDCOMTagType.INDI) {
                curTag = tree.AddRecord(new GDMIndividualRecord(tree));
                addHandler = TagHandler.IndividualRecordTag;

            } else if (tagType == GEDCOMTagType.FAM) {
                curTag = tree.AddRecord(new GDMFamilyRecord(tree));
                addHandler = TagHandler.FamilyRecordTag;

            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = tree.AddRecord(new GDMMultimediaRecord(tree));
                addHandler = TagHandler.MultimediaRecordTag;

            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = tree.AddRecord(new GDMNoteRecord(tree));
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteRecordTag;

            } else if (tagType == GEDCOMTagType.REPO) {
                curTag = tree.AddRecord(new GDMRepositoryRecord(tree));
                addHandler = TagHandler.RepositoryRecordTag;

            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = tree.AddRecord(new GDMSourceRecord(tree));
                addHandler = TagHandler.SourceRecordTag;

            } else if (tagType == GEDCOMTagType.SUBN) {
                curTag = tree.AddRecord(new GDMSubmissionRecord(tree));
                addHandler = TagHandler.SubmissionRecordTag;

            } else if (tagType == GEDCOMTagType.SUBM) {
                curTag = tree.AddRecord(new GDMSubmitterRecord(tree));
                addHandler = TagHandler.SubmitterRecordTag;

            } else if (tagType == GEDCOMTagType._GROUP) {
                curTag = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = TagHandler.GroupRecordTag;

            } else if ((tagType == GEDCOMTagType._GRP) && (tree.Format == GEDCOMFormat.Genney)) {
                curTag = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = TagHandler.GroupRecordTag;

            } else if (tagType == GEDCOMTagType._RESEARCH) {
                curTag = tree.AddRecord(new GDMResearchRecord(tree));
                addHandler = TagHandler.ResearchRecordTag;

            } else if (tagType == GEDCOMTagType._TASK) {
                curTag = tree.AddRecord(new GDMTaskRecord(tree));
                addHandler = TagHandler.TaskRecordTag;

            } else if (tagType == GEDCOMTagType._COMM) {
                curTag = tree.AddRecord(new GDMCommunicationRecord(tree));
                addHandler = TagHandler.CommunicationRecordTag;

            } else if (tagType == GEDCOMTagType._LOC) {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = TagHandler.LocationRecordTag;

            } else if ((tagType == GEDCOMTagType._PLAC) && (tree.Format == GEDCOMFormat.FamilyHistorian)) {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                ((GDMLocationRecord)curTag).LocationName = tagValue;
                addHandler = TagHandler.LocationRecordTag;

            } else if ((tagType == GEDCOMTagType._PLC) && (tree.Format == GEDCOMFormat.Genney)) {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = TagHandler.LocationRecordTag;

            } else if (tagType == GEDCOMTagType.HEAD) {
                curTag = tree.Header;
                addHandler = TagHandler.HeaderTag;

            } else if (tagType == GEDCOMTagType.TRLR) {
                curTag = null;

            } else {
                curTag = tree.AddRecord(new GDMRecord(tree));
                addHandler = TagHandler.RecordTag;
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        #endregion

        #region Saving functions

        public virtual void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!
            using (var fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write)) {
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
                for (int i = 0, num = list.Count; i < num; i++) {
                    GDMRecord record = list[i];
                    WriteRecordEx(writer, record);
                }
            }

            // write footer
            WriteTagLine(writer, 0, GEDCOMTagName.TRLR, string.Empty);
        }

        private static void WriteRecordEx(StreamWriter writer, GDMRecord record)
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
                    if (!Strict) WriteGroupRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtResearch:
                    if (!Strict) WriteResearchRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtTask:
                    if (!Strict) WriteTaskRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtCommunication:
                    if (!Strict) WriteCommunicationRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtLocation:
                    if (!Strict) WriteLocationRecord(writer, 0, record);
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

        private static StackTuple AddIndividualRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMIndividualRecord indiRec = (GDMIndividualRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FAMC) {
                curTag = indiRec.ChildToFamilyLinks.Add(new GDMChildToFamilyLink());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.ChildToFamilyLinkTag;
            } else if (tagType == GEDCOMTagType.FAMS) {
                curTag = indiRec.SpouseToFamilyLinks.Add(new GDMSpouseToFamilyLink());
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.NAME) {
                curTag = indiRec.AddPersonalName(new GDMPersonalName());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.PersonalNameTag;
            } else if (tagType == GEDCOMTagType.SEX) {
                indiRec.Sex = GEDCOMUtils.GetSexVal(tagValue);
            } else if (GEDCOMUtils.IsIndiEvent(tagType)) {
                curTag = indiRec.AddEvent(new GDMIndividualEvent(tagId, tagValue));
                addHandler = TagHandler.IndividualEventTag;
            } else if (GEDCOMUtils.IsIndiAttr(tagType)) {
                curTag = indiRec.AddEvent(new GDMIndividualAttribute(tagId, tagValue));
                addHandler = TagHandler.CustomEventTag;
            } else if (tagType == GEDCOMTagType.ASSO) {
                curTag = indiRec.Associations.Add(new GDMAssociation());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.AssociationTag;
            } else if (tagType == GEDCOMTagType._GROUP) {
                curTag = indiRec.Groups.Add(new GDMPointer(tagId, tagValue));
            } else if (tagType == GEDCOMTagType.ALIA) {
                var asso = new GDMAssociation();
                asso.ParseString(tagValue);
                asso.Relation = "possible_duplicate";
                curTag = indiRec.Associations.Add(asso);
                addHandler = TagHandler.AssociationTag;
            } else if (tagType == GEDCOMTagType._DNA) {
                curTag = indiRec.DNATests.Add(new GDMDNATest());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.DNATestTag;
            } else {
                return AddRecordWithEventsTag(tree, owner, tagLevel, tagId, tagValue);
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
            WriteList(stream, level, indiRec.SpouseToFamilyLinks, WriteBaseTag);

            if (indiRec.HasEvents) WriteList(stream, level, indiRec.Events, WriteIndividualEvent);
            if (indiRec.HasAssociations) WriteList(stream, level, indiRec.Associations, WriteAssociation);

            if (!Strict) {
                if (indiRec.HasGroups) WriteList(stream, level, indiRec.Groups, WriteBaseTag);
                if (indiRec.HasDNATests) WriteList(stream, level, indiRec.DNATests, WriteDNATest);
            }
        }


        private static StackTuple AddFamilyRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMFamilyRecord famRec = (GDMFamilyRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.HUSB) {
                curTag = famRec.Husband;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.WIFE) {
                curTag = famRec.Wife;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.CHIL) {
                curTag = famRec.Children.Add(new GDMChildLink(tagId, tagValue));
            } else if (tagType == GEDCOMTagType._STAT) {
                famRec.Status = GEDCOMUtils.GetMarriageStatusVal(tagValue);
            } else if (GEDCOMUtils.IsFamEvent(tagType)) {
                curTag = famRec.AddEvent(new GDMFamilyEvent(tagId, tagValue));
                addHandler = TagHandler.FamilyEventTag;
            } else {
                return AddRecordWithEventsTag(tree, owner, tagLevel, tagId, tagValue);
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
            if (!Strict) WriteTagLine(stream, level, GEDCOMTagName._STAT, GEDCOMUtils.GetMarriageStatusStr(famRec.Status), true);

            WriteList(stream, level, famRec.Children, WriteBaseTag);

            if (famRec.HasEvents) WriteList(stream, level, famRec.Events, WriteFamilyEvent);
        }


        private static StackTuple AddRecordWithEventsTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMRecordWithEvents evtRec = (GDMRecordWithEvents)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.RESN) {
                evtRec.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddGroupRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                groupRec.GroupName = tagValue;
            } else if (tagType == GEDCOMTagType._MEMBER) {
                curTag = groupRec.Members.Add(new GDMIndividualLink(tagId, tagValue));
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteGroupRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)tag;

            WriteRecord(stream, level, groupRec);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, groupRec.GroupName, true);
            WriteList(stream, level, groupRec.Members, WriteBaseTag);
        }


        private static StackTuple AddMultimediaRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FILE) {
                curTag = mmRec.FileReferences.Add(new GDMFileReferenceWithTitle());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.FileReferenceWithTitleTag;
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteMultimediaRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)tag;

            WriteRecord(stream, level, mmRec);
            WriteList(stream, ++level, mmRec.FileReferences, WriteFileReferenceWithTitle);
        }


        private static StackTuple AddSourceRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSourceRecord sourRec = (GDMSourceRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.REPO) {
                curTag = sourRec.RepositoryCitations.Add(new GDMRepositoryCitation());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.RepositoryCitationTag;
            } else if (tagType == GEDCOMTagType.DATA) {
                curTag = sourRec.Data;
                addHandler = TagHandler.SourceDataTag;
            } else if (tagType == GEDCOMTagType.AUTH) {
                curTag = sourRec.Originator;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.TextTag;
            } else if (tagType == GEDCOMTagType.PUBL) {
                curTag = sourRec.Publication;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.TextTag;
            } else if (tagType == GEDCOMTagType.ABBR) {
                sourRec.ShortTitle = tagValue;
            } else if (tagType == GEDCOMTagType.TEXT) {
                curTag = sourRec.Text;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.TextTag;
            } else if (tagType == GEDCOMTagType.TITL) {
                curTag = sourRec.Title;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.TextTag;
            } else if (tagType == GEDCOMTagType._DATE) {
                curTag = sourRec.Date;
                GEDCOMUtils.ParseDateValue(tree, sourRec.Date, tagValue);
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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
            WriteList(stream, level, sourRec.RepositoryCitations, WriteRepositoryCitation);

            WriteSourceData(stream, level, sourRec.Data);
            WriteText(stream, level, sourRec.Originator);
            WriteText(stream, level, sourRec.Text);

            if (!Strict) WriteBaseTag(stream, level, sourRec.Date);
        }


        private static StackTuple AddRepositoryCitationTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMRepositoryCitation repCit = (GDMRepositoryCitation)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE) {
                curTag = repCit.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else if (tagType == GEDCOMTagType.CALN) {
                curTag = repCit.CallNumbers.Add(new GDMSourceCallNumber());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.SourceCallNumberTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteRepositoryCitation(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRepositoryCitation repCit = (GDMRepositoryCitation)tag;

            if (!WriteBaseTag(stream, level, tag)) return false;

            level += 1;
            if (repCit.HasNotes) WriteList(stream, level, repCit.Notes, WriteNote);
            if (repCit.HasCallNumbers) WriteList(stream, level, repCit.CallNumbers, WriteSourceCallNumber);
            return true;
        }


        private static StackTuple AddSourceCallNumberTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSourceCallNumber fileRef = (GDMSourceCallNumber)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.MEDI) {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteSourceCallNumber(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceCallNumber callNum = (GDMSourceCallNumber)tag;

            if (!WriteBaseTag(stream, level, callNum)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.MEDI, GEDCOMUtils.GetMediaTypeStr(callNum.MediaType), true);
            return true;
        }


        private static StackTuple AddResearchRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMResearchRecord resRec = (GDMResearchRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

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
                GEDCOMUtils.ParseDate(resRec.StartDate, tagValue);
            } else if (tagType == GEDCOMTagType._STOPDATE) {
                curTag = resRec.StopDate;
                GEDCOMUtils.ParseDate(resRec.StopDate, tagValue);
            } else if (tagType == GEDCOMTagType._TASK) {
                curTag = resRec.Tasks.Add(new GDMPointer(tagId, tagValue));
            } else if (tagType == GEDCOMTagType._COMM) {
                curTag = resRec.Communications.Add(new GDMPointer(tagId, tagValue));
            } else if (tagType == GEDCOMTagType._GROUP) {
                curTag = resRec.Groups.Add(new GDMPointer(tagId, tagValue));
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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

            WriteList(stream, level, resRec.Tasks, WriteBaseTag);
            WriteList(stream, level, resRec.Communications, WriteBaseTag);
            WriteList(stream, level, resRec.Groups, WriteBaseTag);
        }


        private static StackTuple AddNoteRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMNoteRecord noteRec = (GDMNoteRecord)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType)) {
                return AddTextTag(tree, noteRec, tagLevel, tagId, tagValue);
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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

            if (noteRec.HasSourceCitations) WriteList(stream, level, noteRec.SourceCitations, WriteSourceCitation);
            if (noteRec.HasUserReferences) WriteList(stream, level, noteRec.UserReferences, WriteUserReference);
        }


        private static StackTuple AddRepositoryRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMRepositoryRecord repoRec = (GDMRepositoryRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                repoRec.RepositoryName = tagValue;
            } else if (tagType == GEDCOMTagType.ADDR) {
                curTag = repoRec.Address;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.AddressTag;
            } else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW) {
                return AddAddressTag(tree, repoRec.Address, tagLevel, tagId, tagValue);
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddTaskRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMTaskRecord taskRec = (GDMTaskRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._GOAL) {
                taskRec.Goal = tagValue;
            } else if (tagType == GEDCOMTagType._PRIORITY) {
                taskRec.Priority = GEDCOMUtils.GetPriorityVal(tagValue);
            } else if (tagType == GEDCOMTagType._STARTDATE) {
                curTag = taskRec.StartDate;
                GEDCOMUtils.ParseDate(taskRec.StartDate, tagValue);
            } else if (tagType == GEDCOMTagType._STOPDATE) {
                curTag = taskRec.StopDate;
                GEDCOMUtils.ParseDate(taskRec.StopDate, tagValue);
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddCommunicationRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMCommunicationRecord commRec = (GDMCommunicationRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

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
                GEDCOMUtils.ParseDate(commRec.Date, tagValue);
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddSubmissionRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSubmissionRecord submnRec = (GDMSubmissionRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

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
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddSubmitterRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                submrRec.Name = tagValue;
                return SkipReaderStackTuple(tagLevel, null, null);
            } else if (tagType == GEDCOMTagType.ADDR) {
                curTag = submrRec.Address;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.AddressTag;
            } else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW) {
                return AddAddressTag(tree, submrRec.Address, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.LANG) {
                curTag = submrRec.AddLanguage(new GDMLanguage(tagId, tagValue));
            } else if (tagType == GEDCOMTagType.RFN) {
                submrRec.RegisteredReference = tagValue;
            } else {
                // 'ADDR' defines by default
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteSubmitterRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)tag;

            WriteRecord(stream, level, submrRec);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, submrRec.Name, true);
            WriteList(stream, level, submrRec.Languages, WriteBaseTag);
            WriteAddress(stream, level, submrRec.Address);
            WriteTagLine(stream, level, GEDCOMTagName.RFN, submrRec.RegisteredReference, true);
        }


        private static StackTuple AddRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMRecord record = (GDMRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._UID) {
                record.UID = tagValue;
            } else if (tagType == GEDCOMTagType.CHAN) {
                curTag = record.ChangeDate;
                addHandler = TagHandler.ChangeDateTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = record.SourceCitations.Add(new GDMSourceCitation());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.SourceCitationTag;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = record.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = record.MultimediaLinks.Add(new GDMMultimediaLink());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.MultimediaLinkTag;
            } else if (tagType == GEDCOMTagType.REFN) {
                curTag = record.UserReferences.Add(new GDMUserReference());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.UserReferenceTag;
            } else if (tagType == GEDCOMTagType.RIN) {
                record.AutomatedRecordID = tagValue;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRecord record = (GDMRecord)tag;

            WriteRecordValue(stream, level, record);

            level += 1;
            if (!DebugWrite) {
                if (!Strict) WriteTagLine(stream, level, GEDCOMTagName._UID, record.UID, true);
                WriteChangeDate(stream, level, record.ChangeDate);
            }
            WriteSubTags(stream, level, tag);

            WriteTagLine(stream, level, GEDCOMTagName.RIN, record.AutomatedRecordID, true);

            if (record.HasNotes) WriteList(stream, level, record.Notes, WriteNote);
            if (record.HasSourceCitations) WriteList(stream, level, record.SourceCitations, WriteSourceCitation);
            if (record.HasMultimediaLinks) WriteList(stream, level, record.MultimediaLinks, WriteMultimediaLink);
            if (record.HasUserReferences) WriteList(stream, level, record.UserReferences, WriteUserReference);
        }


        private static StackTuple AddHeaderTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMHeader header = (GDMHeader)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.CHAR) {
                curTag = header.CharacterSet;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.HeaderCharSetTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = header.Source;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.HeaderSourceTag;
            } else if (tagType == GEDCOMTagType.GEDC) {
                curTag = header.GEDCOM;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.HeaderGEDCOMTag;
            } else if (tagType == GEDCOMTagType.LANG) {
                header.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            } else if (tagType == GEDCOMTagType.COPR) {
                header.Copyright = tagValue;
            } else if (tagType == GEDCOMTagType.DEST) {
                header.ReceivingSystemName = tagValue;
            } else if (tagType == GEDCOMTagType.PLAC) {
                curTag = header.Place;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.PlaceTag;
            } else if (tagType == GEDCOMTagType.SUBM) {
                curTag = header.Submitter;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.SUBN) {
                curTag = header.Submission;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = header.Note;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.TextTag;
            } else if (tagType == GEDCOMTagType.DATE) {
                DateTime date;
                GEDCOMUtils.ParseDate(tree, tagValue, out date);
                header.TransmissionDateTime = date;
                curTag = header;
                addHandler = TagHandler.HeaderTag;
            } else if (tagType == GEDCOMTagType.TIME) {
                TimeSpan time;
                GEDCOMUtils.ParseTime(tagValue, out time);
                DateTime date = header.TransmissionDateTime;
                header.TransmissionDateTime = date.Add(time);
            } else if (tagType == GEDCOMTagType.FILE) {
                curTag = header.File;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.HeaderFileTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddHeaderSourceTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMHeaderSource headerSource = (GDMHeaderSource)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS) {
                headerSource.Version = tagValue;
            } else if (tagType == GEDCOMTagType.NAME) {
                headerSource.ProductName = tagValue;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddHeaderGEDCOMTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMHeaderGEDCOM headerGEDCOM = (GDMHeaderGEDCOM)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS) {
                headerGEDCOM.Version = tagValue;
            } else if (tagType == GEDCOMTagType.FORM) {
                headerGEDCOM.Form = tagValue;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddHeaderCharSetTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMHeaderCharSet headerCharSet = (GDMHeaderCharSet)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS) {
                headerCharSet.Version = tagValue;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddHeaderFileTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMHeaderFile headerFile = (GDMHeaderFile)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._UID) {
                headerFile.UID = tagValue;
            } else if (tagType == GEDCOMTagType._REV) {
                headerFile.Revision = GEDCOMUtils.GetIntVal(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteHeaderFile(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderFile headerFile = (GDMHeaderFile)tag;

            if (!WriteBaseTag(stream, level, headerFile)) return false;

            if (!Strict) {
                level += 1;
                if (!DebugWrite) {
                    WriteTagLine(stream, level, GEDCOMTagName._UID, headerFile.UID, true);
                }
                WriteTagLine(stream, level, GEDCOMTagName._REV, GEDCOMUtils.GetIntStr(headerFile.Revision), true);
            }

            return true;
        }


        private static StackTuple AddChangeDateTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMChangeDate changeDate = (GDMChangeDate)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                DateTime date;
                GEDCOMUtils.ParseDate(tree, tagValue, out date);
                changeDate.ChangeDateTime = date;
                curTag = changeDate;
                addHandler = TagHandler.ChangeDateTag;
            } else if (tagType == GEDCOMTagType.TIME) {
                TimeSpan time;
                GEDCOMUtils.ParseTime(tagValue, out time);
                DateTime date = changeDate.ChangeDateTime;
                changeDate.ChangeDateTime = date.Add(time);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = changeDate.AddTag(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddCustomEventTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE) {
                custEvent.Classification = tagValue;
            } else if (tagType == GEDCOMTagType.DATE) {
                curTag = custEvent.Date;
                GEDCOMUtils.ParseDateValue(tree, custEvent.Date, tagValue);
            } else if (tagType == GEDCOMTagType.PLAC) {
                curTag = custEvent.Place;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.PlaceTag;
            } else if (tagType == GEDCOMTagType.AGNC) {
                custEvent.Agency = tagValue;
            } else if (tagType == GEDCOMTagType.CAUS) {
                custEvent.Cause = tagValue;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = custEvent.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = custEvent.SourceCitations.Add(new GDMSourceCitation());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.SourceCitationTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = custEvent.MultimediaLinks.Add(new GDMMultimediaLink());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.MultimediaLinkTag;
            } else if (tagType == GEDCOMTagType.ADDR) {
                curTag = custEvent.Address;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.AddressTag;
            } else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW) {
                return AddAddressTag(tree, custEvent.Address, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.RELI) {
                custEvent.ReligiousAffilation = tagValue;
            } else if (tagType == GEDCOMTagType.RESN) {
                custEvent.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddIndividualEventTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMIndividualEvent indiEvent = (GDMIndividualEvent)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.AGE) {
                indiEvent.Age.ParseString(tagValue);
            } else {
                return AddCustomEventTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddFamilyEventTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMFamilyEvent famEvent = (GDMFamilyEvent)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.HUSB) {
                curTag = famEvent.HusbandAge;
                addHandler = TagHandler.AgeTag;
            } else if (tagType == GEDCOMTagType.WIFE) {
                curTag = famEvent.WifeAge;
                addHandler = TagHandler.AgeTag;
            } else {
                return AddCustomEventTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddAgeTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.AGE) {
                ((GDMAge)owner).ParseString(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, null, TagHandler.Null);
        }

        private static bool WriteCustomEvent(StreamWriter stream, int level, GDMTag tag)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)tag;

            if (!WriteBaseTag(stream, level, custEvent)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.TYPE, custEvent.Classification, true);
            WriteBaseTag(stream, level, custEvent.Date);

            if (custEvent.HasPlace) WritePlace(stream, level, custEvent.Place);
            if (custEvent.HasAddress) WriteAddress(stream, level, custEvent.Address);

            WriteTagLine(stream, level, GEDCOMTagName.CAUS, custEvent.Cause, true);
            WriteTagLine(stream, level, GEDCOMTagName.AGNC, custEvent.Agency, true);
            WriteTagLine(stream, level, GEDCOMTagName.RELI, custEvent.ReligiousAffilation, true);
            WriteTagLine(stream, level, GEDCOMTagName.RESN, GEDCOMUtils.GetRestrictionStr(custEvent.Restriction), true);

            if (custEvent.HasNotes) WriteList(stream, level, custEvent.Notes, WriteNote);
            if (custEvent.HasSourceCitations) WriteList(stream, level, custEvent.SourceCitations, WriteSourceCitation);
            if (custEvent.HasMultimediaLinks) WriteList(stream, level, custEvent.MultimediaLinks, WriteMultimediaLink);

            return true;
        }

        private static bool WriteIndividualEvent(StreamWriter stream, int level, GDMTag tag)
        {
            if (!WriteCustomEvent(stream, level, tag)) return false;

            // GDMIndividualEvent | GDMIndividualAttribute
            if (tag is GDMIndividualEvent indiEvent) {
                level += 1;
                if (indiEvent.HasAge) WriteTagLine(stream, level, GEDCOMTagName.AGE, indiEvent.Age.StringValue, true);
            }

            return true;
        }

        private static bool WriteFamilyEvent(StreamWriter stream, int level, GDMTag tag)
        {
            if (!WriteCustomEvent(stream, level, tag)) return false;

            GDMFamilyEvent famEvent = (GDMFamilyEvent)tag;

            level += 1;
            var sublevel = level + 1;

            if (famEvent.HasHusbandAge) {
                WriteTagLine(stream, level, GEDCOMTagName.HUSB, string.Empty);
                WriteTagLine(stream, sublevel, GEDCOMTagName.AGE, famEvent.HusbandAge.StringValue, true);
            }

            if (famEvent.HasWifeAge) {
                WriteTagLine(stream, level, GEDCOMTagName.WIFE, string.Empty);
                WriteTagLine(stream, sublevel, GEDCOMTagName.AGE, famEvent.WifeAge.StringValue, true);
            }

            return true;
        }


        private static StackTuple AddSourceDataTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSourceData sourData = (GDMSourceData)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.EVEN) {
                curTag = sourData.Events.Add(new GDMSourceEvent());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.SourceDataEventTag;
            } else if (tagType == GEDCOMTagType.AGNC) {
                sourData.Agency = tagValue;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = sourData.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteSourceData(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceData sourData = (GDMSourceData)tag;

            if (!WriteBaseTag(stream, level, tag)) return false;

            level += 1;

            if (sourData.HasNotes) WriteList(stream, level, sourData.Notes, WriteNote);

            WriteTagLine(stream, level, GEDCOMTagName.AGNC, sourData.Agency, true);
            WriteList(stream, level, sourData.Events, WriteSourceDataEvent);
            return true;
        }


        private static StackTuple AddSourceDataEventTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                curTag = dataEvent.Date;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.PLAC) {
                curTag = dataEvent.Place;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.PlaceTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteSourceDataEvent(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)tag;

            if (!WriteBaseTag(stream, level, dataEvent)) return false;

            level += 1;
            WriteBaseTag(stream, level, dataEvent.Date);
            if (dataEvent.HasPlace) WritePlace(stream, level, dataEvent.Place);
            return true;
        }

        private static StackTuple AddPointerWithNotesTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMPointerWithNotes ptrWN = (GDMPointerWithNotes)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE) {
                curTag = ptrWN.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }


        private static bool IsTextTag(GEDCOMTagType tag)
        {
            return (tag == GEDCOMTagType.CONT || tag == GEDCOMTagType.CONC);
        }

        private static StackTuple AddTextTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            IGDMTextObject textTag = (IGDMTextObject)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            var strings = textTag.Lines;
            if (tagType == GEDCOMTagType.CONC) {
                int strCount = strings.Count;
                if (strCount > 0) {
                    strings[strCount - 1] = string.Concat(strings[strCount - 1], tagValue);
                } else {
                    strings.Add(tagValue);
                }
            } else if (tagType == GEDCOMTagType.CONT || tagType == GEDCOMTagType.TEXT) {
                strings.Add(tagValue);
            }

            return CreateReaderStackTuple(tagLevel, null, TagHandler.Null);
        }

        private static int CheckLineLength(string str)
        {
            int len = Math.Min(str.Length, GEDCOMConsts.MaxLineLength);
            // Fix for disappearing spaces when they are at the beginning of the tag value
            // (tag parser skips all spaces between tag's name and value tokens).
            // It is undesirable to change the logic of the parser or the functions of merging these parts -
            // there will be much more exceptions than in this case.
            if (len > 0 && len < str.Length && str[len] == ' ') {
                len--;
            }
            return len;
        }

        private static bool WriteText(StreamWriter stream, int level, IGDMTextObject textTag, bool skipTag = false)
        {
            if (textTag.IsEmpty()) return false;

            var strings = textTag.Lines;
            int strCount = strings.Count;
            for (int i = 0; i < strCount; i++) {
                string str = strings[i];

                int len = CheckLineLength(str);
                string sub = str.Substring(0, len);
                str = str.Remove(0, len);

                if (i == 0 && !skipTag) {
                    WriteTagLine(stream, level, ((GDMTag)textTag).GetTagName(), sub);
                    level += 1;
                } else {
                    WriteTagLine(stream, level, GEDCOMTagName.CONT, sub);
                }

                while (str.Length > 0) {
                    len = CheckLineLength(str);

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
                        WriteBaseTag(stream, level, subtag);
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

        private static StackTuple AddBaseTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMTag curTag = owner.AddTag(new GDMValueTag(tagId, tagValue));

            return CreateReaderStackTuple(tagLevel, curTag, TagHandler.Null);
        }

        private static StackTuple CreateReaderStackTuple(int level, GDMTag tag, TagHandler addHandler)
        {
            if (tag == null) {
                return new StackTuple(-1, null, TagHandler.Null);
            } else {
                if (addHandler == TagHandler.Null) {
                    addHandler = TagHandler.BaseTag;
                }
                return new StackTuple(level, tag, addHandler);
            }
        }

        private static StackTuple SkipTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            return SkipReaderStackTuple(tagLevel, null, null);
        }

        private static StackTuple SkipReaderStackTuple(int level, GDMTag tag, AddTagHandler addHandler)
        {
            return new StackTuple(level, null, TagHandler.SkipTag);
        }

        private static bool WriteBaseTag(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            WriteTagValue(stream, level, tag);
            WriteSubTags(stream, ++level, tag);
            return true;
        }

        private static bool SkipEmptyTag(int tagId)
        {
            GEDCOMTagProps tagProps = GEDCOMTagsTable.GetTagProps(tagId);
            return (tagProps != null && tagProps.SkipEmpty);
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


        private static StackTuple AddLocationRecordTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMLocationRecord locRec = (GDMLocationRecord)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                curTag = locRec.Names.Add(new GDMLocationName());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.LocationNameTag;
            } else if (tagType == GEDCOMTagType._LOC) {
                curTag = locRec.TopLevels.Add(new GDMLocationLink());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.LocationLinkTag;
            } else if (tagType == GEDCOMTagType.MAP) {
                curTag = locRec.Map;
                addHandler = TagHandler.MapTag;
            } else {
                return AddRecordTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static void WriteLocationRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMLocationRecord locRec = (GDMLocationRecord)tag;

            WriteRecord(stream, level, locRec);

            level += 1;
            WriteMap(stream, level, locRec.Map);

            WriteList(stream, level, locRec.Names, WriteLocationName);
            WriteList(stream, level, locRec.TopLevels, WriteLocationLink);
        }


        private static StackTuple AddLocationNameTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMLocationName locName = (GDMLocationName)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.ABBR) {
                locName.Abbreviation = tagValue;
            } else if (tagType == GEDCOMTagType.DATE) {
                curTag = locName.Date;
                GEDCOMUtils.ParseDateValue(tree, locName.Date, tagValue);
            } else if (tagType == GEDCOMTagType.LANG) {
                locName.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            } else {
                return AddBaseTag(tree, locName, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteLocationName(StreamWriter stream, int level, GDMTag tag)
        {
            GDMLocationName locName = (GDMLocationName)tag;

            if (!WriteBaseTag(stream, level, locName)) return false;

            int lev = level + 1;
            WriteTagLine(stream, lev, GEDCOMTagName.ABBR, locName.Abbreviation, true);
            WriteBaseTag(stream, lev, locName.Date);
            WriteTagLine(stream, lev, GEDCOMTagName.LANG, GEDCOMUtils.GetLanguageStr(locName.Language), true);

            return true;
        }


        private static StackTuple AddLocationLinkTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMLocationLink locLink = (GDMLocationLink)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                curTag = locLink.Date;
                GEDCOMUtils.ParseDateValue(tree, locLink.Date, tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteLocationLink(StreamWriter stream, int level, GDMTag tag)
        {
            GDMLocationLink locLink = (GDMLocationLink)tag;

            if (!WriteBaseTag(stream, level, locLink)) return false;

            level += 1;
            WriteBaseTag(stream, level, locLink.Date);
            return true;
        }


        private static StackTuple AddPlaceTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMPlace place = (GDMPlace)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FORM) {
                place.Form = tagValue;
            } else if (tagType == GEDCOMTagType.MAP) {
                curTag = place.Map;
                addHandler = TagHandler.MapTag;
            } else if (tagType == GEDCOMTagType._LOC) {
                curTag = place.Location;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = place.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WritePlace(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPlace place = (GDMPlace)tag;

            if (!WriteBaseTag(stream, level, tag)) return false;

            level += 1;

            if (place.HasNotes) WriteList(stream, level, place.Notes, WriteNote);

            if (!Strict) WriteBaseTag(stream, level, place.Location);

            WriteMap(stream, level, place.Map);
            WriteTagLine(stream, level, GEDCOMTagName.FORM, place.Form, true);

            return true;
        }


        private static StackTuple AddMapTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMMap map = (GDMMap)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.LATI) {
                map.Lati = GEDCOMUtils.GetGeoCoord(tagValue, GEDCOMGeoCoord.Lati);
            } else if (tagType == GEDCOMTagType.LONG) {
                map.Long = GEDCOMUtils.GetGeoCoord(tagValue, GEDCOMGeoCoord.Long);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteMap(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMap map = (GDMMap)tag;
            if (map.IsEmpty() && GEDCOMProvider.SkipEmptyTag(map.Id)) return false;

            WriteTagValue(stream, level, tag);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.LATI, GEDCOMUtils.CoordToStr(map.Lati, GEDCOMGeoCoord.Lati, Strict), true);
            WriteTagLine(stream, level, GEDCOMTagName.LONG, GEDCOMUtils.CoordToStr(map.Long, GEDCOMGeoCoord.Long, Strict), true);
            return true;
        }


        private static StackTuple AddUserReferenceTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMUserReference userRef = (GDMUserReference)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE) {
                userRef.ReferenceType = tagValue;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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
        private static StackTuple AddFileReferenceWithTitleTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMFileReferenceWithTitle fileRef = (GDMFileReferenceWithTitle)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TITL) {
                fileRef.Title = tagValue;
            } else if (tagType == GEDCOMTagType.FORM) {
                fileRef.MultimediaFormat = tagValue;
                curTag = fileRef;
                addHandler = TagHandler.FileReferenceWithTitleTag;
            } else if (tagType == GEDCOMTagType.TYPE) {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        // Format: FORM\TYPE
        private static bool WriteFileReferenceWithTitle(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFileReferenceWithTitle fileRef = (GDMFileReferenceWithTitle)tag;

            if (!WriteBaseTag(stream, level, fileRef)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.FORM, fileRef.MultimediaFormat, true);
            GEDCOMProvider.WriteTagLine(stream, (level+1), GEDCOMTagName.TYPE, GEDCOMUtils.GetMediaTypeStr(fileRef.MediaType), true);
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.TITL, fileRef.Title, true);
            return true;
        }


        // Format: FORM\MEDI
        private static StackTuple AddFileReferenceTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMFileReference fileRef = (GDMFileReference)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FORM) {
                fileRef.MultimediaFormat = tagValue;
                curTag = fileRef;
                addHandler = TagHandler.FileReferenceTag;
            } else if (tagType == GEDCOMTagType.MEDI) {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        // Format: FORM\MEDI
        private static bool WriteFileReference(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFileReference fileRef = (GDMFileReference)tag;

            if (!WriteBaseTag(stream, level, fileRef)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.FORM, fileRef.MultimediaFormat, true);
            GEDCOMProvider.WriteTagLine(stream, ++level, GEDCOMTagName.MEDI, GEDCOMUtils.GetMediaTypeStr(fileRef.MediaType), true);
            return true;
        }


        private static StackTuple AddMultimediaLinkTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TITL) {
                mmLink.Title = tagValue;
            } else if (tagType == GEDCOMTagType._PRIM) {
                mmLink.IsPrimary = GEDCOMUtils.GetBoolVal(tagValue);
            } else if (tagType == GEDCOMTagType._PRIM_CUTOUT) {
                mmLink.IsPrimaryCutout = GEDCOMUtils.GetBoolVal(tagValue);
            } else if (tagType == GEDCOMTagType._POSITION) {
                curTag = mmLink.CutoutPosition;
                GEDCOMUtils.ParseCutoutPosition(tagValue, mmLink.CutoutPosition);
            } else if (tagType == GEDCOMTagType.FILE) {
                curTag = mmLink.FileReferences.Add(new GDMFileReference());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.FileReferenceTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteMultimediaLink(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)tag;

            if (!WriteBaseTag(stream, level, mmLink)) return false;

            level += 1;
            WriteList(stream, level, mmLink.FileReferences, WriteFileReference);
            WriteTagLine(stream, level, GEDCOMTagName.TITL, mmLink.Title, true);

            if (!Strict) {
                if (mmLink.IsPrimary) WriteTagLine(stream, level, GEDCOMTagName._PRIM, GEDCOMUtils.GetBoolStr(mmLink.IsPrimary), true);
                if (mmLink.IsPrimaryCutout) WriteTagLine(stream, level, GEDCOMTagName._PRIM_CUTOUT, GEDCOMUtils.GetBoolStr(mmLink.IsPrimaryCutout), true);
                WriteBaseTag(stream, level, mmLink.CutoutPosition);
            }

            return true;
        }


        private static StackTuple AddNoteTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMNotes note = (GDMNotes)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType)) {
                return AddTextTag(tree, note, tagLevel, tagId, tagValue);
            } else {
                return AddBaseTag(tree, note, tagLevel, tagId, tagValue);
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


        private static StackTuple AddSourceCitationTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSourceCitation sourCit = (GDMSourceCitation)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.QUAY) {
                sourCit.CertaintyAssessment = GEDCOMUtils.GetIntVal(tagValue);
            } else if (tagType == GEDCOMTagType.PAGE) {
                sourCit.Page = tagValue;
            } else if (tagType == GEDCOMTagType.TEXT) {
                curTag = sourCit.Text;
                curTag.ParseString(tagValue);
                addHandler = TagHandler.TextTag;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = sourCit.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = sourCit.MultimediaLinks.Add(new GDMMultimediaLink());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.MultimediaLinkTag;
            } else if (tagType == GEDCOMTagType.DATA) {
                curTag = sourCit.Data;
                addHandler = TagHandler.SourceCitationDataTag;
            } else if (IsTextTag(tagType)) {
                return AddTextTag(tree, sourCit, tagLevel, tagId, tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteSourceCitation(StreamWriter stream, int level, GDMTag tag)
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

            if (sourCit.HasNotes) WriteList(stream, level, sourCit.Notes, WriteNote);
            if (sourCit.HasMultimediaLinks) WriteList(stream, level, sourCit.MultimediaLinks, WriteMultimediaLink);

            return true;
        }


        private static StackTuple AddSourceCitationDataTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMSourceCitationData srcitData = (GDMSourceCitationData)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE) {
                curTag = srcitData.Date;
                curTag.ParseString(tagValue);
            } else if (tagType == GEDCOMTagType.TEXT) {
                curTag = srcitData.Text;
                ((GDMTextTag)curTag).Lines.Add(tagValue);
                // without curTag.ParseString(tagValue) because next TEXT
                // will rewrite all previous lines
                // GK will be without support for a list of text chunks
                addHandler = TagHandler.TextTag;
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddAssociationTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMAssociation asso = (GDMAssociation)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.RELA) {
                asso.Relation = tagValue;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = asso.SourceCitations.Add(new GDMSourceCitation());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.SourceCitationTag;
            } else {
                return AddPointerWithNotesTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteAssociation(StreamWriter stream, int level, GDMTag tag)
        {
            GDMAssociation asso = (GDMAssociation)tag;

            if (!WriteBaseTag(stream, level, asso)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.RELA, asso.Relation, true);

            if (asso.HasSourceCitations) WriteList(stream, level, asso.SourceCitations, WriteSourceCitation);

            return true;
        }


        private static StackTuple AddAddressTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMAddress addr = (GDMAddress)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

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
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
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

            WriteList(stream, level, addr.PhoneNumbers, WriteBaseTag);
            WriteList(stream, level, addr.EmailAddresses, WriteBaseTag);
            WriteList(stream, level, addr.FaxNumbers, WriteBaseTag);
            WriteList(stream, level, addr.WebPages, WriteBaseTag);
            return true;
        }


        private static StackTuple AddChildToFamilyLinkTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMChildToFamilyLink cfl = (GDMChildToFamilyLink)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.STAT) {
                cfl.ChildLinkageStatus = GEDCOMUtils.GetChildLinkageStatusVal(tagValue);
            } else if (tagType == GEDCOMTagType.PEDI) {
                cfl.PedigreeLinkageType = GEDCOMUtils.GetPedigreeLinkageTypeVal(tagValue);
            } else {
                return AddPointerWithNotesTag(tree, owner, tagLevel, tagId, tagValue);
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


        private static StackTuple AddPersonalNameTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMPersonalName persName = (GDMPersonalName)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE) {
                persName.NameType = GEDCOMUtils.GetNameTypeVal(tagValue);
            } else if (tagType == GEDCOMTagType._LANG || tagType == GEDCOMTagType.LANG) {
                persName.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            } else if (tagType == GEDCOMTagType.FONE || tagType == GEDCOMTagType.ROMN) {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = persName.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else if (tagType == GEDCOMTagType.SOUR) {
                curTag = persName.SourceCitations.Add(new GDMSourceCitation());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.SourceCitationTag;
            } else {
                return AddPersonalNamePiecesTag(tree, persName, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WritePersonalName(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPersonalName persName = (GDMPersonalName)tag;

            if (!WriteBaseTag(stream, level, persName)) return false;

            int lev = level + 1;
            if (!Strict) WriteTagLine(stream, lev, GEDCOMTagName.LANG, GEDCOMUtils.GetLanguageStr(persName.Language), true);
            WriteTagLine(stream, lev, GEDCOMTagName.TYPE, GEDCOMUtils.GetNameTypeStr(persName.NameType), true);

            if (persName.HasNotes) WriteList(stream, lev, persName.Notes, WriteNote);
            if (persName.HasSourceCitations) WriteList(stream, lev, persName.SourceCitations, WriteSourceCitation);

            WritePersonalNamePieces(stream, lev, persName);
            return true;
        }


        private static StackTuple AddPersonalNamePiecesTag(GDMTree tree, GDMPersonalName persName, int tagLevel, int tagId, StringSpan tagValue)
        {
            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NPFX) {
                persName.NamePrefix = tagValue;
            } else if (tagType == GEDCOMTagType.GIVN) {
                persName.Given = tagValue;
            } else if (tagType == GEDCOMTagType.NICK) {
                persName.Nickname = tagValue;
            } else if (tagType == GEDCOMTagType.SPFX) {
                persName.SurnamePrefix = tagValue;
            } else if (tagType == GEDCOMTagType.SURN) {
                persName.Surname = tagValue;
            } else if (tagType == GEDCOMTagType.NSFX) {
                persName.NameSuffix = tagValue;
            } else if (tagType == GEDCOMTagType._PATN || tagType == GEDCOMTagType._MIDN) {
                persName.PatronymicName = tagValue;
            } else if (tagType == GEDCOMTagType._MARN || tagType == GEDCOMTagType._MARNM) {
                persName.MarriedName = tagValue;
            } else if (tagType == GEDCOMTagType._RELN) {
                persName.ReligiousName = tagValue;
            } else if (tagType == GEDCOMTagType._CENN) {
                persName.CensusName = tagValue;
            } else {
                return AddBaseTag(tree, persName, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, null, TagHandler.Null);
        }

        private static void WritePersonalNamePieces(StreamWriter stream, int level, GDMPersonalName persName)
        {
            // Given (first + middle (second and subsequent) name) and surname are always included in the NAME tag,
            // and without modification tags of NAME - not required.
            // The second and subsequent parts of the middle name (patronymic) can be extracted from given name if necessary.
            if (KeepRichNames ||
                !string.IsNullOrEmpty(persName.NamePrefix) || !string.IsNullOrEmpty(persName.SurnamePrefix) || !string.IsNullOrEmpty(persName.NameSuffix)) {
                WriteTagLine(stream, level, GEDCOMTagName.SURN, persName.Surname, true);
                WriteTagLine(stream, level, GEDCOMTagName.GIVN, persName.Given, true);

                if (!Strict) WriteTagLine(stream, level, GEDCOMTagName._PATN, persName.PatronymicName, true);
            }

            // Name modifier tags that, by standard, can be included in the NAME string (other than nickname), but cannot be unambiguously extracted from it.
            // Therefore, if they are, the main parts must also be in the file for an unambiguous interpretation.
            WriteTagLine(stream, level, GEDCOMTagName.NPFX, persName.NamePrefix, true);
            WriteTagLine(stream, level, GEDCOMTagName.NICK, persName.Nickname, true);
            WriteTagLine(stream, level, GEDCOMTagName.SPFX, persName.SurnamePrefix, true);
            WriteTagLine(stream, level, GEDCOMTagName.NSFX, persName.NameSuffix, true);

            // Extended parts of the name, missing by the standard
            if (!Strict) {
                WriteTagLine(stream, level, GEDCOMTagName._MARN, persName.MarriedName, true);
                WriteTagLine(stream, level, GEDCOMTagName._RELN, persName.ReligiousName, true);
                WriteTagLine(stream, level, GEDCOMTagName._CENN, persName.CensusName, true);
            }
        }


        private static StackTuple AddDNATestTag(GDMTree tree, GDMTag owner, int tagLevel, int tagId, StringSpan tagValue)
        {
            GDMDNATest dnaTest = (GDMDNATest)owner;
            GDMTag curTag = null;
            TagHandler addHandler = TagHandler.Null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME) {
                dnaTest.TestName = tagValue;
            } else if (tagType == GEDCOMTagType.DATE) {
                curTag = dnaTest.Date;
                GEDCOMUtils.ParseDate(dnaTest.Date, tagValue);
            } else if (tagType == GEDCOMTagType.FILE) {
                dnaTest.FileReference = tagValue;
                curTag = dnaTest;
                addHandler = TagHandler.DNATestTag;
            } else if (tagType == GEDCOMTagType.FORM) {
                // FILE/FORM
                dnaTest.FileFormat = GEDCOMUtils.GetDNAFileFormatVal(tagValue);
            } else if (tagType == GEDCOMTagType._MHAP) {
                dnaTest.MHaplogroup = tagValue;
            } else if (tagType == GEDCOMTagType._YHAP) {
                dnaTest.YHaplogroup = tagValue;
            } else if (tagType == GEDCOMTagType.AGNC) {
                dnaTest.Agency = tagValue;
            } else if (tagType == GEDCOMTagType.NOTE) {
                curTag = dnaTest.Notes.Add(new GDMNotes());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.NoteTag;
            } else if (tagType == GEDCOMTagType.OBJE) {
                curTag = dnaTest.MultimediaLinks.Add(new GDMMultimediaLink());
                curTag.ParseString(tagValue);
                addHandler = TagHandler.MultimediaLinkTag;
            } else if (tagType == GEDCOMTagType.RESN) {
                dnaTest.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            } else {
                return AddBaseTag(tree, owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool WriteDNATest(StreamWriter stream, int level, GDMTag tag)
        {
            GDMDNATest dnaTest = (GDMDNATest)tag;

            if (!WriteBaseTag(stream, level, dnaTest)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, dnaTest.TestName, true);
            WriteBaseTag(stream, level, dnaTest.Date);
            WriteTagLine(stream, level, GEDCOMTagName.FILE, dnaTest.FileReference, true);
            GEDCOMProvider.WriteTagLine(stream, (level + 1), GEDCOMTagName.FORM, GEDCOMUtils.GetDNAFileFormatStr(dnaTest.FileFormat), true);
            WriteTagLine(stream, level, GEDCOMTagName._MHAP, dnaTest.MHaplogroup, true);
            WriteTagLine(stream, level, GEDCOMTagName._YHAP, dnaTest.YHaplogroup, true);

            WriteTagLine(stream, level, GEDCOMTagName.AGNC, dnaTest.Agency, true);
            WriteTagLine(stream, level, GEDCOMTagName.RESN, GEDCOMUtils.GetRestrictionStr(dnaTest.Restriction), true);

            if (dnaTest.HasNotes) WriteList(stream, level, dnaTest.Notes, WriteNote);
            if (dnaTest.HasMultimediaLinks) WriteList(stream, level, dnaTest.MultimediaLinks, WriteMultimediaLink);

            return true;
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
        private void FixBreakedLine(GDMTree tree, GDMTag curRecord, GDMTag curTag, int lineNum, StringSpan str)
        {
            try {
                if (curTag != null) {
                    if (curTag is IGDMTextObject) {
                        AddTextTag(tree, curTag, 0, (int)GEDCOMTagType.CONT, str);
                    } else {
                        var tagType = curTag.GetTagType();

                        if (tagType == GEDCOMTagType.CONT || tagType == GEDCOMTagType.CONC || tagType == GEDCOMTagType.NAME) {
                            curTag.StringValue = string.Concat(curTag.StringValue, str);
                        } else {
                            AddBaseTag(tree, curTag, 0, (int)GEDCOMTagType.NOTE, str);
                        }
                    }
                } else if (curRecord != null) {
                    if (curRecord is IGDMTextObject) {
                        AddTextTag(tree, curRecord, 0, (int)GEDCOMTagType.CONT, str);
                    } else {
                        AddRecordTag(tree, curRecord, 0, (int)GEDCOMTagType.NOTE, str);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDCOMProvider.FixBreakedLine(): Line " + lineNum.ToString() + " failed correct", ex);
            }
        }

        public static GEDCOMFormat GetGEDCOMFormat(GDMTree tree, out bool badLines)
        {
            if (tree != null) {
                string sour = tree.Header.Source.StringValue.Trim();

                int num = GEDCOMProvider.GEDCOMFormats.Length;
                for (int i = 1; i < num; i++) {
                    var appFmt = GEDCOMProvider.GEDCOMFormats[i];
                    if (string.Equals(appFmt.Sign, sour, StringComparison.Ordinal)) {
                        badLines = appFmt.BadLines;
                        return appFmt.Format;
                    }
                }
            }

            badLines = true;
            return GEDCOMFormat.Unknown;
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

        #region Handlers Index

        /// <summary>
        /// Introduced to minimize memory allocation for parsing method delegates.
        /// </summary>
        private static readonly AddTagHandler[] HandlersIndex = new AddTagHandler[] {
            null,
            AddIndividualRecordTag,     /* IndividualRecordTag */
            AddFamilyRecordTag,         /* FamilyRecordTag */
            AddGroupRecordTag,          /* GroupRecordTag */
            AddMultimediaRecordTag,     /* MultimediaRecordTag */
            AddSourceRecordTag,         /* SourceRecordTag */
            AddResearchRecordTag,       /* ResearchRecordTag */
            AddNoteRecordTag,           /* NoteRecordTag */
            AddRepositoryRecordTag,     /* RepositoryRecordTag */
            AddTaskRecordTag,           /* TaskRecordTag */
            AddCommunicationRecordTag,  /* CommunicationRecordTag */
            AddSubmissionRecordTag,     /* SubmissionRecordTag */
            AddSubmitterRecordTag,      /* SubmitterRecordTag */
            AddRecordTag,               /* RecordTag */
            AddHeaderTag,               /* HeaderTag */
            AddHeaderSourceTag,         /* HeaderSourceTag */
            AddHeaderGEDCOMTag,         /* HeaderGEDCOMTag */
            AddHeaderCharSetTag,        /* HeaderCharSetTag */
            AddHeaderFileTag,           /* HeaderFileTag */
            AddChangeDateTag,           /* ChangeDateTag */
            AddCustomEventTag,          /* CustomEventTag */
            AddSourceDataTag,           /* SourceDataTag */
            AddSourceDataEventTag,      /* SourceDataEventTag */
            AddTextTag,                 /* TextTag */
            AddBaseTag,                 /* BaseTag */
            AddLocationRecordTag,       /* LocationRecordTag */
            AddLocationNameTag,         /* LocationNameTag */
            AddLocationLinkTag,         /* LocationLinkTag */
            AddPlaceTag,                /* PlaceTag */
            AddMapTag,                  /* MapTag */
            AddUserReferenceTag,        /* UserReferenceTag */
            AddFileReferenceWithTitleTag,/* FileReferenceWithTitleTag */
            AddFileReferenceTag,        /* FileReferenceTag */
            AddMultimediaLinkTag,       /* MultimediaLinkTag */
            AddNoteTag,                 /* NoteTag */
            AddSourceCitationTag,       /* SourceCitationTag */
            AddSourceCitationDataTag,   /* SourceCitationDataTag */
            AddAssociationTag,          /* AssociationTag */
            AddAddressTag,              /* AddressTag */
            AddChildToFamilyLinkTag,    /* ChildToFamilyLinkTag */
            AddPersonalNameTag,         /* PersonalNameTag */
            AddRepositoryCitationTag,   /* RepositoryCitationTag */
            AddSourceCallNumberTag,     /* SourceCallNumberTag */
            SkipTag,                    /* SkipTag */
            AddIndividualEventTag,      /* IndividualEventTag */
            AddFamilyEventTag,          /* FamilyEventTag */
            AddAgeTag,                  /* AgeTag */
            AddDNATestTag,              /* DNATestTag */
        };

        #endregion

        #region Tag properties

        static GEDCOMProvider()
        {
            GEDCOMFormats = new GEDCOMAppFormat[] {
                new GEDCOMAppFormat(GEDCOMFormat.Unknown, "", "", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Native, "GEDKeeper", "", -1),

                new GEDCOMAppFormat(GEDCOMFormat.AGES, "AGES", "Ages!", -1),
                new GEDCOMAppFormat(GEDCOMFormat.ALTREE, "ALTREE", "Agelong Tree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Ahnenblatt, "AHN", "Ahnenblatt", -1),
                new GEDCOMAppFormat(GEDCOMFormat.AncestQuest, "AncestQuest", "Ancestral Quest", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Ancestry, "Ancestry.com Family Trees", "WikiTree", -1, true),
                new GEDCOMAppFormat(GEDCOMFormat.EasyTree, "EasyTree", "EasyTree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.FamilyHistorian, "FAMILY_HISTORIAN", "Family Historian", -1),
                new GEDCOMAppFormat(GEDCOMFormat.FamilyTreeMaker, "FTM", "Family Tree Maker", -1),
                new GEDCOMAppFormat(GEDCOMFormat.FamilyTreeMaker, "FTW", "Family Tree Maker", -1),
                new GEDCOMAppFormat(GEDCOMFormat.FamyTale, "FAMYTALE", "FamyTale", -1, true),
                new GEDCOMAppFormat(GEDCOMFormat.FTB, "MYHERITAGE", "MyHeritage Family Tree Builder", -1, true),
                new GEDCOMAppFormat(GEDCOMFormat.GENBOX, "GENBOX", "Genbox Family History", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Genealogy_RusOld, "├σφσαδεπΦ", "Genealogy (Rus, old)", 1251), // signature in CP437
                new GEDCOMAppFormat(GEDCOMFormat.gedcom4j, "gedcom4j", "gedcom4j", -1),
                new GEDCOMAppFormat(GEDCOMFormat.GENJ, "GENJ", "GENJ", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Geni, "Geni.com", "Geni", -1, true),
                new GEDCOMAppFormat(GEDCOMFormat.GeneWeb, "GeneWeb", "GeneWeb", 1252),
                new GEDCOMAppFormat(GEDCOMFormat.Genney, "Genney", "Genney", -1),
                new GEDCOMAppFormat(GEDCOMFormat.GenoPro, "GenoPro", "GenoPro", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Gramps, "Gramps", "Gramps", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Heredis, "HEREDIS 12 PC", "Heredis", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Legacy, "Legacy", "Legacy", -1),
                new GEDCOMAppFormat(GEDCOMFormat.Lifelines, "Lifelines", "Lifelines", -1),
                new GEDCOMAppFormat(GEDCOMFormat.PAF, "PAF", "Personal Ancestral File", -1, true),
                new GEDCOMAppFormat(GEDCOMFormat.Reunion, "Reunion", "Reunion", -1),
                new GEDCOMAppFormat(GEDCOMFormat.RootsMagic, "RootsMagic", "RootsMagic", -1),
                new GEDCOMAppFormat(GEDCOMFormat.WikiTree, "WikiTree.com", "WikiTree", -1, true),
            };


            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ABBR, GEDCOMTagName.ABBR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADDR, GEDCOMTagName.ADDR, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADOP, GEDCOMTagName.ADOP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR1, GEDCOMTagName.ADR1);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR2, GEDCOMTagName.ADR2);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR3, GEDCOMTagName.ADR3);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AFN, GEDCOMTagName.AFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AGE, GEDCOMTagName.AGE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AGNC, GEDCOMTagName.AGNC, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ALIA, GEDCOMTagName.ALIA, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANCE, GEDCOMTagName.ANCE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANCI, GEDCOMTagName.ANCI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANUL, GEDCOMTagName.ANUL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ASSO, GEDCOMTagName.ASSO, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AUTH, GEDCOMTagName.AUTH, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BAPL, GEDCOMTagName.BAPL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BAPM, GEDCOMTagName.BAPM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BARM, GEDCOMTagName.BARM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BASM, GEDCOMTagName.BASM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BIRT, GEDCOMTagName.BIRT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BLES, GEDCOMTagName.BLES);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BURI, GEDCOMTagName.BURI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CALN, GEDCOMTagName.CALN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CAST, GEDCOMTagName.CAST);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CAUS, GEDCOMTagName.CAUS, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CENS, GEDCOMTagName.CENS, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHAN, GEDCOMTagName.CHAN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHAR, GEDCOMTagName.CHAR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHIL, GEDCOMTagName.CHIL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHR, GEDCOMTagName.CHR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHRA, GEDCOMTagName.CHRA);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CITY, GEDCOMTagName.CITY, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONC, GEDCOMTagName.CONC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONF, GEDCOMTagName.CONF);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONL, GEDCOMTagName.CONL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONT, GEDCOMTagName.CONT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.COPR, GEDCOMTagName.COPR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CORP, GEDCOMTagName.CORP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CREM, GEDCOMTagName.CREM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CTRY, GEDCOMTagName.CTRY, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DATA, GEDCOMTagName.DATA, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DATE, GEDCOMTagName.DATE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DEAT, GEDCOMTagName.DEAT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DESC, GEDCOMTagName.DESC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DESI, GEDCOMTagName.DESI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DEST, GEDCOMTagName.DEST);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DIV, GEDCOMTagName.DIV);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DIVF, GEDCOMTagName.DIVF);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DSCR, GEDCOMTagName.DSCR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EDUC, GEDCOMTagName.EDUC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ENDL, GEDCOMTagName.ENDL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EVEN, GEDCOMTagName.EVEN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EMAIL, GEDCOMTagName.EMAIL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EMIG, GEDCOMTagName.EMIG);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ENGA, GEDCOMTagName.ENGA);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FACT, GEDCOMTagName.FACT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAM, GEDCOMTagName.FAM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMC, GEDCOMTagName.FAMC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMF, GEDCOMTagName.FAMF);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMS, GEDCOMTagName.FAMS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAX, GEDCOMTagName.FAX);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FCOM, GEDCOMTagName.FCOM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FILE, GEDCOMTagName.FILE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FONE, GEDCOMTagName.FONE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FORM, GEDCOMTagName.FORM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FROM, GEDCOMTagName.FROM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GEDC, GEDCOMTagName.GEDC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GIVN, GEDCOMTagName.GIVN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GRAD, GEDCOMTagName.GRAD);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.HEAD, GEDCOMTagName.HEAD);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.HUSB, GEDCOMTagName.HUSB, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.IDNO, GEDCOMTagName.IDNO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.IMMI, GEDCOMTagName.IMMI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.INDI, GEDCOMTagName.INDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.INT, GEDCOMTagName.INT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LANG, GEDCOMTagName.LANG, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LATI, GEDCOMTagName.LATI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LONG, GEDCOMTagName.LONG);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MAP, GEDCOMTagName.MAP, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARB, GEDCOMTagName.MARB);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARC, GEDCOMTagName.MARC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARL, GEDCOMTagName.MARL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARS, GEDCOMTagName.MARS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARR, GEDCOMTagName.MARR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MEDI, GEDCOMTagName.MEDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NAME, GEDCOMTagName.NAME);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NATI, GEDCOMTagName.NATI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NATU, GEDCOMTagName.NATU);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NCHI, GEDCOMTagName.NCHI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NICK, GEDCOMTagName.NICK, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NMR, GEDCOMTagName.NMR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NOTE, GEDCOMTagName.NOTE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NPFX, GEDCOMTagName.NPFX, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NSFX, GEDCOMTagName.NSFX, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.OBJE, GEDCOMTagName.OBJE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.OCCU, GEDCOMTagName.OCCU);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ORDI, GEDCOMTagName.ORDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ORDN, GEDCOMTagName.ORDN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PAGE, GEDCOMTagName.PAGE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PEDI, GEDCOMTagName.PEDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PHON, GEDCOMTagName.PHON, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PLAC, GEDCOMTagName.PLAC, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.POST, GEDCOMTagName.POST, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PROB, GEDCOMTagName.PROB);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PROP, GEDCOMTagName.PROP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PUBL, GEDCOMTagName.PUBL, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.QUAY, GEDCOMTagName.QUAY, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.REFN, GEDCOMTagName.REFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RELA, GEDCOMTagName.RELA);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RELI, GEDCOMTagName.RELI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.REPO, GEDCOMTagName.REPO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RESI, GEDCOMTagName.RESI, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RESN, GEDCOMTagName.RESN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RETI, GEDCOMTagName.RETI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RFN, GEDCOMTagName.RFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RIN, GEDCOMTagName.RIN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ROMN, GEDCOMTagName.ROMN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SEX, GEDCOMTagName.SEX);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SLGC, GEDCOMTagName.SLGC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SLGS, GEDCOMTagName.SLGS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SOUR, GEDCOMTagName.SOUR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SPFX, GEDCOMTagName.SPFX, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SSN, GEDCOMTagName.SSN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.STAE, GEDCOMTagName.STAE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.STAT, GEDCOMTagName.STAT, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SUBM, GEDCOMTagName.SUBM, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SUBN, GEDCOMTagName.SUBN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SURN, GEDCOMTagName.SURN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TEMP, GEDCOMTagName.TEMP, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TEXT, GEDCOMTagName.TEXT, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TIME, GEDCOMTagName.TIME, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TITL, GEDCOMTagName.TITL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TO, GEDCOMTagName.TO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TRLR, GEDCOMTagName.TRLR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TYPE, GEDCOMTagName.TYPE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.VERS, GEDCOMTagName.VERS, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WIFE, GEDCOMTagName.WIFE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WILL, GEDCOMTagName.WILL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WWW, GEDCOMTagName.WWW, true);

            // non-standard extended tags (other applications)
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._AWARD, GEDCOMTagName._AWARD, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._BGRO, GEDCOMTagName._BGRO, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._CENN, GEDCOMTagName._CENN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._DATE, GEDCOMTagName._DATE, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._DNA, GEDCOMTagName._DNA, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._ELEC, GEDCOMTagName._ELEC, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._EXCM, GEDCOMTagName._EXCM, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._EYES, GEDCOMTagName._EYES, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GRP, GEDCOMTagName._GRP, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._HAIR, GEDCOMTagName._HAIR, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._HOBBY, GEDCOMTagName._HOBBY, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._LOC, GEDCOMTagName._LOC, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MARN, GEDCOMTagName._MARN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MDCL, GEDCOMTagName._MDCL, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MDNA, GEDCOMTagName._MDNA, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MHAP, GEDCOMTagName._MHAP, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._OBIT, GEDCOMTagName._OBIT, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PATN, GEDCOMTagName._PATN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PLAC, GEDCOMTagName._PLAC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PLC, GEDCOMTagName._PLC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._POSITION, GEDCOMTagName._POSITION, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIM, GEDCOMTagName._PRIM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIM_CUTOUT, GEDCOMTagName._PRIM_CUTOUT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._RELN, GEDCOMTagName._RELN, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STAT, GEDCOMTagName._STAT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._TRAVEL, GEDCOMTagName._TRAVEL, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._UID, GEDCOMTagName._UID);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._YDNA, GEDCOMTagName._YDNA, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._YHAP, GEDCOMTagName._YHAP, true);

            // import only
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MARNM, GEDCOMTagName._MARNM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MIDN, GEDCOMTagName._MIDN);

            // non-standard convertible tags
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._FREL, GEDCOMTagName._FREL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MREL, GEDCOMTagName._MREL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._FSFTID, GEDCOMTagName._FSFTID);

            // non-standard extended tags (GEDKeeper)
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._BOOKMARK, GEDCOMTagName._BOOKMARK);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._COMM, GEDCOMTagName._COMM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._FOLDER, GEDCOMTagName._FOLDER);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GOAL, GEDCOMTagName._GOAL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GROUP, GEDCOMTagName._GROUP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._LANG, GEDCOMTagName._LANG, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MEMBER, GEDCOMTagName._MEMBER);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI, GEDCOMTagName._MILI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_DIS, GEDCOMTagName._MILI_DIS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_IND, GEDCOMTagName._MILI_IND);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_RANK, GEDCOMTagName._MILI_RANK);
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

        #endregion

        #region Utilities

        public static string GetTagStreamText(GDMTag tag, int level, bool debugWrite = true)
        {
            DebugWrite = debugWrite;

            string result;
            using (MemoryStream stm = new MemoryStream()) {
                using (StreamWriter fs = new StreamWriter(stm)) {
                    if (tag is GDMRecord) {
                        WriteRecordEx(fs, (GDMRecord)tag);
                    } else if (tag is GDMIndividualEvent) {
                        WriteIndividualEvent(fs, 1, tag);
                    } else if (tag is GDMPersonalName) {
                        WritePersonalName(fs, 1, tag);
                    } else if (tag is GDMMultimediaLink) {
                        WriteMultimediaLink(fs, 1, tag);
                    } else if (tag is GDMSourceCitation) {
                        WriteSourceCitation(fs, 1, tag);
                    } else if (tag is GDMSourceData) {
                        WriteSourceData(fs, 1, tag);
                    } else {
                        WriteBaseTag(fs, level, tag);
                    }
                    fs.Flush();
                    result = Encoding.UTF8.GetString(stm.ToArray());
                }
            }
            return result;
        }

        #endregion
    }
}
