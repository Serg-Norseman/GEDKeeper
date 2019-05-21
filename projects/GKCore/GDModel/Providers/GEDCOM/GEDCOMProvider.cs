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
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using BSLib;
using GDModel.Providers.GEDCOM;
using GKCore;

namespace GKCommon.GEDCOM
{
    public enum GEDCOMFormat
    {
        gf_Unknown,
        gf_Native,
        gf_GENBOX,
        gf_ALTREE,
        gf_AGES,
        gf_PAF,
        gf_Ahnenblatt,
        gf_Genealogy_RusOld,
        gf_FTB,
        gf_FamilyTreeMaker,
        gf_FamilyHistorian,
        gf_Heredis,
        gf_AncestQuest,
        gf_Geni,
        gf_Legacy,
        gf_EasyTree,
        gf_Genney,
        gf_GeneWeb,
        gf_GENJ,

        gf_Last = gf_GENJ
    }


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


    /// <summary>
    /// 
    /// </summary>
    public class GEDCOMProvider : FileProvider
    {
        private static readonly GEDCOMFactory fIndividualTags;
        private static readonly GEDCOMFactory fFamilyTags;

        public const char GEDCOM_DELIMITER = ' ';
        public const char GEDCOM_YEAR_MODIFIER_SEPARATOR = '/';
        public const char GEDCOM_NAME_SEPARATOR = '/';
        public const string GEDCOM_YEAR_BC = "B.C.";
        public const char GEDCOM_POINTER_DELIMITER = '@';
        public const string GEDCOM_NEWLINE = "\r\n";
        public const int MAX_LINE_LENGTH = 248;

        // deprecated
        //public const byte GEDCOMMaxPhoneNumbers = 3;
        //public const byte GEDCOMMaxEmailAddresses = 3;
        //public const byte GEDCOMMaxFaxNumbers = 3;
        //public const byte GEDCOMMaxWebPages = 3;
        //public const byte GEDCOMMaxLanguages = 3;

        public static readonly GEDCOMAppFormat[] GEDCOMFormats;


        public GEDCOMProvider(GEDCOMTree tree) : base(tree)
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
            GEDCOMCharacterSet charSet = fTree.Header.CharacterSet;
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
                            string cpVers = fTree.Header.CharacterSetVersion;
                            if (!string.IsNullOrEmpty(cpVers)) {
                                int sourceCodepage = ConvertHelper.ParseInt(cpVers, DEF_CODEPAGE);
                                SetEncoding(Encoding.GetEncoding(sourceCodepage));
                            } else {
                                if (fTree.Header.Language.Value == GEDCOMLanguageID.Russian) {
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
        private const int LB_SIZE = 1024;

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
            fTree.State = GEDCOMState.osLoading;
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
                GEDCOMCustomRecord curRecord = null;
                GEDCOMTag curTag = null;
                var stack = new Stack<StackTuple>(9);

                int lineNum = 0;
                int lineLen;
                while ((lineLen = ReadLine(reader)) != -1) {
                    lineNum++;

                    int tagLevel;
                    string tagXRef, tagName, tagValue;

                    try {
                        strTok.Reset(fLineBuffer, 0, lineLen);
                        int lineRes = GEDCOMUtils.ParseTag(strTok, out tagLevel, out tagXRef, out tagName, out tagValue);

                        // empty line
                        if (lineRes == -2) continue;

                        // line with text but not in standard tag format
                        if (lineRes == -1) {
                            if (fTree.Format == GEDCOMFormat.gf_FTB) {
                                FixFTBLine(curRecord, curTag, lineNum, tagValue);
                                continue;
                            } else {
                                throw new EGEDCOMException(string.Format("The string {0} doesn't start with a valid number", lineNum));
                            }
                        }

                        tagName = invariantText.ToUpper(tagName);
                    } catch (EGEDCOMException ex) {
                        throw new EGEDCOMException("Syntax error in line " + Convert.ToString(lineNum) + ".\r" + ex.Message);
                    }

                    if (tagLevel == 0) {
                        if (curRecord == fTree.Header && fEncodingState == EncodingState.esUnchecked) {
                            // beginning recognition of the first is not header record
                            // to check for additional versions of the code page
                            var format = GetGEDCOMFormat(fTree);
                            fTree.Format = format;
                            DefineEncoding(reader, format, streamCharset);
                        }

                        StackTuple stackTuple = AddTreeTag(fTree, tagLevel, tagName, tagValue);
                        if (stackTuple != null) {
                            stack.Clear();
                            stack.Push(stackTuple);
                            curRecord = (GEDCOMCustomRecord)stackTuple.Tag;
                            if (!string.IsNullOrEmpty(tagXRef)) {
                                curRecord.XRef = tagXRef;
                            }
                            curTag = null;
                        } else {
                            // only TRLR
                            break;
                        }
                    } else {
                        if (curRecord != null) {
                            curTag = ProcessTag(stack, tagLevel, tagName, tagValue);
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
            } finally {
                fTree.State = GEDCOMState.osReady;
            }
        }

        internal static GEDCOMTag ProcessTag(Stack<StackTuple> stack, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMTag curTag = null;

            GEDCOMTag parentTag = null;
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
                if (addTagHandler != null) {
                    var tuple = addTagHandler(parentTag, tagLevel, tagName, tagValue);
                    if (tuple != null) {
                        stack.Push(tuple);
                        curTag = (GEDCOMTag)tuple.Tag;
                    }
                } else {
                    curTag = parentTag.AddTag(tagName, tagValue, null);
                    if (curTag != null) {
                        stack.Push(new StackTuple(tagLevel, curTag));
                    }
                }
            }

            return curTag;
        }

        #region AddTag Handlers

        internal static StackTuple AddTreeTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMTree tree = (GEDCOMTree)owner;
            GEDCOMCustomRecord curRecord = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.INDI) {
                curRecord = tree.AddRecord(new GEDCOMIndividualRecord(tree));
                addHandler = AddIndividualRecordTag;

            } else if (tagName == GEDCOMTagType.FAM) {
                curRecord = tree.AddRecord(new GEDCOMFamilyRecord(tree));
                addHandler = AddFamilyRecordTag;

            } else if (tagName == GEDCOMTagType.OBJE) {
                curRecord = tree.AddRecord(new GEDCOMMultimediaRecord(tree));
                addHandler = AddMultimediaRecordTag;

            } else if (tagName == GEDCOMTagType.NOTE) {
                curRecord = tree.AddRecord(new GEDCOMNoteRecord(tree));
                curRecord.ParseString(tagValue);
                addHandler = AddNoteRecordTag;

            } else if (tagName == GEDCOMTagType.REPO) {
                curRecord = tree.AddRecord(new GEDCOMRepositoryRecord(tree));
                addHandler = AddRepositoryRecordTag;

            } else if (tagName == GEDCOMTagType.SOUR) {
                curRecord = tree.AddRecord(new GEDCOMSourceRecord(tree));
                addHandler = AddSourceRecordTag;

            } else if (tagName == GEDCOMTagType.SUBN) {
                curRecord = tree.AddRecord(new GEDCOMSubmissionRecord(tree));
                addHandler = AddSubmissionRecordTag;

            } else if (tagName == GEDCOMTagType.SUBM) {
                curRecord = tree.AddRecord(new GEDCOMSubmitterRecord(tree));
                addHandler = AddSubmitterRecordTag;

            } else if (tagName == GEDCOMTagType._GROUP) {
                curRecord = tree.AddRecord(new GEDCOMGroupRecord(tree));
                addHandler = AddGroupRecordTag;

            } else if (tagName == GEDCOMTagType._RESEARCH) {
                curRecord = tree.AddRecord(new GEDCOMResearchRecord(tree));
                addHandler = AddResearchRecordTag;

            } else if (tagName == GEDCOMTagType._TASK) {
                curRecord = tree.AddRecord(new GEDCOMTaskRecord(tree));
                addHandler = AddTaskRecordTag;

            } else if (tagName == GEDCOMTagType._COMM) {
                curRecord = tree.AddRecord(new GEDCOMCommunicationRecord(tree));
                addHandler = AddCommunicationRecordTag;

            } else if (tagName == GEDCOMTagType._LOC) {
                curRecord = tree.AddRecord(new GEDCOMLocationRecord(tree));
                addHandler = AddLocationRecordTag;

            } else if (tagName == GEDCOMTagType.HEAD) {
                curRecord = tree.Header;
                addHandler = AddHeaderTag;

            } else if (tagName == GEDCOMTagType.TRLR) {
                curRecord = null;

            } else {
                curRecord = tree.AddRecord(new GEDCOMRecord(tree));
                addHandler = AddRecordTag;
            }

            return CreateReaderStackTuple(tagLevel, curRecord, addHandler);
        }

        private static StackTuple AddIndividualRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMIndividualRecord indiRec = (GEDCOMIndividualRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            curTag = fIndividualTags.CreateTag(indiRec, tagName, tagValue);
            if (curTag != null) {
                if (curTag is GEDCOMCustomEvent) {
                    curTag = indiRec.AddEvent(curTag as GEDCOMCustomEvent);
                    addHandler = AddCustomEventTag;
                } else if (curTag is GEDCOMChildToFamilyLink) {
                    curTag = indiRec.ChildToFamilyLinks.Add(curTag as GEDCOMChildToFamilyLink);
                } else if (curTag is GEDCOMSpouseToFamilyLink) {
                    curTag = indiRec.SpouseToFamilyLinks.Add(curTag as GEDCOMSpouseToFamilyLink);
                } else if (curTag is GEDCOMPersonalName) {
                    curTag = indiRec.AddPersonalName(curTag as GEDCOMPersonalName);
                    addHandler = AddPersonalNameTag;
                } else if (curTag is GEDCOMAssociation) {
                    curTag = indiRec.Associations.Add(curTag as GEDCOMAssociation);
                    addHandler = AddAssociationTag;
                } else if (curTag is GEDCOMAlias) {
                    curTag = indiRec.Aliases.Add(curTag as GEDCOMAlias);
                }
            } else {
                if (tagName == GEDCOMTagType._GROUP) {
                    curTag = indiRec.Groups.Add(new GEDCOMPointer(indiRec, tagName, tagValue));
                } else if (tagName == GEDCOMTagType.SEX) {
                    indiRec.Sex = GEDCOMUtils.GetSexVal(tagValue);
                    curTag = null;
                } else {
                    return AddRecordWithEventsTag(owner, tagLevel, tagName, tagValue);
                }
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddFamilyRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMFamilyRecord famRec = (GEDCOMFamilyRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.HUSB || tagName == GEDCOMTagType.WIFE) {
                curTag = famRec.AddTag(new GEDCOMPointer(famRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.CHIL) {
                curTag = famRec.Children.Add(new GEDCOMPointer(famRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._STAT) {
                famRec.Status = GEDCOMUtils.GetMarriageStatusVal(tagValue);
                curTag = null;
            } else {
                curTag = fFamilyTags.CreateTag(famRec, tagName, tagValue);

                if (curTag != null) {
                    if (curTag is GEDCOMFamilyEvent) {
                        curTag = famRec.AddEvent(curTag as GEDCOMFamilyEvent);
                        addHandler = AddCustomEventTag;
                    }
                } else {
                    return AddRecordWithEventsTag(owner, tagLevel, tagName, tagValue);
                }
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRecordWithEventsTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMRecordWithEvents evtRec = (GEDCOMRecordWithEvents)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.RESN) {
                evtRec.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
                curTag = null;
            } else if (tagName == GEDCOMTagType.SUBM) {
                curTag = evtRec.Submittors.Add(new GEDCOMPointer(evtRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddNoteRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            /*GEDCOMNoteRecord noteRec = (GEDCOMNoteRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;*/

            return AddRecordTag(owner, tagLevel, tagName, tagValue);

            /*return CreateReaderStackTuple(tagLevel, curTag, addHandler);*/
        }

        private static StackTuple AddMultimediaRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMMultimediaRecord mmRec = (GEDCOMMultimediaRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.FILE) {
                curTag = mmRec.FileReferences.Add(new GEDCOMFileReferenceWithTitle(mmRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddSourceRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMSourceRecord sourRec = (GEDCOMSourceRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.REPO) {
                curTag = sourRec.RepositoryCitations.Add(new GEDCOMRepositoryCitation(sourRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.DATA) {
                curTag = sourRec.AddTag(new GEDCOMData(sourRec, tagName, tagValue));
                addHandler = AddDataTag;
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRepositoryRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMRepositoryRecord repoRec = (GEDCOMRepositoryRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                return AddAddressTag(repoRec.Address, tagLevel, tagName, tagValue);
            } else {
                // 'ADDR' defines by default
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddGroupRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMGroupRecord groupRec = (GEDCOMGroupRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = groupRec.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType._MEMBER) {
                curTag = groupRec.Members.Add(new GEDCOMPointer(groupRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddResearchRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMResearchRecord resRec = (GEDCOMResearchRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = resRec.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType._STARTDATE || tagName == GEDCOMTagType._STOPDATE) {
                curTag = resRec.AddTag(new GEDCOMDate(resRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._TASK) {
                curTag = resRec.Tasks.Add(new GEDCOMPointer(resRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._COMM) {
                curTag = resRec.Communications.Add(new GEDCOMPointer(resRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._GROUP) {
                curTag = resRec.Groups.Add(new GEDCOMPointer(resRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddTaskRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMTaskRecord taskRec = (GEDCOMTaskRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType._STARTDATE || tagName == GEDCOMTagType._STOPDATE) {
                curTag = taskRec.AddTag(new GEDCOMDate(taskRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddCommunicationRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMCommunicationRecord commRec = (GEDCOMCommunicationRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = commRec.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType.DATE) {
                curTag = commRec.AddTag(new GEDCOMDate(commRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddLocationRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            /*GEDCOMLocationRecord locRec = (GEDCOMLocationRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;*/

            return AddRecordTag(owner, tagLevel, tagName, tagValue);

            /*return CreateReaderStackTuple(tagLevel, curTag, addHandler);*/
        }

        private static StackTuple AddSubmissionRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMSubmissionRecord submnRec = (GEDCOMSubmissionRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.SUBM) {
                curTag = submnRec.AddTag(new GEDCOMPointer(submnRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddSubmitterRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMSubmitterRecord submrRec = (GEDCOMSubmitterRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = submrRec.AddTag(new GEDCOMPersonalName(submrRec, tagName, tagValue));
                addHandler = AddPersonalNameTag;
            } else if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                return AddAddressTag(submrRec.Address, tagLevel, tagName, tagValue);
            } else if (tagName == GEDCOMTagType.LANG) {
                curTag = submrRec.AddLanguage(new GEDCOMLanguage(submrRec, tagName, tagValue));
            } else {
                // 'ADDR' defines by default
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRecordTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMRecord record = (GEDCOMRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NOTE) {
                curTag = record.Notes.Add(new GEDCOMNotes(record, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SOUR) {
                curTag = record.SourceCitations.Add(new GEDCOMSourceCitation(record, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.OBJE) {
                curTag = record.MultimediaLinks.Add(new GEDCOMMultimediaLink(record, tagName, tagValue));
                addHandler = AddMultimediaLinkTag;
            } else if (tagName == GEDCOMTagType.REFN) {
                curTag = record.UserReferences.Add(new GEDCOMUserReference(record, tagName, tagValue));
                addHandler = AddUserReferenceTag;
            } else if (tagName == GEDCOMTagType._UID) {
                record.UID = tagValue;
                curTag = null;
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddHeaderTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMHeader header = (GEDCOMHeader)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = header.AddTag(new GEDCOMDate(header, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SUBM) {
                curTag = header.AddTag(new GEDCOMPointer(header, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SUBN) {
                curTag = header.AddTag(new GEDCOMPointer(header, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddAddressTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMAddress addr = (GEDCOMAddress)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.PHON) {
                curTag = addr.AddPhoneNumber(tagValue);
            } else if (tagName == GEDCOMTagType.EMAIL) {
                curTag = addr.AddEmailAddress(tagValue);
            } else if (tagName == GEDCOMTagType.FAX) {
                curTag = addr.AddFaxNumber(tagValue);
            } else if (tagName == GEDCOMTagType.WWW) {
                curTag = addr.AddWebPage(tagValue);
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddAssociationTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMAssociation asso = (GEDCOMAssociation)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.RELA) {
                asso.Relation = tagValue;
                curTag = null;
            } else if (tagName == GEDCOMTagType.SOUR) {
                curTag = asso.SourceCitations.Add(new GEDCOMSourceCitation(asso, tagName, tagValue));
            } else {
                return AddPointerWithNotesTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddChangeDateTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMChangeDate changeDate = (GEDCOMChangeDate)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = changeDate.AddTag(new GEDCOMDate(changeDate, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.NOTE) {
                curTag = changeDate.AddTag(new GEDCOMNotes(changeDate, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddCustomEventTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMCustomEvent custEvent = (GEDCOMCustomEvent)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                return AddAddressTag(custEvent.Address, tagLevel, tagName, tagValue);
            } else {
                // define 'PLAC', 'ADDR', 'DATE' by default
                return AddTagWithListsTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddDataTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMData data = (GEDCOMData)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.EVEN) {
                curTag = data.Events.Add(new GEDCOMEvent(data, tagName, tagValue));
                addHandler = AddDataEventTag;
            } else {
                return AddTagWithListsTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddDateStatusTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMDateStatus dateStatus = (GEDCOMDateStatus)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = dateStatus.AddTag(new GEDCOMDate(dateStatus, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddDataEventTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMEvent dataEvent = (GEDCOMEvent)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = dataEvent.AddTag(new GEDCOMDatePeriod(dataEvent, tagName, tagValue));
            } else {
                // define 'PLAC' by default
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddIndividualOrdinanceTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMIndividualOrdinance indiOrd = (GEDCOMIndividualOrdinance)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.STAT) {
                curTag = indiOrd.AddTag(new GEDCOMDateStatus(indiOrd, tagName, tagValue));
                addHandler = AddDateStatusTag;
            } else {
                // define 'DATE', 'FAMC' by default
                return AddTagWithListsTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddMultimediaLinkTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMMultimediaLink mmLink = (GEDCOMMultimediaLink)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.FILE) {
                curTag = mmLink.FileReferences.Add(new GEDCOMFileReference(mmLink, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddPersonalNameTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMPersonalName persName = (GEDCOMPersonalName)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.TYPE || tagName == GEDCOMTagType.FONE || tagName == GEDCOMTagType.ROMN || tagName == "_LANG") {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            } else {
                curTag = persName.Pieces.AddTag(tagName, tagValue, null);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddPointerWithNotesTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMPointerWithNotes ptrWN = (GEDCOMPointerWithNotes)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NOTE) {
                curTag = ptrWN.Notes.Add(new GEDCOMNotes(ptrWN, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddTagWithListsTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMTagWithLists tagWL = (GEDCOMTagWithLists)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NOTE) {
                curTag = tagWL.Notes.Add(new GEDCOMNotes(tagWL, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SOUR) {
                curTag = tagWL.SourceCitations.Add(new GEDCOMSourceCitation(tagWL, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.OBJE) {
                curTag = tagWL.MultimediaLinks.Add(new GEDCOMMultimediaLink(tagWL, tagName, tagValue));
                addHandler = AddMultimediaLinkTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddUserReferenceTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMUserReference userRef = (GEDCOMUserReference)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.TYPE) {
                userRef.ReferenceType = tagValue;
                curTag = null;
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddBaseTag(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMTag ownerTag = (GEDCOMTag)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;

            curTag = ownerTag.AddTag(tagName, tagValue, null);

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple CreateReaderStackTuple(int level, GEDCOMTag tag, AddTagHandler addHandler)
        {
            if (tag == null) {
                return null;
            } else {
                if (addHandler == null) {
                    addHandler = GEDCOMFactory.GetInstance().GetAddHandler(tag.Name);
                }
                return new StackTuple(level, tag, addHandler);
            }
        }

        #endregion

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

            fTree.Pack();
            using (StreamWriter writer = new StreamWriter(outputStream, GEDCOMUtils.GetEncodingByCharacterSet(charSet))) {
                IList<GEDCOMRecord> records = fTree.GetRecords().GetList();
                SaveToStream(writer, records);

                writer.Flush();
            }

            fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
        }

        public void SaveToStream(StreamWriter writer, IList<GEDCOMRecord> list)
        {
            // write header
            fTree.Header.SaveToStream(writer, 0);

            if (list != null) {
                int num = list.Count;
                for (int i = 0; i < num; i++) {
                    list[i].SaveToStream(writer, 0);
                }
            }

            // write footer
            const string str = "0 TRLR";
            writer.Write(str + GEDCOMProvider.GEDCOM_NEWLINE);
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

            if (su == GEDCOMCustomDate.GEDCOMDateRangeArray[0] ||
                su == GEDCOMCustomDate.GEDCOMDateRangeArray[1] ||
                su == GEDCOMCustomDate.GEDCOMDateApproximatedArray[1] ||
                su == GEDCOMCustomDate.GEDCOMDateApproximatedArray[2] ||
                su == GEDCOMCustomDate.GEDCOMDateApproximatedArray[3])
            {
                result = result.Remove(0, 4);
            }
            return result;
        }

        /// <summary>
        /// Fix of line errors that are in the files of FamilyTreeBuilder.
        /// </summary>
        private static void FixFTBLine(GEDCOMCustomRecord curRecord, GEDCOMTag curTag, int lineNum, string str)
        {
            try {
                if (curTag.Name == GEDCOMTagType.CONT) {
                    curTag.StringValue += " " + str;
                } else if (curTag is GEDCOMNotes) {
                    curTag.AddTag(GEDCOMTagType.CONT, str, null);
                } else {
                    if (curRecord != null) {
                        curRecord.AddTag(GEDCOMTagType.NOTE, str, null);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("GEDCOMProvider.FixFTBLine(): Line " + lineNum.ToString() + " failed correct: " + ex.Message);
            }
        }

        public static GEDCOMFormat GetGEDCOMFormat(GEDCOMTree tree)
        {
            if (tree != null) {
                string sour = tree.Header.Source.Trim();

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
                new GEDCOMAppFormat(GEDCOMFormat.gf_GENBOX, "GENBOX", "Genbox Family History", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_ALTREE, "ALTREE", "Agelong Tree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_AGES, "AGES", "Ages!", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_PAF, "PAF", "Personal Ancestral File", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Ahnenblatt, "AHN", "Ahnenblatt", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Genealogy_RusOld, "├σφσαδεπΦ", "Genealogy (Rus, old)", 1251), // signature in CP437
                new GEDCOMAppFormat(GEDCOMFormat.gf_FTB, "MYHERITAGE", "MyHeritage Family Tree Builder", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyTreeMaker, "FTM", "Family Tree Maker for Windows", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyTreeMaker, "FTW", "Family Tree Maker for Windows", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyHistorian, "FAMILY_HISTORIAN", "Family Historian", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Heredis, "HEREDIS 12 PC", "Heredis", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_AncestQuest, "AncestQuest", "Ancestral Quest", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Geni, "Geni.com", "Geni", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Legacy, "Legacy", "Legacy", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_EasyTree, "EasyTree", "EasyTree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Genney, "Genney", "Genney", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GeneWeb, "GeneWeb", "GeneWeb", 1252),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GENJ, "GENJ", "GENJ", -1),
            };

            fTagsBase = CreatePropertiesDict();

            GEDCOMFactory f = GEDCOMFactory.GetInstance();

            f.RegisterTag(GEDCOMTagType.ADDR, GEDCOMAddress.Create, AddAddressTag);
            f.RegisterTag(GEDCOMTagType.ANCI, GEDCOMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.BAPL, GEDCOMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.CHAN, GEDCOMChangeDate.Create, AddChangeDateTag);
            f.RegisterTag(GEDCOMTagType.CONL, GEDCOMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.DATE, GEDCOMDateValue.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.DESI, GEDCOMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.ENDL, GEDCOMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.FAMC, GEDCOMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.LANG, GEDCOMLanguage.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.MAP, GEDCOMMap.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.PLAC, GEDCOMPlace.Create, AddTagWithListsTag);
            f.RegisterTag(GEDCOMTagType.SLGC, GEDCOMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.SLGS, GEDCOMSpouseSealing.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.TIME, GEDCOMTime.Create, AddBaseTag);

            f.RegisterTag("_LANG", GEDCOMLanguage.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType._LOC, GEDCOMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType._POSITION, GEDCOMCutoutPosition.Create, AddBaseTag);

            f = new GEDCOMFactory();
            fIndividualTags = f;

            f.RegisterTag(GEDCOMTagType.NAME, GEDCOMPersonalName.Create, AddPersonalNameTag);
            f.RegisterTag(GEDCOMTagType.FAMC, GEDCOMChildToFamilyLink.Create, AddPointerWithNotesTag);
            f.RegisterTag(GEDCOMTagType.FAMS, GEDCOMSpouseToFamilyLink.Create, AddPointerWithNotesTag);
            f.RegisterTag(GEDCOMTagType.ASSO, GEDCOMAssociation.Create, AddAssociationTag);
            f.RegisterTag(GEDCOMTagType.ALIA, GEDCOMAlias.Create, AddBaseTag);

            f.RegisterTag(GEDCOMTagType.ADOP, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BAPM, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BARM, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BASM, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BIRT, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BLES, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BURI, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CENS, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CHR, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CHRA, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CONF, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CREM, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DEAT, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EVEN, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EMIG, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.FCOM, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.GRAD, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.IMMI, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NATU, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.ORDN, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.PROB, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RETI, GEDCOMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.WILL, GEDCOMIndividualEvent.Create, AddCustomEventTag);

            f.RegisterTag(GEDCOMTagType.CAST, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DSCR, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EDUC, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.FACT, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.IDNO, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NATI, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NCHI, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NMR, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.OCCU, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.PROP, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RELI, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RESI, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.SSN, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.TITL, GEDCOMIndividualAttribute.Create, AddCustomEventTag);

            f.RegisterTag(GEDCOMTagType._TRAVEL, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._HOBBY, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._AWARD, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI_IND, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI_DIS, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI_RANK, GEDCOMIndividualAttribute.Create, AddCustomEventTag);

            f.RegisterTag(GEDCOMTagType._BGRO, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._EYES, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._HAIR, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MDNA, GEDCOMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._YDNA, GEDCOMIndividualAttribute.Create, AddCustomEventTag);

            f = new GEDCOMFactory();
            fFamilyTags = f;

            f.RegisterTag(GEDCOMTagType.ANUL, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CENS, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DIV, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DIVF, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.ENGA, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARB, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARC, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARR, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARL, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARS, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RESI, GEDCOMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EVEN, GEDCOMFamilyEvent.Create, AddCustomEventTag);
        }

        private static readonly Dictionary<string, TagProperties> fTagsBase;

        private static Dictionary<string, TagProperties> CreatePropertiesDict()
        {
            var result = new Dictionary<string, TagProperties>();

            result.Add(GEDCOMTagType.ADDR, new TagProperties(GEDCOMTagType.ADDR, true, false));
            result.Add(GEDCOMTagType.AGNC, new TagProperties(GEDCOMTagType.AGNC, true, false));
            result.Add(GEDCOMTagType.AUTH, new TagProperties(GEDCOMTagType.AUTH, true, false));
            result.Add(GEDCOMTagType.CAUS, new TagProperties(GEDCOMTagType.CAUS, true, false));
            result.Add(GEDCOMTagType.CHAN, new TagProperties(GEDCOMTagType.CHAN, true, false));
            result.Add(GEDCOMTagType.CITY, new TagProperties(GEDCOMTagType.CITY, true, false));
            result.Add(GEDCOMTagType.CTRY, new TagProperties(GEDCOMTagType.CTRY, true, false));
            result.Add(GEDCOMTagType.DATE, new TagProperties(GEDCOMTagType.DATE, true, false));
            result.Add(GEDCOMTagType.PAGE, new TagProperties(GEDCOMTagType.PAGE, true, false));
            result.Add(GEDCOMTagType.PHON, new TagProperties(GEDCOMTagType.PHON, true, false));
            result.Add(GEDCOMTagType.PLAC, new TagProperties(GEDCOMTagType.PLAC, true, false));
            result.Add(GEDCOMTagType.POST, new TagProperties(GEDCOMTagType.POST, true, false));
            result.Add(GEDCOMTagType.PUBL, new TagProperties(GEDCOMTagType.PUBL, true, false));
            result.Add(GEDCOMTagType.RESN, new TagProperties(GEDCOMTagType.RESN, true, false));
            result.Add(GEDCOMTagType.STAE, new TagProperties(GEDCOMTagType.STAE, true, false));
            result.Add(GEDCOMTagType.TEXT, new TagProperties(GEDCOMTagType.TEXT, true, false));
            result.Add(GEDCOMTagType.TIME, new TagProperties(GEDCOMTagType.TIME, true, false));
            result.Add(GEDCOMTagType.TYPE, new TagProperties(GEDCOMTagType.TYPE, true, false));
            result.Add(GEDCOMTagType.SUBM, new TagProperties(GEDCOMTagType.SUBM, true, false));
            result.Add(GEDCOMTagType.VERS, new TagProperties(GEDCOMTagType.VERS, true, false));
            result.Add(GEDCOMTagType.LANG, new TagProperties(GEDCOMTagType.LANG, true, false));
            result.Add(GEDCOMTagType.NPFX, new TagProperties(GEDCOMTagType.NPFX, true, false));
            result.Add(GEDCOMTagType.GIVN, new TagProperties(GEDCOMTagType.GIVN, true, false));
            result.Add(GEDCOMTagType.NICK, new TagProperties(GEDCOMTagType.NICK, true, false));
            result.Add(GEDCOMTagType.SPFX, new TagProperties(GEDCOMTagType.SPFX, true, false));
            result.Add(GEDCOMTagType.SURN, new TagProperties(GEDCOMTagType.SURN, true, false));
            result.Add(GEDCOMTagType.NSFX, new TagProperties(GEDCOMTagType.NSFX, true, false));
            result.Add(GEDCOMTagType.ALIA, new TagProperties(GEDCOMTagType.ALIA, true, false));
            result.Add(GEDCOMTagType.HUSB, new TagProperties(GEDCOMTagType.HUSB, true, false));
            result.Add(GEDCOMTagType.WIFE, new TagProperties(GEDCOMTagType.WIFE, true, false));

            // extensions
            result.Add(GEDCOMTagType._PATN, new TagProperties(GEDCOMTagType._PATN, true, true));
            result.Add(GEDCOMTagType._MARN, new TagProperties(GEDCOMTagType._MARN, true, true));
            result.Add(GEDCOMTagType._RELN, new TagProperties(GEDCOMTagType._RELN, true, true));
            result.Add(GEDCOMTagType._CENN, new TagProperties(GEDCOMTagType._CENN, true, true));
            result.Add(GEDCOMTagType._LOC, new TagProperties(GEDCOMTagType._LOC, true,  true));
            result.Add(GEDCOMTagType._POSITION, new TagProperties(GEDCOMTagType._POSITION, true,  true));
            result.Add(GEDCOMTagType._BGRO, new TagProperties(GEDCOMTagType._BGRO, true,  true));
            result.Add(GEDCOMTagType._HAIR, new TagProperties(GEDCOMTagType._HAIR, true,  true));
            result.Add(GEDCOMTagType._EYES, new TagProperties(GEDCOMTagType._EYES, true,  true));
            result.Add(GEDCOMTagType._MDNA, new TagProperties(GEDCOMTagType._MDNA, true,  true));
            result.Add(GEDCOMTagType._YDNA, new TagProperties(GEDCOMTagType._YDNA, true,  true));

            result.Add("_LANG", new TagProperties("_LANG", true,  true));

            // TODO
            //result.Add("_OBIT", new TagProperties("_OBIT", true,  true)); // Obituary
            //result.Add("_ELEC", new TagProperties("_ELEC", true,  true)); // Election
            //result.Add("_MDCL", new TagProperties("_MDCL", true,  true)); // Medical condition
            //result.Add("_EXCM", new TagProperties("_EXCM", true,  true)); // Excommunication!

            return result;
        }

        public static TagProperties GetTagProps(string tagName)
        {
            TagProperties result;
            fTagsBase.TryGetValue(tagName, out result);
            return result;
        }

        public static bool SkipEmptyTag(string tagName)
        {
            TagProperties props = GEDCOMProvider.GetTagProps(tagName);
            return (props != null && props.SkipEmpty);
        }

        #endregion
    }
}
