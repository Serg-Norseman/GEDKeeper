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
using GDModel.Providers;
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
                GDMCustomRecord curRecord = null;
                GDMTag curTag = null;
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
                                throw new GDMException(string.Format("The string {0} doesn't start with a valid number", lineNum));
                            }
                        }

                        tagName = invariantText.ToUpper(tagName);
                    } catch (GDMException ex) {
                        throw new GDMException("Syntax error in line " + Convert.ToString(lineNum) + ".\r" + ex.Message);
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
                            curRecord = (GDMCustomRecord)stackTuple.Tag;
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

        internal static GDMTag ProcessTag(Stack<StackTuple> stack, int tagLevel, string tagName, string tagValue)
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
                if (addTagHandler != null) {
                    var tuple = addTagHandler(parentTag, tagLevel, tagName, tagValue);
                    if (tuple != null) {
                        stack.Push(tuple);
                        curTag = (GDMTag)tuple.Tag;
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

        internal static StackTuple AddTreeTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMTree tree = (GDMTree)owner;
            GDMCustomRecord curRecord = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.INDI) {
                curRecord = tree.AddRecord(new GDMIndividualRecord(tree));
                addHandler = AddIndividualRecordTag;

            } else if (tagName == GEDCOMTagType.FAM) {
                curRecord = tree.AddRecord(new GDMFamilyRecord(tree));
                addHandler = AddFamilyRecordTag;

            } else if (tagName == GEDCOMTagType.OBJE) {
                curRecord = tree.AddRecord(new GDMMultimediaRecord(tree));
                addHandler = AddMultimediaRecordTag;

            } else if (tagName == GEDCOMTagType.NOTE) {
                curRecord = tree.AddRecord(new GDMNoteRecord(tree));
                curRecord.ParseString(tagValue);
                addHandler = AddNoteRecordTag;

            } else if (tagName == GEDCOMTagType.REPO) {
                curRecord = tree.AddRecord(new GDMRepositoryRecord(tree));
                addHandler = AddRepositoryRecordTag;

            } else if (tagName == GEDCOMTagType.SOUR) {
                curRecord = tree.AddRecord(new GDMSourceRecord(tree));
                addHandler = AddSourceRecordTag;

            } else if (tagName == GEDCOMTagType.SUBN) {
                curRecord = tree.AddRecord(new GDMSubmissionRecord(tree));
                addHandler = AddSubmissionRecordTag;

            } else if (tagName == GEDCOMTagType.SUBM) {
                curRecord = tree.AddRecord(new GDMSubmitterRecord(tree));
                addHandler = AddSubmitterRecordTag;

            } else if (tagName == GEDCOMTagType._GROUP) {
                curRecord = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = AddGroupRecordTag;

            } else if (tagName == GEDCOMTagType._RESEARCH) {
                curRecord = tree.AddRecord(new GDMResearchRecord(tree));
                addHandler = AddResearchRecordTag;

            } else if (tagName == GEDCOMTagType._TASK) {
                curRecord = tree.AddRecord(new GDMTaskRecord(tree));
                addHandler = AddTaskRecordTag;

            } else if (tagName == GEDCOMTagType._COMM) {
                curRecord = tree.AddRecord(new GDMCommunicationRecord(tree));
                addHandler = AddCommunicationRecordTag;

            } else if (tagName == GEDCOMTagType._LOC) {
                curRecord = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = AddLocationRecordTag;

            } else if (tagName == GEDCOMTagType.HEAD) {
                curRecord = tree.Header;
                addHandler = AddHeaderTag;

            } else if (tagName == GEDCOMTagType.TRLR) {
                curRecord = null;

            } else {
                curRecord = tree.AddRecord(new GDMRecord(tree));
                addHandler = AddRecordTag;
            }

            return CreateReaderStackTuple(tagLevel, curRecord, addHandler);
        }

        private static StackTuple AddIndividualRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMIndividualRecord indiRec = (GDMIndividualRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            curTag = fIndividualTags.CreateTag(indiRec, tagName, tagValue);
            if (curTag != null) {
                if (curTag is GDMCustomEvent) {
                    curTag = indiRec.AddEvent(curTag as GDMCustomEvent);
                    addHandler = AddCustomEventTag;
                } else if (curTag is GDMChildToFamilyLink) {
                    curTag = indiRec.ChildToFamilyLinks.Add(curTag as GDMChildToFamilyLink);
                } else if (curTag is GDMSpouseToFamilyLink) {
                    curTag = indiRec.SpouseToFamilyLinks.Add(curTag as GDMSpouseToFamilyLink);
                } else if (curTag is GDMPersonalName) {
                    curTag = indiRec.AddPersonalName(curTag as GDMPersonalName);
                    addHandler = AddPersonalNameTag;
                } else if (curTag is GDMAssociation) {
                    curTag = indiRec.Associations.Add(curTag as GDMAssociation);
                    addHandler = AddAssociationTag;
                } else if (curTag is GDMAlias) {
                    curTag = indiRec.Aliases.Add(curTag as GDMAlias);
                }
            } else {
                if (tagName == GEDCOMTagType._GROUP) {
                    curTag = indiRec.Groups.Add(new GDMPointer(indiRec, tagName, tagValue));
                } else if (tagName == GEDCOMTagType.SEX) {
                    indiRec.Sex = GEDCOMUtils.GetSexVal(tagValue);
                    curTag = null;
                } else {
                    return AddRecordWithEventsTag(owner, tagLevel, tagName, tagValue);
                }
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddFamilyRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMFamilyRecord famRec = (GDMFamilyRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.HUSB || tagName == GEDCOMTagType.WIFE) {
                curTag = famRec.AddTag(new GDMPointer(famRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.CHIL) {
                curTag = famRec.Children.Add(new GDMPointer(famRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._STAT) {
                famRec.Status = GEDCOMUtils.GetMarriageStatusVal(tagValue);
                curTag = null;
            } else {
                curTag = fFamilyTags.CreateTag(famRec, tagName, tagValue);

                if (curTag != null) {
                    if (curTag is GDMFamilyEvent) {
                        curTag = famRec.AddEvent(curTag as GDMFamilyEvent);
                        addHandler = AddCustomEventTag;
                    }
                } else {
                    return AddRecordWithEventsTag(owner, tagLevel, tagName, tagValue);
                }
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRecordWithEventsTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMRecordWithEvents evtRec = (GDMRecordWithEvents)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.RESN) {
                evtRec.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
                curTag = null;
            } else if (tagName == GEDCOMTagType.SUBM) {
                curTag = evtRec.Submittors.Add(new GDMPointer(evtRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddNoteRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            /*GEDCOMNoteRecord noteRec = (GEDCOMNoteRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;*/

            return AddRecordTag(owner, tagLevel, tagName, tagValue);

            /*return CreateReaderStackTuple(tagLevel, curTag, addHandler);*/
        }

        private static StackTuple AddMultimediaRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.FILE) {
                curTag = mmRec.FileReferences.Add(new GDMFileReferenceWithTitle(mmRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddSourceRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMSourceRecord sourRec = (GDMSourceRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.REPO) {
                curTag = sourRec.RepositoryCitations.Add(new GDMRepositoryCitation(sourRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.DATA) {
                curTag = sourRec.AddTag(new GDMSourceData(sourRec, tagName, tagValue));
                addHandler = AddDataTag;
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRepositoryRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMRepositoryRecord repoRec = (GDMRepositoryRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                return AddAddressTag(repoRec.Address, tagLevel, tagName, tagValue);
            } else {
                // 'ADDR' defines by default
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddGroupRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = groupRec.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType._MEMBER) {
                curTag = groupRec.Members.Add(new GDMPointer(groupRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddResearchRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMResearchRecord resRec = (GDMResearchRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = resRec.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType._STARTDATE || tagName == GEDCOMTagType._STOPDATE) {
                curTag = resRec.AddTag(new GDMDate(resRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._TASK) {
                curTag = resRec.Tasks.Add(new GDMPointer(resRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._COMM) {
                curTag = resRec.Communications.Add(new GDMPointer(resRec, tagName, tagValue));
            } else if (tagName == GEDCOMTagType._GROUP) {
                curTag = resRec.Groups.Add(new GDMPointer(resRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddTaskRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMTaskRecord taskRec = (GDMTaskRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType._STARTDATE || tagName == GEDCOMTagType._STOPDATE) {
                curTag = taskRec.AddTag(new GDMDate(taskRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddCommunicationRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMCommunicationRecord commRec = (GDMCommunicationRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = commRec.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType.DATE) {
                curTag = commRec.AddTag(new GDMDate(commRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddLocationRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            /*GEDCOMLocationRecord locRec = (GEDCOMLocationRecord)owner;
            GEDCOMTag curTag = null;
            AddTagHandler addHandler = null;*/

            return AddRecordTag(owner, tagLevel, tagName, tagValue);

            /*return CreateReaderStackTuple(tagLevel, curTag, addHandler);*/
        }

        private static StackTuple AddSubmissionRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMSubmissionRecord submnRec = (GDMSubmissionRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.SUBM) {
                curTag = submnRec.AddTag(new GDMPointer(submnRec, tagName, tagValue));
            } else {
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddSubmitterRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NAME) {
                curTag = submrRec.AddTag(new GDMPersonalName(submrRec, tagName, tagValue));
                addHandler = AddPersonalNameTag;
            } else if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                return AddAddressTag(submrRec.Address, tagLevel, tagName, tagValue);
            } else if (tagName == GEDCOMTagType.LANG) {
                curTag = submrRec.AddLanguage(new GDMLanguage(submrRec, tagName, tagValue));
            } else {
                // 'ADDR' defines by default
                return AddRecordTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRecordTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMRecord record = (GDMRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NOTE) {
                curTag = record.Notes.Add(new GDMNotes(record, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SOUR) {
                curTag = record.SourceCitations.Add(new GDMSourceCitation(record, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.OBJE) {
                curTag = record.MultimediaLinks.Add(new GDMMultimediaLink(record, tagName, tagValue));
                addHandler = AddMultimediaLinkTag;
            } else if (tagName == GEDCOMTagType.REFN) {
                curTag = record.UserReferences.Add(new GDMUserReference(record, tagName, tagValue));
                addHandler = AddUserReferenceTag;
            } else if (tagName == GEDCOMTagType._UID) {
                record.UID = tagValue;
                curTag = null;
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddHeaderTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMHeader header = (GEDCOMHeader)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = header.AddTag(new GDMDate(header, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SUBM) {
                curTag = header.AddTag(new GDMPointer(header, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SUBN) {
                curTag = header.AddTag(new GDMPointer(header, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddAddressTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMAddress addr = (GDMAddress)owner;
            GDMTag curTag = null;
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

        private static StackTuple AddAssociationTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMAssociation asso = (GDMAssociation)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.RELA) {
                asso.Relation = tagValue;
                curTag = null;
            } else if (tagName == GEDCOMTagType.SOUR) {
                curTag = asso.SourceCitations.Add(new GDMSourceCitation(asso, tagName, tagValue));
            } else {
                return AddPointerWithNotesTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddChangeDateTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMChangeDate changeDate = (GDMChangeDate)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = changeDate.AddTag(new GDMDate(changeDate, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.NOTE) {
                curTag = changeDate.AddTag(new GDMNotes(changeDate, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddCustomEventTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                return AddAddressTag(custEvent.Address, tagLevel, tagName, tagValue);
            } else {
                // define 'PLAC', 'ADDR', 'DATE' by default
                return AddTagWithListsTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddDataTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMSourceData data = (GDMSourceData)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.EVEN) {
                curTag = data.Events.Add(new GDMSourceEvent(data, tagName, tagValue));
                addHandler = AddDataEventTag;
            } else {
                return AddTagWithListsTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddDateStatusTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMDateStatus dateStatus = (GDMDateStatus)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = dateStatus.AddTag(new GDMDate(dateStatus, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddDataEventTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.DATE) {
                curTag = dataEvent.AddTag(new GDMDatePeriod(dataEvent, tagName, tagValue));
            } else {
                // define 'PLAC' by default
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddIndividualOrdinanceTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMIndividualOrdinance indiOrd = (GDMIndividualOrdinance)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.STAT) {
                curTag = indiOrd.AddTag(new GDMDateStatus(indiOrd, tagName, tagValue));
                addHandler = AddDateStatusTag;
            } else {
                // define 'DATE', 'FAMC' by default
                return AddTagWithListsTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddMultimediaLinkTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.FILE) {
                curTag = mmLink.FileReferences.Add(new GDMFileReference(mmLink, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddPersonalNameTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMPersonalName persName = (GDMPersonalName)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.TYPE || tagName == GEDCOMTagType.FONE || tagName == GEDCOMTagType.ROMN || tagName == "_LANG") {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            } else {
                curTag = persName.Pieces.AddTag(tagName, tagValue, null);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddPointerWithNotesTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMPointerWithNotes ptrWN = (GDMPointerWithNotes)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NOTE) {
                curTag = ptrWN.Notes.Add(new GDMNotes(ptrWN, tagName, tagValue));
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddTagWithListsTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMTagWithLists tagWL = (GDMTagWithLists)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.NOTE) {
                curTag = tagWL.Notes.Add(new GDMNotes(tagWL, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SOUR) {
                curTag = tagWL.SourceCitations.Add(new GDMSourceCitation(tagWL, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.OBJE) {
                curTag = tagWL.MultimediaLinks.Add(new GDMMultimediaLink(tagWL, tagName, tagValue));
                addHandler = AddMultimediaLinkTag;
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddUserReferenceTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMUserReference userRef = (GDMUserReference)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.TYPE) {
                userRef.ReferenceType = tagValue;
                curTag = null;
            } else {
                return AddBaseTag(owner, tagLevel, tagName, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddBaseTag(GDMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GDMTag ownerTag = (GDMTag)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            curTag = ownerTag.AddTag(tagName, tagValue, null);

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple CreateReaderStackTuple(int level, GDMTag tag, AddTagHandler addHandler)
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
                IList<GDMRecord> records = fTree.GetRecords().GetList();
                SaveToStream(writer, records);

                writer.Flush();
            }

            fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
        }

        public void SaveToStream(StreamWriter writer, IList<GDMRecord> list)
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

            if (su == GDMCustomDate.GEDCOMDateRangeArray[0] ||
                su == GDMCustomDate.GEDCOMDateRangeArray[1] ||
                su == GDMCustomDate.GEDCOMDateApproximatedArray[1] ||
                su == GDMCustomDate.GEDCOMDateApproximatedArray[2] ||
                su == GDMCustomDate.GEDCOMDateApproximatedArray[3])
            {
                result = result.Remove(0, 4);
            }
            return result;
        }

        /// <summary>
        /// Fix of line errors that are in the files of FamilyTreeBuilder.
        /// </summary>
        private static void FixFTBLine(GDMCustomRecord curRecord, GDMTag curTag, int lineNum, string str)
        {
            try {
                if (curTag.Name == GEDCOMTagType.CONT) {
                    curTag.StringValue += " " + str;
                } else if (curTag is GDMNotes) {
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

        public static GEDCOMFormat GetGEDCOMFormat(GDMTree tree)
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

            f.RegisterTag(GEDCOMTagType.ADDR, GDMAddress.Create, AddAddressTag);
            f.RegisterTag(GEDCOMTagType.ANCI, GDMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.BAPL, GDMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.CHAN, GDMChangeDate.Create, AddChangeDateTag);
            f.RegisterTag(GEDCOMTagType.CONL, GDMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.DATE, GDMDateValue.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.DESI, GDMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.ENDL, GDMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.FAMC, GDMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.LANG, GDMLanguage.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.MAP, GDMMap.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType.PLAC, GDMPlace.Create, AddTagWithListsTag);
            f.RegisterTag(GEDCOMTagType.SLGC, GDMIndividualOrdinance.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.SLGS, GDMSpouseSealing.Create, AddIndividualOrdinanceTag);
            f.RegisterTag(GEDCOMTagType.TIME, GDMTime.Create, AddBaseTag);

            f.RegisterTag("_LANG", GDMLanguage.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType._LOC, GDMPointer.Create, AddBaseTag);
            f.RegisterTag(GEDCOMTagType._POSITION, GDMCutoutPosition.Create, AddBaseTag);

            f = new GEDCOMFactory();
            fIndividualTags = f;

            f.RegisterTag(GEDCOMTagType.NAME, GDMPersonalName.Create, AddPersonalNameTag);
            f.RegisterTag(GEDCOMTagType.FAMC, GDMChildToFamilyLink.Create, AddPointerWithNotesTag);
            f.RegisterTag(GEDCOMTagType.FAMS, GDMSpouseToFamilyLink.Create, AddPointerWithNotesTag);
            f.RegisterTag(GEDCOMTagType.ASSO, GDMAssociation.Create, AddAssociationTag);
            f.RegisterTag(GEDCOMTagType.ALIA, GDMAlias.Create, AddBaseTag);

            f.RegisterTag(GEDCOMTagType.ADOP, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BAPM, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BARM, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BASM, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BIRT, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BLES, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.BURI, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CENS, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CHR, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CHRA, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CONF, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CREM, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DEAT, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EVEN, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EMIG, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.FCOM, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.GRAD, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.IMMI, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NATU, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.ORDN, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.PROB, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RETI, GDMIndividualEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.WILL, GDMIndividualEvent.Create, AddCustomEventTag);

            f.RegisterTag(GEDCOMTagType.CAST, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DSCR, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EDUC, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.FACT, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.IDNO, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NATI, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NCHI, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.NMR, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.OCCU, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.PROP, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RELI, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RESI, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.SSN, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.TITL, GDMIndividualAttribute.Create, AddCustomEventTag);

            f.RegisterTag(GEDCOMTagType._TRAVEL, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._HOBBY, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._AWARD, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI_IND, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI_DIS, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MILI_RANK, GDMIndividualAttribute.Create, AddCustomEventTag);

            f.RegisterTag(GEDCOMTagType._BGRO, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._EYES, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._HAIR, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._MDNA, GDMIndividualAttribute.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType._YDNA, GDMIndividualAttribute.Create, AddCustomEventTag);

            f = new GEDCOMFactory();
            fFamilyTags = f;

            f.RegisterTag(GEDCOMTagType.ANUL, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.CENS, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DIV, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.DIVF, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.ENGA, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARB, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARC, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARR, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARL, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.MARS, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.RESI, GDMFamilyEvent.Create, AddCustomEventTag);
            f.RegisterTag(GEDCOMTagType.EVEN, GDMFamilyEvent.Create, AddCustomEventTag);
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
