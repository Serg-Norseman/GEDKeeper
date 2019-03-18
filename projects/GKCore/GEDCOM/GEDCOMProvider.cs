/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2019 by Sergey V. Zhdanovskih.
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
using GKCore;

namespace GKCommon.GEDCOM
{
    public sealed class TagProperties
    {
        public readonly string Name;
        public readonly bool SkipEmpty;
        public readonly bool GKExtend;

        public TagProperties(string name, bool skipEmpty, bool extend)
        {
            Name = name;
            SkipEmpty = skipEmpty;
            GKExtend = extend;
        }
    }

    public sealed class GEDCOMAppFormat
    {
        public readonly string Sign;
        public readonly string Name;
        public readonly int PredefCharset;

        public GEDCOMAppFormat(string sign, string name, int predefCharset)
        {
            Sign = sign;
            Name = name;
            PredefCharset = predefCharset;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class GEDCOMProvider
    {
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


        private readonly GEDCOMTree fTree;


        public GEDCOMProvider(GEDCOMTree tree)
        {
            fTree = tree;
        }

        static GEDCOMProvider()
        {
            fTagsBase = CreatePropertiesDict();

            GEDCOMFormats = new GEDCOMAppFormat[] {
                new GEDCOMAppFormat("", "", -1),
                new GEDCOMAppFormat("GEDKeeper", "", -1),
                new GEDCOMAppFormat("GENBOX", "Genbox Family History", -1),
                new GEDCOMAppFormat("ALTREE", "Agelong Tree", -1),
                new GEDCOMAppFormat("AGES", "Ages!", -1),
                new GEDCOMAppFormat("PAF", "Personal Ancestral File", -1),
                new GEDCOMAppFormat("AHN", "Ahnenblatt", -1),
                new GEDCOMAppFormat("├σφσαδεπΦ", "Genealogy (Rus, old)", 1251), // signature in CP437
                new GEDCOMAppFormat("MYHERITAGE", "MyHeritage Family Tree Builder", -1),
            };
        }

        #region Encoding hack

        private enum EncodingState { esUnchecked, esUnchanged, esChanged }

        private const int DEF_CODEPAGE = 437;
        private static readonly Encoding DEFAULT_ENCODING = Encoding.GetEncoding(DEF_CODEPAGE);
        private Encoding fSourceEncoding;
        private EncodingState fEncodingState;

        private void SetEncoding(Encoding encoding)
        {
            fSourceEncoding = encoding;
            fEncodingState = (DEFAULT_ENCODING.Equals(fSourceEncoding)) ? EncodingState.esUnchanged : EncodingState.esChanged;
        }

        private void DefineEncoding(StreamReader reader, GEDCOMFormat format)
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
                    if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        SetEncoding(Encoding.Unicode); // file without BOM
                    } else {
                        fEncodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csANSEL:
                    if (format == GEDCOMFormat.gf_ALTREE) {
                        // Agelong Tree 4.0 with ANSEL is actually characteristic 
                        // for the Russian-language data export
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else {
                        SetEncoding(new AnselEncoding());
                    }
                    break;

                case GEDCOMCharacterSet.csASCII:
                    if (format == GEDCOMFormat.gf_Native) {
                        // GEDKeeper native format (old) and ASCII charset
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else {
                        var fmtProps = GEDCOMFormats[(int)format];
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
                                    SetEncoding(Encoding.GetEncoding(DEF_CODEPAGE));
                                }
                            }
                        }
                    }
                    break;
            }
        }

        #endregion

        #region Loading functions

        public void LoadFromString(string gedcomText)
        {
            using (MemoryStream stream = new MemoryStream(Encoding.UTF8.GetBytes(gedcomText))) {
                LoadFromStreamExt(stream, stream);
            }
        }

        public void LoadFromFile(string fileName)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromStreamExt(fileStream, fileStream);
            }
        }

        public void LoadFromStreamExt(Stream fileStream, Stream inputStream)
        {
            using (StreamReader reader = FileHelper.OpenStreamReader(inputStream, DEFAULT_ENCODING)) {
                fTree.Clear();
                LoadFromStream(fileStream, reader);
                fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }

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
                            byte[] src = DEFAULT_ENCODING.GetBytes(fLineBuffer, 0, linePos);
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

        private void LoadFromStream(Stream fileStream, StreamReader reader)
        {
            fTree.State = GEDCOMState.osLoading;
            try {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                fSourceEncoding = DEFAULT_ENCODING;
                fEncodingState = EncodingState.esUnchecked;

                long fileSize = fileStream.Length;
                int progress = 0;
                var invariantText = GEDCOMUtils.InvariantTextInfo;

                InitBuffers();
                var strTok = new GEDCOMParser(false);
                GEDCOMCustomRecord curRecord = null;
                GEDCOMTag curTag = null;

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
                            DefineEncoding(reader, format);
                        }

                        if (tagName == GEDCOMTagType.INDI) {
                            curRecord = fTree.AddRecord(new GEDCOMIndividualRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.FAM) {
                            curRecord = fTree.AddRecord(new GEDCOMFamilyRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.OBJE) {
                            curRecord = fTree.AddRecord(new GEDCOMMultimediaRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.NOTE) {
                            curRecord = fTree.AddRecord(new GEDCOMNoteRecord(fTree, fTree));
                            curRecord.ParseString(tagValue);
                        } else if (tagName == GEDCOMTagType.REPO) {
                            curRecord = fTree.AddRecord(new GEDCOMRepositoryRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.SOUR) {
                            curRecord = fTree.AddRecord(new GEDCOMSourceRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.SUBN) {
                            curRecord = fTree.AddRecord(new GEDCOMSubmissionRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.SUBM) {
                            curRecord = fTree.AddRecord(new GEDCOMSubmitterRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType._GROUP) {
                            curRecord = fTree.AddRecord(new GEDCOMGroupRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType._RESEARCH) {
                            curRecord = fTree.AddRecord(new GEDCOMResearchRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType._TASK) {
                            curRecord = fTree.AddRecord(new GEDCOMTaskRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType._COMM) {
                            curRecord = fTree.AddRecord(new GEDCOMCommunicationRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType._LOC) {
                            curRecord = fTree.AddRecord(new GEDCOMLocationRecord(fTree, fTree));
                        } else if (tagName == GEDCOMTagType.HEAD) {
                            curRecord = fTree.Header;
                        } else if (tagName == GEDCOMTagType.TRLR) {
                            break;
                        } else {
                            curRecord = null;
                        }

                        if (curRecord != null && tagXRef != "") {
                            curRecord.XRef = tagXRef;
                        }
                        curTag = null;
                    } else {
                        if (curRecord != null) {
                            GEDCOMTag parentTag;
                            if (curTag == null || tagLevel == 1) {
                                parentTag = curRecord;
                            } else {
                                parentTag = curTag;
                                while (tagLevel <= parentTag.Level) {
                                    parentTag = (GEDCOMTag)parentTag.Parent;
                                }
                            }

                            curTag = parentTag.AddTag(tagName, tagValue, null);

                            /*if (curTag != null) {
                                ParseTagValue(fTree, curTag, strTok, tagValue);
                            }*/
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
            } finally {
                fTree.State = GEDCOMState.osReady;
            }
        }

        /*private static void ParseTagValue(GEDCOMTree owner, GEDCOMTag tag, GEDCOMParser lineParser, string strValue)
        {
            var parseFunc = GEDCOMUtils.TagParseFuncs[(int)tag.GetParseFunc()];
            if (parseFunc == null) {
                tag.ParseString(lineParser.GetRest()); // strValue
            } else {
                parseFunc(owner, tag, lineParser);
            }
        }*/

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
                SaveToStream(writer);
                writer.Flush();
            }

            fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
        }

        private void SaveToStream(StreamWriter writer)
        {
            IList<GEDCOMRecord> records = fTree.GetRecords().GetList();

            SaveToStream(writer, records);
        }

        public void SaveToStream(StreamWriter writer, IList<GEDCOMRecord> list)
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
            fTree.Header.SaveToStream(stream);
        }

        private static void SaveFooterToStream(StreamWriter stream)
        {
            const string str = "0 TRLR";
            stream.Write(str + GEDCOMProvider.GEDCOM_NEWLINE);
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
                if (curTag.Name == "CONT") {
                    curTag.StringValue += " " + str;
                } else if (curTag is GEDCOMNotes) {
                    curTag.AddTag("CONT", str, null);
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

                for (GEDCOMFormat gf = GEDCOMFormat.gf_Native; gf <= GEDCOMFormat.gf_Last; gf++) {
                    string fmtSign = GEDCOMProvider.GEDCOMFormats[(int)gf].Sign;
                    if (string.Equals(fmtSign, sour, StringComparison.Ordinal)) {
                        return gf;
                    }
                }
            }

            return GEDCOMFormat.gf_Unknown;
        }

        #endregion

        #region Tag properties

        private static readonly Dictionary<string, TagProperties> fTagsBase;

        private static void RegisterTagProps(Dictionary<string, TagProperties> dict, TagProperties props)
        {
            // TODO
        }

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

        #endregion
    }
}
