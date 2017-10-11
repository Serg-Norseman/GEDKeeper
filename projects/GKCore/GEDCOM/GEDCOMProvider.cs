/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
        public string Sign;
        public string Name;

        public GEDCOMAppFormat(string sign, string name)
        {
            Sign = sign;
            Name = name;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class GEDCOMProvider
    {
        public const char GEDCOM_DELIMITER = ' ';
        public const char GEDCOM_YEAR_MODIFIER_SEPARATOR = '/';
        public const string GEDCOM_YEAR_BC = "B.C.";
        public const char GEDCOM_POINTER_DELIMITER = '@';
        public const string GEDCOM_NEWLINE = "\r\n";

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
                new GEDCOMAppFormat("", ""),
                new GEDCOMAppFormat("GEDKeeper", ""),
                new GEDCOMAppFormat("GENBOX", "Genbox Family History"),
                new GEDCOMAppFormat("ALTREE", "Agelong Tree"),
                new GEDCOMAppFormat("AGES", "Ages!"),
                new GEDCOMAppFormat("PAF", "Personal Ancestral File"),
                new GEDCOMAppFormat("AHN", "Ahnenblatt")
            };
        }

        #region Encoding hack

        private enum EncodingState { esUnchecked, esUnchanged, esChanged }

        private const int DEF_CODEPAGE = 1251;
        private static readonly Encoding DEFAULT_ENCODING = Encoding.GetEncoding(DEF_CODEPAGE);
        private Encoding fSourceEncoding;
        private EncodingState fEncodingState;

        private static string ConvertStr(Encoding encoding, string str)
        {
            byte[] src = DEFAULT_ENCODING.GetBytes(str);
            str = encoding.GetString(src);
            return str;
        }

        private void DefineEncoding(StreamReader reader)
        {
            GEDCOMCharacterSet charSet = fTree.Header.CharacterSet;
            switch (charSet)
            {
                case GEDCOMCharacterSet.csUTF8:
                    if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        fSourceEncoding = Encoding.UTF8;
                        fEncodingState = EncodingState.esChanged; // file without BOM
                    } else {
                        fEncodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        fSourceEncoding = Encoding.Unicode;
                        fEncodingState = EncodingState.esChanged; // file without BOM
                    } else {
                        fEncodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csASCII:
                    string cpVers = fTree.Header.CharacterSetVersion;
                    if (!string.IsNullOrEmpty(cpVers)) {
                        int sourceCodepage = SysUtils.ParseInt(cpVers, DEF_CODEPAGE);
                        fSourceEncoding = Encoding.GetEncoding(sourceCodepage);
                        fEncodingState = EncodingState.esChanged;
                    } else {
                        fSourceEncoding = Encoding.GetEncoding(DEF_CODEPAGE);
                        fEncodingState = EncodingState.esChanged;
                    }
                    break;
            }
        }

        #endregion

        #region Loading functions

        public void LoadFromString(string gedcomText)
        {
            using (MemoryStream stream = new MemoryStream(Encoding.UTF8.GetBytes(gedcomText))) {
                LoadFromStreamExt(stream, stream, "");
            }
        }

        public void LoadFromFile(string fileName)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromStreamExt(fileStream, fileStream, fileName);
            }
        }

        public void LoadFromStreamExt(Stream fileStream, Stream inputStream, string fileName)
        {
            using (StreamReader reader = SysUtils.OpenStreamReader(inputStream, DEFAULT_ENCODING)) {
                fTree.Clear();
                LoadFromStream(fileStream, reader);
                fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }

        private void LoadFromStream(Stream fileStream, StreamReader reader)
        {
            fTree.State = GEDCOMState.osLoading;
            try
            {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                fSourceEncoding = DEFAULT_ENCODING;
                fEncodingState = EncodingState.esUnchecked;
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

                    if (!SysUtils.IsDigit(str[0]))
                    {
                        FixFTBLine(curRecord, curTag, lineNum, str);
                    }
                    else
                    {
                        int tagLevel;
                        string tagXRef = "", tagName, tagValue = "";

                        try
                        {
                            var strTok = new StringTokenizer(str);
                            strTok.RecognizeDecimals = false;
                            strTok.IgnoreWhiteSpace = false;
                            strTok.RecognizeIdents = true;

                            var token = strTok.Next(); // already trimmed
                            if (token.Kind != TokenKind.Number) {
                                // syntax error
                                throw new EGEDCOMException(string.Format("The string {0} doesn't start with a valid number", str));
                            }
                            tagLevel = (int)token.ValObj;

                            token = strTok.Next();
                            if (token.Kind != TokenKind.WhiteSpace) {
                                // syntax error
                            }

                            token = strTok.Next();
                            if (token.Kind == TokenKind.Symbol && token.Value[0] == '@')
                            {
                                token = strTok.Next();
                                while (token.Kind != TokenKind.Symbol && token.Value[0] != '@') {
                                    tagXRef += token.Value;
                                    token = strTok.Next();
                                }
                                // FIXME: check for errors
                                //throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", str));
                                //throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", str));

                                token = strTok.Next();
                                strTok.SkipWhiteSpaces();
                            }

                            token = strTok.CurrentToken;
                            if (token.Kind != TokenKind.Word && token.Kind != TokenKind.Ident) {
                                // syntax error
                            }
                            tagName = token.Value.ToUpperInvariant();

                            token = strTok.Next();
                            if (token.Kind == TokenKind.WhiteSpace) {
                                tagValue = strTok.GetRest();
                            }
                        }
                        catch (EGEDCOMException ex)
                        {
                            throw new EGEDCOMException("Syntax error in line " + Convert.ToString(lineNum) + ".\r" + ex.Message);
                        }

                        // convert codepages
                        if (!string.IsNullOrEmpty(tagValue) && fEncodingState == EncodingState.esChanged)
                        {
                            tagValue = ConvertStr(fSourceEncoding, tagValue);
                        }

                        if (tagLevel == 0)
                        {
                            if (curRecord == fTree.Header && fEncodingState == EncodingState.esUnchecked) {
                                // beginning recognition of the first is not header record
                                // to check for additional versions of the code page
                                DefineEncoding(reader);
                            }

                            if (tagName == "INDI")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMIndividualRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "FAM")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMFamilyRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "OBJE")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMMultimediaRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "NOTE")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMNoteRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "REPO")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMRepositoryRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "SOUR")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMSourceRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "SUBN")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMSubmissionRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "SUBM")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMSubmitterRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "_GROUP")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMGroupRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "_RESEARCH")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMResearchRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "_TASK")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMTaskRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "_COMM")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMCommunicationRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "_LOC")
                            {
                                curRecord = fTree.AddRecord(new GEDCOMLocationRecord(fTree, fTree, "", ""));
                            }
                            else if (tagName == "HEAD")
                            {
                                curRecord = fTree.Header;
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

                    if (progressHandler != null) {
                        int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);

                        if (progress != newProgress) {
                            progress = newProgress;
                            progressHandler(fTree, progress);
                        }
                    }
                }
            }
            finally
            {
                fTree.State = GEDCOMState.osReady;
            }
        }

        #endregion

        #region Saving functions

        public void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            using (FileStream fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write))
            {
                SaveToStreamExt(fileStream, fileName, charSet);
            }
        }

        public void SaveToStreamExt(Stream outputStream, string fileName, GEDCOMCharacterSet charSet)
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
                Logger.LogWrite("GEDCOMProvider.FixFTBLine(): Line " + lineNum.ToString() + " failed correct: " + ex.Message);
            }
        }

        public static GEDCOMFormat GetGEDCOMFormat(GEDCOMTree tree)
        {
            if (tree != null) {
                string sour = tree.Header.Source;

                for (GEDCOMFormat gf = GEDCOMFormat.gf_Native; gf <= GEDCOMFormat.gf_Last; gf++) {
                    if (GEDCOMProvider.GEDCOMFormats[(int)gf].Sign == sour) {
                        return gf;
                    }
                }
            }

            return GEDCOMFormat.gf_Unknown;
        }

        #endregion

        #region Tag properties

        private static readonly Dictionary<string, TagProperties> fTagsBase;

        private static Dictionary<string, TagProperties> CreatePropertiesDict()
        {
            var result = new Dictionary<string, TagProperties>();

            result.Add("ADDR", new TagProperties("ADDR", true, false));
            result.Add("AGNC", new TagProperties("AGNC", true, false));
            result.Add("AUTH", new TagProperties("AUTH", true, false));
            result.Add("CAUS", new TagProperties("CAUS", true, false));
            result.Add("CHAN", new TagProperties("CHAN", true, false));
            result.Add("CITY", new TagProperties("CITY", true, false));
            result.Add("CTRY", new TagProperties("CTRY", true, false));
            result.Add("DATE", new TagProperties("DATE", true, false));
            result.Add("PAGE", new TagProperties("PAGE", true, false));
            result.Add("PLAC", new TagProperties("PLAC", true, false));
            result.Add("POST", new TagProperties("POST", true, false));
            result.Add("PUBL", new TagProperties("PUBL", true, false));
            result.Add("RESN", new TagProperties("RESN", true, false));
            result.Add("STAE", new TagProperties("STAE", true, false));
            result.Add("TEXT", new TagProperties("TEXT", true, false));
            result.Add("TIME", new TagProperties("TIME", true, false));
            result.Add("TYPE", new TagProperties("TYPE", true, false));
            result.Add("SUBM", new TagProperties("SUBM", true, false));
            result.Add("VERS", new TagProperties("VERS", true, false));
            result.Add("LANG", new TagProperties("LANG", true, false));

            result.Add("NPFX", new TagProperties("NPFX", true, false));
            result.Add("GIVN", new TagProperties("GIVN", true, false));
            result.Add("NICK", new TagProperties("NICK", true, false));
            result.Add("SPFX", new TagProperties("SPFX", true, false));
            result.Add("SURN", new TagProperties("SURN", true, false));
            result.Add("NSFX", new TagProperties("NSFX", true, false));

            result.Add("_PATN", new TagProperties("_PATN", true, true));
            result.Add("_MARN", new TagProperties("_MARN", true, true));
            result.Add("_RELN", new TagProperties("_RELN", true, true));
            result.Add("_CENN", new TagProperties("_CENN", true, true));

            result.Add("_LOC", new TagProperties("_LOC", true,  true));
            result.Add("_POSITION", new TagProperties("_POSITION", true,  true));
            result.Add("ALIA", new TagProperties("ALIA", true, false));

            #if !DEBUG
            // need for compatibility with Agelong Tree (ALTREE), and other
            result.Add("HUSB", new TagProperties("HUSB", true, false));
            result.Add("WIFE", new TagProperties("WIFE", true, false));
            #endif

            result.Add("_BGRO", new TagProperties("_BGRO", true,  true));
            result.Add("_HAIR", new TagProperties("_HAIR", true,  true));
            result.Add("_EYES", new TagProperties("_EYES", true,  true));
            result.Add("_MDNA", new TagProperties("_MDNA", true,  true));
            result.Add("_YDNA", new TagProperties("_YDNA", true,  true));

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
