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
using System.Globalization;
using System.IO;
using System.Text;
using BSLib;
using GDModel;

namespace GDModel.Providers.GEDCOM
{
    public sealed class EnumTuple : IComparable<EnumTuple>
    {
        public string Key;
        public int Value;

        public EnumTuple(string key, int value)
        {
            Key = key;
            Value = value;
        }

        public int CompareTo(EnumTuple other)
        {
            // GEDCOM enums is ASCII identifiers
            return string.Compare(Key, other.Key, StringComparison.OrdinalIgnoreCase);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public static class GEDCOMUtils
    {
        public static readonly TextInfo InvariantTextInfo = CultureInfo.InvariantCulture.TextInfo;
        public static readonly NumberFormatInfo InvariantNumberFormatInfo = NumberFormatInfo.InvariantInfo;


        #region Parse functions

        public static string Trim(string str)
        {
            if (string.IsNullOrEmpty(str)) return string.Empty;

            int li = 0;
            int ri = str.Length - 1;

            while (li < ri && str[li] <= ' ') li++;
            while (ri >= li && str[ri] <= ' ') ri--;

            string result = str.Substring(li, ri - li + 1);
            return result;
        }

        /// <summary>
        /// This function is optimized for maximum performance and combines Trim and ToLower operations.
        /// </summary>
        public static string NormalizeLo(string str, bool trim = true)
        {
            if (string.IsNullOrEmpty(str)) return string.Empty;

            char[] strChars = str.ToCharArray();
            int li = 0;
            int ri = strChars.Length - 1;

            if (trim) {
                while (li < ri && strChars[li] <= ' ') li++;
                while (ri >= li && strChars[ri] <= ' ') ri--;
            }

            for (int i = li; i <= ri; i++) {
                char ch = strChars[i];
                char ltr = (char)(ch | ' ');
                if (ltr >= 'a' && ltr <= 'z') {
                    strChars[i] = ltr;
                } else {
                    strChars[i] = ch;
                }
            }

            string result = new string(strChars, li, ri - li + 1);
            return result;
        }

        /// <summary>
        /// This function is optimized for maximum performance and combines Trim and ToUpper operations.
        /// </summary>
        public static string NormalizeUp(string str, bool trim = true)
        {
            if (string.IsNullOrEmpty(str)) return string.Empty;

            char[] strChars = str.ToCharArray();
            int li = 0;
            int ri = strChars.Length - 1;

            if (trim) {
                while (li < ri && strChars[li] <= ' ') li++;
                while (ri >= li && strChars[ri] <= ' ') ri--;
            }

            for (int i = li; i <= ri; i++) {
                char ch = strChars[i];
                char ltr = (char)(ch & -33);
                if (ltr >= 'A' && ltr <= 'Z') {
                    strChars[i] = ltr;
                } else {
                    strChars[i] = ch;
                }
            }

            string result = new string(strChars, li, ri - li + 1);
            return result;
        }

        public static string CleanXRef(string str)
        {
            string result = str;
            if (!string.IsNullOrEmpty(str)) {
                char[] strChars = str.ToCharArray();
                int strLen = strChars.Length;
                int sx = -1;
                for (int i = 0; i < strLen; i++) {
                    char chr = strChars[i];
                    if (chr == GEDCOMProvider.GEDCOM_POINTER_DELIMITER) {
                        if (sx == -1) {
                            sx = i;
                        } else {
                            result = new string(strChars, sx + 1, i - 1 - sx);
                            break;
                        }
                    }
                }
            }
            return result;
        }

        public static string EncloseXRef(string xref)
        {
            if (!string.IsNullOrEmpty(xref)) {
                if (xref[0] != '@') {
                    xref = "@" + xref;
                }

                if (xref[xref.Length - 1] != '@') {
                    xref += "@";
                }
            }
            return xref;
        }

        // Performance improvement: x3.5
        public static int GetXRefNumber(string str)
        {
            if (string.IsNullOrEmpty(str)) {
                return -1;
            }

            char[] strChars = str.ToCharArray();
            int strLen = strChars.Length;

            int result = 0;
            for (int i = 0; i < strLen; i++) {
                char ch = strChars[i];
                if (ch >= '0' && ch <= '9') {
                    result = (result * 10 + ((int)ch - 48));
                }
            }
            return result;
        }

        #endregion

        #region Special parsing routines

        // Line format: <level>_<@xref@>_<tag>_<value> (for test's purpose)
        public static int ParseTag(string str, out int tagLevel, out string tagXRef, out string tagName, out string tagValue)
        {
            var strTok = new GEDCOMParser(str, false);
            return ParseTag(strTok, out tagLevel, out tagXRef, out tagName, out tagValue);
        }

        // Line format: <level>_<@xref@>_<tag>_<value>
        public static int ParseTag(GEDCOMParser strTok, out int tagLevel, out string tagXRef, out string tagName, out string tagValue)
        {
            tagLevel = 0;
            tagXRef = string.Empty;
            tagName = string.Empty;
            tagValue = string.Empty;

            int result = 0;
            //var strTok = new GEDCOMParser(str, false);
            strTok.SkipWhitespaces();

            var token = strTok.CurrentToken; // already trimmed
            if (token == GEDCOMToken.EOL) {
                return -2;
            }
            if (token != GEDCOMToken.Number) {
                tagValue = /*str;//*/strTok.GetFullStr();
                return -1;
            }
            tagLevel = strTok.GetNumber();
            result += 1;

            token = strTok.Next();
            if (token != GEDCOMToken.Whitespace) {
                // syntax error
            }

            token = strTok.Next();
            if (token == GEDCOMToken.XRef) {
                tagXRef = strTok.GetWord();

                // FIXME: check for errors
                //throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", str));
                //throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", str));
                result += 1;

                token = strTok.Next();
                strTok.SkipWhitespaces();
            }

            token = strTok.CurrentToken;
            if (token != GEDCOMToken.Word) {
                // syntax error
            }
            tagName = strTok.GetWord();
            result += 1;

            token = strTok.Next();
            if (token == GEDCOMToken.Whitespace) {
                tagValue = strTok.GetRest();
                result += 1;
            }

            return result;
        }

        // XRefPtr format: ...@<xref>@...
        public static string ParseXRefPointer(string str, out string xref)
        {
            xref = string.Empty;
            if (str == null) {
                return string.Empty;
            }

            char[] strChars = str.ToCharArray();
            int strLen = strChars.Length;

            // skip leading whitespaces
            int strBeg = 0;
            while (strBeg < strLen && strChars[strBeg] == ' ') strBeg++;

            // check empty string
            if (strLen - strBeg == 0) {
                return string.Empty;
            }

            int init = -1, fin = strBeg;
            for (int i = strBeg; i < strLen; i++) {
                char chr = strChars[i];
                if (chr == GEDCOMProvider.GEDCOM_POINTER_DELIMITER) {
                    if (init == -1) {
                        init = i;
                    } else {
                        fin = i;
                        xref = new string(strChars, init, fin + 1 - init);
                        fin += 1;
                        break;
                    }
                } else if (chr == '#' && i == init + 1) {
                    break;
                }
            }

            xref = GEDCOMUtils.CleanXRef(xref);
            return new string(strChars, fin, strLen - fin);
        }

        // Time format: hour:minutes:seconds.fraction
        public static string ParseTime(string strValue, GDMTime time)
        {
            byte hour = 0;
            byte minutes = 0;
            byte seconds = 0;
            short fraction = 0;

            if (!string.IsNullOrEmpty(strValue)) {
                var strTok = new GEDCOMParser(strValue, true);

                hour = (byte)strTok.RequestNextInt();
                strTok.RequestNextSymbol(':');
                minutes = (byte)strTok.RequestNextInt();

                var tok = strTok.Next();
                if (tok == GEDCOMToken.Symbol && strTok.GetSymbol() == ':') {
                    seconds = (byte)strTok.RequestNextInt();

                    tok = strTok.Next();
                    if (tok == GEDCOMToken.Symbol && strTok.GetSymbol() == '.') {
                        fraction = (short)strTok.RequestNextInt();
                    }
                }

                strValue = strTok.GetRest();
            }

            time.SetRawData(hour, minutes, seconds, fraction);

            return strValue;
        }

        // CutoutPosition format: x1 y1 x2 y2
        public static string ParseCutoutPosition(string strValue, GDMCutoutPosition position)
        {
            int x1 = 0;
            int y1 = 0;
            int x2 = 0;
            int y2 = 0;

            if (!string.IsNullOrEmpty(strValue)) {
                var parser = new GEDCOMParser(strValue, true);
                x1 = parser.RequestNextInt();
                y1 = parser.RequestNextInt();
                x2 = parser.RequestNextInt();
                y2 = parser.RequestNextInt();
            }

            position.SetRawData(x1, y1, x2, y2);

            return string.Empty;
        }

        // DateValue format: INT/FROM/TO/etc..._<date>
        public static string ParseDateValue(GDMTree owner, GDMDateValue dateValue, string str)
        {
            if (str == null) {
                return null;
            }

            var strTok = new GEDCOMParser(str, false);
            return ParseDateValue(owner, dateValue, strTok);
        }

        // DateValue format: INT/FROM/TO/etc..._<date>
        public static string ParseDateValue(GDMTree owner, GDMTag dateValue, GEDCOMParser strTok)
        {
            return ParseDateValue(owner, (GDMDateValue)dateValue, strTok);
        }

        // DateValue format: INT/FROM/TO/etc..._<date>
        public static string ParseDateValue(GDMTree owner, GDMDateValue dateValue, GEDCOMParser strTok)
        {
            strTok.SkipWhitespaces();

            int idx = 0;
            var token = strTok.CurrentToken;
            if (token == GEDCOMToken.Word) {
                string su = strTok.GetWord();
                idx = Algorithms.BinarySearch(GDMCustomDate.GEDCOMDateTypes, su, string.CompareOrdinal);
            }
            var dateType = (idx < 0) ? GEDCOMDateType.SIMP : (GEDCOMDateType)idx;

            string result;
            GDMCustomDate date;
            switch (dateType) {
                case GEDCOMDateType.AFT:
                case GEDCOMDateType.BEF:
                case GEDCOMDateType.BET:
                    date = new GDMDateRange(dateValue);
                    result = GEDCOMUtils.ParseRangeDate(owner, (GDMDateRange)date, strTok);
                    break;
                case GEDCOMDateType.INT:
                    date = new GDMDateInterpreted(dateValue);
                    result = GEDCOMUtils.ParseIntDate(owner, (GDMDateInterpreted)date, strTok);
                    break;
                case GEDCOMDateType.FROM:
                case GEDCOMDateType.TO:
                    date = new GDMDatePeriod(dateValue);
                    result = GEDCOMUtils.ParsePeriodDate(owner, (GDMDatePeriod)date, strTok);
                    break;
                default:
                    date = new GDMDate(dateValue);
                    result = GEDCOMUtils.ParseDate(owner, (GDMDate)date, strTok);
                    break;
            }

            dateValue.SetRawData(date);
            return result;
        }

        // Format: FROM DATE1 TO DATE2
        public static string ParsePeriodDate(GDMTree owner, GDMDatePeriod date, string strValue)
        {
            var strTok = new GEDCOMParser(strValue, false);
            return ParsePeriodDate(owner, date, strTok);
        }

        // Format: FROM DATE1 TO DATE2
        public static string ParsePeriodDate(GDMTree owner, GDMDatePeriod date, GEDCOMParser strTok)
        {
            strTok.SkipWhitespaces();

            if (strTok.RequireWord(GEDCOMTagType.FROM)) {
                strTok.Next();
                ParseDate(owner, date.DateFrom, strTok);
                strTok.SkipWhitespaces();
            }

            if (strTok.RequireWord(GEDCOMTagType.TO)) {
                strTok.Next();
                ParseDate(owner, date.DateTo, strTok);
                strTok.SkipWhitespaces();
            }

            return strTok.GetRest();
        }

        // Format: AFT DATE | BEF DATE | BET AFT_DATE AND BEF_DATE
        public static string ParseRangeDate(GDMTree owner, GDMDateRange date, string strValue)
        {
            var strTok = new GEDCOMParser(strValue, false);
            return ParseRangeDate(owner, date, strTok);
        }

        // Format: AFT DATE | BEF DATE | BET AFT_DATE AND BEF_DATE
        public static string ParseRangeDate(GDMTree owner, GDMDateRange date, GEDCOMParser strTok)
        {
            strTok.SkipWhitespaces();

            var token = strTok.CurrentToken;
            if (token != GEDCOMToken.Word) {
                // error!
            }
            string su = strTok.GetWord();
            int dateType = Algorithms.BinarySearch(GDMCustomDate.GEDCOMDateRangeArray, su, string.CompareOrdinal);

            if (dateType == 0) { // "AFT"
                strTok.Next();
                ParseDate(owner, date.After, strTok);
            } else if (dateType == 1) { // "BEF"
                strTok.Next();
                ParseDate(owner, date.Before, strTok);
            } else if (dateType == 2) { // "BET"
                strTok.Next();
                //result = GEDCOMProvider.FixFTB(result);
                ParseDate(owner, date.After, strTok);
                strTok.SkipWhitespaces();

                if (!strTok.RequireWord(GDMCustomDate.GEDCOMDateRangeArray[3])) { // "AND"
                    throw new GDMDateException(string.Format("The range date '{0}' doesn't contain 'and' token", strTok.GetFullStr()));
                }

                strTok.Next();
                strTok.SkipWhitespaces();
                //result = GEDCOMProvider.FixFTB(result);
                ParseDate(owner, date.Before, strTok);
            }

            return strTok.GetRest();
        }

        // Format: INT DATE (phrase)
        public static string ParseIntDate(GDMTree owner, GDMDateInterpreted date, string strValue)
        {
            var strTok = new GEDCOMParser(strValue, false);
            return ParseIntDate(owner, date, strTok);
        }

        // Format: INT DATE (phrase)
        public static string ParseIntDate(GDMTree owner, GDMDateInterpreted date, GEDCOMParser strTok)
        {
            strTok.SkipWhitespaces();

            if (!strTok.RequireWord(GEDCOMTagType.INT)) {
                throw new GDMDateException(string.Format("The interpreted date '{0}' doesn't start with a valid ident", strTok.GetFullStr()));
            }
            strTok.Next();
            ParseDate(owner, date, strTok);

            strTok.SkipWhitespaces();
            var token = strTok.CurrentToken;
            if (token == GEDCOMToken.Symbol && strTok.GetSymbol() == '(') {
                var phrase = new StringBuilder();
                phrase.Append(strTok.GetWord());
                do {
                    token = strTok.Next();
                    phrase.Append(strTok.GetWord());
                } while (token != GEDCOMToken.Symbol || strTok.GetSymbol() != ')');

                date.DatePhrase = phrase.ToString();
            } else {
                date.DatePhrase = string.Empty;
            }

            return strTok.GetRest();
        }

        public static string ParseDate(GDMTree owner, GDMDate date, string strValue)
        {
            var strTok = new GEDCOMParser(strValue, false);
            return ParseDate(owner, date, strTok);
        }

        public static string ParseDate(GDMTree owner, GDMTag date, GEDCOMParser strTok)
        {
            return ParseDate(owner, (GDMDate)date, strTok);
        }

        public static string ParseDate(GDMTree owner, GDMDate date, GEDCOMParser strTok)
        {
            GEDCOMApproximated approximated = GEDCOMApproximated.daExact;
            GEDCOMCalendar calendar = GEDCOMCalendar.dcGregorian;
            short year = GDMDate.UNKNOWN_YEAR;
            bool yearBC = false;
            string yearModifier = string.Empty;
            byte month = 0;
            byte day = 0;
            GEDCOMDateFormat dateFormat = GEDCOMDateFormat.dfGEDCOMStd;

            strTok.SkipWhitespaces();

            GEDCOMFormat format = (owner == null) ? GEDCOMFormat.gf_Native : owner.Format;
            bool isAhnDeviance = (format == GEDCOMFormat.gf_Ahnenblatt);

            var token = strTok.CurrentToken;
            if (isAhnDeviance && token == GEDCOMToken.Symbol && strTok.GetSymbol() == '(') {
                GEDCOMUtils.PrepareAhnenblattDate(strTok.Data, strTok.Position, strTok.Length);
                token = strTok.Next();
            }

            // extract approximated
            token = strTok.CurrentToken;
            if (token == GEDCOMToken.Word) {
                string su = InvariantTextInfo.ToUpper(strTok.GetWord());
                int idx = Algorithms.BinarySearch(GDMCustomDate.GEDCOMDateApproximatedArray, su, string.CompareOrdinal);
                if (idx >= 0) {
                    approximated = (GEDCOMApproximated)idx;
                    strTok.Next();
                    strTok.SkipWhitespaces();
                }
            }

            // extract escape
            token = strTok.CurrentToken;
            if (token == GEDCOMToken.XRef) {
                // FIXME: check for errors
                var escapeStr = "@" + strTok.GetWord() + "@";
                int idx = Algorithms.IndexOf(GDMCustomDate.GEDCOMDateEscapeArray, escapeStr);
                if (idx >= 0) {
                    calendar = (GEDCOMCalendar)idx;
                }

                strTok.Next();
                strTok.SkipWhitespaces();
            }

            // extract day
            token = strTok.CurrentToken;
            int dNum;
            if (token == GEDCOMToken.Number && ((dNum = strTok.GetNumber()) <= 31)) {
                day = (byte)dNum;
                token = strTok.Next();
            }

            // extract delimiter
            if (token == GEDCOMToken.Whitespace && strTok.GetSymbol() == ' ') {
                dateFormat = GEDCOMDateFormat.dfGEDCOMStd;
                token = strTok.Next();
            } else if (token == GEDCOMToken.Symbol && strTok.GetSymbol() == '.') {
                dateFormat = GEDCOMDateFormat.dfSystem;
                token = strTok.Next();
            }

            // extract month
            if (token == GEDCOMToken.Word) {
                // in this case, according to performance test results, BinarySearch is more efficient
                // than a simple search or even a dictionary search (why?!)
                string su = InvariantTextInfo.ToUpper(strTok.GetWord());
                int idx = BinarySearch(GDMCustomDate.GEDCOMMonthValues, su, string.CompareOrdinal);
                month = (byte)((idx < 0) ? 0 : idx);

                token = strTok.Next();
            } else if (dateFormat == GEDCOMDateFormat.dfSystem && token == GEDCOMToken.Number) {
                month = (byte)strTok.GetNumber();

                token = strTok.Next();
            }

            // extract delimiter
            if (dateFormat == GEDCOMDateFormat.dfSystem) {
                if (token == GEDCOMToken.Symbol && strTok.GetSymbol() == '.') {
                    token = strTok.Next();
                }
            } else {
                if (token == GEDCOMToken.Whitespace && strTok.GetSymbol() == ' ') {
                    token = strTok.Next();
                }
            }

            // extract year
            if (token == GEDCOMToken.Number) {
                year = (short)strTok.GetNumber();
                token = strTok.Next();

                // extract year modifier
                if (token == GEDCOMToken.Symbol && strTok.GetSymbol() == '/') {
                    token = strTok.Next();
                    if (token != GEDCOMToken.Number) {
                        // error
                    } else {
                        yearModifier = strTok.GetWord();
                    }
                    token = strTok.Next();
                }

                // extract bc/ad
                if (token == GEDCOMToken.Word && strTok.GetWord() == "B") {
                    token = strTok.Next();
                    if (token != GEDCOMToken.Symbol || strTok.GetSymbol() != '.') {
                        // error
                    }
                    token = strTok.Next();
                    if (token != GEDCOMToken.Word || strTok.GetWord() != "C") {
                        // error
                    }
                    token = strTok.Next();
                    if (token != GEDCOMToken.Symbol || strTok.GetSymbol() != '.') {
                        // error
                    }
                    strTok.Next();
                    yearBC = true;
                }
            }

            token = strTok.CurrentToken;
            if (isAhnDeviance && token == GEDCOMToken.Symbol && strTok.GetSymbol() == ')') {
                token = strTok.Next();
            }

            date.SetRawData(approximated, calendar, year, yearBC, yearModifier, month, day, dateFormat);
            string result = strTok.GetRest();
            return result;
        }

        private static void PrepareAhnenblattDate(char[] str, int startIndex, int length)
        {
            // ALERT: Ahnenblatt GEDCOM files can contain the dates with any separator and in (...)!
            // by standard it's "(<DATE_PHRASE>)" (gedcom-5.5.1, p.47)
            // FIXME: this code need to move to GEDCOMDateInterpreted?

            // Different execution path if the input is from a string or a parser
            int lastIndex = Math.Min(str.Length - 1, startIndex + length - 1);

            for (int i = startIndex; i <= lastIndex; i++) {
                char ch = str[i];
                if (ch == '/' || ch == '-' || ch == ' ') {
                    str[i] = '.';
                }
            }
        }

        public static string ParseName(string strValue, GDMPersonalName persName)
        {
            string firstPart = string.Empty;
            string surname = string.Empty;
            string lastPart = string.Empty;

            if (strValue == null) {
                return string.Empty;
            }

            char[] strChars = strValue.ToCharArray();
            int strLen = strChars.Length;

            // skip leading whitespaces
            int strBeg = 0;
            while (strBeg < strLen && strChars[strBeg] == ' ') strBeg++;

            // check empty string
            if (strLen - strBeg == 0) {
                return string.Empty;
            }

            int fs = -1, ss = strBeg;
            for (int i = strBeg; i < strLen; i++) {
                char chr = strChars[i];
                if (chr == GEDCOMProvider.GEDCOM_NAME_SEPARATOR) {
                    if (fs == -1) {
                        fs = i;
                        firstPart = new string(strChars, 0, i);
                    } else {
                        ss = i;
                        surname = new string(strChars, fs + 1, (ss - fs) - 1);
                        ss += 1;
                        break;
                    }
                }
            }

            if (fs < 0) {
                firstPart = new string(strChars, ss, strLen - ss);
            } else {
                lastPart = new string(strChars, ss, strLen - ss);
            }

            persName.SetNameParts(firstPart, surname, lastPart);

            return string.Empty;
        }

        #endregion

        #region GEDCOM Enums Parse

        public static string Enum2Str(IConvertible enumVal, string[] values)
        {
            #if PCL
            int idx = (int)Convert.ChangeType(enumVal, typeof(int), null);
            #else
            int idx = (int)enumVal;
            #endif

            if (idx < 0 || idx >= values.Length) {
                return string.Empty;
            } else {
                return values[idx];
            }
        }

        /**
         * Performance (100.000 iterations, random access):
         *  - if-based (origin): 430 (100 %), 417 (100 %)
         *  - simple-loop based (test): 449 (104 %), 437 (104.8 %)
         *  - dict-based (GEDCOMEnumHelper): 399 (92.8 %), 397 (92 %)
         *  - bin-search-based: 326 (75.8 %), 324 (77.7 %)
         * 
         * On all tests wins BinarySearch-based.
         */
        public static T Str2Enum<T>(string val, string[] values, T defVal, bool normalize = true)
        {
            if (string.IsNullOrEmpty(val)) return defVal;

            if (normalize) {
                val = GEDCOMUtils.InvariantTextInfo.ToLower(val.Trim());
            }

            int idx = Algorithms.BinarySearch<string>(values, val, string.CompareOrdinal);
            if (idx >= 0) {
                #if PCL
                return (T)Convert.ChangeType(idx, typeof(T), null);
                #else
                return (T)((IConvertible)idx);
                #endif
            } else {
                return defVal;
            }
        }

        #endregion

        #region GEDCOM Enums processing

        public static Encoding GetEncodingByCharacterSet(GEDCOMCharacterSet cs)
        {
            Encoding res = Encoding.Default;

            switch (cs) {
                case GEDCOMCharacterSet.csANSEL:
                    res = new AnselEncoding();
                    break;

                case GEDCOMCharacterSet.csASCII:
                    res = Encoding.GetEncoding(1251);
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    res = Encoding.Unicode;
                    break;

                case GEDCOMCharacterSet.csUTF8:
                    res = Encoding.UTF8;
                    break;
            }

            return res;
        }


        public static GEDCOMRestriction GetRestrictionVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMRestriction.rnNone;

            GEDCOMRestriction res;
            str = str.Trim().ToLowerInvariant();
            
            if (str == "confidential") {
                res = GEDCOMRestriction.rnConfidential;
            } else if (str == "locked") {
                res = GEDCOMRestriction.rnLocked;
            } else if (str == "privacy") {
                res = GEDCOMRestriction.rnPrivacy;
            } else {
                res = GEDCOMRestriction.rnNone;
            }
            return res;
        }

        public static string GetRestrictionStr(GEDCOMRestriction value)
        {
            string s;

            switch (value) {
                case GEDCOMRestriction.rnConfidential:
                    s = "confidential";
                    break;

                case GEDCOMRestriction.rnLocked:
                    s = "locked";
                    break;

                case GEDCOMRestriction.rnPrivacy:
                    s = "privacy";
                    break;

                default:
                    s = "";
                    break;
            }

            return s;
        }


        public static string[] PedigreeLinkageTypes = new string[] {
            "", "adopted", "birth", "foster" };

        public static GDMPedigreeLinkageType GetPedigreeLinkageTypeVal(string str)
        {
            return Str2Enum(str, PedigreeLinkageTypes, GDMPedigreeLinkageType.plNone);
        }

        public static string GetPedigreeLinkageTypeStr(GDMPedigreeLinkageType value)
        {
            return GEDCOMUtils.Enum2Str(value, PedigreeLinkageTypes);
        }


        public static string[] ChildLinkageStatuses = new string[] {
            "", "challenged", "disproven", "proven" };

        public static GDMChildLinkageStatus GetChildLinkageStatusVal(string str)
        {
            return Str2Enum(str, ChildLinkageStatuses, GDMChildLinkageStatus.clNone);
        }

        public static string GetChildLinkageStatusStr(GDMChildLinkageStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, ChildLinkageStatuses);
        }


        public static string[] CommunicationTypes = new string[] {
            "call", "email", "fax", "letter", "tape", "visit" };

        public static GDMCommunicationType GetCommunicationTypeVal(string str)
        {
            return Str2Enum(str, CommunicationTypes, GDMCommunicationType.ctVisit);
        }

        public static string GetCommunicationTypeStr(GDMCommunicationType value)
        {
            return GEDCOMUtils.Enum2Str(value, CommunicationTypes);
        }


        public static GEDCOMMultimediaFormat GetMultimediaFormatVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMMultimediaFormat.mfNone;

            GEDCOMMultimediaFormat result;
            str = str.Trim().ToLowerInvariant();
            
            if (str == "bmp") {
                result = GEDCOMMultimediaFormat.mfBMP;
            } else if (str == "gif") {
                result = GEDCOMMultimediaFormat.mfGIF;
            } else if (str == "jpg" || str == "jpeg") {
                result = GEDCOMMultimediaFormat.mfJPG;
            } else if (str == "ole") {
                result = GEDCOMMultimediaFormat.mfOLE;
            } else if (str == "pcx") {
                result = GEDCOMMultimediaFormat.mfPCX;
            } else if (str == "tif" || str == "tiff") {
                result = GEDCOMMultimediaFormat.mfTIF;
            } else if (str == "wav") {
                result = GEDCOMMultimediaFormat.mfWAV;
            } else if (str == "txt") {
                result = GEDCOMMultimediaFormat.mfTXT;
            } else if (str == "rtf") {
                result = GEDCOMMultimediaFormat.mfRTF;
            } else if (str == "avi") {
                result = GEDCOMMultimediaFormat.mfAVI;
            } else if (str == "tga") {
                result = GEDCOMMultimediaFormat.mfTGA;
            } else if (str == "png") {
                result = GEDCOMMultimediaFormat.mfPNG;
            } else if (str == "mpg" || str == "mpeg") {
                result = GEDCOMMultimediaFormat.mfMPG;
            } else if (str == "htm" || str == "html") {
                result = GEDCOMMultimediaFormat.mfHTM;
            } else if (str == "raw") {
                result = GEDCOMMultimediaFormat.mfRAW;
            } else if (str == "mp3") {
                result = GEDCOMMultimediaFormat.mfMP3;
            } else if (str == "wma") {
                result = GEDCOMMultimediaFormat.mfWMA;
            } else if (str == "psd") {
                result = GEDCOMMultimediaFormat.mfPSD;
            } else if (str == "pdf") {
                result = GEDCOMMultimediaFormat.mfPDF;
            } else if (str == "mp4") {
                result = GEDCOMMultimediaFormat.mfMP4;
            } else if (str == "ogv") {
                result = GEDCOMMultimediaFormat.mfOGV;
            } else if (str == "mka") {
                result = GEDCOMMultimediaFormat.mfMKA;
            } else if (str == "wmv") {
                result = GEDCOMMultimediaFormat.mfWMV;
            } else if (str == "mkv") {
                result = GEDCOMMultimediaFormat.mfMKV;
            } else if (str == "mov") {
                result = GEDCOMMultimediaFormat.mfMOV;
            } else {
                result = GEDCOMMultimediaFormat.mfUnknown;
            }
            return result;
        }

        public static string GetMultimediaFormatStr(GEDCOMMultimediaFormat value)
        {
            string s;
            switch (value) {
                case GEDCOMMultimediaFormat.mfBMP:
                    s = "bmp";
                    break;
                case GEDCOMMultimediaFormat.mfGIF:
                    s = "gif";
                    break;
                case GEDCOMMultimediaFormat.mfJPG:
                    s = "jpg";
                    break;
                case GEDCOMMultimediaFormat.mfOLE:
                    s = "ole";
                    break;
                case GEDCOMMultimediaFormat.mfPCX:
                    s = "pcx";
                    break;
                case GEDCOMMultimediaFormat.mfTIF:
                    s = "tif";
                    break;
                case GEDCOMMultimediaFormat.mfWAV:
                    s = "wav";
                    break;
                case GEDCOMMultimediaFormat.mfTXT:
                    s = "txt";
                    break;
                case GEDCOMMultimediaFormat.mfRTF:
                    s = "rtf";
                    break;
                case GEDCOMMultimediaFormat.mfAVI:
                    s = "avi";
                    break;
                case GEDCOMMultimediaFormat.mfTGA:
                    s = "tga";
                    break;
                case GEDCOMMultimediaFormat.mfPNG:
                    s = "png";
                    break;
                case GEDCOMMultimediaFormat.mfMPG:
                    s = "mpg";
                    break;
                case GEDCOMMultimediaFormat.mfHTM:
                    s = "htm";
                    break;
                case GEDCOMMultimediaFormat.mfRAW:
                    s = "raw";
                    break;
                case GEDCOMMultimediaFormat.mfMP3:
                    s = "mp3";
                    break;
                case GEDCOMMultimediaFormat.mfWMA:
                    s = "wma";
                    break;
                case GEDCOMMultimediaFormat.mfPSD:
                    s = "psd";
                    break;
                case GEDCOMMultimediaFormat.mfPDF:
                    s = "pdf";
                    break;
                case GEDCOMMultimediaFormat.mfMP4:
                    s = "mp4";
                    break;
                case GEDCOMMultimediaFormat.mfOGV:
                    s = "ogv";
                    break;
                case GEDCOMMultimediaFormat.mfMKA:
                    s = "mka";
                    break;
                case GEDCOMMultimediaFormat.mfWMV:
                    s = "wmv";
                    break;
                case GEDCOMMultimediaFormat.mfMKV:
                    s = "mkv";
                    break;
                case GEDCOMMultimediaFormat.mfMOV:
                    s = "mov";
                    break;
                default:
                    s = "";
                    break;
            }
            return s;
        }


        public static string[] MediaTypes = new string[] {
            "", "audio", "book", "card", "electronic", "fiche", "film", "magazine",
            "manuscript", "map", "newspaper", "photo", "tombstone", "video", "z" };

        public static GEDCOMMediaType GetMediaTypeVal(string str)
        {
            return Str2Enum(str, MediaTypes, GEDCOMMediaType.mtUnknown);
        }

        public static string GetMediaTypeStr(GEDCOMMediaType value)
        {
            return GEDCOMUtils.Enum2Str(value, MediaTypes);
        }


        public static readonly string[] LangNames = new string[] {
            "",
            "Afrikaans", "Akkadian", "Albanian", "Amharic", "Ancient Greek", "Anglo-Saxon", "Arabic", "Armenian", "Assamese",
            "Belorusian", "Bengali", "Braj", "Bulgarian", "Burmese",
            "Cantonese", "Catalan", "Catalan_Spn", "Church-Slavic", "Czech",
            "Danish", "Dogri", "Dutch",
            "Eblaite", "English", "Esperanto", "Estonian",
            "Faroese", "Finnish", "French",
            "Georgian", "German", "Greek", "Gujarati",
            "Hattic", "Hawaiian", "Hebrew", "Hindi", "Hittite", "Hungarian", "Hurrian",
            "Icelandic", "Indonesian", "Italian",
            "Japanese",
            "Kannada", "Kazakh", "Khmer", "Konkani", "Korean",
            "Lahnda", "Lao", "Latin", "Latvian", "Lithuanian", "Luwian", 
            "Macedonian", "Maithili", "Malayalam", "Mandrin", "Manipuri", "Marathi", "Mewari", "Mitanni-Aryan", 
            "Navaho", "Nepali", "Norwegian",
            "Oriya", 
            "Pahari", "Palaic", "Pali", "Panjabi", "Persian", "Polish", "Portuguese", "Prakrit", "Pusto",
            "Rajasthani", "Romanian", "Russian", 
            "Sanskrit", "Serb", "Serbo_Croa", "Slovak", "Slovene", "Spanish", "Sumerian", "Swedish",
            "Tagalog", "Tamil", "Telugu", "Thai", "Tibetan", "Turkish",
            "Ukrainian", "Urdu",
            "Vietnamese", "Wendic",
            "Yiddish",
        };

        public static GEDCOMLanguageID GetLanguageVal(string str)
        {
            return Str2Enum(str, LangNames, GEDCOMLanguageID.Unknown, false);
        }

        public static string GetLanguageStr(GEDCOMLanguageID value)
        {
            return GEDCOMUtils.Enum2Str(value, LangNames);
        }


        public static string[] NameTypes = new string[] {
            "", "aka", "birth", "immigrant", "maiden", "married" };

        public static GEDCOMNameType GetNameTypeVal(string str)
        {
            return Str2Enum(str, NameTypes, GEDCOMNameType.ntNone);
        }

        public static string GetNameTypeStr(GEDCOMNameType value)
        {
            return GEDCOMUtils.Enum2Str(value, NameTypes);
        }


        public static GDMResearchStatus GetStatusVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GDMResearchStatus.rsDefined;

            GDMResearchStatus result;
            str = str.Trim().ToLowerInvariant();
            
            if (str == "inprogress") {
                result = GDMResearchStatus.rsInProgress;
            } else if (str == "onhold") {
                result = GDMResearchStatus.rsOnHold;
            } else if (str == "problems") {
                result = GDMResearchStatus.rsProblems;
            } else if (str == "completed") {
                result = GDMResearchStatus.rsCompleted;
            } else if (str == "withdrawn") {
                result = GDMResearchStatus.rsWithdrawn;
            } else {
                result = GDMResearchStatus.rsDefined;
            }
            return result;
        }

        public static string GetStatusStr(GDMResearchStatus value)
        {
            string s = "";
            switch (value) {
                case GDMResearchStatus.rsDefined:
                    s = "defined";
                    break;
                case GDMResearchStatus.rsInProgress:
                    s = "inprogress";
                    break;
                case GDMResearchStatus.rsOnHold:
                    s = "onhold";
                    break;
                case GDMResearchStatus.rsProblems:
                    s = "problems";
                    break;
                case GDMResearchStatus.rsCompleted:
                    s = "completed";
                    break;
                case GDMResearchStatus.rsWithdrawn:
                    s = "withdrawn";
                    break;
            }
            return s;
        }


        public static string[] SpouseSealingDateStatuses = new string[] {
            "", "CANCELED", "COMPLETED", "DNS", "DNS/CAN", "EXCLUDED", "PRE-1970", "SUBMITTED", "UNCLEARED" };

        public static GDMSpouseSealingDateStatus GetSpouseSealingDateStatusVal(string str)
        {
            return Str2Enum(str, SpouseSealingDateStatuses, GDMSpouseSealingDateStatus.sdsNone, false);
        }

        public static string GetSpouseSealingDateStatusStr(GDMSpouseSealingDateStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, SpouseSealingDateStatuses);
        }


        public static GEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string su)
        {
            if (string.IsNullOrEmpty(su)) return GEDCOMOrdinanceProcessFlag.opNone;

            GEDCOMOrdinanceProcessFlag result;
            su = NormalizeUp(su);
            
            if (su == "YES") {
                result = GEDCOMOrdinanceProcessFlag.opYes;
            } else if (su == "NO") {
                result = GEDCOMOrdinanceProcessFlag.opNo;
            } else {
                result = GEDCOMOrdinanceProcessFlag.opNone;
            }
            return result;
        }

        public static string GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag value)
        {
            string str = "";
            switch (value) {
                case GEDCOMOrdinanceProcessFlag.opNone:
                    str = "";
                    break;
                case GEDCOMOrdinanceProcessFlag.opYes:
                    str = "yes";
                    break;
                case GEDCOMOrdinanceProcessFlag.opNo:
                    str = "no";
                    break;
            }
            return str;
        }


        public static string GetPriorityStr(GDMResearchPriority value)
        {
            string str = "";
            switch (value) {
                case GDMResearchPriority.rpNone:
                    str = "";
                    break;
                case GDMResearchPriority.rpLow:
                    str = "low";
                    break;
                case GDMResearchPriority.rpNormal:
                    str = "normal";
                    break;
                case GDMResearchPriority.rpHigh:
                    str = "high";
                    break;
                case GDMResearchPriority.rpTop:
                    str = "top";
                    break;
            }
            return str;
        }

        public static GDMResearchPriority GetPriorityVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GDMResearchPriority.rpNone;

            string su = str.Trim().ToLowerInvariant();
            GDMResearchPriority result;

            if (su == "low") {
                result = GDMResearchPriority.rpLow;
            } else if (su == "normal") {
                result = GDMResearchPriority.rpNormal;
            } else if (su == "high") {
                result = GDMResearchPriority.rpHigh;
            } else if (su == "top") {
                result = GDMResearchPriority.rpTop;
            } else {
                result = GDMResearchPriority.rpNone;
            }
            
            return result;
        }


        public static string GetCharacterSetStr(GEDCOMCharacterSet value)
        {
            string str = "";
            switch (value) {
                case GEDCOMCharacterSet.csASCII:
                    str = "ASCII";
                    break;
                case GEDCOMCharacterSet.csANSEL:
                    str = "ANSEL";
                    break;
                case GEDCOMCharacterSet.csUNICODE:
                    str = "UNICODE";
                    break;
                case GEDCOMCharacterSet.csUTF8:
                    str = "UTF-8";
                    break;
            }
            return str;
        }

        public static GEDCOMCharacterSet GetCharacterSetVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMCharacterSet.csASCII;

            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(str);
            GEDCOMCharacterSet result;

            if (su == "ASCII" || su == "ANSI" || su == "IBMPC") {
                result = GEDCOMCharacterSet.csASCII;
            } else if (su == "ANSEL") {
                result = GEDCOMCharacterSet.csANSEL;
            } else if (su == "UNICODE") {
                result = GEDCOMCharacterSet.csUNICODE;
            } else if (su == "UTF8" || su == "UTF-8") {
                result = GEDCOMCharacterSet.csUTF8;
            } else {
                result = GEDCOMCharacterSet.csASCII;
            }
            
            return result;
        }


        public static GEDCOMSex GetSexVal(string str)
        {
            str = NormalizeUp(str);
            if (str.Length != 1) return GEDCOMSex.svNone;

            char sl = str[0];

            GEDCOMSex result;
            if (sl == 'M') {
                result = GEDCOMSex.svMale;
            } else if (sl == 'F') {
                result = GEDCOMSex.svFemale;
            } else if (sl == 'U') {
                result = GEDCOMSex.svUndetermined;
            } else {
                result = GEDCOMSex.svNone;
            }
            return result;
        }

        public static string GetSexStr(GEDCOMSex value)
        {
            string str;
            
            switch (value) {
                case GEDCOMSex.svMale:
                    str = "M";
                    break;
                case GEDCOMSex.svFemale:
                    str = "F";
                    break;
                case GEDCOMSex.svUndetermined:
                    str = "U";
                    break;
                default:
                    str = "";
                    break;
            }
            
            return str;
        }


        public static string[] BaptismDateStatuses = new string[] {
            "", "CHILD", "COMPLETED", "EXCLUDED", "PRE-1970", "STILLBORN", "SUBMITTED", "UNCLEARED" };

        public static GEDCOMBaptismDateStatus GetBaptismDateStatusVal(string str)
        {
            return Str2Enum(str, BaptismDateStatuses, GEDCOMBaptismDateStatus.bdsNone, false);
        }

        public static string GetBaptismDateStatusStr(GEDCOMBaptismDateStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, BaptismDateStatuses);
        }


        public static string[] EndowmentDateStatuses = new string[] {
            "", "CHILD", "COMPLETED", "EXCLUDED", "INFANT", "PRE-1970", "STILLBORN", "SUBMITTED", "UNCLEARED" };

        public static GEDCOMEndowmentDateStatus GetEndowmentDateStatusVal(string str)
        {
            return Str2Enum(str, EndowmentDateStatuses, GEDCOMEndowmentDateStatus.edsNone, false);
        }

        public static string GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, EndowmentDateStatuses);
        }


        public static string[] ChildSealingDateStatuses = new string[] {
            "", "BIC", "EXCLUDED", "PRE-1970", "STILLBORN", "SUBMITTED", "UNCLEARED" };

        public static GEDCOMChildSealingDateStatus GetChildSealingDateStatusVal(string str)
        {
            return Str2Enum(str, ChildSealingDateStatuses, GEDCOMChildSealingDateStatus.cdsNone, false);
        }

        public static string GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, ChildSealingDateStatuses);
        }


        public static string[] MarriageStatuses = new string[] {
            "", "married", "marrnotreg", "notmarr" };

        public static GDMMarriageStatus GetMarriageStatusVal(string str)
        {
            return Str2Enum(str, MarriageStatuses, GDMMarriageStatus.Unknown);
        }

        public static string GetMarriageStatusStr(GDMMarriageStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, MarriageStatuses);
        }

        #endregion

        public static string GetSignByRecord(GDMRecord record)
        {
            string result = string.Empty;
            if (record == null) return result;

            switch (record.RecordType)
            {
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

        /// <summary>
        /// Strange values were found, possibly from other genealogical programs.
        /// </summary>
        /// <param name="value">Input value of CertaintyAssessment</param>
        /// <returns>Checked value</returns>
        public static int GetValidCertaintyAssessment(int value)
        {
            return (value >= 0 && value <= 3) ? value : 0;
        }

        public static string EncodeUID(byte[] binaryKey)
        {
            var invNFI = GEDCOMUtils.InvariantNumberFormatInfo;

            StringBuilder result = new StringBuilder(36);
            byte checkA = 0;
            byte checkB = 0;

            int num = binaryKey.Length;
            for (int i = 0; i < num; i++) {
                byte val = binaryKey[i];
                checkA = unchecked((byte)(checkA + (uint)val));
                checkB = unchecked((byte)(checkB + (uint)checkA));
                result.Append(val.ToString("X2", invNFI));
            }

            result.Append(checkA.ToString("X2", invNFI));
            result.Append(checkB.ToString("X2", invNFI));

            return result.ToString();
        }

        public static string CreateUID()
        {
            byte[] binary = Guid.NewGuid().ToByteArray();
            string result = GEDCOMUtils.EncodeUID(binary);
            return result;
        }

        public static int BinarySearch(EnumTuple[] array, string key, Comparison<string> comparer)
        {
            int i = 0;
            int num = array.Length - 1;
            while (i <= num) {
                int num2 = i + (num - i >> 1);

                EnumTuple ekv = array[num2];
                int num3 = comparer(ekv.Key, key);

                if (num3 == 0) {
                    return ekv.Value;
                }
                if (num3 < 0) {
                    i = num2 + 1;
                }
                else {
                    num = num2 - 1;
                }
            }
            return ~i;
        }

        /// <summary>
        /// Decode the blob string (for multimedia embedded in the GEDCOM file)
        /// </summary>
        public static MemoryStream DecodeBlob(string blob)
        {
            const string validChars = "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

            var stream = new MemoryStream();

            int i = 0;
            int length = blob.Length - 3;
            while (i < length) {
                byte c1, c2, c3, c4, b1, b2, b3;

                int ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c1 = (byte)ix;
                } else {
                    throw new GDMException("DecodeBlob");
                }

                ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c2 = (byte)ix;
                } else {
                    throw new GDMException("DecodeBlob");
                }

                ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c3 = (byte)ix;
                } else {
                    throw new GDMException("DecodeBlob");
                }

                ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c4 = (byte)ix;
                } else {
                    throw new GDMException("DecodeBlob");
                }

                // The following decodes Family Historian blobs.
                // This might differ from the GEDCOM 5.5 spec in terms of bit ordering.
                /*b1 = (byte)((c2 & 0x03) << 6 | (c1 & 0x3f));
                b2 = (byte)((c3 & 0x0f) << 4 | (c2 & 0x3c) >> 2);
                b3 = (byte)((c4 & 0x3f) << 2 | (c3 & 0x30) >> 4);*/
                uint group = (uint)((c4 & 0x3f) | (c3 & 0x3f) << 6 | (c2 & 0x3f) << 12 | (c1 & 0x3f) << 18);
                b3 = (byte)(group & 0xff);
                b2 = (byte)((group >> 8) & 0xff);
                b1 = (byte)((group >> 16) & 0xff);

                stream.WriteByte(b1);
                stream.WriteByte(b2);
                stream.WriteByte(b3);
            }

            return stream;
        }
    }
}
