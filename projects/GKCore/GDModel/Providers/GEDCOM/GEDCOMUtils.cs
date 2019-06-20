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


    public enum GeoCoord { Lati, Long }


    public enum GDMDateFormat
    {
        dfGEDCOMStd,
        dfSystem
    }


    public enum GDMDateType
    {
        SIMP, ABT, AFT, BEF, BET, CAL, EST, FROM, INT, TO
    }


    /// <summary>
    /// 
    /// </summary>
    public static class GEDCOMUtils
    {
        public static readonly string[] GEDCOMDateTypes;

        public static readonly TextInfo InvariantTextInfo = CultureInfo.InvariantCulture.TextInfo;
        public static readonly NumberFormatInfo InvariantNumberFormatInfo = NumberFormatInfo.InvariantInfo;


        static GEDCOMUtils()
        {
            GEDCOMDateTypes = new string[] { "", "ABT", "AFT", "BEF", "BET", "CAL", "EST", "FROM", "INT", "TO" };
        }


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

        /// <summary>
        /// It is agreed to follow the requirements of the GEDCOM standard:
        /// PLACE_LATITUDE must be represented as [Ngg.nnnnnn (+) | Sgg.nnnnnn (-)]
        /// PLACE_LONGITUDE must be represented as [Wgg.nnnnnn (-) | Egg.nnnnnn (+)]
        /// </summary>
        public static double GetGeoCoord(string value, GeoCoord coordType)
        {
            if (string.IsNullOrEmpty(value)) {
                return 0.0;
            }

            int sign = 1;
            char firstChr = value[0];
            if ("NSWE".IndexOf(firstChr) >= 0) {
                switch (firstChr) {
                    case 'N':
                    case 'E':
                        sign = +1;
                        break;

                    case 'S':
                    case 'W':
                        sign = -1;
                        break;
                }
                value = value.Substring(1);
            }

            double result = ConvertHelper.ParseFloat(value, 0.0);
            return result * sign;
        }

        public static string GetFloatStr(double value)
        {
            NumberFormatInfo nfi = new NumberFormatInfo();
            nfi.NumberDecimalSeparator = ".";
            return value.ToString("0.000000", nfi);
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
            byte hour;
            byte minutes;
            byte seconds;
            short fraction;

            strValue = ParseTime(strValue, out hour, out minutes, out seconds, out fraction);
            time.SetRawData(hour, minutes, seconds, fraction);
            return strValue;
        }

        // Time format: hour:minutes:seconds.fraction
        public static string ParseTime(string strValue, out byte hour, out byte minutes, out byte seconds, out short fraction)
        {
            hour = 0;
            minutes = 0;
            seconds = 0;
            fraction = 0;

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

            //time.SetRawData(hour, minutes, seconds, fraction);

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
                idx = Algorithms.BinarySearch(GEDCOMDateTypes, su, string.CompareOrdinal);
            }
            var dateType = (idx < 0) ? GDMDateType.SIMP : (GDMDateType)idx;

            string result;
            GDMCustomDate date;
            switch (dateType) {
                case GDMDateType.AFT:
                case GDMDateType.BEF:
                case GDMDateType.BET:
                    date = new GDMDateRange(dateValue);
                    result = GEDCOMUtils.ParseRangeDate(owner, (GDMDateRange)date, strTok);
                    break;
                case GDMDateType.INT:
                    date = new GDMDateInterpreted(dateValue);
                    result = GEDCOMUtils.ParseIntDate(owner, (GDMDateInterpreted)date, strTok);
                    break;
                case GDMDateType.FROM:
                case GDMDateType.TO:
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
            GDMApproximated approximated;
            GDMCalendar calendar;
            short year;
            bool yearBC;
            string yearModifier;
            byte month;
            byte day;

            string result = ParseDate(owner, strTok, out approximated, out calendar, out year, out yearBC, 
                                      out yearModifier, out month, out day);

            date.SetRawData(approximated, calendar, year, yearBC, yearModifier, month, day);

            return result;
        }

        public static string ParseDate(GDMTree owner, GEDCOMParser strTok, out GDMApproximated approximated,
                                       out GDMCalendar calendar, out short year, out bool yearBC,
                                       out string yearModifier, out byte month, out byte day)
        {
            approximated = GDMApproximated.daExact;
            calendar = GDMCalendar.dcGregorian;
            year = GDMDate.UNKNOWN_YEAR;
            yearBC = false;
            yearModifier = string.Empty;
            month = 0;
            day = 0;
            GDMDateFormat dateFormat = GDMDateFormat.dfGEDCOMStd;

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
                    approximated = (GDMApproximated)idx;
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
                    calendar = (GDMCalendar)idx;
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
                dateFormat = GDMDateFormat.dfGEDCOMStd;
                token = strTok.Next();
            } else if (token == GEDCOMToken.Symbol && strTok.GetSymbol() == '.') {
                dateFormat = GDMDateFormat.dfSystem;
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
            } else if (dateFormat == GDMDateFormat.dfSystem && token == GEDCOMToken.Number) {
                month = (byte)strTok.GetNumber();

                token = strTok.Next();
            }

            // extract delimiter
            if (dateFormat == GDMDateFormat.dfSystem) {
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

            //date.SetRawData(approximated, calendar, year, yearBC, yearModifier, month, day, dateFormat);
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

        public static string ParseDate(GDMTree owner, string strValue, out DateTime date)
        {
            GDMApproximated approximated;
            GDMCalendar calendar;
            short year;
            bool yearBC;
            string yearModifier;
            byte month;
            byte day;

            var strTok = new GEDCOMParser(strValue, false);
            string result = ParseDate(owner, strTok, out approximated, out calendar, out year, out yearBC,
                                      out yearModifier, out month, out day);

            date = new DateTime(year, month, day);

            return result;
        }

        public static string GetDateStr(DateTime date)
        {
            string result;
            result = string.Format("{0:00} {1} {2:0000}", new object[] { date.Day, GDMCustomDate.GEDCOMMonthArray[date.Month-1], date.Year });
            return result;
        }

        public static string GetTimeStr(TimeSpan time)
        {
            byte hour = (byte)time.Hours;
            byte minutes = (byte)time.Minutes;
            byte seconds = (byte)time.Seconds;
            short fraction = (short)Math.Truncate(time.Milliseconds / 100.0);

            string result;
            if (hour == 0 && minutes == 0 && seconds == 0) {
                result = string.Empty;
            } else {
                result = string.Format("{0:00}:{1:00}:{2:00}", new object[] { hour, minutes, seconds });
                if (fraction > 0) {
                    result = result + "." + fraction.ToString();
                }
            }
            return result;
        }

        public static string ParseTime(string strValue, out TimeSpan time)
        {
            byte hour;
            byte minutes;
            byte seconds;
            short fraction;

            strValue = ParseTime(strValue, out hour, out minutes, out seconds, out fraction);
            time = new TimeSpan(0, hour, minutes, seconds, (int)(100u * fraction));
            return strValue;
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


        public static GDMRestriction GetRestrictionVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GDMRestriction.rnNone;

            GDMRestriction res;
            str = str.Trim().ToLowerInvariant();
            
            if (str == "confidential") {
                res = GDMRestriction.rnConfidential;
            } else if (str == "locked") {
                res = GDMRestriction.rnLocked;
            } else if (str == "privacy") {
                res = GDMRestriction.rnPrivacy;
            } else {
                res = GDMRestriction.rnNone;
            }
            return res;
        }

        public static string GetRestrictionStr(GDMRestriction value)
        {
            string s;

            switch (value) {
                case GDMRestriction.rnConfidential:
                    s = "confidential";
                    break;

                case GDMRestriction.rnLocked:
                    s = "locked";
                    break;

                case GDMRestriction.rnPrivacy:
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


        public static GDMMultimediaFormat GetMultimediaFormatVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GDMMultimediaFormat.mfNone;

            GDMMultimediaFormat result;
            str = str.Trim().ToLowerInvariant();
            
            if (str == "bmp") {
                result = GDMMultimediaFormat.mfBMP;
            } else if (str == "gif") {
                result = GDMMultimediaFormat.mfGIF;
            } else if (str == "jpg" || str == "jpeg") {
                result = GDMMultimediaFormat.mfJPG;
            } else if (str == "ole") {
                result = GDMMultimediaFormat.mfOLE;
            } else if (str == "pcx") {
                result = GDMMultimediaFormat.mfPCX;
            } else if (str == "tif" || str == "tiff") {
                result = GDMMultimediaFormat.mfTIF;
            } else if (str == "wav") {
                result = GDMMultimediaFormat.mfWAV;
            } else if (str == "txt") {
                result = GDMMultimediaFormat.mfTXT;
            } else if (str == "rtf") {
                result = GDMMultimediaFormat.mfRTF;
            } else if (str == "avi") {
                result = GDMMultimediaFormat.mfAVI;
            } else if (str == "tga") {
                result = GDMMultimediaFormat.mfTGA;
            } else if (str == "png") {
                result = GDMMultimediaFormat.mfPNG;
            } else if (str == "mpg" || str == "mpeg") {
                result = GDMMultimediaFormat.mfMPG;
            } else if (str == "htm" || str == "html") {
                result = GDMMultimediaFormat.mfHTM;
            } else if (str == "raw") {
                result = GDMMultimediaFormat.mfRAW;
            } else if (str == "mp3") {
                result = GDMMultimediaFormat.mfMP3;
            } else if (str == "wma") {
                result = GDMMultimediaFormat.mfWMA;
            } else if (str == "psd") {
                result = GDMMultimediaFormat.mfPSD;
            } else if (str == "pdf") {
                result = GDMMultimediaFormat.mfPDF;
            } else if (str == "mp4") {
                result = GDMMultimediaFormat.mfMP4;
            } else if (str == "ogv") {
                result = GDMMultimediaFormat.mfOGV;
            } else if (str == "mka") {
                result = GDMMultimediaFormat.mfMKA;
            } else if (str == "wmv") {
                result = GDMMultimediaFormat.mfWMV;
            } else if (str == "mkv") {
                result = GDMMultimediaFormat.mfMKV;
            } else if (str == "mov") {
                result = GDMMultimediaFormat.mfMOV;
            } else {
                result = GDMMultimediaFormat.mfUnknown;
            }
            return result;
        }

        public static string GetMultimediaFormatStr(GDMMultimediaFormat value)
        {
            string s;
            switch (value) {
                case GDMMultimediaFormat.mfBMP:
                    s = "bmp";
                    break;
                case GDMMultimediaFormat.mfGIF:
                    s = "gif";
                    break;
                case GDMMultimediaFormat.mfJPG:
                    s = "jpg";
                    break;
                case GDMMultimediaFormat.mfOLE:
                    s = "ole";
                    break;
                case GDMMultimediaFormat.mfPCX:
                    s = "pcx";
                    break;
                case GDMMultimediaFormat.mfTIF:
                    s = "tif";
                    break;
                case GDMMultimediaFormat.mfWAV:
                    s = "wav";
                    break;
                case GDMMultimediaFormat.mfTXT:
                    s = "txt";
                    break;
                case GDMMultimediaFormat.mfRTF:
                    s = "rtf";
                    break;
                case GDMMultimediaFormat.mfAVI:
                    s = "avi";
                    break;
                case GDMMultimediaFormat.mfTGA:
                    s = "tga";
                    break;
                case GDMMultimediaFormat.mfPNG:
                    s = "png";
                    break;
                case GDMMultimediaFormat.mfMPG:
                    s = "mpg";
                    break;
                case GDMMultimediaFormat.mfHTM:
                    s = "htm";
                    break;
                case GDMMultimediaFormat.mfRAW:
                    s = "raw";
                    break;
                case GDMMultimediaFormat.mfMP3:
                    s = "mp3";
                    break;
                case GDMMultimediaFormat.mfWMA:
                    s = "wma";
                    break;
                case GDMMultimediaFormat.mfPSD:
                    s = "psd";
                    break;
                case GDMMultimediaFormat.mfPDF:
                    s = "pdf";
                    break;
                case GDMMultimediaFormat.mfMP4:
                    s = "mp4";
                    break;
                case GDMMultimediaFormat.mfOGV:
                    s = "ogv";
                    break;
                case GDMMultimediaFormat.mfMKA:
                    s = "mka";
                    break;
                case GDMMultimediaFormat.mfWMV:
                    s = "wmv";
                    break;
                case GDMMultimediaFormat.mfMKV:
                    s = "mkv";
                    break;
                case GDMMultimediaFormat.mfMOV:
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

        public static GDMMediaType GetMediaTypeVal(string str)
        {
            return Str2Enum(str, MediaTypes, GDMMediaType.mtUnknown);
        }

        public static string GetMediaTypeStr(GDMMediaType value)
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

        public static GDMLanguageID GetLanguageVal(string str)
        {
            return Str2Enum(str, LangNames, GDMLanguageID.Unknown, false);
        }

        public static string GetLanguageStr(GDMLanguageID value)
        {
            return GEDCOMUtils.Enum2Str(value, LangNames);
        }


        public static string[] NameTypes = new string[] {
            "", "aka", "birth", "immigrant", "maiden", "married" };

        public static GDMNameType GetNameTypeVal(string str)
        {
            return Str2Enum(str, NameTypes, GDMNameType.ntNone);
        }

        public static string GetNameTypeStr(GDMNameType value)
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


        public static int GetIntVal(string str, int defValue = 0)
        {
            int result = (string.IsNullOrEmpty(str) ? defValue : ConvertHelper.ParseInt(str, defValue));
            return result;
        }

        public static string GetIntStr(int value, int emptyValue = -1)
        {
            string result = (value == emptyValue) ? string.Empty : value.ToString();
            return result;
        }


        public static bool GetBoolVal(string str, bool defValue = false)
        {
            bool result = string.IsNullOrEmpty(str) ? defValue : ((str == "Y") ? true : false);
            return result;
        }

        public static string GetBoolStr(bool value)
        {
            return (value) ? "Y" : "N";
        }


        public static GDMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string su)
        {
            if (string.IsNullOrEmpty(su)) return GDMOrdinanceProcessFlag.opNone;

            GDMOrdinanceProcessFlag result;
            su = NormalizeUp(su);
            
            if (su == "YES") {
                result = GDMOrdinanceProcessFlag.opYes;
            } else if (su == "NO") {
                result = GDMOrdinanceProcessFlag.opNo;
            } else {
                result = GDMOrdinanceProcessFlag.opNone;
            }
            return result;
        }

        public static string GetOrdinanceProcessFlagStr(GDMOrdinanceProcessFlag value)
        {
            string str = "";
            switch (value) {
                case GDMOrdinanceProcessFlag.opNone:
                    str = "";
                    break;
                case GDMOrdinanceProcessFlag.opYes:
                    str = "yes";
                    break;
                case GDMOrdinanceProcessFlag.opNo:
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


        public static GDMSex GetSexVal(string str)
        {
            str = NormalizeUp(str);
            if (str.Length != 1) return GDMSex.svUnknown;

            char sl = str[0];

            GDMSex result;
            if (sl == 'M') {
                result = GDMSex.svMale;
            } else if (sl == 'F') {
                result = GDMSex.svFemale;
            } else if (sl == 'X') {
                result = GDMSex.svIntersex;
            } else {
                result = GDMSex.svUnknown;
            }
            return result;
        }

        public static string GetSexStr(GDMSex value)
        {
            string str;
            switch (value) {
                case GDMSex.svMale:
                    str = "M";
                    break;
                case GDMSex.svFemale:
                    str = "F";
                    break;
                case GDMSex.svIntersex:
                    str = "X";
                    break;
                default:
                    str = "U";
                    break;
            }
            return str;
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

        #region Aux functions

        public static string GetSignByRecord(GDMRecord record)
        {
            string result = string.Empty;
            if (record == null) return result;

            switch (record.RecordType)
            {
                case GDMRecordType.rtIndividual:
                    result = "I"; // Std, p24
                    break;
                case GDMRecordType.rtFamily:
                    result = "F"; // Std, p24
                    break;
                case GDMRecordType.rtNote:
                    result = "N";
                    break;
                case GDMRecordType.rtMultimedia:
                    result = "O";
                    break;
                case GDMRecordType.rtSource:
                    result = "S"; // Std, p24
                    break;
                case GDMRecordType.rtRepository:
                    result = "R"; // Std, p24
                    break;
                case GDMRecordType.rtGroup:
                    result = "G";
                    break;
                case GDMRecordType.rtResearch:
                    result = "RS";
                    break;
                case GDMRecordType.rtTask:
                    result = "TK";
                    break;
                case GDMRecordType.rtCommunication:
                    result = "CM";
                    break;
                case GDMRecordType.rtLocation:
                    result = "L";
                    break;
                case GDMRecordType.rtSubmission:
                    result = "????"; // FIXME: to standard
                    break;
                case GDMRecordType.rtSubmitter:
                    result = "SUB";
                    break;
            }

            return result;
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

        #endregion

        #region Event type detection

        public static string[] IndiEvents = new string[] {
            GEDCOMTagType.ADOP, GEDCOMTagType.BAPM, GEDCOMTagType.BARM, GEDCOMTagType.BASM, GEDCOMTagType.BIRT,
            GEDCOMTagType.BLES, GEDCOMTagType.BURI, GEDCOMTagType.CENS, GEDCOMTagType.CHR, GEDCOMTagType.CHRA,
            GEDCOMTagType.CONF, GEDCOMTagType.CREM, GEDCOMTagType.DEAT, GEDCOMTagType.EMIG, GEDCOMTagType.EVEN,
            GEDCOMTagType.FCOM, GEDCOMTagType.GRAD, GEDCOMTagType.IMMI, GEDCOMTagType.NATU, GEDCOMTagType.ORDN,
            GEDCOMTagType.PROB, GEDCOMTagType.RETI, GEDCOMTagType.WILL, 
        };

        public static bool IsIndiEvent(string tagName)
        {
            int idx = Algorithms.BinarySearch<string>(IndiEvents, tagName, string.CompareOrdinal);
            return idx >= 0;
        }


        public static string[] IndiAttrs = new string[] {
            GEDCOMTagType.CAST, GEDCOMTagType.DSCR, GEDCOMTagType.EDUC, GEDCOMTagType.FACT, GEDCOMTagType.IDNO,
            GEDCOMTagType.NATI, GEDCOMTagType.NCHI, GEDCOMTagType.NMR, GEDCOMTagType.OCCU, GEDCOMTagType.PROP,
            GEDCOMTagType.RELI, GEDCOMTagType.RESI, GEDCOMTagType.SSN, GEDCOMTagType.TITL,

            GEDCOMTagType._AWARD, GEDCOMTagType._BGRO, GEDCOMTagType._EYES, GEDCOMTagType._HAIR, GEDCOMTagType._HOBBY,
            GEDCOMTagType._MDNA, GEDCOMTagType._MILI, GEDCOMTagType._MILI_DIS, GEDCOMTagType._MILI_IND,
            GEDCOMTagType._MILI_RANK, GEDCOMTagType._TRAVEL, GEDCOMTagType._YDNA,
        };

        public static bool IsIndiAttr(string tagName)
        {
            int idx = Algorithms.BinarySearch<string>(IndiAttrs, tagName, string.CompareOrdinal);
            return idx >= 0;
        }


        public static string[] FamEvents = new string[] {
            GEDCOMTagType.ANUL, GEDCOMTagType.CENS, GEDCOMTagType.DIV, GEDCOMTagType.DIVF, GEDCOMTagType.ENGA,
            GEDCOMTagType.EVEN, GEDCOMTagType.MARB, GEDCOMTagType.MARC, GEDCOMTagType.MARL, GEDCOMTagType.MARR,
            GEDCOMTagType.MARS, GEDCOMTagType.RESI,
        };

        public static bool IsFamEvent(string tagName)
        {
            int idx = Algorithms.BinarySearch<string>(FamEvents, tagName, string.CompareOrdinal);
            return idx >= 0;
        }

        public static StringList GetTagStrings(GDMTag strTag)
        {
            StringList strings = new StringList();

            if (strTag != null) {
                if (strTag.StringValue != "") {
                    strings.Add(strTag.StringValue);
                }

                var subTags = strTag.SubTags;
                int num = subTags.Count;
                for (int i = 0; i < num; i++) {
                    GDMTag tag = subTags[i];

                    if (tag.Name == GEDCOMTagType.CONC) {
                        if (strings.Count > 0) {
                            strings[strings.Count - 1] = strings[strings.Count - 1] + tag.StringValue;
                        } else {
                            strings.Add(tag.StringValue);
                        }
                    } else {
                        if (tag.Name == GEDCOMTagType.CONT) {
                            strings.Add(tag.StringValue);
                        }
                    }
                }
            }

            return strings;
        }

        public static void SetTagStrings(GDMTag tag, StringList strings)
        {
            if (tag == null) return;

            tag.StringValue = "";
            var subTags = tag.SubTags;
            for (int i = subTags.Count - 1; i >= 0; i--) {
                string subtag = subTags[i].Name;
                if (subtag == GEDCOMTagType.CONT || subtag == GEDCOMTagType.CONC) {
                    subTags.DeleteAt(i);
                }
            }

            if (strings != null) {
                bool isRecordTag = (tag is GDMRecord);

                int num = strings.Count;
                for (int i = 0; i < num; i++) {
                    string str = strings[i];

                    int len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        GEDCOMProvider.AddBaseTag(tag, 0, GEDCOMTagType.CONT, sub);
                    }

                    while (str.Length > 0) {
                        len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                        GEDCOMProvider.AddBaseTag(tag, 0, GEDCOMTagType.CONC, str.Substring(0, len));
                        str = str.Remove(0, len);
                    }
                }
            }
        }

        #endregion
    }
}
