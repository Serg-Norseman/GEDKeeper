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

using System;
using System.Globalization;
using System.IO;
using System.Text;
using BSLib;
using GKCore;

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


    public enum GEDCOMGeoCoord
    {
        None,
        Lati,
        Long
    }


    public enum GEDCOMDateFormat
    {
        Standard,   // GEDCOM
        System      // OS
    }


    public enum GEDCOMDateType
    {
        SIMP, ABT, AFT, BEF, BET, CAL, EST, FROM, INT, TO
    }


    public static class GEDCOMExtensions
    {
        public static GEDCOMTagType GetTagType(this GDMTag tag)
        {
            return (GEDCOMTagType)tag.Id;
        }

        public static string GetTagName(this GDMTag tag)
        {
            GEDCOMTagProps tagProps = GEDCOMTagsTable.GetTagProps(tag.Id);
            return tagProps != null ? tagProps.TagName : string.Empty;
        }
    }


    /// <summary>
    /// This class contains helper methods for working with the GEDCOM data format.
    /// </summary>
    /// <remarks>
    /// This class has been heavily refactored under profiling. Any alterations must take into account the factor 
    /// of performance degradation when changing the approach, even in small things.
    /// </remarks>
    public static class GEDCOMUtils
    {
        public static readonly TextInfo InvariantTextInfo = CultureInfo.InvariantCulture.TextInfo;
        public static readonly NumberFormatInfo InvariantNumberFormatInfo = NumberFormatInfo.InvariantInfo;


        #region Parse functions

        public static unsafe string Trim(string str)
        {
            if (string.IsNullOrEmpty(str)) return string.Empty;

            int strLen = str.Length;
            fixed (char* strPtr = str) {
                int li = 0;
                int ri = strLen - 1;

                while (li < ri && strPtr[li] <= ' ') li++;
                while (ri >= li && strPtr[ri] <= ' ') ri--;
                int newLen = ri - li + 1;

                string result = (newLen == strLen) ? str : new string(strPtr, li, newLen);
                return result;
            }
        }

        public static unsafe string CleanXRef(string str)
        {
            string result = str;
            if (!string.IsNullOrEmpty(str)) {
                int strLen = str.Length;
                int sx = -1;
                fixed (char* strPtr = str) {
                    for (int i = 0; i < strLen; i++) {
                        char chr = strPtr[i];
                        if (chr == GEDCOMConsts.PointerDelimiter) {
                            if (sx == -1) {
                                sx = i;
                            } else {
                                result = new string(strPtr, sx + 1, i - 1 - sx);
                                break;
                            }
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

        public static bool IsXRef(string str)
        {
            if (string.IsNullOrEmpty(str)) {
                return false;
            } else {
                return ((str[0] == '@') && (str[str.Length - 1] == '@'));
            }
        }

        public static unsafe long GetXRefNumber(string str)
        {
            int strLen = (str == null) ? 0 : str.Length;
            if (strLen == 0)
                return -1;

            long result = 0;
            fixed (char* ch_ptr = str) {
                for (int i = 0; i < strLen; i++) {
                    char ch = ch_ptr[i];
                    if (ch >= '0' && ch <= '9') {
                        result = (result * 10 + ((int)ch - 48));
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// It is agreed to follow the requirements of the GEDCOM standard:
        /// PLACE_LATITUDE must be represented as [Ngg.nnnnnn (+) | Sgg.nnnnnn (-)]
        /// PLACE_LONGITUDE must be represented as [Wgg.nnnnnn (-) | Egg.nnnnnn (+)]
        /// </summary>
        public static double GetGeoCoord(string value, GEDCOMGeoCoord coordType)
        {
            if (string.IsNullOrEmpty(value)) {
                return 0.0;
            }

            int sign = 1;
            bool spec = false;

            char firstChr = value[0];
            switch (firstChr) {
                case 'N':
                case 'E':
                    sign = +1;
                    spec = true;
                    break;

                case 'S':
                case 'W':
                    sign = -1;
                    spec = true;
                    break;
            }

            if (spec)
                value = value.Substring(1);

            double result = ConvertHelper.ParseFloat(value, 0.0);
            return result * sign;
        }


        public static readonly NumberFormatInfo CoordNumberFormatInfo = new NumberFormatInfo() { NumberDecimalSeparator = "." };
        public static readonly string CoordFormat = "0.000000";

        public static string CoordToStr(double val, GEDCOMGeoCoord coordType = GEDCOMGeoCoord.None, bool spec = false)
        {
            if (coordType == GEDCOMGeoCoord.None || !spec)
                return val.ToString(CoordFormat, CoordNumberFormatInfo);

            string prefix = string.Empty;

            int sign = Math.Sign(val);
            if (sign != 0) {
                switch (coordType) {
                    case GEDCOMGeoCoord.Lati:
                        prefix = (sign == +1) ? "N" : "S";
                        break;

                    case GEDCOMGeoCoord.Long:
                        prefix = (sign == +1) ? "E" : "W";
                        break;
                }
                val = Math.Abs(val);
            }

            return string.Concat(prefix, val.ToString(CoordFormat, CoordNumberFormatInfo));
        }

        #endregion

        #region Special parsing routines

        // Line format: <level>_<@xref@>_<tag>_<value>
        public static int ParseTag(GEDCOMParser strTok, out int tagLevel, out string tagXRef, out string tagName, out StringSpan tagValue)
        {
            tagLevel = 0;
            tagXRef = string.Empty;
            tagName = string.Empty;
            tagValue = StringSpan.Empty;

            int result = 0;
            strTok.SkipWhitespaces();

            var token = strTok.CurrentToken; // already trimmed
            if (token == GEDCOMToken.EOL) {
                return -2;
            }
            if (token != GEDCOMToken.Number) {
                tagValue = strTok.GetFullSpan();
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
                // here XRef is a pure value without delimiters
                tagXRef = strTok.GetWord();

                // FIXME: check for errors
                //throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", str));
                //throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", str));
                result += 1;

                strTok.Next();
                strTok.SkipWhitespaces();
            }

            if (strTok.CurrentToken != GEDCOMToken.Word) {
                // syntax error
            }
            tagName = strTok.GetWord();
            result += 1;

            // GEDCOM specification (https://gedcom.io/specifications/ged551.pdf, page 86)
            // When importing values from CONT lines the reader should assume only one delimiter character following the CONT tag.
            // Assume that the rest of the leading spaces are to be a part of the value.
            bool skipOneSpace = string.Equals(tagName, GEDCOMTagName.CONT);
            token = strTok.Next(skipOneSpace);

            if (token == GEDCOMToken.Whitespace) {
                tagValue = strTok.GetRestSpan();
                result += 1;
            }

            return result;
        }

        // XRefPtr format: ...@<xref>@...
        public static unsafe string ParseXRefPointer(StringSpan str, out string xref)
        {
            xref = string.Empty;
            if (str.IsEmptyOrEnd) {
                return string.Empty;
            }

            int strBeg = str.Pos;
            int strLen = str.Length;

            fixed (char* strPtr = str.Data) {
                // skip leading whitespaces
                while (strBeg < strLen && strPtr[strBeg] == GEDCOMConsts.Delimiter) strBeg++;

                // check empty string
                if (strLen - strBeg == 0) {
                    return string.Empty;
                }

                int init = -1, fin = strBeg;
                for (int i = strBeg; i < strLen; i++) {
                    char chr = strPtr[i];
                    if (chr == GEDCOMConsts.PointerDelimiter) {
                        if (init == -1) {
                            if (i == strBeg) {
                                init = i;
                            }
                        } else {
                            fin = i;
                            xref = new string(strPtr, init + 1, fin - 1 - init);
                            fin += 1;
                            break;
                        }
                    } else if (chr == '#' && i == init + 1) {
                        break;
                    }
                }

                return new string(strPtr, fin, strLen - fin);
            }
        }

        // Time format: hour:minutes:seconds.fraction
        public static string ParseTime(GDMTime time, StringSpan strValue)
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
        public static string ParseTime(StringSpan strValue, out byte hour, out byte minutes, out byte seconds, out short fraction)
        {
            hour = 0;
            minutes = 0;
            seconds = 0;
            fraction = 0;

            string result;
            if (!strValue.IsEmpty) {
                var strTok = GEDCOMParser.Default;
                strTok.Reset(strValue);

                hour = (byte)strTok.RequestInt();
                strTok.RequestSymbol(':');
                minutes = (byte)strTok.RequestInt();

                strTok.Next();
                if (strTok.HasSymbol(':')) {
                    seconds = (byte)strTok.RequestInt();

                    strTok.Next();
                    if (strTok.HasSymbol('.')) {
                        fraction = (short)strTok.RequestInt();
                    }
                }

                result = strTok.GetRest();
            } else {
                result = string.Empty;
            }

            return result;
        }

        // CutoutPosition format: x1 y1 x2 y2
        public static string ParseCutoutPosition(GDMCutoutPosition position, StringSpan strValue)
        {
            try {
                int x1 = 0;
                int y1 = 0;
                int x2 = 0;
                int y2 = 0;

                if (!strValue.IsEmptyOrEnd) {
                    var parser = new GEDCOMParser(strValue, true);
                    x1 = parser.RequestSignedInt();
                    y1 = parser.RequestSignedInt();
                    x2 = parser.RequestSignedInt();
                    y2 = parser.RequestSignedInt();
                }

                position.SetRawData(x1, y1, x2, y2);
            } catch (Exception ex) {
                Logger.WriteError(string.Format("GEDCOMUtils.ParseCutoutPosition({0}): ", strValue), ex);
            }

            return string.Empty;
        }

        // DateValue format: INT/FROM/TO/etc..._<date>
        public static string ParseDateValue(GDMTree owner, GDMDateValue dateValue, StringSpan strSpan)
        {
            if (strSpan.IsEmptyOrEnd) {
                return string.Empty;
            }

            var strTok = GEDCOMParser.Default;
            strTok.Reset(strSpan);

            strTok.SkipWhitespaces();

            int idx = 0;
            if (strTok.CurrentToken == GEDCOMToken.Word) {
                string su = strTok.GetWord();
                idx = ArrayHelper.BinarySearch(GEDCOMConsts.GEDCOMDateTypes, su, string.CompareOrdinal);
            }
            var dateType = (idx < 0) ? GEDCOMDateType.SIMP : (GEDCOMDateType)idx;

            string result;
            GDMCustomDate custDate;
            switch (dateType) {
                case GEDCOMDateType.AFT:
                case GEDCOMDateType.BEF:
                case GEDCOMDateType.BET: {
                        var dateRange = new GDMDateRange();
                        result = ParseRangeDate(owner, dateRange, strTok);
                        custDate = dateRange;
                    }
                    break;

                case GEDCOMDateType.INT: {
                        var dateInt = new GDMDateInterpreted();
                        result = ParseIntDate(owner, dateInt, strTok);
                        custDate = dateInt;
                    }
                    break;

                case GEDCOMDateType.FROM:
                case GEDCOMDateType.TO: {
                        var datePeriod = new GDMDatePeriod();
                        result = ParsePeriodDate(owner, datePeriod, strTok);
                        custDate = datePeriod;
                    }
                    break;

                default: {
                        var date = new GDMDate();
                        result = ParseDate(owner, date, strTok);
                        custDate = date;
                    }
                    break;
            }

            dateValue.SetRawData(custDate);
            return result;
        }

        // Format: FROM DATE1 TO DATE2
        public static string ParsePeriodDate(GDMDatePeriod date, StringSpan strValue)
        {
            var strTok = GEDCOMParser.Default;
            strTok.Reset(strValue);
            // only standard GEDCOM dates (for owner == null)
            return ParsePeriodDate(null, date, strTok);
        }

        // Format: FROM DATE1 TO DATE2
        public static string ParsePeriodDate(GDMTree owner, GDMDatePeriod date, GEDCOMParser strTok)
        {
            strTok.SkipWhitespaces();

            if (strTok.HasWord(GEDCOMTagName.FROM)) {
                strTok.Next();
                ParseDate(owner, date.DateFrom, strTok);
                strTok.SkipWhitespaces();
            }

            if (strTok.HasWord(GEDCOMTagName.TO)) {
                strTok.Next();
                ParseDate(owner, date.DateTo, strTok);
                strTok.SkipWhitespaces();
            }

            return strTok.GetRest();
        }

        // Format: AFT DATE | BEF DATE | BET AFT_DATE AND BEF_DATE
        public static string ParseRangeDate(GDMDateRange date, StringSpan strValue)
        {
            var strTok = GEDCOMParser.Default;
            strTok.Reset(strValue);
            // only standard GEDCOM dates (for owner == null)
            return ParseRangeDate(null, date, strTok);
        }

        // Format: AFT DATE | BEF DATE | BET AFT_DATE AND BEF_DATE
        public static string ParseRangeDate(GDMTree owner, GDMDateRange date, GEDCOMParser strTok)
        {
            /*GEDCOMFormat format = (owner == null) ? GEDCOMFormat.gf_Native : owner.Format;
            bool isAQDeviance = (format == GEDCOMFormat.gf_AncestQuest);*/

            strTok.SkipWhitespaces();

            if (strTok.CurrentToken != GEDCOMToken.Word) {
                // error!
            }
            string su = strTok.GetWord();
            int dateType = ArrayHelper.BinarySearch(GEDCOMConsts.GEDCOMDateRangeArray, su, string.CompareOrdinal);

            if (dateType == 0) { // "AFT"
                strTok.Next();
                ParseDate(owner, date.After, strTok);
            } else if (dateType == 1) { // "BEF"
                strTok.Next();
                ParseDate(owner, date.Before, strTok);
            } else if (dateType == 2) { // "BET"
                strTok.Next();

                /*if (isAQDeviance && strTok.RequireSymbol('.')) {
                    strTok.Next();
                }*/

                ParseDate(owner, date.After, strTok);
                strTok.SkipWhitespaces();

                if (!strTok.HasWord(GEDCOMTagName.AND)) {
                    //&& !(isAQDeviance && strTok.RequireSymbol('-'))) {
                    throw new GEDCOMRangeDateException(strTok.GetFullStr());
                }

                strTok.Next();
                strTok.SkipWhitespaces();
                ParseDate(owner, date.Before, strTok);
            }

            return strTok.GetRest();
        }

        // Format: INT DATE (phrase)
        public static string ParseIntDate(GDMDateInterpreted date, StringSpan strValue)
        {
            var strTok = GEDCOMParser.Default;
            strTok.Reset(strValue);
            // only standard GEDCOM dates (for owner == null)
            return ParseIntDate(null, date, strTok);
        }

        // Format: INT DATE (phrase)
        public static string ParseIntDate(GDMTree owner, GDMDateInterpreted date, GEDCOMParser strTok)
        {
            strTok.SkipWhitespaces();

            if (!strTok.HasWord(GEDCOMTagName.INT)) {
                throw new GEDCOMIntDateException(strTok.GetFullStr());
            }
            strTok.Next();
            ParseDate(owner, date, strTok);

            strTok.SkipWhitespaces();
            if (strTok.HasSymbol('(')) {
                var phrase = new StringBuilder();
                phrase.Append(strTok.GetWord());
                do {
                    strTok.Next();
                    phrase.Append(strTok.GetWord());
                } while (!strTok.HasSymbol(')'));

                date.DatePhrase = phrase.ToString();
            } else {
                date.DatePhrase = string.Empty;
            }

            return strTok.GetRest();
        }

        /*public static string ParseDate(GDMDate date, string strValue)
        {
            var strTok = GEDCOMParser.Default;
            strTok.Reset(strValue);
            // only standard GEDCOM dates (for owner == null)
            return ParseDate(null, date, strTok);
        }*/

        public static string ParseDate(GDMDate date, StringSpan strValue)
        {
            var strTok = GEDCOMParser.Default;
            strTok.Reset(strValue);
            // only standard GEDCOM dates (for owner == null)
            return ParseDate(null, date, strTok);
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

        // Format: [ <YEAR>[B.C.] | <MONTH> <YEAR> | <DAY> <MONTH> <YEAR> ] (see p.45-46)
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
            GEDCOMDateFormat dateFormat = GEDCOMDateFormat.Standard;

            strTok.SkipWhitespaces();

            GEDCOMFormat format = (owner == null) ? GEDCOMFormat.Native : owner.Format;
            bool isAhnDeviance = (format == GEDCOMFormat.Ahnenblatt);

            if (isAhnDeviance && strTok.HasSymbol('(')) {
                GEDCOMUtils.PrepareAhnenblattDate(strTok.Data, strTok.Position, strTok.Length);
                strTok.Next();
            }

            // extract approximated
            if (strTok.CurrentToken == GEDCOMToken.Word) {
                string su = InvariantTextInfo.ToUpper(strTok.GetWord());
                int idx = ArrayHelper.BinarySearch(GEDCOMConsts.GEDCOMDateApproximatedArray, su, string.CompareOrdinal);
                if (idx >= 0) {
                    approximated = (GDMApproximated)idx;
                    strTok.Next();
                    strTok.SkipWhitespaces();
                }
            }

            // extract escape
            if (strTok.CurrentToken == GEDCOMToken.XRef) {
                // FIXME: check for errors
                var escapeStr = strTok.GetWord();
                int idx = ArrayHelper.IndexOf(GEDCOMConsts.GEDCOMDateEscapeArray, escapeStr);
                if (idx >= 0) {
                    calendar = (GDMCalendar)idx;
                }

                strTok.Next();
                strTok.SkipWhitespaces();
            }

            // extract day
            int dNum;
            if (strTok.HasNumber(2, out dNum) && (dNum <= 31)) {
                day = (byte)dNum;
                strTok.Next();
            }

            // extract delimiter
            if (strTok.HasWhitespace(' ')) {
                dateFormat = GEDCOMDateFormat.Standard;
                strTok.Next();
            } else if (strTok.HasSymbol('.')) {
                dateFormat = GEDCOMDateFormat.System;
                strTok.Next();
            }

            // extract month
            if (strTok.CurrentToken == GEDCOMToken.Word) {
                // in this case, according to performance test results, BinarySearch is more efficient
                // than a simple search or even a dictionary search (why?!)
                string su = InvariantTextInfo.ToUpper(strTok.GetWord());
                int idx = BinarySearch(GEDCOMConsts.GEDCOMMonthValues, su, string.CompareOrdinal);
                month = (byte)((idx < 0) ? 0 : idx);

                strTok.Next();
            } else if (dateFormat == GEDCOMDateFormat.System && strTok.HasNumber(2, out int mNum)) {
                month = (byte)mNum;

                strTok.Next();
            }

            // extract delimiter
            if (dateFormat == GEDCOMDateFormat.System) {
                if (strTok.HasSymbol('.')) {
                    strTok.Next();
                }
            } else {
                if (strTok.HasWhitespace(' ')) {
                    strTok.Next();
                }
            }

            // extract negative years
            if (strTok.HasSymbol('-')) {
                yearBC = true;
                strTok.Next();
            }

            // extract year
            int yNum;
            if (strTok.HasNumber(4, out yNum)) {
                year = (short)yNum;
                strTok.Next();

                // extract year modifier
                if (strTok.HasSymbol(GEDCOMConsts.YearModifierSeparator)) {
                    strTok.Next();
                    if (strTok.CurrentToken != GEDCOMToken.Number) {
                        // error
                    } else {
                        yearModifier = strTok.GetWord();
                    }
                    strTok.Next();
                }

                // extract bc/ad
                // GEDCOM readers should accept B.C., BC and BCE (GEDCOM 5.5.1 Annotated Edition)
                if (strTok.CurrentToken == GEDCOMToken.Word && strTok.GetSymbol() == 'B') {
                    strTok.Next();
                    if (strTok.HasSymbol('.')) {
                        strTok.Next();
                    }
                    if (strTok.CurrentToken != GEDCOMToken.Word || strTok.GetSymbol() != 'C') {
                        // error
                    }
                    strTok.Next();
                    if (strTok.HasSymbol('.')) {
                        strTok.Next();
                    }
                    if (strTok.CurrentToken == GEDCOMToken.Word && strTok.GetSymbol() == 'E') {
                        strTok.Next();
                    }
                    yearBC = true;
                }
            }

            if (isAhnDeviance && strTok.HasSymbol(')')) {
                strTok.Next();
            }

            if (day > 0 && month == 0 && year == GDMDate.UNKNOWN_YEAR) {
                year = day;
                day = 0;
            }

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

        public static unsafe string ParseName(GDMPersonalName persName, StringSpan strValue)
        {
            if (strValue.IsEmptyOrEnd) {
                return string.Empty;
            }

            string firstPart = string.Empty;
            string surname = string.Empty;
            string lastPart = string.Empty;

            int strBeg = strValue.Pos;
            int strLen = strValue.Length;

            fixed (char* strPtr = strValue.Data) {
                // skip leading whitespaces
                while (strBeg < strLen && strPtr[strBeg] == ' ') strBeg++;

                // check empty string
                if (strLen - strBeg == 0) {
                    return string.Empty;
                }

                int fs = -1, ss = strBeg;
                for (int i = strBeg; i < strLen; i++) {
                    char chr = strPtr[i];
                    if (chr == GEDCOMConsts.NameSeparator) {
                        if (fs == -1) {
                            fs = i;
                            firstPart = new string(strPtr, strBeg, i - strBeg);
                        } else {
                            ss = i;
                            surname = new string(strPtr, fs + 1, (ss - fs) - 1);
                            ss += 1;
                            break;
                        }
                    }
                }

                if (fs < 0) {
                    firstPart = new string(strPtr, ss, strLen - ss);
                } else {
                    lastPart = new string(strPtr, ss, strLen - ss);
                }
            }

            persName.SetRawData(firstPart, surname, lastPart);
            return string.Empty;
        }

        public static string GetFullName(string firstPart, string surname, string lastPart)
        {
            string result = firstPart;
            if (!string.IsNullOrEmpty(surname)) {
                result += " " + surname;

                if (!string.IsNullOrEmpty(lastPart)) {
                    result += " " + lastPart;
                }
            }
            return result;
        }

        // see "THE GEDCOM STANDARD Release 5.5.1", p.54 ("NAME_PERSONAL")
        public static string GetNameTagValue(GDMPersonalName personalName)
        {
            string firstPart = personalName.FirstPart;
            string surname = personalName.Surname;
            string lastPart = personalName.NameSuffix;

            if (!string.IsNullOrEmpty(personalName.NamePrefix)) {
                if (!string.IsNullOrEmpty(firstPart)) {
                    firstPart = " " + firstPart;
                }
                firstPart = personalName.NamePrefix + firstPart;
            }

            if (!string.IsNullOrEmpty(personalName.SurnamePrefix)) {
                if (!string.IsNullOrEmpty(surname)) {
                    surname = " " + surname;
                }
                surname = personalName.SurnamePrefix + surname;
            }

            string result = firstPart;
            if (!string.IsNullOrEmpty(surname)) {
                result += " /" + surname + "/";

                if (!string.IsNullOrEmpty(lastPart)) {
                    result += " " + lastPart;
                }
            }
            return result;
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
                    throw new GEDCOMBlobDecodeException();
                }

                ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c2 = (byte)ix;
                } else {
                    throw new GEDCOMBlobDecodeException();
                }

                ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c3 = (byte)ix;
                } else {
                    throw new GEDCOMBlobDecodeException();
                }

                ix = validChars.IndexOf(blob[i++]);
                if (ix >= 0) {
                    c4 = (byte)ix;
                } else {
                    throw new GEDCOMBlobDecodeException();
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

        public static string ParseDate(GDMTree owner, StringSpan strValue, out DateTime date)
        {
            short year;
            byte month, day;

            var strTok = GEDCOMParser.Default;
            strTok.Reset(strValue);
            string result = ParseDate(owner, strTok, out _, out _, out year, out _, out _, out month, out day);

            try {
                date = new DateTime(year, month, day);
            } catch (Exception ex) {
                Logger.WriteError(string.Format("GEDCOMUtils.ParseDate({0}-{1}-{2}): ", year, month, day), ex);
                date = DateTime.MinValue;
            }

            return result;
        }

        public static string GetDateStr(DateTime date)
        {
            var result = string.Format("{0:00} {1} {2:0000}", new object[] { date.Day, GEDCOMConsts.GEDCOMMonthArray[date.Month - 1], date.Year });
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

        public static string ParseTime(StringSpan strValue, out TimeSpan time)
        {
            byte hour, minutes, seconds;
            short fraction;

            try {
                strValue = ParseTime(strValue, out hour, out minutes, out seconds, out fraction);
                time = new TimeSpan(0, hour, minutes, seconds, (int)(100u * fraction));
            } catch (Exception ex) {
                Logger.WriteError(string.Format("GEDCOMUtils.ParseTime({0}): ", strValue), ex);
                time = TimeSpan.Zero;
            }

            return strValue;
        }

        public static string ParseAge(GDMAge age, StringSpan strValue)
        {
            age.Clear();
            if (strValue.IsEmptyOrEnd) {
                return string.Empty;
            }

            var strTok = new GEDCOMParser(true, true);
            strTok.Reset(strValue);
            strTok.SkipWhitespaces();

            if (strTok.CurrentToken == GEDCOMToken.Word) {
                string su = InvariantTextInfo.ToUpper(strTok.GetWord());

                if (su == "INFANT") {
                    age.Relative = -1;
                    age.Years = 1;
                    return string.Empty;
                } else if (su == "CHILD") {
                    age.Relative = -1;
                    age.Years = 8;
                    return string.Empty;
                } else if (su == "STILLBORN") {
                    age.Relative = 0;
                    age.Years = 0;
                    age.Months = 0;
                    age.Days = 0;
                    return string.Empty;
                }

                strTok.Next();
            }

            int relative = 0;
            int years = -1;
            int months = -1;
            int days = -1;

            if (strTok.CurrentToken == GEDCOMToken.Symbol) {
                var sym = strTok.GetSymbol();
                if (sym == '<') {
                    relative = -1;
                } else if (sym == '>') {
                    relative = 1;
                }
                strTok.Next();
            }

            int val = -1;
            bool valid = true;

            GEDCOMToken token;
            while ((token = strTok.CurrentToken) != GEDCOMToken.EOL) {
                if (token == GEDCOMToken.Number) {
                    val = strTok.GetNumber();
                } else if (token == GEDCOMToken.Word) {
                    var c = strTok.GetSymbol();
                    if (c == 'Y' || c == 'y') {
                        years = val;
                    } else if (c == 'M' || c == 'm') {
                        months = val;
                    } else if (c == 'D' || c == 'd') {
                        days = val;
                    }
                }
                strTok.Next();
            }

            if (valid && (years != -1 || months != -1 || days != -1)) {
                age.Relative = relative;
                age.Years = years;
                age.Months = months;
                age.Days = days;
            }

            return strTok.GetRest();
        }

        /*public static string ParseAge(GDMAge age, string strValue)
        {
            if (string.Compare(strValue, "INFANT", true) == 0) {
                age.Relative = -1;
                age.Years = 1;
                return string.Empty;
            } else if (string.Compare(strValue, "CHILD", true) == 0) {
                age.Relative = -1;
                age.Years = 8;
                return string.Empty;
            } else if (string.Compare(strValue, "STILLBORN", true) == 0) {
                age.Relative = 0;
                age.Years = 0;
                age.Months = 0;
                age.Days = 0;
                return string.Empty;
            } else {
                int relative = 0;
                int years = -1;
                int months = -1;
                int days = -1;

                int chIdx = 0;
                if (strValue[0] == '<') {
                    relative = -1;
                    chIdx = 1;
                } else if (strValue[0] == '>') {
                    relative = 1;
                    chIdx = 1;
                }

                bool valid = true;
                int val = -1;
                while (valid && (chIdx < strValue.Length)) {
                    char c = strValue[chIdx];

                    if (!char.IsWhiteSpace(c)) {
                        bool isDigit = char.IsDigit(c);

                        if (val == -1 && !isDigit) {
                            valid = false;
                        } else if (isDigit) {
                            int digit = c - '0';
                            val = (val == -1) ? digit : (val * 10 + digit);
                        } else if (c == 'Y' || c == 'y') {
                            if (years != -1) {
                                valid = false;
                            } else {
                                years = val;
                                val = -1;
                            }
                        } else if (c == 'M' || c == 'm') {
                            if (months != -1) {
                                valid = false;
                            } else {
                                months = val;
                                val = -1;
                            }
                        } else if (c == 'D' || c == 'd') {
                            if (days != -1) {
                                valid = false;
                            } else {
                                days = val;
                                val = -1;
                            }
                        } else {
                            valid = false;
                        }
                    }

                    chIdx++;
                }

                if (valid && (years != -1 || months != -1 || days != -1)) {
                    age.Relative = relative;
                    age.Years = years;
                    age.Months = months;
                    age.Days = days;
                }

                return strValue.Substring(chIdx);
            }
        }*/

        #endregion

        #region GEDCOM Enums Parse

        public static string Enum2Str(IConvertible enumVal, string[] values)
        {
#if PCL
            int idx = (int)Convert.ChangeType(enumVal, typeof(int), null);
#else
            int idx = (int)enumVal;
#endif

            return (idx < 0 || idx >= values.Length) ? string.Empty : values[idx];
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

            int idx = ArrayHelper.BinarySearch<string>(values, val, string.CompareOrdinal);
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
                } else {
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


        public static readonly string[] Restrictions = new string[] {
            "", "locked", "confidential", "privacy" };

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
            return GEDCOMUtils.Enum2Str(value, Restrictions);
        }


        public static readonly string[] PedigreeLinkageTypes = new string[] {
            "", "adopted", "birth", "foster" };

        public static GDMPedigreeLinkageType GetPedigreeLinkageTypeVal(string str)
        {
            return Str2Enum(str, PedigreeLinkageTypes, GDMPedigreeLinkageType.plNone);
        }

        public static string GetPedigreeLinkageTypeStr(GDMPedigreeLinkageType value)
        {
            return GEDCOMUtils.Enum2Str(value, PedigreeLinkageTypes);
        }


        public static readonly string[] ChildLinkageStatuses = new string[] {
            "", "challenged", "disproven", "proven" };

        public static GDMChildLinkageStatus GetChildLinkageStatusVal(string str)
        {
            return Str2Enum(str, ChildLinkageStatuses, GDMChildLinkageStatus.clNone);
        }

        public static string GetChildLinkageStatusStr(GDMChildLinkageStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, ChildLinkageStatuses);
        }


        public static readonly string[] CommunicationTypes = new string[] {
            "call", "email", "fax", "letter", "tape", "visit" };

        public static GDMCommunicationType GetCommunicationTypeVal(string str)
        {
            return Str2Enum(str, CommunicationTypes, GDMCommunicationType.ctVisit);
        }

        public static string GetCommunicationTypeStr(GDMCommunicationType value)
        {
            return GEDCOMUtils.Enum2Str(value, CommunicationTypes);
        }


        public static readonly string[] MultimediaFormats = new string[] {
            "",
            "bmp", "gif", "jpg", "pcx", "tif", "tga", "png", "raw", "psd", "webp",
            "txt", "rtf", "htm", "pdf",
            "wav", "mp3", "wma", "mka",
            "avi", "mpg", "wmv", "mp4", "ogv", "mkv", "mov",
            "djvu", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "odt", "ods", "odp",
            "zip", "rar", "7z",
            "ole", "z" };

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
            } else if (str == "djvu") {
                result = GDMMultimediaFormat.mfDJVU;
            } else if (str == "doc") {
                result = GDMMultimediaFormat.mfDOC;
            } else if (str == "docx") {
                result = GDMMultimediaFormat.mfDOCX;
            } else if (str == "xls") {
                result = GDMMultimediaFormat.mfXLS;
            } else if (str == "xlsx") {
                result = GDMMultimediaFormat.mfXLSX;
            } else if (str == "ppt") {
                result = GDMMultimediaFormat.mfPPT;
            } else if (str == "pptx") {
                result = GDMMultimediaFormat.mfPPTX;
            } else if (str == "odt") {
                result = GDMMultimediaFormat.mfODT;
            } else if (str == "ods") {
                result = GDMMultimediaFormat.mfODS;
            } else if (str == "odp") {
                result = GDMMultimediaFormat.mfODP;
            } else if (str == "zip") {
                result = GDMMultimediaFormat.mfZIP;
            } else if (str == "rar") {
                result = GDMMultimediaFormat.mfRAR;
            } else if (str == "7z") {
                result = GDMMultimediaFormat.mf7Z;
            } else if (str == "webp") {
                result = GDMMultimediaFormat.mfWEBP;
            } else {
                result = GDMMultimediaFormat.mfUnknown;
            }
            return result;
        }

        public static string GetMultimediaFormatStr(GDMMultimediaFormat value)
        {
            return GEDCOMUtils.Enum2Str(value, MultimediaFormats);
        }


        public static readonly string[] MediaTypes = new string[] {
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


        public static readonly string[] NameTypes = new string[] {
            "", "adoption", "aka", "birth", "immigrant", "maiden", "married" };

        public static GDMNameType GetNameTypeVal(string str)
        {
            return Str2Enum(str, NameTypes, GDMNameType.ntNone);
        }

        public static string GetNameTypeStr(GDMNameType value)
        {
            return GEDCOMUtils.Enum2Str(value, NameTypes);
        }


        public static readonly string[] ResearchStatuses = new string[] {
            "defined", "inprogress", "onhold", "problems", "completed", "withdrawn" };

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
            string str = ResearchStatuses[(int)value];
            return str;
        }


        public static int GetIntVal(string str, int defValue = 0)
        {
            int result = string.IsNullOrEmpty(str) ? defValue : ConvertHelper.ParseInt(str, defValue);
            return result;
        }

        public static string GetIntStr(int value, int emptyValue = -1)
        {
            string result = (value == emptyValue) ? string.Empty : value.ToString();
            return result;
        }


        public static bool GetBoolVal(string str, bool defValue = false)
        {
            bool result = string.IsNullOrEmpty(str) ? defValue : (str == "Y");
            return result;
        }

        public static string GetBoolStr(bool value)
        {
            return (value) ? "Y" : "N";
        }


        public static readonly string[] OrdinanceProcessFlags = new string[] {
            "", "no", "yes" };

        public static GDMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string su)
        {
            return Str2Enum(su, OrdinanceProcessFlags, GDMOrdinanceProcessFlag.opNone);
        }

        public static string GetOrdinanceProcessFlagStr(GDMOrdinanceProcessFlag value)
        {
            return GEDCOMUtils.Enum2Str(value, OrdinanceProcessFlags);
        }


        public static readonly string[] ResearchPriorities = new string[] {
            "", "low", "normal", "high", "top" };

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

        public static string GetPriorityStr(GDMResearchPriority value)
        {
            return ResearchPriorities[(int)value];
        }


        public static readonly string[] CharacterSets = new string[] {
            "ASCII", "ANSEL", "UNICODE", "UTF-8" };

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

        public static string GetCharacterSetStr(GEDCOMCharacterSet value)
        {
            return CharacterSets[(int)value];
        }


        public static readonly string[] Sexes = new string[] {
            "U", "M", "F", "X" };

        public static GDMSex GetSexVal(string str)
        {
            if (str.Length != 1) return GDMSex.svUnknown;

            GDMSex result;

            char sl = InvariantTextInfo.ToUpper(str[0]);
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
            return Sexes[(int)value];
        }


        public static readonly string[] MarriageStatuses = new string[] {
            "", "married", "marrnotreg", "notmarr" };

        public static GDMMarriageStatus GetMarriageStatusVal(string str)
        {
            return Str2Enum(str, MarriageStatuses, GDMMarriageStatus.Unknown);
        }

        public static string GetMarriageStatusStr(GDMMarriageStatus value)
        {
            return GEDCOMUtils.Enum2Str(value, MarriageStatuses);
        }


        public static readonly string[] DNAFileFormats = new string[] {
            "", "SNP", "STR" };

        public static GDMDNAFileFormat GetDNAFileFormatVal(string str)
        {
            return Str2Enum(str, DNAFileFormats, GDMDNAFileFormat.None, false);
        }

        public static string GetDNAFileFormatStr(GDMDNAFileFormat value)
        {
            return GEDCOMUtils.Enum2Str(value, DNAFileFormats);
        }

        #endregion

        #region Aux functions

        public static string GetSignByRecord(GDMRecord record)
        {
            string result = string.Empty;
            if (record == null) return result;

            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    result = "I"; // Std (p.24)
                    break;
                case GDMRecordType.rtFamily:
                    result = "F"; // Std (p.24)
                    break;
                case GDMRecordType.rtNote:
                    result = "N"; // Std=!T!
                    break;
                case GDMRecordType.rtMultimedia:
                    result = "O"; // Std=!M!
                    break;
                case GDMRecordType.rtSource:
                    result = "S"; // Std (p.24)
                    break;
                case GDMRecordType.rtRepository:
                    result = "R"; // Std (p.24)
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
                    result = "L"; // GEDCOM 5.5 EL !P!
                    break;
                case GDMRecordType.rtSubmission:
                    result = "????"; // FIXME: to standard !N!
                    break;
                case GDMRecordType.rtSubmitter:
                    result = "SUB"; // Std=!U!
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

        public static GDMLines GetTagStrings(GDMTag strTag)
        {
            var strings = new GDMLines();

            if (strTag != null) {
                if (strTag.StringValue != "") {
                    strings.Add(strTag.StringValue);
                }

                var subTags = strTag.SubTags;
                int num = subTags.Count;
                for (int i = 0; i < num; i++) {
                    GDMTag tag = subTags[i];
                    var tagType = tag.GetTagType();

                    if (tagType == GEDCOMTagType.CONC) {
                        if (strings.Count > 0) {
                            strings[strings.Count - 1] = string.Concat(strings[strings.Count - 1], tag.StringValue);
                        } else {
                            strings.Add(tag.StringValue);
                        }
                    } else {
                        if (tagType == GEDCOMTagType.CONT) {
                            strings.Add(tag.StringValue);
                        }
                    }
                }
            }

            return strings;
        }

        public static void SetTagStrings(GDMTag tag, GDMLines strings)
        {
            if (tag == null) return;

            tag.StringValue = "";
            var subTags = tag.SubTags;
            for (int i = subTags.Count - 1; i >= 0; i--) {
                GDMTag subtag = subTags[i];
                var tagType = subtag.GetTagType();

                if (tagType == GEDCOMTagType.CONT || tagType == GEDCOMTagType.CONC) {
                    subTags.RemoveAt(i);
                }
            }

            if (strings != null) {
                bool isRecordTag = (tag is GDMRecord);

                int num = strings.Count;
                for (int i = 0; i < num; i++) {
                    string str = strings[i];

                    int len = Math.Min(str.Length, GEDCOMConsts.MaxLineLength);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag(new GDMValueTag((int)GEDCOMTagType.CONT, sub));
                    }

                    while (str.Length > 0) {
                        len = Math.Min(str.Length, GEDCOMConsts.MaxLineLength);
                        tag.AddTag(new GDMValueTag((int)GEDCOMTagType.CONC, str.Substring(0, len)));
                        str = str.Remove(0, len);
                    }
                }
            }
        }

        #endregion

        #region Event type detection

        public static bool IsIndiEvent(GEDCOMTagType tag)
        {
            return (tag == GEDCOMTagType.CENS || tag == GEDCOMTagType.EVEN) || (tag >= GEDCOMTagType.ADOP && tag <= GEDCOMTagType.WILL);
        }

        public static bool IsIndiAttr(GEDCOMTagType tag)
        {
            return (tag == GEDCOMTagType.RESI) || (tag >= GEDCOMTagType.CAST && tag <= GEDCOMTagType._YDNA);
        }

        public static bool IsFamEvent(GEDCOMTagType tag)
        {
            return (tag >= GEDCOMTagType.CENS && tag <= GEDCOMTagType.RESI) || (tag >= GEDCOMTagType.ANUL && tag <= GEDCOMTagType.MARS);
        }

        #endregion
    }
}
