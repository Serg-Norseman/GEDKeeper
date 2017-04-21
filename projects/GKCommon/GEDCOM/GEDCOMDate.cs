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
using System.Globalization;
using System.Threading;

namespace GKCommon.GEDCOM
{
    [Serializable]
    public class GEDCOMDateException : EGEDCOMException
    {
        public GEDCOMDateException(string message) : base(message)
        {
        }
    }

    /// <summary>
    /// Class to hold simple standard GEDCOM dates.
    /// Note: Year cannot be used externally with negative values even for "BC",
    /// because these dates there is a special property.
    /// Dates of type "BC" should have a positive Year + the property YearBC.
    /// </summary>
    public class GEDCOMDate : GEDCOMCustomDate
    {
        public const int UNKNOWN_YEAR = -1;

        private GEDCOMApproximated fApproximated;
        private GEDCOMCalendar fCalendar;
        private GEDCOMDateFormat fDateFormat;
        private ushort fDay;
        private string fMonth;
        private int fYear;
        private bool fYearBC;
        private string fYearModifier;
        private UDN fUDN;


        public GEDCOMApproximated Approximated
        {
            get { return fApproximated; }
            set { fApproximated = value; }
        }

        public GEDCOMCalendar DateCalendar
        {
            get { return fCalendar; }
        }

        public ushort Day
        {
            get { return fDay; }
            set {
                fDay = value;
                DateChanged();
            }
        }

        public string Month
        {
            get { return fMonth; }
            set {
                fMonth = value;
                DateChanged();
            }
        }

        public int Year
        {
            get { return fYear; }
            set {
                fYear = value;
                DateChanged();
            }
        }

        public bool YearBC
        {
            get { return fYearBC; }
            set {
                fYearBC = value;
                DateChanged();
            }
        }

        public string YearModifier
        {
            get { return fYearModifier; }
            set { fYearModifier = value; }
        }


        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMDate(owner, parent, tagName, tagValue);
        }

        public GEDCOMDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("DATE");

            fApproximated = GEDCOMApproximated.daExact;
            fCalendar = GEDCOMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = "";
            fMonth = "";
            fDay = 0;
            fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
        }

        public override void Clear()
        {
            base.Clear();

            fCalendar = GEDCOMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = "";
            fMonth = "";
            fDay = 0;

            DateChanged();
        }

        public bool IsValidDate()
        {
            return (fYear > 0 && fMonth != "" && fDay > 0);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fYear <= 0 && fMonth == "" && fDay <= 0;
        }

        public override void Assign(GEDCOMTag source)
        {
            //base.Assign(source);

            GEDCOMDate srcDate = source as GEDCOMDate;
            if (srcDate != null) {
                fCalendar = srcDate.fCalendar;
                fYear = srcDate.fYear;
                fYearBC = srcDate.fYearBC;
                fYearModifier = srcDate.fYearModifier;
                fMonth = srcDate.fMonth;
                fDay = srcDate.fDay;

                DateChanged();
            } else {
                base.Assign(source);
            }
        }

        public override DateTime GetDateTime()
        {
            DateTime result;

            ushort month = GEDCOMMonthToInt(fMonth);
            ushort day = fDay;
            if (fYear >= 0 && month >= 1 && month <= 12 && day >= 1 && day < 32)
            {
                result = new DateTime(fYear, month, day);
                return result;
            }

            result = new DateTime(0);
            return result;
        }

        public override void SetDateTime(DateTime value)
        {
            SetGregorian((ushort)value.Day, GEDCOMMonthArray[value.Month - 1], value.Year, "", false);
        }

        public override string ParseString(string strValue)
        {
            GEDCOMFormat format = GEDCOMProvider.GetGEDCOMFormat(Owner);

            fCalendar = GEDCOMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = "";
            fMonth = "";
            fDay = 0;

            string result = strValue;

            if (!string.IsNullOrEmpty(result))
            {
                if (format == GEDCOMFormat.gf_Ahnenblatt) {
                    result = PrepareAhnenblattDate(result);
                }

                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = ExtractApproximated(result);

                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = ExtractEscape(result);

                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = ExtractDay(result);

                if (result.Length > 0)
                {
                    if (result[0] == ' ')
                    {
                        fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
                    }
                    else
                    {
                        if (result[0] == '.')
                        {
                            fDateFormat = GEDCOMDateFormat.dfSystem;
                        }
                    }
                }

                result = ExtractDelimiterEx(result);
                result = ExtractMonth(result);
                result = ExtractDelimiterEx(result);
                result = ExtractYear(result);
            }

            DateChanged();

            return result;
        }

        #region Private methods of parsing of the input format

        private static string PrepareAhnenblattDate(string str)
        {
            // TODO: remove this dirty hack!
            string result = str.Trim();
            if (!string.IsNullOrEmpty(result)) {
                if (result.StartsWith("(") && result.EndsWith(")")) {
                    result = result.Substring(1, result.Length - 2);

                    // ALERT: Ahnenblatt GEDCOM files can contain the dates with any separator!
                    // by standard it's "(<DATE_PHRASE>)" (gedcom-5.5.1, p.47)
                    // FIXME: this code need to move to GEDCOMDateInterpreted
                    result = result.Replace('/', '.');
                    result = result.Replace('-', '.');
                    result = result.Replace(' ', '.');
                }
            }
            return result;
        }

        private string DayString(bool noDelimiter)
        {
            string result;

            if (fDay <= 0)
            {
                result = "";
            }
            else
            {
                result = fDay.ToString();
                if (result.Length == 1)
                {
                    result = "0" + result;
                }
                if (!noDelimiter)
                {
                    result += " ";
                }
            }

            return result;
        }

        private string EscapeString(bool noDelimiter, bool alwaysShowEscape)
        {
            string result;
            if (alwaysShowEscape || fCalendar != GEDCOMCalendar.dcGregorian)
            {
                result = GEDCOMDateEscapeArray[(int)fCalendar];
                if (!noDelimiter)
                {
                    result += " ";
                }
            }
            else
            {
                result = "";
            }
            return result;
        }

        private string MonthString(bool noDelimiter)
        {
            string result;
            if (fMonth == "")
            {
                result = "";
            }
            else
            {
                result = fMonth;
                if (!noDelimiter)
                {
                    result += " ";
                }
            }
            return result;
        }

        private string YearGregString(bool noDelimiter)
        {
            string result;

            if (fYear == UNKNOWN_YEAR)
            {
                result = "";
            }
            else
            {
                result = fYear.ToString();
                if (fYearModifier != "")
                {
                    result = result + "/" + fYearModifier;
                }
                if (fYearBC)
                {
                    result += GEDCOMProvider.GEDCOM_YEAR_BC;
                }
                if (!noDelimiter)
                {
                    result += " ";
                }
            }

            return result;
        }

        private string YearString(bool noDelimiter)
        {
            string result;

            if (fYear == UNKNOWN_YEAR)
            {
                result = "";
            }
            else
            {
                result = fYear.ToString();
                if (fYearBC)
                {
                    result += GEDCOMProvider.GEDCOM_YEAR_BC;
                }
                if (!noDelimiter)
                {
                    result += " ";
                }
            }

            return result;
        }

        private string ExtractApproximated(string str)
        {
            string result = str;
            string su = result.Substring(0, 3).ToUpperInvariant();

            for (GEDCOMApproximated i = GEDCOMApproximated.daAbout; i <= GEDCOMApproximated.daEstimated; i++) {
                if (su == GEDCOMDateApproximatedArray[(int)i]) {
                    fApproximated = i;
                    result = result.Remove(0, 3);
                    break;
                }
            }

            return result;
        }

        private string ExtractEscape(string str)
        {
            string result = str;

            if (result.StartsWith("@#"))
            {
                int p = result.IndexOf("@", 2);
                if (p >= 0)
                {
                    string su = result.Substring(0, p + 1);

                    for (GEDCOMCalendar I = GEDCOMCalendar.dcGregorian; I <= GEDCOMCalendar.dcLast; I++)
                    {
                        if (GEDCOMDateEscapeArray[(int)I] == su)
                        {
                            fCalendar = I;
                            result = result.Remove(0, su.Length);
                            break;
                        }
                    }
                }
            }

            return result;
        }

        private string ExtractDay(string str)
        {
            if (string.IsNullOrEmpty(str)) return str;

            string result = str;

            int I = 0;
            int num = result.Length;
            while (I < num && SysUtils.IsDigit(result[I]))
            {
                I++;
            }

            if (I >= 1 && I <= 2)
            {
                fDay = (ushort)int.Parse(result.Substring(0, I));
                result = result.Remove(0, I);
            }

            return result;
        }

        private string ExtractDelimiterEx(string str)
        {
            string result = (fDateFormat == GEDCOMDateFormat.dfSystem) ? GEDCOMUtils.ExtractDotDelimiter(str, 0) : GEDCOMUtils.ExtractDelimiter(str, 0);
            return result;
        }

        private string ExtractMonth(string str)
        {
            string result = str;
            if (!string.IsNullOrEmpty(result))
            {
                switch (fCalendar)
                {
                    case GEDCOMCalendar.dcHebrew:
                        {
                            string su = result.Substring(0, 3).ToUpperInvariant();

                            for (int I = 1; I <= GEDCOMMonthHebrewArray.Length; I++)
                            {
                                if (GEDCOMMonthHebrewArray[I - 1] == su)
                                {
                                    fMonth = su;
                                    result = result.Remove(0, 3);
                                    break;
                                }
                            }
                            break;
                        }

                    case GEDCOMCalendar.dcFrench:
                        {
                            string su = result.Substring(0, 4).ToUpperInvariant();

                            for (int I = 1; I <= GEDCOMMonthFrenchArray.Length; I++)
                            {
                                if (GEDCOMMonthFrenchArray[I - 1] == su)
                                {
                                    fMonth = su;
                                    result = result.Remove(0, 4);
                                    break;
                                }
                            }
                            break;
                        }

                    default:
                        {
                            if (!SysUtils.IsDigit(result[0]))
                            {
                                DateTimeFormatInfo dtInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;

                                string su = result.Substring(0, 3).ToUpper();

                                for (int I = 1; I <= GEDCOMMonthArray.Length; I++)
                                {
                                    if (GEDCOMMonthArray[I - 1] == su || dtInfo.AbbreviatedMonthNames[I - 1].ToUpper() == su)
                                    {
                                        fMonth = GEDCOMMonthArray[I - 1];
                                        result = result.Remove(0, 3);
                                        break;
                                    }
                                }
                            }
                            else
                            {
                                string su = result.Substring(0, 3).ToUpper();

                                for (int I = 1; I <= GEDCOMMonthSysArray.Length; I++)
                                {
                                    if (GEDCOMMonthSysArray[I - 1] == su)
                                    {
                                        fMonth = GEDCOMMonthArray[I - 1];
                                        result = result.Remove(0, 2);
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                }
            }
            return result;
        }

        private string ExtractYear(string str)
        {
            if (string.IsNullOrEmpty(str)) return str;

            string result = str;

            int I = 0;
            int num = result.Length;
            while (I < num && SysUtils.IsDigit(result[I]))
            {
                I++;
            }

            if (I > 0)
            {
                fYear = int.Parse(result.Substring(0, I));
                result = result.Remove(0, I);

                if (result != "" && result[0] == '/')
                {
                    result = result.Remove(0, 1);

                    if (result.Length > 0) {
                        int len = (result.Length >= 4) ? 4 : 2;
                        if (!SysUtils.IsDigits(result.Substring(0, len))) {
                            len = (result.Length >= 2) ? 2 : 0;
                            if (!SysUtils.IsDigits(result.Substring(0, len))) {
                                len = 0;
                            }
                        }

                        if (len > 0) {
                            fYearModifier = result.Substring(0, len);
                            result = result.Remove(0, len);
                        }
                    }
                }

                if (result != "" && result.Substring(0, 4).ToUpper() == GEDCOMProvider.GEDCOM_YEAR_BC)
                {
                    fYearBC = true;
                    result = result.Remove(0, 4);
                }
            }

            return result;
        }

        private static string CheckGEDCOMMonth(string str)
        {
            // An empty string is a valid identifier for an unknown month
            if (string.IsNullOrEmpty(str)) return string.Empty;

            if (str != null && str.Length == 3)
            {
                str = str.ToUpperInvariant();

                for (int m = 1; m <= 12; m++)
                {
                    if (GEDCOMMonthArray[m - 1] == str)
                    {
                        return GEDCOMMonthArray[m - 1];
                    }
                }
            }

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid month identifier", str));
        }

        private static string CheckGEDCOMMonthFrench(string str)
        {
            // An empty string is a valid identifier for an unknown month
            if (string.IsNullOrEmpty(str)) return string.Empty;

            if (str != null && str.Length == 4)
            {
                str = str.ToUpperInvariant();

                for (int m = 1; m <= 13; m++)
                {
                    if (GEDCOMMonthFrenchArray[m - 1] == str)
                    {
                        return GEDCOMMonthFrenchArray[m - 1];
                    }
                }
            }

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid French month identifier", str));
        }

        private static string CheckGEDCOMMonthHebrew(string str)
        {
            // An empty string is a valid identifier for an unknown month
            if (string.IsNullOrEmpty(str)) return string.Empty;

            if (str != null && str.Length == 3)
            {
                str = str.ToUpperInvariant();

                for (int m = 1; m <= 13; m++)
                {
                    if (GEDCOMMonthHebrewArray[m - 1] == str)
                    {
                        return GEDCOMMonthHebrewArray[m - 1];
                    }
                }
            }

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid Hebrew month identifier", str));
        }

        private static string IntToGEDCOMMonth(ushort m)
        {
            return (m == 0) ? string.Empty : GEDCOMMonthArray[m - 1];
        }

        private static string IntToGEDCOMMonthFrench(ushort m)
        {
            return (m == 0) ? string.Empty : GEDCOMMonthFrenchArray[m - 1];
        }

        private static string IntToGEDCOMMonthHebrew(ushort m)
        {
            return (m == 0) ? string.Empty : GEDCOMMonthHebrewArray[m - 1];
        }

        private static ushort GEDCOMMonthToInt(string st)
        {
            ushort result = 0;

            if (!string.IsNullOrEmpty(st))
            {
                st = st.ToUpperInvariant();

                for (int m = 1; m <= 12; m++)
                {
                    if (GEDCOMMonthArray[m - 1] == st)
                    {
                        result = (ushort)m;
                        break;
                    }
                }
            }

            return result;
        }

        private static ushort GEDCOMMonthFrenchToInt(string str)
        {
            ushort result = 0;

            if (str != null)
            {
                str = str.ToUpperInvariant();

                for (ushort m = 1; m <= 13; m++)
                {
                    if (GEDCOMMonthFrenchArray[m - 1] == str)
                    {
                        result = m;
                        break;
                    }
                }
            }

            return result;
        }

        private static ushort GEDCOMMonthHebrewToInt(string str)
        {
            ushort result = 0;

            if (str != null)
            {
                str = str.ToUpperInvariant();

                for (ushort m = 1; m <= 13; m++)
                {
                    if (GEDCOMMonthHebrewArray[m - 1] == str)
                    {
                        result = m;
                        break;
                    }
                }
            }

            return result;
        }

        #endregion

        protected override string GetStringValue()
        {
            string result;

            string prefix;
            if (fApproximated == GEDCOMApproximated.daExact) {
                prefix = "";
            } else {
                prefix = GEDCOMDateApproximatedArray[(int)fApproximated];
                prefix += " ";
            }

            if (fCalendar == GEDCOMCalendar.dcGregorian) {
                result = prefix + EscapeString(false, false) + DayString(false) + MonthString(false) + YearGregString(true);
            } else {
                result = prefix + EscapeString(false, false) + DayString(false) + MonthString(false) + YearString(true);
            }

            return result;
        }

        public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
        {
            year = fYear;

            switch (fCalendar) {
                case GEDCOMCalendar.dcHebrew:
                    month = GEDCOMMonthHebrewToInt(fMonth);
                    break;

                case GEDCOMCalendar.dcFrench:
                    month = GEDCOMMonthFrenchToInt(fMonth);
                    break;

                default:
                    month = GEDCOMMonthToInt(fMonth);
                    break;
            }

            day = fDay;
            yearBC = fYearBC;
        }

        public void SetDate(GEDCOMCalendar calendar, ushort day, ushort month, int year)
        {
            switch (calendar) {
                case GEDCOMCalendar.dcGregorian:
                    SetGregorian(day, month, year);
                    break;

                case GEDCOMCalendar.dcJulian:
                    SetJulian(day, month, year);
                    break;

                case GEDCOMCalendar.dcHebrew:
                    SetHebrew(day, month, year);
                    break;

                case GEDCOMCalendar.dcFrench:
                    SetFrench(day, month, year);
                    break;

                case GEDCOMCalendar.dcRoman:
                    //SetRoman(day, month, year);
                    break;

                case GEDCOMCalendar.dcIslamic:
                    //SetIslamic(day, month, year);
                    break;

                case GEDCOMCalendar.dcUnknown:
                    //SetUnknown(day, month, year);
                    break;
            }
        }

        public void SetGregorian(ushort day, ushort month, int year)
        {
            SetGregorian(day, IntToGEDCOMMonth(month), year, "", false);
        }

        public void SetGregorian(ushort day, string month, int year, string yearModifier, bool yearBC)
        {
            fCalendar = GEDCOMCalendar.dcGregorian;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = yearModifier;
            fDay = day;
            fMonth = CheckGEDCOMMonth(month);

            DateChanged();
        }

        public void SetJulian(ushort day, ushort month, int year)
        {
            SetJulian(day, IntToGEDCOMMonth(month), year, false);
        }

        public void SetJulian(ushort day, string month, int year, bool yearBC)
        {
            fCalendar = GEDCOMCalendar.dcJulian;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonth(month);

            DateChanged();
        }

        public void SetHebrew(ushort day, ushort month, int year)
        {
            SetHebrew(day, IntToGEDCOMMonthHebrew(month), year, false);
        }

        public void SetHebrew(ushort day, string month, int year, bool yearBC)
        {
            fCalendar = GEDCOMCalendar.dcHebrew;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonthHebrew(month);

            DateChanged();
        }

        public void SetFrench(ushort day, ushort month, int year)
        {
            SetFrench(day, IntToGEDCOMMonthFrench(month), year, false);
        }

        public void SetFrench(ushort day, string month, int year, bool yearBC)
        {
            fCalendar = GEDCOMCalendar.dcFrench;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonthFrench(month);

            DateChanged();
        }

        public void SetRoman(ushort day, string month, int year, bool yearBC)
        {
            fCalendar = GEDCOMCalendar.dcRoman;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonth(month);

            DateChanged();
        }

        public void SetUnknown(ushort day, string month, int year, bool yearBC)
        {
            fCalendar = GEDCOMCalendar.dcUnknown;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonth(month);

            DateChanged();
        }

        #region UDN processing

        // GEDCOMCalendar { dcGregorian, dcJulian, dcHebrew, dcFrench, dcRoman, dcIslamic, dcUnknown }
        private static readonly UDNCalendarType[] UDNCalendars = new UDNCalendarType[] {
            /* dcGregorian */   UDNCalendarType.ctGregorian,
            /* dcJulian */      UDNCalendarType.ctJulian,
            /* dcHebrew */      UDNCalendarType.ctHebrew,
            /* dcFrench */      UDNCalendarType.ctGregorian, // not supported yet
            /* dcRoman */       UDNCalendarType.ctGregorian, // not supported yet
            /* dcIslamic */     UDNCalendarType.ctIslamic,
            /* dcUnknown */     UDNCalendarType.ctGregorian
        };

        private void DateChanged()
        {
            int year;
            ushort month, day;
            bool yearBC;
            GetDateParts(out year, out month, out day, out yearBC);
            if (yearBC) year = -year;

            UDNCalendarType udnCalendar = UDNCalendars[(int)fCalendar];
            fUDN = new UDN(udnCalendar, year, month, day);
        }

        public override UDN GetUDN()
        {
            return (fApproximated == GEDCOMApproximated.daExact) ? fUDN : UDN.CreateApproximate(fUDN);
        }

        #endregion

        #region Utilities

        public static GEDCOMDate CreateByFormattedStr(string strDate, bool aException)
        {
            return CreateByFormattedStr(strDate, GEDCOMCalendar.dcGregorian, aException);
        }

        /// <summary>
        /// This function transforms the string into a date. All components of
        /// the date's string must be given by numbers in order of day / month / year.
        /// </summary>
        /// <param name="strDate"></param>
        /// <param name="calendar"></param>
        /// <param name="aException"></param>
        /// <returns></returns>
        public static GEDCOMDate CreateByFormattedStr(string dateStr, GEDCOMCalendar calendar, bool aException)
        {
            if (string.IsNullOrEmpty(dateStr)) return null;

            if (dateStr.IndexOf("/") >= 0) dateStr = dateStr.Replace("/", ".");
            if (dateStr.IndexOf("_") >= 0) dateStr = dateStr.Replace("_", " ");

            string[] dtParts = dateStr.Split('.');
            if (dtParts.Length < 3)
            {
                if (aException) {
                    throw new GEDCOMDateException(string.Format("GEDCOMDate.CreateByFormattedStr(): date format is invalid {0}", dateStr));
                }

                return null;
            }

            string pd = dtParts[0].Trim();
            string pm = dtParts[1].Trim();
            string py = dtParts[2].Trim();

            ushort day = (pd == "") ? (ushort)0 : (ushort)SysUtils.ParseInt(pd, 0);
            ushort month = (pm == "") ? (ushort)0 : (ushort)SysUtils.ParseInt(pm, 0);
            int year = (py == "") ? UNKNOWN_YEAR : SysUtils.ParseInt(py, UNKNOWN_YEAR);

            var date = new GEDCOMDate(null, null, "", "");
            date.SetDate(calendar, day, month, year);
            return date;
        }

        public static UDN GetUDNByFormattedStr(string dateStr, GEDCOMCalendar calendar)
        {
            try
            {
                GEDCOMDate dtx = GEDCOMDate.CreateByFormattedStr(dateStr, calendar, false);
                return dtx.GetUDN();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GEDCOMDate.GetUDNByFormattedStr(" + dateStr + "): " + ex.Message);
                return UDN.CreateEmpty();
            }
        }

        #endregion
    }
}
