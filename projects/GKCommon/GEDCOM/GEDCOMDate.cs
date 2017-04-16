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
    /// Note: Year cannot be used externally with negative values even for "BC", because these dates there is a special property.
    /// Dates of type "BC" should have a positive Year + the property YearBC.
    /// </summary>
    public class GEDCOMDate : GEDCOMCustomDate
    {
        private GEDCOMCalendar fDateCalendar;
        private GEDCOMDateFormat fDateFormat;
        private ushort fDay;
        private string fMonth;
        private int fYear;
        private bool fYearBC;
        private string fYearModifier;
        private UDN fUDN;


        public GEDCOMCalendar DateCalendar
        {
            get { return fDateCalendar; }
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


        public GEDCOMDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("DATE");

            fDateCalendar = GEDCOMCalendar.dcGregorian;
            fYear = -1;
            fYearBC = false;
            fYearModifier = "";
            fMonth = "";
            fDay = 0;
            fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
        }

        public override void Clear()
        {
            base.Clear();

            fDateCalendar = GEDCOMCalendar.dcGregorian;
            fYear = -1;
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
            GEDCOMDate date = source as GEDCOMDate;
            if (date != null)
            {
                GEDCOMDate srcDate = date;

                fDateCalendar = srcDate.fDateCalendar;
                fYear = srcDate.fYear;
                fYearBC = srcDate.fYearBC;
                fYearModifier = srcDate.fYearModifier;
                fMonth = srcDate.fMonth;
                fDay = srcDate.fDay;

                DateChanged();
            }
            else
            {
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
            GEDCOMFormat format = (Owner == null) ? GEDCOMFormat.gf_Unknown : Owner.GetGEDCOMFormat();

            fDateCalendar = GEDCOMCalendar.dcGregorian;
            fYear = -1;
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
            if (alwaysShowEscape || fDateCalendar != GEDCOMCalendar.dcGregorian)
            {
                result = GEDCOMDateEscapeArray[(int)fDateCalendar];
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

            if (fYear == -1)
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

            if (fYear == -1)
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
                            fDateCalendar = I;
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
            while (I < num && GEDCOMUtils.IsDigit(result[I]))
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
                switch (fDateCalendar)
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
                            if (!GEDCOMUtils.IsDigit(result[0]))
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
            while (I < num && GEDCOMUtils.IsDigit(result[I]))
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
                        if (!GEDCOMUtils.IsDigits(result.Substring(0, len))) {
                            len = (result.Length >= 2) ? 2 : 0;
                            if (!GEDCOMUtils.IsDigits(result.Substring(0, len))) {
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
            return GEDCOMMonthArray[m - 1];
        }

        private static string IntToGEDCOMMonthFrench(ushort m)
        {
            return GEDCOMMonthFrenchArray[m - 1];
        }

        private static string IntToGEDCOMMonthHebrew(ushort m)
        {
            return GEDCOMMonthHebrewArray[m - 1];
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
            if (fDateCalendar == GEDCOMCalendar.dcGregorian)
            {
                result = EscapeString(false, false) + DayString(false) + MonthString(false) + YearGregString(true);
            }
            else
            {
                result = EscapeString(false, false) + DayString(false) + MonthString(false) + YearString(true);
            }
            return result;
        }

        public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
        {
            year = fYear;

            switch (fDateCalendar) {
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

        public void SetGregorian(ushort day, ushort month, int year)
        {
            SetGregorian(day, IntToGEDCOMMonth(month), year, "", false);
        }

        public void SetGregorian(ushort day, string month, int year, string yearModifier, bool yearBC)
        {
            fDateCalendar = GEDCOMCalendar.dcGregorian;
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
            fDateCalendar = GEDCOMCalendar.dcJulian;
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
            fDateCalendar = GEDCOMCalendar.dcHebrew;
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
            fDateCalendar = GEDCOMCalendar.dcFrench;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonthFrench(month);

            DateChanged();
        }

        public void SetRoman(ushort day, string month, int year, bool yearBC)
        {
            fDateCalendar = GEDCOMCalendar.dcRoman;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = "";
            fDay = day;
            fMonth = CheckGEDCOMMonth(month);

            DateChanged();
        }

        public void SetUnknown(ushort day, string month, int year, bool yearBC)
        {
            fDateCalendar = GEDCOMCalendar.dcUnknown;
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

            UDNCalendarType udnCalendar = UDNCalendars[(int)fDateCalendar];
            fUDN = new UDN(udnCalendar, year, month, day);
        }

        public override UDN GetUDN()
        {
            return fUDN;
        }

        #endregion
    }
}
