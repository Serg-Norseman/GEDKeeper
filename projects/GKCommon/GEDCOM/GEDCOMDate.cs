/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
            get { return this.fDateCalendar; }
        }

        public ushort Day
        {
            get { return this.fDay; }
            set {
                this.fDay = value;
                this.DateChanged();
            }
        }

        public string Month
        {
            get { return this.fMonth; }
            set {
                this.fMonth = value;
                this.DateChanged();
            }
        }

        public int Year
        {
            get { return this.fYear; }
            set {
                this.fYear = value;
                this.DateChanged();
            }
        }

        public bool YearBC
        {
            get { return this.fYearBC; }
            set {
                this.fYearBC = value;
                this.DateChanged();
            }
        }

        public string YearModifier
        {
            get { return this.fYearModifier; }
            set { this.fYearModifier = value; }
        }


        public GEDCOMDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetName("DATE");

            this.fDateCalendar = GEDCOMCalendar.dcGregorian;
            this.fYear = -1;
            this.fYearBC = false;
            this.fYearModifier = "";
            this.fMonth = "";
            this.fDay = 0;
            this.fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
        }

        public override void Clear()
        {
            base.Clear();

            this.fDateCalendar = GEDCOMCalendar.dcGregorian;
            this.fYear = -1;
            this.fYearBC = false;
            this.fYearModifier = "";
            this.fMonth = "";
            this.fDay = 0;

            this.DateChanged();
        }

        public bool IsValidDate()
        {
            return (this.fYear > 0 && this.fMonth != "" && this.fDay > 0);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fYear <= 0 && this.fMonth == "" && this.fDay <= 0;
        }

        public override void Assign(GEDCOMTag source)
        {
            if (source is GEDCOMDate)
            {
                GEDCOMDate srcDate = (source as GEDCOMDate);

                this.fDateCalendar = srcDate.fDateCalendar;
                this.fYear = srcDate.fYear;
                this.fYearBC = srcDate.fYearBC;
                this.fYearModifier = srcDate.fYearModifier;
                this.fMonth = srcDate.fMonth;
                this.fDay = srcDate.fDay;

                this.DateChanged();
            }
            else
            {
                base.Assign(source);
            }
        }

        public override DateTime GetDateTime()
        {
            DateTime result;

            ushort month = GEDCOMMonthToInt(this.fMonth);
            ushort day = this.fDay;
            if (this.fYear >= 0 && month >= 1 && month <= 12 && day >= 1 && day < 32)
            {
                result = new DateTime(this.fYear, month, day);
                return result;
            }

            result = new DateTime(0);
            return result;
        }

        public override void SetDateTime(DateTime value)
        {
            this.SetGregorian((ushort)value.Day, GEDCOMMonthArray[value.Month - 1], value.Year, "", false);
        }

        public override string ParseString(string strValue)
        {
            GEDCOMFormat format = (Owner == null) ? GEDCOMFormat.gf_Unknown : Owner.GetGEDCOMFormat();

            this.fDateCalendar = GEDCOMCalendar.dcGregorian;
            this.fYear = -1;
            this.fYearBC = false;
            this.fYearModifier = "";
            this.fMonth = "";
            this.fDay = 0;

            string result = strValue;

            if (!string.IsNullOrEmpty(result))
            {
                if (format == GEDCOMFormat.gf_Ahnenblatt) {
                    result = PrepareAhnenblattDate(result);
                }

                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = this.ExtractEscape(result);
                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = this.ExtractDay(result);

                if (result.Length > 0)
                {
                    if (result[0] == ' ')
                    {
                        this.fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
                    }
                    else
                    {
                        if (result[0] == '.')
                        {
                            this.fDateFormat = GEDCOMDateFormat.dfSystem;
                        }
                    }
                }

                result = this.ExtractDelimiterEx(result);
                result = this.ExtractMonth(result);
                result = this.ExtractDelimiterEx(result);
                result = this.ExtractYear(result);
            }

            this.DateChanged();

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

            if (this.fDay <= 0)
            {
                result = "";
            }
            else
            {
                result = this.fDay.ToString();
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
            if (alwaysShowEscape || this.fDateCalendar != GEDCOMCalendar.dcGregorian)
            {
                result = GEDCOMDateEscapeArray[(int)this.fDateCalendar];
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
            if (this.fMonth == "")
            {
                result = "";
            }
            else
            {
                result = this.fMonth;
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

            if (this.fYear == -1)
            {
                result = "";
            }
            else
            {
                result = this.fYear.ToString();
                if (this.fYearModifier != "")
                {
                    result = result + "/" + this.fYearModifier;
                }
                if (this.fYearBC)
                {
                    result += GEDCOM_YEAR_BC;
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

            if (this.fYear == -1)
            {
                result = "";
            }
            else
            {
                result = this.fYear.ToString();
                if (this.fYearBC)
                {
                    result += GEDCOM_YEAR_BC;
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
                            this.fDateCalendar = I;
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
                this.fDay = (ushort)int.Parse(result.Substring(0, I));
                result = result.Remove(0, I);
            }

            return result;
        }

        private string ExtractDelimiterEx(string str)
        {
            string result = (this.fDateFormat == GEDCOMDateFormat.dfSystem) ? GEDCOMUtils.ExtractDotDelimiter(str, 0) : GEDCOMUtils.ExtractDelimiter(str, 0);
            return result;
        }

        private string ExtractMonth(string str)
        {
            string result = str;
            if (!string.IsNullOrEmpty(result))
            {
                switch (this.fDateCalendar)
                {
                    case GEDCOMCalendar.dcHebrew:
                        {
                            string su = result.Substring(0, 3).ToUpperInvariant();

                            for (int I = 1; I <= GEDCOMMonthHebrewArray.Length; I++)
                            {
                                if (GEDCOMMonthHebrewArray[I - 1] == su)
                                {
                                    this.fMonth = su;
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
                                    this.fMonth = su;
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
                                        this.fMonth = GEDCOMMonthArray[I - 1];
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
                                        this.fMonth = GEDCOMMonthArray[I - 1];
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
                this.fYear = int.Parse(result.Substring(0, I));
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
                            this.fYearModifier = result.Substring(0, len);
                            result = result.Remove(0, len);
                        }
                    }
                }

                if (result != "" && result.Substring(0, 4).ToUpper() == GEDCOM_YEAR_BC)
                {
                    this.fYearBC = true;
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
            if (this.fDateCalendar == GEDCOMCalendar.dcGregorian)
            {
                result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearGregString(true);
            }
            else
            {
                result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearString(true);
            }
            return result;
        }

        public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
        {
            year = this.fYear;

            switch (this.fDateCalendar) {
                case GEDCOMCalendar.dcHebrew:
                    month = GEDCOMMonthHebrewToInt(this.fMonth);
                    break;

                case GEDCOMCalendar.dcFrench:
                    month = GEDCOMMonthFrenchToInt(this.fMonth);
                    break;

                default:
                    month = GEDCOMMonthToInt(this.fMonth);
                    break;
            }

            day = this.fDay;
            yearBC = this.fYearBC;
        }

        public void SetGregorian(ushort day, ushort month, int year)
        {
            this.SetGregorian(day, IntToGEDCOMMonth(month), year, "", false);
        }

        public void SetGregorian(ushort day, string month, int year, string yearModifier, bool yearBC)
        {
            this.fDateCalendar = GEDCOMCalendar.dcGregorian;
            this.fYear = year;
            this.fYearBC = yearBC;
            this.fYearModifier = yearModifier;
            this.fDay = day;
            this.fMonth = CheckGEDCOMMonth(month);

            this.DateChanged();
        }

        public void SetJulian(ushort day, ushort month, int year)
        {
            this.SetJulian(day, IntToGEDCOMMonth(month), year, false);
        }

        public void SetJulian(ushort day, string month, int year, bool yearBC)
        {
            this.fDateCalendar = GEDCOMCalendar.dcJulian;
            this.fYear = year;
            this.fYearBC = yearBC;
            this.fYearModifier = "";
            this.fDay = day;
            this.fMonth = CheckGEDCOMMonth(month);

            this.DateChanged();
        }

        public void SetHebrew(ushort day, ushort month, int year)
        {
            this.SetHebrew(day, IntToGEDCOMMonthHebrew(month), year, false);
        }

        public void SetHebrew(ushort day, string month, int year, bool yearBC)
        {
            this.fDateCalendar = GEDCOMCalendar.dcHebrew;
            this.fYear = year;
            this.fYearBC = yearBC;
            this.fYearModifier = "";
            this.fDay = day;
            this.fMonth = CheckGEDCOMMonthHebrew(month);

            this.DateChanged();
        }

        public void SetFrench(ushort day, ushort month, int year)
        {
            this.SetFrench(day, IntToGEDCOMMonthFrench(month), year, false);
        }

        public void SetFrench(ushort day, string month, int year, bool yearBC)
        {
            this.fDateCalendar = GEDCOMCalendar.dcFrench;
            this.fYear = year;
            this.fYearBC = yearBC;
            this.fYearModifier = "";
            this.fDay = day;
            this.fMonth = CheckGEDCOMMonthFrench(month);

            this.DateChanged();
        }

        public void SetRoman(ushort day, string month, int year, bool yearBC)
        {
            this.fDateCalendar = GEDCOMCalendar.dcRoman;
            this.fYear = year;
            this.fYearBC = yearBC;
            this.fYearModifier = "";
            this.fDay = day;
            this.fMonth = CheckGEDCOMMonth(month);

            this.DateChanged();
        }

        public void SetUnknown(ushort day, string month, int year, bool yearBC)
        {
            this.fDateCalendar = GEDCOMCalendar.dcUnknown;
            this.fYear = year;
            this.fYearBC = yearBC;
            this.fYearModifier = "";
            this.fDay = day;
            this.fMonth = CheckGEDCOMMonth(month);

            this.DateChanged();
        }

        #region UDN processing

        // GEDCOMCalendar { dcGregorian, dcJulian, dcHebrew, dcFrench, dcRoman, dcIslamic, dcUnknown }
        private static UDNCalendarType[] UDNCalendars = new UDNCalendarType[] {
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
            this.GetDateParts(out year, out month, out day, out yearBC);
            if (yearBC) year = -year;

            UDNCalendarType udnCalendar = UDNCalendars[(int)this.fDateCalendar];
            this.fUDN = new UDN(udnCalendar, year, month, day);
        }

        public override UDN GetUDN()
        {
            return this.fUDN;
        }

        #endregion
    }
}
