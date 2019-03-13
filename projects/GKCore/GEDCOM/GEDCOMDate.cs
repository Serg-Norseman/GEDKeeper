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
using BSLib;
using BSLib.Calendar;
using GKCore;
using GKCore.Types;

namespace GKCommon.GEDCOM
{
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
        private byte fDay;
        private byte fMonth;
        private short fYear;
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

        public byte Day
        {
            get { return fDay; }
            set {
                fDay = value;
                DateChanged();
            }
        }

        public byte Month
        {
            get { return fMonth; }
            set {
                fMonth = value;
                DateChanged();
            }
        }

        public short Year
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

        public GEDCOMDate(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            fApproximated = GEDCOMApproximated.daExact;
            fCalendar = GEDCOMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = string.Empty;
            fMonth = 0;
            fDay = 0;
            fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
        }

        public GEDCOMDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : this(owner, parent)
        {
            SetNameValue(tagName, tagValue);
        }

        public override void Clear()
        {
            base.Clear();

            fApproximated = GEDCOMApproximated.daExact;
            fCalendar = GEDCOMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = "";
            fMonth = 0;
            fDay = 0;

            DateChanged();
        }

        public bool IsValidDate()
        {
            return (fYear > 0 && fMonth > 0 && fDay > 0);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fYear <= 0 && fMonth <= 0 && fDay <= 0;
        }

        public override void Assign(GEDCOMTag source)
        {
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

            // FIXME: check if the calendar is gregorian
            if (fYear >= 0 && fMonth >= 1 && fMonth <= 12 && fDay >= 1 && fDay <= 31) {
                result = new DateTime(fYear, fMonth, fDay);
                return result;
            }

            result = new DateTime(0);
            return result;
        }

        public override void SetDateTime(DateTime value)
        {
            SetGregorian(value.Day, value.Month, value.Year);
        }

        public override string ParseString(string strValue)
        {
            string result;
            if (string.IsNullOrEmpty(strValue)) {
                Clear();
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseDate(strValue, Owner, this);
            }
            return result;
        }

        /// <summary>
        /// Internal helper method for parser
        /// </summary>
        internal void SetRawData(GEDCOMApproximated approximated, GEDCOMCalendar calendar, 
                                 short year, bool yearBC, string yearModifier, byte month, byte day,
                                 GEDCOMDateFormat dateFormat)
        {
            fApproximated = approximated;
            fCalendar = calendar;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = yearModifier;
            fMonth = month;
            fDay = day;
            fDateFormat = dateFormat;

            DateChanged();
        }

        #region Private methods of parsing of the input format

        // FIXME
        public static string[] GetMonthNames(GEDCOMCalendar calendar)
        {
            string[] monthes;
            switch (calendar)
            {
                case GEDCOMCalendar.dcGregorian:
                case GEDCOMCalendar.dcJulian:
                case GEDCOMCalendar.dcRoman:
                    monthes = GEDCOMMonthArray;
                    break;

                case GEDCOMCalendar.dcHebrew:
                    monthes = GEDCOMMonthHebrewArray;
                    break;

                case GEDCOMCalendar.dcFrench:
                    monthes = GEDCOMMonthFrenchArray;
                    break;

                case GEDCOMCalendar.dcIslamic:
                    monthes = GEDCOMMonthArray;
                    break;

                case GEDCOMCalendar.dcUnknown:
                default:
                    monthes = GEDCOMMonthArray;
                    break;
            }
            return monthes;
        }

        private static string CheckGEDCOMMonth(string str)
        {
            // An empty string is a valid identifier for an unknown month
            if (string.IsNullOrEmpty(str)) return string.Empty;

            if (str.Length == 3)
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

            if (str.Length == 4)
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

            if (str.Length == 3)
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

        private static string IntToGEDCOMMonth(int m)
        {
            return (m == 0) ? string.Empty : GEDCOMMonthArray[m - 1];
        }

        private static string IntToGEDCOMMonthFrench(int m)
        {
            return (m == 0) ? string.Empty : GEDCOMMonthFrenchArray[m - 1];
        }

        private static string IntToGEDCOMMonthHebrew(int m)
        {
            return (m == 0) ? string.Empty : GEDCOMMonthHebrewArray[m - 1];
        }

        #endregion

        protected override string GetStringValue()
        {
            string prefix = string.Empty;
            if (fApproximated != GEDCOMApproximated.daExact) {
                prefix = GEDCOMDateApproximatedArray[(int)fApproximated] + " ";
            }

            string escapeStr = string.Empty;
            if (fCalendar != GEDCOMCalendar.dcGregorian) {
                escapeStr = GEDCOMDateEscapeArray[(int)fCalendar] + " ";
            }

            string dayStr = string.Empty;
            if (fDay > 0) {
                dayStr = fDay.ToString();
                if (dayStr.Length == 1) {
                    dayStr = "0" + dayStr;
                }
                dayStr += " ";
            }

            string monthStr = string.Empty;
            if (fMonth > 0) {
                string[] monthes = GEDCOMDate.GetMonthNames(fCalendar);
                monthStr = monthes[fMonth - 1] + " ";
            }

            string yearStr = string.Empty;
            if (fYear != UNKNOWN_YEAR) {
                yearStr = fYear.ToString();
                if (fYearModifier != "") {
                    yearStr = yearStr + "/" + fYearModifier;
                }
                if (fYearBC) {
                    yearStr += GEDCOMProvider.GEDCOM_YEAR_BC;
                }
            }

            string result = prefix + escapeStr + dayStr + monthStr + yearStr;
            return result;
        }

        private static byte GetMonthNumber(GEDCOMCalendar calendar, string strMonth)
        {
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(strMonth);

            int month;
            switch (calendar) {
                case GEDCOMCalendar.dcHebrew:
                    month = Algorithms.IndexOf(GEDCOMMonthHebrewArray, su);
                    break;

                case GEDCOMCalendar.dcFrench:
                    month = Algorithms.IndexOf(GEDCOMMonthFrenchArray, su);
                    break;

                default:
                    month = Algorithms.IndexOf(GEDCOMMonthArray, su);
                    break;
            }

            return (byte)(month + 1);
        }

        public void SetDate(GEDCOMCalendar calendar, int day, int month, int year, bool yearBC = false)
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
                    SetRoman(day, month, year, yearBC);
                    break;

                case GEDCOMCalendar.dcIslamic:
                    SetIslamic(day, month, year);
                    break;

                case GEDCOMCalendar.dcUnknown:
                    SetUnknown(day, month, year, yearBC);
                    break;
            }
        }

        private void SetDateInternal(GEDCOMCalendar calendar, int day, string month, int year, string yearModifier, bool yearBC)
        {
            SetDateInternal(calendar, day, GetMonthNumber(calendar, month), year, yearModifier, yearBC);
        }

        private void SetDateInternal(GEDCOMCalendar calendar, int day, int month, int year, string yearModifier, bool yearBC)
        {
            fCalendar = calendar;
            fDay = (byte)day;
            fMonth = (byte)month;
            fYear = (short)year;
            fYearModifier = yearModifier;
            fYearBC = yearBC;

            DateChanged();
        }

        public void SetGregorian(int day, int month, int year)
        {
            SetDateInternal(GEDCOMCalendar.dcGregorian, day, month, year, "", false);
        }

        public void SetGregorian(int day, string month, int year, string yearModifier, bool yearBC)
        {
            SetDateInternal(GEDCOMCalendar.dcGregorian, day, CheckGEDCOMMonth(month), year, yearModifier, yearBC);
        }

        public void SetJulian(int day, int month, int year)
        {
            SetJulian(day, IntToGEDCOMMonth(month), year, false);
        }

        public void SetJulian(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GEDCOMCalendar.dcJulian, day, CheckGEDCOMMonth(month), year, "", yearBC);
        }

        public void SetHebrew(int day, int month, int year)
        {
            SetHebrew(day, IntToGEDCOMMonthHebrew(month), year, false);
        }

        public void SetHebrew(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GEDCOMCalendar.dcHebrew, day, CheckGEDCOMMonthHebrew(month), year, "", yearBC);
        }

        public void SetFrench(int day, int month, int year)
        {
            SetFrench(day, IntToGEDCOMMonthFrench(month), year, false);
        }

        public void SetFrench(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GEDCOMCalendar.dcFrench, day, CheckGEDCOMMonthFrench(month), year, "", yearBC);
        }

        // TODO: not implemented yet
        public void SetRoman(int day, int month, int year, bool yearBC)
        {
            SetRoman(day, IntToGEDCOMMonth(month), year, yearBC);
        }

        // TODO: not implemented yet
        public void SetRoman(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GEDCOMCalendar.dcRoman, day, CheckGEDCOMMonth(month), year, "", yearBC);
        }

        // TODO: not implemented yet
        public void SetUnknown(int day, int month, int year, bool yearBC)
        {
            SetUnknown(day, IntToGEDCOMMonth(month), year, yearBC);
        }

        // TODO: not implemented yet
        public void SetUnknown(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GEDCOMCalendar.dcUnknown, day, CheckGEDCOMMonth(month), year, "", yearBC);
        }

        // TODO: not implemented yet
        public void SetIslamic(int day, int month, int year)
        {
            SetIslamic(day, IntToGEDCOMMonth(month), year);
        }

        // TODO: not implemented yet
        public void SetIslamic(int day, string month, int year)
        {
            SetDateInternal(GEDCOMCalendar.dcIslamic, day, CheckGEDCOMMonth(month), year, "", false);
        }

        #region UDN processing

        // GEDCOMCalendar: dcGregorian, dcJulian, dcHebrew, dcFrench, dcRoman, dcIslamic, dcUnknown.
        private static readonly UDNCalendarType[] UDNCalendars = new UDNCalendarType[] {
            /* dcGregorian */   UDNCalendarType.ctGregorian,
            /* dcJulian */      UDNCalendarType.ctJulian,
            /* dcHebrew */      UDNCalendarType.ctHebrew,
            /* dcFrench */      UDNCalendarType.ctGregorian, // not supported yet
            /* dcRoman */       UDNCalendarType.ctGregorian, // not supported yet
            /* dcIslamic */     UDNCalendarType.ctIslamic,
            /* dcUnknown */     UDNCalendarType.ctGregorian
        };

        protected override void DateChanged()
        {
            int year = fYear;
            if (year == UNKNOWN_YEAR) {
                year = UDN.UnknownYear;
            } else {
                if (fYearBC) year = -year;
            }

            UDNCalendarType udnCalendar = UDNCalendars[(int)fCalendar];
            fUDN = new UDN(udnCalendar, year, fMonth, fDay);
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

            if (dateStr.IndexOf("-") >= 0) dateStr = dateStr.Replace("-", ".");
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

            int day = (pd == "") ? 0 : ConvertHelper.ParseInt(pd, 0);
            int month = (pm == "") ? 0 : ConvertHelper.ParseInt(pm, 0);
            int year = (py == "") ? UNKNOWN_YEAR : ConvertHelper.ParseInt(py, UNKNOWN_YEAR);

            var date = new GEDCOMDate(null, null);
            date.SetDate(calendar, day, month, year);
            return date;
        }

        public static UDN GetUDNByFormattedStr(string dateStr, GEDCOMCalendar calendar, bool aException = false)
        {
            GEDCOMDate dtx = GEDCOMDate.CreateByFormattedStr(dateStr, calendar, aException);
            return (dtx != null) ? dtx.GetUDN() : UDN.CreateEmpty();
        }

        public string GetDisplayString(DateFormat format, bool includeBC = false, bool showCalendar = false)
        {
            string result = "";

            int year = fYear;
            int month = fMonth;
            int day = fDay;
            bool ybc = fYearBC;

            if (year > 0 || month > 0 || day > 0)
            {
                switch (format) {
                    case DateFormat.dfDD_MM_YYYY:
                        result += day > 0 ? ConvertHelper.AdjustNumber(day, 2) + "." : "__.";
                        result += month > 0 ? ConvertHelper.AdjustNumber(month, 2) + "." : "__.";
                        result += year > 0 ? year.ToString().PadLeft(4, '_') : "____";
                        break;

                    case DateFormat.dfYYYY_MM_DD:
                        result += year > 0 ? year.ToString().PadLeft(4, '_') + "." : "____.";
                        result += month > 0 ? ConvertHelper.AdjustNumber(month, 2) + "." : "__.";
                        result += day > 0 ? ConvertHelper.AdjustNumber(day, 2) : "__";
                        break;

                    case DateFormat.dfYYYY:
                        if (year > 0) {
                            result = year.ToString().PadLeft(4, '_');
                        }
                        break;
                }
            }

            if (includeBC && ybc)
            {
                switch (format) {
                    case DateFormat.dfDD_MM_YYYY:
                        result = result + " BC";
                        break;
                    case DateFormat.dfYYYY_MM_DD:
                        result = "BC " + result;
                        break;
                    case DateFormat.dfYYYY:
                        result = "BC " + result;
                        break;
                }
            }

            if (showCalendar)
            {
                result = result + GKData.DateCalendars[(int)fCalendar].Sign;
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar)
        {
            string result = "";

            result = GetDisplayString(format, true, showCalendar);
            if (sign && fApproximated != GEDCOMApproximated.daExact) {
                result = "~ " + result;
            }

            return result;
        }

        #endregion
    }
}
