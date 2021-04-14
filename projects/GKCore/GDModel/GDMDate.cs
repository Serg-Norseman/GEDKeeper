/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Calendar;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Types;

namespace GDModel
{
    /// <summary>
    /// Class to hold simple standard GEDCOM dates.
    /// Note: Year cannot be used externally with negative values even for "BC",
    /// because these dates there is a special property.
    /// Dates of type "BC" should have a positive Year + the property YearBC.
    /// </summary>
    public class GDMDate : GDMCustomDate
    {
        public const int UNKNOWN_YEAR = -1;

        private GDMApproximated fApproximated;
        private GDMCalendar fCalendar;
        private byte fDay;
        private byte fMonth;
        private short fYear;
        private bool fYearBC;
        private string fYearModifier;
        private UDN fUDN;


        public GDMApproximated Approximated
        {
            get { return fApproximated; }
            set { fApproximated = value; }
        }

        public GDMCalendar DateCalendar
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


        public GDMDate()
        {
            fApproximated = GDMApproximated.daExact;
            fCalendar = GDMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = string.Empty;
            fMonth = 0;
            fDay = 0;
        }

        public GDMDate(int tagId, string tagValue) : this()
        {
            SetNameValue(tagId, tagValue);
        }

        public override void Clear()
        {
            base.Clear();

            fApproximated = GDMApproximated.daExact;
            fCalendar = GDMCalendar.dcGregorian;
            fYear = UNKNOWN_YEAR;
            fYearBC = false;
            fYearModifier = string.Empty;
            fMonth = 0;
            fDay = 0;

            DateChanged();
        }

        /// <summary>
        /// This function is intended only for checking the completeness of parts of the date 
        /// (year, month and day are defined, are not unknown).
        /// </summary>
        public bool IsValidDate()
        {
            return (fYear > 0 && fMonth > 0 && fDay > 0);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fYear <= 0 && fMonth <= 0 && fDay <= 0;
        }

        public override void Assign(GDMTag source)
        {
            GDMDate srcDate = source as GDMDate;
            if (srcDate == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            fApproximated = srcDate.fApproximated;
            fCalendar = srcDate.fCalendar;
            fYear = srcDate.fYear;
            fYearBC = srcDate.fYearBC;
            fYearModifier = srcDate.fYearModifier;
            fMonth = srcDate.fMonth;
            fDay = srcDate.fDay;

            DateChanged();
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
                result = GEDCOMUtils.ParseDate(this, strValue);
            }
            return result;
        }

        /// <summary>
        /// Internal helper method for parser
        /// </summary>
        internal void SetRawData(GDMApproximated approximated, GDMCalendar calendar, 
                                 short year, bool yearBC, string yearModifier, byte month, byte day)
        {
            fApproximated = approximated;
            fCalendar = calendar;
            fYear = year;
            fYearBC = yearBC;
            fYearModifier = yearModifier;
            fMonth = month;
            fDay = day;

            DateChanged();
        }

        #region Private methods of parsing of the input format

        public static string[] GetMonthNames(GDMCalendar calendar)
        {
            string[] monthes;
            switch (calendar)
            {
                case GDMCalendar.dcGregorian:
                case GDMCalendar.dcJulian:
                case GDMCalendar.dcRoman:
                    monthes = GEDCOMConsts.GEDCOMMonthArray;
                    break;

                case GDMCalendar.dcHebrew:
                    monthes = GEDCOMConsts.GEDCOMMonthHebrewArray;
                    break;

                case GDMCalendar.dcFrench:
                    monthes = GEDCOMConsts.GEDCOMMonthFrenchArray;
                    break;

                case GDMCalendar.dcIslamic:
                    monthes = GEDCOMConsts.GEDCOMMonthIslamicArray;
                    break;

                case GDMCalendar.dcUnknown:
                default:
                    monthes = GEDCOMConsts.GEDCOMMonthArray;
                    break;
            }
            return monthes;
        }

        private static string CheckGEDCOMMonth(GDMCalendar calendar, string str)
        {
            // An empty string is a valid identifier for an unknown month
            if (string.IsNullOrEmpty(str)) return string.Empty;

            string[] monthes = GDMDate.GetMonthNames(calendar);
            str = str.ToUpperInvariant();
            for (int m = 0; m < monthes.Length; m++) {
                if (monthes[m] == str) {
                    return str;
                }
            }

            throw new GDMDateException("The string {0} is not a valid {1} month identifier", str, calendar.ToString());
        }

        private static string IntToGEDCOMMonth(int m)
        {
            return (m == 0) ? string.Empty : GEDCOMConsts.GEDCOMMonthArray[m - 1];
        }

        private static string IntToGEDCOMMonthFrench(int m)
        {
            return (m == 0) ? string.Empty : GEDCOMConsts.GEDCOMMonthFrenchArray[m - 1];
        }

        private static string IntToGEDCOMMonthHebrew(int m)
        {
            return (m == 0) ? string.Empty : GEDCOMConsts.GEDCOMMonthHebrewArray[m - 1];
        }

        #endregion

        protected override string GetStringValue()
        {
            var parts = new List<string>(5);
            if (fApproximated != GDMApproximated.daExact) {
                parts.Add(GEDCOMConsts.GEDCOMDateApproximatedArray[(int)fApproximated]);
            }

            if (fCalendar != GDMCalendar.dcGregorian) {
                parts.Add(GEDCOMConsts.GEDCOMDateEscapeArray[(int)fCalendar]);
            }

            if (fDay > 0) {
                parts.Add(fDay.ToString("D2"));
            }

            if (fMonth > 0) {
                string[] months = GetMonthNames(fCalendar);
                parts.Add(months[fMonth - 1]);
            }

            if (fYear != UNKNOWN_YEAR) {
                string yearStr = fYear.ToString("D3");
                if (!string.IsNullOrEmpty(fYearModifier)) {
                    yearStr = yearStr + "/" + fYearModifier;
                }

                if (fYearBC) {
                    yearStr += GEDCOMConsts.YearBC;
                }

                parts.Add(yearStr);
            }

            return string.Join(" ", parts);
        }

        private static byte GetMonthNumber(GDMCalendar calendar, string strMonth)
        {
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(strMonth);

            int month;
            switch (calendar) {
                case GDMCalendar.dcHebrew:
                    month = Algorithms.IndexOf(GEDCOMConsts.GEDCOMMonthHebrewArray, su);
                    break;

                case GDMCalendar.dcFrench:
                    month = Algorithms.IndexOf(GEDCOMConsts.GEDCOMMonthFrenchArray, su);
                    break;

                default:
                    month = Algorithms.IndexOf(GEDCOMConsts.GEDCOMMonthArray, su);
                    break;
            }

            return (byte)(month + 1);
        }

        public void SetDate(GDMCalendar calendar, int day, int month, int year, bool yearBC = false)
        {
            switch (calendar) {
                case GDMCalendar.dcGregorian:
                    SetGregorian(day, month, year);
                    break;

                case GDMCalendar.dcJulian:
                    SetJulian(day, month, year);
                    break;

                case GDMCalendar.dcHebrew:
                    SetHebrew(day, month, year);
                    break;

                case GDMCalendar.dcFrench:
                    SetFrench(day, month, year);
                    break;

                case GDMCalendar.dcRoman:
                    SetRoman(day, month, year, yearBC);
                    break;

                case GDMCalendar.dcIslamic:
                    SetIslamic(day, month, year);
                    break;

                case GDMCalendar.dcUnknown:
                    SetUnknown(day, month, year, yearBC);
                    break;
            }
        }

        private void SetDateInternal(GDMCalendar calendar, int day, string month, int year, string yearModifier, bool yearBC)
        {
            SetDateInternal(calendar, day, GetMonthNumber(calendar, month), year, yearModifier, yearBC);
        }

        private void SetDateInternal(GDMCalendar calendar, int day, int month, int year, string yearModifier, bool yearBC)
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
            SetDateInternal(GDMCalendar.dcGregorian, day, month, year, "", false);
        }

        public void SetGregorian(int day, string month, int year, string yearModifier, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcGregorian, day, CheckGEDCOMMonth(GDMCalendar.dcGregorian, month), year, yearModifier, yearBC);
        }

        public void SetJulian(int day, int month, int year)
        {
            SetDateInternal(GDMCalendar.dcJulian, day, month, year, "", false);
        }

        public void SetJulian(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcJulian, day, CheckGEDCOMMonth(GDMCalendar.dcJulian, month), year, "", yearBC);
        }

        public void SetHebrew(int day, int month, int year)
        {
            SetDateInternal(GDMCalendar.dcHebrew, day, month, year, "", false);
        }

        public void SetHebrew(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcHebrew, day, CheckGEDCOMMonth(GDMCalendar.dcHebrew, month), year, "", yearBC);
        }

        public void SetFrench(int day, int month, int year)
        {
            SetDateInternal(GDMCalendar.dcFrench, day, month, year, "", false);
        }

        public void SetFrench(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcFrench, day, CheckGEDCOMMonth(GDMCalendar.dcFrench, month), year, "", yearBC);
        }

        public void SetRoman(int day, int month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcRoman, day, month, year, "", yearBC);
        }

        public void SetRoman(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcRoman, day, CheckGEDCOMMonth(GDMCalendar.dcRoman, month), year, "", yearBC);
        }

        public void SetUnknown(int day, int month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcUnknown, day, month, year, "", yearBC);
        }

        public void SetUnknown(int day, string month, int year, bool yearBC)
        {
            SetDateInternal(GDMCalendar.dcUnknown, day, CheckGEDCOMMonth(GDMCalendar.dcUnknown, month), year, "", yearBC);
        }

        public void SetIslamic(int day, int month, int year)
        {
            SetDateInternal(GDMCalendar.dcIslamic, day, month, year, "", false);
        }

        public void SetIslamic(int day, string month, int year)
        {
            SetDateInternal(GDMCalendar.dcIslamic, day, CheckGEDCOMMonth(GDMCalendar.dcIslamic, month), year, "", false);
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
            return (fApproximated == GDMApproximated.daExact) ? fUDN : UDN.CreateApproximate(fUDN);
        }

        #endregion

        #region Utilities

        public static GDMDate CreateByFormattedStr(string strDate, bool aException)
        {
            return CreateByFormattedStr(strDate, GDMCalendar.dcGregorian, aException);
        }

        /// <summary>
        /// This function transforms the string into a date. All components of
        /// the date's string must be given by numbers in order of day / month / year.
        /// This function is intended only for use with the date entry controls (fixed format of date's string).
        /// </summary>
        public static GDMDate CreateByFormattedStr(string dateStr, GDMCalendar calendar, bool aException)
        {
            if (string.IsNullOrEmpty(dateStr)) return null;

            if (dateStr.IndexOf("-") >= 0) dateStr = dateStr.Replace("-", ".");
            if (dateStr.IndexOf("/") >= 0) dateStr = dateStr.Replace("/", ".");
            if (dateStr.IndexOf("_") >= 0) dateStr = dateStr.Replace("_", " ");

            string[] dtParts = dateStr.Split('.');
            if (dtParts.Length < 3) {
                if (aException) {
                    throw new GDMDateException("Invalid date format '{0}'", dateStr);
                }

                return null;
            }

            string pd = dtParts[0].Trim();
            string pm = dtParts[1].Trim();
            string py = dtParts[2].Trim();

            int day = (pd == "") ? 0 : ConvertHelper.ParseInt(pd, 0);
            int month = (pm == "") ? 0 : ConvertHelper.ParseInt(pm, 0);
            int year = (py == "") ? UNKNOWN_YEAR : ConvertHelper.ParseInt(py, UNKNOWN_YEAR);

            var date = new GDMDate();
            date.SetDate(calendar, day, month, year);
            return date;
        }

        public static UDN GetUDNByFormattedStr(string dateStr, GDMCalendar calendar, bool aException = false)
        {
            GDMDate dtx = GDMDate.CreateByFormattedStr(dateStr, calendar, aException);
            return (dtx != null) ? dtx.GetUDN() : UDN.CreateEmpty();
        }

        public string GetDisplayString(DateFormat format, bool includeBC = false, bool showCalendar = false)
        {
            string result = "";

            int year = fYear;
            int month = fMonth;
            int day = fDay;
            bool ybc = fYearBC;

            if (year > 0 || month > 0 || day > 0) {
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

            if (includeBC && ybc) {
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

            if (showCalendar) {
                result = result + GKData.DateCalendars[(int)fCalendar].Sign;
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar)
        {
            string result = "";

            result = GetDisplayString(format, true, showCalendar);
            if (sign && fApproximated != GDMApproximated.daExact) {
                result = "~ " + result;
            }

            return result;
        }

        #endregion
    }
}
