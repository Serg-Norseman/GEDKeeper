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
using BSLib;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Calendar;
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
        public static readonly GDMDate Empty = new GDMDate();

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

        public GDMDate(int tagId) : this()
        {
            SetName(tagId);
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
            switch (calendar) {
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

        #endregion

        protected override string GetStringValue()
        {
            var parts = new string[5];
            int pIdx = 0;
            if (fApproximated != GDMApproximated.daExact) {
                parts[pIdx++] = GEDCOMConsts.GEDCOMDateApproximatedArray[(int)fApproximated];
            }

            if (fCalendar != GDMCalendar.dcGregorian) {
                parts[pIdx++] = GEDCOMConsts.GEDCOMDateEscapeArray[(int)fCalendar];
            }

            if (fDay > 0) {
                parts[pIdx++] = fDay.ToString("D2");
            }

            if (fMonth > 0) {
                string[] months = GetMonthNames(fCalendar);
                parts[pIdx++] = months[fMonth - 1];
            }

            if (fYear != UNKNOWN_YEAR) {
                string yearStr = fYear.ToString("D3");
                if (!string.IsNullOrEmpty(fYearModifier)) {
                    yearStr = yearStr + "/" + fYearModifier;
                }

                if (fYearBC) {
                    yearStr += GEDCOMConsts.YearBC;
                }

                parts[pIdx++] = yearStr;
            }

            return string.Join(" ", parts, 0, pIdx);
        }

        private static byte GetMonthNumber(GDMCalendar calendar, string strMonth)
        {
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(strMonth);

            int month;
            switch (calendar) {
                case GDMCalendar.dcHebrew:
                    month = ArrayHelper.IndexOf(GEDCOMConsts.GEDCOMMonthHebrewArray, su);
                    break;

                case GDMCalendar.dcFrench:
                    month = ArrayHelper.IndexOf(GEDCOMConsts.GEDCOMMonthFrenchArray, su);
                    break;

                default:
                    month = ArrayHelper.IndexOf(GEDCOMConsts.GEDCOMMonthArray, su);
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
            return (dtx != null) ? dtx.GetUDN() : UDN.Unknown;
        }

        public string GetDisplayString(DateFormat format, bool includeBC = false, bool showCalendar = false)
        {
            var parts = new string[5];
            int pIdx = 0;

            int year = fYear;
            int month = fMonth;
            int day = fDay;
            bool ybc = fYearBC;

            if (year > 0 || month > 0 || day > 0) {
                switch (format) {
                    case DateFormat.dfDD_MM_YYYY:
                        parts[pIdx++] = day > 0 ? day.ToString("D2", null) + "." : "__.";
                        parts[pIdx++] = month > 0 ? month.ToString("D2", null) + "." : "__.";
                        parts[pIdx++] = year > 0 ? year.ToString().PadLeft(4, '_') : "____";
                        if (includeBC && ybc) {
                            parts[pIdx++] = " BC";
                        }
                        break;

                    case DateFormat.dfYYYY_MM_DD:
                        if (includeBC && ybc) {
                            parts[pIdx++] = "BC ";
                        }
                        parts[pIdx++] = year > 0 ? year.ToString().PadLeft(4, '_') + "." : "____.";
                        parts[pIdx++] = month > 0 ? month.ToString("D2", null) + "." : "__.";
                        parts[pIdx++] = day > 0 ? day.ToString("D2", null) : "__";
                        break;

                    case DateFormat.dfYYYY:
                        if (year > 0) {
                            if (includeBC && ybc) {
                                parts[pIdx++] = "BC ";
                            }
                            parts[pIdx++] = year.ToString().PadLeft(4, '_');
                        }
                        break;
                }

                if (showCalendar) {
                    parts[pIdx] = GKUtils.GetCalendarSign(fCalendar);
                }
            }

            return string.Concat(parts);
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar, bool shorten = false)
        {
            string result = GetDisplayString(format, true, showCalendar);
            if (sign && fApproximated != GDMApproximated.daExact) {
                result = "~ " + result;
            }

            return result;
        }

        #endregion

        public override void GetDateRange(out GDMDate dateStart, out GDMDate dateEnd)
        {
            dateStart = this;
            dateEnd = this;
        }

        public static GDMDate Increment(GDMDate date)
        {
            if (date.IsEmpty()) {
                return Empty;
            }

            var calendar = date.fCalendar;
            var day = date.fDay;
            var month = date.fMonth;
            var year = date.fYear;
            var yearBc = date.fYearBC;

            Increment(ref day, ref month, ref year, ref yearBc, calendar);

            var result = new GDMDate();
            result.SetRawData(date.fApproximated, calendar, year, yearBc, date.fYearModifier, month, day);
            return result;
        }

        private static void Increment(ref byte day, ref byte month, ref short year, ref bool yearBC,
            GDMCalendar calendar)
        {
            if (day > 0) {
                if (day < DaysInMonth(yearBC, year, month, calendar)) {
                    day++;
                    return;
                }

                day = 1;
            }

            if (month > 0) {
                if (month < 12) {
                    month++;
                    return;
                }

                month = 1;
            }

            if (year == 1 && yearBC) {
                yearBC = false;
            } else {
                year++;
            }
        }

        public static GDMDate Decrement(GDMDate date)
        {
            if (date.IsEmpty()) {
                return Empty;
            }

            var calendar = date.fCalendar;
            var day = date.fDay;
            var month = date.fMonth;
            var year = date.fYear;
            var yearBc = date.fYearBC;

            Decrement(ref yearBc, ref year, ref month, ref day, calendar);

            var result = new GDMDate();
            result.SetRawData(date.fApproximated, calendar, year, yearBc, date.fYearModifier, month, day);
            return result;
        }

        private static void Decrement(ref bool yearBc, ref short year, ref byte month, ref byte day,
            GDMCalendar calendar)
        {
            if (day > 1) {
                day--;
                return;
            }

            var monthDecremented = month > 1;
            if (monthDecremented) {
                month--;
            } else if (month > 0) {
                month = 12;
            }

            if (day > 0) {
                day = DaysInMonth(yearBc, year, month, calendar);
            }

            if (monthDecremented) return;

            if (year == 1 && !yearBc) {
                yearBc = true;
            } else {
                year--;
            }
        }

        private static byte DaysInMonth(bool yearBC, short year, byte month, GDMCalendar calendar)
        {
            if (yearBC) {
                year -= 1;
                if (year == 0) {
                    year = 4; // DateTime.DaysInMonth does not support year 0
                }
            }

            if (calendar == GDMCalendar.dcJulian && month == 2 && year % 4 == 0) {
                return 29;
            }

            return (byte)DateTime.DaysInMonth(year, month);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fApproximated);
            hashCode.Add(fCalendar);
            hashCode.Add(fDay);
            hashCode.Add(fMonth);
            hashCode.Add(fYear);
            hashCode.Add(fYearBC);
            hashCode.Add(fYearModifier);
        }
    }
}
