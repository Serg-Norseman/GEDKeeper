/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Calendar;

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
        public const sbyte EMPTY_YEAR_MOD = -1; // because the value "00" is valid for reading/writing

        private GDMApproximated fApproximated;
        private GDMCalendar fCalendar;
        private byte fDay;
        private byte fMonth;
        private short fYear;
        private bool fYearBC;
        private sbyte fYearModifier;
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

        /// <summary>
        /// Year modifier shows the possible date alternatives for pre-1752 date
        /// brought about by a changing the beginning of the year from MAR to JAN
        /// in the English calendar change of 1752, for example, 15 APR 1699/00.
        /// </summary>
        public sbyte YearModifier
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
            fYearModifier = EMPTY_YEAR_MOD;
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
            fYearModifier = EMPTY_YEAR_MOD;
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
            SetDate(GDMCalendar.dcGregorian, value.Day, value.Month, value.Year);
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

        public override string ParseString(StringSpan strValue)
        {
            string result;
            if (strValue.IsEmptyOrEnd) {
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
                                 short year, bool yearBC, sbyte yearModifier, byte month, byte day)
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

        private static string[] GetMonthNames(GDMCalendar calendar)
        {
            string[] monthes;
            switch (calendar) {
                case GDMCalendar.dcHebrew:
                    monthes = GEDCOMConsts.GEDCOMMonthHebrewArray;
                    break;

                case GDMCalendar.dcFrench:
                    monthes = GEDCOMConsts.GEDCOMMonthFrenchArray;
                    break;

                case GDMCalendar.dcIslamic:
                    monthes = GEDCOMConsts.GEDCOMMonthIslamicArray;
                    break;

                //case GDMCalendar.dcGregorian:
                //case GDMCalendar.dcJulian:
                //case GDMCalendar.dcRoman:
                //case GDMCalendar.dcUnknown:
                default:
                    monthes = GEDCOMConsts.GEDCOMMonthArray;
                    break;
            }
            return monthes;
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
                parts[pIdx++] = GEDCOMConsts.GEDCOMDateFullEscapeArray[(int)fCalendar];
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

                if (fYearModifier >= 0) {
                    yearStr = yearStr + "/" + fYearModifier.ToString("D2");
                }

                if (fYearBC) {
                    yearStr += GEDCOMConsts.YearBC;
                }

                parts[pIdx++] = yearStr;
            }

            return string.Join(" ", parts, 0, pIdx);
        }

        public void SetDate(GDMCalendar calendar, int day, string month, int year, bool yearBC)
        {
            string[] monthArray = GetMonthNames(calendar);
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(month);
            int monthIdx = ArrayHelper.IndexOf(monthArray, su);
            /*if (monthIdx < 0)
                throw new GDMDateException("The string {0} is not a valid {1} month identifier", str, calendar.ToString());*/

            SetDate(calendar, day, monthIdx + 1, year, yearBC);
        }

        public void SetDate(GDMCalendar calendar, int day, int month, int year, bool yearBC = false)
        {
            fCalendar = calendar;
            fDay = (byte)day;
            fMonth = (byte)month;
            fYear = (short)year;
            fYearModifier = EMPTY_YEAR_MOD;
            fYearBC = yearBC;

            DateChanged();
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

        // Left as a reminder.
        // In all cases, 20-23% slower than int.ToString().PadLeft(...)!
        // A complete failure in the attempt at optimization. Respect to MS :)
        /*private static unsafe string xI2S(int number, int totalWidth)
        {
            char paddingChar = (totalWidth > 2 || number == 0) ? '_' : '0';
            var result = new string(paddingChar, totalWidth);
            fixed (char* ch_ptr = result) {
                while (number > 0) {
                    ch_ptr[--totalWidth] = (char)(48 + number % 10);
                    number /= 10;
                }
            }
            return result;
        }*/

        public string GetDisplayString(DateFormat format, bool includeBC = false, bool showCalendar = false)
        {
            if (fYear <= 0 && fMonth <= 0 && fDay <= 0)
                return string.Empty;

            int year = fYear;
            int month = fMonth;
            int day = fDay;
            bool ybc = fYearBC;

            var parts = new string[7];
            int pIdx = 0;

            switch (format) {
                case DateFormat.dfDD_MM_YYYY:
                    parts[pIdx++] = day > 0 ? day.ToString("D2", null) : "__";
                    parts[pIdx++] = ".";
                    parts[pIdx++] = month > 0 ? month.ToString("D2", null) : "__";
                    parts[pIdx++] = ".";
                    parts[pIdx++] = year > 0 ? year.ToString().PadLeft(4, '_') : "____";
                    if (includeBC && ybc) {
                        parts[pIdx++] = " BC";
                    }
                    break;

                case DateFormat.dfYYYY_MM_DD:
                    if (includeBC && ybc) {
                        parts[pIdx++] = "BC ";
                    }
                    parts[pIdx++] = year > 0 ? year.ToString().PadLeft(4, '_') : "____";
                    parts[pIdx++] = ".";
                    parts[pIdx++] = month > 0 ? month.ToString("D2", null) : "__";
                    parts[pIdx++] = ".";
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

        public static GDMDate Subtract(GDMDate date, GDMAge age)
        {
            if (date.IsEmpty() || age.IsEmpty()) {
                return Empty;
            }

            if (date.fYearBC)
                throw new NotSupportedException();

            int ageYears = age.Years;
            int ageMonths = age.Months;
            int ageDays = age.Days;

            int resYear = date.Year;
            int resMonth = date.Month;
            int resDay = date.Day;
            var calendar = date.fCalendar;
            var yearBC = date.fYearBC;

            // years
            if (ageYears > 0) {
                if (resYear == UNKNOWN_YEAR)
                    return Empty;

                resYear -= ageYears;
                if (resYear < 1)
                    return Empty;

                if (ageMonths < 1 && ageDays < 1) {
                    resMonth = 0;
                    resDay = 0;
                }
            }

            // months
            if (ageMonths > 0) {
                if (resMonth > 0) {
                    if (resYear != UNKNOWN_YEAR) {
                        int totalMonths = (resYear * 12 + resMonth) - ageMonths;
                        if (totalMonths < 1)
                            return Empty;

                        resYear = (totalMonths - 1) / 12;
                        resMonth = (totalMonths - 1) % 12 + 1;
                    } else {
                        resMonth -= ageMonths;
                        if (resMonth < 1)
                            return Empty;
                    }
                } else {
                    if (ageYears > 0)
                        resYear -= 1;
                }

                if (ageDays < 1) {
                    resDay = 0;
                }
            }

            // days
            if (ageDays > 0 && resDay > 0) {
                while (ageDays > 0) {
                    if (resDay > ageDays) {
                        resDay -= ageDays;
                        ageDays = 0;
                    } else {
                        if (resMonth == 0 || resYear == UNKNOWN_YEAR)
                            return Empty;

                        ageDays = Math.Abs(resDay - ageDays);
                        if (resMonth > 1) {
                            resMonth -= 1;
                        } else {
                            resMonth = 12;
                            resYear -= 1;
                            if (resYear < 1)
                                return Empty;
                        }
                        resDay = DaysInMonth(yearBC, (short)resYear, (byte)resMonth, calendar);
                    }
                }
            }

            // check date parts
            //resYear = date.Year == UNKNOWN_YEAR ? UNKNOWN_YEAR : resYear;
            //resMonth = date.Month == 0 ? 0 : resMonth;
            //resDay = date.Day == 0 ? 0 : resDay;

            var result = new GDMDate();
            result.SetRawData(GDMApproximated.daCalculated, calendar, (short)resYear, yearBC, date.fYearModifier, (byte)resMonth, (byte)resDay);
            return result;
        }
    }
}
