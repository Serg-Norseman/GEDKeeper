/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2016-2025 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using System.Runtime.CompilerServices;
using BSLib;

namespace GKCore.Calendar
{
    public enum UDNCalendarType { ctGregorian, ctJulian, ctHebrew, ctIslamic }

    /// <summary>
    /// The Unified number of dates.
    /// Unification of any dates, given a calendar and unknown components for the needs of comparison and sorting.
    /// Works on the basis of algorithms Julian day.
    /// </summary>
    public struct UDN : ICloneable<UDN>, IComparable, IComparable<UDN>, IEquatable<UDN>
    {
        public static readonly UDN Empty = new UDN(0);
        public static readonly UDN Unknown = new UDN(UDNCalendarType.ctGregorian, UnknownYear, UnknownMonth, UnknownDay);

        private const int IgnoreYear = 1 << 31;
        private const int IgnoreMonth = 1 << 30;
        private const int IgnoreDay = 1 << 29;
        private const int DateAfter = 1 << 28;
        private const int DateBefore = 1 << 27;
        private const int ApproximateDate = 1 << 26;

        private const int IgnoreYMD = IgnoreYear | IgnoreMonth | IgnoreDay;

        private const int ValueMask = 0x3FFFFFF;

        public const int UnknownYear = 0;
        public const int UnknownMonth = 0;
        public const int UnknownDay = 0;

        /// <summary>
        /// `fValue` is masked Julian day nubmer (JDN, https://en.wikipedia.org/wiki/Julian_day).
        /// It's a usual JDN, having optional flags part.
        ///
        /// The following is the scheme of `fValue` bits value:
        /// +----+----+----+----+----+----+----------+
        /// | 31 | 30 | 29 | 28 | 27 | 26 | 25 ... 0 | bits number
        /// +----+----+----+----+----+----+----------+
        /// Bits from 0 to 25 is JDN (masked by `ValueMask` member).
        /// The 26th bit is approximate date flag (it's the `ApproximateDate`).
        /// The 27th bit is date before flag (it's the `DateBefore`).
        /// The 28th bit is date after flag (it's the `DateAfter` member).
        /// The 29th bit is unknown day flag (it's the `IgnoreDay` member).
        /// The 30th bit is unknown month flag (it's the `IgnoreMonth` member).
        /// The 31th bit is unknown year flag (it's the `IgnoreYear` member).
        ///
        /// Bits `ApproximateDate`, `DateBefore` and `DateAfter` are mutually exclusive.
        ///
        /// Why can we use the flags.
        /// Because normal Julian day number is based on the Julian Period. Which size is 7980 years. The next Julian
        /// Period begins in the year 3268 AD. Thus, JDN upper limit is 0x2c7986 (0b1011000111100110000110). We need at
        /// least 22 bits available for JDN. But in .NET's int (the current type for `fValue`) we have 32 bits -- isn't
        /// a waste of resources, heh! Therefore, we use upper unused bits as flags.
        /// </summary>
        private readonly int fValue;


        /// <summary>
        /// Private constructor for internal purposes.
        /// </summary>
        /// <param name="value"></param>
        private UDN(int value)
        {
            fValue = value;
        }

        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="calendar"></param>
        /// <param name="year"></param>
        /// <param name="month"></param>
        /// <param name="day"></param>
        public UDN(UDNCalendarType calendar, int year, int month, int day)
        {
            fValue = CreateVal(calendar, year, month, day);
        }

        #region Object overrides

        public override string ToString()
        {
            int y = 0, m = 0, d = 0;

            if (HasKnownYear() || HasKnownMonth() || HasKnownDay()) {
                int unmaskedVal = GetUnmaskedValue();
                CalendarConverter.jd_to_gregorian2(unmaskedVal, out y, out m, out d);
            }

            int sign = Math.Sign(y);
            y = Math.Abs(y);
            string sy = HasKnownYear() ? y.ToString().PadLeft(4, '0') : "????";
            if (sign == -1) sy = "-" + sy;

            string sm = HasKnownMonth() ? m.ToString().PadLeft(2, '0') : "??";
            string sd = HasKnownDay() ? d.ToString().PadLeft(2, '0') : "??";

            string result = string.Format("{0}/{1}/{2}", sy, sm, sd);
            if (IsApproximateDate()) {
                result = "~" + result;
            } else if (IsDateBefore()) {
                result = "<" + result;
            } else if (IsDateAfter()) {
                result = ">" + result;
            }

            return result;
        }

        public override int GetHashCode()
        {
            return fValue.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if (obj is UDN) {
                return (fValue == ((UDN)obj).fValue);
            }
            return false;
        }

        #endregion

        #region ICloneable implementation

        /// <summary>
        /// Create a copy of the current object.
        /// </summary>
        /// <returns></returns>
        public UDN Clone()
        {
            return new UDN(fValue);
        }

        #endregion

        #region IComparable implementation

        /// <summary>
        /// 
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public int CompareTo(object obj)
        {
            if (obj is UDN) {
                return CompareVal(fValue, ((UDN)obj).fValue);
            }
            return -1;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="other"></param>
        /// <returns></returns>
        public int CompareTo(UDN other)
        {
            return CompareVal(fValue, other.fValue);
        }

        #endregion

        #region IEquatable implementation

        /// <summary>
        /// 
        /// </summary>
        /// <param name="other"></param>
        /// <returns></returns>
        public bool Equals(UDN other)
        {
            return (fValue == other.fValue);
        }

        #endregion

        #region Private methods

        /// <summary>
        /// Compares two masked JDN.
        /// </summary>
        /// <param name="l">The left value to compare.</param>
        /// <param name="r">The right value to compare</param>
        /// <returns>'-1' when the `l` is less than the `r`, '1' when the `l` is greater than the `r` and '0' when
        /// the `l` and the `r` are equal.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int CompareVal(int l, int r)
        {
            int result = 0;
            if ((IgnoreYear & l) == 0)
            {
                if ((IgnoreYear & r) == 0)
                {
                    result = Math.Sign(((int) (ValueMask & l)) - ((int) (ValueMask & r)));
                }
                else
                {
                    result = 1;
                }
            }
            else if ((IgnoreYear & r) == 0)
            {
                result = -1;
            }
            else if ((IgnoreMonth & l) == 0)
            {
                if ((IgnoreMonth & r) == 0)
                {
                    result = Math.Sign(((int) (ValueMask & l)) - ((int) (ValueMask & r)));
                }
                else
                {
                    result = 1;
                }
            }
            else if ((IgnoreMonth & r) == 0)
            {
                result = -1;
            }
            else if ((IgnoreDay & l) == 0)
            {
                if ((IgnoreDay & r) == 0)
                {
                    result = Math.Sign(((int) (ValueMask & l)) - ((int) (ValueMask & r)));
                }
                else
                {
                    result = 1;
                }
            }
            else if ((IgnoreDay & r) == 0)
            {
                result = -1;
            }
            if (result == 0)
            {
                /*
                 * Here we got equal JDNs-UDNs. Now we need to do an additional processing of `ApproximateDate`,
                 * `DateBefore` and `DateAfter` flags to implement the following ordering:
                 * 1. Before date A.
                 * 2. Near the date A.
                 * 3. Exact date A.
                 * 4. After date A.
                 *
                 * The following table describes how the code below implements such processing.
                 * +-----+------------+-----+---------------------------------------------------+
                 * | `l` | CMP result | `r` | Description                                       |
                 * +-----+------------+-----+---------------------------------------------------+
                 * | < A | -1         | A   | This is implemented by code inside                |
                 * | < A | 0          | < A | `if (0 != (DateBefore & l))` statement.           |
                 * | < A | -1         | > A |                                                   |
                 * | < A | -1         | ~ A |                                                   |
                 * |     |            |     |                                                   |
                 * | > A | 1          | A   | This is implemented by code inside                |
                 * | > A | 1          | < A | `else if (0 != (DateAfter & l))` statement.       |
                 * | > A | 0          | > A |                                                   |
                 * | > A | 1          | ~ A |                                                   |
                 * |     |            |     |                                                   |
                 * | ~ A | -1         | A   | This is implemented by code inside                |
                 * | ~ A | 1          | < A | `else if (0 != (ApproximateDate & l))` statement. |
                 * | ~ A | -1         | > A |                                                   |
                 * | ~ A | 0          | ~ A |                                                   |
                 * |     |            |     |                                                   |
                 * | A   | 0          | A   | This is implemented by code inside                |
                 * | A   | 1          | < A | `else if (0 != (DateAfter & r))` and              |
                 * | A   | -1         | > A | `else` statements.                                |
                 * | A   | 1          | ~ A |                                                   |
                 * +-----+------------+-----+---------------------------------------------------+
                 */
                if ((DateBefore & l) != 0)
                {
                    result = ((DateBefore & r) != 0) ? 0 : -1;
                }
                else if ((DateAfter & l) != 0)
                {
                    result = ((DateAfter & r) != 0) ? 0 : 1;
                }
                else if ((ApproximateDate & l) != 0)
                {
                    if ((ApproximateDate & r) != 0)
                    {
                        result = 0;
                    }
                    else
                    {
                        result = ((DateBefore & r) != 0) ? 1 : -1;
                    }
                }
                else if ((DateAfter & r) != 0)
                {
                    result = -1;
                }
                else
                {
                    result = (((DateBefore | ApproximateDate) & r) != 0) ? 1 : 0;
                }
            }
            return result;
        }

        /// <summary>
        /// Calculates Julian day number (JDN, https://en.wikipedia.org/wiki/Julian_day) using the specified date in
        /// the specified <paramref name="calendar"/>.
        /// Return value of this method ain't a usual JDN. See Returns section for more information.
        /// </summary>
        /// <param name="calendar">Source calendar. The <paramref name="year"/>, <paramref name="month"/> and
        /// <paramref name="day"/> are in this calendar.</param>
        /// <param name="year">Year number. Pass `UnknownYear` constant when year is unknown.
        /// This method DOES NOT check that `year` is inside a valid range. It's duty of a caller.</param>
        /// <param name="month">Month number. Pass `UnknownMonth` constant when month is unknown.
        /// This method DOES NOT check that `month` is inside a valid range. It's duty of a caller.</param>
        /// <param name="day">Day number. Pass `UnknownDay` constant when day is unknown.
        /// This method DOES NOT check that `day` is inside a valid range. It's duty of a caller.</param>
        /// <returns>Masked Julian day nubmer. See description of the <see cref="fValue"/> member for detailed
        /// information about masked JDN value.
        ///
        /// This method doesn't change the 27th and 28th bit ("date before" and "date after").
        /// </returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int CreateVal(UDNCalendarType calendar, int year, int month, int day)
        {
            int result = 0;

            /*
             * @ruslangaripov:
             *
             * The next conversation assume that `UnknownYear` member is 0.
             *
             * If all the `CalendarConverter::{calendar type}_to_jd` functions guarantee valid JDN for 0th year, we can
             * safely use the following code:
             *
             * int uYear = year;
             *
             * But if at least one of `CalendarConverter::{calendar type}_to_jd` functions fails on 0th year (read:
             * gives incorrect JDN) we HAVE TO use code like this:
             *
             * int uYear = (UnknownYear != year) ? year : 1;
             *
             * Here `1` is the first valid year for an algorithm that calculates JDN. I believe any such calculation
             * succeeds doing such calculation with 1st year.
             *
             * Wikipedia says that "For the year (Y) astronomical year numbering is used", and therefore I currently
             * stick with "0th year always valid for calculation JDN". And therefore, a
             * `CalendarConverter::{calendar type}_to_jd` succeeds with `UnknownYear`:
             *
             * int result = (int) (CalendarConverter::{calendar type}_to_jd(UnknownYear, uMonth, uDay));
             *
             * `result` after the code from above is a valid JDN.
             */
            int uYear = year;
            int uMonth = Math.Max(UnknownMonth + 1, month);
            int uDay = Math.Max(UnknownDay + 1, day);

            switch (calendar) {
                case UDNCalendarType.ctGregorian:
                    result = CalendarConverter.gregorian_to_jd2(uYear, uMonth, uDay); // fixed
                    break;

                case UDNCalendarType.ctJulian:
                    result = CalendarConverter.julian_to_jd2(uYear, uMonth, uDay); // fixed
                    break;

                case UDNCalendarType.ctHebrew:
                    result = CalendarConverter.hebrew_to_jd3(uYear, uMonth, uDay); // fixed to the 3rd variant
                    break;

                case UDNCalendarType.ctIslamic:
                    result = CalendarConverter.islamic_to_jd3(uYear, uMonth, uDay); // fixed to the 3rd variant
                    break;
            }

            if (UnknownYear == year) {
                result |= IgnoreYear;
            }
            if (UnknownMonth + 1 > month) {
                result |= IgnoreMonth;
            }
            if (UnknownDay + 1 > day) {
                result |= IgnoreDay;
            }

            return result;
        }

        #endregion

        #region Static constructors

        /// <summary>
        /// Creates a new UDN instance that represents a date before the specified date in the specified
        /// <paramref name="calendar"/>.</summary>
        /// <param name="calendar">Source calendar. The <paramref name="year"/>, <paramref name="month"/> and
        /// <paramref name="day"/> are in this calendar.</param>
        /// <param name="year">Year number. Pass `UnknownYear` constant when year is unknown.
        /// This method DOES NOT check that `year` is inside a valid range. It's duty of a caller.</param>
        /// <param name="month">Month number. Pass `UnknownMonth` constant when month is unknown.
        /// This method DOES NOT check that `month` is inside a valid range. It's duty of a caller.</param>
        /// <param name="day">Day number. Pass `UnknownDay` constant when day is unknown.
        /// This method DOES NOT check that `day` is inside a valid range. It's duty of a caller.</param>
        /// <returns>UDN object representing a date before the specified one.</returns>
        public static UDN CreateBefore(UDNCalendarType calendar, int year, int month, int day)
        {
            return new UDN(CreateVal(calendar, year, month, day) | DateBefore);
        }

        /// <summary>
        /// Creates a new UDN instance that represents a date before the specified universal date.</summary>
        /// <param name="udn">Source date to be converted to "date before it".</param>
        /// <returns>UDN object representing a date before the specified one.</returns>
        public static UDN CreateBefore(UDN udn)
        {
            // We should guarantee that result UDN will have only `ApproximateDate`, `DateBefore` or `DateAfter`.
            return new UDN((udn.fValue & ~(DateAfter | ApproximateDate)) | DateBefore);
        }

        /// <summary>
        /// Creates a new UDN instance that represents a date after the specified date in the specified
        /// <paramref name="calendar"/>.</summary>
        /// <param name="calendar">Source calendar. The <paramref name="year"/>, <paramref name="month"/> and
        /// <paramref name="day"/> are in this calendar.</param>
        /// <param name="year">Year number. Pass `UnknownYear` constant when year is unknown.
        /// This method DOES NOT check that `year` is inside a valid range. It's duty of a caller.</param>
        /// <param name="month">Month number. Pass `UnknownMonth` constant when month is unknown.
        /// This method DOES NOT check that `month` is inside a valid range. It's duty of a caller.</param>
        /// <param name="day">Day number. Pass `UnknownDay` constant when day is unknown.
        /// This method DOES NOT check that `day` is inside a valid range. It's duty of a caller.</param>
        /// <returns>UDN object representing a date after the specified one.</returns>
        public static UDN CreateAfter(UDNCalendarType calendar, int year, int month, int day)
        {
            return new UDN(CreateVal(calendar, year, month, day) | DateAfter);
        }

        /// <summary>
        /// Creates a new UDN instance that represents a date after the specified universal date.</summary>
        /// <param name="udn">Source date to be converted to "date after it".</param>
        /// <returns>UDN object representing a date after the specified one.</returns>
        public static UDN CreateAfter(UDN udn)
        {
            // We should guarantee that result UDN will have only `ApproximateDate`, `DateBefore` or `DateAfter`.
            return new UDN((udn.fValue & ~(DateBefore | ApproximateDate)) | DateAfter);
        }

        /// <summary>
        /// Creates a new UDN instance that represents a date near the specified date in the specified
        /// <paramref name="calendar"/>.</summary>
        /// <param name="calendar">Source calendar. The <paramref name="year"/>, <paramref name="month"/> and
        /// <paramref name="day"/> are in this calendar.</param>
        /// <param name="year">Year number. Pass `UnknownYear` constant when year is unknown.
        /// This method DOES NOT check that `year` is inside a valid range. It's duty of a caller.</param>
        /// <param name="month">Month number. Pass `UnknownMonth` constant when month is unknown.
        /// This method DOES NOT check that `month` is inside a valid range. It's duty of a caller.</param>
        /// <param name="day">Day number. Pass `UnknownDay` constant when day is unknown.
        /// This method DOES NOT check that `day` is inside a valid range. It's duty of a caller.</param>
        /// <returns>UDN object representing a date near the specified one.</returns>
        public static UDN CreateApproximate(UDNCalendarType calendar, int year, int month, int day)
        {
            return new UDN(CreateVal(calendar, year, month, day) | ApproximateDate);
        }

        /// <summary>
        /// Creates a new UDN instance that represents a date near the specified universal date.</summary>
        /// <param name="udn">Source date to be converted to "date near it".</param>
        /// <returns>UDN object representing a date near the specified one.</returns>
        public static UDN CreateApproximate(UDN udn)
        {
            return new UDN((udn.fValue & ~(DateBefore | DateAfter)) | ApproximateDate);
        }

        /// <summary>Finds a date that lies in the middle between the two specified dates.</summary>
        /// <param name="left">The first date. Must be a date with valid year.</param>
        /// <param name="right">The second date. Must be a date with valid year.</param>
        /// <returns>A date that is "average" of the <paramref name="left"/> and <paramref name="right"/>.
        /// Both the dates must have valid year parts (must not have `IgnoreYear` flag set). Otherwise this method
        /// throws an exception.</returns>
        public static UDN CreateBetween(UDN left, UDN right, bool checkYears = true)
        {
            if (checkYears && (((IgnoreYear & left.fValue) != 0) || ((IgnoreYear & right.fValue) != 0)))
            {
                throw new Exception("`Between` member requires dates with valid years");
            }
            /*
             * Wikipedia states that "Julian day is the continuous count of days since the beginning of the Julian
             * Period" (https://en.wikipedia.org/wiki/Julian_day).
             * Therefore, for two dates A and B we can state that 'JDN(A) - JDN(B)' is number of days beetween A and B.
             *
             * Now look on implementation of the `CreateVal` method: it uses the first month in a year and/or the first
             * day of a month when month and/or day respectively is unknown.
             *
             * Thus we can assume that both `left` and `right` always have valid date parts even if some of those parts
             * must be ignored.
             *
             * Next, if we found a month between an unknown month and a known one, result is a known month. If we found
             * a month between two unknown monthes, result is an unknown month. The same stuff is applied to days.
             */
            int value = (left.GetUnmaskedValue() + right.GetUnmaskedValue()) >> 1;
            value |=
                (IgnoreMonth & left.fValue) & (IgnoreMonth & right.fValue) |
                (IgnoreDay & left.fValue) & (IgnoreDay & right.fValue) &
                ~(ApproximateDate | DateBefore | DateAfter);
            return new UDN(value);
        }

        #endregion

        /// <summary>Checks year part of this date.</summary>
        /// <returns>True if this date has valid year (`IgnoreYear` flag isn't set) and false otherwise.</returns>
        public bool HasKnownYear()
        {
            return (IgnoreYear & fValue) == 0;
        }

        /// <summary>Checks month part of this date.</summary>
        /// <returns>True if this date has valid month (`IgnoreMonth` flag isn't set) and false otherwise.</returns>
        public bool HasKnownMonth()
        {
            return (IgnoreMonth & fValue) == 0;
        }

        /// <summary>Checks day part of this date.</summary>
        /// <returns>True if this date has valid day (`IgnoreDay` flag isn't set) and false otherwise.</returns>
        public bool HasKnownDay()
        {
            return (IgnoreDay & fValue) == 0;
        }

        /// <summary>Checks if this date defines an approximate date.</summary>
        /// <returns>True if this date is approximate date (`ApproximateDate` flag is set) and false otherwise.
        /// </returns>
        public bool IsApproximateDate()
        {
            return (ApproximateDate & fValue) != 0;
        }

        /// <summary>Checks if this date defines a "date before".</summary>
        /// <returns>True if this date is a "date before" (`DateBefore` flag is set) and false otherwise.</returns>
        public bool IsDateBefore()
        {
            return (DateBefore & fValue) != 0;
        }

        /// <summary>Checks if this date defines a "date after".</summary>
        /// <returns>True if this date is a "date after" (`DateAfter` flag is set) and false otherwise.</returns>
        public bool IsDateAfter()
        {
            return (DateAfter & fValue) != 0;
        }

        public bool IsEmpty()
        {
            return (fValue == 0) || (IgnoreYMD & fValue) == IgnoreYMD;
        }

        /// <summary>
        /// Utility function for debugging and testing.
        /// </summary>
        /// <returns></returns>
        public int GetUnmaskedValue()
        {
            return (ValueMask & fValue);
        }

        public DateTime GetGregorianDateTime()
        {
            DateTime result;

            if (HasKnownYear()) {
                int jdn = GetUnmaskedValue();

                int year, month, day;
                CalendarConverter.jd_to_gregorian2(jdn, out year, out month, out day);

                if (month <= 0) month = 1;
                if (day <= 0) day = 1;

                try {
                    result = new DateTime(year, month, day);
                    return result;
                } catch (Exception ex) {
                    Logger.WriteError(string.Format("UDN.GetGregorianDateTime({0}, {1}, {2})", year, month, day), ex);
                }
            }

            result = new DateTime(0);
            return result;
        }
    }
}
