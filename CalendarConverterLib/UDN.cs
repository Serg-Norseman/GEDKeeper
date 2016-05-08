/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2016 by Serg V. Zhdanovskih, Ruslan Garipov.
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
using System.Runtime.InteropServices;

namespace GKCommon
{
    public enum UDNCalendarType { ctGregorian, ctJulian, ctHebrew, ctIslamic }

    /// <summary>
    /// The Unified numbers of dates.
    /// Unification of any dates, given a calendar and unknown components for the needs of comparison and sorting.
    /// Works on the basis of algorithms Julian day.
    /// </summary>
    [Serializable]
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct UDN : ICloneable, IComparable
    {
        private const uint IgnoreYear = 1u << 31;
        private const uint IgnoreMonth = 1u << 30;
        private const uint IgnoreDay = 1u << 29;
        private const uint DateAfter = 1u << 28;
        private const uint DateBefore = 1u << 27;

        private const uint ValueMask = 0x7FFFFFF;

        public const int UnknownYear = 0;
        public const int UnknownMonth = 0;
        public const int UnknownDay = 0;

        /// <summary>
        /// `fValue` is masked Julian day nubmer (JDN, https://en.wikipedia.org/wiki/Julian_day).
        /// It's a usual JDN, having optional flags part.
        ///
        /// The following is the scheme of `fValue` bits value:
        /// +----+----+----+----+----+----------+
        /// | 31 | 30 | 29 | 28 | 27 | 26 ... 0 | bits number
        /// +----+----+----+----+----+----------+
        /// Bits from 0 to 26 is JDN (masked by `ValueMask` member).
        /// The 27th bit is date before flag (it's the `DateBefore`).
        /// The 28th bit is date after flag (it's the `DateAfter` member).
        /// The 29th bit is unknown day flag (it's the `IgnoreDay` member).
        /// The 30th bit is unknown month flag (it's the `IgnoreMonth` member).
        /// The 31th bit is unknown year flag (it's the `IgnoreYear` member).
        /// </summary>
        private readonly uint fValue;


        /// <summary>
        /// Private constructor for internal purposes.
        /// </summary>
        /// <param name="value"></param>
        private UDN(uint value)
        {
            this.fValue = value;
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
            uint result = CreateVal(calendar, year, month, day);
            this.fValue = result;
        }

        /// <summary>
        /// Utility function for debugging and testing.
        /// </summary>
        /// <returns></returns>
        public uint GetUnmaskedValue()
        {
            return (UDN.ValueMask & this.fValue);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public object Clone()
        {
            return new UDN(this.fValue);
        }

        public override string ToString()
        {
            int y, m, d;
            CalendarConverter.jd_to_gregorian(this.GetUnmaskedValue() + 0.5, out y, out m, out d);
            
            string sy = hasKnownYear() ? y.ToString() : "????";
            string sm = hasKnownMonth() ? m.ToString() : "??";
            string sd = hasKnownDay() ? d.ToString() : "??";

            return string.Format("{0}/{1}/{2}", sy, sm, sd);
        }

        public override int GetHashCode()
        {
            return (int)this.fValue;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public int CompareTo(object obj)
        {
            if (obj is UDN) {
                return CompareUDN(this.fValue, ((UDN)obj).fValue);
            }

            return -1;
        }

        /// <summary>
        /// Compares two masked JDN.
        /// </summary>
        /// <param name="l">The left value to compare.</param>
        /// <param name="r">The right value to compare</param>
        /// <returns>'-1' when the `l` is less than the `r`, '1' when the `l` is greater than the `r` and '0' when
        /// the `l` and the `r` are equal.</returns>
        private static int CompareUDN(uint l, uint r)
        {
            if (0 == (UDN.IgnoreYear & l))
            {
                if (0 == (UDN.IgnoreYear & r))
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if (0 == (UDN.IgnoreYear & r))
            {
                return -1;
            }
            else if (0 == (UDN.IgnoreMonth & l))
            {
                if (0 == (UDN.IgnoreMonth & r))
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if (0 == (UDN.IgnoreMonth & r))
            {
                return -1;
            }
            else if (0 == (UDN.IgnoreDay & l))
            {
                if (0 == (UDN.IgnoreDay & r))
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if (0 == (UDN.IgnoreDay & r))
            {
                return -1;
            }
            else
            {
                return 0;
            }
        }

        /// <summary>
        /// Calculates Julian day nubmer (JDN, https://en.wikipedia.org/wiki/Julian_day) using the specified date in
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
        private static uint CreateVal(UDNCalendarType calendar, int year, int month, int day)
        {
            uint result = 0;

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
             * uint result = (uint) (CalendarConverter::{calendar type}_to_jd(UnknownYear, uMonth, uDay));
             *
             * `result` after the code from above is a valid JDN.
             */
            int uYear = year;
            int uMonth = Math.Max(UnknownMonth + 1, month);
            int uDay = Math.Max(UnknownDay + 1, day);

            switch (calendar)
            {
                case UDNCalendarType.ctGregorian:
                    result = (uint)CalendarConverter.gregorian_to_jd(uYear, uMonth, uDay);
                    break;

                case UDNCalendarType.ctJulian:
                    result = (uint)CalendarConverter.julian_to_jd(uYear, uMonth, uDay);
                    break;

                case UDNCalendarType.ctHebrew:
                    result = (uint)CalendarConverter.hebrew_to_jd(uYear, uMonth, uDay);
                    break;

                case UDNCalendarType.ctIslamic:
                    result = (uint)CalendarConverter.islamic_to_jd(uYear, uMonth, uDay);
                    break;
            }

            if (UnknownYear == year)
            {
                result |= IgnoreYear;
            }
            if (UnknownMonth + 1 > month)
            {
                result |= IgnoreMonth;
            }
            if (UnknownDay + 1 > day)
            {
                result |= IgnoreDay;
            }

            return result;
        }

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
        /// <returns>UDN object representing a date before the specified one.
        ///
        /// This method sets "date before" flag on the new date.</returns>
        public static UDN CreateBefore(UDNCalendarType calendar, int year, int month, int day)
        {
            return new UDN(CreateVal(calendar, year, month, day) | DateBefore);
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
        /// <returns>UDN object representing a date after the specified one.
        ///
        /// This method sets "date after" flag on the new date.</returns>
        public static UDN CreateAfter(UDNCalendarType calendar, int year, int month, int day)
        {
            return new UDN(CreateVal(calendar, year, month, day) | DateAfter);
        }

        /// <summary>Finds a date that lies in the middle between the two specified dates.</summary>
        /// <param name="left">The first date. Must be a date with valid year.</param>
        /// <param name="right">The second date. Must be a date with valid year.</param>
        /// <returns>A date that is "average" of the <paramref name="left"/> amd <paramref name="right"/>.
        /// Both the dates must have valid year parts (must not have `IgnoreYear` flag set). Otherwise this method
        /// throws an exception.</returns>
        public static UDN Between(UDN left, UDN right)
        {
            if ((0 != (IgnoreYear & left.fValue)) || (0 != (IgnoreYear & right.fValue)))
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
            uint value = (left.GetUnmaskedValue() + right.GetUnmaskedValue()) >> 1;
            value |=
                (IgnoreMonth & left.fValue) & (IgnoreMonth & right.fValue) |
                (IgnoreDay & left.fValue) & (IgnoreDay & right.fValue);
            return new UDN(value);
        }

        /// <summary>Checks year part of this date.</summary>
        /// <returns>True if this date has valid year (`IgnoreYear` flag isn't set) and false otherwise.</returns>
        public bool hasKnownYear()
        {
            return 0 == (IgnoreYear & fValue);
        }

        /// <summary>Checks month part of this date.</summary>
        /// <returns>True if this date has valid month (`IgnoreMonth` flag isn't set) and false otherwise.</returns>
        public bool hasKnownMonth()
        {
            return 0 == (IgnoreMonth & fValue);
        }

        /// <summary>Checks day part of this date.</summary>
        /// <returns>True if this date has valid day (`IgnoreDay` flag isn't set) and false otherwise.</returns>
        public bool hasKnownDay()
        {
            return 0 == (IgnoreDay & fValue);
        }

        /// <summary>Checks if this date defines a "date before".</summary>
        /// <returns>True if this date is a "date before" (`DateBefore` flag is set) and false otherwise.</returns>
        public bool isDateBefore()
        {
            return 0 == (DateBefore & fValue);
        }

        /// <summary>Checks if this date defines a "date after".</summary>
        /// <returns>True if this date is a "date after" (`DateAfter` flag is set) and false otherwise.</returns>
        public bool isDateAfter()
        {
            return 0 == (DateAfter & fValue);
        }
    }
}
