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

        private const uint YearMask = IgnoreYear;
        private const uint MonthMask = IgnoreMonth;
        private const uint DayMask = IgnoreDay;

        private const uint ValueMask = 0x1fffffff;

        public const int UnknownYear = -4713;
        public const int UnknownMonth = 0;
        public const int UnknownDay = 0;


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
            string result;

            int y, m, d;
            CalendarConverter.jd_to_gregorian(this.GetUnmaskedValue() + 0.5, out y, out m, out d);
            
            string sy = ((UDN.YearMask & fValue) == UDN.IgnoreYear) ? "????" : y.ToString();
            string sm = ((UDN.MonthMask & fValue) == UDN.IgnoreMonth) ? "??" : m.ToString();
            string sd = ((UDN.DayMask & fValue) == UDN.IgnoreDay) ? "??" : d.ToString();

            return result = string.Format("{0}/{1}/{2}", sy, sm, sd);
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
        /// 
        /// </summary>
        /// <param name="l"></param>
        /// <param name="r"></param>
        /// <returns></returns>
        private static int CompareUDN(uint l, uint r)
        {
            if ((UDN.YearMask & l) != UDN.IgnoreYear)
            {
                if ((UDN.YearMask & r) != UDN.IgnoreYear)
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDN.YearMask & r) != UDN.IgnoreYear)
            {
                return -1;
            }
            else if ((UDN.MonthMask & l) != UDN.IgnoreMonth)
            {
                if ((UDN.MonthMask & r) != UDN.IgnoreMonth)
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDN.MonthMask & r) != UDN.IgnoreMonth)
            {
                return -1;
            }
            else if ((UDN.DayMask & l) != UDN.IgnoreDay)
            {
                if ((UDN.DayMask & r) != UDN.IgnoreDay)
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDN.DayMask & r) != UDN.IgnoreDay)
            {
                return -1;
            }
            else
            {
                return 0;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="calendar"></param>
        /// <param name="year"></param>
        /// <param name="month"></param>
        /// <param name="day"></param>
        /// <returns></returns>
        private static uint CreateVal(UDNCalendarType calendar, int year, int month, int day)
        {
            uint result = 0;

            int uYear = Math.Max(UnknownYear + 1, year);
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

            if (UnknownYear + 1 > year)
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
    }
}
