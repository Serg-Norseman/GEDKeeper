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
using BSLib.Calendar;
using GKCore.Types;

namespace GKCommon.GEDCOM
{
    public enum GEDCOMCalendar
    {
        dcGregorian,
        dcJulian,
        dcHebrew,
        dcFrench,
        dcRoman,
        dcIslamic, // GK+ (nonstandard)
        dcUnknown,

        dcLast = dcUnknown
    }


    public enum GEDCOMDateFormat
    {
        dfGEDCOMStd,
        dfSystem
    }


    public enum GEDCOMApproximated
    {
        daExact,
        daAbout,
        daCalculated,
        daEstimated
    }


    public enum GEDCOMRange
    {
        drAfter,
        drBefore,
        drBetween,
        drAnd
    }


    public enum GEDCOMDateType
    {
        SIMP, ABT, AFT, BEF, BET, CAL, EST, FROM, INT, TO
    }


    public abstract class GDMCustomDate : GDMTag, IComparable, IEquatable<GDMCustomDate>
    {
        public static readonly string[] GEDCOMDateTypes;
        public static readonly string[] GEDCOMDateApproximatedArray;
        public static readonly string[] GEDCOMDateRangeArray;
        public static readonly string[] GEDCOMDateEscapeArray;

        public static readonly string[] GEDCOMMonthArray;
        public static readonly string[] GEDCOMMonthHebrewArray;
        public static readonly string[] GEDCOMMonthFrenchArray;
        public static readonly string[] GEDCOMMonthSysArray;

        public static readonly EnumTuple[] GEDCOMMonthValues;

        static GDMCustomDate()
        {
            GEDCOMDateTypes = new string[] { "", "ABT", "AFT", "BEF", "BET", "CAL", "EST", "FROM", "INT", "TO" };

            GEDCOMDateApproximatedArray = new string[] { "", "ABT", "CAL", "EST" };
            GEDCOMDateRangeArray = new string[] { "AFT", "BEF", "BET", "AND" };

            GEDCOMDateEscapeArray = new string[]
            {
                "@#DGREGORIAN@", "@#DJULIAN@", "@#DHEBREW@", "@#DFRENCH R@", "@#DROMAN@",
                "@#DISLAMIC@", // GK+ (nonstandard)
                "@#DUNKNOWN@"
            };


            GEDCOMMonthArray = new string[]
            {
                "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
            };

            GEDCOMMonthHebrewArray = new string[]
            {
                "TSH", "CSH", "KSL", "TVT", "SHV", "ADR",
                "ADS", "NSN", "IYR", "SVN", "TMZ", "AAV", "ELL"
            };

            GEDCOMMonthFrenchArray = new string[]
            {
                "VEND", "BRUM", "FRIM", "NIVO", "PLUV", "VENT",
                "GERM", "FLOR", "PRAI", "MESS", "THER", "FRUC", "COMP"
            };

            GEDCOMMonthSysArray = new string[]
            {
                "01.", "02.", "03.", "04.", "05.", "06.",
                "07.", "08.", "09.", "10.", "11.", "12."
            };

            // post-arranged array
            GEDCOMMonthValues = new EnumTuple[] {
                new EnumTuple("JAN", 1), // J/G
                new EnumTuple("FEB", 2), // J/G
                new EnumTuple("MAR", 3), // J/G
                new EnumTuple("APR", 4), // J/G
                new EnumTuple("MAY", 5), // J/G
                new EnumTuple("JUN", 6), // J/G
                new EnumTuple("JUL", 7), // J/G
                new EnumTuple("AUG", 8), // J/G
                new EnumTuple("SEP", 9), // J/G
                new EnumTuple("OCT", 10), // J/G
                new EnumTuple("NOV", 11), // J/G
                new EnumTuple("DEC", 12), // J/G

                new EnumTuple("TSH", 1), // H
                new EnumTuple("CSH", 2), // H
                new EnumTuple("KSL", 3), // H
                new EnumTuple("TVT", 4), // H
                new EnumTuple("SHV", 5), // H
                new EnumTuple("ADR", 6), // H
                new EnumTuple("ADS", 7), // H
                new EnumTuple("NSN", 8), // H
                new EnumTuple("IYR", 9), // H
                new EnumTuple("SVN", 10), // H
                new EnumTuple("TMZ", 11), // H
                new EnumTuple("AAV", 12), // H
                new EnumTuple("ELL", 13), // H

                new EnumTuple("VEND", 1), // F
                new EnumTuple("BRUM", 2), // F
                new EnumTuple("FRIM", 3), // F
                new EnumTuple("NIVO", 4), // F
                new EnumTuple("PLUV", 5), // F
                new EnumTuple("VENT", 6), // F
                new EnumTuple("GERM", 7), // F
                new EnumTuple("FLOR", 8), // F
                new EnumTuple("PRAI", 9), // F
                new EnumTuple("MESS", 10), // F
                new EnumTuple("THER", 11), // F
                new EnumTuple("FRUC", 12), // F
                new EnumTuple("COMP", 13), // F

                // for files with poor standard support (Russian-localized names of the months)
                new EnumTuple("ЯНВ", 1), // J/G
                new EnumTuple("ФЕВ", 2), // J/G
                new EnumTuple("МАР", 3), // J/G
                new EnumTuple("АПР", 4), // J/G
                new EnumTuple("МАЙ", 5), // J/G
                new EnumTuple("ИЮН", 6), // J/G
                new EnumTuple("ИЮЛ", 7), // J/G
                new EnumTuple("АВГ", 8), // J/G
                new EnumTuple("СЕН", 9), // J/G
                new EnumTuple("ОКТ", 10), // J/G
                new EnumTuple("НОЯ", 11), // J/G
                new EnumTuple("ДЕК", 12), // J/G
            };
            // BinarySearch requires a sorted array
            Array.Sort(GEDCOMMonthValues);
        }

        public DateTime Date
        {
            get { return GetDateTime(); }
            set { SetDateTime(value); }
        }


        protected GDMCustomDate(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.DATE);
        }

        public abstract DateTime GetDateTime();
        public abstract void SetDateTime(DateTime value);
        public abstract string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar);

        protected virtual void DateChanged()
        {
        }

        /// <summary>
        /// Obtaining UDN (Unified Date Number) for purposes of processing and sorting.
        /// </summary>
        /// <returns></returns>
        public abstract UDN GetUDN();

        /// <summary>
        /// In the historical chronology of the year 0 does not exist.
        /// Therefore, the digit 0 in the year value can be used as a sign of lack or error.
        /// ChronologicalYear - introduced for the purposes of uniform chronology years in the Gregorian calendar.
        /// Is estimated from -4714 BC to 3268 AD.
        /// </summary>
        /// <returns>chronological year</returns>
        public virtual int GetChronologicalYear()
        {
            int resultYear;

            UDN udn = GetUDN();
            if (udn.HasKnownYear()) {
                int month, day;
                CalendarConverter.jd_to_gregorian2(udn.GetUnmaskedValue(), out resultYear, out month, out day);
            } else {
                resultYear = 0;
            }

            return resultYear;
        }

        public int CompareTo(object obj)
        {
            GDMCustomDate otherDate = obj as GDMCustomDate;

            if (otherDate != null) {
                UDN abs1 = GetUDN();
                UDN abs2 = otherDate.GetUDN();
                return abs1.CompareTo(abs2);
            }

            return -1;
        }

        public override int GetHashCode()
        {
            var udn = GetUDN();
            return udn.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            GDMCustomDate otherDate = obj as GDMCustomDate;

            if (otherDate != null) {
                UDN abs1 = GetUDN();
                UDN abs2 = otherDate.GetUDN();
                return abs1.Equals(abs2);
            }

            return false;
        }

        public bool Equals(GDMCustomDate other)
        {
            UDN abs1 = GetUDN();
            UDN abs2 = other.GetUDN();
            return abs1.Equals(abs2);
        }

        internal virtual string ParseContext(GEDCOMParser context)
        {
            return string.Empty;
        }

        public static GDMDate CreateApproximated(GDMObject owner, GDMDate date, GEDCOMApproximated approximated)
        {
            GDMDate result = new GDMDate(owner);
            result.Assign(date);
            result.Approximated = approximated;
            return result;
        }

        public static GDMDatePeriod CreatePeriod(GDMObject owner, GDMDate dateFrom, GDMDate dateTo)
        {
            GDMDatePeriod result = new GDMDatePeriod(owner);
            result.DateFrom.Assign(dateFrom);
            result.DateTo.Assign(dateTo);
            return result;
        }

        public static GDMDateRange CreateRange(GDMObject owner, GDMDate dateAfter, GDMDate dateBefore)
        {
            GDMDateRange result = new GDMDateRange(owner);
            result.After.Assign(dateAfter);
            result.Before.Assign(dateBefore);
            return result;
        }
    }
}
