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
using GKCore.Types;

namespace GKCommon.GEDCOM
{
    public abstract class GEDCOMCustomDate : GEDCOMTag, IComparable
    {
        public static readonly string[] GEDCOMDateApproximatedArray;
        public static readonly string[] GEDCOMDateRangeArray;
        public static readonly string[] GEDCOMDateEscapeArray;

        public static readonly string[] GEDCOMMonthArray;
        public static readonly string[] GEDCOMMonthHebrewArray;
        public static readonly string[] GEDCOMMonthFrenchArray;

        public static readonly string[] GEDCOMMonthSysArray;
        public static readonly string[] GEDCOMMonthRusArray;

        static GEDCOMCustomDate()
        {
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

            GEDCOMMonthRusArray = new string[]
            {
                "ЯНВ", "ФЕВ", "МАР", "АПР", "МАЙ", "ИЮН",
                "ИЮЛ", "АВГ", "СЕН", "ОКТ", "НОЯ", "ДЕК"
            };
        }

        public DateTime Date
        {
            get { return GetDateTime(); }
            set { SetDateTime(value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("DATE");
        }

        protected GEDCOMCustomDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
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
                int m, d;
                CalendarConverter.jd_to_gregorian2((int)udn.GetUnmaskedValue(), out resultYear, out m, out d);
            } else {
                resultYear = 0;
            }

            return resultYear;
        }

        public int CompareTo(object obj)
        {
            GEDCOMCustomDate otherDate = obj as GEDCOMCustomDate;

            if (otherDate != null) {
                UDN abs1 = GetUDN();
                UDN abs2 = otherDate.GetUDN();
                return abs1.CompareTo(abs2);
            }

            return -1;
        }

        public static GEDCOMDate CreateApproximated(GEDCOMTree owner, GEDCOMObject parent, GEDCOMDate date, GEDCOMApproximated approximated)
        {
            GEDCOMDate result = new GEDCOMDate(owner, parent, "", "");
            result.Assign(date);
            result.Approximated = approximated;
            return result;
        }

        public static GEDCOMDatePeriod CreatePeriod(GEDCOMTree owner, GEDCOMObject parent, GEDCOMDate dateFrom, GEDCOMDate dateTo)
        {
            GEDCOMDatePeriod result = new GEDCOMDatePeriod(owner, parent, "", "");
            result.DateFrom.Assign(dateFrom);
            result.DateTo.Assign(dateTo);
            return result;
        }

        public static GEDCOMDateRange CreateRange(GEDCOMTree owner, GEDCOMObject parent, GEDCOMDate dateAfter, GEDCOMDate dateBefore)
        {
            GEDCOMDateRange result = new GEDCOMDateRange(owner, parent, "", "");
            result.After.Assign(dateAfter);
            result.Before.Assign(dateBefore);
            return result;
        }
    }
}
