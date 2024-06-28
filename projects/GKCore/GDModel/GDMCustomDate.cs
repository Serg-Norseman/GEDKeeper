/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Types;

namespace GDModel
{
    public enum GDMCalendar
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


    public enum GDMDateType
    {
        Exact,
        Before,
        After,
        Between,
        PeriodTo,
        PeriodFrom,
        PeriodBetween,
        About,
        Calculated,
        Estimated,

        None
    }


    public enum GDMApproximated
    {
        daExact,
        daAbout,
        daCalculated,
        daEstimated
    }


    public abstract class GDMCustomDate : GDMTag, IComparable, IComparable<GDMCustomDate>, IEquatable<GDMCustomDate>
    {
        public DateTime Date
        {
            get { return GetDateTime(); }
            set { SetDateTime(value); }
        }


        protected GDMCustomDate()
        {
            SetName(GEDCOMTagType.DATE);
        }

        public abstract DateTime GetDateTime();
        public abstract void SetDateTime(DateTime value);
        public abstract string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar, bool shorten = false);

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
            return CompareTo(obj as GDMCustomDate);
        }

        public int CompareTo(GDMCustomDate other)
        {
            if (other != null) {
                UDN abs1 = GetUDN();
                UDN abs2 = other.GetUDN();
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

        public abstract void GetDateRange(out GDMDate dateStart, out GDMDate dateEnd);

        public static GDMDate CreateApproximated(GDMDate date, GDMApproximated approximated)
        {
            GDMDate result = new GDMDate();
            result.Assign(date);
            result.Approximated = approximated;
            return result;
        }

        public static GDMDatePeriod CreatePeriod(GDMDate dateFrom, GDMDate dateTo)
        {
            GDMDatePeriod result = new GDMDatePeriod();
            if (dateFrom != null) result.DateFrom.Assign(dateFrom);
            if (dateTo != null) result.DateTo.Assign(dateTo);
            return result;
        }

        public static GDMDateRange CreateRange(GDMDate dateAfter, GDMDate dateBefore)
        {
            GDMDateRange result = new GDMDateRange();
            if (dateAfter != null) result.After.Assign(dateAfter);
            if (dateBefore != null) result.Before.Assign(dateBefore);
            return result;
        }

        public static GDMDatePeriod GetIntersection(GDMCustomDate range1, GDMCustomDate range2)
        {
            if (range1 == null || range1.IsEmpty() || range2 == null || range2.IsEmpty())
                return GDMDatePeriod.Empty;

            GDMDate r1start, r1end, r2start, r2end;
            range1.GetDateRange(out r1start, out r1end);
            range2.GetDateRange(out r2start, out r2end);

            GDMDate greatestStart = r1start.IsEmpty() ? r2start : (r2start.IsEmpty() ? r1start : (r1start.CompareTo(r2start) > 0) ? r1start : r2start);
            GDMDate smallestEnd = r1end.IsEmpty() ? r2end : (r2end.IsEmpty() ? r1end : (r1end.CompareTo(r2end) < 0) ? r1end : r2end);

            // no intersection
            if (greatestStart.CompareTo(smallestEnd) > 0 && !greatestStart.IsEmpty() && !smallestEnd.IsEmpty()) {
                return GDMDatePeriod.Empty;
            }

            return CreatePeriod(greatestStart, smallestEnd);
        }

        public static GDMList<GDMDatePeriod> GetDifference(GDMCustomDate range1, GDMCustomDate range2)
        {
            GDMDate r1start, r1end, r2start, r2end, i2start, i2end;
            range1.GetDateRange(out r1start, out r1end);
            range2.GetDateRange(out r2start, out r2end);
            range2.GetDateRange(out i2start, out i2end);

            var smallestStart = r1start.IsEmpty() || !r2start.IsEmpty() && r1start.CompareTo(r2start) < 0 ? r1start : r2start;
            var greatestStart = r1start.IsEmpty() || !r2start.IsEmpty() && r2start.CompareTo(r1start) > 0 ? r2start : r1start;
            var smallestEnd = r1end.IsEmpty() || !r2end.IsEmpty() && r1end.CompareTo(r2end) > 0 ? r2end : r1end;
            var greatestEnd = r1end.IsEmpty() || !r2end.IsEmpty() && r1end.CompareTo(r2end) > 0 ? r1end : r2end;

            var result = new GDMList<GDMDatePeriod>();
            result.Add(smallestStart.CompareTo(GDMDate.Decrement(greatestStart)) <= 0 || smallestStart.IsEmpty()
                ? CreatePeriod(smallestStart, GDMDate.Decrement(greatestStart))
                : GDMDatePeriod.Empty);

            result.Add(GDMDate.Increment(smallestEnd).CompareTo(greatestEnd) <= 0 || greatestEnd.IsEmpty()
                ? CreatePeriod(GDMDate.Increment(smallestEnd), greatestEnd)
                : GDMDatePeriod.Empty);

            return result;
        }
    }
}
