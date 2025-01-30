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
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMDatePeriod : GDMCustomDate
    {
        public static readonly GDMDatePeriod Empty = new GDMDatePeriod();


        private readonly GDMDate fDateFrom;
        private readonly GDMDate fDateTo;

        public GDMDate DateFrom
        {
            get { return fDateFrom; }
        }

        public GDMDate DateTo
        {
            get { return fDateTo; }
        }


        public GDMDatePeriod()
        {
            fDateFrom = new GDMDate();
            fDateTo = new GDMDate();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fDateFrom.TrimExcess();
            fDateTo.TrimExcess();
        }

        protected override string GetStringValue()
        {
            string result;

            bool frEmpty = fDateFrom.IsEmpty();
            bool toEmpty = fDateTo.IsEmpty();

            if (!frEmpty) {
                if (!toEmpty) {
                    result = string.Concat("FROM ", fDateFrom.StringValue, " TO ", fDateTo.StringValue);
                } else {
                    result = "FROM " + fDateFrom.StringValue;
                }
            } else if (!toEmpty) {
                result = "TO " + fDateTo.StringValue;
            } else {
                result = "";
            }

            return result;
        }

        public override DateTime GetDateTime()
        {
            DateTime result;

            bool frEmpty = fDateFrom.IsEmpty();
            bool toEmpty = fDateTo.IsEmpty();

            if (frEmpty) {
                result = fDateTo.GetDateTime();
            } else if (toEmpty) {
                result = fDateFrom.GetDateTime();
            } else if (fDateFrom.GetDateTime() == fDateTo.GetDateTime()) {
                result = fDateFrom.GetDateTime();
            } else {
                result = new DateTime(0);
            }

            return result;
        }

        public override void SetDateTime(DateTime value)
        {
            // The risk of undefined behavior
            throw new NotSupportedException();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fDateFrom.Dispose();
                fDateTo.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            base.Clear();
            fDateFrom.Clear();
            fDateTo.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDateFrom.IsEmpty() && fDateTo.IsEmpty();
        }

        public override string ParseString(string strValue)
        {
            Clear();
            string result = string.IsNullOrEmpty(strValue) ? string.Empty : GEDCOMUtils.ParsePeriodDate(this, strValue);
            return result;
        }

        public override UDN GetUDN()
        {
            UDN result;

            bool frEmpty = fDateFrom.IsEmpty();
            bool toEmpty = fDateTo.IsEmpty();

            if (!frEmpty) {
                if (!toEmpty) {
                    result = UDN.CreateBetween(fDateFrom.GetUDN(), fDateTo.GetUDN(), false);
                } else {
                    result = UDN.CreateAfter(fDateFrom.GetUDN());
                }
            } else if (!toEmpty) {
                result = UDN.CreateBefore(fDateTo.GetUDN());
            } else {
                result = UDN.Unknown;
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar, bool shorten = false)
        {
            string result;

            bool frEmpty = fDateFrom.IsEmpty();
            bool toEmpty = fDateTo.IsEmpty();

            if (!frEmpty) {
                if (!toEmpty) {
                    result = fDateFrom.GetDisplayString(format, true, showCalendar) + " - " + fDateTo.GetDisplayString(format, true, showCalendar);
                } else {
                    result = fDateFrom.GetDisplayString(format, true, showCalendar);
                    if (sign) result += " >";
                }
            } else if (!toEmpty) {
                result = fDateTo.GetDisplayString(format, true, showCalendar);
                if (sign) result = "< " + result;
            } else {
                result = "";
            }

            return result;
        }

        public override void GetDateRange(out GDMDate dateStart, out GDMDate dateEnd)
        {
            dateStart = fDateFrom;
            dateEnd = fDateTo;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fDateFrom);
            hashCode.Add(fDateTo);
        }
    }
}
