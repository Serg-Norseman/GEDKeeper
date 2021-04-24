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
using BSLib.Calendar;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMDatePeriod : GDMCustomDate
    {
        private GDMDate fDateFrom;
        private GDMDate fDateTo;

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
            if (!fDateFrom.IsEmpty() && !fDateTo.IsEmpty()) {
                result = string.Concat("FROM ", fDateFrom.StringValue, " TO ", fDateTo.StringValue);
            } else if (!fDateFrom.IsEmpty()) {
                result = "FROM " + fDateFrom.StringValue;
            } else if (!fDateTo.IsEmpty()) {
                result = "TO " + fDateTo.StringValue;
            } else {
                result = "";
            }
            return result;
        }

        public override DateTime GetDateTime()
        {
            DateTime result;
            if (fDateFrom.IsEmpty()) {
                result = fDateTo.GetDateTime();
            } else if (fDateTo.IsEmpty()) {
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

            if (fDateFrom.StringValue != "" && fDateTo.StringValue == "") {
                result = UDN.CreateAfter(fDateFrom.GetUDN());
            } else if (fDateFrom.StringValue == "" && fDateTo.StringValue != "") {
                result = UDN.CreateBefore(fDateTo.GetUDN());
            } else if (fDateFrom.StringValue != "" && fDateTo.StringValue != "") {
                result = UDN.CreateBetween(fDateFrom.GetUDN(), fDateTo.GetUDN());
            } else {
                result = UDN.CreateEmpty();
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar)
        {
            string result = "";

            if (fDateFrom.StringValue != "" && fDateTo.StringValue == "") {
                result = fDateFrom.GetDisplayString(format, true, showCalendar);
                if (sign) result += " >";
            } else if (fDateFrom.StringValue == "" && fDateTo.StringValue != "") {
                result = fDateTo.GetDisplayString(format, true, showCalendar);
                if (sign) result = "< " + result;
            } else if (fDateFrom.StringValue != "" && fDateTo.StringValue != "") {
                result = fDateFrom.GetDisplayString(format, true, showCalendar) + " - " + fDateTo.GetDisplayString(format, true, showCalendar);
            }

            return result;
        }
    }
}
