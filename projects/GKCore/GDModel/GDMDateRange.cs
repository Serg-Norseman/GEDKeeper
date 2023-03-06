/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#define ShortenDateRanges

using System;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMDateRange : GDMCustomDate
    {
        private readonly GDMDate fDateAfter;
        private readonly GDMDate fDateBefore;

        public GDMDate After
        {
            get { return fDateAfter; }
        }

        public GDMDate Before
        {
            get { return fDateBefore; }
        }


        public GDMDateRange()
        {
            fDateAfter = new GDMDate();
            fDateBefore = new GDMDate();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fDateAfter.TrimExcess();
            fDateBefore.TrimExcess();
        }

        protected override string GetStringValue()
        {
            string result;
            if (!fDateAfter.IsEmpty() && !fDateBefore.IsEmpty()) {
                result = string.Concat(GEDCOMConsts.GEDCOMDateRangeArray[2], " ", fDateAfter.StringValue, " ", GEDCOMConsts.GEDCOMDateRangeArray[3], " ", fDateBefore.StringValue);
            } else if (!fDateAfter.IsEmpty()) {
                result = GEDCOMConsts.GEDCOMDateRangeArray[0] + " " + fDateAfter.StringValue;
            } else if (!fDateBefore.IsEmpty()) {
                result = GEDCOMConsts.GEDCOMDateRangeArray[1] + " " + fDateBefore.StringValue;
            } else {
                result = "";
            }
            return result;
        }

        public override DateTime GetDateTime()
        {
            DateTime result;
            if (fDateAfter.IsEmpty()) {
                result = fDateBefore.GetDateTime();
            } else if (fDateBefore.IsEmpty()) {
                result = fDateAfter.GetDateTime();
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
                fDateAfter.Dispose();
                fDateBefore.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            base.Clear();

            fDateAfter.Clear();
            fDateBefore.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDateAfter.IsEmpty() && fDateBefore.IsEmpty();
        }

        public override string ParseString(string strValue)
        {
            Clear();
            string result = string.IsNullOrEmpty(strValue) ? string.Empty : GEDCOMUtils.ParseRangeDate(this, strValue);
            return result;
        }

        public override UDN GetUDN()
        {
            UDN result;

            if (fDateAfter.StringValue == "" && fDateBefore.StringValue != "") {
                result = UDN.CreateBefore(fDateBefore.GetUDN());
            } else if (fDateAfter.StringValue != "" && fDateBefore.StringValue == "") {
                result = UDN.CreateAfter(fDateAfter.GetUDN());
            } else if (fDateAfter.StringValue != "" && fDateBefore.StringValue != "") {
                result = UDN.CreateBetween(fDateAfter.GetUDN(), fDateBefore.GetUDN());
            } else {
                result = UDN.CreateEmpty();
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar, bool shorten = false)
        {
            string result = "";

            if (fDateAfter.StringValue == "" && fDateBefore.StringValue != "") {
                result = fDateBefore.GetDisplayString(format, true, showCalendar);
                if (sign) result = "< " + result;
            } else if (fDateAfter.StringValue != "" && fDateBefore.StringValue == "") {
                result = fDateAfter.GetDisplayString(format, true, showCalendar);
                if (sign) result += " >";
            } else if (fDateAfter.StringValue != "" && fDateBefore.StringValue != "") {
                var dateAfter = fDateAfter.GetDisplayString(format, true, showCalendar);
                var dateBefore = fDateBefore.GetDisplayString(format, true, showCalendar);

                if (shorten) {
                    // FIXME: bad algorithm!
                    string dtA = dateAfter.Replace("__.__.", "");
                    string dtB = dateBefore.Replace("__.__.", "");
                    if (dtA.Length == 4 && dtB.Length == 4 && dtB.StartsWith(dtA.Substring(0, 2))) {
                        result = dtA + "/" + dtB.Substring(2);
                    } else {
                        result = dateAfter + " - " + dateBefore;
                    }
                } else {
                    result = dateAfter + " - " + dateBefore;
                }
            }

            return result;
        }
    }
}
