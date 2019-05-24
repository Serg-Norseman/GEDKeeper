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
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMDateRange : GDMCustomDate
    {
        private GDMDate fDateAfter;
        private GDMDate fDateBefore;

        public GDMDate After
        {
            get { return fDateAfter; }
        }

        public GDMDate Before
        {
            get { return fDateBefore; }
        }


        public new static GDMTag Create(GDMObject owner, string tagName, string tagValue)
        {
            return new GDMDateRange(owner, tagName, tagValue);
        }

        public GDMDateRange(GDMObject owner) : base(owner)
        {
            fDateAfter = new GDMDate(this);
            fDateBefore = new GDMDate(this);
        }

        public GDMDateRange(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        protected override string GetStringValue()
        {
            string result;
            if (!fDateAfter.IsEmpty() && !fDateBefore.IsEmpty()) {
                result = string.Concat(GEDCOMDateRangeArray[2], " ", fDateAfter.StringValue, " ", GEDCOMDateRangeArray[3], " ", fDateBefore.StringValue);
            } else if (!fDateAfter.IsEmpty()) {
                result = GEDCOMDateRangeArray[0] + " " + fDateAfter.StringValue;
            } else if (!fDateBefore.IsEmpty()) {
                result = GEDCOMDateRangeArray[1] + " " + fDateBefore.StringValue;
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
            string result;
            if (string.IsNullOrEmpty(strValue)) {
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseRangeDate(GetTree(), this, strValue);
            }
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

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar)
        {
            string result = "";

            if (fDateAfter.StringValue == "" && fDateBefore.StringValue != "") {
                result = fDateBefore.GetDisplayString(format, true, showCalendar);
                if (sign) result = "< " + result;
            } else if (fDateAfter.StringValue != "" && fDateBefore.StringValue == "") {
                result = fDateAfter.GetDisplayString(format, true, showCalendar);
                if (sign) result += " >";
            } else if (fDateAfter.StringValue != "" && fDateBefore.StringValue != "") {
                result = fDateAfter.GetDisplayString(format, true, showCalendar) + " - " + fDateBefore.GetDisplayString(format, true, showCalendar);
            }

            return result;
        }
    }
}
