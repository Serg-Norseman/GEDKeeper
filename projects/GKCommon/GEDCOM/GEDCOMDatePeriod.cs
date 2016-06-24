/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMDatePeriod : GEDCOMCustomDate
    {
        private GEDCOMDate fDateFrom;
        private GEDCOMDate fDateTo;

        public GEDCOMDate DateFrom
        {
            get { return this.fDateFrom; }
        }

        public GEDCOMDate DateTo
        {
            get { return this.fDateTo; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.fDateFrom = new GEDCOMDate(owner, this, "", "");
            this.fDateTo = new GEDCOMDate(owner, this, "", "");
        }

        protected override string GetStringValue()
        {
            string result;

            if (!this.fDateFrom.IsEmpty() && !this.fDateTo.IsEmpty())
            {
                result = string.Concat("FROM ", this.fDateFrom.StringValue, " TO ", this.fDateTo.StringValue);
            }
            else
            {
                if (!this.fDateFrom.IsEmpty())
                {
                    result = "FROM " + this.fDateFrom.StringValue;
                }
                else
                {
                    if (!this.fDateTo.IsEmpty())
                    {
                        result = "TO " + this.fDateTo.StringValue;
                    }
                    else
                    {
                        result = "";
                    }
                }
            }
            return result;
        }

        public override DateTime GetDateTime()
        {
            DateTime result;
            if (this.fDateFrom.IsEmpty())
            {
                result = this.fDateTo.GetDateTime();
            }
            else
            {
                if (this.fDateTo.IsEmpty())
                {
                    result = this.fDateFrom.GetDateTime();
                }
                else
                {
                    if (this.fDateFrom.GetDateTime() == this.fDateTo.GetDateTime())
                    {
                        result = this.fDateFrom.GetDateTime();
                    }
                    else
                    {
                        result = new DateTime(0);
                    }
                }
            }
            return result;
        }

        public override void SetDateTime(DateTime value)
        {
            if (!this.fDateFrom.IsEmpty() && this.fDateTo.IsEmpty())
            {
                this.fDateFrom.SetDateTime(value);
            }
            else
            {
                if (!this.fDateTo.IsEmpty() && this.fDateFrom.IsEmpty())
                {
                    this.fDateTo.SetDateTime(value);
                }
                else
                {
                    this.fDateFrom.SetDateTime(value);
                    this.fDateTo.SetDateTime(value);
                }
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fDateFrom.Dispose();
                this.fDateTo.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            this.fDateFrom.Clear();
            this.fDateTo.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fDateFrom.IsEmpty() && this.fDateTo.IsEmpty();
        }

        public override string ParseString(string strValue)
        {
            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                if (result.StartsWith("FROM"))
                {
                    result = result.Remove(0, 4);
                    result = GEDCOMUtils.ExtractDelimiter(result, 0);
                    result = this.fDateFrom.ParseString(result);
                    result = GEDCOMUtils.ExtractDelimiter(result, 0);
                }
                if (result.StartsWith("TO"))
                {
                    result = result.Remove(0, 2);
                    result = GEDCOMUtils.ExtractDelimiter(result, 0);
                    result = this.fDateTo.ParseString(result);
                }
            }
            return result;
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            if (this.fDateFrom != null)
            {
                this.fDateFrom.ResetOwner(newOwner);
            }
            if (this.fDateTo != null)
            {
                this.fDateTo.ResetOwner(newOwner);
            }
        }

        public GEDCOMDatePeriod(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMDatePeriod(owner, parent, tagName, tagValue);
        }

        public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
        {
            year = -1;
            month = 0;
            day = 0;
            yearBC = false;

            if (this.fDateFrom.StringValue != "" && this.fDateTo.StringValue == "")
            {
                this.fDateFrom.GetDateParts(out year, out month, out day, out yearBC);
            }
            else if (this.fDateFrom.StringValue == "" && this.fDateTo.StringValue != "")
            {
                this.fDateTo.GetDateParts(out year, out month, out day, out yearBC);
            }
            else if (this.fDateFrom.StringValue != "" && this.fDateTo.StringValue != "")
            {
                this.fDateFrom.GetDateParts(out year, out month, out day, out yearBC);
            }
        }

        public override UDN GetUDN()
        {
            UDN result;

            if (this.fDateFrom.StringValue != "" && this.fDateTo.StringValue == "")
            {
                result = UDN.CreateAfter(this.fDateFrom.GetUDN());
            }
            else if (this.fDateFrom.StringValue == "" && this.fDateTo.StringValue != "")
            {
                result = UDN.CreateBefore(this.fDateTo.GetUDN());
            }
            else if (this.fDateFrom.StringValue != "" && this.fDateTo.StringValue != "")
            {
                result = UDN.CreateBetween(this.fDateFrom.GetUDN(), this.fDateTo.GetUDN());
            }
            else
            {
                result = UDN.CreateEmpty();
            }

            return result;
        }
    }
}
