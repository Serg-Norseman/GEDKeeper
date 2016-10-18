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
    public sealed class GEDCOMDateRange : GEDCOMCustomDate
    {
        private GEDCOMDate fDateAfter;
        private GEDCOMDate fDateBefore;

        public GEDCOMDate After
        {
            get { return this.fDateAfter; }
        }

        public GEDCOMDate Before
        {
            get { return this.fDateBefore; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.fDateAfter = new GEDCOMDate(owner, parent, "", "");
            this.fDateBefore = new GEDCOMDate(owner, parent, "", "");
        }

        protected override string GetStringValue()
        {
            string result;
            if (!this.fDateAfter.IsEmpty() && !this.fDateBefore.IsEmpty())
            {
                result = string.Concat(GEDCOMDateRangeArray[2], " ", this.fDateAfter.StringValue, " ", GEDCOMDateRangeArray[3], " ", this.fDateBefore.StringValue);
            }
            else
            {
                if (!this.fDateAfter.IsEmpty())
                {
                    result = GEDCOMDateRangeArray[0] + " " + this.fDateAfter.StringValue;
                }
                else
                {
                    if (!this.fDateBefore.IsEmpty())
                    {
                        result = GEDCOMDateRangeArray[1] + " " + this.fDateBefore.StringValue;
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
            if (this.fDateAfter.IsEmpty())
            {
                result = this.fDateBefore.GetDateTime();
            }
            else
            {
                if (this.fDateBefore.IsEmpty())
                {
                    result = this.fDateAfter.GetDateTime();
                }
                else
                {
                    result = new DateTime(0);
                }
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
            if (disposing)
            {
                this.fDateAfter.Dispose();
                this.fDateBefore.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            base.Clear();

            this.fDateAfter.Clear();
            this.fDateBefore.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fDateAfter.IsEmpty() && this.fDateBefore.IsEmpty();
        }

        public override string ParseString(string strValue)
        {
            this.fDateAfter.Clear();
            this.fDateBefore.Clear();

            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                string su = result.Substring(0, 3).ToUpperInvariant();

                if (su == GEDCOMDateRangeArray[0])
                {
                    result = result.Remove(0, 3);
                    result = GEDCOMUtils.ExtractDelimiter(result, 0);
                    result = this.fDateAfter.ParseString(result);
                }
                else
                {
                    if (su == GEDCOMDateRangeArray[1])
                    {
                        result = result.Remove(0, 3);
                        result = GEDCOMUtils.ExtractDelimiter(result, 0);
                        result = this.fDateBefore.ParseString(result);
                    }
                    else
                    {
                        if (su == GEDCOMDateRangeArray[2])
                        {
                            result = result.Remove(0, 3);
                            result = GEDCOMUtils.ExtractDelimiter(result, 0);
                            result = GEDCOMUtils.FixFTB(result);
                            result = this.fDateAfter.ParseString(result);
                            result = GEDCOMUtils.ExtractDelimiter(result, 0);

                            su = result.Substring(0, 3).ToUpper();

                            if (su == GEDCOMDateRangeArray[3])
                            {
                                result = result.Remove(0, 3);
                                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                                result = GEDCOMUtils.FixFTB(result);
                                result = this.fDateBefore.ParseString(result);
                            }
                        }
                    }
                }
            }
            return result;
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            if (this.fDateAfter != null) this.fDateAfter.ResetOwner(newOwner);
            if (this.fDateBefore != null) this.fDateBefore.ResetOwner(newOwner);
        }

        public GEDCOMDateRange(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
        {
            year = -1;
            month = 0;
            day = 0;
            yearBC = false;

            if (this.fDateAfter.StringValue == "" && this.fDateBefore.StringValue != "")
            {
                this.fDateBefore.GetDateParts(out year, out month, out day, out yearBC);
            }
            else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue == "")
            {
                this.fDateAfter.GetDateParts(out year, out month, out day, out yearBC);
            }
            else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue != "")
            {
                this.fDateAfter.GetDateParts(out year, out month, out day, out yearBC);
            }
        }

        public override UDN GetUDN()
        {
            UDN result;

            if (this.fDateAfter.StringValue == "" && this.fDateBefore.StringValue != "")
            {
                result = UDN.CreateBefore(this.fDateBefore.GetUDN());
            }
            else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue == "")
            {
                result = UDN.CreateAfter(this.fDateAfter.GetUDN());
            }
            else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue != "")
            {
                result = UDN.CreateBetween(this.fDateAfter.GetUDN(), this.fDateBefore.GetUDN());
            }
            else
            {
                result = UDN.CreateEmpty();
            }

            return result;
        }
    }
}
