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
    public sealed class GEDCOMDateRange : GEDCOMCustomDate
    {
        private GEDCOMDate fDateAfter;
        private GEDCOMDate fDateBefore;

        public GEDCOMDate After
        {
            get { return fDateAfter; }
        }

        public GEDCOMDate Before
        {
            get { return fDateBefore; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            fDateAfter = new GEDCOMDate(owner, parent, "", "");
            fDateBefore = new GEDCOMDate(owner, parent, "", "");
        }

        protected override string GetStringValue()
        {
            string result;
            if (!fDateAfter.IsEmpty() && !fDateBefore.IsEmpty())
            {
                result = string.Concat(GEDCOMDateRangeArray[2], " ", fDateAfter.StringValue, " ", GEDCOMDateRangeArray[3], " ", fDateBefore.StringValue);
            }
            else
            {
                if (!fDateAfter.IsEmpty())
                {
                    result = GEDCOMDateRangeArray[0] + " " + fDateAfter.StringValue;
                }
                else
                {
                    if (!fDateBefore.IsEmpty())
                    {
                        result = GEDCOMDateRangeArray[1] + " " + fDateBefore.StringValue;
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
            if (fDateAfter.IsEmpty())
            {
                result = fDateBefore.GetDateTime();
            }
            else
            {
                if (fDateBefore.IsEmpty())
                {
                    result = fDateAfter.GetDateTime();
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
            // Format: AFT DATE, BEF DATE, BET AFT_DATE AND BEF_DATE

            fDateAfter.Clear();
            fDateBefore.Clear();

            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                string su = result.Substring(0, 3).ToUpperInvariant();

                if (su == GEDCOMDateRangeArray[0])
                {
                    result = result.Remove(0, 3);
                    result = GEDCOMUtils.ExtractDelimiter(result, 0);
                    result = fDateAfter.ParseString(result);
                }
                else
                {
                    if (su == GEDCOMDateRangeArray[1])
                    {
                        result = result.Remove(0, 3);
                        result = GEDCOMUtils.ExtractDelimiter(result, 0);
                        result = fDateBefore.ParseString(result);
                    }
                    else
                    {
                        if (su == GEDCOMDateRangeArray[2])
                        {
                            result = result.Remove(0, 3);
                            result = GEDCOMUtils.ExtractDelimiter(result, 0);
                            result = GEDCOMProvider.FixFTB(result);
                            result = fDateAfter.ParseString(result);
                            result = GEDCOMUtils.ExtractDelimiter(result, 0);

                            su = result.Substring(0, 3).ToUpper();

                            if (su == GEDCOMDateRangeArray[3])
                            {
                                result = result.Remove(0, 3);
                                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                                result = GEDCOMProvider.FixFTB(result);
                                result = fDateBefore.ParseString(result);
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
            if (fDateAfter != null) fDateAfter.ResetOwner(newOwner);
            if (fDateBefore != null) fDateBefore.ResetOwner(newOwner);
        }

        public GEDCOMDateRange(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMDateRange(owner, parent, tagName, tagValue);
        }

        public override UDN GetUDN()
        {
            UDN result;

            if (fDateAfter.StringValue == "" && fDateBefore.StringValue != "")
            {
                result = UDN.CreateBefore(fDateBefore.GetUDN());
            }
            else if (fDateAfter.StringValue != "" && fDateBefore.StringValue == "")
            {
                result = UDN.CreateAfter(fDateAfter.GetUDN());
            }
            else if (fDateAfter.StringValue != "" && fDateBefore.StringValue != "")
            {
                result = UDN.CreateBetween(fDateAfter.GetUDN(), fDateBefore.GetUDN());
            }
            else
            {
                result = UDN.CreateEmpty();
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar)
        {
            string result = "";

            if (fDateAfter.StringValue == "" && fDateBefore.StringValue != "")
            {
                result = fDateBefore.GetDisplayString(format, true, showCalendar);
                if (sign) result = "< " + result;
            }
            else if (fDateAfter.StringValue != "" && fDateBefore.StringValue == "")
            {
                result = fDateAfter.GetDisplayString(format, true, showCalendar);
                if (sign) result += " >";
            }
            else if (fDateAfter.StringValue != "" && fDateBefore.StringValue != "")
            {
                result = fDateAfter.GetDisplayString(format, true, showCalendar) + " - " + fDateBefore.GetDisplayString(format, true, showCalendar);
            }

            return result;
        }
    }
}
