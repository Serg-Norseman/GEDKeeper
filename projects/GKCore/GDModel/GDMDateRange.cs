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
    public sealed class GDMDateRange : GDMCustomDate
    {
        public static readonly GDMDateRange Empty = new GDMDateRange();


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

            bool aftEmpty = fDateAfter.IsEmpty();
            bool befEmpty = fDateBefore.IsEmpty();

            if (!aftEmpty) {
                if (!befEmpty) {
                    result = string.Concat(GEDCOMConsts.GEDCOMDateRangeArray[2], " ", fDateAfter.StringValue, " ", GEDCOMConsts.GEDCOMDateRangeArray[3], " ", fDateBefore.StringValue);
                } else {
                    result = GEDCOMConsts.GEDCOMDateRangeArray[0] + " " + fDateAfter.StringValue;
                }
            } else if (!befEmpty) {
                result = GEDCOMConsts.GEDCOMDateRangeArray[1] + " " + fDateBefore.StringValue;
            } else {
                result = "";
            }

            return result;
        }

        public override DateTime GetDateTime()
        {
            DateTime result;

            bool aftEmpty = fDateAfter.IsEmpty();
            bool befEmpty = fDateBefore.IsEmpty();

            if (aftEmpty) {
                result = fDateBefore.GetDateTime();
            } else if (befEmpty) {
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

            bool aftEmpty = fDateAfter.IsEmpty();
            bool befEmpty = fDateBefore.IsEmpty();

            if (!aftEmpty) {
                if (!befEmpty) {
                    result = UDN.CreateBetween(fDateAfter.GetUDN(), fDateBefore.GetUDN(), false);
                } else {
                    result = UDN.CreateAfter(fDateAfter.GetUDN());
                }
            } else if (!befEmpty) {
                result = UDN.CreateBefore(fDateBefore.GetUDN());
            } else {
                result = UDN.Unknown;
            }

            return result;
        }

        public override string GetDisplayStringExt(DateFormat format, bool sign, bool showCalendar, bool shorten = false)
        {
            string result;

            bool aftEmpty = fDateAfter.IsEmpty();
            bool befEmpty = fDateBefore.IsEmpty();

            if (!aftEmpty) {
                if (!befEmpty) {
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
                } else {
                    result = fDateAfter.GetDisplayString(format, true, showCalendar);
                    if (sign) result += " >";
                }
            } else if (!befEmpty) {
                result = fDateBefore.GetDisplayString(format, true, showCalendar);
                if (sign) result = "< " + result;
            } else {
                result = "";
            }

            return result;
        }

        public override void GetDateRange(out GDMDate dateStart, out GDMDate dateEnd)
        {
            dateStart = fDateAfter;
            dateEnd = fDateBefore;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fDateAfter);
            hashCode.Add(fDateBefore);
        }
    }
}
