/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;

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

        public override string ParseString(StringSpan strValue)
        {
            Clear();
            string result = (strValue.IsEmptyOrEnd) ? string.Empty : GEDCOMUtils.ParsePeriodDate(this, strValue);
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

        public override string GetDisplayString(DateFormat format, bool sign = false, bool showCalendar = false, bool shorten = false)
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
