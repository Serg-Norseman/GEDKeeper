/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Calendar;

namespace GDModel
{
    /// <summary>
    /// This class is an envelope/wrapper for different types of dates.
    /// </summary>
    public class GDMDateValue : GDMCustomDate
    {
        private GDMCustomDate fValue;

        public GDMCustomDate Value
        {
            get { return fValue; }
        }


        public GDMDateValue()
        {
            fValue = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fValue != null) fValue.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            if (fValue != null) {
                fValue.TrimExcess();
            }
        }

        protected override string GetStringValue()
        {
            return (fValue == null) ? string.Empty : fValue.StringValue;
        }

        public override DateTime GetDateTime()
        {
            return (fValue == null) ? new DateTime(0) : fValue.GetDateTime();
        }

        public override void SetDateTime(DateTime value)
        {
            if (fValue != null) {
                fValue.SetDateTime(value);
            } else {
                fValue = new GDMDate();
                fValue.Date = value;
            }
        }

        public override void Clear()
        {
            if (fValue != null) fValue.Clear();
        }

        public override bool IsEmpty()
        {
            return (fValue == null || fValue.IsEmpty());
        }

        public override string ParseString(string strValue)
        {
            try {
                if (fValue != null) {
                    fValue.Dispose();
                    fValue = null;
                }

                return string.IsNullOrEmpty(strValue) ? string.Empty : GEDCOMUtils.ParseDateValue(null, this, strValue);
            } catch (Exception ex) {
                Logger.WriteError("GDMDateValue.ParseString(\"" + strValue + "\")", ex);
                return strValue;
            }
        }

        public override string ParseString(StringSpan strValue)
        {
            try {
                if (fValue != null) {
                    fValue.Dispose();
                    fValue = null;
                }

                return (strValue.IsEmptyOrEnd) ? string.Empty : GEDCOMUtils.ParseDateValue(null, this, strValue);
            } catch (Exception ex) {
                Logger.WriteError("GDMDateValue.ParseString(\"" + strValue + "\")", ex);
                return strValue;
            }
        }

        /// <summary>
        /// Internal helper method for parser
        /// </summary>
        internal void SetRawData(GDMCustomDate value)
        {
            fValue = value;
        }

        /// <summary>
        /// This function compares dates only by chronological year.
        /// Month and day are not taken into account, the year is compared with the calendar.
        /// </summary>
        /// <param name="tag"></param>
        /// <param name="matchParams"></param>
        /// <returns></returns>
        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            if (tag == null) return 0.0f;
            GDMDateValue date = (GDMDateValue)tag;

            if (IsEmpty() || date.IsEmpty()) return 0.0f;

            int absVal1 = this.GetChronologicalYear();
            int absVal2 = date.GetChronologicalYear();

            float match = 0.0f;
            float matches = 0.0f;

            if (absVal1 != 0 && absVal2 != 0) {
                matches += 1.0f;
                if (Math.Abs(absVal1 - absVal2) <= matchParams.YearsInaccuracy) match += 100.0f;
            }

            return (match / matches);
        }

        public override UDN GetUDN()
        {
            return (fValue == null) ? UDN.Unknown : fValue.GetUDN();
        }

        /// <summary>
        /// In the historical chronology of the year 0 does not exist.
        /// Therefore, the digit 0 in the year value can be used as a sign of lack or error.
        /// ChronologicalYear - introduced for the purposes of uniform chronology years in the Gregorian calendar.
        /// Is estimated from -4714 BC to 3268 AD.
        /// </summary>
        /// <returns>chronological year</returns>
        public override int GetChronologicalYear()
        {
            return (fValue == null) ? 0 : fValue.GetChronologicalYear();
        }

        public override string GetDisplayString(DateFormat format, bool sign = false, bool showCalendar = false, bool shorten = false)
        {
            return (fValue == null) ? string.Empty : fValue.GetDisplayString(format, sign, showCalendar, shorten);
        }

        public override string ToString()
        {
            return GetDisplayString(DateFormat.dfDD_MM_YYYY, true, true, false);
        }

        public override void GetDateRange(out GDMDate dateStart, out GDMDate dateEnd)
        {
            if (fValue == null) {
                dateStart = null;
                dateEnd = null;
            } else {
                fValue.GetDateRange(out dateStart, out dateEnd);
            }
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            hashCode.Add(fValue);
        }
    }
}
