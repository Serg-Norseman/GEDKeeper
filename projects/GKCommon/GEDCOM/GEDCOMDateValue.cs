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
    public class GEDCOMDateValue : GEDCOMCustomDate
    {
        private GEDCOMCustomDate fValue;

        public GEDCOMCustomDate Value
        {
            get { return this.fValue; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.fValue = null;
        }

        protected override string GetStringValue()
        {
            return ((this.fValue == null) ? "" : this.fValue.StringValue);
        }

        public override DateTime GetDateTime()
        {
            DateTime result = ((this.fValue == null) ? new DateTime(0) : this.fValue.GetDateTime());
            return result;
        }

        public override void SetDateTime(DateTime value)
        {
            if (this.fValue != null)
            {
                this.fValue.SetDateTime(value);
            }
            else
            {
                this.fValue = new GEDCOMDateExact(base.Owner, this, "", "");
                this.fValue.Date = value;
            }
        }

        public override void Clear()
        {
            if (this.fValue != null) this.fValue.Clear();
        }

        public override bool IsEmpty()
        {
            return this.fValue == null || this.fValue.IsEmpty();
        }

        public override string ParseString(string strValue)
        {
            try
            {
                if (this.fValue != null) {
                    this.fValue.Dispose();
                    this.fValue = null;
                }

                if (string.IsNullOrEmpty(strValue)) {
                    return "";
                }

                string su = strValue.Substring(0, 3).ToUpperInvariant();

                if (su == GEDCOMDateApproximatedArray[1] || su == GEDCOMDateApproximatedArray[2] || su == GEDCOMDateApproximatedArray[3])
                {
                    this.fValue = new GEDCOMDateApproximated(base.Owner, this, "", "");
                }
                else if (su == "INT")
                {
                    this.fValue = new GEDCOMDateInterpreted(base.Owner, this, "", "");
                }
                else if (su == GEDCOMDateRangeArray[0] || su == GEDCOMDateRangeArray[1] || su == GEDCOMDateRangeArray[2])
                {
                    this.fValue = new GEDCOMDateRange(base.Owner, this, "", "");
                }
                else if (strValue.StartsWith("FROM", StringComparison.InvariantCulture) || strValue.StartsWith("TO", StringComparison.InvariantCulture))
                {
                    this.fValue = new GEDCOMDatePeriod(base.Owner, this, "", "");
                }
                else
                {
                    this.fValue = new GEDCOMDate(base.Owner, this, "", "");
                }

                return this.fValue.ParseString(strValue);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GEDCOMDateValue.ParseString(\"" + strValue + "\"): " + ex.Message);
                return strValue;
            }
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            if (this.fValue != null) this.fValue.ResetOwner(newOwner);
        }

        public GEDCOMDateValue(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMDateValue(owner, parent, tagName, tagValue);
        }

        #region Auxiliary

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            if (tag == null) return 0.0f;
            GEDCOMDateValue date = (GEDCOMDateValue)tag;

            if (this.IsEmpty() || date.IsEmpty()) return 0.0f;

            int absVal1 = GEDCOMUtils.GetRelativeYear(this);
            int absVal2 = GEDCOMUtils.GetRelativeYear(date);

            float match = 0.0f;
            float matches = 0.0f;

            if (absVal1 != 0 && absVal2 != 0)
            {
                matches += 1.0f;
                if (Math.Abs(absVal1 - absVal2) <= matchParams.YearsInaccuracy) match += 100.0f;
            }

            return (match / matches);
        }

        public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
        {
            if (this.fValue == null) {
                year = -1;
                month = 0;
                day = 0;
                yearBC = false;
            } else {
                this.fValue.GetDateParts(out year, out month, out day, out yearBC);
            }
        }

        public override UDN GetUDN()
        {
            return (this.fValue == null) ? UDN.CreateEmpty() : this.fValue.GetUDN();
        }

        /*public override AbsDate GetAbstractDate()
        {
            return (this.fValue == null) ? AbsDate.Empty() : this.fValue.GetAbstractDate();
        }*/

        #endregion
    }
}
