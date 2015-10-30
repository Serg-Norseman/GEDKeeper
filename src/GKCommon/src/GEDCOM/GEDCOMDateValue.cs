using System;
using ExtUtils;

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

				string SU = strValue.Substring(0, 3).ToUpperInvariant();

				if (SU == GEDCOMDateApproximatedArray[1] || SU == GEDCOMDateApproximatedArray[2] || SU == GEDCOMDateApproximatedArray[3])
				{
					this.fValue = new GEDCOMDateApproximated(base.Owner, this, "", "");
				}
				else if (SU == "INT")
				{
					this.fValue = new GEDCOMDateInterpreted(base.Owner, this, "", "");
				}
				else if (SU == GEDCOMDateRangeArray[0] || SU == GEDCOMDateRangeArray[1] || SU == GEDCOMDateRangeArray[2])
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
				SysUtils.LogWrite("GEDCOMDateValue.ParseString(\"" + strValue + "\"): " + ex.Message);
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

			int year1, year2;
			ushort month1, day1, month2, day2;
			this.GetIndependentDate(out year1, out month1, out day1);
			date.GetIndependentDate(out year2, out month2, out day2);

			float match = 0.0f;
			float matches = 0.0f;
			if (year1 >= 0 && year2 >= 0) {
				matches++;
				if (Math.Abs(year1 - year2) <= matchParams.YearsInaccuracy) match = 100.0f;
			}

			/*if (month1 == month2) matches++;
			if (day1 == day2) matches++;*/

			match = (match / matches);
			return match;
		}

		// FIXME
		public void GetIndependentDate(out int year, out ushort month, out ushort day)
		{
			bool yearBC;
			GetIndependentDate(out year, out month, out day, out yearBC);
		}

		// FIXME
		public void GetIndependentDate(out int year, out ushort month, out ushort day, out bool yearBC)
		{
			GEDCOMUtils.GetDateParts(this.Value, out year, out month, out day, out yearBC);
		}

        #endregion
	}
}
