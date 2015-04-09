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

		public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			GEDCOMDateValue date = (GEDCOMDateValue)tag;

			if (this.IsEmpty() || date.IsEmpty()) return 0.0f;

			int year1, year2;
			ushort month1, day1, month2, day2;
			this.aux_GetIndependentDate(out year1, out month1, out day1);
			date.aux_GetIndependentDate(out year2, out month2, out day2);

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

		public void aux_GetIndependentDate(out int AYear, out ushort AMonth, out ushort ADay)
		{
			bool BC;
			this.aux_GetIndependentDate(out AYear, out AMonth, out ADay, out BC);
		}

		public void aux_GetIndependentDate(out int AYear, out ushort AMonth, out ushort ADay, out bool YearBC)
		{
			AYear = -1;
			AMonth = 0;
			ADay = 0;
			YearBC = false;

			if (fValue is GEDCOMDateApproximated)
			{
				GEDCOMDate dt = (fValue as GEDCOMDate);
				dt.GetDate(out AYear, out AMonth, out ADay);
				YearBC = dt.YearBC;
			}
			else
			{
				if (fValue is GEDCOMDateRange)
				{
					GEDCOMDateRange dt_range = fValue as GEDCOMDateRange;
					if (dt_range.After.StringValue == "" && dt_range.Before.StringValue != "")
					{
						dt_range.Before.GetDate(out AYear, out AMonth, out ADay);
						YearBC = dt_range.Before.YearBC;
					}
					else
					{
						if (dt_range.After.StringValue != "" && dt_range.Before.StringValue == "")
						{
							dt_range.After.GetDate(out AYear, out AMonth, out ADay);
							YearBC = dt_range.After.YearBC;
						}
						else
						{
							if (dt_range.After.StringValue != "" && dt_range.Before.StringValue != "")
							{
								dt_range.After.GetDate(out AYear, out AMonth, out ADay);
								YearBC = dt_range.After.YearBC;
							}
						}
					}
				}
				else
				{
					if (fValue is GEDCOMDatePeriod)
					{
						GEDCOMDatePeriod dt_period = fValue as GEDCOMDatePeriod;
						if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue == "")
						{
							dt_period.DateFrom.GetDate(out AYear, out AMonth, out ADay);
							YearBC = dt_period.DateFrom.YearBC;
						}
						else
						{
							if (dt_period.DateFrom.StringValue == "" && dt_period.DateTo.StringValue != "")
							{
								dt_period.DateTo.GetDate(out AYear, out AMonth, out ADay);
								YearBC = dt_period.DateTo.YearBC;
							}
							else
							{
								if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue != "")
								{
									dt_period.DateFrom.GetDate(out AYear, out AMonth, out ADay);
									YearBC = dt_period.DateFrom.YearBC;
								}
							}
						}
					}
					else
					{
						if (fValue is GEDCOMDate)
						{
							GEDCOMDate dt = (fValue as GEDCOMDate);
							dt.GetDate(out AYear, out AMonth, out ADay);
							YearBC = dt.YearBC;
						}
					}
				}
			}
		}

		public DateTime aux_GetDate()
		{
			DateTime res;

			try
			{
				int year;
				ushort month, day;
				this.aux_GetIndependentDate(out year, out month, out day);
				if (day == 0) day = 1;
				if (month == 0) month = 1;

				res = ((year <= 0) ? new DateTime(0) : new DateTime(year, (int)month, (int)day));
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GEDCOMDateValue.aux_GetDate(" + this.StringValue + "): " + ex.Message);
				res = new DateTime(0);
			}

			return res;
		}

		public GEDCOMDateValue(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMDateValue(owner, parent, tagName, tagValue);
		}
	}
}
