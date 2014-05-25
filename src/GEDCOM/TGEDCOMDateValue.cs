using System;
using ExtUtils;

namespace GedCom551
{
	public class TGEDCOMDateValue : TGEDCOMCustomDate
	{
		private TGEDCOMCustomDate fValue;

		public TGEDCOMCustomDate Value
		{
			get { return this.fValue; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
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
				this.fValue = new TGEDCOMDateExact(base.Owner, this, "", "");
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
					this.fValue = new TGEDCOMDateApproximated(base.Owner, this, "", "");
				}
				else if (SU == "INT")
				{
					this.fValue = new TGEDCOMDateInterpreted(base.Owner, this, "", "");
				}
				else if (SU == GEDCOMDateRangeArray[0] || SU == GEDCOMDateRangeArray[1] || SU == GEDCOMDateRangeArray[2])
				{
					this.fValue = new TGEDCOMDateRange(base.Owner, this, "", "");
				}
				else if (strValue.StartsWith("FROM", StringComparison.InvariantCulture) || strValue.StartsWith("TO", StringComparison.InvariantCulture))
				{
					this.fValue = new TGEDCOMDatePeriod(base.Owner, this, "", "");
				}
				else
				{
					this.fValue = new TGEDCOMDate(base.Owner, this, "", "");
				}

				return this.fValue.ParseString(strValue);
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TGEDCOMDateValue.ParseString(\"" + strValue + "\"): " + ex.Message);
				return strValue;
			}
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			if (this.fValue != null) this.fValue.ResetOwner(newOwner);
		}

		public override float IsMatch(TGEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			TGEDCOMDateValue date = (TGEDCOMDateValue)tag;

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

			if (fValue is TGEDCOMDateApproximated)
			{
				TGEDCOMDate dt = (fValue as TGEDCOMDate);
				dt.GetDate(out AYear, out AMonth, out ADay);
				YearBC = dt.YearBC;
			}
			else
			{
				if (fValue is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dt_range = fValue as TGEDCOMDateRange;
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
					if (fValue is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dt_period = fValue as TGEDCOMDatePeriod;
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
						if (fValue is TGEDCOMDate)
						{
							TGEDCOMDate dt = (fValue as TGEDCOMDate);
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
				SysUtils.LogWrite("TGEDCOMDateValue.aux_GetDate(" + this.StringValue + "): " + ex.Message);
				res = new DateTime(0);
			}

			return res;
		}

		public TGEDCOMDateValue(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMDateValue(owner, parent, tagName, tagValue);
		}
	}
}
