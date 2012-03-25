using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public class TGEDCOMDateValue : TGEDCOMCustomDate
	{
		private TGEDCOMCustomDate FValue;

		public TGEDCOMCustomDate Value
		{
			get { return this.FValue; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FValue = null;
		}

		protected override string GetStringValue()
		{
			return ((this.FValue == null) ? "" : this.FValue.StringValue);
		}

		public override DateTime GetDateTime()
		{
			DateTime Result = ((this.FValue == null) ? new DateTime(0) : this.FValue.GetDateTime());
			return Result;
		}

		public override void SetDateTime(DateTime ADateTime)
		{
			if (this.FValue != null)
			{
				this.FValue.SetDateTime(ADateTime);
			}
			else
			{
				this.FValue = new TGEDCOMDateExact(base.Owner, this, "", "");
				this.FValue.Date = ADateTime;
			}
		}

		public override void Clear()
		{
			if (this.FValue != null) this.FValue.Clear();
		}

		public override bool IsEmpty()
		{
			return this.FValue == null || this.FValue.IsEmpty();
		}

		public override string ParseString([In] string S)
		{
			try
			{
				if (this.FValue != null)
				{
					this.FValue.Dispose();
					this.FValue = null;
				}

				if (string.IsNullOrEmpty(S))
				{
					return "";
				}

				string SU = S.Substring(0, 3).ToUpper();

				if (SU == TGEDCOMDate.GEDCOMDateApproximatedArray[1] || SU == TGEDCOMDate.GEDCOMDateApproximatedArray[2] || SU == TGEDCOMDate.GEDCOMDateApproximatedArray[3])
				{
					this.FValue = new TGEDCOMDateApproximated(base.Owner, this, "", "");
				}
				else
				{
					if (SU == "INT")
					{
						this.FValue = new TGEDCOMDateInterpreted(base.Owner, this, "", "");
					}
					else
					{
						if (SU == TGEDCOMDate.GEDCOMDateRangeArray[0] || SU == TGEDCOMDate.GEDCOMDateRangeArray[1] || SU == TGEDCOMDate.GEDCOMDateRangeArray[2])
						{
							this.FValue = new TGEDCOMDateRange(base.Owner, this, "", "");
						}
						else
						{
							// checkit: this safe (by indexes and length), but std validness?
							if (S.IndexOf("FROM", 0) == 0 || S.IndexOf("TO", 0) == 0)
							{
								this.FValue = new TGEDCOMDatePeriod(base.Owner, this, "", "");
							}
							else
							{
								this.FValue = new TGEDCOMDate(base.Owner, this, "", "");
							}
						}
					}
				}

				return this.FValue.ParseString(S);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGEDCOMDateValue.ParseString(\"" + S + "\"): " + E.Message);
				return S;
			}
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FValue != null) this.FValue.ResetOwner(AOwner);
		}

		public float IsMatch(TGEDCOMDateValue date)
		{
			float match = 0.0F;

			if (date != null) {
				int year1, year2;
				ushort month1, day1, month2, day2;
				this.aux_GetIndependentDate(out year1, out month1, out day1);
				date.aux_GetIndependentDate(out year2, out month2, out day2);

				float matches = 0;
				if (year1 == year2) matches++;
				if (month1 == month2) matches++;
				if (day1 == day2) matches++;
				match = (matches / 3.0F) * 100.0F;
			}

			return match;
		}

		public void aux_GetIndependentDate(out int AYear, out ushort AMonth, out ushort ADay)
		{
			bool BC;
			aux_GetIndependentDate(out AYear, out AMonth, out ADay, out BC);
		}

		public void aux_GetIndependentDate(out int AYear, out ushort AMonth, out ushort ADay, out bool YearBC)
		{
			AYear = -1;
			AMonth = 0;
			ADay = 0;
			YearBC = false;

			if (FValue is TGEDCOMDateApproximated)
			{
				TGEDCOMDate dt = (FValue as TGEDCOMDate);
				dt.GetDate(out AYear, out AMonth, out ADay);
				YearBC = dt.YearBC;
			}
			else
			{
				if (FValue is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dt_range = FValue as TGEDCOMDateRange;
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
					if (FValue is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dt_period = FValue as TGEDCOMDatePeriod;
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
						if (FValue is TGEDCOMDate)
						{
							TGEDCOMDate dt = (FValue as TGEDCOMDate);
							dt.GetDate(out AYear, out AMonth, out ADay);
							YearBC = dt.YearBC;
						}
					}
				}
			}
		}

		public TGEDCOMDateValue(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMDateValue(AOwner, AParent, AName, AValue);
		}
	}
}
