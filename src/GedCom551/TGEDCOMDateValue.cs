using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public class TGEDCOMDateValue : TGEDCOMCustomDate
	{
		private TGEDCOMCustomDate FValue;

		public TGEDCOMCustomDate Value
		{
			get { return this.FValue; }
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FValue = null;
		}

		protected override string GetStringValue()
		{
			string Result;
			if (this.FValue != null)
			{
				Result = this.FValue.StringValue;
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public override DateTime GetDateTime()
		{
			DateTime Result;
			if (this.FValue != null)
			{
				Result = this.FValue.GetDateTime();
			}
			else
			{
				Result = new DateTime(0);
			}
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
			if (this.FValue != null)
			{
				this.FValue.Clear();
			}
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
					this.FValue.Free();
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
				SysUtils.LogWrite("TGEDCOMDateValue.ParseString(): " + E.Message);
				return S;
			}
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FValue != null)
			{
				this.FValue.ResetOwner(AOwner);
			}
		}

		public TGEDCOMDateValue(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
