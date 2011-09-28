using System;
using System.Runtime.InteropServices;

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
			if (this.FValue != null)
			{
				this.FValue.Free();
				this.FValue = null;
			}

			if (string.IsNullOrEmpty(S)) {
				return "";
			}

			string SU = S.Substring(0, 3).ToUpper();

			if (SU == TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[1] || 
			    SU == TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[2] || 
			    SU == TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[3])
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
					if (SU == TGEDCOMDateRange.GEDCOMDateRangeArray[0] || 
					    SU == TGEDCOMDateRange.GEDCOMDateRangeArray[1] || 
					    SU == TGEDCOMDateRange.GEDCOMDateRangeArray[2])
					{
						this.FValue = new TGEDCOMDateRange(base.Owner, this, "", "");
					}
					else
					{
						if (S.Substring(0, 4).ToUpper() == "FROM" || 
						    S.Substring(0, 2).ToUpper() == "TO")
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
