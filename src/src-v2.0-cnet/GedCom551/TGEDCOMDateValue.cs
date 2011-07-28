using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMDateValue : TGEDCOMCustomDate
	{

		internal TGEDCOMCustomDate FValue;

		[Browsable(false)]
		public TGEDCOMCustomDate Value
		{
			get
			{
				return this.FValue;
			}
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FValue = null;
		}
		protected internal override string GetStringValue()
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
		protected internal override DateTime GetDateTime()
		{
			DateTime Result;
			if (this.FValue != null)
			{
				Result = this.FValue.GetDateTime();
			}
			else
			{
				Result = new DateTime((long)((ulong)0));
			}
			return Result;
		}
		protected internal override void SetDateTime(DateTime ADateTime)
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
		public override void Assign(TGEDCOMCustomTag Source)
		{
			base.Assign(Source);
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
			string SU = BDSSystem.WStrCopy(S, 1, 3).ToUpper();
			if (BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[1]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[2]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[3]) == 0)
			{
				this.FValue = new TGEDCOMDateApproximated(base.Owner, this, "", "");
			}
			else
			{
				if (BDSSystem.WStrCmp(SU, "INT") == 0)
				{
					this.FValue = new TGEDCOMDateInterpreted(base.Owner, this, "", "");
				}
				else
				{
					if (BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[0]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[1]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[2]) == 0)
					{
						this.FValue = new TGEDCOMDateRange(base.Owner, this, "", "");
					}
					else
					{
						if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(S, 1, 4).ToUpper(), "FROM") == 0 || BDSSystem.WStrCmp(BDSSystem.WStrCopy(S, 1, 2).ToUpper(), "TO") == 0)
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
