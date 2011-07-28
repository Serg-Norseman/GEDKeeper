using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMDatePeriod : TGEDCOMCustomDate
	{

		internal TGEDCOMDate FDateFrom;
		internal TGEDCOMDate FDateTo;

		[Browsable(false)]
		public TGEDCOMDate DateFrom
		{
			get
			{
				return this.FDateFrom;
			}
		}

		[Browsable(false)]
		public TGEDCOMDate DateTo
		{
			get
			{
				return this.FDateTo;
			}
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDateFrom = new TGEDCOMDate(AOwner, this, "", "");
			this.FDateTo = new TGEDCOMDate(AOwner, this, "", "");
		}

		protected internal override string GetStringValue()
		{
			string Result;
			if (!this.FDateFrom.IsEmpty() && !this.FDateTo.IsEmpty())
			{
				Result = string.Concat(new string[]
				{
					"FROM ", 
					this.FDateFrom.StringValue, 
					" ", 
					"TO", 
					" ", 
					this.FDateTo.StringValue
				});
			}
			else
			{
				if (!this.FDateFrom.IsEmpty())
				{
					Result = "FROM " + this.FDateFrom.StringValue;
				}
				else
				{
					if (!this.FDateTo.IsEmpty())
					{
						Result = "TO " + this.FDateTo.StringValue;
					}
					else
					{
						Result = "";
					}
				}
			}
			return Result;
		}

		protected internal override DateTime GetDateTime()
		{
			DateTime Result;
			if (this.FDateFrom.IsEmpty())
			{
				Result = this.FDateTo.GetDateTime();
			}
			else
			{
				if (this.FDateTo.IsEmpty())
				{
					Result = this.FDateFrom.GetDateTime();
				}
				else
				{
					if (this.FDateFrom.GetDateTime() == this.FDateTo.GetDateTime())
					{
						Result = this.FDateFrom.GetDateTime();
					}
					else
					{
						Result = new DateTime((long)((ulong)0));
					}
				}
			}
			return Result;
		}

		protected internal override void SetDateTime(DateTime ADateTime)
		{
			if (!this.FDateFrom.IsEmpty() && this.FDateTo.IsEmpty())
			{
				this.FDateFrom.SetDateTime(ADateTime);
			}
			else
			{
				if (!this.FDateTo.IsEmpty() && this.FDateFrom.IsEmpty())
				{
					this.FDateTo.SetDateTime(ADateTime);
				}
				else
				{
					this.FDateFrom.SetDateTime(ADateTime);
					this.FDateTo.SetDateTime(ADateTime);
				}
			}
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FDateFrom.Free();
				this.FDateTo.Free();
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Clear()
		{
			this.FDateFrom.Clear();
			this.FDateTo.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FDateFrom.IsEmpty() && this.FDateTo.IsEmpty();
		}

		public override string ParseString([In] string S)
		{
			string Result = S;
			if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 4).ToUpper(), "FROM") == 0)
			{
				Result = Result.Remove(0, 4);
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.FDateFrom.ParseString(Result);
				Result = base.ExtractDelimiter(Result, 0);
			}
			if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 2).ToUpper(), "TO") == 0)
			{
				Result = Result.Remove(0, 2);
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.FDateTo.ParseString(Result);
			}
			return Result;
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FDateFrom != null)
			{
				this.FDateFrom.ResetOwner(AOwner);
			}
			if (this.FDateTo != null)
			{
				this.FDateTo.ResetOwner(AOwner);
			}
		}

		public TGEDCOMDatePeriod(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
