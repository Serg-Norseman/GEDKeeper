using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMDateRange : TGEDCOMCustomDate
	{

		public enum TGEDCOMRange : byte
		{
			drAfter,
			drBefore,
			drBetween,
			drAnd
		}
		public static readonly string[] GEDCOMDateRangeArray;
		internal TGEDCOMDate FDateAfter;
		internal TGEDCOMDate FDateBefore;
		[Browsable(false)]
		public TGEDCOMDate After
		{
			get
			{
				return this.FDateAfter;
			}
		}
		[Browsable(false)]
		public TGEDCOMDate Before
		{
			get
			{
				return this.FDateBefore;
			}
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDateAfter = new TGEDCOMDate(AOwner, AParent, "", "");
			this.FDateBefore = new TGEDCOMDate(AOwner, AParent, "", "");
		}
		protected internal override string GetStringValue()
		{
			string Result;
			if (!this.FDateAfter.IsEmpty() && !this.FDateBefore.IsEmpty())
			{
				Result = string.Concat(new string[]
				{
					TGEDCOMDateRange.GEDCOMDateRangeArray[2], 
					" ", 
					this.FDateAfter.StringValue, 
					" ", 
					TGEDCOMDateRange.GEDCOMDateRangeArray[3], 
					" ", 
					this.FDateBefore.StringValue
				});
			}
			else
			{
				if (!this.FDateAfter.IsEmpty())
				{
					Result = TGEDCOMDateRange.GEDCOMDateRangeArray[0] + " " + this.FDateAfter.StringValue;
				}
				else
				{
					if (!this.FDateBefore.IsEmpty())
					{
						Result = TGEDCOMDateRange.GEDCOMDateRangeArray[1] + " " + this.FDateBefore.StringValue;
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
			if (this.FDateAfter.IsEmpty())
			{
				Result = this.FDateBefore.GetDateTime();
			}
			else
			{
				if (this.FDateBefore.IsEmpty())
				{
					Result = this.FDateAfter.GetDateTime();
				}
				else
				{
					Result = new DateTime((long)((ulong)0));
				}
			}
			return Result;
		}
		protected internal override void SetDateTime(DateTime ADateTime)
		{
			if (!this.FDateAfter.IsEmpty() && this.FDateBefore.IsEmpty())
			{
				this.FDateAfter.SetDateTime(ADateTime);
			}
			else
			{
				if (!this.FDateBefore.IsEmpty() && this.FDateAfter.IsEmpty())
				{
					this.FDateBefore.SetDateTime(ADateTime);
				}
				else
				{
					this.FDateAfter.SetDateTime(ADateTime);
					this.FDateBefore.SetDateTime(ADateTime);
				}
			}
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FDateAfter.Free();
				this.FDateBefore.Free();
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Clear()
		{
			this.FDateAfter.Clear();
			this.FDateBefore.Clear();
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FDateAfter.IsEmpty() && this.FDateBefore.IsEmpty();
		}
		public override string ParseString([In] string S)
		{
			this.FDateAfter.Clear();
			this.FDateBefore.Clear();
			string Result = S;
			string SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
			if (BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[0]) == 0)
			{
				Result = Result.Remove(0, 3);
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.FDateAfter.ParseString(Result);
			}
			else
			{
				if (BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[1]) == 0)
				{
					Result = Result.Remove(0, 3);
					Result = base.ExtractDelimiter(Result, 0);
					Result = this.FDateBefore.ParseString(Result);
				}
				else
				{
					if (BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[2]) == 0)
					{
						Result = Result.Remove(0, 3);
						Result = base.ExtractDelimiter(Result, 0);
						Result = TGEDCOMDateRange._ParseString_FixFTB(Result);
						Result = this.FDateAfter.ParseString(Result);
						Result = base.ExtractDelimiter(Result, 0);
						SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
						if (BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[3]) == 0)
						{
							Result = Result.Remove(0, 3);
							Result = base.ExtractDelimiter(Result, 0);
							Result = TGEDCOMDateRange._ParseString_FixFTB(Result);
							Result = this.FDateBefore.ParseString(Result);
						}
					}
				}
			}
			return Result;
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FDateAfter != null)
			{
				this.FDateAfter.ResetOwner(AOwner);
			}
			if (this.FDateBefore != null)
			{
				this.FDateBefore.ResetOwner(AOwner);
			}
		}

		static TGEDCOMDateRange()
		{
			TGEDCOMDateRange.GEDCOMDateRangeArray = new string[]
			{
				"AFT", 
				"BEF", 
				"BET", 
				"AND"
			};
		}

		public TGEDCOMDateRange(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
		private static string _ParseString_FixFTB([In] string S)
		{
			string Result = S;
			string SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
			if (BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[0]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateRange.GEDCOMDateRangeArray[1]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[1]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[2]) == 0 || BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[3]) == 0)
			{
				Result = Result.Remove(0, 4);
			}
			return Result;
		}
	}
}
