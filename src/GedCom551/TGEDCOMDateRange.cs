using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateRange : TGEDCOMCustomDate
	{
		private TGEDCOMDate FDateAfter;
		private TGEDCOMDate FDateBefore;

		public TGEDCOMDate After
		{
			get { return this.FDateAfter; }
		}

		public TGEDCOMDate Before
		{
			get { return this.FDateBefore; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FDateAfter = new TGEDCOMDate(owner, parent, "", "");
			this.FDateBefore = new TGEDCOMDate(owner, parent, "", "");
		}

		protected override string GetStringValue()
		{
			string Result;
			if (!this.FDateAfter.IsEmpty() && !this.FDateBefore.IsEmpty())
			{
				Result = string.Concat(new string[]
				{
					TGEDCOMDate.GEDCOMDateRangeArray[2], 
					" ", 
					this.FDateAfter.StringValue, 
					" ", 
					TGEDCOMDate.GEDCOMDateRangeArray[3], 
					" ", 
					this.FDateBefore.StringValue
				});
			}
			else
			{
				if (!this.FDateAfter.IsEmpty())
				{
					Result = TGEDCOMDate.GEDCOMDateRangeArray[0] + " " + this.FDateAfter.StringValue;
				}
				else
				{
					if (!this.FDateBefore.IsEmpty())
					{
						Result = TGEDCOMDate.GEDCOMDateRangeArray[1] + " " + this.FDateBefore.StringValue;
					}
					else
					{
						Result = "";
					}
				}
			}
			return Result;
		}

		public override DateTime GetDateTime()
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
					Result = new DateTime(0);
				}
			}
			return Result;
		}

		public override void SetDateTime(DateTime ADateTime)
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
				this.FDateAfter.Dispose();
				this.FDateBefore.Dispose();

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

		public override string ParseString(string S)
		{
			this.FDateAfter.Clear();
			this.FDateBefore.Clear();

			string Result = S;
			if (!string.IsNullOrEmpty(Result))
			{
				string SU = Result.Substring(0, 3).ToUpper();

				if (SU == TGEDCOMDate.GEDCOMDateRangeArray[0])
				{
					Result = Result.Remove(0, 3);
					Result = base.ExtractDelimiter(Result, 0);
					Result = this.FDateAfter.ParseString(Result);
				}
				else
				{
					if (SU == TGEDCOMDate.GEDCOMDateRangeArray[1])
					{
						Result = Result.Remove(0, 3);
						Result = base.ExtractDelimiter(Result, 0);
						Result = this.FDateBefore.ParseString(Result);
					}
					else
					{
						if (SU == TGEDCOMDate.GEDCOMDateRangeArray[2])
						{
							Result = Result.Remove(0, 3);
							Result = base.ExtractDelimiter(Result, 0);

							Result = TGEDCOMDateRange._ParseString_FixFTB(Result);

							Result = this.FDateAfter.ParseString(Result);
							Result = base.ExtractDelimiter(Result, 0);

							SU = Result.Substring(0, 3).ToUpper();

							if (SU == TGEDCOMDate.GEDCOMDateRangeArray[3])
							{
								Result = Result.Remove(0, 3);
								Result = base.ExtractDelimiter(Result, 0);
								Result = TGEDCOMDateRange._ParseString_FixFTB(Result);
								Result = this.FDateBefore.ParseString(Result);
							}
						}
					}
				}
			}
			return Result;
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FDateAfter != null) this.FDateAfter.ResetOwner(AOwner);
			if (this.FDateBefore != null) this.FDateBefore.ResetOwner(AOwner);
		}

		public TGEDCOMDateRange(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		private static string _ParseString_FixFTB(string S)
		{
			string Result = S;
			string SU = Result.Substring(0, 3).ToUpper();

			if (SU == TGEDCOMDate.GEDCOMDateRangeArray[0] ||
			    SU == TGEDCOMDate.GEDCOMDateRangeArray[1] || 
			    SU == TGEDCOMDate.GEDCOMDateApproximatedArray[1] || 
			    SU == TGEDCOMDate.GEDCOMDateApproximatedArray[2] || 
			    SU == TGEDCOMDate.GEDCOMDateApproximatedArray[3])
			{
				Result = Result.Remove(0, 4);
			}
			return Result;
		}
	}
}
