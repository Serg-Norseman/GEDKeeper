using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateRange : TGEDCOMCustomDate
	{
		private TGEDCOMDate fDateAfter;
		private TGEDCOMDate fDateBefore;

		public TGEDCOMDate After
		{
			get { return this.fDateAfter; }
		}

		public TGEDCOMDate Before
		{
			get { return this.fDateBefore; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDateAfter = new TGEDCOMDate(owner, parent, "", "");
			this.fDateBefore = new TGEDCOMDate(owner, parent, "", "");
		}

		protected override string GetStringValue()
		{
			string result;
			if (!this.fDateAfter.IsEmpty() && !this.fDateBefore.IsEmpty())
			{
				result = string.Concat(new string[]
				{
					TGEDCOMCustomDate.GEDCOMDateRangeArray[2], 
					" ", 
					this.fDateAfter.StringValue, 
					" ", 
					TGEDCOMCustomDate.GEDCOMDateRangeArray[3], 
					" ", 
					this.fDateBefore.StringValue
				});
			}
			else
			{
				if (!this.fDateAfter.IsEmpty())
				{
                    result = TGEDCOMCustomDate.GEDCOMDateRangeArray[0] + " " + this.fDateAfter.StringValue;
				}
				else
				{
					if (!this.fDateBefore.IsEmpty())
					{
                        result = TGEDCOMCustomDate.GEDCOMDateRangeArray[1] + " " + this.fDateBefore.StringValue;
					}
					else
					{
						result = "";
					}
				}
			}
			return result;
		}

		public override DateTime GetDateTime()
		{
			DateTime result;
			if (this.fDateAfter.IsEmpty())
			{
				result = this.fDateBefore.GetDateTime();
			}
			else
			{
				if (this.fDateBefore.IsEmpty())
				{
					result = this.fDateAfter.GetDateTime();
				}
				else
				{
					result = new DateTime(0);
				}
			}
			return result;
		}

		public override void SetDateTime(DateTime value)
		{
			if (!this.fDateAfter.IsEmpty() && this.fDateBefore.IsEmpty())
			{
				this.fDateAfter.SetDateTime(value);
			}
			else
			{
				if (!this.fDateBefore.IsEmpty() && this.fDateAfter.IsEmpty())
				{
					this.fDateBefore.SetDateTime(value);
				}
				else
				{
					this.fDateAfter.SetDateTime(value);
					this.fDateBefore.SetDateTime(value);
				}
			}
		}

        protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fDateAfter.Dispose();
				this.fDateBefore.Dispose();
			}
            base.Dispose(disposing);
		}

		public override void Clear()
		{
			this.fDateAfter.Clear();
			this.fDateBefore.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fDateAfter.IsEmpty() && this.fDateBefore.IsEmpty();
		}

		public override string ParseString(string strValue)
		{
			this.fDateAfter.Clear();
			this.fDateBefore.Clear();

			string result = strValue;
			if (!string.IsNullOrEmpty(result))
			{
				string SU = result.Substring(0, 3).ToUpperInvariant();

                if (SU == TGEDCOMCustomDate.GEDCOMDateRangeArray[0])
				{
					result = result.Remove(0, 3);
					result = GEDCOMUtils.ExtractDelimiter(result, 0);
					result = this.fDateAfter.ParseString(result);
				}
				else
				{
                    if (SU == TGEDCOMCustomDate.GEDCOMDateRangeArray[1])
					{
						result = result.Remove(0, 3);
						result = GEDCOMUtils.ExtractDelimiter(result, 0);
						result = this.fDateBefore.ParseString(result);
					}
					else
					{
                        if (SU == TGEDCOMCustomDate.GEDCOMDateRangeArray[2])
						{
							result = result.Remove(0, 3);
							result = GEDCOMUtils.ExtractDelimiter(result, 0);

							result = TGEDCOMDateRange._ParseString_FixFTB(result);

							result = this.fDateAfter.ParseString(result);
							result = GEDCOMUtils.ExtractDelimiter(result, 0);

							SU = result.Substring(0, 3).ToUpper();

                            if (SU == TGEDCOMCustomDate.GEDCOMDateRangeArray[3])
							{
								result = result.Remove(0, 3);
								result = GEDCOMUtils.ExtractDelimiter(result, 0);
								result = TGEDCOMDateRange._ParseString_FixFTB(result);
								result = this.fDateBefore.ParseString(result);
							}
						}
					}
				}
			}
			return result;
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			if (this.fDateAfter != null) this.fDateAfter.ResetOwner(newOwner);
			if (this.fDateBefore != null) this.fDateBefore.ResetOwner(newOwner);
		}

		public TGEDCOMDateRange(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		private static string _ParseString_FixFTB(string S)
		{
			string result = S;
			string SU = result.Substring(0, 3).ToUpperInvariant();

            if (SU == TGEDCOMCustomDate.GEDCOMDateRangeArray[0] ||
                SU == TGEDCOMCustomDate.GEDCOMDateRangeArray[1] ||
                SU == TGEDCOMCustomDate.GEDCOMDateApproximatedArray[1] ||
                SU == TGEDCOMCustomDate.GEDCOMDateApproximatedArray[2] ||
                SU == TGEDCOMCustomDate.GEDCOMDateApproximatedArray[3])
			{
				result = result.Remove(0, 4);
			}
			return result;
		}
	}
}
