using System;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMDateRange : GEDCOMCustomDate
	{
		private GEDCOMDate fDateAfter;
		private GEDCOMDate fDateBefore;

		public GEDCOMDate After
		{
			get { return this.fDateAfter; }
		}

		public GEDCOMDate Before
		{
			get { return this.fDateBefore; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDateAfter = new GEDCOMDate(owner, parent, "", "");
			this.fDateBefore = new GEDCOMDate(owner, parent, "", "");
		}

		protected override string GetStringValue()
		{
			string result;
			if (!this.fDateAfter.IsEmpty() && !this.fDateBefore.IsEmpty())
			{
				result = string.Concat(new string[]
				{
					GEDCOMDateRangeArray[2], 
					" ", 
					this.fDateAfter.StringValue, 
					" ", 
					GEDCOMDateRangeArray[3], 
					" ", 
					this.fDateBefore.StringValue
				});
			}
			else
			{
				if (!this.fDateAfter.IsEmpty())
				{
                    result = GEDCOMDateRangeArray[0] + " " + this.fDateAfter.StringValue;
				}
				else
				{
					if (!this.fDateBefore.IsEmpty())
					{
                        result = GEDCOMDateRangeArray[1] + " " + this.fDateBefore.StringValue;
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
				string su = result.Substring(0, 3).ToUpperInvariant();

                if (su == GEDCOMDateRangeArray[0])
				{
					result = result.Remove(0, 3);
					result = GEDCOMUtils.ExtractDelimiter(result, 0);
					result = this.fDateAfter.ParseString(result);
				}
				else
				{
                    if (su == GEDCOMDateRangeArray[1])
					{
						result = result.Remove(0, 3);
						result = GEDCOMUtils.ExtractDelimiter(result, 0);
						result = this.fDateBefore.ParseString(result);
					}
					else
					{
                        if (su == GEDCOMDateRangeArray[2])
						{
							result = result.Remove(0, 3);
							result = GEDCOMUtils.ExtractDelimiter(result, 0);

							result = _ParseString_FixFTB(result);

							result = this.fDateAfter.ParseString(result);
							result = GEDCOMUtils.ExtractDelimiter(result, 0);

							su = result.Substring(0, 3).ToUpper();

                            if (su == GEDCOMDateRangeArray[3])
							{
								result = result.Remove(0, 3);
								result = GEDCOMUtils.ExtractDelimiter(result, 0);
								result = _ParseString_FixFTB(result);
								result = this.fDateBefore.ParseString(result);
							}
						}
					}
				}
			}
			return result;
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			if (this.fDateAfter != null) this.fDateAfter.ResetOwner(newOwner);
			if (this.fDateBefore != null) this.fDateBefore.ResetOwner(newOwner);
		}

		public GEDCOMDateRange(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		private static string _ParseString_FixFTB(string str)
		{
			string result = str;
			string su = result.Substring(0, 3).ToUpperInvariant();

            if (su == GEDCOMDateRangeArray[0] ||
                su == GEDCOMDateRangeArray[1] ||
                su == GEDCOMDateApproximatedArray[1] ||
                su == GEDCOMDateApproximatedArray[2] ||
                su == GEDCOMDateApproximatedArray[3])
			{
				result = result.Remove(0, 4);
			}
			return result;
		}

		public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
		{
			year = -1;
			month = 0;
			day = 0;
			yearBC = false;

			if (this.fDateAfter.StringValue == "" && this.fDateBefore.StringValue != "")
			{
				this.fDateBefore.GetDateParts(out year, out month, out day, out yearBC);
			}
			else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue == "")
			{
				this.fDateAfter.GetDateParts(out year, out month, out day, out yearBC);
			}
			else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue != "")
			{
				this.fDateAfter.GetDateParts(out year, out month, out day, out yearBC);
			}
		}

		public override double GetUDN()
		{
			double result;

			if (this.fDateAfter.StringValue == "" && this.fDateBefore.StringValue != "")
			{
				result = this.fDateBefore.GetUDN() - AbsDate.ABS_DATE_DELTA;
			}
			else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue == "")
			{
				result = this.fDateAfter.GetUDN() + AbsDate.ABS_DATE_DELTA;
			}
			else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue != "")
			{
				result = (this.fDateAfter.GetUDN() + this.fDateBefore.GetUDN()) / 2;
			}
			else
			{
				result = double.NaN;
			}

			return result;
		}

		public override AbsDate GetAbstractDate()
		{
			AbsDate result;

			if (this.fDateAfter.StringValue == "" && this.fDateBefore.StringValue != "")
			{
				result = this.fDateBefore.GetAbstractDate().Before();
			}
			else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue == "")
			{
				result = this.fDateAfter.GetAbstractDate().After();
			}
			else if (this.fDateAfter.StringValue != "" && this.fDateBefore.StringValue != "")
			{
				result = AbsDate.Between(this.fDateAfter.GetAbstractDate(), this.fDateBefore.GetAbstractDate());
			}
			else
			{
				result = AbsDate.Empty();
			}

			return result;
		}
	}
}
