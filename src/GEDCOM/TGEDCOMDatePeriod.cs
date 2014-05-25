using System;

namespace GedCom551
{
	public sealed class TGEDCOMDatePeriod : TGEDCOMCustomDate
	{
		private TGEDCOMDate fDateFrom;
		private TGEDCOMDate fDateTo;

		public TGEDCOMDate DateFrom
		{
			get { return this.fDateFrom; }
		}

		public TGEDCOMDate DateTo
		{
			get { return this.fDateTo; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDateFrom = new TGEDCOMDate(owner, this, "", "");
			this.fDateTo = new TGEDCOMDate(owner, this, "", "");
		}

		protected override string GetStringValue()
		{
			string result;

			if (!this.fDateFrom.IsEmpty() && !this.fDateTo.IsEmpty())
			{
				result = string.Concat(new string[]
				{
					"FROM ", this.fDateFrom.StringValue, " ", "TO", " ", this.fDateTo.StringValue
				});
			}
			else
			{
				if (!this.fDateFrom.IsEmpty())
				{
					result = "FROM " + this.fDateFrom.StringValue;
				}
				else
				{
					if (!this.fDateTo.IsEmpty())
					{
						result = "TO " + this.fDateTo.StringValue;
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
			if (this.fDateFrom.IsEmpty())
			{
				result = this.fDateTo.GetDateTime();
			}
			else
			{
				if (this.fDateTo.IsEmpty())
				{
					result = this.fDateFrom.GetDateTime();
				}
				else
				{
					if (this.fDateFrom.GetDateTime() == this.fDateTo.GetDateTime())
					{
						result = this.fDateFrom.GetDateTime();
					}
					else
					{
						result = new DateTime(0);
					}
				}
			}
			return result;
		}

		public override void SetDateTime(DateTime value)
		{
			if (!this.fDateFrom.IsEmpty() && this.fDateTo.IsEmpty())
			{
				this.fDateFrom.SetDateTime(value);
			}
			else
			{
				if (!this.fDateTo.IsEmpty() && this.fDateFrom.IsEmpty())
				{
					this.fDateTo.SetDateTime(value);
				}
				else
				{
					this.fDateFrom.SetDateTime(value);
					this.fDateTo.SetDateTime(value);
				}
			}
		}

        protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fDateFrom.Dispose();
				this.fDateTo.Dispose();
			}
            base.Dispose(disposing);
		}

		public override void Clear()
		{
			this.fDateFrom.Clear();
			this.fDateTo.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fDateFrom.IsEmpty() && this.fDateTo.IsEmpty();
		}

		public override string ParseString(string strValue)
		{
			string result = strValue;
			if (!string.IsNullOrEmpty(result))
			{
				if (result.StartsWith("FROM"))
				{
					result = result.Remove(0, 4);
					result = GEDCOMUtils.ExtractDelimiter(result, 0);
					result = this.fDateFrom.ParseString(result);
					result = GEDCOMUtils.ExtractDelimiter(result, 0);
				}
				if (result.StartsWith("TO"))
				{
					result = result.Remove(0, 2);
					result = GEDCOMUtils.ExtractDelimiter(result, 0);
					result = this.fDateTo.ParseString(result);
				}
			}
			return result;
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			if (this.fDateFrom != null)
			{
				this.fDateFrom.ResetOwner(newOwner);
			}
			if (this.fDateTo != null)
			{
				this.fDateTo.ResetOwner(newOwner);
			}
		}

		public TGEDCOMDatePeriod(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMDatePeriod(owner, parent, tagName, tagValue);
		}
	}
}
