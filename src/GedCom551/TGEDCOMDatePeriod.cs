using System;

namespace GedCom551
{
	public sealed class TGEDCOMDatePeriod : TGEDCOMCustomDate
	{
		private TGEDCOMDate FDateFrom;
		private TGEDCOMDate FDateTo;

		public TGEDCOMDate DateFrom
		{
			get { return this.FDateFrom; }
		}

		public TGEDCOMDate DateTo
		{
			get { return this.FDateTo; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FDateFrom = new TGEDCOMDate(owner, this, "", "");
			this.FDateTo = new TGEDCOMDate(owner, this, "", "");
		}

		protected override string GetStringValue()
		{
			string Result;
			if (!this.FDateFrom.IsEmpty() && !this.FDateTo.IsEmpty())
			{
				Result = string.Concat(new string[]
				{
					"FROM ", this.FDateFrom.StringValue, " ", "TO", " ", this.FDateTo.StringValue
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

		public override DateTime GetDateTime()
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
						Result = new DateTime(0);
					}
				}
			}
			return Result;
		}

		public override void SetDateTime(DateTime ADateTime)
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
				this.FDateFrom.Dispose();
				this.FDateTo.Dispose();
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

		public override string ParseString(string S)
		{
			string Result = S;
			if (!string.IsNullOrEmpty(Result))
			{
				if (Result.StartsWith("FROM"))
				{
					Result = Result.Remove(0, 4);
					Result = base.ExtractDelimiter(Result, 0);
					Result = this.FDateFrom.ParseString(Result);
					Result = base.ExtractDelimiter(Result, 0);
				}
				if (Result.StartsWith("TO"))
				{
					Result = Result.Remove(0, 2);
					Result = base.ExtractDelimiter(Result, 0);
					Result = this.FDateTo.ParseString(Result);
				}
			}
			return Result;
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
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

		public TGEDCOMDatePeriod(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMDatePeriod(owner, parent, tagName, tagValue);
		}
	}
}
