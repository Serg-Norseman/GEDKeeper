using System;

namespace GKCommon.GEDCOM
{
	public class GEDCOMDateValue : GEDCOMCustomDate
	{
		private GEDCOMCustomDate fValue;

		public GEDCOMCustomDate Value
		{
			get { return this.fValue; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fValue = null;
		}

		protected override string GetStringValue()
		{
			return ((this.fValue == null) ? "" : this.fValue.StringValue);
		}

		public override DateTime GetDateTime()
		{
			DateTime result = ((this.fValue == null) ? new DateTime(0) : this.fValue.GetDateTime());
			return result;
		}

		public override void SetDateTime(DateTime value)
		{
			if (this.fValue != null)
			{
				this.fValue.SetDateTime(value);
			}
			else
			{
				this.fValue = new GEDCOMDateExact(base.Owner, this, "", "");
				this.fValue.Date = value;
			}
		}

		public override void Clear()
		{
			if (this.fValue != null) this.fValue.Clear();
		}

		public override bool IsEmpty()
		{
			return this.fValue == null || this.fValue.IsEmpty();
		}

		public override string ParseString(string strValue)
		{
			try
			{
				if (this.fValue != null) {
					this.fValue.Dispose();
					this.fValue = null;
				}

				if (string.IsNullOrEmpty(strValue)) {
					return "";
				}

				string su = strValue.Substring(0, 3).ToUpperInvariant();

				if (su == GEDCOMDateApproximatedArray[1] || su == GEDCOMDateApproximatedArray[2] || su == GEDCOMDateApproximatedArray[3])
				{
					this.fValue = new GEDCOMDateApproximated(base.Owner, this, "", "");
				}
				else if (su == "INT")
				{
					this.fValue = new GEDCOMDateInterpreted(base.Owner, this, "", "");
				}
				else if (su == GEDCOMDateRangeArray[0] || su == GEDCOMDateRangeArray[1] || su == GEDCOMDateRangeArray[2])
				{
					this.fValue = new GEDCOMDateRange(base.Owner, this, "", "");
				}
				else if (strValue.StartsWith("FROM", StringComparison.InvariantCulture) || strValue.StartsWith("TO", StringComparison.InvariantCulture))
				{
					this.fValue = new GEDCOMDatePeriod(base.Owner, this, "", "");
				}
				else
				{
					this.fValue = new GEDCOMDate(base.Owner, this, "", "");
				}

				return this.fValue.ParseString(strValue);
			}
			catch (Exception ex)
			{
				Logger.LogWrite("GEDCOMDateValue.ParseString(\"" + strValue + "\"): " + ex.Message);
				return strValue;
			}
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			if (this.fValue != null) this.fValue.ResetOwner(newOwner);
		}

		public GEDCOMDateValue(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMDateValue(owner, parent, tagName, tagValue);
		}
        
        #region Auxiliary

		public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			GEDCOMDateValue date = (GEDCOMDateValue)tag;

			if (this.IsEmpty() || date.IsEmpty()) return 0.0f;

			AbsDate absVal1 = this.GetAbstractDate();
			AbsDate absVal2 = date.GetAbstractDate();

			float match = 0.0f;
			float matches = 0.0f;
			if (absVal1.IsValid() && absVal2.IsValid()) {
				matches += 1.0f;
				if (Math.Abs(absVal1.Year - absVal2.Year) <= matchParams.YearsInaccuracy) match += 100.0f;
			}

			return (match / matches);
		}

		public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
		{
			if (this.fValue == null) {
				year = -1;
				month = 0;
				day = 0;
				yearBC = false;
			} else {
				this.fValue.GetDateParts(out year, out month, out day, out yearBC);
			}
		}

		public override AbsDate GetAbstractDate()
		{
			return (this.fValue == null) ? AbsDate.Empty() : this.fValue.GetAbstractDate();
		}

        #endregion
	}
}
