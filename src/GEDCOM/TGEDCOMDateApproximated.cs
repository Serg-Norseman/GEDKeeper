using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateApproximated : TGEDCOMDate
	{
		private TGEDCOMApproximated fDateApproximated;

		public TGEDCOMApproximated Approximated
		{
			get { return this.fDateApproximated; }
			set { this.fDateApproximated = value; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDateApproximated = TGEDCOMApproximated.daExact;
		}

		protected override string GetStringValue()
		{
			string prefix;
			if (this.fDateApproximated == TGEDCOMApproximated.daExact) {
				prefix = "";
			} else {
                prefix = TGEDCOMCustomDate.GEDCOMDateApproximatedArray[(int)this.fDateApproximated];
				prefix += " ";
			}

			return prefix + base.GetStringValue();
		}

		private string ExtractApproximated(string S)
		{
			string result = S;
			string SU = result.Substring(0, 3).ToUpperInvariant();

			for (TGEDCOMApproximated i = TGEDCOMApproximated.daAbout; i <= TGEDCOMApproximated.daEstimated; i++)
			{
				if (SU == TGEDCOMCustomDate.GEDCOMDateApproximatedArray[(int)i])
				{
					this.fDateApproximated = i;
					result = result.Remove(0, 3);
					break;
				}
			}

			return result;
		}

		public override string ParseString(string strValue)
		{
			string result = GEDCOMUtils.ExtractDelimiter(strValue, 0);
			result = this.ExtractApproximated(result);
			result = GEDCOMUtils.ExtractDelimiter(result, 0);
			return base.ParseString(result);
		}

		public TGEDCOMDateApproximated(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
