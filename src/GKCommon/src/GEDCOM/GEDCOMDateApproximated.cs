using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMDateApproximated : GEDCOMDate
	{
		private GEDCOMApproximated fDateApproximated;

		public GEDCOMApproximated Approximated
		{
			get { return this.fDateApproximated; }
			set { this.fDateApproximated = value; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDateApproximated = GEDCOMApproximated.daExact;
		}

		protected override string GetStringValue()
		{
			string prefix;
			if (this.fDateApproximated == GEDCOMApproximated.daExact) {
				prefix = "";
			} else {
                prefix = GEDCOMCustomDate.GEDCOMDateApproximatedArray[(int)this.fDateApproximated];
				prefix += " ";
			}

			return prefix + base.GetStringValue();
		}

		private string ExtractApproximated(string S)
		{
			string result = S;
			string SU = result.Substring(0, 3).ToUpperInvariant();

			for (GEDCOMApproximated i = GEDCOMApproximated.daAbout; i <= GEDCOMApproximated.daEstimated; i++)
			{
				if (SU == GEDCOMCustomDate.GEDCOMDateApproximatedArray[(int)i])
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

		public GEDCOMDateApproximated(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
