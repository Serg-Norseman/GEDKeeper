using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateApproximated : TGEDCOMDate
	{
		private TGEDCOMApproximated FDateApproximated;

		public TGEDCOMApproximated Approximated
		{
			get { return this.FDateApproximated; }
			set { this.FDateApproximated = value; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FDateApproximated = TGEDCOMApproximated.daExact;
		}

		protected override string GetStringValue()
		{
			string prefix;
			if (this.FDateApproximated == TGEDCOMApproximated.daExact) {
				prefix = "";
			} else {
				prefix = TGEDCOMDate.GEDCOMDateApproximatedArray[(int)this.FDateApproximated];
				prefix += " ";
			}

			return prefix + base.GetStringValue();
		}

		// code-state: ugly
		private string ExtractApproximated(string S)
		{
			string result = S;

			string SU = result.Substring(0, 3).ToUpper();
			TGEDCOMApproximated I = TGEDCOMApproximated.daAbout;
			while (SU != TGEDCOMDate.GEDCOMDateApproximatedArray[(int)I])
			{
				I++;
				if (I == (TGEDCOMApproximated)4)
				{
					return result;
				}
			}
			this.FDateApproximated = I;
			result = result.Remove(0, 3);

			return result;
		}

		public override string ParseString(string S)
		{
			string result = GEDCOMUtils.ExtractDelimiter(S, 0);
			result = this.ExtractApproximated(result);
			result = GEDCOMUtils.ExtractDelimiter(result, 0);
			return base.ParseString(result);
		}

		public TGEDCOMDateApproximated(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
