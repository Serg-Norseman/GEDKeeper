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
			return this.ApproximatedString(false) + base.GetStringValue();
		}

		private string ApproximatedString(bool NoDelimiter)
		{
			string Result;
			if (this.FDateApproximated == TGEDCOMApproximated.daExact)
			{
				Result = "";
			}
			else
			{
				Result = TGEDCOMDate.GEDCOMDateApproximatedArray[(int)this.FDateApproximated];
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}

		private string ExtractApproximated(string S)
		{
			string Result = S;
			string SU = Result.Substring(0, 3).ToUpper();
			TGEDCOMApproximated I = TGEDCOMApproximated.daAbout;
			while (SU != TGEDCOMDate.GEDCOMDateApproximatedArray[(int)I])
			{
				I++;
				if (I == (TGEDCOMApproximated)4)
				{
					return Result;
				}
			}
			this.FDateApproximated = I;
			Result = Result.Remove(0, 3);
			return Result;
		}

		public override string ParseString(string S)
		{
			string Result = base.ExtractDelimiter(S, 0);
			Result = this.ExtractApproximated(Result);
			Result = base.ExtractDelimiter(Result, 0);
			return base.ParseString(Result);
		}

		public TGEDCOMDateApproximated(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
