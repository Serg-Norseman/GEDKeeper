using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMDateApproximated : TGEDCOMDate
	{

		public enum TGEDCOMApproximated : byte
		{
			daExact,
			daAbout,
			daCalculated,
			daEstimated
		}
		public static readonly string[] GEDCOMDateApproximatedArray;
		internal TGEDCOMDateApproximated.TGEDCOMApproximated FDateApproximated;
		[Browsable(false)]
		public TGEDCOMDateApproximated.TGEDCOMApproximated Approximated
		{
			get
			{
				return this.FDateApproximated;
			}
			set
			{
				this.FDateApproximated = value;
			}
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDateApproximated = TGEDCOMDateApproximated.TGEDCOMApproximated.daExact;
		}
		protected internal override string GetStringValue()
		{
			return this.ApproximatedString(false) + base.GetStringValue();
		}
		protected internal string ApproximatedString(bool NoDelimiter)
		{
			string Result;
			if (this.FDateApproximated == TGEDCOMDateApproximated.TGEDCOMApproximated.daExact)
			{
				Result = "";
			}
			else
			{
				Result = TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[(int)this.FDateApproximated];
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}
		protected internal string ExtractApproximated([In] string S)
		{
			string Result = S;
			string SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
			TGEDCOMDateApproximated.TGEDCOMApproximated I = TGEDCOMDateApproximated.TGEDCOMApproximated.daAbout;
			while (BDSSystem.WStrCmp(SU, TGEDCOMDateApproximated.GEDCOMDateApproximatedArray[(int)I]) != 0)
			{
				I++;
				if (I == (TGEDCOMDateApproximated.TGEDCOMApproximated)4)
				{
					return Result;
				}
			}
			this.FDateApproximated = I;
			Result = Result.Remove(0, 3);
			return Result;
		}
		public override string ParseString([In] string S)
		{
			string Result = base.ExtractDelimiter(S, 0);
			Result = this.ExtractApproximated(Result);
			Result = base.ExtractDelimiter(Result, 0);
			return base.ParseString(Result);
		}

		static TGEDCOMDateApproximated()
		{
			TGEDCOMDateApproximated.GEDCOMDateApproximatedArray = new string[]
			{
				"", 
				"ABT", 
				"CAL", 
				"EST"
			};
		}

		public TGEDCOMDateApproximated(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
