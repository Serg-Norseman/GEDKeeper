using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPointer : TGEDCOMTag
	{
		internal string FXRef;

		[Browsable(false)]
		public TGEDCOMRecord Value
		{
			get	{ return this.GetValue(); }
			set	{ this.SetValue(value); }
		}

		[Browsable(false)]
		public string XRef
		{
			get { return this.GetXRef(); }
			set { this.SetXRef(value); }
		}

		internal TGEDCOMRecord GetValue()
		{
			return base.FindRecord(this.GetXRef());
		}

		internal void SetValue(TGEDCOMRecord AValue)
		{
			this.FXRef = "";
			if (AValue != null)
			{
				string XRef = AValue.XRef;
				if (BDSSystem.WStrCmp(XRef, "") == 0)
				{
					XRef = AValue.NewXRef();
				}
				this.SetXRef(XRef);
			}
		}

		internal string GetXRef()
		{
			return TGEDCOMObject.CleanXRef(this.FXRef);
		}

		internal void SetXRef([In] string Value)
		{
			this.FXRef = TGEDCOMObject.EncloseXRef(Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FXRef = "";
		}

		protected internal override string GetStringValue()
		{
			return this.FXRef;
		}

		public override bool IsEmpty()
		{
			return BDSSystem.WStrCmp(this.FXRef, "") == 0;
		}

		public override string ParseString([In] string AString)
		{
			this.FXRef = "";
			string Result = AString;
			Result = base.ExtractDelimiter(Result, 0);
			if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 1), "@") == 0 && BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 2, 1), "#") != 0 && BDSSystem.Pos("@", BDSSystem.WStrCopy(Result, 3, 2147483647)) > 0)
			{
				Result = Result.Remove(0, 1);
				this.FXRef = "@" + BDSSystem.WStrCopy(Result, 1, BDSSystem.Pos("@", Result));
				int num = BDSSystem.Pos("@", Result);
				Result = Result.Remove(0, num);
			}
			return Result;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.SetXRef(aMap.FindNewXRef(this.XRef));
		}

		public void SetNamedValue([In] string aName, TGEDCOMRecord aValue)
		{
			base.Name = aName;
			this.SetValue(aValue);
		}

		public TGEDCOMPointer(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
