using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMUserReference : TGEDCOMTag
	{
		[Browsable(false)]
		public string ReferenceType
		{
			get	{ return this.GetReferenceType(); }
			set	{ this.SetReferenceType(value);	}
		}

		internal string GetReferenceType()
		{
			return base.GetTagStringValue("TYPE");
		}

		internal void SetReferenceType([In] string Value)
		{
			base.SetTagStringValue("TYPE", Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "REFN";
		}

		public TGEDCOMUserReference(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
