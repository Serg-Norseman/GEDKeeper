using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMFileReferenceWithTitle : TGEDCOMFileReference
	{
		[Browsable(false)]
		public string Title
		{
			get { return this.GetTitle(); }
			set { this.SetTitle(value); }
		}

		internal string GetTitle()
		{
			return base.GetTagStringValue("TITL");
		}

		internal void SetTitle([In] string Value)
		{
			base.SetTagStringValue("TITL", Value);
		}

		protected internal override string MediaTypeTagName()
		{
			return "FORM\\TYPE";
		}

		public TGEDCOMFileReferenceWithTitle(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
