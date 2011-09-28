using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMFileReferenceWithTitle : TGEDCOMFileReference
	{
		public string Title
		{
			get { return base.GetTagStringValue("TITL"); }
			set { base.SetTagStringValue("TITL", value); }
		}

		protected override string MediaTypeTagName()
		{
			return "FORM\\TYPE";
		}

		public TGEDCOMFileReferenceWithTitle(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
