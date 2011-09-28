using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMTag : TGEDCOMCustomTag
	{
		public new TGEDCOMCustomRecord ParentRecord
		{
			get	{ return base.GetParentRecord(); }
		}

		public TGEDCOMTag(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
