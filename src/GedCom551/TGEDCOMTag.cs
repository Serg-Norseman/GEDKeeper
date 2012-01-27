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

		public TGEDCOMTag(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMTag(AOwner, AParent, AName, AValue);
		}
	}
}
