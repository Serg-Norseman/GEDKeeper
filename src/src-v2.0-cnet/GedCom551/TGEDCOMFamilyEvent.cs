using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMFamilyEvent : TGEDCOMCustomEvent
	{
		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			return this.Detail.AddTag(ATag, AValue, AClass);
		}

		public TGEDCOMFamilyEvent(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
