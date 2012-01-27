using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMFamilyEvent : TGEDCOMCustomEvent
	{
		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			return this.Detail.AddTag(ATag, AValue, ATagConstructor);
		}

		public TGEDCOMFamilyEvent(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
