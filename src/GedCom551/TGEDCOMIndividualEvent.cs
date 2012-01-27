using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualEvent : TGEDCOMCustomEvent
	{
		public TGEDCOMPointer Family
		{
			get	{ return base.TagClass("FAMC", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "FAMC")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
			}
			else
			{
				Result = this.Detail.AddTag(ATag, AValue, ATagConstructor);
			}
			return Result;
		}

		public TGEDCOMIndividualEvent(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
