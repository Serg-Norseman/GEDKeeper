using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualEvent : TGEDCOMCustomEvent
	{
		public TGEDCOMPointer Family
		{
			get	{ return base.TagClass("FAMC", typeof(TGEDCOMPointer)) as TGEDCOMPointer; }
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "FAMC")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
			}
			else
			{
				Result = this.Detail.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public TGEDCOMIndividualEvent(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
