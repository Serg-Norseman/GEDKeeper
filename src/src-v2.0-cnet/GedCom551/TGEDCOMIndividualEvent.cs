using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMIndividualEvent : TGEDCOMCustomEvent
	{
		[Browsable(false)]
		public TGEDCOMPointer Family
		{
			get	{ return this.GetFamily(); }
		}

		internal TGEDCOMPointer GetFamily()
		{
			return base.TagClass("FAMC", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "FAMC") == 0)
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
