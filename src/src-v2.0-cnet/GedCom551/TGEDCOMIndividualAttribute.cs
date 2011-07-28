using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMIndividualAttribute : TGEDCOMCustomEvent
	{

		internal TStrings FPhysicalDescription;

		[Browsable(false)]
		public TStrings PhysicalDescription
		{
			get
			{
				return this.GetPhysicalDescription();
			}
			set
			{
				this.SetPhysicalDescription(value);
			}
		}

		internal TStrings GetPhysicalDescription()
		{
			return base.GetTagStrings(this, ref this.FPhysicalDescription);
		}

		internal void SetPhysicalDescription(TStrings Value)
		{
			base.SetTagStrings(this, Value);
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "CONC") == 0 || BDSSystem.WStrCmp(ATag, "CONT") == 0)
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			else
			{
				Result = this.Detail.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public TGEDCOMIndividualAttribute(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
