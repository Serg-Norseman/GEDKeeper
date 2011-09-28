using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualAttribute : TGEDCOMCustomEvent
	{
		private TStrings FPhysicalDescription;


		public TStrings PhysicalDescription
		{
			get { return this.GetPhysicalDescription(); }
			set { this.SetPhysicalDescription(value); }
		}

		private TStrings GetPhysicalDescription()
		{
			return base.GetTagStrings(this, ref this.FPhysicalDescription);
		}

		private void SetPhysicalDescription(TStrings Value)
		{
			base.SetTagStrings(this, Value);
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "CONC" || ATag == "CONT")
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
