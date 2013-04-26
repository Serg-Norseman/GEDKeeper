using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualAttribute : TGEDCOMCustomEvent
	{
		private StringList FPhysicalDescription;


		public StringList PhysicalDescription
		{
			get { return base.GetTagStrings(this, ref this.FPhysicalDescription); }
			set { base.SetTagStrings(this, value); }
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "CONC" || ATag == "CONT")
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}
			else
			{
				Result = this.Detail.AddTag(ATag, AValue, ATagConstructor);
			}
			return Result;
		}

		public TGEDCOMIndividualAttribute(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMIndividualAttribute(AOwner, AParent, AName, AValue);
		}
	}
}
