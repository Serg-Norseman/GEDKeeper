using System;
using Ext.Utils;

namespace GedCom551
{
	public class TGEDCOMIndividualAttribute : TGEDCOMCustomEvent
	{
		private StringList FPhysicalDescription;


		public StringList PhysicalDescription
		{
			get { return base.GetTagStrings(this, ref this.FPhysicalDescription); }
			set { base.SetTagStrings(this, value); }
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
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

		public TGEDCOMIndividualAttribute(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMIndividualAttribute(owner, parent, tagName, tagValue);
		}
	}
}
