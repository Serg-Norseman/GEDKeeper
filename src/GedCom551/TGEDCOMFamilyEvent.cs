using System;

namespace GedCom551
{
	public sealed class TGEDCOMFamilyEvent : TGEDCOMCustomEvent
	{
		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			return this.Detail.AddTag(ATag, AValue, ATagConstructor);
		}

		public TGEDCOMFamilyEvent(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMFamilyEvent(owner, parent, tagName, tagValue);
		}
	}
}
