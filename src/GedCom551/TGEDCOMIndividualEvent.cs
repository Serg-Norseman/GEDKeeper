using System;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualEvent : TGEDCOMCustomEvent
	{
		public TGEDCOMPointer Family
		{
			get	{ return base.TagClass("FAMC", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
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

		public TGEDCOMIndividualEvent(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMIndividualEvent(owner, parent, tagName, tagValue);
		}
	}
}
