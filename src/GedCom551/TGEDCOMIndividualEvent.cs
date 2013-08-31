using System;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualEvent : TGEDCOMCustomEvent
	{
		public TGEDCOMPointer Family
		{
			get	{ return base.TagClass("FAMC", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "FAMC")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPointer.Create);
			}
			else
			{
				result = this.Detail.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
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
