using System;
using ExtUtils;

namespace GedCom551
{
	public class TGEDCOMIndividualAttribute : TGEDCOMCustomEvent
	{
		public StringList PhysicalDescription
		{
			get { return base.GetTagStrings(this); }
			set { base.SetTagStrings(this, value); }
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "CONC" || tagName == "CONT")
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}
			else
			{
				result = this.Detail.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMIndividualAttribute(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMIndividualAttribute(owner, parent, tagName, tagValue);
		}
	}
}
