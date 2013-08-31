using System;
using Ext.Utils;

namespace GedCom551
{
	public class TGEDCOMIndividualAttribute : TGEDCOMCustomEvent
	{
		private StringList fPhysicalDescription;


		public StringList PhysicalDescription
		{
			get { return base.GetTagStrings(this, ref this.fPhysicalDescription); }
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

		public TGEDCOMIndividualAttribute(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMIndividualAttribute(owner, parent, tagName, tagValue);
		}
	}
}
