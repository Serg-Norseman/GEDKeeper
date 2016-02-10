using BSLib;

namespace GKCommon.GEDCOM
{
	public class GEDCOMIndividualAttribute : GEDCOMCustomEvent
	{
		public StringList PhysicalDescription
		{
			get { return base.GetTagStrings(this); }
			set { base.SetTagStrings(this, value); }
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

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

		public GEDCOMIndividualAttribute(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMIndividualAttribute(owner, parent, tagName, tagValue);
		}
	}
}
