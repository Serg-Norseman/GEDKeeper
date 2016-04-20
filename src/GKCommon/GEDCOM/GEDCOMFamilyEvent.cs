namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMFamilyEvent : GEDCOMCustomEvent
	{
		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			return this.Detail.AddTag(tagName, tagValue, tagConstructor);
		}

		public GEDCOMFamilyEvent(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMFamilyEvent(owner, parent, tagName, tagValue);
		}
	}
}
