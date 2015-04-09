namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMIndividualEvent : GEDCOMCustomEvent
	{
		public GEDCOMPointer Family
		{
			get	{ return base.TagClass("FAMC", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "FAMC")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
			}
			else
			{
				result = this.Detail.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMIndividualEvent(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMIndividualEvent(owner, parent, tagName, tagValue);
		}
	}
}
