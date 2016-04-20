namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMEvent : GEDCOMTag
	{
		public GEDCOMDatePeriod Date
		{
			get	{ return base.TagClass("DATE", GEDCOMDatePeriod.Create) as GEDCOMDatePeriod; }
		}

		public GEDCOMPlace Place
		{
			get	{ return base.TagClass("PLAC", GEDCOMPlace.Create) as GEDCOMPlace; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("EVEN");
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDatePeriod.Create);
			}
			else
			{
				// define "PLAC" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMEvent(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMEvent(owner, parent, tagName, tagValue);
		}
	}
}
