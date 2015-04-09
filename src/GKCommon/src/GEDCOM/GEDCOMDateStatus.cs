namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMDateStatus : GEDCOMTag
	{
		public GEDCOMDateExact ChangeDate
		{
			get { return base.TagClass("DATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "STAT";
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMDateStatus(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMDateStatus(owner, parent, tagName, tagValue);
		}
	}
}
