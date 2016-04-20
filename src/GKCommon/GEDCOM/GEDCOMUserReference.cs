namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMUserReference : GEDCOMTag
	{
		public string ReferenceType
		{
			get	{ return base.GetTagStringValue("TYPE"); }
			set	{ base.SetTagStringValue("TYPE", value); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
            base.SetName("REFN");
		}

		public GEDCOMUserReference(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMUserReference(owner, parent, tagName, tagValue);
		}
	}
}
