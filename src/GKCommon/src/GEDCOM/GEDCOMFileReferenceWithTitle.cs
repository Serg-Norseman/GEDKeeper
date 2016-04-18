namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMFileReferenceWithTitle : GEDCOMFileReference
	{
		public string Title
		{
			get { return base.GetTagStringValue("TITL"); }
			set { base.SetTagStringValue("TITL", value); }
		}

		protected override string MediaTypeTagName()
		{
			return @"FORM\TYPE";
		}

		public GEDCOMFileReferenceWithTitle(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
