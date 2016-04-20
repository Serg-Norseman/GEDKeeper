namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMDateExact : GEDCOMDate
	{
		public GEDCOMDateExact(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMDateExact(owner, parent, tagName, tagValue);
		}
	}
}
