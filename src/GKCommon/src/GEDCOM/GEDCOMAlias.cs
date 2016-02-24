namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMAlias : GEDCOMPointer
	{
		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("ALIA");
		}

		public GEDCOMAlias(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMAlias(owner, parent, tagName, tagValue);
		}
	}
}
