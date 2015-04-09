namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMRepositoryCitation : GEDCOMPointer
	{
		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "REPO";
		}

		public GEDCOMRepositoryCitation(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
