namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMSpouseToFamilyLink : GEDCOMPointerWithNotes
	{
		public GEDCOMFamilyRecord Family
		{
			get { return (base.Value as GEDCOMFamilyRecord); }
			set { base.Value = value; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "FAMS";
		}

		public GEDCOMSpouseToFamilyLink(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMSpouseToFamilyLink(owner, parent, tagName, tagValue);
		}
	}
}
