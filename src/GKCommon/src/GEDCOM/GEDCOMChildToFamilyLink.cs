namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMChildToFamilyLink : GEDCOMPointerWithNotes
	{
		public GEDCOMChildLinkageStatus ChildLinkageStatus
		{
			get { return GEDCOMUtils.GetChildLinkageStatusVal(base.GetTagStringValue("STAT")); }
			set { base.SetTagStringValue("STAT", GEDCOMUtils.GetChildLinkageStatusStr(value)); }
		}

		public GEDCOMPedigreeLinkageType PedigreeLinkageType
		{
			get { return GEDCOMUtils.GetPedigreeLinkageTypeVal(base.GetTagStringValue("PEDI")); }
			set { base.SetTagStringValue("PEDI", GEDCOMUtils.GetPedigreeLinkageTypeStr(value)); }
		}

		public GEDCOMFamilyRecord Family
		{
			get { return (base.Value as GEDCOMFamilyRecord); }
			set { base.Value = value; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("FAMC");
		}

		public GEDCOMChildToFamilyLink(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMChildToFamilyLink(owner, parent, tagName, tagValue);
		}
	}
}
