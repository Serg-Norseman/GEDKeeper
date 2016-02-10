namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMLocationRecord : GEDCOMRecord
	{
		public GEDCOMMap Map
		{
			get { return base.TagClass("MAP", GEDCOMMap.Create) as GEDCOMMap; }
		}

		public string LocationName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtLocation;
			this.fName = "_LOC";
		}

		/*public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			// "MAP" defines by default
			return base.AddTag(tagName, tagValue, tagConstructor);
		}*/

		public GEDCOMLocationRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMLocationRecord(owner, parent, tagName, tagValue);
		}
	}
}
