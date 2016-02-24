namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMPlace : GEDCOMTagWithLists
	{
		public string Form
		{
			get { return base.GetTagStringValue("FORM"); }
			set { base.SetTagStringValue("FORM", value); }
		}

		public GEDCOMPointer Location
		{
			get { return base.TagClass("_LOC", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public GEDCOMMap Map
		{
			get { return base.TagClass("MAP", GEDCOMMap.Create) as GEDCOMMap; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("PLAC");
		}

		/*public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			// "MAP", "_LOC" defines by default
			return base.AddTag(tagName, tagValue, tagConstructor);
		}*/

		public GEDCOMPlace(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMPlace(owner, parent, tagName, tagValue);
		}
	}
}
