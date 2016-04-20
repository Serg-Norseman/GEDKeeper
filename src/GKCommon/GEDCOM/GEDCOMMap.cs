namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMMap : GEDCOMTag
	{
		public double Lati
		{
			get { return base.GetTagFloatValue("LATI", 0.0); }
			set { base.SetTagFloatValue("LATI", value); }
		}

		public double Long
		{
			get { return base.GetTagFloatValue("LONG", 0.0); }
			set { base.SetTagFloatValue("LONG", value); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("MAP");
		}

		public GEDCOMMap(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMMap(owner, parent, tagName, tagValue);
		}
	}
}
