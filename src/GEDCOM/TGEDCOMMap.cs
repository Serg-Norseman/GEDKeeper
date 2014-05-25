using System;

namespace GedCom551
{
	public sealed class TGEDCOMMap : TGEDCOMTag
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

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "MAP";
		}

		public TGEDCOMMap(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMMap(owner, parent, tagName, tagValue);
		}
	}
}
