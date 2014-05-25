using System;

namespace GedCom551
{
	public sealed class TGEDCOMLocationRecord : TGEDCOMRecord
	{
		public TGEDCOMMap Map
		{
			get { return base.TagClass("MAP", TGEDCOMMap.Create) as TGEDCOMMap; }
		}

		public string LocationName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtLocation;
			this.fName = "_LOC";
		}

		/*public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			// "MAP" defines by default
			return base.AddTag(tagName, tagValue, tagConstructor);
		}*/

		public TGEDCOMLocationRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMLocationRecord(owner, parent, tagName, tagValue);
		}
	}
}
