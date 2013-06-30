using System;

namespace GedCom551
{
	public sealed class TGEDCOMPlace : TGEDCOMTagWithLists
	{
		public string Form
		{
			get { return base.GetTagStringValue("FORM"); }
			set { base.SetTagStringValue("FORM", value); }
		}

		public TGEDCOMPointer Location
		{
			get { return base.TagClass("_LOC", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMMap Map
		{
			get { return base.TagClass("MAP", typeof(TGEDCOMMap), TGEDCOMMap.Create) as TGEDCOMMap; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "PLAC";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			// "MAP", "_LOC" defines by default
			return base.AddTag(tagName, tagValue, tagConstructor);
		}

		public TGEDCOMPlace(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMPlace(owner, parent, tagName, tagValue);
		}
	}
}
