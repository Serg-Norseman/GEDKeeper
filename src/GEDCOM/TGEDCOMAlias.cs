using System;

namespace GedCom551
{
	public sealed class TGEDCOMAlias : TGEDCOMPointer
	{
		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "ALIA";
		}

		public TGEDCOMAlias(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMAlias(owner, parent, tagName, tagValue);
		}
	}
}
