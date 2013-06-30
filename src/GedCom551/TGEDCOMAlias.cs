using System;

namespace GedCom551
{
	public sealed class TGEDCOMAlias : TGEDCOMPointer
	{
		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "ALIA";
		}

		public TGEDCOMAlias(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMAlias(owner, parent, tagName, tagValue);
		}
	}
}
