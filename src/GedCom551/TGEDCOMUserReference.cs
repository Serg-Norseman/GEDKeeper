using System;

namespace GedCom551
{
	public sealed class TGEDCOMUserReference : TGEDCOMTag
	{
		public string ReferenceType
		{
			get	{ return base.GetTagStringValue("TYPE"); }
			set	{ base.SetTagStringValue("TYPE", value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "REFN";
		}

		public TGEDCOMUserReference(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMUserReference(owner, parent, tagName, tagValue);
		}
	}
}
