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

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "REFN";
		}

		public TGEDCOMUserReference(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMUserReference(owner, parent, tagName, tagValue);
		}
	}
}
