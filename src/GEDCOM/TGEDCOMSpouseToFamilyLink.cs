using System;

namespace GedCom551
{
	public sealed class TGEDCOMSpouseToFamilyLink : TGEDCOMPointerWithNotes
	{
		public TGEDCOMFamilyRecord Family
		{
			get { return (base.Value as TGEDCOMFamilyRecord); }
			set { base.Value = value; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "FAMS";
		}

		public TGEDCOMSpouseToFamilyLink(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMSpouseToFamilyLink(owner, parent, tagName, tagValue);
		}
	}
}
