using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMSpouseToFamilyLink : TGEDCOMPointerWithNotes
	{
		public TGEDCOMFamilyRecord Family
		{
			get { return base.GetValue() as TGEDCOMFamilyRecord; }
			set { base.SetValue(value); }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FAMS";
		}

		public TGEDCOMSpouseToFamilyLink(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMSpouseToFamilyLink(AOwner, AParent, AName, AValue);
		}
	}
}
