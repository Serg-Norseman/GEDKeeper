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

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FAMS";
		}

		public TGEDCOMSpouseToFamilyLink(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
