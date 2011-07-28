using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMSpouseToFamilyLink : TGEDCOMPointerWithNotes
	{
		public TGEDCOMFamilyRecord Family
		{
			get { return this.GetFamily(); }
			set { this.SetFamily(value); }
		}

		internal TGEDCOMFamilyRecord GetFamily()
		{
			return base.GetValue() as TGEDCOMFamilyRecord;
		}

		internal void SetFamily([In] TGEDCOMFamilyRecord Value)
		{
			base.SetValue(Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FAMS";
		}

		public TGEDCOMSpouseToFamilyLink(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
