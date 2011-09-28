using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMRepositoryCitation : TGEDCOMPointer
	{
		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "REPO";
		}

		public TGEDCOMRepositoryCitation(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
