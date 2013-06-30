using System;

namespace GedCom551
{
	public sealed class TGEDCOMRepositoryCitation : TGEDCOMPointer
	{
		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "REPO";
		}

		public TGEDCOMRepositoryCitation(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
