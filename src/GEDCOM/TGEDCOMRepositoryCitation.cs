using System;

namespace GedCom551
{
	public sealed class TGEDCOMRepositoryCitation : TGEDCOMPointer
	{
		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "REPO";
		}

		public TGEDCOMRepositoryCitation(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
