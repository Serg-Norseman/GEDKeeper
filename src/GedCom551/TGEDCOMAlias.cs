using System;

namespace GedCom551
{
	public sealed class TGEDCOMAlias : TGEDCOMPointer
	{
		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "ALIA";
		}

		public TGEDCOMAlias(TGEDCOMTree AOwner, TGEDCOMObject AParent, string AName, string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, string AName, string AValue)
		{
			return new TGEDCOMAlias(AOwner, AParent, AName, AValue);
		}
	}
}
