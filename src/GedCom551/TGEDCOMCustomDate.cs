using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public abstract class TGEDCOMCustomDate : TGEDCOMTag
	{
		public DateTime Date
		{
			get { return this.GetDateTime(); }
			set { this.SetDateTime(value); }
		}

		public abstract DateTime GetDateTime();
		public abstract void SetDateTime(DateTime Value);

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "DATE";
		}

		public TGEDCOMCustomDate(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
