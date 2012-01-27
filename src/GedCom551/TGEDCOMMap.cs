using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMMap : TGEDCOMTag
	{
		public string Lati
		{
			get { return base.GetTagStringValue("LATI"); }
			set { base.SetTagStringValue("LATI", value); }
		}

		public string Long
		{
			get { return base.GetTagStringValue("LONG"); }
			set { base.SetTagStringValue("LONG", value); }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "MAP";
		}

		public TGEDCOMMap(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMMap(AOwner, AParent, AName, AValue);
		}
	}
}
