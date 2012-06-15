using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMMap : TGEDCOMTag
	{
		public double Lati
		{
			get { return base.GetTagFloatValue("LATI", 0.0); }
			set { base.SetTagFloatValue("LATI", value); }
		}

		public double Long
		{
			get { return base.GetTagFloatValue("LONG", 0.0); }
			set { base.SetTagFloatValue("LONG", value); }
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
