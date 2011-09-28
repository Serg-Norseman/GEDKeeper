using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMDateExact : TGEDCOMDate
	{
		public TGEDCOMDateExact(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
