using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMTag : TGEDCOMCustomTag
	{
		public new TGEDCOMCustomRecord ParentRecord
		{
			get	{ return base.ParentRecord;	}
		}

		public TGEDCOMTag(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
