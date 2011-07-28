using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMEvent : TGEDCOMTag
	{
		[Browsable(false)]
		public TGEDCOMDatePeriod Date
		{
			get	{ return this.GetDate(); }
		}

		[Browsable(false)]
		public TGEDCOMPlace Place
		{
			get	{ return this.GetPlace(); }
		}

		internal TGEDCOMDatePeriod GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDatePeriod)) as TGEDCOMDatePeriod;
		}

		internal TGEDCOMPlace GetPlace()
		{
			return base.TagClass("PLAC", typeof(TGEDCOMPlace)) as TGEDCOMPlace;
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "EVEN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "DATE") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDatePeriod));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "PLAC") == 0)
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPlace));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public TGEDCOMEvent(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
