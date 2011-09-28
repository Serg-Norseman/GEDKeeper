using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMEvent : TGEDCOMTag
	{
		public TGEDCOMDatePeriod Date
		{
			get	{ return base.TagClass("DATE", typeof(TGEDCOMDatePeriod)) as TGEDCOMDatePeriod; }
		}

		public TGEDCOMPlace Place
		{
			get	{ return base.TagClass("PLAC", typeof(TGEDCOMPlace)) as TGEDCOMPlace; }
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "EVEN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDatePeriod));
			}
			else
			{
				if (ATag == "PLAC")
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
