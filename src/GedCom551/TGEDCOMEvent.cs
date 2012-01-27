using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMEvent : TGEDCOMTag
	{
		public TGEDCOMDatePeriod Date
		{
			get	{ return base.TagClass("DATE", typeof(TGEDCOMDatePeriod), TGEDCOMDatePeriod.Create) as TGEDCOMDatePeriod; }
		}

		public TGEDCOMPlace Place
		{
			get	{ return base.TagClass("PLAC", typeof(TGEDCOMPlace), TGEDCOMPlace.Create) as TGEDCOMPlace; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "EVEN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDatePeriod.Create);
			}
			else
			{
				// define "PLAC" by default
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public TGEDCOMEvent(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
