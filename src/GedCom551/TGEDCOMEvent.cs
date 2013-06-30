using System;

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

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "EVEN";
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
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

		public TGEDCOMEvent(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
