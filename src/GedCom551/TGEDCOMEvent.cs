using System;

namespace GedCom551
{
	public sealed class TGEDCOMEvent : TGEDCOMTag
	{
		public TGEDCOMDatePeriod Date
		{
			get	{ return base.TagClass("DATE", TGEDCOMDatePeriod.Create) as TGEDCOMDatePeriod; }
		}

		public TGEDCOMPlace Place
		{
			get	{ return base.TagClass("PLAC", TGEDCOMPlace.Create) as TGEDCOMPlace; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "EVEN";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDatePeriod.Create);
			}
			else
			{
				// define "PLAC" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMEvent(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
