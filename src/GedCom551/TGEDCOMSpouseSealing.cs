using System;

namespace GedCom551
{
	public sealed class TGEDCOMSpouseSealing : TGEDCOMTagWithLists
	{
		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateValue), TGEDCOMDateValue.Create) as TGEDCOMDateValue; }
		}

		public string TempleCode
		{
			get { return base.GetTagStringValue("TEMP"); }
			set { base.SetTagStringValue("TEMP", value); }
		}

		public string Place
		{
			get { return base.GetTagStringValue("PLAC"); }
			set { base.SetTagStringValue("PLAC", value); }
		}

		public TGEDCOMSpouseSealingDateStatus SpouseSealingDateStatus
		{
			get { return GetSpouseSealingDateStatusVal(base.GetTagStringValue("STAT").Trim().ToUpper()); }
			set { base.SetTagStringValue("STAT", GetSpouseSealingDateStatusStr(value)); }
		}

		public TGEDCOMDateExact SpouseSealingChangeDate
		{
			get { return this.DateStatus.TagClass("CHAN", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMDateStatus DateStatus
		{
			get { return base.TagClass("STAT", typeof(TGEDCOMDateStatus), TGEDCOMDateStatus.Create) as TGEDCOMDateStatus; }
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag Result;

			if (tagName == "DATE")
			{
				Result = base.AddTag(tagName, tagValue, TGEDCOMDateValue.Create);
			}
			else if (tagName == "STAT")
			{
				Result = base.AddTag(tagName, tagValue, TGEDCOMDateStatus.Create);
			}
			else
			{
				Result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return Result;
		}

		public TGEDCOMSpouseSealing(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
