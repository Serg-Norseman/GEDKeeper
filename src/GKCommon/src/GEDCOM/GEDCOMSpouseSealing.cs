using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMSpouseSealing : GEDCOMTagWithLists
	{
		public GEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", GEDCOMDateValue.Create) as GEDCOMDateValue; }
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

		public GEDCOMSpouseSealingDateStatus SpouseSealingDateStatus
		{
			get { return GEDCOMUtils.GetSpouseSealingDateStatusVal(base.GetTagStringValue("STAT")); }
			set { base.SetTagStringValue("STAT", GEDCOMUtils.GetSpouseSealingDateStatusStr(value)); }
		}

		public GEDCOMDateExact SpouseSealingChangeDate
		{
			get { return this.DateStatus.TagClass("CHAN", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		public GEDCOMDateStatus DateStatus
		{
			get { return base.TagClass("STAT", GEDCOMDateStatus.Create) as GEDCOMDateStatus; }
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "STAT")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateStatus.Create);
			}
			else
			{
				// define "DATE" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMSpouseSealing(owner, parent, tagName, tagValue);
        }

        public GEDCOMSpouseSealing(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
