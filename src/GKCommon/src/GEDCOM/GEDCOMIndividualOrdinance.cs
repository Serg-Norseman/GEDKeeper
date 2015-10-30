using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	// FIXME: возможны многочисленные нарушения стандарта, перепроверить вложенность тэгов
	public sealed class GEDCOMIndividualOrdinance : GEDCOMTagWithLists
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

		public GEDCOMBaptismDateStatus BaptismDateStatus
		{
			get { return GEDCOMUtils.GetBaptismDateStatusVal(base.GetTagStringValue("STAT")); }
			set { base.SetTagStringValue("STAT", GEDCOMUtils.GetBaptismDateStatusStr(value)); }
		}

		public GEDCOMDateExact BaptismChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public GEDCOMEndowmentDateStatus EndowmentDateStatus
		{
			get { return GEDCOMUtils.GetEndowmentDateStatusVal(base.GetTagStringValue("STAT")); }
			set { base.SetTagStringValue("STAT", GEDCOMUtils.GetEndowmentDateStatusStr(value)); }
		}

		public GEDCOMDateExact EndowmentChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public GEDCOMPointer Family
		{
			get { return base.TagClass("FAMC", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public GEDCOMChildSealingDateStatus ChildSealingDateStatus
		{
			get { return GEDCOMUtils.GetChildSealingDateStatusVal(base.GetTagStringValue("STAT")); }
			set { base.SetTagStringValue("STAT", GEDCOMUtils.GetChildSealingDateStatusStr(value)); }
		}

		public GEDCOMDateExact ChildSealingChangeDate
		{
			get { return this.GetChangeDate(); }
		}


		private GEDCOMDateExact GetChangeDate()
		{
			return this.DateStatus.TagClass("CHAN", GEDCOMDateExact.Create) as GEDCOMDateExact;
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
			else if (tagName == "FAMC")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
			}
			else
			{
				// define "DATE" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMIndividualOrdinance(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMIndividualOrdinance(owner, parent, tagName, tagValue);
		}
	}
}
