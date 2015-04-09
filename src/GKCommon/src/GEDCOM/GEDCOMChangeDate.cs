using System;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMChangeDate : GEDCOMTag
	{
		public GEDCOMDateExact ChangeDate
		{
			get {
				return base.TagClass("DATE", GEDCOMDateExact.Create) as GEDCOMDateExact;
			}
		}

		public GEDCOMTime ChangeTime
		{
			get {
				GEDCOMTag dateTag = this.ChangeDate;
				return dateTag.TagClass("TIME", GEDCOMTime.Create) as GEDCOMTime;
			}
		}

		public DateTime ChangeDateTime
		{
			get {
				return this.ChangeDate.Date + this.ChangeTime.Value;
			}
			set {
				this.ChangeDate.Date = value.Date;
				this.ChangeTime.Value = value.TimeOfDay;
			}
		}

		public GEDCOMNotes Notes
		{
			get { return base.TagClass("NOTE", GEDCOMNotes.Create) as GEDCOMNotes; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "CHAN";
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
			}
			else if (tagName == "NOTE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMNotes.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override string ToString()
		{
			DateTime cdt = this.ChangeDateTime;
			string result = ((cdt.Ticks == 0) ? "" : cdt.ToString("yyyy.MM.dd HH:mm:ss", null));
			return result;
		}

		public GEDCOMChangeDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMChangeDate(owner, parent, tagName, tagValue);
		}
	}
}
