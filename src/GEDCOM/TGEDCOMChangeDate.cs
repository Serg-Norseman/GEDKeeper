using System;

namespace GedCom551
{
	public sealed class TGEDCOMChangeDate : TGEDCOMTag
	{
		public TGEDCOMDateExact ChangeDate
		{
			get {
				return base.TagClass("DATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact;
			}
		}

		public TGEDCOMTime ChangeTime
		{
			get {
				TGEDCOMTag dateTag = this.ChangeDate;
				return dateTag.TagClass("TIME", TGEDCOMTime.Create) as TGEDCOMTime;
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

		public TGEDCOMNotes Notes
		{
			get { return base.TagClass("NOTE", TGEDCOMNotes.Create) as TGEDCOMNotes; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "CHAN";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else if (tagName == "NOTE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMNotes.Create);
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

		public TGEDCOMChangeDate(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMChangeDate(owner, parent, tagName, tagValue);
		}
	}
}
