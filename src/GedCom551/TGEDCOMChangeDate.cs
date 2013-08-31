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
				TGEDCOMTag DateTag = this.ChangeDate;
				return DateTag.TagClass("TIME", TGEDCOMTime.Create) as TGEDCOMTime;
			}
		}

		public DateTime ChangeDateTime
		{
			get {
				return this.ChangeDate.Date + this.ChangeTime.Value;
			}
			set {
				this.ChangeDate.Date = value.Date;
				if (value.TimeOfDay != TimeSpan.Zero) {
					this.ChangeTime.Value = value.TimeOfDay;
				}
			}
		}

		public TGEDCOMNotes Notes
		{
			get { return base.TagClass("NOTE", TGEDCOMNotes.Create) as TGEDCOMNotes; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "CHAN";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag Result;

			if (tagName == "DATE")
			{
				Result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else if (tagName == "NOTE")
			{
				Result = base.AddTag(tagName, tagValue, TGEDCOMNotes.Create);
			}
			else
			{
				Result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return Result;
		}

		public override string ToString()
		{
			DateTime cdt = this.ChangeDateTime;
			string result = ((cdt.Ticks == 0) ? "" : cdt.ToString("yyyy.MM.dd HH:mm:ss", null));
			return result;
		}

		public TGEDCOMChangeDate(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMChangeDate(owner, parent, tagName, tagValue);
		}
	}
}
