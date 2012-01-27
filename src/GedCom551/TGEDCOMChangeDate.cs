using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMChangeDate : TGEDCOMTag
	{
		public TGEDCOMDateExact ChangeDate
		{
			get { return this.GetDate(); }
		}

		public TGEDCOMTime ChangeTime
		{
			get { return this.GetTime(); }
		}

		public DateTime ChangeDateTime
		{
			get { return this.GetChangeDateTime(); }
			set { this.SetChangeDateTime(value); }
		}

		public TGEDCOMNotes Notes
		{
			get { return base.TagClass("NOTE", typeof(TGEDCOMNotes), TGEDCOMNotes.Create) as TGEDCOMNotes; }
		}

		private TGEDCOMDateExact GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact;
		}

		private TGEDCOMTime GetTime()
		{
			TGEDCOMTag DateTag = base.FindTag("DATE", 0);
			if (DateTag == null)
			{
				DateTag = this.AddTag("DATE", "", null);
			}
			return DateTag.TagClass("TIME", typeof(TGEDCOMTime), TGEDCOMTime.Create) as TGEDCOMTime;
		}

		private DateTime GetChangeDateTime()
		{
			return this.GetDate().Date + this.GetTime().Time;
		}

		private void SetChangeDateTime([In] DateTime Value)
		{
			this.GetDate().Date = Value.Date;
			if (Value.TimeOfDay != TimeSpan.Zero)
			{
				this.GetTime().Time = Value.TimeOfDay;
			}
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "CHAN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
			}
			else
			{
				if (ATag == "NOTE")
				{
					Result = base.AddTag(ATag, AValue, TGEDCOMNotes.Create);
				}
				else
				{
					Result = base.AddTag(ATag, AValue, ATagConstructor);
				}
			}
			return Result;
		}

		public override string ToString()
		{
			DateTime cdt = this.GetChangeDateTime();
			string result = ((cdt.Ticks == 0) ? "" : cdt.ToString("yyyy.MM.dd HH:mm:ss", null));
			return result;
		}

		public TGEDCOMChangeDate(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMChangeDate(AOwner, AParent, AName, AValue);
		}
	}
}
