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
			get { return this.GetNotes(); }
		}

		private TGEDCOMDateExact GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		private TGEDCOMTime GetTime()
		{
			TGEDCOMTag DateTag = base.FindTag("DATE", 0);
			if (DateTag == null)
			{
				DateTag = this.AddTag("DATE", "", null);
			}
			return DateTag.TagClass("TIME", typeof(TGEDCOMTime)) as TGEDCOMTime;
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

		private TGEDCOMNotes GetNotes()
		{
			return base.TagClass("NOTE", typeof(TGEDCOMNotes)) as TGEDCOMNotes;
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "CHAN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
			}
			else
			{
				if (ATag == "NOTE")
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMNotes));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public override string ToString()
		{
			DateTime cdt = this.GetChangeDateTime();
			string Result;
			if (cdt.Ticks == 0)
			{
				Result = "";
			}
			else
			{
				Result = cdt.ToString("yyyy.MM.dd HH:mm:ss", null);
			}
			return Result;
		}

		public TGEDCOMChangeDate(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
