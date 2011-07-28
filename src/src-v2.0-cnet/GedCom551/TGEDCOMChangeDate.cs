using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMChangeDate : TGEDCOMTag
	{
		[Browsable(false)]
		public TGEDCOMDateExact ChangeDate
		{
			get
			{
				return this.GetDate();
			}
		}
		[Browsable(false)]
		public TGEDCOMTime ChangeTime
		{
			get
			{
				return this.GetTime();
			}
		}
		[Browsable(false)]
		public DateTime ChangeDateTime
		{
			get
			{
				return this.GetChangeDateTime();
			}
			set
			{
				this.SetChangeDateTime(value);
			}
		}
		[Browsable(false)]
		public TGEDCOMNotes Notes
		{
			get
			{
				return this.GetNotes();
			}
		}
		internal TGEDCOMDateExact GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}
		internal TGEDCOMTime GetTime()
		{
			TGEDCOMTag DateTag = base.FindTag("DATE", 0);
			if (DateTag == null)
			{
				DateTag = this.AddTag("DATE", "", null);
			}
			return DateTag.TagClass("TIME", typeof(TGEDCOMTime)) as TGEDCOMTime;
		}
		internal DateTime GetChangeDateTime()
		{
			return this.GetDate().Date + this.GetTime().Time;
		}
		internal void SetChangeDateTime([In] DateTime Value)
		{
			this.GetDate().Date = Value.Date;
			if (Value.TimeOfDay != TimeSpan.Zero)
			{
				this.GetTime().Time = Value.TimeOfDay;
			}
		}
		internal TGEDCOMNotes GetNotes()
		{
			return base.TagClass("NOTE", typeof(TGEDCOMNotes)) as TGEDCOMNotes;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "CHAN";
		}
		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "DATE") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "NOTE") == 0)
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
		public override bool IsEmpty()
		{
			return base.IsEmpty();
		}
		public override string ToString()
		{
			DateTime cdt = this.GetChangeDateTime();
			string Result;
			if (cdt.Ticks == (long)((ulong)0))
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
