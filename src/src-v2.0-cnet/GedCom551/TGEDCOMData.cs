using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMData : TGEDCOMTagWithLists
	{

		internal TGEDCOMList FEvents;

		/*[Browsable(false)]
		public TGEDCOMEvent Events
		{
			get
			{
				return this.GetEvents(Index);
			}
		}

		[Browsable(false)]
		public int EventsCount
		{
			get
			{
				return this.GetEventsCount();
			}
		}*/

		[Browsable(false)]
		public string Agency
		{
			get { return this.GetAgency(); }
			set { this.SetAgency(value); }
		}

		/*[Browsable(false)]
		public new TGEDCOMNotes Notes
		{
			get
			{
				return base.GetNote(Index);
			}
		}
		[Browsable(false)]
		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		internal TGEDCOMEvent GetEvent(int Index)
		{
			TGEDCOMEvent Result;
			if (this.FEvents == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FEvents[Index] as TGEDCOMEvent);
			}
			return Result;
		}
		internal int GetEventsCount()
		{
			int Result;
			if (this.FEvents == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FEvents.Count;
			}
			return Result;
		}
		internal string GetAgency()
		{
			return base.GetTagStringValue("AGNC");
		}
		internal void SetAgency([In] string Value)
		{
			base.SetTagStringValue("AGNC", Value);
		}
		protected internal TGEDCOMEvent AddEvent(TGEDCOMEvent Value)
		{
			if (this.FEvents == null)
			{
				this.FEvents = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FEvents.Add(Value);
			}
			return Value;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FName = "DATA";
			this.FEvents = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FEvents != null)
				{
					this.FEvents.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag.Equals("EVEN"))
			{
				Result = this.AddEvent(new TGEDCOMEvent(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FEvents != null)
			{
				this.FEvents.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetEventsCount() == 0;
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FEvents != null)
			{
				this.FEvents.ResetOwner(AOwner);
			}
		}

		public TGEDCOMData(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
