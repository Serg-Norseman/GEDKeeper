using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMData : TGEDCOMTagWithLists
	{
		private TGEDCOMListEx<TGEDCOMEvent> _Events;

		public TGEDCOMListEx<TGEDCOMEvent> Events
		{
			get { return this._Events; }
		}

		public string Agency
		{
			get { return base.GetTagStringValue("AGNC"); }
			set { base.SetTagStringValue("AGNC", value); }
		}

		/*
		public new TGEDCOMNotes Notes
		{
			get
			{
				return base.GetNote(Index);
			}
		}

		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FName = "DATA";

			this._Events = new TGEDCOMListEx<TGEDCOMEvent>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Events.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "EVEN")
			{
				Result = this._Events.Add(new TGEDCOMEvent(base.Owner, this, ATag, AValue));
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
			this._Events.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Events.Count == 0;
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			this._Events.ResetOwner(AOwner);
		}

		public TGEDCOMData(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
