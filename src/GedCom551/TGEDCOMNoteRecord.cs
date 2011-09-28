using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMNoteRecord : TGEDCOMRecord
	{
		private TStrings FNote;

		public TStrings Note
		{
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
		}

		private TStrings GetNotes()
		{
			return base.GetTagStrings(this, ref this.FNote);
		}

		private void SetNotes(TStrings Value)
		{
			base.SetTagStrings(this, Value);
		}

		public void SetNotesArray(params string[] Value)
		{
			base.SetTagStrings(this, Value);
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stSource
			}));
			this.FRecordType = TGEDCOMRecordType.rtNote;
			this.FName = "NOTE";

			this.FNote = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FNote != null)
				{
					this.FNote.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "REFN") {
				Result = base.UserReferences.Add(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
			} else {
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			TStringList cont = new TStringList();
			try
			{
				cont.Text = (aToRecord as TGEDCOMNoteRecord).Note.Text;
				base.MoveTo(aToRecord, aClearDest);
				(aToRecord as TGEDCOMNoteRecord).Note = cont;
			}
			finally
			{
				cont.Free();
			}
		}

		public TGEDCOMNoteRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
