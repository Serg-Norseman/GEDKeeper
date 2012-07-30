using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMNoteRecord : TGEDCOMRecord
	{
		private StringList FNote;

		public StringList Note
		{
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
		}

		private StringList GetNotes()
		{
			return base.GetTagStrings(this, ref this.FNote);
		}

		private void SetNotes(StringList Value)
		{
			base.SetTagStrings(this, Value);
		}

		public void SetNotesArray(params string[] Value)
		{
			base.SetTagStrings(this, Value);
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stSource }));
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			Result = base.AddTag(ATag, AValue, ATagConstructor);
			return Result;
		}

		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			StringList cont = new StringList();
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

		public override bool IsMatch(TGEDCOMRecord record, float matchThreshold, MatchParams matchParams)
		{
			bool match = false;

			if (record != null) {
				TGEDCOMNoteRecord note = (TGEDCOMNoteRecord)record;

				string text1 = this.Note.Text;
				string text2 = note.Note.Text;

				match = (string.Compare(text1, text2, true) == 0);
			}

			return match;
		}

		public TGEDCOMNoteRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMNoteRecord(AOwner, AParent, AName, AValue);
		}
	}
}
