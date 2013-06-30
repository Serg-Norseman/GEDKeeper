using System;
using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMNoteRecord : TGEDCOMRecord
	{
		private StringList FNote;

		public StringList Note
		{
			get {
				return base.GetTagStrings(this, ref this.FNote);
			}
			set {
				base.SetTagStrings(this, value);
			}
		}

		public void SetNotesArray(params string[] Value)
		{
			base.SetTagStrings(this, Value);
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
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

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			return base.AddTag(ATag, AValue, ATagConstructor);
		}

		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			StringList cont = new StringList();
			try
			{
				TGEDCOMNoteRecord to_note = (aToRecord as TGEDCOMNoteRecord);
				
				cont.Text = to_note.Note.Text;
				base.MoveTo(aToRecord, aClearDest);
				to_note.Note = cont;
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
				TGEDCOMNoteRecord note = record as TGEDCOMNoteRecord;

				string text1 = this.Note.Text;
				string text2 = note.Note.Text;

				match = (string.Compare(text1, text2, true) == 0);
			}

			return match;
		}

		public TGEDCOMNoteRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMNoteRecord(owner, parent, tagName, tagValue);
		}
	}
}
