using System;
using ExtUtils;

namespace GedCom551
{
	public sealed class TGEDCOMNoteRecord : TGEDCOMRecord
	{
		public StringList Note
		{
			get { return base.GetTagStrings(this); }
			set { base.SetTagStrings(this, value); }
		}

		public void SetNotesArray(params string[] value)
		{
			base.SetTagStrings(this, value);
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtNote;
			this.fName = "NOTE";
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
			}
			base.Dispose(disposing);
		}

        public override void MoveTo(TGEDCOMRecord targetRecord, bool clearDest)
        {
            if (targetRecord == null) return;

			StringList cont = new StringList();
			try
			{
				TGEDCOMNoteRecord targetNote = (targetRecord as TGEDCOMNoteRecord);
				
				cont.Text = targetNote.Note.Text;
                base.MoveTo(targetRecord, clearDest);
				targetNote.Note = cont;
			}
			finally
			{
                cont.Dispose();
			}
		}

		public TGEDCOMNoteRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMNoteRecord(owner, parent, tagName, tagValue);
		}

        public override float IsMatch(TGEDCOMTag tag, MatchParams matchParams)
		{
        	if (tag == null) return 0.0f;
			float match = 0.0f;

			TGEDCOMNoteRecord note = tag as TGEDCOMNoteRecord;
			if (string.Compare(this.Note.Text, note.Note.Text, true) == 0) {
				match = 100.0f;
			}

			return match;
		}

        #region Auxiliary

		public void aux_AddNoteText(string aText)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = this.Note.Text.Trim();
				strData.Add(aText);
				this.Note = strData;
			}
			finally
			{
                strData.Dispose();
			}
		}

        #endregion
	}
}
