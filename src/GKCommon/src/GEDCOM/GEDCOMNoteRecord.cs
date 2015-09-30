using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMNoteRecord : GEDCOMRecord
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

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtNote;
			this.fName = "NOTE";
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
			}
			base.Dispose(disposing);
		}

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            if (targetRecord == null) return;

			StringList cont = new StringList();
			try
			{
				GEDCOMNoteRecord targetNote = (targetRecord as GEDCOMNoteRecord);
				
				cont.Text = targetNote.Note.Text;
                base.MoveTo(targetRecord, clearDest);
				targetNote.Note = cont;
			}
			finally
			{
                cont.Dispose();
			}
		}

		public GEDCOMNoteRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMNoteRecord(owner, parent, tagName, tagValue);
		}

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
        	if (tag == null) return 0.0f;
			float match = 0.0f;

			GEDCOMNoteRecord note = tag as GEDCOMNoteRecord;
			if (string.Compare(this.Note.Text, note.Note.Text, true) == 0) {
				match = 100.0f;
			}

			return match;
		}

        #region Auxiliary

		public void AddNoteText(string text)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = this.Note.Text.Trim();
				strData.Add(text);
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
