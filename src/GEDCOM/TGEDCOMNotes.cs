using System;
using ExtUtils;

namespace GedCom551
{
	public sealed class TGEDCOMNotes : TGEDCOMPointer
	{
		public bool IsPointer
		{
			get { return (!string.IsNullOrEmpty(base.XRef)); }
		}

		public StringList Notes
		{
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
		}

		private StringList GetNotes()
		{
			StringList notes;

            if (!this.IsPointer)
			{
				notes = base.GetTagStrings(this);
			}
			else
			{
				TGEDCOMRecord notesRecord = base.Value;
				if (notesRecord != null && notesRecord is TGEDCOMNoteRecord) {
					notes = ((notesRecord as TGEDCOMNoteRecord).Note);
				} else {
					notes = new StringList();
				}
			}

            return notes;
		}

		private void SetNotes(StringList value)
		{
			this.Clear();
			base.SetTagStrings(this, value);
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "NOTE";
		}

		protected override string GetStringValue()
		{
			string result = this.IsPointer ? base.GetStringValue() : this.fStringValue;
			return result;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
			}
			base.Dispose(disposing);
		}

		public override void Clear()
		{
			base.Clear();
		}

		public override bool IsEmpty()
		{
			bool result;
			if (this.IsPointer) {
				result = base.IsEmpty();
			} else {
				result = (this.fStringValue == "" && base.Count == 0);
			}
			return result;
		}

		public override string ParseString(string strValue)
		{
			this.fStringValue = "";
			base.XRef = "";
			string result = strValue;
			if (!string.IsNullOrEmpty(result))
			{
				result = GEDCOMUtils.ExtractDelimiter(result, 0);
				result = base.ParseString(result);
				if (!this.IsPointer)
				{
					this.fStringValue = result;
					result = "";
				}
			}
			return result;
		}

		public TGEDCOMNotes(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMNotes(owner, parent, tagName, tagValue);
		}
	}
}
