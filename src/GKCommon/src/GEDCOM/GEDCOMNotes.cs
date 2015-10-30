using ExtUtils;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMNotes : GEDCOMPointer
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
				GEDCOMRecord notesRecord = base.Value;
				if (notesRecord != null && notesRecord is GEDCOMNoteRecord) {
					notes = ((notesRecord as GEDCOMNoteRecord).Note);
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

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "NOTE";
		}

		protected override string GetStringValue()
		{
			string result = this.IsPointer ? base.GetStringValue() : this.fStringValue;
			return result;
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

		public GEDCOMNotes(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMNotes(owner, parent, tagName, tagValue);
		}
	}
}
