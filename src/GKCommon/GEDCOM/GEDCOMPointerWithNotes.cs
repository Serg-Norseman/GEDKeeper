namespace GKCommon.GEDCOM
{
	public class GEDCOMPointerWithNotes : GEDCOMPointer
	{
		private GEDCOMList<GEDCOMNotes> fNotes;

		public GEDCOMList<GEDCOMNotes> Notes
		{
			get { return this.fNotes; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fNotes = new GEDCOMList<GEDCOMNotes>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fNotes.Dispose();
			}
			base.Dispose(disposing);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NOTE")
			{
				result = this.fNotes.Add(new GEDCOMNotes(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this.fNotes.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && base.Count == 0 && this.fNotes.Count == 0;
		}

		public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fNotes.ReplaceXRefs(map);
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fNotes.ResetOwner(newOwner);
		}

		public GEDCOMPointerWithNotes(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
