namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMAssociation : GEDCOMPointerWithNotes
	{
		private GEDCOMList<GEDCOMSourceCitation> fSourceCitations;

		public GEDCOMIndividualRecord Individual
		{
			get { return (base.Value as GEDCOMIndividualRecord); }
			set { base.Value = value; }
		}

		public string Relation
		{
			get { return base.GetTagStringValue("RELA"); }
			set { base.SetTagStringValue("RELA", value); }
		}

		public GEDCOMList<GEDCOMSourceCitation> SourceCitations
		{
			get { return this.fSourceCitations; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("ASSO");
			this.fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fSourceCitations.Dispose();
			}
			base.Dispose(disposing);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;
			if (tagName == "SOUR")
			{
				result = this.fSourceCitations.Add(new GEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
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
			this.fSourceCitations.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fSourceCitations.Count == 0;
		}

		public override void ReplaceXRefs(XRefReplacer map)
		{
			base.ReplaceXRefs(map);
			this.fSourceCitations.ReplaceXRefs(map);
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fSourceCitations.ResetOwner(newOwner);
		}

		public GEDCOMAssociation(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMAssociation(owner, parent, tagName, tagValue);
		}
	}
}
