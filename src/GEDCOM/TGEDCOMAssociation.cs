using System;

namespace GedCom551
{
	public sealed class TGEDCOMAssociation : TGEDCOMPointerWithNotes
	{
		private GEDCOMList<TGEDCOMSourceCitation> fSourceCitations;

		public TGEDCOMIndividualRecord Individual
		{
			get { return (base.Value as TGEDCOMIndividualRecord); }
			set { base.Value = value; }
		}

		public string Relation
		{
			get { return base.GetTagStringValue("RELA"); }
			set { base.SetTagStringValue("RELA", value); }
		}

		public GEDCOMList<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this.fSourceCitations; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "ASSO";
			this.fSourceCitations = new GEDCOMList<TGEDCOMSourceCitation>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fSourceCitations.Dispose();
			}
			base.Dispose(disposing);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;
			if (tagName == "SOUR")
			{
				result = this.fSourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
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

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fSourceCitations.ResetOwner(newOwner);
		}

		public TGEDCOMAssociation(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMAssociation(owner, parent, tagName, tagValue);
		}
	}
}
