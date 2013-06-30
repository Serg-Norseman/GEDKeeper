using System;

namespace GedCom551
{
	public sealed class TGEDCOMAssociation : TGEDCOMPointerWithNotes
	{
		private GEDCOMList<TGEDCOMSourceCitation> _SourceCitations;

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
			get { return this._SourceCitations; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "ASSO";
			this._SourceCitations = new GEDCOMList<TGEDCOMSourceCitation>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._SourceCitations.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag Result;
			if (tagName == "SOUR")
			{
				Result = this._SourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
			}
			else
			{
				Result = base.AddTag(tagName, tagValue, tagConstructor);
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this._SourceCitations.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._SourceCitations.Count == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this._SourceCitations.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this._SourceCitations.ResetOwner(AOwner);
		}

		public TGEDCOMAssociation(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMAssociation(owner, parent, tagName, tagValue);
		}
	}
}
