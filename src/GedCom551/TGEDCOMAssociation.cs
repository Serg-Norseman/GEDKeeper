using System;

namespace GedCom551
{
	public sealed class TGEDCOMAssociation : TGEDCOMPointerWithNotes
	{
		private TGEDCOMListEx<TGEDCOMSourceCitation> _SourceCitations;

		public TGEDCOMIndividualRecord Individual
		{
			get { return base.GetValue() as TGEDCOMIndividualRecord; }
			set { base.SetValue(value); }
		}

		public string Relation
		{
			get { return base.GetTagStringValue("RELA"); }
			set { base.SetTagStringValue("RELA", value); }
		}

		public TGEDCOMListEx<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this._SourceCitations; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "ASSO";
			this._SourceCitations = new TGEDCOMListEx<TGEDCOMSourceCitation>(this);
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

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "SOUR")
			{
				Result = this._SourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
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

		public TGEDCOMAssociation(TGEDCOMTree AOwner, TGEDCOMObject AParent, string AName, string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, string AName, string AValue)
		{
			return new TGEDCOMAssociation(AOwner, AParent, AName, AValue);
		}
	}
}
