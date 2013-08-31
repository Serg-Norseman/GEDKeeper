using System;
using System.IO;

namespace GedCom551
{
	public class TGEDCOMTagWithLists : TGEDCOMTag
	{
		protected GEDCOMList<TGEDCOMNotes> _Notes;
		protected GEDCOMList<TGEDCOMSourceCitation> _SourceCitations;
		protected GEDCOMList<TGEDCOMMultimediaLink> _MultimediaLinks;

		public GEDCOMList<TGEDCOMNotes> Notes
		{
			get { return this._Notes; }
		}

		public GEDCOMList<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this._SourceCitations; }
		}

		public GEDCOMList<TGEDCOMMultimediaLink> MultimediaLinks
		{
			get { return this._MultimediaLinks; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);

			this._Notes = new GEDCOMList<TGEDCOMNotes>(this);
			this._SourceCitations = new GEDCOMList<TGEDCOMSourceCitation>(this);
			this._MultimediaLinks = new GEDCOMList<TGEDCOMMultimediaLink>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Notes.Dispose();
				this._SourceCitations.Dispose();
				this._MultimediaLinks.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Pack()
		{
			base.Pack();

			this._Notes.Pack();
			this._SourceCitations.Pack();
			this._MultimediaLinks.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);

			this._Notes.ReplaceXRefs(aMap);
			this._SourceCitations.ReplaceXRefs(aMap);
			this._MultimediaLinks.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);

			this._Notes.ResetOwner(AOwner);
			this._SourceCitations.ResetOwner(AOwner);
			this._MultimediaLinks.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);

			this._Notes.SaveToStream(AStream);
			this._SourceCitations.SaveToStream(AStream);
			this._MultimediaLinks.SaveToStream(AStream);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NOTE")
			{
				result = this.Notes.Add(new TGEDCOMNotes(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "SOUR")
			{
				result = this.SourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "OBJE")
			{
				result = this.MultimediaLinks.Add(new TGEDCOMMultimediaLink(base.Owner, this, tagName, tagValue));
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

			this._Notes.Clear();
			this._SourceCitations.Clear();
			this._MultimediaLinks.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Notes.Count == 0 && this._SourceCitations.Count == 0 && this._MultimediaLinks.Count == 0;
		}

		public TGEDCOMTagWithLists(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
