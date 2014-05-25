using System;
using System.IO;

namespace GedCom551
{
	public class TGEDCOMTagWithLists : TGEDCOMTag, IGEDCOMStructWithLists
	{
		protected GEDCOMList<TGEDCOMNotes> fNotes;
		protected GEDCOMList<TGEDCOMSourceCitation> fSourceCitations;
		protected GEDCOMList<TGEDCOMMultimediaLink> fMultimediaLinks;

		public GEDCOMList<TGEDCOMNotes> Notes
		{
			get { return this.fNotes; }
		}

		public GEDCOMList<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this.fSourceCitations; }
		}

		public GEDCOMList<TGEDCOMMultimediaLink> MultimediaLinks
		{
			get { return this.fMultimediaLinks; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);

			this.fNotes = new GEDCOMList<TGEDCOMNotes>(this);
			this.fSourceCitations = new GEDCOMList<TGEDCOMSourceCitation>(this);
			this.fMultimediaLinks = new GEDCOMList<TGEDCOMMultimediaLink>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fNotes.Dispose();
				this.fSourceCitations.Dispose();
				this.fMultimediaLinks.Dispose();
			}
			base.Dispose(disposing);
		}

		public override void Pack()
		{
			base.Pack();

			this.fNotes.Pack();
			this.fSourceCitations.Pack();
			this.fMultimediaLinks.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);

            this.fNotes.ReplaceXRefs(map);
            this.fSourceCitations.ReplaceXRefs(map);
            this.fMultimediaLinks.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);

			this.fNotes.ResetOwner(newOwner);
			this.fSourceCitations.ResetOwner(newOwner);
			this.fMultimediaLinks.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);

			this.fNotes.SaveToStream(stream);
			this.fSourceCitations.SaveToStream(stream);
			this.fMultimediaLinks.SaveToStream(stream);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NOTE")
			{
				result = this.fNotes.Add(new TGEDCOMNotes(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "SOUR")
			{
				result = this.fSourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "OBJE")
			{
				result = this.fMultimediaLinks.Add(new TGEDCOMMultimediaLink(base.Owner, this, tagName, tagValue));
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
			this.fSourceCitations.Clear();
			this.fMultimediaLinks.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fNotes.Count == 0 && this.fSourceCitations.Count == 0 && this.fMultimediaLinks.Count == 0;
		}

		protected TGEDCOMTagWithLists(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        #region Auxiliary

        public TGEDCOMNotes aux_AddNote(TGEDCOMNoteRecord noteRec)
        {
        	TGEDCOMNotes note = null;
        	
            if (noteRec != null)
            {
                note = new TGEDCOMNotes(this.Owner, this, "", "");
                note.Value = noteRec;
                this.Notes.Add(note);
            }
            
            return note;
        }

        public TGEDCOMSourceCitation aux_AddSource(TGEDCOMSourceRecord sourceRec, string page, int quality)
        {
        	TGEDCOMSourceCitation cit = null;
        	
            if (sourceRec != null)
            {
                cit = new TGEDCOMSourceCitation(this.Owner, this, "", "");
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                this.SourceCitations.Add(cit);
            }
            
            return cit;
        }

        public TGEDCOMMultimediaLink aux_AddMultimedia(TGEDCOMMultimediaRecord mediaRec)
        {
            TGEDCOMMultimediaLink result = null;

            if (mediaRec != null)
            {
                result = new TGEDCOMMultimediaLink(this.Owner, this, "", "");
                result.Value = mediaRec;
                this.MultimediaLinks.Add(result);
            }

            return result;
        }

        #endregion
    }
}
