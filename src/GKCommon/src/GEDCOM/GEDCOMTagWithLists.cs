using System.IO;

namespace GKCommon.GEDCOM
{
	public class GEDCOMTagWithLists : GEDCOMTag, IGEDCOMStructWithLists
	{
		protected GEDCOMList<GEDCOMNotes> fNotes;
		protected GEDCOMList<GEDCOMSourceCitation> fSourceCitations;
		protected GEDCOMList<GEDCOMMultimediaLink> fMultimediaLinks;

		public GEDCOMList<GEDCOMNotes> Notes
		{
			get { return this.fNotes; }
		}

		public GEDCOMList<GEDCOMSourceCitation> SourceCitations
		{
			get { return this.fSourceCitations; }
		}

		public GEDCOMList<GEDCOMMultimediaLink> MultimediaLinks
		{
			get { return this.fMultimediaLinks; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);

			this.fNotes = new GEDCOMList<GEDCOMNotes>(this);
			this.fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
			this.fMultimediaLinks = new GEDCOMList<GEDCOMMultimediaLink>(this);
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

		public override void ResetOwner(GEDCOMTree newOwner)
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

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NOTE")
			{
				result = this.fNotes.Add(new GEDCOMNotes(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "SOUR")
			{
				result = this.fSourceCitations.Add(new GEDCOMSourceCitation(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "OBJE")
			{
				result = this.fMultimediaLinks.Add(new GEDCOMMultimediaLink(base.Owner, this, tagName, tagValue));
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

		protected GEDCOMTagWithLists(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        #region Auxiliary

        public GEDCOMNotes AddNote(GEDCOMNoteRecord noteRec)
        {
        	GEDCOMNotes note = null;
        	
            if (noteRec != null)
            {
                note = new GEDCOMNotes(this.Owner, this, "", "");
                note.Value = noteRec;
                this.Notes.Add(note);
            }
            
            return note;
        }

        public GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality)
        {
        	GEDCOMSourceCitation cit = null;
        	
            if (sourceRec != null)
            {
                cit = new GEDCOMSourceCitation(this.Owner, this, "", "");
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                this.SourceCitations.Add(cit);
            }
            
            return cit;
        }

        public GEDCOMMultimediaLink AddMultimedia(GEDCOMMultimediaRecord mediaRec)
        {
            GEDCOMMultimediaLink result = null;

            if (mediaRec != null)
            {
                result = new GEDCOMMultimediaLink(this.Owner, this, "", "");
                result.Value = mediaRec;
                this.MultimediaLinks.Add(result);
            }

            return result;
        }

        #endregion
    }
}
