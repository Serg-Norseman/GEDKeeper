using System;
using System.IO;

using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
    public abstract class TGEDCOMRecord : TGEDCOMCustomRecord, IGEDCOMStructWithLists
	{
		protected TGEDCOMRecordType fRecordType;

		private GEDCOMList<TGEDCOMMultimediaLink> fMultimediaLinks;
		private GEDCOMList<TGEDCOMNotes> fNotes;
		private GEDCOMList<TGEDCOMSourceCitation> fSourceCitations;
		private GEDCOMList<TGEDCOMUserReference> fUserReferences;

		public string AutomatedRecordID
		{
			get { return base.GetTagStringValue("RIN"); }
			set { base.SetTagStringValue("RIN", value); }
		}

		public TGEDCOMChangeDate ChangeDate
		{
			get { return base.TagClass("CHAN", TGEDCOMChangeDate.Create) as TGEDCOMChangeDate; }
		}

		public GEDCOMList<TGEDCOMMultimediaLink> MultimediaLinks
		{
			get	{ return this.fMultimediaLinks; }
		}

		public GEDCOMList<TGEDCOMNotes> Notes
		{
			get { return this.fNotes; }
		}

		public TGEDCOMRecordType RecordType
		{
			get { return this.fRecordType; }
		}

		public GEDCOMList<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this.fSourceCitations; }
		}

		public string UID
		{
			get { return base.GetTagStringValue("_UID"); }
			set { base.SetTagStringValue("_UID", value); }
		}

		public GEDCOMList<TGEDCOMUserReference> UserReferences
		{
			get { return this.fUserReferences; }
		}

		private static string CreateUID()
		{
			string result = "";
			byte checkA = 0;
			byte checkB = 0;
			byte[] binary = Guid.NewGuid().ToByteArray();

			int num = binary.Length - 1;
			for (int i = 0; i <= num; i++)
			{
				byte val = binary[i];
				checkA = unchecked((byte)((uint)checkA + (uint)val));
				checkB = unchecked((byte)((uint)checkB + (uint)checkA));
				result += string.Format("{0:X2}", val);
			}
			result += string.Format("{0:X2}", checkA);
			result += string.Format("{0:X2}", checkB);
			return result;
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtNone;

			this.fNotes = new GEDCOMList<TGEDCOMNotes>(this);
			this.fSourceCitations = new GEDCOMList<TGEDCOMSourceCitation>(this);
			this.fMultimediaLinks = new GEDCOMList<TGEDCOMMultimediaLink>(this);
			this.fUserReferences = new GEDCOMList<TGEDCOMUserReference>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fNotes.Dispose();
				this.fSourceCitations.Dispose();
				this.fMultimediaLinks.Dispose();
				this.fUserReferences.Dispose();
			}
			base.Dispose(disposing);
		}

		public int IndexOfSource(TGEDCOMSourceRecord aSource)
		{
			int result = -1;
            if (aSource == null) return result;

			int num = this.fSourceCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fSourceCitations[i].XRef == aSource.XRef)
				{
					result = i;
					break;
				}
			}

			return result;
		}

        public virtual void MoveTo(TGEDCOMRecord targetRecord, bool clearDest)
		{
            if (clearDest)
			{
				targetRecord.Clear();
			}

            if (this.fTags != null)
			{
				while (this.fTags.Count > 0)
				{
					TGEDCOMTag tag = this.fTags.Extract(0);
                    if (tag.Name == "CHAN" && !clearDest)
					{
						tag.Dispose();
					}
					else
					{
						tag.ResetParent(targetRecord);
						targetRecord.InsertTag(tag);
					}
				}
			}

			while (this.fNotes.Count > 0)
			{
				TGEDCOMTag tag = this.fNotes.Extract(0) as TGEDCOMTag;
				tag.ResetParent(targetRecord);
				targetRecord.Notes.Add(tag as TGEDCOMNotes);
			}

			while (this.fMultimediaLinks.Count > 0)
			{
				TGEDCOMTag tag = this.fMultimediaLinks.Extract(0) as TGEDCOMTag;
				tag.ResetParent(targetRecord);
				targetRecord.MultimediaLinks.Add(tag as TGEDCOMMultimediaLink);
			}

			while (this.fSourceCitations.Count > 0)
			{
				TGEDCOMTag tag = this.fSourceCitations.Extract(0) as TGEDCOMTag;
				tag.ResetParent(targetRecord);
				targetRecord.SourceCitations.Add(tag as TGEDCOMSourceCitation);
			}

			while (this.fUserReferences.Count > 0)
			{
				TGEDCOMTag tag = this.fUserReferences.Extract(0) as TGEDCOMTag;
				tag.ResetParent(targetRecord);
				targetRecord.UserReferences.Add(tag as TGEDCOMUserReference);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this.fNotes.Pack();
			this.fSourceCitations.Pack();
			this.fMultimediaLinks.Pack();
			this.fUserReferences.Pack();
		}

		public override void ReplaceXRefs(XRefReplacer map)
		{
			this.fNotes.ReplaceXRefs(map);
			this.fSourceCitations.ReplaceXRefs(map);
			this.fMultimediaLinks.ReplaceXRefs(map);
			this.fUserReferences.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);

			this.fNotes.ResetOwner(newOwner);
			this.fSourceCitations.ResetOwner(newOwner);
			this.fMultimediaLinks.ResetOwner(newOwner);
			this.fUserReferences.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);

			this.fNotes.SaveToStream(stream);
			this.fSourceCitations.SaveToStream(stream);
			this.fMultimediaLinks.SaveToStream(stream);
			this.fUserReferences.SaveToStream(stream);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "CHAN")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMChangeDate.Create);
			}
			else if (tagName == "NOTE")
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
			else if (tagName == "REFN")
			{
				result = this.fUserReferences.Add(new TGEDCOMUserReference(base.Owner, this, tagName, tagValue));
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
			this.fUserReferences.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fNotes.Count == 0 && this.fSourceCitations.Count == 0 && this.fMultimediaLinks.Count == 0 && this.fUserReferences.Count == 0;
		}

		public string NewXRef()
		{
			if (this.fOwner != null)
			{
				string new_xref = this.fOwner.XRefIndex_NewXRef(this);
				this.XRef = new_xref;
			}
			return this.FXRef;
		}

		public void NewUID()
		{
			this.UID = CreateUID();
		}

		public void InitNew()
		{
			this.NewXRef();
			this.NewUID();
		}

	    protected TGEDCOMRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		#region Auxiliary

		public string aux_GetXRefNum()
		{
			string xref = this.FXRef;

			int I = 0;
			int L = xref.Length - 1;
			while (I <= L && (xref[I] < '0' || xref[I] > '9')) I++;
			xref = ((I <= L) ? xref.Substring(I) : "");
			return xref;
		}

		public int aux_GetId()
		{
			int result;
			try
			{
				string xref = this.aux_GetXRefNum();
				result = SysUtils.ParseInt(xref, 0);
			}
			catch (Exception)
			{
				result = -1;
			}
			return result;
		}

		public TGEDCOMNotes aux_AddNote(TGEDCOMNoteRecord noteRec)
		{
			TGEDCOMNotes note = null;
			
			if (noteRec != null) {
				note = new TGEDCOMNotes(this.Owner, this, "", "");
				note.Value = noteRec;
				this.Notes.Add(note);
			}
			
			return note;
		}

		public TGEDCOMSourceCitation aux_AddSource(TGEDCOMSourceRecord sourceRec, string page, int quality)
		{
			TGEDCOMSourceCitation cit = null;
			
			if (sourceRec != null) {
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
			TGEDCOMMultimediaLink mmLink = null;

			if (mediaRec != null) {
				mmLink = new TGEDCOMMultimediaLink(this.Owner, this, "", "");
				mmLink.Value = mediaRec;
				this.MultimediaLinks.Add(mmLink);
			}

			return mmLink;
		}

		#endregion
	}
}
