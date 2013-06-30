using System;
using System.IO;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public abstract class TGEDCOMRecord : TGEDCOMCustomRecord
	{
		protected TGEDCOMRecordType FRecordType;

		private GEDCOMList<TGEDCOMMultimediaLink> _MultimediaLinks;
		private GEDCOMList<TGEDCOMNotes> _Notes;
		private GEDCOMList<TGEDCOMSourceCitation> _SourceCitations;
		private GEDCOMList<TGEDCOMUserReference> _UserReferences;

		public string AutomatedRecordID
		{
			get { return base.GetTagStringValue("RIN"); }
			set { base.SetTagStringValue("RIN", value); }
		}

		public TGEDCOMChangeDate ChangeDate
		{
			get { return base.TagClass("CHAN", typeof(TGEDCOMChangeDate), TGEDCOMChangeDate.Create) as TGEDCOMChangeDate; }
		}

		public GEDCOMList<TGEDCOMMultimediaLink> MultimediaLinks
		{
			get	{ return this._MultimediaLinks; }
		}

		public GEDCOMList<TGEDCOMNotes> Notes
		{
			get { return this._Notes; }
		}

		public TGEDCOMRecordType RecordType
		{
			get { return this.FRecordType; }
		}

		public GEDCOMList<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this._SourceCitations; }
		}

		public string UID
		{
			get { return base.GetTagStringValue("_UID"); }
			set { base.SetTagStringValue("_UID", value); }
		}

		public GEDCOMList<TGEDCOMUserReference> UserReferences
		{
			get { return this._UserReferences; }
		}

		private string CreateUID()
		{
			string Result = "";
			byte checkA = 0;
			byte checkB = 0;
			byte[] binary = Guid.NewGuid().ToByteArray();

			int num = binary.Length - 1;
			for (int i = 0; i <= num; i++)
			{
				byte val = binary[i];
				checkA = unchecked((byte)((uint)checkA + (uint)val));
				checkB = unchecked((byte)((uint)checkB + (uint)checkA));
				Result += string.Format("{0:X2}", val);
			}
			Result += string.Format("{0:X2}", checkA);
			Result += string.Format("{0:X2}", checkB);
			return Result;
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtNone;

			this._Notes = new GEDCOMList<TGEDCOMNotes>(this);
			this._SourceCitations = new GEDCOMList<TGEDCOMSourceCitation>(this);
			this._MultimediaLinks = new GEDCOMList<TGEDCOMMultimediaLink>(this);
			this._UserReferences = new GEDCOMList<TGEDCOMUserReference>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Notes.Dispose();
				this._SourceCitations.Dispose();
				this._MultimediaLinks.Dispose();
				this._UserReferences.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public int IndexOfSource(TGEDCOMSourceRecord aSource)
		{
			int Result = -1;
			int num = this._SourceCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._SourceCitations[i].XRef == aSource.XRef)
				{
					Result = i;
					break;
				}
			}

			return Result;
		}

		public virtual void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			if (aClearDest)
			{
				aToRecord.Clear();
			}
			if (this.FTags != null)
			{
				while (this.FTags.Count > 0)
				{
					TGEDCOMTag tag = this.FTags.Extract(0) as TGEDCOMTag;
					if (tag.Name == "CHAN" && !aClearDest)
					{
						tag.Dispose();
					}
					else
					{
						tag.ResetParent(aToRecord);
						aToRecord.InsertTag(tag);
					}
				}
			}

			while (this._Notes.Count > 0)
			{
				TGEDCOMTag tag = this._Notes.Extract(0) as TGEDCOMTag;
				tag.ResetParent(aToRecord);
				aToRecord.Notes.Add(tag as TGEDCOMNotes);
			}

			while (this._MultimediaLinks.Count > 0)
			{
				TGEDCOMTag tag = this._MultimediaLinks.Extract(0) as TGEDCOMTag;
				tag.ResetParent(aToRecord);
				aToRecord.MultimediaLinks.Add(tag as TGEDCOMMultimediaLink);
			}

			while (this._SourceCitations.Count > 0)
			{
				TGEDCOMTag tag = this._SourceCitations.Extract(0) as TGEDCOMTag;
				tag.ResetParent(aToRecord);
				aToRecord.SourceCitations.Add(tag as TGEDCOMSourceCitation);
			}

			while (this._UserReferences.Count > 0)
			{
				TGEDCOMTag tag = this._UserReferences.Extract(0) as TGEDCOMTag;
				tag.ResetParent(aToRecord);
				aToRecord.UserReferences.Add(tag as TGEDCOMUserReference);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this._Notes.Pack();
			this._SourceCitations.Pack();
			this._MultimediaLinks.Pack();
			this._UserReferences.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			this._Notes.ReplaceXRefs(aMap);
			this._SourceCitations.ReplaceXRefs(aMap);
			this._MultimediaLinks.ReplaceXRefs(aMap);
			this._UserReferences.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);

			this._Notes.ResetOwner(AOwner);
			this._SourceCitations.ResetOwner(AOwner);
			this._MultimediaLinks.ResetOwner(AOwner);
			this._UserReferences.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);

			this._Notes.SaveToStream(AStream);
			this._SourceCitations.SaveToStream(AStream);
			this._MultimediaLinks.SaveToStream(AStream);
			this._UserReferences.SaveToStream(AStream);
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "CHAN")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMChangeDate.Create);
			}
			else if (ATag == "NOTE")
			{
				Result = this._Notes.Add(new TGEDCOMNotes(base.Owner, this, ATag, AValue));
			}
			else if (ATag == "SOUR")
			{
				Result = this._SourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, ATag, AValue));
			}
			else if (ATag == "OBJE")
			{
				Result = this._MultimediaLinks.Add(new TGEDCOMMultimediaLink(base.Owner, this, ATag, AValue));
			}
			else if (ATag == "REFN")
			{
				Result = this._UserReferences.Add(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
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

			this._Notes.Clear();
			this._SourceCitations.Clear();
			this._MultimediaLinks.Clear();
			this._UserReferences.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Notes.Count == 0 && this._SourceCitations.Count == 0 && this._MultimediaLinks.Count == 0 && this._UserReferences.Count == 0;
		}

		public string NewXRef()
		{
			if (this.FOwner != null)
			{
				string new_xref = this.FOwner.XRefIndex_NewXRef(this);
				this.XRef = new_xref;
			}
			return this.FXRef;
		}

		public void NewUID()
		{
			this.UID = this.CreateUID();
		}

		public void InitNew()
		{
			this.NewXRef();
			this.NewUID();
		}

		public struct MatchParams
		{
			public bool IndistinctNameMatching;
			public double IndistinctThreshold;
			
			public bool RusNames;

			public bool CheckBirthYear;
			public int YearInaccuracy;
		}

		public virtual bool IsMatch(TGEDCOMRecord record, float matchThreshold, MatchParams matchParams)
		{
			return false;
		}

		public TGEDCOMRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

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

		public void aux_AddNote(TGEDCOMNoteRecord aNoteRec)
		{
			if (aNoteRec != null) {
				TGEDCOMNotes note = new TGEDCOMNotes(this.Owner, this, "", "");
				note.Value = aNoteRec;
				this.Notes.Add(note);
			}
		}

		public void aux_AddSource(TGEDCOMSourceRecord aSrcRec, string aPage, int aQuality)
		{
			if (aSrcRec != null) {
				TGEDCOMSourceCitation cit = new TGEDCOMSourceCitation(this.Owner, this, "", "");
				cit.Value = aSrcRec;
				cit.Page = aPage;
				cit.CertaintyAssessment = aQuality;
				this.SourceCitations.Add(cit);
			}
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

	}
}
