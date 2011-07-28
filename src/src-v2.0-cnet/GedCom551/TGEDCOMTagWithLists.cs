using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMTagWithLists : TGEDCOMTag
	{
		internal TEnumSet FLists;
		internal TGEDCOMList FNotes;
		internal TGEDCOMList FSourceCitations;
		internal TGEDCOMList FMultimediaLinks;

		/*public TGEDCOMNotes Notes
		{
			get { return this.GetNotes(Index); }
		}
		public int NotesCount
		{
			get { return this.GetNotesCount(); }
		}*/

		/*public TGEDCOMSourceCitation SourceCitations
		{
			get { return this.GetSourceCitations(Index); }
		}
		public int SourceCitationsCount
		{
			get { return this.GetSourceCitationsCount(); }
		}*/

		/*public TGEDCOMMultimediaLink MultimediaLinks
		{
			get { return this.GetMultimediaLinks(Index); }
		}
		public int MultimediaLinksCount
		{
			get { return this.GetMultimediaLinksCount(); }
		}*/

		public TGEDCOMNotes GetNote(int Index)
		{
			TGEDCOMNotes Result;
			if (this.FNotes == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FNotes[Index] as TGEDCOMNotes);
			}
			return Result;
		}

		internal int GetNotesCount()
		{
			int Result;
			if (this.FNotes == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FNotes.Count;
			}
			return Result;
		}

		public TGEDCOMSourceCitation GetSourceCitation(int Index)
		{
			TGEDCOMSourceCitation Result;
			if (this.FSourceCitations == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FSourceCitations[Index] as TGEDCOMSourceCitation);
			}
			return Result;
		}

		internal int GetSourceCitationsCount()
		{
			int Result;
			if (this.FSourceCitations == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FSourceCitations.Count;
			}
			return Result;
		}

		internal TGEDCOMMultimediaLink GetMultimediaLink(int Index)
		{
			TGEDCOMMultimediaLink Result;
			if (this.FMultimediaLinks == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FMultimediaLinks[Index] as TGEDCOMMultimediaLink);
			}
			return Result;
		}

		internal int GetMultimediaLinksCount()
		{
			int Result;
			if (this.FMultimediaLinks == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FMultimediaLinks.Count;
			}
			return Result;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FLists = TEnumSet.Create();
			this.FNotes = null;
			this.FSourceCitations = null;
			this.FMultimediaLinks = null;
		}

		protected internal void SetLists(TEnumSet ALists)
		{
			this.FLists = ALists;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FNotes != null)
				{
					this.FNotes.Free();
				}
				if (this.FSourceCitations != null)
				{
					this.FSourceCitations.Free();
				}
				if (this.FMultimediaLinks != null)
				{
					this.FMultimediaLinks.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMNotes AddNotes(TGEDCOMNotes ANotes)
		{
			if (this.FNotes == null)
			{
				this.FNotes = new TGEDCOMList(this);
			}
			if (ANotes != null)
			{
				this.FNotes.Add(ANotes);
			}
			return ANotes;
		}
		public TGEDCOMSourceCitation AddSourceCitation(TGEDCOMSourceCitation ASourceCitation)
		{
			if (this.FSourceCitations == null)
			{
				this.FSourceCitations = new TGEDCOMList(this);
			}
			if (ASourceCitation != null)
			{
				this.FSourceCitations.Add(ASourceCitation);
			}
			return ASourceCitation;
		}
		public TGEDCOMMultimediaLink AddMultimediaLink(TGEDCOMMultimediaLink AMultimediaLink)
		{
			if (this.FMultimediaLinks == null)
			{
				this.FMultimediaLinks = new TGEDCOMList(this);
			}
			if (AMultimediaLink != null)
			{
				this.FMultimediaLinks.Add(AMultimediaLink);
			}
			return AMultimediaLink;
		}
		public void DeleteNotes(int aIndex)
		{
			if (this.FNotes != null)
			{
				this.FNotes.Delete(aIndex);
			}
		}
		public void DeleteNotes(TGEDCOMNotes ANotes)
		{
			if (this.FNotes != null)
			{
				this.FNotes.DeleteObject(ANotes);
			}
		}
		public void DeleteSourceCitation(int aIndex)
		{
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.Delete(aIndex);
			}
		}
		public void DeleteSourceCitation(TGEDCOMSourceCitation ASourceCitation)
		{
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.DeleteObject(ASourceCitation);
			}
		}
		public void DeleteMultimediaLink(int aIndex)
		{
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.Delete(aIndex);
			}
		}
		public void DeleteMultimediaLink(TGEDCOMMultimediaLink AMultimediaLink)
		{
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.DeleteObject(AMultimediaLink);
			}
		}
		public override void Pack()
		{
			base.Pack();
			if (this.FNotes != null)
			{
				this.FNotes.Pack();
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.Pack();
			}
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.Pack();
			}
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FNotes != null)
			{
				this.FNotes.ReplaceXRefs(aMap);
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.ReplaceXRefs(aMap);
			}
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FNotes != null)
			{
				this.FNotes.ResetOwner(AOwner);
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.ResetOwner(AOwner);
			}
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.ResetOwner(AOwner);
			}
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FNotes != null)
			{
				this.FNotes.SaveToStream(AStream);
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.SaveToStream(AStream);
			}
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.SaveToStream(AStream);
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "NOTE") == 0 && this.FLists.InSet(TGEDCOMObject.TGEDCOMSubList.stNotes))
			{
				Result = this.AddNotes(new TGEDCOMNotes(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "SOUR") == 0 && this.FLists.InSet(TGEDCOMObject.TGEDCOMSubList.stSource))
				{
					Result = this.AddSourceCitation(new TGEDCOMSourceCitation(base.Owner, this, ATag, AValue));
				}
				else
				{
					if (BDSSystem.WStrCmp(ATag, "OBJE") == 0 && this.FLists.InSet(TGEDCOMObject.TGEDCOMSubList.stMultimedia))
					{
						Result = this.AddMultimediaLink(new TGEDCOMMultimediaLink(base.Owner, this, ATag, AValue));
					}
					else
					{
						Result = base.AddTag(ATag, AValue, AClass);
					}
				}
			}
			return Result;
		}
		public override void Clear()
		{
			base.Clear();
			if (this.FNotes != null)
			{
				this.FNotes.Clear();
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.Clear();
			}
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetNotesCount() == 0 && this.GetSourceCitationsCount() == 0 && this.GetMultimediaLinksCount() == 0;
		}

		public TGEDCOMTagWithLists(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
