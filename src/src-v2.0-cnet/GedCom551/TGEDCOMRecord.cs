using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMRecord : TGEDCOMCustomRecord
	{
		public enum TGEDCOMRecordType : byte
		{
			rtNone,
			rtIndividual,
			rtFamily,
			rtNote,
			rtMultimedia,
			rtSource,
			rtRepository,
			rtGroup,
			rtResearch,
			rtTask,
			rtCommunication,
			rtLocation,
			rtSubmission,
			rtSubmitter,
			
			rtLast
		}

		internal TEnumSet FLists;
		internal TGEDCOMList FMultimediaLinks;
		internal TGEDCOMList FNotes;
		internal TGEDCOMRecord.TGEDCOMRecordType FRecordType;
		internal TGEDCOMList FSourceCitations;
		internal TGEDCOMList FUserReferences;

		public TGEDCOMChangeDate ChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		/*public TGEDCOMMultimediaLink MultimediaLinks[int mmIndex]
		{
			get	{ return this.GetMultimediaLinks(Index); }
		}
		public int MultimediaLinksCount
		{
			get	{ return this.GetMultimediaLinksCount(); }
		}*/

		/*public TGEDCOMNotes Notes[int nIndex]
		{
			get { return this.GetNotes(Index); }
		}
		public int NotesCount
		{
			get { return this.GetNotesCount(); }
		}*/

		public TGEDCOMRecord.TGEDCOMRecordType RecordType
		{
			get { return this.FRecordType; }
		}

		/*public TGEDCOMSourceCitation SourceCitations
		{
			get { return this.GetSourceCitations(Index); }
		}
		public int SourceCitationsCount
		{
			get { return this.GetSourceCitationsCount(); }
		}*/

		public string UID
		{
			get { return this.GetUID(); }
			set { this.SetUID(value); }
		}

		/*public TGEDCOMUserReference UserReferences
		{
			get { return this.GetUserReferences(Index); }
		}
		public int UserReferencesCount
		{
			get { return this.GetUserReferencesCount(); }
		}*/

		public new string XRef
		{
			get { return base.XRef; }
			set { base.XRef = value; }
		}

		internal string CreateUID()
		{
			string Result = "";
			byte checkA = 0;
			byte checkB = 0;
			byte[] binary = Guid.NewGuid().ToByteArray();
			int arg_2A_0 = 0;
			int num = ((binary != null) ? binary.Length : 0) - 1;
			int i = arg_2A_0;
			if (num >= i)
			{
				num++;
				do
				{
					byte val = binary[i];
					checkA = (byte)((uint)checkA + (uint)val);
					checkB = (byte)((uint)checkB + (uint)checkA);
					Result += string.Format("{0:X2}", new object[]
					{
						val
					});
					i++;
				}
				while (i != num);
			}
			Result += string.Format("{0:X2}", new object[]
			{
				checkA
			});
			Result += string.Format("{0:X2}", new object[]
			{
				checkB
			});
			return Result;
		}

		internal TGEDCOMChangeDate GetChangeDate()
		{
			return base.TagClass("CHAN", typeof(TGEDCOMChangeDate)) as TGEDCOMChangeDate;
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

		internal TGEDCOMNotes GetNote(int Index)
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

		internal TGEDCOMSourceCitation GetSourceCitation(int Index)
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

		internal TGEDCOMUserReference GetUserReference(int Index)
		{
			TGEDCOMUserReference Result;
			if (this.FUserReferences == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FUserReferences[Index] as TGEDCOMUserReference);
			}
			return Result;
		}

		internal int GetUserReferencesCount()
		{
			int Result;
			if (this.FUserReferences == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FUserReferences.Count;
			}
			return Result;
		}

		internal string GetUID()
		{
			return base.GetTagStringValue("_UID");
		}

		internal void SetUID([In] string Value)
		{
			base.SetTagStringValue("_UID", Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtNone;
			this.FLists = TEnumSet.Create();
			this.FNotes = null;
			this.FSourceCitations = null;
			this.FMultimediaLinks = null;
			this.FUserReferences = null;
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
				if (this.FUserReferences != null)
				{
					this.FUserReferences.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
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

		public TGEDCOMUserReference AddUserReference(TGEDCOMUserReference AUserReference)
		{
			if (this.FUserReferences == null)
			{
				this.FUserReferences = new TGEDCOMList(this);
			}
			if (this.FUserReferences != null)
			{
				this.FUserReferences.Add(AUserReference);
			}
			return AUserReference;
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

		public void DeleteUserReference(int aIndex)
		{
			if (this.FUserReferences != null)
			{
				this.FUserReferences.Delete(aIndex);
			}
		}

		public void DeleteUserReference(TGEDCOMUserReference AUserReference)
		{
			if (this.FUserReferences != null)
			{
				this.FUserReferences.DeleteObject(AUserReference);
			}
		}

		public int IndexOfSource(TGEDCOMSourceRecord aSource)
		{
			int Result = -1;
			if (this.FSourceCitations != null)
			{
				int arg_19_0 = 0;
				int num = this.FSourceCitations.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (BDSSystem.WStrCmp((this.FSourceCitations[i] as TGEDCOMPointer).XRef, aSource.XRef) != 0)
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}

		public int IndexOfMultimediaLink(TGEDCOMMultimediaLink aMultimediaLink)
		{
			int Result = -1;
			if (this.FMultimediaLinks != null)
			{
				int arg_19_0 = 0;
				int num = this.FMultimediaLinks.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (!object.Equals(this.FMultimediaLinks[i], aMultimediaLink))
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}

		public int IndexOfSourceCitation(TGEDCOMSourceCitation SourceCit)
		{
			int Result = -1;
			if (this.FSourceCitations != null)
			{
				int arg_19_0 = 0;
				int num = this.FSourceCitations.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (!object.Equals(this.FSourceCitations[i], SourceCit))
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}

		public void ExchangeSources(int Index1, int Index2)
		{
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.Exchange(Index1, Index2);
			}
		}

		public void ExchangeMedia(int Index1, int Index2)
		{
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.Exchange(Index1, Index2);
			}
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
					if (BDSSystem.WStrCmp(tag.Name, "CHAN") == 0 && !aClearDest)
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
			if (this.FNotes != null)
			{
				while (this.FNotes.Count > 0)
				{
					TGEDCOMTag tag = this.FNotes.Extract(0) as TGEDCOMTag;
					tag.ResetParent(aToRecord);
					aToRecord.AddNotes(tag as TGEDCOMNotes);
				}
			}
			if (this.FMultimediaLinks != null)
			{
				while (this.FMultimediaLinks.Count > 0)
				{
					TGEDCOMTag tag = this.FMultimediaLinks.Extract(0) as TGEDCOMTag;
					tag.ResetParent(aToRecord);
					aToRecord.AddMultimediaLink(tag as TGEDCOMMultimediaLink);
				}
			}
			if (this.FSourceCitations != null)
			{
				while (this.FSourceCitations.Count > 0)
				{
					TGEDCOMTag tag = this.FSourceCitations.Extract(0) as TGEDCOMTag;
					tag.ResetParent(aToRecord);
					aToRecord.AddSourceCitation(tag as TGEDCOMSourceCitation);
				}
			}
			if (this.FUserReferences != null)
			{
				while (this.FUserReferences.Count > 0)
				{
					TGEDCOMTag tag = this.FUserReferences.Extract(0) as TGEDCOMTag;
					tag.ResetParent(aToRecord);
					aToRecord.AddUserReference(tag as TGEDCOMUserReference);
				}
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
			if (this.FUserReferences != null)
			{
				this.FUserReferences.Pack();
			}
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
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
			if (this.FUserReferences != null)
			{
				this.FUserReferences.ReplaceXRefs(aMap);
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
			if (this.FUserReferences != null)
			{
				this.FUserReferences.ResetOwner(AOwner);
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
			if (this.FUserReferences != null)
			{
				this.FUserReferences.SaveToStream(AStream);
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "CHAN") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMChangeDate));
			}
			else
			{
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
			if (this.FUserReferences != null)
			{
				this.FUserReferences.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetNotesCount() == 0 && this.GetSourceCitationsCount() == 0 && this.GetMultimediaLinksCount() == 0 && this.GetUserReferencesCount() == 0;
		}

		public string NewXRef()
		{
			if (this.FOwner != null)
			{
				this.FXRef = (this.FOwner as TGEDCOMTree).XRefIndex_NewXRef(this);
				this.XRef = this.FXRef;
			}
			return this.FXRef;
		}

		public void NewUID()
		{
			this.SetUID(this.CreateUID());
		}

		public void InitNew()
		{
			this.NewXRef();
			this.NewUID();
		}

		public TGEDCOMRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
