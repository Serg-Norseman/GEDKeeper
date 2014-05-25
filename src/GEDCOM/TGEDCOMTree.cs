using System;
using System.Collections;
using System.IO;
using System.Text;

using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public delegate void ProgressEventHandler(object sender, int progress);

	public interface ITreeEnumerator
	{
		bool MoveNext(out TGEDCOMRecord current);
		void Reset();
	}

	public sealed class TGEDCOMTree : GEDCOMObject
	{
		#region Tree Enumerator

		public struct TreeEnumerator : ITreeEnumerator
		{
			private readonly TGEDCOMTree tree;
            private readonly TGEDCOMRecordType rec_type;
            private readonly int endIndex;

            private int index;

			internal TreeEnumerator(TGEDCOMTree tree)
			{
				this.tree = tree;
				this.index = -1;
				this.endIndex = tree.RecordsCount - 1;
				this.rec_type = TGEDCOMRecordType.rtNone;
			}

			internal TreeEnumerator(TGEDCOMTree tree, TGEDCOMRecordType rec_type)
			{
				this.tree = tree;
				this.index = -1;
				this.endIndex = tree.RecordsCount - 1;
				this.rec_type = rec_type;
			}

			public bool MoveNext(out TGEDCOMRecord current)
			{
				if (this.rec_type == TGEDCOMRecordType.rtNone)
				{
					if (this.index < this.endIndex)
					{
						this.index++;
						current = this.tree[this.index];
						return true;
					}
				} else {
					while (this.index < this.endIndex)
					{
						this.index++;
						TGEDCOMRecord rec = this.tree[this.index];
						if (rec.RecordType == this.rec_type) {
							current = rec;
							return true;
						}
					}
				}

				this.index = this.endIndex + 1;
				current = null;
				return false;
			}

			public void Reset()
			{
				this.index = -1;
			}
		}

		#endregion

		private readonly TGEDCOMHeader fHeader;
        private readonly GEDCOMList<TGEDCOMRecord> fRecords;
        private readonly Hashtable fXRefIndex;

        private TGEDCOMState fState;
		private string fFileName;
		private ProgressEventHandler fOnProgressEvent;

		public string FileName
		{
			get { return this.fFileName; }
			//set { this.fFileName = value; }
		}

		public event ProgressEventHandler OnProgress
		{
			add {
				this.fOnProgressEvent = value;
			}
			remove {
				if (this.fOnProgressEvent == value) {
					this.fOnProgressEvent = null;
				}
			}
		}

		public ITreeEnumerator GetEnumerator(TGEDCOMRecordType recType)
		{
			return new TreeEnumerator(this, recType);
		}

		public int RecordsCount
		{
			get { return this.fRecords.Count; }
		}

		public TGEDCOMRecord this[int index]
		{
			get { return this.fRecords[index]; }
		}

		public TGEDCOMHeader Header
		{
			get { return this.fHeader; }
		}

		public TGEDCOMState State
		{
			get { return this.fState; }
			set { this.fState = value; }
		}

		public TGEDCOMTree()
		{
			this.fRecords = new GEDCOMList<TGEDCOMRecord>(this);
			this.fHeader = new TGEDCOMHeader(this, this, "", "");
			this.fXRefIndex = new Hashtable();
			this.fFileName = "";
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				//this.fXRefIndex.Dispose();
				this.fHeader.Dispose();
				this.fRecords.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Internal

		static TGEDCOMTree()
		{
			GEDCOMFactory f = GEDCOMFactory.GetInstance();

			f.RegisterTag("DATE", TGEDCOMDateValue.Create);
			f.RegisterTag("TIME", TGEDCOMTime.Create);
			f.RegisterTag("ADDR", TGEDCOMAddress.Create);
			f.RegisterTag("PLAC", TGEDCOMPlace.Create);
			f.RegisterTag("MAP", TGEDCOMMap.Create);
			f.RegisterTag("_LOC", TGEDCOMPointer.Create);

			//f.RegisterTag("xxxx", xxxx.Create);
		}

		private static string StrToUtf8(string S)
		{
			byte[] src = Encoding.GetEncoding(1251).GetBytes(S);
			return Encoding.UTF8.GetString(src);
		}

		private static string GetSignByRecord(TGEDCOMRecord record)
		{
			string result = "";

			if (record != null)
			{
				switch (record.RecordType) {
					case TGEDCOMRecordType.rtIndividual:
						result = "I";
						break;
					case TGEDCOMRecordType.rtFamily:
						result = "F";
						break;
					case TGEDCOMRecordType.rtNote:
						result = "N";
						break;
					case TGEDCOMRecordType.rtMultimedia:
						result = "O";
						break;
					case TGEDCOMRecordType.rtSource:
						result = "S";
						break;
					case TGEDCOMRecordType.rtRepository:
						result = "R";
						break;
					case TGEDCOMRecordType.rtGroup:
						result = "G";
						break;
					case TGEDCOMRecordType.rtResearch:
						result = "RS";
						break;
					case TGEDCOMRecordType.rtTask:
						result = "TK";
						break;
					case TGEDCOMRecordType.rtCommunication:
						result = "CM";
						break;
					case TGEDCOMRecordType.rtLocation:
						result = "L";
						break;
					case TGEDCOMRecordType.rtSubmission:
						result = "????";
						break;
					case TGEDCOMRecordType.rtSubmitter:
						result = "SUB";
						break;
				}
			}

			return result;
		}

		#endregion

		#region XRef Search

		private void XRefIndex_Clear()
		{
			this.fXRefIndex.Clear();
		}

		private void XRefIndex_AddRecord(TGEDCOMCustomRecord record)
		{
			if (record != null && !string.IsNullOrEmpty(record.XRef))
			{
				bool exists = this.fXRefIndex.ContainsKey(record.XRef);
				if (!exists) this.fXRefIndex.Add(record.XRef, record);
			}
		}

		public void SetXRef(string oldXRef, TGEDCOMCustomRecord sender)
		{
			if (!string.IsNullOrEmpty(oldXRef))
			{
				bool exists = this.fXRefIndex.ContainsKey(oldXRef);
				if (exists) this.fXRefIndex.Remove(oldXRef);
			}

			this.XRefIndex_AddRecord(sender);
		}

		private void XRefIndex_DeleteRecord(TGEDCOMRecord record)
		{
			bool exists = this.fXRefIndex.ContainsKey(record.XRef);
			if (exists) this.fXRefIndex.Remove(record.XRef);
		}

		public TGEDCOMRecord XRefIndex_Find(string XRef)
		{
			return (this.fXRefIndex[XRef] as TGEDCOMRecord);
		}

		public string XRefIndex_NewXRef(TGEDCOMRecord sender)
		{
			string sign = GetSignByRecord(sender);
			int I = 1;
			while (this.fXRefIndex.ContainsKey(sign + I.ToString()))
			{
				I++;
			}
			return sign + I.ToString();
		}

		#endregion

		#region Main functionality

		public TGEDCOMRecord AddRecord(TGEDCOMRecord record)
		{
			this.fRecords.Add(record);
			this.XRefIndex_AddRecord(record);
			return record;
		}

		public void Clear()
		{
			this.fRecords.Clear();
			this.fHeader.Clear();
			this.XRefIndex_Clear();
		}

		public void Delete(int index)
		{
			this.XRefIndex_DeleteRecord(this.fRecords[index]);
			this.fRecords.Delete(index);
		}

		public void DeleteRecord(TGEDCOMRecord sender)
		{
			this.XRefIndex_DeleteRecord(sender);
			this.fRecords.DeleteObject(sender);
		}

		public TGEDCOMRecord Extract(int index)
		{
			this.XRefIndex_DeleteRecord(this.fRecords[index]);
			return this.fRecords.Extract(index);
		}

		public int IndexOfRecord(TGEDCOMRecord record)
		{
			return this.fRecords.IndexOfObject(record);
		}

		public void Pack()
		{
			int num = this.fRecords.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				this.fRecords[i].Pack();
			}
		}

		public TGEDCOMRecord FindUID(string UID)
		{
			TGEDCOMRecord res = null;

			int num = this.fRecords.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.fRecords[i];
				if (rec.UID == UID)
				{
					res = rec;
					break;
				}
			}

			return res;
		}

		#endregion

		#region Load/Save

		public void SetFileName(string fileName)
		{
			this.fFileName = fileName;
		}

		public void LoadFromFile(string fileName)
		{
			this.fFileName = fileName;

			using (StreamReader fs = new StreamReader(fileName, Encoding.GetEncoding(1251))) {
				this.Clear();
				this.LoadFromStream(fs);
				this.fHeader.CharacterSet = TGEDCOMCharacterSet.csASCII;
			}
		}

		public void SaveToFile(string fileName, TGEDCOMCharacterSet charSet)
		{
			this.fFileName = fileName;

			string subm = this.fHeader.GetTagStringValue("SUBM");
			int rev = this.fHeader.FileRevision;

			this.fHeader.Clear();
			this.fHeader.Source = "GEDKeeper";
			this.fHeader.ReceivingSystemName = "GEDKeeper";
			this.fHeader.CharacterSet = charSet;
			this.fHeader.Language = "Russian";
			this.fHeader.GEDCOMVersion = "5.5";
			this.fHeader.GEDCOMForm = "LINEAGE-LINKED";
			this.fHeader.FileName = Path.GetFileName(fileName);
			this.fHeader.TransmissionDateTime = DateTime.Now;
			this.fHeader.FileRevision = rev + 1;

			if (subm != "")
			{
				this.fHeader.SetTagStringValue("SUBM", subm);
			}

			this.Pack();

			using (StreamWriter fs = new StreamWriter(fileName, false, GEDCOMUtils.GetEncodingByCharacterSet(charSet))) {
				this.SaveToStream(fs);
				this.fHeader.CharacterSet = TGEDCOMCharacterSet.csASCII;
			}
		}

        private static void CorrectLine(TGEDCOMCustomRecord curRecord, TGEDCOMTag curTag, int lineNum, string S)
		{
			try
			{
				if (curTag != null && curTag is TGEDCOMNotes) {
					curTag.AddTag("CONT", S, null);
				} else {
					if (curRecord != null) {
						curRecord.AddTag("NOTE", S, null);
					}
				}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("TGEDCOMTree.LoadFromStream().CorrectLine(): Line " + lineNum.ToString() + " failed correct: " + ex.Message);
			}
		}

		public void LoadFromStream(StreamReader stream)
		{
			long fileSize = stream.BaseStream.Length;
			long filePos = 0;
			int progress = 0;

			this.fState = TGEDCOMState.osLoading;
			try
			{
				TGEDCOMCustomRecord curRecord = null;
				TGEDCOMTag curTag = null;
				TGEDCOMCharacterSet charSet = TGEDCOMCharacterSet.csASCII;

				int I = -1;
				while (stream.Peek() != -1)
				{
					I++;
					string srcLine = stream.ReadLine();
					string S = SysUtils.TrimLeft(srcLine);

					if (S.Length != 0)
					{
						if (!GEDCOMUtils.IsDigit(S[0]))
						{
							TGEDCOMTree.CorrectLine(curRecord, curTag, I + 1, S.Trim());
						}
						else
						{
							int ALevel;
							string AXRef;
							string ATag;
							string AValue;
							try
							{
								S = GEDCOMUtils.ExtractNumber(S, out ALevel, false, 0);
								S = GEDCOMUtils.ExtractDelimiter(S, 0);
								S = GEDCOMUtils.ExtractXRef(S, out AXRef, true, "");
								S = GEDCOMUtils.ExtractDelimiter(S, 0);
								S = GEDCOMUtils.ExtractString(S, out ATag, "");
								ATag = ATag.ToUpperInvariant();
								S = GEDCOMUtils.ExtractDelimiter(S, 1);
								AValue = S;
							}
							catch (EGEDCOMException E)
							{
								throw new EGEDCOMException("Syntax error in line " + Convert.ToString(I + 1) + ".\r" + E.Message);
							}
							catch (Exception)
							{
								throw;
							}

							// temp hack
							if (AValue != null && AValue.Length != 0 && charSet == TGEDCOMCharacterSet.csUTF8)
							{
								if (stream.CurrentEncoding != Encoding.UTF8) {
									AValue = StrToUtf8(AValue);
								}
							}
							// end

							if (ALevel == 0)
							{
								if (ATag == "INDI")
								{
									curRecord = this.AddRecord(new TGEDCOMIndividualRecord(this, this, "", ""));
								}
								else if (ATag == "FAM")
								{
									curRecord = this.AddRecord(new TGEDCOMFamilyRecord(this, this, "", ""));
								}
								else if (ATag == "OBJE")
								{
									curRecord = this.AddRecord(new TGEDCOMMultimediaRecord(this, this, "", ""));
								}
								else if (ATag == "NOTE")
								{
									curRecord = this.AddRecord(new TGEDCOMNoteRecord(this, this, "", ""));
								}
								else if (ATag == "REPO")
								{
									curRecord = this.AddRecord(new TGEDCOMRepositoryRecord(this, this, "", ""));
								}
								else if (ATag == "SOUR")
								{
									curRecord = this.AddRecord(new TGEDCOMSourceRecord(this, this, "", ""));
								}
								else if (ATag == "SUBN")
								{
									curRecord = this.AddRecord(new TGEDCOMSubmissionRecord(this, this, "", ""));
								}
								else if (ATag == "SUBM")
								{
									curRecord = this.AddRecord(new TGEDCOMSubmitterRecord(this, this, "", ""));
								}
								else if (ATag == "_GROUP")
								{
									curRecord = this.AddRecord(new TGEDCOMGroupRecord(this, this, "", ""));
								}
								else if (ATag == "_RESEARCH")
								{
									curRecord = this.AddRecord(new TGEDCOMResearchRecord(this, this, "", ""));
								}
								else if (ATag == "_TASK")
								{
									curRecord = this.AddRecord(new TGEDCOMTaskRecord(this, this, "", ""));
								}
								else if (ATag == "_COMM")
								{
									curRecord = this.AddRecord(new TGEDCOMCommunicationRecord(this, this, "", ""));
								}
								else if (ATag == "_LOC")
								{
									curRecord = this.AddRecord(new TGEDCOMLocationRecord(this, this, "", ""));
								}
								else if (ATag == "HEAD")
								{
									curRecord = this.fHeader;
								}
								else if (ATag == "TRLR")
								{
									break;
								}
								else
								{
									curRecord = null;
								}

								if (curRecord != null && AXRef != "")
								{
									curRecord.XRef = AXRef;
								}
								curTag = null;
							}
							else
							{
								// temp hack
								if (ATag == "CHAR") {
									charSet = GEDCOMUtils.GetCharacterSetVal(AValue);
								}
								// end

								if (curRecord != null)
								{
									if (curTag == null || ALevel == 1)
									{
										curTag = curRecord.AddTag(ATag, AValue, null);
									}
									else
									{
										while (ALevel <= curTag.Level)
										{
											curTag = (curTag.Parent as TGEDCOMTag);
										}
										curTag = curTag.AddTag(ATag, AValue, null);
									}
								}
							}
						}
					}

					if (fOnProgressEvent != null) {
						filePos += Encoding.UTF8.GetByteCount(srcLine) + Environment.NewLine.Length;

						int newProgress = (int)Math.Min(100, (filePos * 100.0F) / fileSize);

						if (progress != newProgress) {
							fOnProgressEvent(this, progress);
							progress = newProgress;
						}
					}
				}
			}
			finally
			{
				this.fState = TGEDCOMState.osReady;
			}
		}

		public void SaveToStream(StreamWriter stream)
		{
			this.SaveHeaderToStream(stream);

			int num = this.fRecords.Count - 1;
			for (int I = 0; I <= num; I++)
			{
				this.fRecords[I].SaveToStream(stream);
			}

			this.SaveFooterToStream(stream);
		}

		public void SaveHeaderToStream(StreamWriter stream)
		{
			this.fHeader.SaveToStream(stream);
		}

		public void SaveFooterToStream(StreamWriter stream)
		{
		    const string S = "0 TRLR";
		    stream.WriteLine(S);
		}

	    #endregion

		#region Auxiliary

		public TGEDCOMSubmitterRecord aux_GetSubmitter()
		{
			TGEDCOMSubmitterRecord submitter = this.fHeader.Submitter.Value as TGEDCOMSubmitterRecord;
			if (submitter == null)
			{
				submitter = new TGEDCOMSubmitterRecord(this, this, "", "");
				submitter.InitNew();
				this.AddRecord(submitter);
				this.fHeader.SetTagStringValue("SUBM", "@" + submitter.XRef + "@");
			}
			return submitter;
		}

		public TGEDCOMIndividualRecord aux_CreateIndividual()
		{
			TGEDCOMIndividualRecord result = new TGEDCOMIndividualRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public TGEDCOMIndividualRecord aux_CreateIndividual(string iName, string iPatronymic, string iSurname, TGEDCOMSex iSex)
		{
			TGEDCOMIndividualRecord result = this.aux_CreateIndividual();

			result.Sex = iSex;
			TGEDCOMPersonalName pn = new TGEDCOMPersonalName(this, result, "", "");
			pn.StringValue = iName.Trim() + " " + iPatronymic.Trim() + " /" + iSurname.Trim() + "/";
			result.AddPersonalName(pn);

			return result;
		}

		public TGEDCOMFamilyRecord aux_CreateFamily()
		{
			TGEDCOMFamilyRecord result = new TGEDCOMFamilyRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public TGEDCOMNoteRecord aux_CreateNote()
		{
			TGEDCOMNoteRecord result = new TGEDCOMNoteRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public TGEDCOMNoteRecord aux_CreateNoteEx(TGEDCOMRecord toRecord, string text)
		{
			TGEDCOMNoteRecord result = null;

			if (toRecord != null && !string.IsNullOrEmpty(text)) {
				result = this.aux_CreateNote();
				result.aux_AddNoteText(text);
				toRecord.aux_AddNote(result);
			}

			return result;
		}

		public TGEDCOMNoteRecord aux_CreateNoteEx(TGEDCOMRecord toRecord, StringList text)
		{
			TGEDCOMNoteRecord result = null;

			if (text != null) {
				result = this.aux_CreateNote();
				result.Note = text;
			}

			if (toRecord != null && result != null) {
				toRecord.aux_AddNote(result);
			}
			
			return result;
		}

		public TGEDCOMSourceRecord aux_CreateSource()
		{
			TGEDCOMSourceRecord result = new TGEDCOMSourceRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public TGEDCOMGroupRecord aux_CreateGroup()
		{
			TGEDCOMGroupRecord result = new TGEDCOMGroupRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		//
		
		public void aux_CleanFamily(TGEDCOMFamilyRecord family)
		{
			if (family != null)
			{
				int num = family.Childrens.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMIndividualRecord child = family.Childrens[i].Value as TGEDCOMIndividualRecord;
					child.DeleteChildToFamilyLink(family);
				}

				TGEDCOMIndividualRecord spouse;

				spouse = family.Husband.Value as TGEDCOMIndividualRecord;
				family.aux_RemoveSpouse(spouse);

				spouse = (family.Wife.Value as TGEDCOMIndividualRecord);
				family.aux_RemoveSpouse(spouse);
			}
		}
		
		public bool aux_DeleteFamilyRecord(TGEDCOMFamilyRecord family)
		{
			bool result = false;
			if (family != null)
			{
				this.aux_CleanFamily(family);

				this.Delete(this.IndexOfRecord(family));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteGroupRecord(TGEDCOMGroupRecord groupRec)
		{
			bool result = false;
			if (groupRec != null)
			{
				for (int i = groupRec.Members.Count - 1; i >= 0; i--)
				{
					TGEDCOMIndividualRecord member = groupRec.Members[i].Value as TGEDCOMIndividualRecord;
					groupRec.aux_RemoveMember(member);
				}

				this.Delete(this.IndexOfRecord(groupRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteIndividualRecord(TGEDCOMIndividualRecord iRec)
		{
			bool result = false;
			if (iRec != null)
			{
				for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--)
				{
					TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[i].Family;
					family.DeleteChild(iRec);
				}

				for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
					family.aux_RemoveSpouse(iRec);
				}

				for (int i = iRec.Groups.Count - 1; i >= 0; i--)
				{
					TGEDCOMPointer ptr = iRec.Groups[i];
					TGEDCOMGroupRecord group = ptr.Value as TGEDCOMGroupRecord;
					group.aux_RemoveMember(iRec);
				}

				this.Delete(this.IndexOfRecord(iRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteMediaRecord(TGEDCOMMultimediaRecord mRec)
		{
			bool result = false;
			if (mRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--)
					{
						if (rec.MultimediaLinks[j].Value == mRec)
						{
							rec.MultimediaLinks.Delete(j);
						}
					}
				}

				this.Delete(this.IndexOfRecord(mRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteNoteRecord(TGEDCOMNoteRecord nRec)
		{
			bool result = false;
			if (nRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					for (int j = rec.Notes.Count - 1; j >= 0; j--)
					{
						if (rec.Notes[j].Value == nRec)
							rec.Notes.Delete(j);
					}
				}

				this.Delete(this.IndexOfRecord(nRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteRepositoryRecord(TGEDCOMRepositoryRecord repRec)
		{
			bool result = false;
			if (repRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					if (rec is TGEDCOMSourceRecord)
					{
						TGEDCOMSourceRecord srcRec = rec as TGEDCOMSourceRecord;
						for (int j = srcRec.RepositoryCitations.Count - 1; j >= 0; j--)
						{
							if (srcRec.RepositoryCitations[j].Value == repRec)
							{
								srcRec.RepositoryCitations.DeleteObject(srcRec.RepositoryCitations[j]);
							}
						}
					}
				}

				this.Delete(this.IndexOfRecord(repRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteResearchRecord(TGEDCOMResearchRecord resRec)
		{
			bool result = false;
			if (resRec != null)
			{
				this.Delete(this.IndexOfRecord(resRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteSourceRecord(TGEDCOMSourceRecord srcRec)
		{
			bool result = false;
			if (srcRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					for (int j = rec.SourceCitations.Count - 1; j >= 0; j--)
					{
						if (rec.SourceCitations[j].Value == srcRec)
						{
							rec.SourceCitations.Delete(j);
						}
					}
				}
				
				this.Delete(this.IndexOfRecord(srcRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteTaskRecord(TGEDCOMTaskRecord taskRec)
		{
			bool result = false;
			if (taskRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					if (rec is TGEDCOMResearchRecord)
					{
						TGEDCOMResearchRecord resRec = rec as TGEDCOMResearchRecord;
						for (int j = resRec.Tasks.Count - 1; j >= 0; j--)
						{
							if (resRec.Tasks[j].Value == taskRec)
							{
								resRec.Tasks.Delete(j);
							}
						}
					}
				}

				this.Delete(this.IndexOfRecord(taskRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteCommunicationRecord(TGEDCOMCommunicationRecord commRec)
		{
			bool result = false;
			if (commRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					if (rec is TGEDCOMResearchRecord)
					{
						TGEDCOMResearchRecord resRec = rec as TGEDCOMResearchRecord;
						for (int j = resRec.Communications.Count - 1; j >= 0; j--)
						{
							if (resRec.Communications[j].Value == commRec)
							{
								resRec.Communications.Delete(j);
							}
						}
					}
				}

				this.Delete(this.IndexOfRecord(commRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteLocationRecord(TGEDCOMLocationRecord locRec)
		{
			bool result = false;
			if (locRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
						for (int j = iRec.IndividualEvents.Count - 1; j >= 0; j--)
						{
							TGEDCOMCustomEvent ev = iRec.IndividualEvents[j];
							if (ev.Detail.Place.Location.Value == locRec)
							{
								ev.Detail.Place.DeleteTag("_LOC");
							}
						}
					}
					else
					{
						if (rec is TGEDCOMFamilyRecord)
						{
							TGEDCOMFamilyRecord fRec = rec as TGEDCOMFamilyRecord;
							for (int j = fRec.FamilyEvents.Count - 1; j >= 0; j--)
							{
								TGEDCOMCustomEvent ev = fRec.FamilyEvents[j];
								if (ev.Detail.Place.Location.Value == locRec)
								{
									ev.Detail.Place.DeleteTag("_LOC");
								}
							}
						}
					}
				}

				this.Delete(this.IndexOfRecord(locRec));
				result = true;
			}
			return result;
		}

		#endregion
	}
}
