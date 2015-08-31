using System;
using System.Collections;
using System.IO;
using System.Text;
using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public delegate void ProgressEventHandler(object sender, int progress);

	public interface ITreeEnumerator
	{
		bool MoveNext(out GEDCOMRecord current);
		void Reset();
	}

    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMTree : GEDCOMObject
	{
		#region Tree Enumerator

		private struct TreeEnumerator : ITreeEnumerator
		{
			private readonly GEDCOMTree tree;
            private readonly GEDCOMRecordType rec_type;
            private readonly int endIndex;

            private int index;

			internal TreeEnumerator(GEDCOMTree tree)
			{
				this.tree = tree;
				this.index = -1;
				this.endIndex = tree.RecordsCount - 1;
				this.rec_type = GEDCOMRecordType.rtNone;
			}

			internal TreeEnumerator(GEDCOMTree tree, GEDCOMRecordType rec_type)
			{
				this.tree = tree;
				this.index = -1;
				this.endIndex = tree.RecordsCount - 1;
				this.rec_type = rec_type;
			}

			public bool MoveNext(out GEDCOMRecord current)
			{
				if (this.rec_type == GEDCOMRecordType.rtNone)
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
						GEDCOMRecord rec = this.tree[this.index];
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

		private readonly GEDCOMHeader fHeader;
        private readonly GEDCOMList<GEDCOMRecord> fRecords;
        private readonly Hashtable fXRefIndex;

        private GEDCOMState fState;
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

		public ITreeEnumerator GetEnumerator(GEDCOMRecordType recType)
		{
			return new TreeEnumerator(this, recType);
		}

		public int RecordsCount
		{
			get { return this.fRecords.Count; }
		}

		public GEDCOMRecord this[int index]
		{
			get { return this.fRecords[index]; }
		}

		public GEDCOMHeader Header
		{
			get { return this.fHeader; }
		}

		public GEDCOMState State
		{
			get { return this.fState; }
			set { this.fState = value; }
		}

		public GEDCOMTree()
		{
			this.fRecords = new GEDCOMList<GEDCOMRecord>(this);
			this.fHeader = new GEDCOMHeader(this, this, "", "");
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

		static GEDCOMTree()
		{
			GEDCOMFactory f = GEDCOMFactory.GetInstance();

			f.RegisterTag("DATE", GEDCOMDateValue.Create);
			f.RegisterTag("TIME", GEDCOMTime.Create);
			f.RegisterTag("ADDR", GEDCOMAddress.Create);
			f.RegisterTag("PLAC", GEDCOMPlace.Create);
			f.RegisterTag("MAP", GEDCOMMap.Create);
			f.RegisterTag("_LOC", GEDCOMPointer.Create);

			//f.RegisterTag("xxxx", xxxx.Create);
		}

		private static string StrToUtf8(string S)
		{
			byte[] src = Encoding.GetEncoding(1251).GetBytes(S);
			return Encoding.UTF8.GetString(src);
		}

		private static string GetSignByRecord(GEDCOMRecord record)
		{
			string result = "";

			if (record != null)
			{
				switch (record.RecordType) {
					case GEDCOMRecordType.rtIndividual:
						result = "I";
						break;
					case GEDCOMRecordType.rtFamily:
						result = "F";
						break;
					case GEDCOMRecordType.rtNote:
						result = "N";
						break;
					case GEDCOMRecordType.rtMultimedia:
						result = "O";
						break;
					case GEDCOMRecordType.rtSource:
						result = "S";
						break;
					case GEDCOMRecordType.rtRepository:
						result = "R";
						break;
					case GEDCOMRecordType.rtGroup:
						result = "G";
						break;
					case GEDCOMRecordType.rtResearch:
						result = "RS";
						break;
					case GEDCOMRecordType.rtTask:
						result = "TK";
						break;
					case GEDCOMRecordType.rtCommunication:
						result = "CM";
						break;
					case GEDCOMRecordType.rtLocation:
						result = "L";
						break;
					case GEDCOMRecordType.rtSubmission:
						result = "????";
						break;
					case GEDCOMRecordType.rtSubmitter:
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

		private void XRefIndex_AddRecord(GEDCOMCustomRecord record)
		{
			if (record != null && !string.IsNullOrEmpty(record.XRef))
			{
				bool exists = this.fXRefIndex.ContainsKey(record.XRef);
				if (!exists) this.fXRefIndex.Add(record.XRef, record);
			}
		}

		public void SetXRef(string oldXRef, GEDCOMCustomRecord sender)
		{
			if (!string.IsNullOrEmpty(oldXRef))
			{
				bool exists = this.fXRefIndex.ContainsKey(oldXRef);
				if (exists) this.fXRefIndex.Remove(oldXRef);
			}

			this.XRefIndex_AddRecord(sender);
		}

		private void XRefIndex_DeleteRecord(GEDCOMRecord record)
		{
			bool exists = this.fXRefIndex.ContainsKey(record.XRef);
			if (exists) this.fXRefIndex.Remove(record.XRef);
		}

		public GEDCOMRecord XRefIndex_Find(string XRef)
		{
			return (this.fXRefIndex[XRef] as GEDCOMRecord);
		}

		public string XRefIndex_NewXRef(GEDCOMRecord sender)
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

		public GEDCOMRecord AddRecord(GEDCOMRecord record)
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

		public void DeleteRecord(GEDCOMRecord sender)
		{
			this.XRefIndex_DeleteRecord(sender);
			this.fRecords.DeleteObject(sender);
		}

		public GEDCOMRecord Extract(int index)
		{
			this.XRefIndex_DeleteRecord(this.fRecords[index]);
			return this.fRecords.Extract(index);
		}

		public int IndexOfRecord(GEDCOMRecord record)
		{
			return this.fRecords.IndexOfObject(record);
		}

		public void Pack()
		{
			int num = this.fRecords.Count;
			for (int i = 0; i < num; i++)
			{
				this.fRecords[i].Pack();
			}
		}

		public GEDCOMRecord FindUID(string UID)
		{
			int num = this.fRecords.Count;
			for (int i = 0; i < num; i++)
			{
				GEDCOMRecord rec = this.fRecords[i];
				if (rec.UID == UID) {
					return rec;
				}
			}

			return null;
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
				this.fHeader.CharacterSet = GEDCOMCharacterSet.csASCII;
			}
		}

		public void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
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
				this.fHeader.CharacterSet = GEDCOMCharacterSet.csASCII;
			}
		}

        private static void CorrectLine(GEDCOMCustomRecord curRecord, GEDCOMTag curTag, int lineNum, string S)
		{
			try
			{
				if (curTag != null && curTag is GEDCOMNotes) {
					curTag.AddTag("CONT", S, null);
				} else {
					if (curRecord != null) {
						curRecord.AddTag("NOTE", S, null);
					}
				}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("GEDCOMTree.LoadFromStream().CorrectLine(): Line " + lineNum.ToString() + " failed correct: " + ex.Message);
			}
		}

		public void LoadFromStream(StreamReader stream)
		{
			long fileSize = stream.BaseStream.Length;
			long filePos = 0;
			int progress = 0;

			this.fState = GEDCOMState.osLoading;
			try
			{
				GEDCOMCustomRecord curRecord = null;
				GEDCOMTag curTag = null;
				GEDCOMCharacterSet charSet = GEDCOMCharacterSet.csASCII;

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
							GEDCOMTree.CorrectLine(curRecord, curTag, I + 1, S.Trim());
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
							if (AValue != null && AValue.Length != 0 && charSet == GEDCOMCharacterSet.csUTF8)
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
									curRecord = this.AddRecord(new GEDCOMIndividualRecord(this, this, "", ""));
								}
								else if (ATag == "FAM")
								{
									curRecord = this.AddRecord(new GEDCOMFamilyRecord(this, this, "", ""));
								}
								else if (ATag == "OBJE")
								{
									curRecord = this.AddRecord(new GEDCOMMultimediaRecord(this, this, "", ""));
								}
								else if (ATag == "NOTE")
								{
									curRecord = this.AddRecord(new GEDCOMNoteRecord(this, this, "", ""));
								}
								else if (ATag == "REPO")
								{
									curRecord = this.AddRecord(new GEDCOMRepositoryRecord(this, this, "", ""));
								}
								else if (ATag == "SOUR")
								{
									curRecord = this.AddRecord(new GEDCOMSourceRecord(this, this, "", ""));
								}
								else if (ATag == "SUBN")
								{
									curRecord = this.AddRecord(new GEDCOMSubmissionRecord(this, this, "", ""));
								}
								else if (ATag == "SUBM")
								{
									curRecord = this.AddRecord(new GEDCOMSubmitterRecord(this, this, "", ""));
								}
								else if (ATag == "_GROUP")
								{
									curRecord = this.AddRecord(new GEDCOMGroupRecord(this, this, "", ""));
								}
								else if (ATag == "_RESEARCH")
								{
									curRecord = this.AddRecord(new GEDCOMResearchRecord(this, this, "", ""));
								}
								else if (ATag == "_TASK")
								{
									curRecord = this.AddRecord(new GEDCOMTaskRecord(this, this, "", ""));
								}
								else if (ATag == "_COMM")
								{
									curRecord = this.AddRecord(new GEDCOMCommunicationRecord(this, this, "", ""));
								}
								else if (ATag == "_LOC")
								{
									curRecord = this.AddRecord(new GEDCOMLocationRecord(this, this, "", ""));
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
											curTag = (curTag.Parent as GEDCOMTag);
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
				this.fState = GEDCOMState.osReady;
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

		public GEDCOMSubmitterRecord aux_GetSubmitter()
		{
			GEDCOMSubmitterRecord submitter = this.fHeader.Submitter.Value as GEDCOMSubmitterRecord;
			if (submitter == null)
			{
				submitter = new GEDCOMSubmitterRecord(this, this, "", "");
				submitter.InitNew();
				this.AddRecord(submitter);
				this.fHeader.SetTagStringValue("SUBM", "@" + submitter.XRef + "@");
			}
			return submitter;
		}

		public GEDCOMIndividualRecord aux_CreateIndividual()
		{
			GEDCOMIndividualRecord result = new GEDCOMIndividualRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public GEDCOMIndividualRecord aux_CreateIndividual(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex)
		{
			GEDCOMIndividualRecord result = this.aux_CreateIndividual();

			result.Sex = iSex;
			GEDCOMPersonalName pn = new GEDCOMPersonalName(this, result, "", "");
			pn.StringValue = iName.Trim() + " " + iPatronymic.Trim() + " /" + iSurname.Trim() + "/";
			result.AddPersonalName(pn);

			return result;
		}

		public GEDCOMFamilyRecord aux_CreateFamily()
		{
			GEDCOMFamilyRecord result = new GEDCOMFamilyRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public GEDCOMNoteRecord aux_CreateNote()
		{
			GEDCOMNoteRecord result = new GEDCOMNoteRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public GEDCOMNoteRecord aux_CreateNoteEx(GEDCOMRecord toRecord, string text)
		{
			GEDCOMNoteRecord result = null;

			if (toRecord != null && !string.IsNullOrEmpty(text)) {
				result = this.aux_CreateNote();
				result.aux_AddNoteText(text);
				toRecord.AddNote(result);
			}

			return result;
		}

		public GEDCOMNoteRecord aux_CreateNoteEx(GEDCOMRecord toRecord, StringList text)
		{
			GEDCOMNoteRecord result = null;

			if (text != null) {
				result = this.aux_CreateNote();
				result.Note = text;
			}

			if (toRecord != null && result != null) {
				toRecord.AddNote(result);
			}
			
			return result;
		}

		public GEDCOMSourceRecord aux_CreateSource()
		{
			GEDCOMSourceRecord result = new GEDCOMSourceRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		public GEDCOMGroupRecord aux_CreateGroup()
		{
			GEDCOMGroupRecord result = new GEDCOMGroupRecord(this, this, "", "");
			result.InitNew();
			result.ChangeDate.ChangeDateTime = DateTime.Now;

			this.AddRecord(result);

			return result;
		}

		//
		
		public void aux_CleanFamily(GEDCOMFamilyRecord family)
		{
			if (family != null)
			{
				int num = family.Childrens.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMIndividualRecord child = family.Childrens[i].Value as GEDCOMIndividualRecord;
					child.DeleteChildToFamilyLink(family);
				}

				GEDCOMIndividualRecord spouse;

				spouse = family.Husband.Value as GEDCOMIndividualRecord;
				family.aux_RemoveSpouse(spouse);

				spouse = (family.Wife.Value as GEDCOMIndividualRecord);
				family.aux_RemoveSpouse(spouse);
			}
		}
		
		public bool aux_DeleteFamilyRecord(GEDCOMFamilyRecord family)
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

		public bool aux_DeleteGroupRecord(GEDCOMGroupRecord groupRec)
		{
			bool result = false;
			if (groupRec != null)
			{
				for (int i = groupRec.Members.Count - 1; i >= 0; i--)
				{
					GEDCOMIndividualRecord member = groupRec.Members[i].Value as GEDCOMIndividualRecord;
					groupRec.aux_RemoveMember(member);
				}

				this.Delete(this.IndexOfRecord(groupRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteIndividualRecord(GEDCOMIndividualRecord iRec)
		{
			bool result = false;
			if (iRec != null)
			{
				for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--)
				{
					GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[i].Family;
					family.DeleteChild(iRec);
				}

				for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
				{
					GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
					family.aux_RemoveSpouse(iRec);
				}

				for (int i = iRec.Groups.Count - 1; i >= 0; i--)
				{
					GEDCOMPointer ptr = iRec.Groups[i];
					GEDCOMGroupRecord group = ptr.Value as GEDCOMGroupRecord;
					group.aux_RemoveMember(iRec);
				}

				this.Delete(this.IndexOfRecord(iRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteMediaRecord(GEDCOMMultimediaRecord mRec)
		{
			bool result = false;
			if (mRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = this[i];
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

		public bool aux_DeleteNoteRecord(GEDCOMNoteRecord nRec)
		{
			bool result = false;
			if (nRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = this[i];
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

		public bool aux_DeleteRepositoryRecord(GEDCOMRepositoryRecord repRec)
		{
			bool result = false;
			if (repRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = this[i];
					if (rec is GEDCOMSourceRecord)
					{
						GEDCOMSourceRecord srcRec = rec as GEDCOMSourceRecord;
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

		public bool aux_DeleteResearchRecord(GEDCOMResearchRecord resRec)
		{
			bool result = false;
			if (resRec != null)
			{
				this.Delete(this.IndexOfRecord(resRec));
				result = true;
			}
			return result;
		}

		public bool aux_DeleteSourceRecord(GEDCOMSourceRecord srcRec)
		{
			bool result = false;
			if (srcRec != null)
			{
				int num = this.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = this[i];
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

		public bool aux_DeleteTaskRecord(GEDCOMTaskRecord taskRec)
		{
			bool result = false;
			if (taskRec != null)
			{
				int num = this.RecordsCount;
				for (int i = 0; i < num; i++)
				{
					GEDCOMRecord rec = this[i];
					if (rec is GEDCOMResearchRecord)
					{
						GEDCOMResearchRecord resRec = rec as GEDCOMResearchRecord;
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

		public bool aux_DeleteCommunicationRecord(GEDCOMCommunicationRecord commRec)
		{
			bool result = false;
			if (commRec != null)
			{
				int num = this.RecordsCount;
				for (int i = 0; i < num; i++)
				{
					GEDCOMRecord rec = this[i];
					if (rec is GEDCOMResearchRecord)
					{
						GEDCOMResearchRecord resRec = rec as GEDCOMResearchRecord;
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

		public bool aux_DeleteLocationRecord(GEDCOMLocationRecord locRec)
		{
			bool result = false;
			if (locRec != null)
			{
				int num = this.RecordsCount;
				for (int i = 0; i < num; i++)
				{
					GEDCOMRecord rec = this[i];
					if (rec is GEDCOMIndividualRecord)
					{
						GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
						for (int j = iRec.IndividualEvents.Count - 1; j >= 0; j--)
						{
							GEDCOMCustomEvent ev = iRec.IndividualEvents[j];
							if (ev.Detail.Place.Location.Value == locRec)
							{
								ev.Detail.Place.DeleteTag("_LOC");
							}
						}
					}
					else
					{
						if (rec is GEDCOMFamilyRecord)
						{
							GEDCOMFamilyRecord fRec = rec as GEDCOMFamilyRecord;
							for (int j = fRec.FamilyEvents.Count - 1; j >= 0; j--)
							{
								GEDCOMCustomEvent ev = fRec.FamilyEvents[j];
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
