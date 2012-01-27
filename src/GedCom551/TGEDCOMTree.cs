using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using GKSys;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public sealed class TGEDCOMTree : TGEDCOMObject, IDisposable
	{
		private TGEDCOMListEx<TGEDCOMRecord> FRecords;
		private TGEDCOMHeader FHeader;
		private Hashtable FXRefIndex;
		private TGEDCOMState FState;
		private bool Disposed_;

		public int RecordsCount
		{
			get { return this.FRecords.Count; }
		}

		public TGEDCOMRecord this[int Index]
		{
			get { return this.FRecords[Index]; }
		}

		public TGEDCOMRecord GetRecord(int Index)
		{
			return this.FRecords[Index];
		}

		public TGEDCOMHeader Header
		{
			get { return this.FHeader; }
		}

		public TGEDCOMState State
		{
			get { return this.FState; }
			set { this.FState = value; }
		}

		public TGEDCOMTree()
		{
			this.FRecords = new TGEDCOMListEx<TGEDCOMRecord>(this);
			this.FHeader = new TGEDCOMHeader(this, this, "", "");
			this.FXRefIndex = new Hashtable();
		}

		static TGEDCOMTree()
		{
			GEDCOMFactory f = GEDCOMFactory.GetInstance();

			f.Register("DATE", TGEDCOMDateValue.Create);
			f.Register("TIME", TGEDCOMTime.Create);
			f.Register("ADDR", TGEDCOMAddress.Create);
			f.Register("PLAC", TGEDCOMPlace.Create);
			f.Register("MAP", TGEDCOMMap.Create);
			f.Register("_LOC", TGEDCOMPointer.Create);

			//f.Register("xxxx", xxxx.Create);
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				//this.FXRefIndex.Dispose();
				this.FHeader.Free();
				this.FRecords.Dispose();

				this.Disposed_ = true;
			}
		}

		private string GetSignByRecord(TGEDCOMRecord aRecord)
		{
			string result = "";
			if (aRecord != null)
			{
				switch (aRecord.RecordType) {
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

		private void XRefIndex_Clear()
		{
			this.FXRefIndex.Clear();
		}

		private void XRefIndex_AddRecord(TGEDCOMCustomRecord ARecord)
		{
			if (ARecord != null && !string.IsNullOrEmpty(ARecord.XRef))
			{
				bool exists = this.FXRefIndex.ContainsKey(ARecord.XRef);
				if (!exists) this.FXRefIndex.Add(ARecord.XRef, ARecord);
			}
		}

		public void SetXRef([In] string oldXRef, TGEDCOMCustomRecord Sender)
		{
			if (!string.IsNullOrEmpty(oldXRef))
			{
				bool exists = this.FXRefIndex.ContainsKey(oldXRef);
				if (exists) this.FXRefIndex.Remove(oldXRef);
			}

			this.XRefIndex_AddRecord(Sender);
		}

		private void XRefIndex_DeleteRecord(TGEDCOMRecord ARecord)
		{
			bool exists = this.FXRefIndex.ContainsKey(ARecord.XRef);
			if (exists) this.FXRefIndex.Remove(ARecord.XRef);
		}

		public TGEDCOMRecord XRefIndex_Find([In] string XRef)
		{
			return (this.FXRefIndex[XRef] as TGEDCOMRecord);
		}

		public string XRefIndex_NewXRef(TGEDCOMRecord Sender)
		{
			string sign = this.GetSignByRecord(Sender);
			int I = 1;
			while (this.FXRefIndex.ContainsKey(sign + I.ToString()))
			{
				I++;
			}
			return sign + I.ToString();
		}

		public TGEDCOMRecord AddRecord(TGEDCOMRecord ARecord)
		{
			this.FRecords.Add(ARecord);
			this.XRefIndex_AddRecord(ARecord);
			return ARecord;
		}

		public void Clear()
		{
			this.FRecords.Clear();
			this.FHeader.Clear();
			this.XRefIndex_Clear();
		}

		public void Delete(int Index)
		{
			this.XRefIndex_DeleteRecord(this.FRecords[Index]);
			this.FRecords.Delete(Index);
		}

		public void DeleteRecord(TGEDCOMRecord Sender)
		{
			this.XRefIndex_DeleteRecord(Sender);
			this.FRecords.DeleteObject(Sender);
		}

		public TGEDCOMRecord Extract(int Index)
		{
			this.XRefIndex_DeleteRecord(this.FRecords[Index]);
			return this.FRecords.Extract(Index) as TGEDCOMRecord;
		}

		public int IndexOfRecord(TGEDCOMRecord ARecord)
		{
			return this.FRecords.IndexOfObject(ARecord);
		}

		public void SaveToFile([In] string aFileName, [In] TGEDCOMCharacterSet CharSet)
		{
			string subm = this.FHeader.GetTagStringValue("SUBM");
			int rev = this.FHeader.FileRevision;

			this.FHeader.Clear();
			this.FHeader.Source = "GEDKeeper";
			this.FHeader.ReceivingSystemName = "GEDKeeper";
			this.FHeader.CharacterSet = CharSet;
			this.FHeader.Language = "Russian";
			this.FHeader.GEDCOMVersion = "5.5";
			this.FHeader.GEDCOMForm = "LINEAGE-LINKED";
			this.FHeader.FileName = Path.GetFileName(aFileName);
			this.FHeader.TransmissionDateTime = DateTime.Now;
			this.FHeader.FileRevision = rev + 1;

			if (subm != "")
			{
				this.FHeader.SetTagStringValue("SUBM", subm);
			}

			this.Pack();

			StreamWriter fs = new StreamWriter(aFileName, false, TGEDCOMObject.GetEncodingByCharacterSet(CharSet));
			try
			{
				this.SaveToStream(fs);
				this.FHeader.CharacterSet = TGEDCOMCharacterSet.csASCII;
			}
			finally
			{
				SysUtils.Free(fs);
			}
		}

		public void LoadFromFile([In] string aFileName)
		{
			StreamReader fs = new StreamReader(aFileName, Encoding.GetEncoding(1251));
			try
			{
				this.Clear();
				this.LoadFromStream(fs);
				this.FHeader.CharacterSet = TGEDCOMCharacterSet.csASCII;
			}
			finally
			{
				fs.Dispose();
			}
		}

		public int Progress;
		public event EventHandler ProgressEvent;

		public void LoadFromStream(StreamReader AStream)
		{
			this.FState = TGEDCOMState.osLoading;
			try
			{
				TGEDCOMCustomRecord CurRecord = null;
				TGEDCOMTag CurTag = null;
				TGEDCOMCharacterSet CharSet = TGEDCOMCharacterSet.csASCII;

				long file_pos = 0;
				long file_size = AStream.BaseStream.Length;

				Progress = 0;

				int I = -1;
				while (AStream.Peek() != -1)
				{
					I++;
					string S = AStream.ReadLine();
					file_pos += S.Length + Environment.NewLine.Length;

					S = SysUtils.TrimLeft(S);

					if (S.Length != 0)
					{
						if (!SysUtils.IsDigit(S[0]))
						{
							TGEDCOMTree._LoadFromStream_LineCorrect(CurRecord, CurTag, I + 1, S.Trim());
						}
						else
						{
							int ALevel;
							string AXRef;
							string ATag;
							string AValue;
							try
							{
								S = TGEDCOMObject.ExtractNumber(S, out ALevel, false, 0);
								S = base.ExtractDelimiter(S, 0);
								S = base.ExtractXRef(S, out AXRef, true, "");
								S = base.ExtractDelimiter(S, 0);
								S = base.ExtractString(S, out ATag, "");
								ATag = ATag.ToUpperInvariant();
								S = base.ExtractDelimiter(S, 1);
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
							if (AValue != null && AValue.Length != 0 && CharSet == TGEDCOMCharacterSet.csUTF8)
							{
								if (AStream.CurrentEncoding != Encoding.UTF8) {
									AValue = SysUtils.StrToUtf8(AValue);
								}
							}
							// end

							if (ALevel == 0)
							{
								if (ATag == "HEAD")
								{
									CurRecord = this.FHeader;
								}
								else
								{
									if (ATag == "TRLR")
									{
										break;
									}

									if (ATag == "FAM")
									{
										CurRecord = this.AddRecord(new TGEDCOMFamilyRecord(this, this, "", ""));
									}
									else if (ATag == "INDI")
									{
										CurRecord = this.AddRecord(new TGEDCOMIndividualRecord(this, this, "", ""));
									}
									else if (ATag == "OBJE")
									{
										CurRecord = this.AddRecord(new TGEDCOMMultimediaRecord(this, this, "", ""));
									}
									else if (ATag == "NOTE")
									{
										CurRecord = this.AddRecord(new TGEDCOMNoteRecord(this, this, "", ""));
									}
									else if (ATag == "REPO")
									{
										CurRecord = this.AddRecord(new TGEDCOMRepositoryRecord(this, this, "", ""));
									}
									else if (ATag == "SOUR")
									{
										CurRecord = this.AddRecord(new TGEDCOMSourceRecord(this, this, "", ""));
									}
									else if (ATag == "SUBN")
									{
										CurRecord = this.AddRecord(new TGEDCOMSubmissionRecord(this, this, "", ""));
									}
									else if (ATag == "SUBM")
									{
										CurRecord = this.AddRecord(new TGEDCOMSubmitterRecord(this, this, "", ""));
									}
									else if (ATag == "_GROUP")
									{
										CurRecord = this.AddRecord(new TGEDCOMGroupRecord(this, this, "", ""));
									}
									else if (ATag == "_RESEARCH")
									{
										CurRecord = this.AddRecord(new TGEDCOMResearchRecord(this, this, "", ""));
									}
									else if (ATag == "_TASK")
									{
										CurRecord = this.AddRecord(new TGEDCOMTaskRecord(this, this, "", ""));
									}
									else if (ATag == "_COMM")
									{
										CurRecord = this.AddRecord(new TGEDCOMCommunicationRecord(this, this, "", ""));
									}
									else if (ATag == "_LOC")
									{
										CurRecord = this.AddRecord(new TGEDCOMLocationRecord(this, this, "", ""));
									}
									else
									{
										CurRecord = null;
									}
								}

								if (CurRecord != null && AXRef != "")
								{
									CurRecord.XRef = AXRef;
								}
								CurTag = null;
							}
							else
							{
								// temp hack
								if (ATag == "CHAR") {
									CharSet = base.GetCharacterSetVal(AValue);
								}
								// end

								if (CurRecord != null)
								{
									if (CurTag == null || ALevel == 1)
									{
										CurTag = CurRecord.AddTag(ATag, AValue, null);
									}
									else
									{
										while (ALevel <= CurTag.Level)
										{
											CurTag = (CurTag.Parent as TGEDCOMTag);
										}
										CurTag = CurTag.AddTag(ATag, AValue, null);
									}
								}
							}
						}
					}

					int new_progress = (int)Math.Min(100, (file_pos * 100.0F) / file_size);
					if (Progress != new_progress)
					{
						Progress = new_progress;
						if (ProgressEvent != null) ProgressEvent(this, EventArgs.Empty);
					}
				}
			}
			finally
			{
				this.FState = TGEDCOMState.osReady;
			}
		}

		public void SaveToStream(StreamWriter AStream)
		{
			this.SaveHeaderToStream(AStream);

			int num = this.FRecords.Count - 1;
			for (int I = 0; I <= num; I++)
			{
				this.FRecords[I].SaveToStream(AStream);
			}

			this.SaveFooterToStream(AStream);
		}

		public void SaveHeaderToStream(StreamWriter AStream)
		{
			this.FHeader.SaveToStream(AStream);
		}

		public void SaveFooterToStream(StreamWriter AStream)
		{
			string S = "0 TRLR";
			AStream.WriteLine(S);
		}

		public void Pack()
		{
			int num = this.FRecords.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				this.FRecords[i].Pack();
			}
		}

		public TGEDCOMRecord FindUID([In] string UID)
		{
			TGEDCOMRecord Result = null;

			int num = this.FRecords.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				TGEDCOMRecord rec;
				while (true)
				{
					rec = this.FRecords[i];
					if (rec.UID == UID)
					{
						break;
					}
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = rec;
			}
			return Result;
		}

		private static void _LoadFromStream_LineCorrect(TGEDCOMCustomRecord CurRecord, TGEDCOMCustomTag CurTag, int LineNum, string S)
		{
			try
			{
				if (CurTag != null && CurTag is TGEDCOMNotes)
				{
					CurTag.AddTag("CONT", S, null);
				}
				else
				{
					if (CurRecord != null)
					{
						CurRecord.AddTag("NOTE", S, null);
					}
				}
				Trace.Write("TGEDCOMTree.LoadFromStream(): " + CurRecord.XRef + " notes correct");
			}
			catch (Exception E)
			{
				Trace.Write("Line " + LineNum.ToString() + ". Failed correct: " + E.Message);
			}
		}
	}
}
