using GKSys;
using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace GedCom551
{
	public class TGEDCOMTree : TGEDCOMObject, IDisposable
	{
		public enum TGEDCOMState : byte
		{
			osLoading,
			osReady
		}

		internal TGEDCOMList FRecords;
		internal TGEDCOMHeader FHeader;
		internal TStringList FXRefIndex;
		internal TGEDCOMTree.TGEDCOMState FState;
		protected internal bool Disposed_;

		[Browsable(false)]
		public int RecordsCount
		{
			get
			{
				return this.GetRecordsCount();
			}
		}
		[Browsable(false)]
		public TGEDCOMHeader Header
		{
			get
			{
				return this.FHeader;
			}
		}

		public TGEDCOMRecord /*Records*/this[int Index]
		{
			get { return this.GetRecord(Index); }
		}

		[Browsable(false)]
		public TGEDCOMTree.TGEDCOMState State
		{
			get
			{
				return this.FState;
			}
			set
			{
				this.FState = value;
			}
		}

		internal string GetSignByRecord(TGEDCOMRecord aRecord)
		{
			string Result;
			if (aRecord == null)
			{
				Result = "";
			}
			else
			{
				switch (aRecord.RecordType)
				{
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						Result = "I";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						Result = "F";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
					{
						Result = "N";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						Result = "O";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						Result = "S";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						Result = "R";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
					{
						Result = "G";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
					{
						Result = "RS";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
					{
						Result = "TK";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
					{
						Result = "CM";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
					{
						Result = "L";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSubmission:
					{
						Result = "????";
						return Result;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSubmitter:
					{
						Result = "SUB";
						return Result;
					}
				}
				Result = "";
			}
			return Result;
		}

		internal int GetRecordsCount()
		{
			return this.FRecords.Count;
		}

		public TGEDCOMRecord GetRecord(int Index)
		{
			return this.FRecords[Index] as TGEDCOMRecord;
		}

		internal void XRefIndex_Clear()
		{
			this.FXRefIndex.Clear();
		}

		internal void XRefIndex_Add([In] string XRef, TGEDCOMRecord ARecord)
		{
			if (XRef != "" && ARecord != null)
			{
				this.FXRefIndex.AddObject(XRef, ARecord);
			}
		}

		internal void XRefIndex_AddRecord(TGEDCOMRecord ARecord)
		{
			int Index = this.FXRefIndex.IndexOfObject(ARecord);
			if (Index == -1 && ARecord.XRef != "")
			{
				this.XRefIndex_Add(ARecord.XRef, ARecord);
			}
		}

		internal void XRefIndex_DeleteRecord(TGEDCOMRecord ARecord)
		{
			int Index = this.FXRefIndex.IndexOfObject(ARecord);
			if (Index >= 0)
			{
				this.FXRefIndex.Delete(Index);
			}
		}

		protected internal void SetXRef(TGEDCOMRecord Sender, [In] string XRef)
		{
			int Index = this.FXRefIndex.IndexOfObject(Sender);
			if (Index >= 0)
			{
				this.FXRefIndex.Sorted = false;
				this.FXRefIndex[Index] = XRef;
				this.FXRefIndex.Sorted = true;
			}
			else
			{
				this.XRefIndex_Add(XRef, Sender);
			}
		}

		public TGEDCOMTree()
		{
			this.FRecords = new TGEDCOMList(this);
			this.FHeader = new TGEDCOMHeader(this, this, "", "");
			this.FXRefIndex = new TStringList();
			this.FXRefIndex.Sorted = true;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FXRefIndex.Free();
				this.FHeader.Free();
				this.FRecords.Free();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMRecord AddRecord(TGEDCOMRecord ARecord)
		{
			this.FRecords.Add(ARecord);
			if (ARecord.XRef != "")	this.XRefIndex_AddRecord(ARecord);
			return ARecord;
		}

		public virtual void Clear()
		{
			this.FRecords.Clear();
			this.FHeader.Clear();
			this.XRefIndex_Clear();
		}
		public void Delete(int Index)
		{
			this.XRefIndex_DeleteRecord(this.GetRecord(Index));
			this.FRecords.Delete(Index);
		}
		public void DeleteRecord(TGEDCOMRecord Sender)
		{
			this.XRefIndex_DeleteRecord(Sender);
			this.FRecords.DeleteObject(Sender);
		}
		public TGEDCOMRecord Extract(int Index)
		{
			this.XRefIndex_DeleteRecord(this.GetRecord(Index));
			return this.FRecords.Extract(Index) as TGEDCOMRecord;
		}
		public int IndexOfRecord(TGEDCOMRecord ARecord)
		{
			return this.FRecords.IndexOfObject(ARecord);
		}

		public void LoadFromFile([In] string aFileName)
		{
			StreamReader fs = new StreamReader(aFileName, Encoding.GetEncoding(1251));
			try
			{
				this.Clear();
				this.LoadFromStream(fs);
				this.FHeader.CharacterSet = TGEDCOMObject.TGEDCOMCharacterSet.csASCII;
			}
			finally
			{
				TObjectHelper.Free(fs);
			}
		}

		public void LoadFromStream(StreamReader AStream)
		{
			this.FState = TGEDCOMTree.TGEDCOMState.osLoading;
			try
			{
				TGEDCOMCustomRecord CurRecord = null;
				TGEDCOMTag CurTag = null;

				int I = -1;
				while (AStream.Peek() != -1)
				{
					string S = AStream.ReadLine().Trim();
					I++;

					if (I == 0 && S[0] != '0')
					{
						S = S.Remove(0, 3);
					}

					while (((S != null) ? S.Length : 0) > 0 && (S[0] == ' ' || S[0] == '\t'))
					{
						S = S.Remove(0, 1);
					}

					if (S != "")
					{
						if (!TGKSys.IsDigit(S[0]))
						{
							TGEDCOMTree._LoadFromStream_LineCorrect(CurRecord, CurTag, I + 1, S.Trim());
						}
						else
						{
							int ALevel = 0;
							string AXRef = "";
							string ATag = "";
							string AValue;
							try
							{
								S = TGEDCOMObject.ExtractNumber(S, ref ALevel, false, 0);
								S = base.ExtractDelimiter(S, 0);
								S = base.ExtractXRef(S, ref AXRef, true, "");
								S = base.ExtractDelimiter(S, 0);
								S = base.ExtractString(S, ref ATag, "");
								ATag = ATag.ToUpper();
								S = base.ExtractDelimiter(S, 1);
								AValue = S;
							}
							catch (EGEDCOMException E)
							{
								int X = I + 1;
								throw new EGEDCOMException("Syntax error in line " + X.ToString() + ".\r" + E.Message);
							}
							catch (Exception E)
							{
								throw;
							}
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
									else
									{
										if (ATag == "INDI")
										{
											CurRecord = this.AddRecord(new TGEDCOMIndividualRecord(this, this, "", ""));
										}
										else
										{
											if (ATag == "OBJE")
											{
												CurRecord = this.AddRecord(new TGEDCOMMultimediaRecord(this, this, "", ""));
											}
											else
											{
												if (ATag == "NOTE")
												{
													CurRecord = this.AddRecord(new TGEDCOMNoteRecord(this, this, "", ""));
												}
												else
												{
													if (ATag == "REPO")
													{
														CurRecord = this.AddRecord(new TGEDCOMRepositoryRecord(this, this, "", ""));
													}
													else
													{
														if (ATag == "SOUR")
														{
															CurRecord = this.AddRecord(new TGEDCOMSourceRecord(this, this, "", ""));
														}
														else
														{
															if (ATag == "SUBN")
															{
																CurRecord = this.AddRecord(new TGEDCOMSubmissionRecord(this, this, "", ""));
															}
															else
															{
																if (ATag == "SUBM")
																{
																	CurRecord = this.AddRecord(new TGEDCOMSubmitterRecord(this, this, "", ""));
																}
																else
																{
																	if (ATag == "_GROUP")
																	{
																		CurRecord = this.AddRecord(new TGEDCOMGroupRecord(this, this, "", ""));
																	}
																	else
																	{
																		if (ATag == "_RESEARCH")
																		{
																			CurRecord = this.AddRecord(new TGEDCOMResearchRecord(this, this, "", ""));
																		}
																		else
																		{
																			if (ATag == "_TASK")
																			{
																				CurRecord = this.AddRecord(new TGEDCOMTaskRecord(this, this, "", ""));
																			}
																			else
																			{
																				if (ATag == "_COMM")
																				{
																					CurRecord = this.AddRecord(new TGEDCOMCommunicationRecord(this, this, "", ""));
																				}
																				else
																				{
																					if (ATag == "_LOC")
																					{
																						CurRecord = this.AddRecord(new TGEDCOMLocationRecord(this, this, "", ""));
																					}
																					else
																					{
																						CurRecord = null;
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
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
				}
			}
			finally
			{
				this.FState = TGEDCOMTree.TGEDCOMState.osReady;
			}
		}

		public void SaveToStream(StreamWriter AStream)
		{
			this.SaveHeaderToStream(AStream);
			int arg_16_0 = 0;
			int num = this.FRecords.Count - 1;
			int I = arg_16_0;
			if (num >= I)
			{
				num++;
				do
				{
					this.GetRecord(I).SaveToStream(AStream);
					I++;
				}
				while (I != num);
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
			int arg_0F_0 = 0;
			int num = this.FRecords.Count - 1;
			int i = arg_0F_0;
			if (num >= i)
			{
				num++;
				do
				{
					this.GetRecord(i).Pack();
					i++;
				}
				while (i != num);
			}
		}
		public TGEDCOMRecord XRefIndex_Find([In] string XRef)
		{
			int Index = this.FXRefIndex.IndexOf(XRef);
			TGEDCOMRecord Result;
			if (Index >= 0)
			{
				Result = (this.FXRefIndex.GetObject(Index) as TGEDCOMRecord);
			}
			else
			{
				Result = null;
			}
			return Result;
		}
		public string XRefIndex_NewXRef(TGEDCOMRecord Sender)
		{
			string sign = this.GetSignByRecord(Sender);
			int I = 1;
			while (this.FXRefIndex.IndexOf(sign + I.ToString()) >= 0)
			{
				I++;
			}
			return sign + I.ToString();
		}
		public TGEDCOMRecord FindUID([In] string UID)
		{
			TGEDCOMRecord Result = null;
			int arg_11_0 = 0;
			int num = this.FRecords.Count - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				TGEDCOMRecord rec;
				while (true)
				{
					rec = this.GetRecord(i);
					if (BDSSystem.WStrCmp(rec.UID, UID) == 0)
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
