using System;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI
{
	public partial class TfmTreeTools : Form
	{
		private enum TCheckDiag : byte
		{
			cdPersonLonglived,
			cdPersonSexless,
			cdLiveYearsInvalid,
			cdStrangeSpouse,
			cdStrangeParent
		}

		private enum TCheckSolve : byte
		{
			csSkip,
			csSetIsDead,
			csDefineSex
		}

		private class TCheckObj
		{
			private string FComment;
			private TCheckDiag FDiag;
			private TGEDCOMRecord FRec;
			private TCheckSolve FSolve;
	
			public string Comment
			{
				get { return this.FComment;	}
				set	{ this.FComment = value; }
			}
	
			public TCheckDiag Diag
			{
				get	{ return this.FDiag; }
				set	{ this.FDiag = value; }
			}
	
			public TGEDCOMRecord Rec
			{
				get	{ return this.FRec;	}
				set	{ this.FRec = value; }
			}
	
			public string RecName
			{
				get	{ return this.GetRecName(); }
			}
	
			public TCheckSolve Solve
			{
				get { return this.FSolve; }
				set	{ this.FSolve = value; }
			}

			private string GetRecName()
			{
				string Result = "";
				if (this.FRec is TGEDCOMIndividualRecord)
				{
					Result = TGenEngine.GetNameStr(this.FRec as TGEDCOMIndividualRecord, true, false);
				}
				return Result;
			}

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		private class TPlaceObj : IDisposable
		{
			public string Name;
			public TList Facts;
			protected bool Disposed_;

			public TPlaceObj()
			{
				this.Facts = new TList();
			}

			public void Dispose()
			{
				if (!this.Disposed_)
				{
					this.Facts.Free();
					this.Disposed_ = true;
				}
			}

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		private enum TMergeMode : byte
		{
			mmPerson,
			mmNote,
			mmFamily,
			mmSource
		}

		private static readonly string[] HelpTopics;

		private TfmBase FBase;
		private TList FSplitList;
		private TGEDCOMTree FTree;
		private TGEDCOMRecord FRec1;
		private TGEDCOMRecord FRec2;
		private TfmTreeTools.TMergeMode FRMMode;
		private TStringList FRMSkip;
		private int FRMIndex;

		private TGKHyperView Memo1;
		private TGKHyperView Memo2;

		private TStringList FPlaces;
		private TGKListView ListPlaces;

		private TObjectList FChecksList;
		private TGKListView ListChecks;

		private TGKListView ListPatriarchs;


		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		private bool GetIndivName(TGEDCOMIndividualRecord iRec, bool only_np, ref string aName)
		{
			bool Result;
			if (only_np)
			{
				string f, i, p;
				TGenEngine.GetNameParts(iRec, out f, out i, out p);
				aName = i + " " + p;
				string text = aName;
				Result = (((text != null) ? text.Length : 0) > 3);
			}
			else
			{
				TGEDCOMPersonalName np = iRec.PersonalNames[0];
				aName = np.StringValue;
				string firstPart = np.FirstPart;
				Result = (((firstPart != null) ? firstPart.Length : 0) > 3);
			}
			return Result;
		}

		private void SearchDups()
		{
			int nameAccuracy = decimal.ToInt32(this.edNameAccuracy.Value);
			int yearInaccuracy = decimal.ToInt32(this.edYearInaccuracy.Value);
			bool only_np = this.chkOnlyNP.Checked;
			bool res = false;
			this.btnSkip.Enabled = false;
			try
			{
				this.ProgressBar1.Minimum = 0;
				this.ProgressBar1.Maximum = this.FTree.RecordsCount;
				this.ProgressBar1.Value = this.FRMIndex;
				int arg_87_0 = this.FRMIndex;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_87_0;
				if (num >= i)
				{
					num++;
					while (true)
					{
						this.FRMIndex = i;
						TGEDCOMRecord iRec = this.FTree.GetRecord(i);
						TfmTreeTools.TMergeMode fRMMode = this.FRMMode;
						if (fRMMode != TfmTreeTools.TMergeMode.mmPerson)
						{
							if (fRMMode != TfmTreeTools.TMergeMode.mmNote)
							{
								if (fRMMode != TfmTreeTools.TMergeMode.mmFamily)
								{
									if (fRMMode != TfmTreeTools.TMergeMode.mmSource)
									{
										goto IL_509;
									}
									if (!(iRec is TGEDCOMSourceRecord))
									{
										goto IL_509;
									}
									TGEDCOMSourceRecord iSrc = (TGEDCOMSourceRecord)iRec;
									string iName = iSrc.FiledByEntry;
									int arg_480_0 = i + 1;
									int num2 = this.FTree.RecordsCount - 1;
									int j = arg_480_0;
									if (num2 >= j)
									{
										num2++;
										TGEDCOMSourceRecord kSrc;
										while (true)
										{
											TGEDCOMRecord kRec = this.FTree.GetRecord(j);
											if (kRec is TGEDCOMSourceRecord)
											{
												kSrc = (TGEDCOMSourceRecord)kRec;
												string kName = kSrc.FiledByEntry;
												res = (iName == kName && this.FRMSkip.IndexOf(iSrc.XRef + "-" + kSrc.XRef) < 0);
												if (res)
												{
													break;
												}
											}
											j++;
											if (j == num2)
											{
												goto IL_509;
											}
										}
										this.SetRec1(iSrc);
										this.SetRec2(kSrc);
										goto IL_509;
									}
									goto IL_509;
								}
								else
								{
									if (!(iRec is TGEDCOMFamilyRecord))
									{
										goto IL_509;
									}
									TGEDCOMFamilyRecord iFam = (TGEDCOMFamilyRecord)iRec;
									string iName = TGenEngine.GetFamilyStr(iFam);
									int arg_3B7_0 = i + 1;
									int num3 = this.FTree.RecordsCount - 1;
									int j = arg_3B7_0;
									if (num3 >= j)
									{
										num3++;
										TGEDCOMFamilyRecord kFam;
										while (true)
										{
											TGEDCOMRecord kRec = this.FTree.GetRecord(j);
											if (kRec is TGEDCOMFamilyRecord)
											{
												kFam = (TGEDCOMFamilyRecord)kRec;
												string kName = TGenEngine.GetFamilyStr(kFam);
												res = (iName == kName && this.FRMSkip.IndexOf(iFam.XRef + "-" + kFam.XRef) < 0);
												if (res)
												{
													break;
												}
											}
											j++;
											if (j == num3)
											{
												goto Block_39;
											}
										}
										this.SetRec1(iFam);
										this.SetRec2(kFam);
										Block_39:
										goto IL_509;
									}
									goto IL_509;
								}
							}
							else
							{
								if (!(iRec is TGEDCOMNoteRecord))
								{
									goto IL_509;
								}
								TGEDCOMNoteRecord iNote = (TGEDCOMNoteRecord)iRec;
								string iName = iNote.Note.Text;
								int arg_2E9_0 = i + 1;
								int num4 = this.FTree.RecordsCount - 1;
								int j = arg_2E9_0;
								if (num4 >= j)
								{
									num4++;
									TGEDCOMNoteRecord kNote;
									while (true)
									{
										TGEDCOMRecord kRec = this.FTree.GetRecord(j);
										if (kRec is TGEDCOMNoteRecord)
										{
											kNote = (TGEDCOMNoteRecord)kRec;
											string kName = kNote.Note.Text;
											res = (iName == kName && this.FRMSkip.IndexOf(iNote.XRef + "-" + kNote.XRef) < 0);
											if (res)
											{
												break;
											}
										}
										j++;
										if (j == num4)
										{
											goto Block_33;
										}
									}
									this.SetRec1(iNote);
									this.SetRec2(kNote);
									Block_33:
									goto IL_509;
								}
								goto IL_509;
							}
						}
						else
						{
							if (!(iRec is TGEDCOMIndividualRecord))
							{
								goto IL_509;
							}
							TGEDCOMIndividualRecord iInd = (TGEDCOMIndividualRecord)iRec;
							string iName = "";
							if (this.GetIndivName(iInd, only_np, ref iName))
							{
								int arg_10C_0 = i + 1;
								int num5 = this.FTree.RecordsCount - 1;
								int j = arg_10C_0;
								if (num5 >= j)
								{
									num5++;
									TGEDCOMIndividualRecord kInd;
									while (true)
									{
										TGEDCOMRecord kRec = this.FTree.GetRecord(j);
										if (kRec is TGEDCOMIndividualRecord)
										{
											kInd = (TGEDCOMIndividualRecord)kRec;
											string kName = "";
											if (this.GetIndivName(kInd, only_np, ref kName) && iInd.Sex == kInd.Sex && this.FRMSkip.IndexOf(iInd.XRef + "-" + kInd.XRef) < 0 && (!only_np || (iInd.Sex == TGEDCOMSex.svFemale && kInd.Sex == TGEDCOMSex.svFemale)))
											{
												if (this.rbDirectMatching.Checked)
												{
													res = (iName == kName);
												}
												else
												{
													if (this.rbIndistinctMatching.Checked)
													{
														res = (TGenEngine.IndistinctMatching(4, iName, kName) > nameAccuracy);
													}
												}
												if (res && this.chkBirthYear.Checked)
												{
													TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iInd, "BIRT");
													int year;
													if (ev == null)
													{
														year = 0;
													}
													else
													{
														year = (ev.Detail.Date.Value as TGEDCOMDate).Year;
													}
													ev = TGenEngine.GetIndividualEvent(kInd, "BIRT");
													int year2;
													if (ev == null)
													{
														year2 = 0;
													}
													else
													{
														year2 = (ev.Detail.Date.Value as TGEDCOMDate).Year;
													}
													res = (res && year >= 0 && year2 >= 0 && Math.Abs(year - year2) <= yearInaccuracy);
												}
												if (res)
												{
													break;
												}
											}
										}
										j++;
										if (j == num5)
										{
											goto Block_27;
										}
									}
									this.SetRec1(iInd);
									this.SetRec2(kInd);
									Block_27:
									goto IL_509;
								}
								goto IL_509;
							}
						}
						IL_518:
						i++;
						if (i == num)
						{
							break;
						}
						continue;
						IL_509:
						if (!res)
						{
							this.ProgressBar1.Increment(1);
							goto IL_518;
						}
						break;
					}
				}
			}
			finally
			{
				this.btnSkip.Enabled = true;
			}
		}

		private void RecordMerge(TGEDCOMRecord aRecBase, TGEDCOMRecord aRecCopy)
		{
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				repMap.AddXRef(aRecCopy, aRecCopy.XRef, aRecBase.XRef);

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					this.FTree.GetRecord(i).ReplaceXRefs(repMap);
				}

				TfmTreeTools.TMergeMode fRMMode = this.FRMMode;
				if (fRMMode != TfmTreeTools.TMergeMode.mmPerson)
				{
					if (fRMMode != TfmTreeTools.TMergeMode.mmNote)
					{
						if (fRMMode != TfmTreeTools.TMergeMode.mmFamily)
						{
							if (fRMMode == TfmTreeTools.TMergeMode.mmSource)
							{
								(aRecCopy as TGEDCOMSourceRecord).MoveTo(aRecBase, false);
								this.Base.DeleteSourceRecord(aRecCopy as TGEDCOMSourceRecord, false);
							}
						}
						else
						{
							(aRecCopy as TGEDCOMFamilyRecord).MoveTo(aRecBase, false);
							this.Base.DeleteFamilyRecord(aRecCopy as TGEDCOMFamilyRecord, false);
						}
					}
					else
					{
						(aRecCopy as TGEDCOMNoteRecord).MoveTo(aRecBase, false);
						this.Base.DeleteNoteRecord(aRecCopy as TGEDCOMNoteRecord, false);
					}
				}
				else
				{
					(aRecCopy as TGEDCOMIndividualRecord).MoveTo(aRecBase, false);
					this.Base.DeleteIndividualRecord(aRecCopy as TGEDCOMIndividualRecord, false);
				}
				this.Base.ChangeRecord(aRecBase);
				this.Base.ListsRefresh(false);
			}
			finally
			{
				repMap.Free();
			}
		}

		private void SetRec1([In] TGEDCOMRecord Value)
		{
			this.FRec1 = Value;
			this.btnMergeToLeft.Enabled = (this.FRec1 != null && this.FRec2 != null);
			this.btnMergeToRight.Enabled = (this.FRec1 != null && this.FRec2 != null);
			if (this.FRec1 == null)
			{
				this.Lab1.Text = "XXX1";
				this.Edit1.Text = "";
				this.Memo1.Lines.Clear();
			}
			else
			{
				this.Lab1.Text = this.FRec1.XRef;
				TfmTreeTools.TMergeMode fRMMode = this.FRMMode;
				if (fRMMode != TfmTreeTools.TMergeMode.mmPerson)
				{
					if (fRMMode != TfmTreeTools.TMergeMode.mmNote)
					{
						if (fRMMode != TfmTreeTools.TMergeMode.mmFamily)
						{
							if (fRMMode == TfmTreeTools.TMergeMode.mmSource)
							{
								this.Edit1.Text = (this.FRec1 as TGEDCOMSourceRecord).FiledByEntry;
								this.Base.ShowSourceInfo(this.FRec1 as TGEDCOMSourceRecord, this.Memo1.Lines);
							}
						}
						else
						{
							this.Edit1.Text = TGenEngine.GetFamilyStr(this.FRec1 as TGEDCOMFamilyRecord);
							this.Base.ShowFamilyInfo(this.FRec1 as TGEDCOMFamilyRecord, this.Memo1.Lines);
						}
					}
					else
					{
						this.Edit1.Text = (this.FRec1 as TGEDCOMNoteRecord).Note[0];
						this.Base.ShowNoteInfo(this.FRec1 as TGEDCOMNoteRecord, this.Memo1.Lines);
					}
				}
				else
				{
					this.Edit1.Text = TGenEngine.GetNameStr(this.FRec1 as TGEDCOMIndividualRecord, true, false);
					this.Base.ShowPersonInfo(this.FRec1 as TGEDCOMIndividualRecord, this.Memo1.Lines);
				}
			}
		}

		private void SetRec2([In] TGEDCOMRecord Value)
		{
			this.FRec2 = Value;
			this.btnMergeToLeft.Enabled = (this.FRec1 != null && this.FRec2 != null);
			this.btnMergeToRight.Enabled = (this.FRec1 != null && this.FRec2 != null);
			if (this.FRec2 == null)
			{
				this.Lab2.Text = "XXX2";
				this.Edit2.Text = "";
				this.Memo2.Lines.Clear();
			}
			else
			{
				this.Lab2.Text = this.FRec2.XRef;
				TfmTreeTools.TMergeMode fRMMode = this.FRMMode;
				if (fRMMode != TfmTreeTools.TMergeMode.mmPerson)
				{
					if (fRMMode != TfmTreeTools.TMergeMode.mmNote)
					{
						if (fRMMode != TfmTreeTools.TMergeMode.mmFamily)
						{
							if (fRMMode == TfmTreeTools.TMergeMode.mmSource)
							{
								this.Edit2.Text = (this.FRec2 as TGEDCOMSourceRecord).FiledByEntry;
								this.Base.ShowSourceInfo(this.FRec2 as TGEDCOMSourceRecord, this.Memo2.Lines);
							}
						}
						else
						{
							this.Edit2.Text = TGenEngine.GetFamilyStr(this.FRec2 as TGEDCOMFamilyRecord);
							this.Base.ShowFamilyInfo(this.FRec2 as TGEDCOMFamilyRecord, this.Memo2.Lines);
						}
					}
					else
					{
						this.Edit2.Text = (this.FRec2 as TGEDCOMNoteRecord).Note[0];
						this.Base.ShowNoteInfo(this.FRec2 as TGEDCOMNoteRecord, this.Memo2.Lines);
					}
				}
				else
				{
					this.Edit2.Text = TGenEngine.GetNameStr(this.FRec2 as TGEDCOMIndividualRecord, true, false);
					this.Base.ShowPersonInfo(this.FRec2 as TGEDCOMIndividualRecord, this.Memo2.Lines);
				}
			}
		}

		private void Select(TGEDCOMIndividualRecord aPerson, TGenEngine.TTreeWalkMode aMode)
		{
			this.FSplitList.Clear();
			TGenEngine.TreeWalk(aPerson, aMode, this.FSplitList);
			this.UpdateSplitLists();
		}

		private void CheckRelations()
		{
			int num = this.FSplitList.Count;
			for (int i = 0; i < num; i++)
			{
				TGEDCOMRecord rec = this.FSplitList[i] as TGEDCOMRecord;
				switch (rec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						_CheckRelations_CheckIndividual((TGEDCOMIndividualRecord)rec);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						_CheckRelations_CheckFamily((TGEDCOMFamilyRecord)rec);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						_CheckRelations_CheckSource((TGEDCOMSourceRecord)rec);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
					case TGEDCOMRecordType.rtSubmitter:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
				}
			}
		}

		private void UpdateSplitLists()
		{
			this.ListSelected.BeginUpdate();
			this.ListSelected.Items.Clear();
			this.ListSkipped.BeginUpdate();
			this.ListSkipped.Items.Clear();
			try
			{
				int cnt = 0;

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this.FTree.GetRecord(i) is TGEDCOMIndividualRecord)
					{
						cnt++;
						TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);
						if (this.FSplitList.IndexOf(i_rec) < 0)
						{
							this.ListSkipped.Items.Add(i_rec.XRef + " / " + TGenEngine.GetNameStr(i_rec, true, false));
						}
						else
						{
							this.ListSelected.Items.Add(i_rec.XRef + " / " + TGenEngine.GetNameStr(i_rec, true, false));
						}
					}
				}
				this.Text = this.FSplitList.Count.ToString() + " / " + cnt.ToString();
			}
			finally
			{
				this.ListSelected.EndUpdate();
				this.ListSkipped.EndUpdate();
			}
		}

		private void TreeCompare(TGEDCOMTree aMainTree, string aFileName)
		{
			this.ListCompare.Clear();
			TGEDCOMTree tempTree = new TGEDCOMTree();
			tempTree.LoadFromFile(aFileName);
			TStringList fams = new TStringList();
			TStringList names = new TStringList();
			try
			{
				this.ListCompare.AppendText(GKL.LSList[520] + "\r\n");

				int i;
				string fam, nam, pat;
				int num = aMainTree.RecordsCount - 1;
				for (i = 0; i <= num; i++)
				{
					if (aMainTree.GetRecord(i) is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)aMainTree.GetRecord(i);
						int idx = names.AddObject(TGenEngine.GetNameStr(iRec, true, false), new TList());
						(names.GetObject(idx) as TList).Add(iRec);
						TGenEngine.GetNameParts(iRec, out fam, out nam, out pat);
						fams.AddObject(TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMSex.svFemale), null);
					}
				}

				int num2 = tempTree.RecordsCount - 1;
				for (i = 0; i <= num2; i++)
				{
					if (tempTree.GetRecord(i) is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)tempTree.GetRecord(i);
						string tm = TGenEngine.GetNameStr(iRec, true, false);
						int idx = names.IndexOf(tm);
						if (idx >= 0)
						{
							(names.GetObject(idx) as TList).Add(iRec);
						}
						TGenEngine.GetNameParts(iRec, out fam, out nam, out pat);
						tm = TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMSex.svFemale);
						idx = fams.IndexOf(tm);
						if (idx >= 0)
						{
							fams.PutObject(idx, 1);
						}
					}
				}

				for (i = fams.Count - 1; i >= 0; i--)
				{
					if (fams.GetObject(i) == null || fams[i] == "?")
						fams.Delete(i);
				}

				for (i = names.Count - 1; i >= 0; i--)
				{
					if ((names.GetObject(i) as TList).Count == 1)
					{
						(names.GetObject(i) as TList).Free();
						names.Delete(i);
					}
				}

				if (fams.Count != 0)
				{
					this.ListCompare.AppendText(GKL.LSList[576] + "\r\n");

					int num3 = fams.Count - 1;
					for (i = 0; i <= num3; i++)
					{
						this.ListCompare.AppendText("    " + fams[i] + "\r\n");
					}
				}

				if (names.Count != 0)
				{
					this.ListCompare.AppendText(GKL.LSList[577] + "\r\n");

					int num4 = names.Count - 1;
					for (i = 0; i <= num4; i++)
					{
						this.ListCompare.AppendText("    " + names[i] + "\r\n");
						TList lst = names.GetObject(i) as TList;

						int num5 = lst.Count - 1;
						for (int j = 0; j <= num5; j++)
						{
							TGEDCOMIndividualRecord iRec = lst[j] as TGEDCOMIndividualRecord;
							this.ListCompare.AppendText(string.Concat(new string[]
							{
								"      * ", 
								TGenEngine.GetNameStr(iRec, true, false), 
								" ", 
								TGenEngine.GetLifeStr(iRec), 
								"\r\n"
							}));
						}
					}
				}
			}
			finally
			{
				int num6 = names.Count - 1;
				for (int i = 0; i <= num6; i++)
				{
					TObjectHelper.Free(names.GetObject(i));
				}
				names.Free();
				fams.Free();
				tempTree.Dispose();
			}
		}

		private void CheckGroups()
		{
			TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[521]);
			TList prepared = new TList();
			try
			{
				int group = 0;
				this.TreeView1.Nodes.Clear();

				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					while (true)
					{
						if (!(this.FTree.GetRecord(i) is TGEDCOMIndividualRecord))
						{
							goto IL_193;
						}
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);
						if (prepared.IndexOf(iRec) < 0)
						{
							group++;
							this.FSplitList.Clear();
							TGenEngine.TreeWalk(iRec, TGenEngine.TTreeWalkMode.twmAll, this.FSplitList);
							TreeNode root = this.TreeView1.Nodes.Add(string.Concat(new string[]
							{
								group.ToString(), 
								" ", 
								GKL.LSList[185].ToLower(), 
								" (", 
								this.FSplitList.Count.ToString(), 
								")"
							}));

							int num2 = this.FSplitList.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								iRec = (TGEDCOMIndividualRecord)this.FSplitList[j];
								prepared.Add(iRec);
								string pn = TGenEngine.GetNameStr(iRec, true, false);
								if (iRec.Patriarch)
								{
									pn = "(*) " + pn;
								}
								root.Nodes.Add(new TGKTreeNode(pn, iRec));
							}
							root.ExpandAll();
							goto IL_193;
						}
						IL_19D:
						i++;
						if (i == num)
						{
							break;
						}
						continue;
						IL_193:
						TfmProgress.ProgressStep();
						Application.DoEvents();
						goto IL_19D;
					}
				}
			}
			finally
			{
				this.FSplitList.Clear();
				prepared.Free();
				TfmProgress.ProgressDone();
			}
		}

		private void PrepareChecksList()
		{
			this.Base.CreateListView(this.Panel1, ref this.ListChecks);
			this.ListChecks.CheckBoxes = true;
			this.ListChecks.DoubleClick += new EventHandler(this.ListChecksDblClick);
			this.ListChecks.AddListColumn(GKL.LSList[579], 400, false);
			this.ListChecks.AddListColumn(GKL.LSList[580], 200, false);
			this.ListChecks.AddListColumn(GKL.LSList[581], 200, false);
		}

		private void CheckBase()
		{
			try
			{
				TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[517]);
				this.FChecksList.Clear();

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TfmProgress.ProgressStep();
					if (this.FTree.GetRecord(i) is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);
						int iAge;
						if (TGenEngine.GetIndividualEvent(iRec, "DEAT") == null)
						{
							string age = TGenEngine.GetAge(iRec, -1);
							if (age != "" && age != "?")
							{
								iAge = int.Parse(age);
								if (iAge >= 130)
								{
									TfmTreeTools.TCheckObj checkObj = new TfmTreeTools.TCheckObj();
									checkObj.Rec = iRec;
									checkObj.Diag = TfmTreeTools.TCheckDiag.cdPersonLonglived;
									checkObj.Solve = TfmTreeTools.TCheckSolve.csSetIsDead;
									checkObj.Comment = string.Format(GKL.LSList[582], new object[]
									{
										age
									});
									this.FChecksList.Add(checkObj);
								}
							}
						}
						TGEDCOMSex sex = iRec.Sex;
						if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
						{
							TfmTreeTools.TCheckObj checkObj = new TfmTreeTools.TCheckObj();
							checkObj.Rec = iRec;
							checkObj.Diag = TfmTreeTools.TCheckDiag.cdPersonSexless;
							checkObj.Solve = TfmTreeTools.TCheckSolve.csDefineSex;
							checkObj.Comment = GKL.LSList[583];
							this.FChecksList.Add(checkObj);
						}
						int y_birth = TGenEngine.GetIndependentYear(iRec, "BIRT");
						int y_death = TGenEngine.GetIndependentYear(iRec, "DEAT");
						if (y_birth > -1 && y_death > -1 && y_death < y_birth)
						{
							TfmTreeTools.TCheckObj checkObj = new TfmTreeTools.TCheckObj();
							checkObj.Rec = iRec;
							checkObj.Diag = TfmTreeTools.TCheckDiag.cdLiveYearsInvalid;
							checkObj.Solve = TfmTreeTools.TCheckSolve.csSkip;
							checkObj.Comment = GKL.LSList[584];
							this.FChecksList.Add(checkObj);
						}
						iAge = TGenEngine.GetMarriageAge(iRec);
						if (iAge > 0 && (iAge <= 13 || iAge >= 50))
						{
							TfmTreeTools.TCheckObj checkObj = new TfmTreeTools.TCheckObj();
							checkObj.Rec = iRec;
							checkObj.Diag = TfmTreeTools.TCheckDiag.cdStrangeSpouse;
							checkObj.Solve = TfmTreeTools.TCheckSolve.csSkip;
							checkObj.Comment = string.Format(GKL.LSList[585], new object[]
							{
								iAge.ToString()
							});
							this.FChecksList.Add(checkObj);
						}
						iAge = TGenEngine.GetFirstbornAge(iRec);
						if (iAge > 0 && (iAge <= 13 || iAge >= 50))
						{
							TfmTreeTools.TCheckObj checkObj = new TfmTreeTools.TCheckObj();
							checkObj.Rec = iRec;
							checkObj.Diag = TfmTreeTools.TCheckDiag.cdStrangeParent;
							checkObj.Solve = TfmTreeTools.TCheckSolve.csSkip;
							checkObj.Comment = string.Format(GKL.LSList[586], new object[]
							{
								iAge.ToString()
							});
							this.FChecksList.Add(checkObj);
						}
					}
				}
				this.ListChecks.Items.Clear();

				int num2 = this.FChecksList.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TfmTreeTools.TCheckObj checkObj = this.FChecksList[i] as TfmTreeTools.TCheckObj;
					ListViewItem item = this.ListChecks.AddItem(checkObj.RecName, checkObj);
					item.SubItems.Add(checkObj.Comment);
				}
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
		}

		private void ListChecksDblClick(object sender, EventArgs e)
		{
			TExtListItem item = this.ListChecks.SelectedItem();
			if (item != null)
			{
				TGEDCOMIndividualRecord i_rec = (item.Data as TfmTreeTools.TCheckObj).Rec as TGEDCOMIndividualRecord;
				if (i_rec != null)
				{
					this.Base.SelectRecordByXRef(i_rec.XRef);
					base.Close();
				}
			}
		}

		private void PreparePatriarchsList()
		{
			this.Base.CreateListView(this.Panel3, ref this.ListPatriarchs);
			this.ListPatriarchs.DoubleClick += new EventHandler(this.ListPatriarchsDblClick);
			this.ListPatriarchs.AddListColumn(GKL.LSList[92], 400, false);
			this.ListPatriarchs.AddListColumn(GKL.LSList[321], 90, false);
			this.ListPatriarchs.AddListColumn(GKL.LSList[587], 90, false);
			this.ListPatriarchs.AddListColumn(GKL.LSList[588], 90, false);
		}

		private void ListPatriarchsDblClick(object sender, EventArgs e)
		{
			TExtListItem item = this.ListPatriarchs.SelectedItem();
			if (item != null)
			{
				TGEDCOMIndividualRecord i_rec = item.Data as TGEDCOMIndividualRecord;
				if (i_rec != null)
				{
					this.Base.SelectRecordByXRef(i_rec.XRef);
					base.Close();
				}
			}
		}

		private void PreparePlacesList()
		{
			this.Base.CreateListView(this.Panel4, ref this.ListPlaces);
			this.ListPlaces.DoubleClick += new EventHandler(this.ListPlacesDblClick);
			this.ListPlaces.AddListColumn(GKL.LSList[204], 400, false);
			this.ListPlaces.AddListColumn(GKL.LSList[589], 100, false);
		}

		private void PlacesClear()
		{
			for (int i = this.FPlaces.Count - 1; i >= 0; i--)
			{
				TObjectHelper.Free(this.FPlaces.GetObject(i));
			}
			this.FPlaces.Clear();
		}

		private void CheckPlaces()
		{
			TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[590]);
			this.ListPlaces.BeginUpdate();
			try
			{
				this.PlacesClear();

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TfmProgress.ProgressStep();
					if (this.FTree.GetRecord(i) is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);

						int num2 = iRec.IndividualEvents.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							_CheckPlaces_PrepareEvent(iRec.IndividualEvents[j]);
						}
					}
					else
					{
						if (this.FTree.GetRecord(i) is TGEDCOMFamilyRecord)
						{
							TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)this.FTree.GetRecord(i);

							int num3 = fRec.FamilyEvents.Count - 1;
							for (int j = 0; j <= num3; j++)
							{
								_CheckPlaces_PrepareEvent(fRec.FamilyEvents[j]);
							}
						}
					}
				}
				this.ListPlaces.Items.Clear();

				int num4 = this.FPlaces.Count - 1;
				for (int i = 0; i <= num4; i++)
				{
					TPlaceObj place_obj = this.FPlaces.GetObject(i) as TfmTreeTools.TPlaceObj;
					TExtListItem item = this.ListPlaces.AddItem(this.FPlaces[i], place_obj);
					item.SubItems.Add(place_obj.Facts.Count.ToString());
				}
			}
			finally
			{
				this.ListPlaces.EndUpdate();
				TfmProgress.ProgressDone();
			}
		}

		private void ListPlacesDblClick(object sender, EventArgs e)
		{
			TExtListItem item = this.ListPlaces.SelectedItem();
			if (item != null)
			{
				TfmTreeTools.TPlaceObj p_obj = item.Data as TfmTreeTools.TPlaceObj;
				if (p_obj != null)
				{
					if (SysUtils.Pos("[*]", p_obj.Name) == 1)
					{
						SysUtils.ShowMessage(GKL.LSList[591]);
					}
					else
					{
						TGEDCOMLocationRecord loc = this.Base.SelectRecord(TGEDCOMRecordType.rtLocation, new object[]
						{
							p_obj.Name
						}) as TGEDCOMLocationRecord;
						if (loc != null)
						{
							int num = p_obj.Facts.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMCustomEvent @event = p_obj.Facts[i] as TGEDCOMCustomEvent;
								@event.Detail.Place.StringValue = loc.LocationName;
								@event.Detail.Place.Location.Value = loc;
							}
							this.CheckPlaces();
							this.Base.ListsRefresh(false);
						}
					}
				}
			}
		}

		private void btnFileChoose_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.edCompareFile.Text = this.OpenDialog1.FileName;
				this.TreeCompare(this.FTree, this.edCompareFile.Text);
			}
		}

		private void btnSelectFamily_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TGenEngine.TTreeWalkMode.twmFamily);
		}

		private void btnSelectAncestors_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TGenEngine.TTreeWalkMode.twmAncestors);
		}

		private void btnSelectDescendants_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TGenEngine.TTreeWalkMode.twmDescendants);
		}

		private void btnDelete_Click(object sender, EventArgs e)
		{
			int num = this.FSplitList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				object obj = this.FSplitList[i];
				if (obj is TGEDCOMIndividualRecord)
				{
					this.Base.DeleteIndividualRecord(obj as TGEDCOMIndividualRecord, false);
				}
			}
			SysUtils.ShowMessage(GKL.LSList[578]);
			this.FSplitList.Clear();
			this.UpdateSplitLists();
			this.Base.ListsRefresh(false);
		}

		private void btnSave_Click(object sender, EventArgs e)
		{
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				this.CheckRelations();
				string subm = this.FTree.Header.GetTagStringValue("SUBM");
				this.FTree.Header.Clear();
				this.FTree.Header.Source = "GEDKeeper";
				this.FTree.Header.ReceivingSystemName = "GEDKeeper";
				this.FTree.Header.CharacterSet = GKUI.TfmGEDKeeper.Instance.Options.DefCharacterSet;
				this.FTree.Header.Language = "Russian";
				this.FTree.Header.GEDCOMVersion = "5.5";
				this.FTree.Header.GEDCOMForm = "LINEAGE-LINKED";
				this.FTree.Header.FileName = Path.GetFileName(this.SaveDialog1.FileName);
				this.FTree.Header.TransmissionDate.Date = DateTime.Now;

				if (subm != "")
				{
					this.FTree.Header.SetTagStringValue("SUBM", subm);
				}

				StreamWriter fs = new StreamWriter(this.SaveDialog1.FileName, false, TGEDCOMObject.GetEncodingByCharacterSet(this.FTree.Header.CharacterSet));
				try
				{
					this.FTree.SaveHeaderToStream(fs);
					int num = this.FTree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (this.FSplitList.IndexOf(rec) >= 0)
						{
							rec.SaveToStream(fs);
						}
					}
					this.FTree.SaveFooterToStream(fs);
					this.FTree.Header.CharacterSet = TGEDCOMCharacterSet.csASCII;
				}
				finally
				{
					TObjectHelper.Free(fs);
				}
			}
		}

		private void btnSearch_Click(object sender, EventArgs e)
		{
			this.FRMIndex = 0;
			this.FRMSkip.Clear();
			this.SearchDups();
		}

		private void btnRec1Select_Click(object sender, EventArgs e)
		{
			TMergeMode fRMMode = this.FRMMode;
			TGEDCOMRecordType sm = TGEDCOMRecordType.rtNone;
			if (fRMMode != TMergeMode.mmPerson)
			{
				if (fRMMode == TMergeMode.mmNote)
				{
					sm = TGEDCOMRecordType.rtNote;
				}
			}
			else
			{
				sm = TGEDCOMRecordType.rtIndividual;
			}

			object[] anArgs = new object[0];
			TGEDCOMRecord irec = this.Base.SelectRecord(sm, anArgs);
			if (irec != null)
			{
				this.SetRec1(irec);
			}
		}

		private void btnRec2Select_Click(object sender, EventArgs e)
		{
			TMergeMode fRMMode = this.FRMMode;

			TGEDCOMRecordType sm = TGEDCOMRecordType.rtNone;
			if (fRMMode != TMergeMode.mmPerson)
			{
				if (fRMMode == TMergeMode.mmNote)
				{
					sm = TGEDCOMRecordType.rtNote;
				}
			}
			else
			{
				sm = TGEDCOMRecordType.rtIndividual;
			}

			object[] anArgs = new object[0];
			TGEDCOMRecord irec = this.Base.SelectRecord(sm, anArgs);
			if (irec != null)
			{
				this.SetRec2(irec);
			}
		}

		private void btnMergeToLeft_Click(object sender, EventArgs e)
		{
			this.RecordMerge(this.FRec1, this.FRec2);
			this.SetRec1(this.FRec1);
			this.SetRec2(null);
		}

		private void btnMergeToRight_Click(object sender, EventArgs e)
		{
			this.RecordMerge(this.FRec2, this.FRec1);
			this.SetRec1(null);
			this.SetRec2(this.FRec2);
		}

		private void btnSkip_Click(object sender, EventArgs e)
		{
			if (this.FRec1 != null && this.FRec2 != null)
			{
				this.FRMSkip.Add(this.FRec1.XRef + "-" + this.FRec2.XRef);
			}
			this.SearchDups();
		}

		private void btnImportFileChoose_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog2.ShowDialog() == DialogResult.OK)
			{
				this.edImportFile.Text = this.OpenDialog2.FileName;
				TGKImporter imp = new TGKImporter(this.Base.Engine, this.ListBox1.Items);
				try
				{
					imp.TreeImportEx(this.edImportFile.Text);
				}
				finally
				{
					imp.Free();
				}
				this.ListBox1.SelectedIndex = this.ListBox1.Items.Count - 1;
				this.Base.ListsRefresh(false);
			}
		}

		private void TreeView1_DoubleClick(object sender, EventArgs e)
		{
			TGKTreeNode node = this.TreeView1.SelectedNode as TGKTreeNode;
			if (node != null)
			{
				TGEDCOMIndividualRecord i_rec = node.Data as TGEDCOMIndividualRecord;
				if (i_rec != null)
				{
					this.Base.SelectRecordByXRef(i_rec.XRef);
					base.Close();
				}
			}
		}

		private void btnBaseRepair_Click(object sender, EventArgs e)
		{
			try
			{
				int num = this.ListChecks.Items.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TExtListItem item = this.ListChecks.Items[i] as TExtListItem;
					TCheckObj checkObj = item.Data as TCheckObj;
					if (item.Checked)
					{
						TCheckDiag diag = checkObj.Diag;
						if (diag != TCheckDiag.cdPersonLonglived)
						{
							if (diag == TCheckDiag.cdPersonSexless)
							{
								TGEDCOMIndividualRecord iRec = checkObj.Rec as TGEDCOMIndividualRecord;
								TfmSexCheck.CheckPersonSex(iRec, GKUI.TfmGEDKeeper.Instance.NamesTable);
								this.Base.ChangeRecord(iRec);
							}
						}
						else
						{
							TGEDCOMIndividualRecord iRec = checkObj.Rec as TGEDCOMIndividualRecord;
							TGenEngine.CreateEventEx(this.FTree, iRec, "DEAT", "", "");
							this.Base.ChangeRecord(iRec);
						}
					}
				}
			}
			finally
			{
				this.Base.ListsRefresh(false);
				this.CheckBase();
			}
		}

		private void btnPatSearch_Click(object sender, EventArgs e)
		{
			this.ListPatriarchs.BeginUpdate();
			TObjectList lst = new TObjectList(true);
			try
			{
				this.ListPatriarchs.Items.Clear();
				this.Base.Engine.GetPatriarchsList(true, false, ref lst, decimal.ToInt32(this.edMinGens.Value), !chkWithoutDates.Checked);

				int num = lst.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGenEngine.TPatriarchObj p_obj = lst[i] as TGenEngine.TPatriarchObj;
					string p_sign;
					if (!p_obj.IRec.Patriarch) {
						p_sign = "";
					} else {
						p_sign = "[*] ";
					}

					TExtListItem item = this.ListPatriarchs.AddItem(p_sign + TGenEngine.GetNameStr(p_obj.IRec, true, false), p_obj.IRec);
					item.SubItems.Add(p_obj.IBirthYear.ToString());
					item.SubItems.Add(p_obj.IDescendantsCount.ToString());
					item.SubItems.Add(p_obj.IDescGenerations.ToString());
				}
			}
			finally
			{
				lst.Free();
				this.ListPatriarchs.EndUpdate();
			}
		}

		private void btnSetPatriarch_Click(object sender, EventArgs e)
		{
			try
			{
				TExtListItem item = this.ListPatriarchs.SelectedItem();
				if (item == null)
				{
				}
				else
				{
					TGEDCOMIndividualRecord i_rec = item.Data as TGEDCOMIndividualRecord;
					if (i_rec == null)
					{
					}
					else
					{
						i_rec.Patriarch = true;
					}
				}
			}
			finally
			{
				this.btnPatSearch_Click(null, null);
				this.Base.ListsRefresh(false);
			}
		}

		private void btnIntoList_Click(object sender, EventArgs e)
		{
			this.ListPlacesDblClick(null, null);
		}

		private void btnHelp_Click(object sender, EventArgs e)
		{
			GKUI.TfmGEDKeeper.Instance.ShowHelpTopic(TfmTreeTools.HelpTopics[this.PageControl.TabIndex]);
		}

		private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (this.PageControl.SelectedTab == this.SheetFamilyGroups)
			{
				this.CheckGroups();
			}
			else
			{
				if (this.PageControl.SelectedTab == this.SheetTreeCheck)
				{
					this.CheckBase();
				}
				else
				{
					if (this.PageControl.SelectedTab == this.SheetPlaceManage)
					{
						this.CheckPlaces();
					}
				}
			}
		}

		private void btnUpdateSelect_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.edUpdateBase.Text = this.OpenDialog1.FileName;
				int tmt = 0;
				if (this.RadioButton3.Checked)
				{
					tmt = 0;
				}
				else
				{
					if (this.RadioButton3.Checked)
					{
						tmt = 1;
					}
				}
				if (tmt != 0)
				{
					if (tmt == 1)
					{
						TGenEngine.TreeSync(this.Base.Tree, this.edUpdateBase.Text, this.mSyncRes);
					}
				}
				else
				{
					TGenEngine.TreeMerge(this.Base.Tree, this.edUpdateBase.Text, this.mSyncRes);
				}
				this.Base.ListsRefresh(false);
			}
		}

		private void btnSelectAll_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TGenEngine.TTreeWalkMode.twmAll);
		}

		private void RadioButton3_Click(object sender, EventArgs e)
		{
			this.gbSyncType.Enabled = this.RadioButton4.Checked;
		}

		private void RadioButton8_Click(object sender, EventArgs e)
		{
			if (this.RadioButton5.Checked)
			{
				this.FRMMode = TMergeMode.mmPerson;
			}
			if (this.RadioButton6.Checked)
			{
				this.FRMMode = TMergeMode.mmNote;
			}
			if (this.RadioButton7.Checked)
			{
				this.FRMMode = TMergeMode.mmFamily;
			}
			if (this.RadioButton8.Checked)
			{
				this.FRMMode = TMergeMode.mmSource;
			}

			this.btnRec1Select.Enabled = (this.FRMMode != TMergeMode.mmFamily);
			this.btnRec2Select.Enabled = (this.FRMMode != TMergeMode.mmFamily);
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FChecksList.Free();
				this.PlacesClear();
				this.FPlaces.Free();
				this.FRMSkip.Free();
				this.FSplitList.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmTreeTools(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FTree = this.Base.Tree;
			this.PageControl.SelectedIndex = 0;
			this.FSplitList = new TList();
			this.Memo1 = new TGKHyperView();
			this.Memo1.Location = new Point(8, 56);
			this.Memo1.Size = new Size(329, 248);
			this.SheetMerge.Controls.Add(this.Memo1);
			this.Memo2 = new TGKHyperView();
			this.Memo2.Location = new Point(344, 56);
			this.Memo2.Size = new Size(329, 248);
			this.SheetMerge.Controls.Add(this.Memo2);
			this.FRMSkip = new TStringList();
			this.SetRec1(null);
			this.SetRec2(null);
			this.FRMMode = TfmTreeTools.TMergeMode.mmPerson;
			this.FPlaces = new TStringList();
			this.FPlaces.Sorted = true;
			this.FChecksList = new TObjectList(true);
			this.PrepareChecksList();
			this.PreparePatriarchsList();
			this.PreparePlacesList();
			this.SetLang();
		}

		public void SetLang()
		{
			this.Text = GKL.GetLS(LSID.LSID_MITreeTools);

			this.SheetTreeCompare.Text = GKL.GetLS(LSID.LSID_ToolOp_1);
			this.SheetTreeMerge.Text = GKL.GetLS(LSID.LSID_ToolOp_2);
			this.SheetTreeSplit.Text = GKL.GetLS(LSID.LSID_ToolOp_3);
			this.SheetRecMerge.Text = GKL.GetLS(LSID.LSID_ToolOp_4);
			this.SheetTreeImport.Text = GKL.GetLS(LSID.LSID_ToolOp_5);
			this.SheetFamilyGroups.Text = GKL.GetLS(LSID.LSID_ToolOp_6);
			this.SheetTreeCheck.Text = GKL.GetLS(LSID.LSID_ToolOp_7);
			this.SheetPatSearch.Text = GKL.GetLS(LSID.LSID_ToolOp_8);
			this.SheetPlaceManage.Text = GKL.GetLS(LSID.LSID_ToolOp_9);
			
			//this.SheetMerge.Text
			//this.SheetOptions.Text

			this.btnClose.Text = GKL.LSList[99];
			this.btnHelp.Text = GKL.LSList[5];
			this.Label1.Text = GKL.LSList[0];
			this.btnFileChoose.Text = GKL.LSList[100] + "...";
			this.btnUpdateSelect.Text = GKL.LSList[100] + "...";
			this.btnSelectAll.Text = GKL.LSList[557];
			this.btnSelectFamily.Text = GKL.LSList[558];
			this.btnSelectAncestors.Text = GKL.LSList[559];
			this.btnSelectDescendants.Text = GKL.LSList[560];
			this.btnDelete.Text = GKL.LSList[231];
			this.btnSave.Text = GKL.LSList[9];
			this.SheetMerge.Text = GKL.LSList[561];
			this.SheetOptions.Text = GKL.LSList[39];
			this.btnRec1Select.Text = GKL.LSList[100] + "...";
			this.btnRec2Select.Text = GKL.LSList[100] + "...";
			this.btnSearch.Text = GKL.LSList[562];
			this.btnSkip.Text = GKL.LSList[563];
			this.rgMode.Text = GKL.LSList[564];
			this.RadioButton5.Text = GKL.LSList[52];
			this.RadioButton6.Text = GKL.LSList[54];
			this.RadioButton7.Text = GKL.LSList[53];
			this.RadioButton8.Text = GKL.LSList[56];
			this.GroupBox1.Text = GKL.LSList[565];
			this.rbDirectMatching.Text = GKL.LSList[566];
			this.rbIndistinctMatching.Text = GKL.LSList[567];
			this.chkOnlyNP.Text = GKL.LSList[568];
			this.chkBirthYear.Text = GKL.LSList[569];
			this.Label5.Text = GKL.LSList[570];
			this.Label6.Text = GKL.LSList[571];
			this.Label3.Text = GKL.LSList[0];
			this.btnImportFileChoose.Text = GKL.LSList[100] + "...";
			this.btnBaseRepair.Text = GKL.LSList[572];
			this.Label8.Text = GKL.LSList[573];
			this.btnSetPatriarch.Text = GKL.LSList[574];
			this.btnPatSearch.Text = GKL.LSList[175];
			this.btnIntoList.Text = GKL.LSList[575];
		}

		static TfmTreeTools()
		{
			TfmTreeTools.HelpTopics = new string[]
			{
				"::/gkhTools_TreeCompare.htm", 
				"::/gkhTools_TreeMerge.htm", 
				"::/gkhTools_TreeSplit.htm", 
				"::/gkhTools_DubsMerge.htm", 
				"::/gkhTools_TreeImport.htm", 
				"::/gkhTools_FamiliesConnectivity.htm", 
				"::/gkhTools_TreeCheck.htm", 
				"::/gkhTools_PatSearch.htm", 
				"::/gkhTools_PlacesManage.htm"
			};
		}

		private void _CheckRelations_AddRel(TGEDCOMRecord aRec)
		{
			if (FSplitList.IndexOf(aRec) < 0)
			{
				FSplitList.Add(aRec);
			}
		}

		private void _CheckRelations_CheckRecord(TGEDCOMRecord rec)
		{
			int num = rec.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(rec.MultimediaLinks[i].Value);
			}

			int num2 = rec.Notes.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(rec.Notes[i].Value);
			}

			int num3 = rec.SourceCitations.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_AddRel(rec.SourceCitations[i].Value);
			}
		}

		private void _CheckRelations_CheckTag(TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(tag.MultimediaLinks[i].Value);
			}

			int num2 = tag.Notes.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(tag.Notes[i].Value);
			}

			int num3 = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_AddRel(tag.SourceCitations[i].Value);
			}
		}

		private void _CheckRelations_CheckIndividual(TGEDCOMIndividualRecord iRec)
		{
			_CheckRelations_CheckRecord(iRec);

			int num = iRec.ChildToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(iRec.ChildToFamilyLinks[i].Family);
			}

			int num2 = iRec.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(iRec.SpouseToFamilyLinks[i].Family);
			}

			int num3 = iRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_CheckTag(iRec.IndividualEvents[i].Detail);
			}

			int num4 = iRec.IndividualOrdinances.Count - 1;
			for (int i = 0; i <= num4; i++)
			{
				_CheckRelations_CheckTag(iRec.IndividualOrdinances[i]);
			}

			int num5 = iRec.Submittors.Count - 1;
			for (int i = 0; i <= num5; i++)
			{
				_CheckRelations_AddRel(iRec.Submittors[i].Value);
			}

			int num6 = iRec.Associations.Count - 1;
			for (int i = 0; i <= num6; i++)
			{
				_CheckRelations_AddRel(iRec.Associations[i].Value);
			}

			int num7 = iRec.Aliasses.Count - 1;
			for (int i = 0; i <= num7; i++)
			{
				_CheckRelations_AddRel(iRec.Aliasses[i].Value);
			}

			int num8 = iRec.AncestorsInterest.Count - 1;
			for (int i = 0; i <= num8; i++)
			{
				_CheckRelations_AddRel(iRec.AncestorsInterest[i].Value);
			}

			int num9 = iRec.DescendantsInterest.Count - 1;
			for (int i = 0; i <= num9; i++)
			{
				_CheckRelations_AddRel(iRec.DescendantsInterest[i].Value);
			}

			int num10 = iRec.Groups.Count - 1;
			for (int i = 0; i <= num10; i++)
			{
				_CheckRelations_AddRel(iRec.Groups[i].Value);
			}
		}

		private void _CheckRelations_CheckFamily(TGEDCOMFamilyRecord fRec)
		{
			_CheckRelations_CheckRecord(fRec);

			int num = fRec.FamilyEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_CheckTag(fRec.FamilyEvents[i].Detail);
			}
			_CheckRelations_AddRel(fRec.Submitter.Value);

			int num2 = fRec.SpouseSealings.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_CheckTag(fRec.SpouseSealings[i]);
			}
		}

		private void _CheckRelations_CheckSource(TGEDCOMSourceRecord sRec)
		{
			_CheckRelations_CheckRecord(sRec);

			int num = sRec.RepositoryCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(sRec.RepositoryCitations[i].Value);
			}
		}

		private string _btnPatSearch_Click_GetLinks([In] ref TObjectList lst, TGenEngine.TPatriarchObj pObj)
		{
			string Result = "";
			int num = pObj.ILinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				byte ix = pObj.ILinks[i];
				if (Result != "") Result += ", ";
				Result += TGenEngine.GetNameStr((lst[ix] as TGenEngine.TPatriarchObj).IRec, true, false);
			}
			return Result;
		}

		private void _CheckPlaces_PrepareEvent(TGEDCOMCustomEvent aEvent)
		{
			string place_str = aEvent.Detail.Place.StringValue;
			if (place_str != "")
			{
				TGEDCOMLocationRecord loc = aEvent.Detail.Place.Location.Value as TGEDCOMLocationRecord;
				if (loc != null)
				{
					place_str = "[*] " + place_str;
				}

				int idx = FPlaces.IndexOf(place_str);

				TPlaceObj place_obj;
				if (idx >= 0)
				{
					place_obj = (FPlaces.GetObject(idx) as TPlaceObj);
				}
				else
				{
					place_obj = new TPlaceObj();
					place_obj.Name = place_str;
					FPlaces.AddObject(place_str, place_obj);
				}
				place_obj.Facts.Add(aEvent);
			}
		}
	}
}
