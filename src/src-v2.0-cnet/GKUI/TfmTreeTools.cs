using GedCom551;
using GKCore;
using GKUI.Controls;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmTreeTools : Form
	{
		internal class TCheckObj
		{
			internal string FComment;
			internal TfmTreeTools.TCheckDiag FDiag;
			internal TGEDCOMRecord FRec;
			internal TfmTreeTools.TCheckSolve FSolve;

			[Browsable(false)]
			public string Comment
			{
				get { return this.FComment;	}
				set	{ this.FComment = value; }
			}

			[Browsable(false)]
			public TfmTreeTools.TCheckDiag Diag
			{
				get	{ return this.FDiag; }
				set	{ this.FDiag = value; }
			}

			[Browsable(false)]
			public TGEDCOMRecord Rec
			{
				get	{ return this.FRec;	}
				set	{ this.FRec = value; }
			}

			[Browsable(false)]
			public string RecName
			{
				get	{ return this.GetRecName(); }
			}

			[Browsable(false)]
			public TfmTreeTools.TCheckSolve Solve
			{
				get { return this.FSolve; }
				set	{ this.FSolve = value; }
			}

			internal string GetRecName()
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

		internal class TPlaceObj : IDisposable
		{
			public string Name;
			public TList Facts;
			protected internal bool Disposed_;

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

		internal enum TMergeMode : byte
		{
			mmPerson,
			mmNote,
			mmFamily,
			mmSource
		}
		internal enum TCheckDiag : byte
		{
			cdPersonLonglived,
			cdPersonSexless,
			cdLiveYearsInvalid,
			cdStrangeSpouse,
			cdStrangeParent
		}
		internal enum TCheckSolve : byte
		{
			csSkip,
			csSetIsDead,
			csDefineSex
		}
		internal static readonly string[] HelpTopics;
		private TabControl PageControl;
		private TabPage SheetTreeCompare;
		private TextBox ListCompare;
		private Button btnClose;
		private Label Label1;
		private TextBox edCompareFile;
		private Button btnFileChoose;
		private OpenFileDialog OpenDialog1;
		private TabPage SheetTreeMerge;
		private TabPage SheetTreeSplit;
		private SaveFileDialog SaveDialog1;
		private Button btnSelectAll;
		private ListBox ListSelected;
		private ListBox ListSkipped;
		private Button btnSelectFamily;
		private Button btnSelectAncestors;
		private Button btnSelectDescendants;
		private Button btnDelete;
		private Button btnSave;
		private TabPage SheetRecMerge;
		private TabControl PageControl1;
		private TabPage SheetMerge;
		private Label Lab1;
		private Label Lab2;
		private Button btnSearch;
		private TextBox Edit1;
		private TextBox Edit2;
		private Button btnRec1Select;
		private Button btnRec2Select;
		private Button btnMergeToLeft;
		private Button btnMergeToRight;
		private Button btnSkip;
		private ProgressBar ProgressBar1;
		private TabPage SheetOptions;
		private GroupBox rgMode;
		private GroupBox GroupBox1;
		private Label Label5;
		private Label Label6;
		private RadioButton rbDirectMatching;
		private RadioButton rbIndistinctMatching;
		private NumericUpDown edNameAccuracy;
		private NumericUpDown edYearInaccuracy;
		private CheckBox chkBirthYear;
		private TabPage SheetTreeImport;
		private Label Label3;
		private TextBox edImportFile;
		private Button btnImportFileChoose;
		private ListBox ListBox1;
		private OpenFileDialog OpenDialog2;
		private TabPage SheetFamilyGroups;
		private TreeView TreeView1;
		private TabPage SheetTreeCheck;
		private Button btnBaseRepair;
		private Panel Panel1;
		private Label Label4;
		private TextBox edMasterBase;
		private Label Label7;
		private TextBox edUpdateBase;
		private Button btnUpdateSelect;
		private GroupBox gbSyncType;
		private RadioButton RadioButton1;
		private RadioButton RadioButton2;
		private TextBox mSyncRes;
		private TabPage SheetPatSearch;
		private Button btnPatSearch;
		private Panel Panel3;
		private Label Label8;
		private NumericUpDown edMinGens;
		private TabPage SheetPlaceManage;
		private Panel Panel4;
		private GroupBox rgTreeMergeType;
		private Button btnHelp;
		private Button btnSetPatriarch;
		private CheckBox chkOnlyNP;
		private Button btnIntoList;
		private RadioButton RadioButton3;
		private RadioButton RadioButton4;
		private RadioButton RadioButton5;
		private RadioButton RadioButton6;
		private RadioButton RadioButton7;
		private RadioButton RadioButton8;
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

		[Browsable(false)]
		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		private bool GetIndivName(TGEDCOMIndividualRecord iRec, bool only_np, ref string aName)
		{
			bool Result;
			if (only_np)
			{
				string f = "";
				string i = "";
				string p = "";
				TGenEngine.GetNameParts(iRec, ref f, ref i, ref p);
				aName = i + " " + p;
				string text = aName;
				Result = (((text != null) ? text.Length : 0) > 3);
			}
			else
			{
				TGEDCOMPersonalName np = iRec.GetPersonalName(0);
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
												res = (BDSSystem.WStrCmp(iName, kName) == 0 && this.FRMSkip.IndexOf(iSrc.XRef + "-" + kSrc.XRef) < 0);
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
												res = (BDSSystem.WStrCmp(iName, kName) == 0 && this.FRMSkip.IndexOf(iFam.XRef + "-" + kFam.XRef) < 0);
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
								string iName = iNote.Notes.Text;
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
											string kName = kNote.Notes.Text;
											res = (BDSSystem.WStrCmp(iName, kName) == 0 && this.FRMSkip.IndexOf(iNote.XRef + "-" + kNote.XRef) < 0);
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
											if (this.GetIndivName(kInd, only_np, ref kName) && iInd.Sex == kInd.Sex && this.FRMSkip.IndexOf(iInd.XRef + "-" + kInd.XRef) < 0 && (!only_np || (iInd.Sex == TGEDCOMObject.TGEDCOMSex.svFemale && kInd.Sex == TGEDCOMObject.TGEDCOMSex.svFemale)))
											{
												if (this.rbDirectMatching.Checked)
												{
													res = (BDSSystem.WStrCmp(iName, kName) == 0);
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
				int arg_28_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_28_0;
				if (num >= i)
				{
					num++;
					do
					{
						this.FTree.GetRecord(i).ReplaceXRefs(repMap);
						i++;
					}
					while (i != num);
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
						this.Edit1.Text = (this.FRec1 as TGEDCOMNoteRecord).Notes[0];
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
						this.Edit2.Text = (this.FRec2 as TGEDCOMNoteRecord).Notes[0];
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
			for (int i = 0; i < this.FSplitList.Count; i++)
			{
				TGEDCOMRecord rec = this.FSplitList[i] as TGEDCOMRecord;
				switch (rec.RecordType)
				{
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						TfmTreeTools._CheckRelations_CheckIndividual(this, (TGEDCOMIndividualRecord)rec);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						TfmTreeTools._CheckRelations_CheckFamily(this, (TGEDCOMFamilyRecord)rec);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
					{
						TfmTreeTools._CheckRelations_CheckRecord(this, rec);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						TfmTreeTools._CheckRelations_CheckRecord(this, rec);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						TfmTreeTools._CheckRelations_CheckSource(this, (TGEDCOMSourceRecord)rec);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						TfmTreeTools._CheckRelations_CheckRecord(this, rec);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSubmitter:
					{
						TfmTreeTools._CheckRelations_CheckRecord(this, rec);
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
				int arg_47_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_47_0;
				if (num >= i)
				{
					num++;
					do
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
						i++;
					}
					while (i != num);
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
				int arg_51_0 = 0;
				int num = aMainTree.RecordsCount - 1;
				int i = arg_51_0;
				string fam = "";
				string nam = "";
				string pat = "";
				if (num >= i)
				{
					num++;
					do
					{
						if (aMainTree.GetRecord(i) is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)aMainTree.GetRecord(i);
							int idx = names.AddObject(TGenEngine.GetNameStr(iRec, true, false), new TList());
							(names.GetObject(idx) as TList).Add(iRec);
							TGenEngine.GetNameParts(iRec, ref fam, ref nam, ref pat);
							fams.AddObject(TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMObject.TGEDCOMSex.svFemale), null);
						}
						i++;
					}
					while (i != num);
				}
				int arg_EC_0 = 0;
				int num2 = tempTree.RecordsCount - 1;
				i = arg_EC_0;
				if (num2 >= i)
				{
					num2++;
					do
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
							TGenEngine.GetNameParts(iRec, ref fam, ref nam, ref pat);
							tm = TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMObject.TGEDCOMSex.svFemale);
							idx = fams.IndexOf(tm);
							if (idx >= 0)
							{
								fams.PutObject(idx, 1);
							}
						}
						i++;
					}
					while (i != num2);
				}
				i = fams.Count - 1;
				if (i >= 0)
				{
					do
					{
						if (fams.GetObject(i) == null || BDSSystem.WStrCmp(fams[i], "?") == 0)
						{
							fams.Delete(i);
						}
						i--;
					}
					while (i != -1);
				}
				i = names.Count - 1;
				if (i >= 0)
				{
					do
					{
						if ((names.GetObject(i) as TList).Count == 1)
						{
							(names.GetObject(i) as TList).Free();
							names.Delete(i);
						}
						i--;
					}
					while (i != -1);
				}
				if (fams.Count != 0)
				{
					this.ListCompare.AppendText(GKL.LSList[576] + "\r\n");
					int arg_24A_0 = 0;
					int num3 = fams.Count - 1;
					i = arg_24A_0;
					if (num3 >= i)
					{
						num3++;
						do
						{
							this.ListCompare.AppendText("    " + fams[i] + "\r\n");
							i++;
						}
						while (i != num3);
					}
				}
				if (names.Count != 0)
				{
					this.ListCompare.AppendText(GKL.LSList[577] + "\r\n");
					int arg_2B6_0 = 0;
					int num4 = names.Count - 1;
					i = arg_2B6_0;
					if (num4 >= i)
					{
						num4++;
						do
						{
							this.ListCompare.AppendText("    " + names[i] + "\r\n");
							TList lst = names.GetObject(i) as TList;
							int arg_300_0 = 0;
							int num5 = lst.Count - 1;
							int j = arg_300_0;
							if (num5 >= j)
							{
								num5++;
								do
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
									j++;
								}
								while (j != num5);
							}
							i++;
						}
						while (i != num4);
					}
				}
			}
			finally
			{
				int arg_38E_0 = 0;
				int num6 = names.Count - 1;
				int i = arg_38E_0;
				if (num6 >= i)
				{
					num6++;
					do
					{
						TObjectHelper.Free(names.GetObject(i));
						i++;
					}
					while (i != num6);
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
				int arg_45_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_45_0;
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
							int arg_128_0 = 0;
							int num2 = this.FSplitList.Count - 1;
							int j = arg_128_0;
							if (num2 >= j)
							{
								num2++;
								do
								{
									iRec = (TGEDCOMIndividualRecord)this.FSplitList[j];
									prepared.Add(iRec);
									string pn = TGenEngine.GetNameStr(iRec, true, false);
									if (iRec.Patriarch)
									{
										pn = "(*) " + pn;
									}
									root.Nodes.Add(new TGKTreeNode(pn, iRec));
									j++;
								}
								while (j != num2);
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
				int arg_35_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_35_0;
				if (num >= i)
				{
					num++;
					do
					{
						TfmProgress.ProgressStep();
						if (this.FTree.GetRecord(i) is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);
							int iAge;
							if (TGenEngine.GetIndividualEvent(iRec, "DEAT") == null)
							{
								string age = TGenEngine.GetAge(iRec, -1);
								if (BDSSystem.WStrCmp(age, "") != 0 && BDSSystem.WStrCmp(age, "?") != 0)
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
							TGEDCOMObject.TGEDCOMSex sex = iRec.Sex;
							if (sex < TGEDCOMObject.TGEDCOMSex.svMale || sex >= TGEDCOMObject.TGEDCOMSex.svUndetermined)
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
						i++;
					}
					while (i != num);
				}
				this.ListChecks.Items.Clear();
				int arg_2D6_0 = 0;
				int num2 = this.FChecksList.Count - 1;
				i = arg_2D6_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TfmTreeTools.TCheckObj checkObj = this.FChecksList[i] as TfmTreeTools.TCheckObj;
						ListViewItem item = this.ListChecks.AddItem(checkObj.RecName, checkObj);
						item.SubItems.Add(checkObj.Comment);
						i++;
					}
					while (i != num2);
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
				TGEDCOMIndividualRecord i_rec = (item.Data as TfmTreeTools.TCheckObj).FRec as TGEDCOMIndividualRecord;
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
			int i = this.FPlaces.Count - 1;
			if (i >= 0)
			{
				do
				{
					TObjectHelper.Free(this.FPlaces.GetObject(i));
					i--;
				}
				while (i != -1);
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
				int arg_3C_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_3C_0;
				if (num >= i)
				{
					num++;
					do
					{
						TfmProgress.ProgressStep();
						if (this.FTree.GetRecord(i) is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);
							int arg_7F_0 = 0;
							int num2 = iRec.GetIndividualEventsCount() - 1;
							int j = arg_7F_0;
							if (num2 >= j)
							{
								num2++;
								do
								{
									TfmTreeTools._CheckPlaces_PrepareEvent(this, iRec.GetIndividualEvent(j));
									j++;
								}
								while (j != num2);
							}
						}
						else
						{
							if (this.FTree.GetRecord(i) is TGEDCOMFamilyRecord)
							{
								TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)this.FTree.GetRecord(i);
								int arg_D1_0 = 0;
								int num3 = fRec.GetFamilyEventCount() - 1;
								int j = arg_D1_0;
								if (num3 >= j)
								{
									num3++;
									do
									{
										TfmTreeTools._CheckPlaces_PrepareEvent(this, fRec.GetFamilyEvent(j));
										j++;
									}
									while (j != num3);
								}
							}
						}
						i++;
					}
					while (i != num);
				}
				this.ListPlaces.Items.Clear();
				int arg_120_0 = 0;
				int num4 = this.FPlaces.Count - 1;
				i = arg_120_0;
				if (num4 >= i)
				{
					num4++;
					do
					{
						TfmTreeTools.TPlaceObj place_obj = this.FPlaces.GetObject(i) as TfmTreeTools.TPlaceObj;
						TExtListItem item = this.ListPlaces.AddItem(this.FPlaces[i], place_obj);
						item.SubItems.Add(place_obj.Facts.Count.ToString());
						i++;
					}
					while (i != num4);
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
					if (BDSSystem.Pos("[*]", p_obj.Name) == 1)
					{
						TGKSys.ShowMessage(GKL.LSList[591]);
					}
					else
					{
						TGEDCOMLocationRecord loc = this.Base.SelectRecord(TGEDCOMRecord.TGEDCOMRecordType.rtLocation, new object[]
						{
							p_obj.Name
						}) as TGEDCOMLocationRecord;
						if (loc != null)
						{
							int arg_8D_0 = 0;
							int num = p_obj.Facts.Count - 1;
							int i = arg_8D_0;
							if (num >= i)
							{
								num++;
								do
								{
									TGEDCOMCustomEvent @event = p_obj.Facts[i] as TGEDCOMCustomEvent;
									@event.Detail.Place.StringValue = loc.Name;
									@event.Detail.Place.Location.Value = loc;
									i++;
								}
								while (i != num);
							}
							this.CheckPlaces();
							this.Base.ListsRefresh(false);
						}
					}
				}
			}
		}
		private void InitializeComponent()
		{
			this.PageControl = new TabControl();
			this.SheetTreeCompare = new TabPage();
			this.Label1 = new Label();
			this.ListCompare = new TextBox();
			this.edCompareFile = new TextBox();
			this.btnFileChoose = new Button();
			this.SheetTreeMerge = new TabPage();
			this.Label4 = new Label();
			this.Label7 = new Label();
			this.edMasterBase = new TextBox();
			this.edUpdateBase = new TextBox();
			this.btnUpdateSelect = new Button();
			this.gbSyncType = new GroupBox();
			this.RadioButton1 = new RadioButton();
			this.RadioButton2 = new RadioButton();
			this.mSyncRes = new TextBox();
			this.rgTreeMergeType = new GroupBox();
			this.RadioButton3 = new RadioButton();
			this.RadioButton4 = new RadioButton();
			this.SheetTreeSplit = new TabPage();
			this.btnSelectAll = new Button();
			this.ListSelected = new ListBox();
			this.ListSkipped = new ListBox();
			this.btnSelectFamily = new Button();
			this.btnSelectAncestors = new Button();
			this.btnSelectDescendants = new Button();
			this.btnDelete = new Button();
			this.btnSave = new Button();
			this.SheetTreeImport = new TabPage();
			this.Label3 = new Label();
			this.edImportFile = new TextBox();
			this.btnImportFileChoose = new Button();
			this.ListBox1 = new ListBox();
			this.SheetRecMerge = new TabPage();
			this.PageControl1 = new TabControl();
			this.SheetMerge = new TabPage();
			this.Lab1 = new Label();
			this.Lab2 = new Label();
			this.btnSearch = new Button();
			this.Edit1 = new TextBox();
			this.Edit2 = new TextBox();
			this.btnRec1Select = new Button();
			this.btnRec2Select = new Button();
			this.btnMergeToLeft = new Button();
			this.btnMergeToRight = new Button();
			this.btnSkip = new Button();
			this.ProgressBar1 = new ProgressBar();
			this.SheetOptions = new TabPage();
			this.rgMode = new GroupBox();
			this.RadioButton8 = new RadioButton();
			this.RadioButton7 = new RadioButton();
			this.RadioButton6 = new RadioButton();
			this.RadioButton5 = new RadioButton();
			this.GroupBox1 = new GroupBox();
			this.Label5 = new Label();
			this.Label6 = new Label();
			this.rbDirectMatching = new RadioButton();
			this.rbIndistinctMatching = new RadioButton();
			this.edNameAccuracy = new NumericUpDown();
			this.edYearInaccuracy = new NumericUpDown();
			this.chkBirthYear = new CheckBox();
			this.chkOnlyNP = new CheckBox();
			this.SheetFamilyGroups = new TabPage();
			this.TreeView1 = new TreeView();
			this.SheetTreeCheck = new TabPage();
			this.btnBaseRepair = new Button();
			this.Panel1 = new Panel();
			this.SheetPatSearch = new TabPage();
			this.Label8 = new Label();
			this.btnPatSearch = new Button();
			this.Panel3 = new Panel();
			this.edMinGens = new NumericUpDown();
			this.btnSetPatriarch = new Button();
			this.SheetPlaceManage = new TabPage();
			this.Panel4 = new Panel();
			this.btnIntoList = new Button();
			this.btnClose = new Button();
			this.OpenDialog1 = new OpenFileDialog();
			this.SaveDialog1 = new SaveFileDialog();
			this.OpenDialog2 = new OpenFileDialog();
			this.btnHelp = new Button();
			this.PageControl.SuspendLayout();
			this.SheetTreeCompare.SuspendLayout();
			this.SheetTreeMerge.SuspendLayout();
			this.gbSyncType.SuspendLayout();
			this.rgTreeMergeType.SuspendLayout();
			this.SheetTreeSplit.SuspendLayout();
			this.SheetTreeImport.SuspendLayout();
			this.SheetRecMerge.SuspendLayout();
			this.PageControl1.SuspendLayout();
			this.SheetMerge.SuspendLayout();
			this.SheetOptions.SuspendLayout();
			this.rgMode.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			((ISupportInitialize)this.edNameAccuracy).BeginInit();
			((ISupportInitialize)this.edYearInaccuracy).BeginInit();
			this.SheetFamilyGroups.SuspendLayout();
			this.SheetTreeCheck.SuspendLayout();
			this.SheetPatSearch.SuspendLayout();
			((ISupportInitialize)this.edMinGens).BeginInit();
			this.SheetPlaceManage.SuspendLayout();
			base.SuspendLayout();
			this.PageControl.Controls.Add(this.SheetTreeCompare);
			this.PageControl.Controls.Add(this.SheetTreeMerge);
			this.PageControl.Controls.Add(this.SheetTreeSplit);
			this.PageControl.Controls.Add(this.SheetTreeImport);
			this.PageControl.Controls.Add(this.SheetRecMerge);
			this.PageControl.Controls.Add(this.SheetFamilyGroups);
			this.PageControl.Controls.Add(this.SheetTreeCheck);
			this.PageControl.Controls.Add(this.SheetPatSearch);
			this.PageControl.Controls.Add(this.SheetPlaceManage);
			this.PageControl.Location = new Point(8, 8);
			this.PageControl.Name = "PageControl";
			this.PageControl.SelectedIndex = 0;
			this.PageControl.Size = new Size(721, 449);
			this.PageControl.TabIndex = 0;
			this.PageControl.SelectedIndexChanged += new EventHandler(this.PageControl_SelectedIndexChanged);
			this.SheetTreeCompare.Controls.Add(this.Label1);
			this.SheetTreeCompare.Controls.Add(this.ListCompare);
			this.SheetTreeCompare.Controls.Add(this.edCompareFile);
			this.SheetTreeCompare.Controls.Add(this.btnFileChoose);
			this.SheetTreeCompare.Location = new Point(4, 22);
			this.SheetTreeCompare.Name = "SheetTreeCompare";
			this.SheetTreeCompare.Size = new Size(713, 423);
			this.SheetTreeCompare.TabIndex = 0;
			this.SheetTreeCompare.Text = "Сравнить базы данных";
			this.Label1.Location = new Point(8, 16);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(35, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Файл";
			this.ListCompare.Location = new Point(8, 40);
			this.ListCompare.Multiline = true;
			this.ListCompare.Name = "ListCompare";
			this.ListCompare.ReadOnly = true;
			this.ListCompare.Size = new Size(697, 376);
			this.ListCompare.TabIndex = 0;
			this.ListCompare.Text = "";
			this.edCompareFile.Location = new Point(48, 8);
			this.edCompareFile.Name = "edCompareFile";
			this.edCompareFile.ReadOnly = true;
			this.edCompareFile.Size = new Size(568, 21);
			this.edCompareFile.TabIndex = 1;
			this.edCompareFile.Text = "";
			this.btnFileChoose.Location = new Point(624, 6);
			this.btnFileChoose.Name = "btnFileChoose";
			this.btnFileChoose.Size = new Size(81, 25);
			this.btnFileChoose.TabIndex = 2;
			this.btnFileChoose.Text = "Выбрать...";
			this.btnFileChoose.Click += new EventHandler(this.btnFileChoose_Click);
			this.SheetTreeMerge.Controls.Add(this.Label4);
			this.SheetTreeMerge.Controls.Add(this.Label7);
			this.SheetTreeMerge.Controls.Add(this.edMasterBase);
			this.SheetTreeMerge.Controls.Add(this.edUpdateBase);
			this.SheetTreeMerge.Controls.Add(this.btnUpdateSelect);
			this.SheetTreeMerge.Controls.Add(this.gbSyncType);
			this.SheetTreeMerge.Controls.Add(this.mSyncRes);
			this.SheetTreeMerge.Controls.Add(this.rgTreeMergeType);
			this.SheetTreeMerge.Location = new Point(4, 22);
			this.SheetTreeMerge.Name = "SheetTreeMerge";
			this.SheetTreeMerge.Size = new Size(713, 423);
			this.SheetTreeMerge.TabIndex = 1;
			this.SheetTreeMerge.Text = "Объединить базы данных";
			this.Label4.Location = new Point(8, 8);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(80, 13);
			this.Label4.TabIndex = 0;
			this.Label4.Text = "Мастер-база";
			this.Label7.Location = new Point(8, 56);
			this.Label7.Name = "Label7";
			this.Label7.Size = new Size(100, 13);
			this.Label7.TabIndex = 1;
			this.Label7.Text = "Обновление базы";
			this.edMasterBase.BackColor = SystemColors.Control;
			this.edMasterBase.Location = new Point(8, 24);
			this.edMasterBase.Name = "edMasterBase";
			this.edMasterBase.ReadOnly = true;
			this.edMasterBase.Size = new Size(609, 21);
			this.edMasterBase.TabIndex = 0;
			this.edMasterBase.Text = "[текущая база данных]";
			this.edUpdateBase.Location = new Point(8, 72);
			this.edUpdateBase.Name = "edUpdateBase";
			this.edUpdateBase.ReadOnly = true;
			this.edUpdateBase.Size = new Size(609, 21);
			this.edUpdateBase.TabIndex = 1;
			this.edUpdateBase.Text = "";
			this.btnUpdateSelect.Location = new Point(624, 70);
			this.btnUpdateSelect.Name = "btnUpdateSelect";
			this.btnUpdateSelect.Size = new Size(81, 25);
			this.btnUpdateSelect.TabIndex = 2;
			this.btnUpdateSelect.Text = "Выбрать...";
			this.btnUpdateSelect.Click += new EventHandler(this.btnUpdateSelect_Click);
			this.gbSyncType.Controls.Add(this.RadioButton1);
			this.gbSyncType.Controls.Add(this.RadioButton2);
			this.gbSyncType.Enabled = false;
			this.gbSyncType.Location = new Point(376, 104);
			this.gbSyncType.Name = "gbSyncType";
			this.gbSyncType.Size = new Size(329, 65);
			this.gbSyncType.TabIndex = 3;
			this.gbSyncType.TabStop = false;
			this.gbSyncType.Text = "Синхронизация";
			this.RadioButton1.Checked = true;
			this.RadioButton1.Location = new Point(16, 16);
			this.RadioButton1.Name = "RadioButton1";
			this.RadioButton1.Size = new Size(289, 17);
			this.RadioButton1.TabIndex = 0;
			this.RadioButton1.TabStop = true;
			this.RadioButton1.Text = "Доверенный источник (безусловная синхронизация)";
			this.RadioButton2.Enabled = false;
			this.RadioButton2.Location = new Point(16, 40);
			this.RadioButton2.Name = "RadioButton2";
			this.RadioButton2.Size = new Size(289, 17);
			this.RadioButton2.TabIndex = 1;
			this.RadioButton2.Text = "Проверка всех элементов баз данных";
			this.mSyncRes.Location = new Point(8, 176);
			this.mSyncRes.Multiline = true;
			this.mSyncRes.Name = "mSyncRes";
			this.mSyncRes.ReadOnly = true;
			this.mSyncRes.Size = new Size(697, 240);
			this.mSyncRes.TabIndex = 4;
			this.mSyncRes.Text = "";
			this.rgTreeMergeType.Controls.Add(this.RadioButton3);
			this.rgTreeMergeType.Controls.Add(this.RadioButton4);
			this.rgTreeMergeType.Location = new Point(8, 104);
			this.rgTreeMergeType.Name = "rgTreeMergeType";
			this.rgTreeMergeType.Size = new Size(361, 65);
			this.rgTreeMergeType.TabIndex = 5;
			this.rgTreeMergeType.TabStop = false;
			this.rgTreeMergeType.Text = "Тип объединения";
			this.RadioButton3.Checked = true;
			this.RadioButton3.Location = new Point(16, 16);
			this.RadioButton3.Name = "RadioButton3";
			this.RadioButton3.Size = new Size(328, 17);
			this.RadioButton3.TabIndex = 2;
			this.RadioButton3.TabStop = true;
			this.RadioButton3.Text = "Простое слияние данных";
			this.RadioButton3.Click += new EventHandler(this.RadioButton3_Click);
			this.RadioButton4.Location = new Point(16, 40);
			this.RadioButton4.Name = "RadioButton4";
			this.RadioButton4.Size = new Size(328, 17);
			this.RadioButton4.TabIndex = 3;
			this.RadioButton4.Text = "Синхронизация (незавершено, только для тестирования)";
			this.RadioButton4.Click += new EventHandler(this.RadioButton3_Click);
			this.SheetTreeSplit.Controls.Add(this.btnSelectAll);
			this.SheetTreeSplit.Controls.Add(this.ListSelected);
			this.SheetTreeSplit.Controls.Add(this.ListSkipped);
			this.SheetTreeSplit.Controls.Add(this.btnSelectFamily);
			this.SheetTreeSplit.Controls.Add(this.btnSelectAncestors);
			this.SheetTreeSplit.Controls.Add(this.btnSelectDescendants);
			this.SheetTreeSplit.Controls.Add(this.btnDelete);
			this.SheetTreeSplit.Controls.Add(this.btnSave);
			this.SheetTreeSplit.Location = new Point(4, 22);
			this.SheetTreeSplit.Name = "SheetTreeSplit";
			this.SheetTreeSplit.Size = new Size(713, 423);
			this.SheetTreeSplit.TabIndex = 2;
			this.SheetTreeSplit.Text = "Разделить базу данных";
			this.btnSelectAll.Location = new Point(8, 352);
			this.btnSelectAll.Name = "btnSelectAll";
			this.btnSelectAll.Size = new Size(120, 25);
			this.btnSelectAll.TabIndex = 0;
			this.btnSelectAll.Text = "Выбрать все связи";
			this.btnSelectAll.Click += new EventHandler(this.btnSelectAll_Click);
			this.ListSelected.Location = new Point(8, 8);
			this.ListSelected.Name = "ListSelected";
			this.ListSelected.Size = new Size(345, 329);
			this.ListSelected.TabIndex = 1;
			this.ListSkipped.Location = new Point(360, 8);
			this.ListSkipped.Name = "ListSkipped";
			this.ListSkipped.Size = new Size(345, 329);
			this.ListSkipped.TabIndex = 2;
			this.btnSelectFamily.Location = new Point(136, 352);
			this.btnSelectFamily.Name = "btnSelectFamily";
			this.btnSelectFamily.Size = new Size(120, 25);
			this.btnSelectFamily.TabIndex = 3;
			this.btnSelectFamily.Text = "Выбрать семью";
			this.btnSelectFamily.Click += new EventHandler(this.btnSelectFamily_Click);
			this.btnSelectAncestors.Location = new Point(8, 384);
			this.btnSelectAncestors.Name = "btnSelectAncestors";
			this.btnSelectAncestors.Size = new Size(120, 25);
			this.btnSelectAncestors.TabIndex = 4;
			this.btnSelectAncestors.Text = "Выбрать предков";
			this.btnSelectAncestors.Click += new EventHandler(this.btnSelectAncestors_Click);
			this.btnSelectDescendants.Location = new Point(136, 384);
			this.btnSelectDescendants.Name = "btnSelectDescendants";
			this.btnSelectDescendants.Size = new Size(120, 25);
			this.btnSelectDescendants.TabIndex = 5;
			this.btnSelectDescendants.Text = "Выбрать потомков";
			this.btnSelectDescendants.Click += new EventHandler(this.btnSelectDescendants_Click);
			this.btnDelete.Location = new Point(600, 352);
			this.btnDelete.Name = "btnDelete";
			this.btnDelete.Size = new Size(105, 25);
			this.btnDelete.TabIndex = 6;
			this.btnDelete.Text = "Удалить";
			this.btnDelete.Click += new EventHandler(this.btnDelete_Click);
			this.btnSave.Location = new Point(600, 384);
			this.btnSave.Name = "btnSave";
			this.btnSave.Size = new Size(105, 25);
			this.btnSave.TabIndex = 7;
			this.btnSave.Text = "Сохранить...";
			this.btnSave.Click += new EventHandler(this.btnSave_Click);
			this.SheetTreeImport.Controls.Add(this.Label3);
			this.SheetTreeImport.Controls.Add(this.edImportFile);
			this.SheetTreeImport.Controls.Add(this.btnImportFileChoose);
			this.SheetTreeImport.Controls.Add(this.ListBox1);
			this.SheetTreeImport.Location = new Point(4, 22);
			this.SheetTreeImport.Name = "SheetTreeImport";
			this.SheetTreeImport.Size = new Size(713, 423);
			this.SheetTreeImport.TabIndex = 4;
			this.SheetTreeImport.Text = "Импорт росписей";
			this.Label3.Location = new Point(8, 16);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(31, 13);
			this.Label3.TabIndex = 0;
			this.Label3.Text = "Файл";
			this.edImportFile.Location = new Point(40, 8);
			this.edImportFile.Name = "edImportFile";
			this.edImportFile.ReadOnly = true;
			this.edImportFile.Size = new Size(577, 21);
			this.edImportFile.TabIndex = 0;
			this.edImportFile.Text = "";
			this.btnImportFileChoose.Location = new Point(624, 6);
			this.btnImportFileChoose.Name = "btnImportFileChoose";
			this.btnImportFileChoose.Size = new Size(81, 25);
			this.btnImportFileChoose.TabIndex = 1;
			this.btnImportFileChoose.Text = "Выбрать...";
			this.btnImportFileChoose.Click += new EventHandler(this.btnImportFileChoose_Click);
			this.ListBox1.Location = new Point(8, 40);
			this.ListBox1.Name = "ListBox1";
			this.ListBox1.Size = new Size(697, 368);
			this.ListBox1.TabIndex = 2;
			this.SheetRecMerge.Controls.Add(this.PageControl1);
			this.SheetRecMerge.Location = new Point(4, 22);
			this.SheetRecMerge.Name = "SheetRecMerge";
			this.SheetRecMerge.Size = new Size(713, 423);
			this.SheetRecMerge.TabIndex = 3;
			this.SheetRecMerge.Text = "Объединить дубликаты";
			this.PageControl1.Controls.Add(this.SheetMerge);
			this.PageControl1.Controls.Add(this.SheetOptions);
			this.PageControl1.Location = new Point(8, 8);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new Size(689, 393);
			this.PageControl1.TabIndex = 0;
			this.SheetMerge.Controls.Add(this.Lab1);
			this.SheetMerge.Controls.Add(this.Lab2);
			this.SheetMerge.Controls.Add(this.btnSearch);
			this.SheetMerge.Controls.Add(this.Edit1);
			this.SheetMerge.Controls.Add(this.Edit2);
			this.SheetMerge.Controls.Add(this.btnRec1Select);
			this.SheetMerge.Controls.Add(this.btnRec2Select);
			this.SheetMerge.Controls.Add(this.btnMergeToLeft);
			this.SheetMerge.Controls.Add(this.btnMergeToRight);
			this.SheetMerge.Controls.Add(this.btnSkip);
			this.SheetMerge.Controls.Add(this.ProgressBar1);
			this.SheetMerge.Location = new Point(4, 22);
			this.SheetMerge.Name = "SheetMerge";
			this.SheetMerge.Size = new Size(681, 367);
			this.SheetMerge.TabIndex = 0;
			this.SheetMerge.Text = "Объединение";
			this.Lab1.Location = new Point(8, 8);
			this.Lab1.Name = "Lab1";
			this.Lab1.Size = new Size(24, 13);
			this.Lab1.TabIndex = 0;
			this.Lab1.Text = "XXX1";
			this.Lab2.Location = new Point(344, 8);
			this.Lab2.Name = "Lab2";
			this.Lab2.Size = new Size(24, 13);
			this.Lab2.TabIndex = 1;
			this.Lab2.Text = "XXX2";
			this.btnSearch.Location = new Point(8, 312);
			this.btnSearch.Name = "btnSearch";
			this.btnSearch.Size = new Size(75, 25);
			this.btnSearch.TabIndex = 0;
			this.btnSearch.Text = "Автопоиск";
			this.btnSearch.Click += new EventHandler(this.btnSearch_Click);
			this.Edit1.Location = new Point(8, 24);
			this.Edit1.Name = "Edit1";
			this.Edit1.ReadOnly = true;
			this.Edit1.Size = new Size(241, 21);
			this.Edit1.TabIndex = 1;
			this.Edit1.Text = "";
			this.Edit2.Location = new Point(344, 24);
			this.Edit2.Name = "Edit2";
			this.Edit2.ReadOnly = true;
			this.Edit2.Size = new Size(241, 21);
			this.Edit2.TabIndex = 2;
			this.Edit2.Text = "";
			this.btnRec1Select.Location = new Point(256, 22);
			this.btnRec1Select.Name = "btnRec1Select";
			this.btnRec1Select.Size = new Size(81, 25);
			this.btnRec1Select.TabIndex = 3;
			this.btnRec1Select.Text = "Выбрать...";
			this.btnRec1Select.Click += new EventHandler(this.btnRec1Select_Click);
			this.btnRec2Select.Location = new Point(592, 22);
			this.btnRec2Select.Name = "btnRec2Select";
			this.btnRec2Select.Size = new Size(81, 25);
			this.btnRec2Select.TabIndex = 4;
			this.btnRec2Select.Text = "Выбрать...";
			this.btnRec2Select.Click += new EventHandler(this.btnRec2Select_Click);
			this.btnMergeToLeft.Location = new Point(256, 312);
			this.btnMergeToLeft.Name = "btnMergeToLeft";
			this.btnMergeToLeft.Size = new Size(81, 25);
			this.btnMergeToLeft.TabIndex = 7;
			this.btnMergeToLeft.Text = "<<<";
			this.btnMergeToLeft.Click += new EventHandler(this.btnMergeToLeft_Click);
			this.btnMergeToRight.Location = new Point(344, 312);
			this.btnMergeToRight.Name = "btnMergeToRight";
			this.btnMergeToRight.Size = new Size(81, 25);
			this.btnMergeToRight.TabIndex = 8;
			this.btnMergeToRight.Text = ">>>";
			this.btnMergeToRight.Click += new EventHandler(this.btnMergeToRight_Click);
			this.btnSkip.Location = new Point(88, 312);
			this.btnSkip.Name = "btnSkip";
			this.btnSkip.Size = new Size(75, 25);
			this.btnSkip.TabIndex = 9;
			this.btnSkip.Text = "Пропустить";
			this.btnSkip.Click += new EventHandler(this.btnSkip_Click);
			this.ProgressBar1.Location = new Point(8, 344);
			this.ProgressBar1.Name = "ProgressBar1";
			this.ProgressBar1.Size = new Size(665, 16);
			this.ProgressBar1.Step = 1;
			this.ProgressBar1.TabIndex = 10;
			this.SheetOptions.Controls.Add(this.rgMode);
			this.SheetOptions.Controls.Add(this.GroupBox1);
			this.SheetOptions.Location = new Point(4, 22);
			this.SheetOptions.Name = "SheetOptions";
			this.SheetOptions.Size = new Size(681, 367);
			this.SheetOptions.TabIndex = 1;
			this.SheetOptions.Text = "Настройки";
			this.rgMode.Controls.Add(this.RadioButton8);
			this.rgMode.Controls.Add(this.RadioButton7);
			this.rgMode.Controls.Add(this.RadioButton6);
			this.rgMode.Controls.Add(this.RadioButton5);
			this.rgMode.Location = new Point(8, 8);
			this.rgMode.Name = "rgMode";
			this.rgMode.Size = new Size(225, 97);
			this.rgMode.TabIndex = 0;
			this.rgMode.TabStop = false;
			this.rgMode.Text = "Записи";
			this.RadioButton8.Location = new Point(16, 72);
			this.RadioButton8.Name = "RadioButton8";
			this.RadioButton8.Size = new Size(192, 16);
			this.RadioButton8.TabIndex = 3;
			this.RadioButton8.Text = "Источники";
			this.RadioButton8.Click += new EventHandler(this.RadioButton8_Click);
			this.RadioButton7.Location = new Point(16, 56);
			this.RadioButton7.Name = "RadioButton7";
			this.RadioButton7.Size = new Size(192, 16);
			this.RadioButton7.TabIndex = 2;
			this.RadioButton7.Text = "Семьи";
			this.RadioButton7.Click += new EventHandler(this.RadioButton8_Click);
			this.RadioButton6.Location = new Point(16, 40);
			this.RadioButton6.Name = "RadioButton6";
			this.RadioButton6.Size = new Size(192, 16);
			this.RadioButton6.TabIndex = 1;
			this.RadioButton6.Text = "Заметки";
			this.RadioButton6.Click += new EventHandler(this.RadioButton8_Click);
			this.RadioButton5.Location = new Point(16, 24);
			this.RadioButton5.Name = "RadioButton5";
			this.RadioButton5.Size = new Size(192, 16);
			this.RadioButton5.TabIndex = 0;
			this.RadioButton5.Text = "Персоны";
			this.RadioButton5.Click += new EventHandler(this.RadioButton8_Click);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.rbDirectMatching);
			this.GroupBox1.Controls.Add(this.rbIndistinctMatching);
			this.GroupBox1.Controls.Add(this.edNameAccuracy);
			this.GroupBox1.Controls.Add(this.edYearInaccuracy);
			this.GroupBox1.Controls.Add(this.chkBirthYear);
			this.GroupBox1.Controls.Add(this.chkOnlyNP);
			this.GroupBox1.Location = new Point(8, 112);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(297, 161);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Поиск персон";
			this.Label5.Location = new Point(8, 104);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(98, 13);
			this.Label5.TabIndex = 0;
			this.Label5.Text = "Точность имени, %";
			this.Label6.Location = new Point(120, 104);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(88, 13);
			this.Label6.TabIndex = 1;
			this.Label6.Text = "Погрешность лет";
			this.rbDirectMatching.Checked = true;
			this.rbDirectMatching.Location = new Point(8, 16);
			this.rbDirectMatching.Name = "rbDirectMatching";
			this.rbDirectMatching.Size = new Size(153, 17);
			this.rbDirectMatching.TabIndex = 0;
			this.rbDirectMatching.TabStop = true;
			this.rbDirectMatching.Text = "Прямое сравнение";
			this.rbIndistinctMatching.Location = new Point(8, 32);
			this.rbIndistinctMatching.Name = "rbIndistinctMatching";
			this.rbIndistinctMatching.Size = new Size(153, 17);
			this.rbIndistinctMatching.TabIndex = 1;
			this.rbIndistinctMatching.Text = "Нечеткое сравнение";
			this.edNameAccuracy.Location = new Point(8, 120);
			this.edNameAccuracy.Name = "edNameAccuracy";
			this.edNameAccuracy.Size = new Size(89, 21);
			this.edNameAccuracy.TabIndex = 2;
			NumericUpDown arg_211A_0 = this.edNameAccuracy;
			int[] array = null;
			int[] array2 = array;
			int[] array3;
			int[] expr_20E2 = array3 = new int[4];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 4)
				{
					num = 4;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_20E2;
			array[0] = 40;
			array[1] = 0;
			array[2] = 0;
			array[3] = 0;
			arg_211A_0.Value = new decimal(array);
			this.edYearInaccuracy.Location = new Point(120, 120);
			this.edYearInaccuracy.Name = "edYearInaccuracy";
			this.edYearInaccuracy.Size = new Size(89, 21);
			this.edYearInaccuracy.TabIndex = 4;
			NumericUpDown arg_21AE_0 = this.edYearInaccuracy;
			int[] array4 = null;
			int[] array5 = array4;
			int[] array6;
			int[] expr_2174 = array6 = new int[4];
			if (array5 != null)
			{
				int num2;
				if ((num2 = array5.Length) > 4)
				{
					num2 = 4;
				}
				if (num2 > 0)
				{
					Array.Copy(array5, array6, num2);
				}
			}
			array4 = expr_2174;
			array4[0] = 3;
			array4[1] = 0;
			array4[2] = 0;
			array4[3] = 0;
			arg_21AE_0.Value = new decimal(array4);
			this.chkBirthYear.Location = new Point(8, 80);
			this.chkBirthYear.Name = "chkBirthYear";
			this.chkBirthYear.Size = new Size(265, 17);
			this.chkBirthYear.TabIndex = 6;
			this.chkBirthYear.Text = "Учитывать год рождения";
			this.chkOnlyNP.Location = new Point(8, 56);
			this.chkOnlyNP.Name = "chkOnlyNP";
			this.chkOnlyNP.Size = new Size(265, 17);
			this.chkOnlyNP.TabIndex = 7;
			this.chkOnlyNP.Text = "Только по имени/отчеству (только женщины)";
			this.SheetFamilyGroups.Controls.Add(this.TreeView1);
			this.SheetFamilyGroups.Location = new Point(4, 22);
			this.SheetFamilyGroups.Name = "SheetFamilyGroups";
			this.SheetFamilyGroups.Size = new Size(713, 423);
			this.SheetFamilyGroups.TabIndex = 5;
			this.SheetFamilyGroups.Text = "Проверка связности семей";
			this.TreeView1.ImageIndex = -1;
			this.TreeView1.Location = new Point(8, 8);
			this.TreeView1.Name = "TreeView1";
			this.TreeView1.SelectedImageIndex = -1;
			this.TreeView1.Size = new Size(697, 401);
			this.TreeView1.TabIndex = 0;
			this.TreeView1.DoubleClick += new EventHandler(this.TreeView1_DoubleClick);
			this.SheetTreeCheck.Controls.Add(this.btnBaseRepair);
			this.SheetTreeCheck.Controls.Add(this.Panel1);
			this.SheetTreeCheck.Location = new Point(4, 22);
			this.SheetTreeCheck.Name = "SheetTreeCheck";
			this.SheetTreeCheck.Size = new Size(713, 423);
			this.SheetTreeCheck.TabIndex = 6;
			this.SheetTreeCheck.Text = "Проверка базы данных";
			this.btnBaseRepair.Location = new Point(560, 382);
			this.btnBaseRepair.Name = "btnBaseRepair";
			this.btnBaseRepair.Size = new Size(145, 25);
			this.btnBaseRepair.TabIndex = 0;
			this.btnBaseRepair.Text = "Исправить";
			this.btnBaseRepair.Click += new EventHandler(this.btnBaseRepair_Click);
			this.Panel1.Location = new Point(0, 0);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new Size(713, 369);
			this.Panel1.TabIndex = 1;
			this.SheetPatSearch.Controls.Add(this.Label8);
			this.SheetPatSearch.Controls.Add(this.btnPatSearch);
			this.SheetPatSearch.Controls.Add(this.Panel3);
			this.SheetPatSearch.Controls.Add(this.edMinGens);
			this.SheetPatSearch.Controls.Add(this.btnSetPatriarch);
			this.SheetPatSearch.Location = new Point(4, 22);
			this.SheetPatSearch.Name = "SheetPatSearch";
			this.SheetPatSearch.Size = new Size(713, 423);
			this.SheetPatSearch.TabIndex = 7;
			this.SheetPatSearch.Text = "Поиск патриархов";
			this.Label8.Location = new Point(8, 392);
			this.Label8.Name = "Label8";
			this.Label8.Size = new Size(166, 13);
			this.Label8.TabIndex = 0;
			this.Label8.Text = "Поколений потомков не менее";
			this.btnPatSearch.Location = new Point(632, 384);
			this.btnPatSearch.Name = "btnPatSearch";
			this.btnPatSearch.Size = new Size(75, 25);
			this.btnPatSearch.TabIndex = 0;
			this.btnPatSearch.Text = "Поиск";
			this.btnPatSearch.Click += new EventHandler(this.btnPatSearch_Click);
			this.Panel3.Location = new Point(0, 0);
			this.Panel3.Name = "Panel3";
			this.Panel3.Size = new Size(713, 369);
			this.Panel3.TabIndex = 1;
			this.edMinGens.Location = new Point(184, 384);
			this.edMinGens.Name = "edMinGens";
			this.edMinGens.Size = new Size(57, 21);
			this.edMinGens.TabIndex = 2;
			NumericUpDown arg_26F4_0 = this.edMinGens;
			int[] array7 = null;
			int[] array8 = array7;
			int[] array9;
			int[] expr_26BA = array9 = new int[4];
			if (array8 != null)
			{
				int num3;
				if ((num3 = array8.Length) > 4)
				{
					num3 = 4;
				}
				if (num3 > 0)
				{
					Array.Copy(array8, array9, num3);
				}
			}
			array7 = expr_26BA;
			array7[0] = 2;
			array7[1] = 0;
			array7[2] = 0;
			array7[3] = 0;
			arg_26F4_0.Value = new decimal(array7);
			this.btnSetPatriarch.Location = new Point(503, 384);
			this.btnSetPatriarch.Name = "btnSetPatriarch";
			this.btnSetPatriarch.Size = new Size(123, 25);
			this.btnSetPatriarch.TabIndex = 4;
			this.btnSetPatriarch.Text = "Установить признак";
			this.btnSetPatriarch.Click += new EventHandler(this.btnSetPatriarch_Click);
			this.SheetPlaceManage.Controls.Add(this.Panel4);
			this.SheetPlaceManage.Controls.Add(this.btnIntoList);
			this.SheetPlaceManage.Location = new Point(4, 22);
			this.SheetPlaceManage.Name = "SheetPlaceManage";
			this.SheetPlaceManage.Size = new Size(713, 423);
			this.SheetPlaceManage.TabIndex = 8;
			this.SheetPlaceManage.Text = "Управление местами";
			this.Panel4.Location = new Point(0, 0);
			this.Panel4.Name = "Panel4";
			this.Panel4.Size = new Size(713, 369);
			this.Panel4.TabIndex = 0;
			this.btnIntoList.Location = new Point(8, 384);
			this.btnIntoList.Name = "btnIntoList";
			this.btnIntoList.Size = new Size(128, 25);
			this.btnIntoList.TabIndex = 1;
			this.btnIntoList.Text = "Внести в справочник";
			this.btnIntoList.Click += new EventHandler(this.btnIntoList_Click);
			this.btnClose.DialogResult = DialogResult.Cancel;
			this.btnClose.Location = new Point(648, 480);
			this.btnClose.Name = "btnClose";
			this.btnClose.Size = new Size(81, 25);
			this.btnClose.TabIndex = 1;
			this.btnClose.Text = "Закрыть";
			this.OpenDialog1.Filter = "GEDCOM|*.ged|Все файлы (*.*)|*.*";
			this.SaveDialog1.DefaultExt = "ged";
			this.SaveDialog1.Filter = "GEDCOM|*.ged";
			this.OpenDialog2.Filter = "Все поддерживаемые форматы (*.txt, *.csv, *.doc, *.xls)|*.txt;*.csv;*.doc;*.xls|Роспись в txt-формате (*.txt)|*.txt|Роспись в csv-формате (*.csv)|*.csv|Роспись в формате Word (*.doc)|*.doc|Роспись в формате Excel (*.xls)|*.xls";
			this.btnHelp.Location = new Point(552, 480);
			this.btnHelp.Name = "btnHelp";
			this.btnHelp.Size = new Size(81, 25);
			this.btnHelp.TabIndex = 2;
			this.btnHelp.Text = "Справка";
			this.btnHelp.Click += new EventHandler(this.btnHelp_Click);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnClose;
			base.ClientSize = new Size(737, 513);
			base.Controls.Add(this.PageControl);
			base.Controls.Add(this.btnClose);
			base.Controls.Add(this.btnHelp);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.KeyPreview = true;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmTreeTools";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Инструменты";
			this.PageControl.ResumeLayout(false);
			this.SheetTreeCompare.ResumeLayout(false);
			this.SheetTreeMerge.ResumeLayout(false);
			this.gbSyncType.ResumeLayout(false);
			this.rgTreeMergeType.ResumeLayout(false);
			this.SheetTreeSplit.ResumeLayout(false);
			this.SheetTreeImport.ResumeLayout(false);
			this.SheetRecMerge.ResumeLayout(false);
			this.PageControl1.ResumeLayout(false);
			this.SheetMerge.ResumeLayout(false);
			this.SheetOptions.ResumeLayout(false);
			this.rgMode.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			((ISupportInitialize)this.edNameAccuracy).EndInit();
			((ISupportInitialize)this.edYearInaccuracy).EndInit();
			this.SheetFamilyGroups.ResumeLayout(false);
			this.SheetTreeCheck.ResumeLayout(false);
			this.SheetPatSearch.ResumeLayout(false);
			((ISupportInitialize)this.edMinGens).EndInit();
			this.SheetPlaceManage.ResumeLayout(false);
			base.ResumeLayout(false);
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
			int arg_0F_0 = 0;
			int num = this.FSplitList.Count - 1;
			int i = arg_0F_0;
			if (num >= i)
			{
				num++;
				do
				{
					object obj = this.FSplitList[i];
					if (obj is TGEDCOMIndividualRecord)
					{
						this.Base.DeleteIndividualRecord(obj as TGEDCOMIndividualRecord, false);
					}
					i++;
				}
				while (i != num);
			}
			TGKSys.ShowMessage(GKL.LSList[578]);
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
				this.FTree.Header.CharacterSet = GKL.fmGEDKeeper.Options.DefCharacterSet;
				this.FTree.Header.Language = "Russian";
				this.FTree.Header.GEDCOMVersion = "5.5";
				this.FTree.Header.GEDCOMForm = "LINEAGE-LINKED";
				this.FTree.Header.FileName = Path.GetFileName(this.SaveDialog1.FileName);
				this.FTree.Header.TransmissionDate.Date = DateTime.Now;
				if (BDSSystem.WStrCmp(subm, "") != 0)
				{
					this.FTree.Header.SetTagStringValue("SUBM", subm);
				}
				StreamWriter fs = new StreamWriter(this.SaveDialog1.FileName, false, Encoding.GetEncoding(1251));
				try
				{
					this.FTree.SaveHeaderToStream(fs);
					int arg_15C_0 = 0;
					int num = this.FTree.RecordsCount - 1;
					int i = arg_15C_0;
					if (num >= i)
					{
						num++;
						do
						{
							TGEDCOMRecord rec = this.FTree.GetRecord(i);
							if (this.FSplitList.IndexOf(rec) >= 0)
							{
								rec.SaveToStream(fs);
							}
							i++;
						}
						while (i != num);
					}
					this.FTree.SaveFooterToStream(fs);
					this.FTree.Header.CharacterSet = TGEDCOMObject.TGEDCOMCharacterSet.csASCII;
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
			TfmTreeTools.TMergeMode fRMMode = this.FRMMode;
			TGEDCOMRecord.TGEDCOMRecordType sm = TGEDCOMRecord.TGEDCOMRecordType.rtNone;
			if (fRMMode != TfmTreeTools.TMergeMode.mmPerson)
			{
				if (fRMMode == TfmTreeTools.TMergeMode.mmNote)
				{
					sm = TGEDCOMRecord.TGEDCOMRecordType.rtNote;
				}
			}
			else
			{
				sm = TGEDCOMRecord.TGEDCOMRecordType.rtIndividual;
			}
			TfmBase arg_25_0 = this.Base;
			TGEDCOMRecord.TGEDCOMRecordType arg_25_1 = sm;
			object[] anArgs = new object[0];
			TGEDCOMRecord irec = arg_25_0.SelectRecord(arg_25_1, anArgs);
			if (irec != null)
			{
				this.SetRec1(irec);
			}
		}
		private void btnRec2Select_Click(object sender, EventArgs e)
		{
			TfmTreeTools.TMergeMode fRMMode = this.FRMMode;
			TGEDCOMRecord.TGEDCOMRecordType sm = TGEDCOMRecord.TGEDCOMRecordType.rtNone;
			if (fRMMode != TfmTreeTools.TMergeMode.mmPerson)
			{
				if (fRMMode == TfmTreeTools.TMergeMode.mmNote)
				{
					sm = TGEDCOMRecord.TGEDCOMRecordType.rtNote;
				}
			}
			else
			{
				sm = TGEDCOMRecord.TGEDCOMRecordType.rtIndividual;
			}
			TfmBase arg_25_0 = this.Base;
			TGEDCOMRecord.TGEDCOMRecordType arg_25_1 = sm;
			object[] anArgs = new object[0];
			TGEDCOMRecord irec = arg_25_0.SelectRecord(arg_25_1, anArgs);
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
				int arg_14_0 = 0;
				int num = this.ListChecks.Items.Count - 1;
				int i = arg_14_0;
				if (num >= i)
				{
					num++;
					do
					{
						TExtListItem item = this.ListChecks.Items[i] as TExtListItem;
						TfmTreeTools.TCheckObj checkObj = item.Data as TfmTreeTools.TCheckObj;
						if (item.Checked)
						{
							TfmTreeTools.TCheckDiag diag = checkObj.Diag;
							if (diag != TfmTreeTools.TCheckDiag.cdPersonLonglived)
							{
								if (diag == TfmTreeTools.TCheckDiag.cdPersonSexless)
								{
									TGEDCOMIndividualRecord iRec = checkObj.Rec as TGEDCOMIndividualRecord;
									TfmSexCheck.CheckPersonSex(iRec, GKL.fmGEDKeeper.NamesTable);
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
						i++;
					}
					while (i != num);
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
				this.Base.Engine.GetPatriarchsList(true, false, ref lst, decimal.ToInt32(this.edMinGens.Value));

				for (int i = 0; i <= lst.Count - 1; i++)
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
			GKL.fmGEDKeeper.ShowHelpTopic(TfmTreeTools.HelpTopics[this.PageControl.TabIndex]);
		}
		private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (object.Equals(this.PageControl.SelectedTab, this.SheetFamilyGroups))
			{
				this.CheckGroups();
			}
			else
			{
				if (object.Equals(this.PageControl.SelectedTab, this.SheetTreeCheck))
				{
					this.CheckBase();
				}
				else
				{
					if (object.Equals(this.PageControl.SelectedTab, this.SheetPlaceManage))
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
				this.FRMMode = TfmTreeTools.TMergeMode.mmPerson;
			}
			if (this.RadioButton6.Checked)
			{
				this.FRMMode = TfmTreeTools.TMergeMode.mmNote;
			}
			if (this.RadioButton7.Checked)
			{
				this.FRMMode = TfmTreeTools.TMergeMode.mmFamily;
			}
			if (this.RadioButton8.Checked)
			{
				this.FRMMode = TfmTreeTools.TMergeMode.mmSource;
			}
			this.btnRec1Select.Enabled = (this.FRMMode != TfmTreeTools.TMergeMode.mmFamily);
			this.btnRec2Select.Enabled = (this.FRMMode != TfmTreeTools.TMergeMode.mmFamily);
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

		private static void _CheckRelations_AddRel([In] TfmTreeTools Self, TGEDCOMRecord aRec)
		{
			if (Self.FSplitList.IndexOf(aRec) < 0)
			{
				Self.FSplitList.Add(aRec);
			}
		}

		private static void _CheckRelations_CheckRecord([In] TfmTreeTools Self, TGEDCOMRecord rec)
		{
			int arg_0A_0 = 0;
			int num = rec.GetMultimediaLinksCount() - 1;
			int i = arg_0A_0;
			if (num >= i)
			{
				num++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, rec.GetMultimediaLink(i).Value);
					i++;
				}
				while (i != num);
			}
			int arg_37_0 = 0;
			int num2 = rec.GetNotesCount() - 1;
			i = arg_37_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, rec.GetNote(i).Value);
					i++;
				}
				while (i != num2);
			}
			int arg_64_0 = 0;
			int num3 = rec.GetSourceCitationsCount() - 1;
			i = arg_64_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, rec.GetSourceCitation(i).Value);
					i++;
				}
				while (i != num3);
			}
		}

		private static void _CheckRelations_CheckTag([In] TfmTreeTools Self, TGEDCOMTagWithLists tag)
		{
			int arg_0A_0 = 0;
			int num = tag.GetMultimediaLinksCount() - 1;
			int i = arg_0A_0;
			if (num >= i)
			{
				num++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, tag.GetMultimediaLink(i).Value);
					i++;
				}
				while (i != num);
			}
			int arg_37_0 = 0;
			int num2 = tag.GetNotesCount() - 1;
			i = arg_37_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, tag.GetNote(i).Value);
					i++;
				}
				while (i != num2);
			}
			int arg_64_0 = 0;
			int num3 = tag.GetSourceCitationsCount() - 1;
			i = arg_64_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, tag.GetSourceCitation(i).Value);
					i++;
				}
				while (i != num3);
			}
		}

		private static void _CheckRelations_CheckIndividual([In] TfmTreeTools Self, TGEDCOMIndividualRecord iRec)
		{
			TfmTreeTools._CheckRelations_CheckRecord(Self, iRec);
			int arg_11_0 = 0;
			int num = iRec.ChildToFamilyLinksCount - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetChildToFamilyLink(i).Family);
					i++;
				}
				while (i != num);
			}
			int arg_3E_0 = 0;
			int num2 = iRec.SpouseToFamilyLinksCount - 1;
			i = arg_3E_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetSpouseToFamilyLink(i).Family);
					i++;
				}
				while (i != num2);
			}
			int arg_6B_0 = 0;
			int num3 = iRec.IndividualEventsCount - 1;
			i = arg_6B_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TfmTreeTools._CheckRelations_CheckTag(Self, iRec.GetIndividualEvent(i).Detail);
					i++;
				}
				while (i != num3);
			}
			int arg_99_0 = 0;
			int num4 = iRec.IndividualOrdinancesCount - 1;
			i = arg_99_0;
			if (num4 >= i)
			{
				num4++;
				do
				{
					TfmTreeTools._CheckRelations_CheckTag(Self, iRec.GetIndividualOrdinance(i));
					i++;
				}
				while (i != num4);
			}
			int arg_C6_0 = 0;
			int num5 = iRec.SubmittorsCount - 1;
			i = arg_C6_0;
			if (num5 >= i)
			{
				num5++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetSubmittor(i).Value);
					i++;
				}
				while (i != num5);
			}
			int arg_F8_0 = 0;
			int num6 = iRec.AssociationsCount - 1;
			i = arg_F8_0;
			if (num6 >= i)
			{
				num6++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetAssociation(i).Value);
					i++;
				}
				while (i != num6);
			}
			int arg_12A_0 = 0;
			int num7 = iRec.AliassesCount - 1;
			i = arg_12A_0;
			if (num7 >= i)
			{
				num7++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetAlias(i).Value);
					i++;
				}
				while (i != num7);
			}
			int arg_15C_0 = 0;
			int num8 = iRec.AncestorsInterestCount - 1;
			i = arg_15C_0;
			if (num8 >= i)
			{
				num8++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetAncestorsInterest(i).Value);
					i++;
				}
				while (i != num8);
			}
			int arg_18E_0 = 0;
			int num9 = iRec.DescendantsInterestCount - 1;
			i = arg_18E_0;
			if (num9 >= i)
			{
				num9++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetDescendantsInterest(i).Value);
					i++;
				}
				while (i != num9);
			}
			int arg_1C0_0 = 0;
			int num10 = iRec.GroupsCount - 1;
			i = arg_1C0_0;
			if (num10 >= i)
			{
				num10++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, iRec.GetGroup(i).Value);
					i++;
				}
				while (i != num10);
			}
		}

		private static void _CheckRelations_CheckFamily([In] TfmTreeTools Self, TGEDCOMFamilyRecord fRec)
		{
			TfmTreeTools._CheckRelations_CheckRecord(Self, fRec);
			int arg_11_0 = 0;
			int num = fRec.GetFamilyEventCount() - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				do
				{
					TfmTreeTools._CheckRelations_CheckTag(Self, fRec.GetFamilyEvent(i).Detail);
					i++;
				}
				while (i != num);
			}
			TfmTreeTools._CheckRelations_AddRel(Self, fRec.Submitter.Value);
			int arg_4F_0 = 0;
			int num2 = fRec.SpouseSealingCount - 1;
			i = arg_4F_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					TfmTreeTools._CheckRelations_CheckTag(Self, fRec.GetSpouseSealing(i));
					i++;
				}
				while (i != num2);
			}
		}
		private static void _CheckRelations_CheckSource([In] TfmTreeTools Self, TGEDCOMSourceRecord sRec)
		{
			TfmTreeTools._CheckRelations_CheckRecord(Self, sRec);
			int arg_11_0 = 0;
			int num = sRec.GetRepositoryCitationsCount() - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				do
				{
					TfmTreeTools._CheckRelations_AddRel(Self, sRec.GetRepositoryCitation(i).Value);
					i++;
				}
				while (i != num);
			}
		}

		private static string _btnPatSearch_Click_GetLinks([In] ref TObjectList lst, TGenEngine.TPatriarchObj pObj)
		{
			string Result = "";
			int num = lst.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					if (BDSSystem.SetTest(pObj.ILinks, i, 32))
					{
						if (Result != "") Result += ", ";
						Result += TGenEngine.GetNameStr((lst[i] as TGenEngine.TPatriarchObj).IRec, true, false);
					}
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		private static void _CheckPlaces_PrepareEvent([In] TfmTreeTools Self, TGEDCOMCustomEvent aEvent)
		{
			string place_str = aEvent.Detail.Place.StringValue;
			if (BDSSystem.WStrCmp(place_str, "") != 0)
			{
				TGEDCOMLocationRecord loc = aEvent.Detail.Place.Location.Value as TGEDCOMLocationRecord;
				if (loc != null)
				{
					place_str = "[*] " + place_str;
				}
				int idx = Self.FPlaces.IndexOf(place_str);
				TfmTreeTools.TPlaceObj place_obj;
				if (idx >= 0)
				{
					place_obj = (Self.FPlaces.GetObject(idx) as TfmTreeTools.TPlaceObj);
				}
				else
				{
					place_obj = new TfmTreeTools.TPlaceObj();
					place_obj.Name = place_str;
					Self.FPlaces.AddObject(place_str, place_obj);
				}
				place_obj.Facts.Add(aEvent);
			}
		}
	}
}
