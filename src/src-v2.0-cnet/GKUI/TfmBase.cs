using GedCom551;
using GKCore;
using GKCore.Commands;
using GKSys;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Lists;
using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmBase : Form, ILocalization
	{
		[StructLayout(LayoutKind.Auto)]
		public struct TRecCount
		{
			public int Total;
			public int Filtered;
		}

		public enum TFilePropertiesMode : byte
		{
			fpmAuthor, fpmDiags, fpmAdvanced
		}

		public enum TRecNotify : byte
		{
			rnDelete
		}

		private ImageList ImageList1;
		private IContainer components;
		private TBackManager FBackman;
		private TList[] FChangedRecords = new TList[14];
		private TGenEngine FEngine;
		private TList FLockedRecords;
		private bool FModified;
		private TGenEngine.TShieldState FShieldState;
		private TGEDCOMTree FTree;
		private TUndoManager FUndoman;
		private TPersonsFilter FXFilter;
		public TabControl PageRecords;
		public TRecordsView ListPersons;
		public TGKHyperView mPersonSummary;
		public TRecordsView ListFamilies;
		public TGKHyperView mFamilySummary;
		public TRecordsView ListNotes;
		public TGKHyperView mNoteSummary;
		public TRecordsView ListMultimedia;
		public TGKHyperView mMediaSummary;
		public TRecordsView ListSources;
		public TGKHyperView mSourceSummary;
		public TRecordsView ListRepositories;
		public TGKHyperView mRepositorySummary;
		public TRecordsView ListGroups;
		public TGKHyperView mGroupSummary;
		public TRecordsView ListResearches;
		public TGKHyperView mResearchSummary;
		public TRecordsView ListTasks;
		public TGKHyperView mTaskSummary;
		public TRecordsView ListCommunications;
		public TGKHyperView mCommunicationSummary;
		public TRecordsView ListLocations;
		public TGKHyperView mLocationSummary;
		public TfmBase.TRecCount[] FCounts = new TfmBase.TRecCount[14];

		public TBackManager Backman
		{
			get { return this.FBackman; }
		}

		public TGenEngine Engine
		{
			get { return this.FEngine; }
		}

		public string FileName
		{
			get { return this.GetFileName(); }
			set { this.SetFileName(value); }
		}

		public TPersonsFilter Filter
		{
			get { return this.FXFilter; }
		}

		public bool Modified
		{
			get { return this.FModified; }
			set { this.SetModified(value); }
		}

		public TGenEngine.TShieldState ShieldState
		{
			get { return this.FShieldState; }
			set { this.SetShieldState(value); }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
		}

		public TUndoManager Undoman
		{
			get { return this.FUndoman; }
		}

		private void OutLink(TGEDCOMRecord aSubject, TStrings aToList, TGEDCOMRecord aRec, TGEDCOMTag aTag, TGEDCOMPointer aExt)
		{
			string prefix;
			if (aSubject is TGEDCOMSourceRecord && aExt != null)
			{
				if (BDSSystem.WStrCmp((aExt as TGEDCOMSourceCitation).Page, "") != 0)
				{
					prefix = (aExt as TGEDCOMSourceCitation).Page + ": ";
				}
				else
				{
					prefix = "";
				}
			}
			else
			{
				prefix = "";
			}
			string suffix;
			if (aTag != null && aTag is TGEDCOMCustomEvent)
			{
				suffix = ", " + TGenEngine.GetEventName(aTag as TGEDCOMCustomEvent).ToLower();
			}
			else
			{
				suffix = "";
			}
			aToList.Add("    " + prefix + TGenEngine.GenRecordLink(this.FTree, aRec, true) + suffix);
		}
		private void PrepareEvent(TGEDCOMRecord aSubject, TStrings aToList, TGEDCOMRecord aRec, TGEDCOMCustomEvent @event)
		{
			if (aSubject is TGEDCOMNoteRecord)
			{
				int arg_18_0 = 0;
				int num = @event.Detail.GetNotesCount() - 1;
				int i = arg_18_0;
				if (num >= i)
				{
					num++;
					do
					{
						if (object.Equals(@event.Detail.GetNote(i).Value, aSubject))
						{
							this.OutLink(aSubject, aToList, aRec, @event, null);
						}
						i++;
					}
					while (i != num);
				}
			}
			else
			{
				if (aSubject is TGEDCOMMultimediaRecord)
				{
					int arg_6F_0 = 0;
					int num2 = @event.Detail.GetMultimediaLinksCount() - 1;
					int i = arg_6F_0;
					if (num2 >= i)
					{
						num2++;
						do
						{
							if (object.Equals(@event.Detail.GetMultimediaLink(i).Value, aSubject))
							{
								this.OutLink(aSubject, aToList, aRec, @event, null);
							}
							i++;
						}
						while (i != num2);
					}
				}
				else
				{
					if (aSubject is TGEDCOMSourceRecord)
					{
						int arg_C3_0 = 0;
						int num3 = @event.Detail.GetSourceCitationsCount() - 1;
						int i = arg_C3_0;
						if (num3 >= i)
						{
							num3++;
							do
							{
								if (object.Equals(@event.Detail.GetSourceCitation(i).Value, aSubject))
								{
									this.OutLink(aSubject, aToList, aRec, @event, @event.Detail.GetSourceCitation(i));
								}
								i++;
							}
							while (i != num3);
						}
					}
				}
			}
		}

		private TGEDCOMFamilyRecord GetFamilyBySpouse(TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord Result = null;

			int num = this.FTree.RecordsCount - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				TGEDCOMFamilyRecord fam;
				while (true)
				{
					if (this.FTree.GetRecord(i) is TGEDCOMFamilyRecord)
					{
						fam = (TGEDCOMFamilyRecord)this.FTree.GetRecord(i);
						TGEDCOMIndividualRecord husb = fam.Husband.Value as TGEDCOMIndividualRecord;
						TGEDCOMIndividualRecord wife = fam.Wife.Value as TGEDCOMIndividualRecord;
						if (object.Equals(husb, aNewParent) || object.Equals(wife, aNewParent))
						{
							string msg = string.Format(GKL.LSList[70], new object[]
							{
								TGenEngine.GetFamilyStr(fam)
							});
							if (TGKSys.ShowQuestion(msg) == DialogResult.Yes)
							{
								break;
							}
						}
					}
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = fam;
			}
			return Result;
		}

		private void IntUpdate(TRecordsView aRecView, int ASCol, bool aTitles)
		{
			TGEDCOMRecord rec = aRecView.GetSelectedRecord();
			aRecView.UpdateContents(this.FShieldState, aTitles, this.FXFilter, ASCol);
			if (rec != null)
			{
				aRecView.SelectItemByRec(rec);
			}
			this.FCounts[(int)aRecView.RecordType].Total = aRecView.TotalCount;
			this.FCounts[(int)aRecView.RecordType].Filtered = aRecView.FilteredCount;
		}

		private bool IsMainList(TGEDCOMRecord.TGEDCOMRecordType aRecType, TGKListView aList)
		{
			bool Result = false;
			switch (aRecType)
			{
				case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
				{
					Result = object.Equals(aList, this.ListPersons);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
				{
					Result = object.Equals(aList, this.ListFamilies);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
				{
					Result = object.Equals(aList, this.ListNotes);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
				{
					Result = object.Equals(aList, this.ListMultimedia);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
				{
					Result = object.Equals(aList, this.ListSources);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
				{
					Result = object.Equals(aList, this.ListRepositories);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
				{
					Result = object.Equals(aList, this.ListGroups);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
				{
					Result = object.Equals(aList, this.ListResearches);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
				{
					Result = object.Equals(aList, this.ListTasks);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
				{
					Result = object.Equals(aList, this.ListCommunications);
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
				{
					Result = object.Equals(aList, this.ListLocations);
					break;
				}
			}
			return Result;
		}

		private void List_SelectedIndexChanged(object sender, EventArgs e)
		{
			TGEDCOMRecord data = (sender as TRecordsView).GetSelectedRecord();
			if (data != null)
			{
				this.NavAdd(data);
			}
			this.ShowRecordInfo(data);
		}

		private void NavAdd(TGEDCOMRecord aRec)
		{
			if (aRec != null && !this.FBackman.Busy)
			{
				this.FBackman.Current = aRec;
				GKL.fmGEDKeeper.UpdateControls(false);
			}
		}

		private void SetFileName([In] string Value)
		{
			this.FEngine.FileName = Value;
			this.SetMainTitle();
			GKL.fmGEDKeeper.Options.LastDir = VCLUtils.ExtractFilePath(this.FEngine.FileName);
		}

		private void SetMainTitle()
		{
			this.Text = Path.GetFileName(this.FileName);
			if (this.FModified)
			{
				this.Text = "* " + this.Text;
			}
		}

		private void SetModified([In] bool Value)
		{
			this.FModified = Value;
			this.SetMainTitle();
		}

		private void ShowAddress(TGEDCOMAddress anAddress, TStrings aSummary)
		{
			if (!anAddress.IsEmpty() && aSummary != null)
			{
				aSummary.Add("    " + GKL.LSList[82] + ":");

				string ts = "";
				if (anAddress.AddressCountry != "")
				{
					ts = ts + anAddress.AddressCountry + ", ";
				}
				if (anAddress.AddressState != "")
				{
					ts = ts + anAddress.AddressState + ", ";
				}
				if (anAddress.AddressCity != "")
				{
					ts += anAddress.AddressCity;
				}
				if (ts != "")
				{
					aSummary.Add("    " + ts);
				}

				ts = "";
				if (anAddress.AddressPostalCode != "")
				{
					ts = ts + anAddress.AddressPostalCode + ", ";
				}
				if (anAddress.Address.Text.Trim() != "")
				{
					ts += anAddress.Address.Text.Trim();
				}
				if (ts != "")
				{
					aSummary.Add("    " + ts);
				}

				int num = anAddress.GetPhoneNumbersCount() - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						aSummary.Add("    " + anAddress.GetPhoneNumber(i));
						i++;
					}
					while (i != num);
				}

				int num2 = anAddress.GetEmailAddressesCount() - 1;
				i = 0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						aSummary.Add("    " + anAddress.GetEmailAddress(i));
						i++;
					}
					while (i != num2);
				}

				int num3 = anAddress.GetWebPagesCount() - 1;
				i = 0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						aSummary.Add("    " + anAddress.GetWebPage(i));
						i++;
					}
					while (i != num3);
				}
			}
		}

		private void SetShieldState([In] TGenEngine.TShieldState Value)
		{
			bool up = (this.FShieldState != TGenEngine.TShieldState.ssNone && Value == TGenEngine.TShieldState.ssNone) || (this.FShieldState == TGenEngine.TShieldState.ssNone && Value != TGenEngine.TShieldState.ssNone);
			this.FShieldState = Value;
			if (up)
			{
				this.ListsRefresh(false);
			}
		}

		private string GetFileName()
		{
			return this.FEngine.FileName;
		}

		private void mPersonSummaryLink(object sender, string LinkName)
		{
			if (BDSSystem.Pos("view_", LinkName) > 0)
			{
				string xref = LinkName.Remove(0, 5);
				TGEDCOMRecord rec = this.FTree.XRefIndex_Find(xref);
				if (rec != null)
				{
					this.ShowMedia(rec as TGEDCOMMultimediaRecord);
				}
			}
			else
			{
				this.SelectRecordByXRef(LinkName);
			}
		}
		private void FormActivate(object sender, EventArgs e)
		{
			GKL.fmGEDKeeper.UpdateControls(false);
			if (GKL.fmGEDKeeper.fmTimeLine != null)
			{
				GKL.fmGEDKeeper.fmTimeLine.CheckTimeWin(this);
			}
		}
		private void FormDeactivate(object sender, EventArgs e)
		{
			GKL.fmGEDKeeper.UpdateControls(true);
			if (GKL.fmGEDKeeper.fmTimeLine != null)
			{
				GKL.fmGEDKeeper.fmTimeLine.CheckTimeWin(null);
			}
		}
		private void InitializeComponent()
		{
			this.components = new Container();
			this.ImageList1 = new ImageList(this.components);
			this.PageRecords = new TabControl();
			base.SuspendLayout();
			this.ImageList1.ImageSize = new Size(16, 16);
			this.ImageList1.TransparentColor = Color.Transparent;
			this.PageRecords.Dock = DockStyle.Fill;
			this.PageRecords.Location = new Point(0, 0);
			this.PageRecords.Name = "PageRecords";
			this.PageRecords.SelectedIndex = 0;
			this.PageRecords.Size = new Size(762, 290);
			this.PageRecords.TabIndex = 0;
			this.PageRecords.SelectedIndexChanged += new EventHandler(this.PageRecords_SelectedIndexChanged);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(762, 290);
			base.Controls.Add(this.PageRecords);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.Name = "TfmBase";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "GEDKeeper";
			base.Closing += new CancelEventHandler(this.TfmBase_Closing);
			base.Activated += new EventHandler(this.FormActivate);
			base.Deactivate += new EventHandler(this.FormDeactivate);
			base.ResumeLayout(false);
		}
		private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
		{
			GKL.fmGEDKeeper.UpdateControls(false);
		}
		private void TfmBase_Closing(object sender, CancelEventArgs e)
		{
			if (!this.CheckModified())
			{
				e.Cancel = true;
			}
			else
			{
				e.Cancel = false;
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FBackman.Dispose();
				this.FUndoman.Dispose();
				this.FLockedRecords.Free();
				this.FXFilter.Free();
				this.FTree = null;
				this.FEngine.Dispose();
				for (TGEDCOMRecord.TGEDCOMRecordType rt = TGEDCOMRecord.TGEDCOMRecordType.rtNone; rt != TGEDCOMRecord.TGEDCOMRecordType.rtLast; rt++)
				{
					this.FChangedRecords[(int)rt].Free();
				}
			}
			base.Dispose(Disposing);
		}

		public TfmBase()
		{
			this.InitializeComponent();
			for (TGEDCOMRecord.TGEDCOMRecordType rt = TGEDCOMRecord.TGEDCOMRecordType.rtNone; rt != TGEDCOMRecord.TGEDCOMRecordType.rtLast; rt++)
			{
				this.FChangedRecords[(int)rt] = new TList();
			}
			this.FEngine = new TGenEngine();
			this.FTree = this.FEngine.Tree;
			this.FXFilter = new TPersonsFilter();
			this.FLockedRecords = new TList();
			this.FBackman = new TBackManager();
			this.FUndoman = new TUndoManager(this.FTree, TUndoManager.TUndoManType.manualCommit);
			this.CreatePage(GKL.LSList[52], TGEDCOMRecord.TGEDCOMRecordType.rtIndividual, ref this.ListPersons, ref this.mPersonSummary);
			this.CreatePage(GKL.LSList[53], TGEDCOMRecord.TGEDCOMRecordType.rtFamily, ref this.ListFamilies, ref this.mFamilySummary);
			this.CreatePage(GKL.LSList[54], TGEDCOMRecord.TGEDCOMRecordType.rtNote, ref this.ListNotes, ref this.mNoteSummary);
			this.CreatePage(GKL.LSList[55], TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia, ref this.ListMultimedia, ref this.mMediaSummary);
			this.CreatePage(GKL.LSList[56], TGEDCOMRecord.TGEDCOMRecordType.rtSource, ref this.ListSources, ref this.mSourceSummary);
			this.CreatePage(GKL.LSList[57], TGEDCOMRecord.TGEDCOMRecordType.rtRepository, ref this.ListRepositories, ref this.mRepositorySummary);
			this.CreatePage(GKL.LSList[58], TGEDCOMRecord.TGEDCOMRecordType.rtGroup, ref this.ListGroups, ref this.mGroupSummary);
			this.CreatePage(GKL.LSList[59], TGEDCOMRecord.TGEDCOMRecordType.rtResearch, ref this.ListResearches, ref this.mResearchSummary);
			this.CreatePage(GKL.LSList[60], TGEDCOMRecord.TGEDCOMRecordType.rtTask, ref this.ListTasks, ref this.mTaskSummary);
			this.CreatePage(GKL.LSList[61], TGEDCOMRecord.TGEDCOMRecordType.rtCommunication, ref this.ListCommunications, ref this.mCommunicationSummary);
			this.CreatePage(GKL.LSList[62], TGEDCOMRecord.TGEDCOMRecordType.rtLocation, ref this.ListLocations, ref this.mLocationSummary);
			this.PageRecords.SelectedIndex = 0;
		}

		public void ApplyFilter()
		{
			if (this.FTree.RecordsCount > 0)
			{
				this.ListsRefresh(false);
			}
		}
		public void ChangeRecord(TGEDCOMRecord aRecord)
		{
			TGEDCOMRecord.TGEDCOMRecordType rt = aRecord.RecordType;
			this.FChangedRecords[(int)rt].Add(aRecord);
			aRecord.ChangeDate.ChangeDateTime = DateTime.Now;
			this.Modified = true;
		}

		public void ChangesClear()
		{
			for (TGEDCOMRecord.TGEDCOMRecordType rt = TGEDCOMRecord.TGEDCOMRecordType.rtNone; rt != TGEDCOMRecord.TGEDCOMRecordType.rtLast; rt++)
			{
				this.FChangedRecords[(int)rt].Clear();
			}
		}

		public bool CheckModified()
		{
			bool Result = true;
			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(GKL.LSList[69], "GEDKeeper", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
				if (dialogResult != DialogResult.Cancel)
				{
					if (dialogResult == DialogResult.Yes)
					{
						GKL.fmGEDKeeper.miFileSaveClick(null, null);
					}
				}
				else
				{
					Result = false;
				}
			}
			return Result;
		}

		public void Clear()
		{
			this.FTree.Clear();
			this.FBackman.Clear();
			this.FUndoman.Clear();
		}

		public void CreateListView(Control aOwner, ref TGKListView aList)
		{
			aList = new TGKListView(null);
			aList.HideSelection = false;
			aList.LabelEdit = false;
			aList.FullRowSelect = true;
			aList.View = View.Details;
			aList.Dock = DockStyle.Fill;
			aOwner.Controls.Add(aList);
		}

		public void CreatePage(string aPageText, TGEDCOMRecord.TGEDCOMRecordType aRecType, ref TRecordsView aList, ref TGKHyperView aSummary)
		{
			this.PageRecords.SuspendLayout();
			TabPage sheet = new TabPage();
			sheet.Text = aPageText;
			this.PageRecords.Controls.Add(sheet);
			this.PageRecords.ResumeLayout(false);
			aSummary = new TGKHyperView();
			Splitter spl = new Splitter();
			aSummary.BorderWidth = 4;
			aSummary.Dock = DockStyle.Right;
			aSummary.Size = new Size(300, 290);
			aSummary.set_OnLink(new TGKHyperView.TLinkEvent(this.mPersonSummaryLink));
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;
			sheet.Controls.Add(aSummary);
			sheet.Controls.Add(spl);
			this.CreateRecordsView(sheet, aRecType, ref aList);
			aList.IsMainList = this.IsMainList(aRecType, aList);
			aList.DoubleClick += new EventHandler(this.RecordEdit);
			aList.SelectedIndexChanged += new EventHandler(this.List_SelectedIndexChanged);
			aList.UpdateTitles();
			sheet.Controls.SetChildIndex(spl, 1);
			sheet.Controls.SetChildIndex(aSummary, 2);
		}

		public TGEDCOMIndividualRecord CreatePersonDialog(TGEDCOMIndividualRecord aTarget, TGenEngine.TTargetMode aTargetMode, TGEDCOMObject.TGEDCOMSex aNeedSex)
		{
			TGEDCOMIndividualRecord Result = null;
			TfmPersonNew dlg = new TfmPersonNew();
			try
			{
				dlg.EditSex.SelectedIndex = (int)((sbyte)aNeedSex);
				dlg.TargetMode = aTargetMode;
				dlg.Target = aTarget;
				if (GKL.fmGEDKeeper.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					Result = TGenEngine.CreatePersonEx(this.FTree, dlg.edName.Text, dlg.edPatronymic.Text, dlg.edFamily.Text, (TGEDCOMObject.TGEDCOMSex)dlg.EditSex.SelectedIndex, true);
					this.ChangeRecord(Result);
				}
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
		public void CreateRecordsView(Control aParent, TGEDCOMRecord.TGEDCOMRecordType aRecordType, ref TRecordsView aList)
		{
			aList = new TRecordsView(this);
			aList.HideSelection = false;
			aList.LabelEdit = false;
			aList.FullRowSelect = true;
			aList.View = View.Details;
			aList.Tree = this.FTree;
			aList.RecordType = aRecordType;
			aList.Dock = DockStyle.Fill;
			aParent.Controls.Add(aList);
			aParent.Controls.SetChildIndex(aList, 0);
		}
		public void ExportToExcel(bool appmode)
		{
			TExcelExporter ex_exp = new TExcelExporter(this.FEngine, this.GetCurFileTempPath());
			try
			{
				ex_exp.Options = GKL.fmGEDKeeper.Options;
				ex_exp.SelectedRecords = this.ListPersons.ContentList;
				ex_exp.AppMode = appmode;
				ex_exp.Generate();
			}
			finally
			{
				ex_exp.Dispose();
			}
		}
		public void ExportToWeb()
		{
			TWebExporter web = new TWebExporter(this.FEngine, this.GetCurFileTempPath());
			try
			{
				web.Generate();
			}
			finally
			{
				web.Dispose();
			}
		}

		public void FileLoad(string aFileName)
		{
			this.ChangesClear();
			this.Clear();

			//try
			//{
				this.FTree.LoadFromFile(aFileName);
			//}
			//catch (Exception E)
			//{
			//	TGKSys.LogWrite("GKBase.FileLoad().TreeLoad(): " + E.Message);
			//	TGKSys.ShowError(GKL.LSList[245]);
			//}

			try
			{
				TGenEngine.CheckGEDCOMFormat(this.FTree);
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.FileLoad().CheckFormat(): " + E.Message);
				TGKSys.ShowError(GKL.LSList[246]);
			}
			this.FileName = aFileName;
			this.Modified = false;
			GKL.fmGEDKeeper.NamesTable.ImportNames(this.FTree);
			GKL.fmGEDKeeper.AddMRU(aFileName);
			this.ListsRefresh(false);
			this.ShowTips();
		}

		public void FileNew()
		{
			this.ChangesClear();
			this.Clear();
			this.ListsRefresh(false);
			this.ShowPersonInfo(null, this.mPersonSummary.Lines);
			this.FileName = GKL.LSList[165];
			this.Modified = false;
		}
		public DialogResult FileProperties(TfmBase.TFilePropertiesMode aMode)
		{
			TfmFileProperties fmFileProps = new TfmFileProperties(this);
			DialogResult Result;
			try
			{
				fmFileProps.PageControl1.SelectedIndex = (int)((sbyte)aMode);
				Result = GKL.fmGEDKeeper.ShowModalEx(fmFileProps, this, false);
			}
			finally
			{
				TObjectHelper.Free(fmFileProps);
			}
			return Result;
		}
		public void FileSave([In] string aFileName)
		{
			string subm = this.FTree.Header.GetTagStringValue("SUBM");
			bool is_advanced = this.IsAdvanced();
			string ext_name = this.FTree.Header.GetTagStringValue("_EXT_NAME");
			this.FTree.Header.Clear();
			this.FTree.Header.Source = "GEDKeeper";
			this.FTree.Header.ReceivingSystemName = "GEDKeeper";
			this.FTree.Header.CharacterSet = GKL.fmGEDKeeper.Options.DefCharacterSet;
			this.FTree.Header.Language = "Russian";
			this.FTree.Header.GEDCOMVersion = "5.5";
			this.FTree.Header.GEDCOMForm = "LINEAGE-LINKED";
			this.FTree.Header.FileName = Path.GetFileName(aFileName);
			this.FTree.Header.TransmissionDate.Date = DateTime.Now;
			if (BDSSystem.WStrCmp(subm, "") != 0)
			{
				this.FTree.Header.SetTagStringValue("SUBM", subm);
			}
			if (is_advanced && BDSSystem.WStrCmp(ext_name, "") != 0)
			{
				this.FTree.Header.AddTag("_ADVANCED", "", null);
				this.FTree.Header.AddTag("_EXT_NAME", ext_name, null);
			}
			this.FTree.Pack();
			StreamWriter fs = new StreamWriter(aFileName, false, Encoding.GetEncoding(1251));
			try
			{
				this.FTree.SaveToStream(fs);
				this.FTree.Header.CharacterSet = TGEDCOMObject.TGEDCOMCharacterSet.csASCII;
			}
			finally
			{
				TObjectHelper.Free(fs);
			}
			this.FileName = aFileName;
			GKL.fmGEDKeeper.AddMRU(aFileName);
			this.Modified = false;
		}
		public void GenPedigree_dAboville()
		{
			TPedigree p = new TPedigree(this.FEngine, this.GetCurFileTempPath());
			try
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKL.fmGEDKeeper.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = TPedigree.TPedigreeKind.pk_dAboville;
				p.Generate();
			}
			finally
			{
				p.Free();
			}
		}
		public void GenPedigree_Konovalov()
		{
			TPedigree p = new TPedigree(this.FEngine, this.GetCurFileTempPath());
			try
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKL.fmGEDKeeper.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = TPedigree.TPedigreeKind.pk_Konovalov;
				p.Generate();
			}
			finally
			{
				p.Free();
			}
		}
		public TGEDCOMFamilyRecord GetChildFamily(TGEDCOMIndividualRecord iChild, bool aCanCreate, TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord Result = null;
			if (iChild != null)
			{
				if (iChild.ChildToFamilyLinksCount != 0)
				{
					Result = iChild.GetChildToFamilyLink(0).Family;
				}
				else
				{
					if (aCanCreate)
					{
						TGEDCOMFamilyRecord fam = this.GetFamilyBySpouse(aNewParent);
						if (fam == null)
						{
							fam = TGenEngine.CreateFamilyEx(this.FTree);
						}
						this.FEngine.AddFamilyChild(fam, iChild);
						Result = fam;
					}
				}
			}
			return Result;
		}
		public string GetCurFileTempPath()
		{
			return VCLUtils.ExtractFilePath(this.FileName) + "~temp\\";
		}

		public TGEDCOMIndividualRecord GetSelectedPerson()
		{
			return this.ListPersons.GetSelectedRecord() as TGEDCOMIndividualRecord;
		}

		public void ListsRefresh(bool aTitles)
		{
			this.IntUpdate(this.ListPersons, 2, aTitles);
			this.IntUpdate(this.ListFamilies, 1, aTitles);
			this.IntUpdate(this.ListNotes, -1, aTitles);
			this.IntUpdate(this.ListMultimedia, 1, aTitles);
			this.IntUpdate(this.ListSources, 1, aTitles);
			this.IntUpdate(this.ListRepositories, 1, aTitles);
			this.IntUpdate(this.ListGroups, 1, aTitles);
			this.IntUpdate(this.ListResearches, 1, aTitles);
			this.IntUpdate(this.ListTasks, 1, aTitles);
			this.IntUpdate(this.ListCommunications, 1, aTitles);
			this.IntUpdate(this.ListLocations, 1, aTitles);

			this.PageRecords_SelectedIndexChanged(null, null);

			if (GKL.fmGEDKeeper.fmTimeLine != null)
			{
				GKL.fmGEDKeeper.fmTimeLine.CheckTimeWin(this);
			}
		}

		public void NavNext()
		{
			this.FBackman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FBackman.Next() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKL.fmGEDKeeper.UpdateControls(false);
			}
			finally
			{
				this.FBackman.EndNav();
			}
		}
		public void NavPrev()
		{
			this.FBackman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FBackman.Back() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKL.fmGEDKeeper.UpdateControls(false);
			}
			finally
			{
				this.FBackman.EndNav();
			}
		}
		public void PersonScan()
		{
			TfmPersonScan fmPersonScan = new TfmPersonScan(this);
			try
			{
				GKL.fmGEDKeeper.ShowModalEx(fmPersonScan, this, false);
			}
			finally
			{
				TObjectHelper.Free(fmPersonScan);
			}
		}
		public void RecordAdd()
		{
			TGEDCOMRecord rec = null;
			bool res = false;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					rec = this.CreatePersonDialog(null, TGenEngine.TTargetMode.tmAncestor, TGEDCOMObject.TGEDCOMSex.svNone);
					res = (rec != null);
					break;
				}
				case 1:
				{
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					res = this.ModifyFamily(ref fam, TGenEngine.TFamilyTarget.ftNone, null);
					rec = fam;
					break;
				}
				case 2:
				{
					TGEDCOMNoteRecord note = rec as TGEDCOMNoteRecord;
					res = this.ModifyNote(ref note);
					rec = note;
					break;
				}
				case 3:
				{
					TGEDCOMMultimediaRecord mm_rec = rec as TGEDCOMMultimediaRecord;
					res = this.ModifyMedia(ref mm_rec);
					rec = mm_rec;
					break;
				}
				case 4:
				{
					TGEDCOMSourceRecord src = rec as TGEDCOMSourceRecord;
					res = this.ModifySource(ref src);
					rec = src;
					break;
				}
				case 5:
				{
					TGEDCOMRepositoryRecord rep = rec as TGEDCOMRepositoryRecord;
					res = this.ModifyRepository(ref rep);
					rec = rep;
					break;
				}
				case 6:
				{
					TGEDCOMGroupRecord grp = rec as TGEDCOMGroupRecord;
					res = this.ModifyGroup(ref grp);
					rec = grp;
					break;
				}
				case 7:
				{
					TGEDCOMResearchRecord rsr = rec as TGEDCOMResearchRecord;
					res = this.ModifyResearch(ref rsr);
					rec = rsr;
					break;
				}
				case 8:
				{
					TGEDCOMTaskRecord tsk = rec as TGEDCOMTaskRecord;
					res = this.ModifyTask(ref tsk);
					rec = tsk;
					break;
				}
				case 9:
				{
					TGEDCOMCommunicationRecord comm = rec as TGEDCOMCommunicationRecord;
					res = this.ModifyCommunication(ref comm);
					rec = comm;
					break;
				}
				case 10:
				{
					TGEDCOMLocationRecord loc = rec as TGEDCOMLocationRecord;
					res = this.ModifyLocation(ref loc);
					rec = loc;
					break;
				}
			}
			if (res)
			{
				this.ListsRefresh(false);
				this.SelectRecordByXRef(rec.XRef);
			}
		}
		public void RecordDelete()
		{
			bool res = false;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					res = this.DeleteRecord(this.ListPersons.GetSelectedRecord(), true);
					break;
				}
				case 1:
				{
					res = this.DeleteRecord(this.ListFamilies.GetSelectedRecord(), true);
					break;
				}
				case 2:
				{
					res = this.DeleteRecord(this.ListNotes.GetSelectedRecord(), true);
					break;
				}
				case 3:
				{
					res = this.DeleteRecord(this.ListMultimedia.GetSelectedRecord(), true);
					break;
				}
				case 4:
				{
					res = this.DeleteRecord(this.ListSources.GetSelectedRecord(), true);
					break;
				}
				case 5:
				{
					res = this.DeleteRecord(this.ListRepositories.GetSelectedRecord(), true);
					break;
				}
				case 6:
				{
					res = this.DeleteRecord(this.ListGroups.GetSelectedRecord(), true);
					break;
				}
				case 7:
				{
					res = this.DeleteRecord(this.ListResearches.GetSelectedRecord(), true);
					break;
				}
				case 8:
				{
					res = this.DeleteRecord(this.ListTasks.GetSelectedRecord(), true);
					break;
				}
				case 9:
				{
					res = this.DeleteRecord(this.ListCommunications.GetSelectedRecord(), true);
					break;
				}
				case 10:
				{
					res = this.DeleteRecord(this.ListLocations.GetSelectedRecord(), true);
					break;
				}
			}
			if (res)
			{
				this.ListsRefresh(false);
			}
		}
		public void RecordEdit(object sender, EventArgs e)
		{
			bool res = false;
			TGEDCOMRecord rec = null;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					rec = this.ListPersons.GetSelectedRecord();
					TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
					res = this.ModifyPerson(ref ind);
					break;
				}
				case 1:
				{
					rec = this.ListFamilies.GetSelectedRecord();
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					res = this.ModifyFamily(ref fam, TGenEngine.TFamilyTarget.ftNone, null);
					break;
				}
				case 2:
				{
					rec = this.ListNotes.GetSelectedRecord();
					TGEDCOMNoteRecord note = rec as TGEDCOMNoteRecord;
					res = this.ModifyNote(ref note);
					break;
				}
				case 3:
				{
					rec = this.ListMultimedia.GetSelectedRecord();
					TGEDCOMMultimediaRecord mm_rec = rec as TGEDCOMMultimediaRecord;
					res = this.ModifyMedia(ref mm_rec);
					break;
				}
				case 4:
				{
					rec = this.ListSources.GetSelectedRecord();
					TGEDCOMSourceRecord src = rec as TGEDCOMSourceRecord;
					res = this.ModifySource(ref src);
					break;
				}
				case 5:
				{
					rec = this.ListRepositories.GetSelectedRecord();
					TGEDCOMRepositoryRecord rep = rec as TGEDCOMRepositoryRecord;
					res = this.ModifyRepository(ref rep);
					break;
				}
				case 6:
				{
					rec = this.ListGroups.GetSelectedRecord();
					TGEDCOMGroupRecord grp = rec as TGEDCOMGroupRecord;
					res = this.ModifyGroup(ref grp);
					break;
				}
				case 7:
				{
					rec = this.ListResearches.GetSelectedRecord();
					TGEDCOMResearchRecord rsr = rec as TGEDCOMResearchRecord;
					res = this.ModifyResearch(ref rsr);
					break;
				}
				case 8:
				{
					rec = this.ListTasks.GetSelectedRecord();
					TGEDCOMTaskRecord tsk = rec as TGEDCOMTaskRecord;
					res = this.ModifyTask(ref tsk);
					break;
				}
				case 9:
				{
					rec = this.ListCommunications.GetSelectedRecord();
					TGEDCOMCommunicationRecord comm = rec as TGEDCOMCommunicationRecord;
					res = this.ModifyCommunication(ref comm);
					break;
				}
				case 10:
				{
					rec = this.ListLocations.GetSelectedRecord();
					TGEDCOMLocationRecord loc = rec as TGEDCOMLocationRecord;
					res = this.ModifyLocation(ref loc);
					break;
				}
			}
			if (res)
			{
				this.ListsRefresh(false);
				this.ShowRecordInfo(rec);
			}
		}
		public void RecordNotify(TGEDCOMRecord aRecord, TfmBase.TRecNotify aNotify)
		{
			if (aRecord != null)
			{
				TRecordsView list = null;
				switch (aRecord.RecordType)
				{
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						list = this.ListPersons;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						list = this.ListFamilies;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
					{
						list = this.ListNotes;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						list = this.ListMultimedia;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						list = this.ListSources;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						list = this.ListRepositories;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
					{
						list = this.ListGroups;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
					{
						list = this.ListResearches;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
					{
						list = this.ListTasks;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
					{
						list = this.ListCommunications;
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
					{
						list = this.ListLocations;
						break;
					}
				}
				if (list != null && aNotify == TfmBase.TRecNotify.rnDelete)
				{
					list.DeleteRecord(aRecord);
				}
			}
		}
		public void SearchSubjectLinks(TGEDCOMRecord aInRecord, TGEDCOMRecord aSubject, TStrings aToList)
		{
			try
			{
				if (aSubject is TGEDCOMNoteRecord)
				{
					int arg_12_0 = 0;
					int num = aInRecord.GetNotesCount() - 1;
					int i = arg_12_0;
					if (num >= i)
					{
						num++;
						do
						{
							if (object.Equals(aInRecord.GetNote(i).Value, aSubject))
							{
								this.OutLink(aSubject, aToList, aInRecord, null, null);
							}
							i++;
						}
						while (i != num);
					}
				}
				else
				{
					if (aSubject is TGEDCOMMultimediaRecord)
					{
						int arg_5D_0 = 0;
						int num2 = aInRecord.GetMultimediaLinksCount() - 1;
						int i = arg_5D_0;
						if (num2 >= i)
						{
							num2++;
							do
							{
								if (object.Equals(aInRecord.GetMultimediaLink(i).Value, aSubject))
								{
									this.OutLink(aSubject, aToList, aInRecord, null, null);
								}
								i++;
							}
							while (i != num2);
						}
					}
					else
					{
						if (aSubject is TGEDCOMSourceRecord)
						{
							int arg_A6_0 = 0;
							int num3 = aInRecord.GetSourceCitationsCount() - 1;
							int i = arg_A6_0;
							if (num3 >= i)
							{
								num3++;
								do
								{
									if (object.Equals(aInRecord.GetSourceCitation(i).Value, aSubject))
									{
										this.OutLink(aSubject, aToList, aInRecord, null, aInRecord.GetSourceCitation(i));
									}
									i++;
								}
								while (i != num3);
							}
						}
					}
				}
				if (aInRecord is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)aInRecord;
					int arg_F9_0 = 0;
					int num4 = i_rec.GetIndividualEventsCount() - 1;
					int i = arg_F9_0;
					if (num4 >= i)
					{
						num4++;
						do
						{
							this.PrepareEvent(aSubject, aToList, i_rec, i_rec.GetIndividualEvent(i));
							i++;
						}
						while (i != num4);
					}
				}
				else
				{
					if (aInRecord is TGEDCOMFamilyRecord)
					{
						TGEDCOMFamilyRecord f_rec = (TGEDCOMFamilyRecord)aInRecord;
						int arg_138_0 = 0;
						int num5 = f_rec.GetFamilyEventCount() - 1;
						int i = arg_138_0;
						if (num5 >= i)
						{
							num5++;
							do
							{
								this.PrepareEvent(aSubject, aToList, f_rec, f_rec.GetFamilyEvent(i));
								i++;
							}
							while (i != num5);
						}
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.SearchSubjectLinks(): " + E.Message);
			}
		}
		public TGEDCOMFamilyRecord SelectFamily(TGEDCOMIndividualRecord aTarget)
		{
			TfmRecordSelect dlg = new TfmRecordSelect(this);
			TGEDCOMFamilyRecord Result;
			try
			{
				dlg.FTarget = aTarget;
				dlg.FNeedSex = TGEDCOMObject.TGEDCOMSex.svNone;
				dlg.TargetMode = TGenEngine.TTargetMode.tmChildToFamily;
				dlg.Mode = TGEDCOMRecord.TGEDCOMRecordType.rtFamily;
				if (GKL.fmGEDKeeper.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					Result = (dlg.ResultRecord as TGEDCOMFamilyRecord);
				}
				else
				{
					Result = null;
				}
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
		public TGEDCOMIndividualRecord SelectPerson(TGEDCOMIndividualRecord aTarget, TGenEngine.TTargetMode aTargetMode, TGEDCOMObject.TGEDCOMSex aNeedSex)
		{
			TfmRecordSelect dlg = new TfmRecordSelect(this);
			TGEDCOMIndividualRecord Result;
			try
			{
				dlg.FTarget = aTarget;
				dlg.FNeedSex = aNeedSex;
				dlg.TargetMode = aTargetMode;
				dlg.Mode = TGEDCOMRecord.TGEDCOMRecordType.rtIndividual;
				if (GKL.fmGEDKeeper.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					Result = (dlg.ResultRecord as TGEDCOMIndividualRecord);
				}
				else
				{
					Result = null;
				}
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
		public TGEDCOMRecord SelectRecord(TGEDCOMRecord.TGEDCOMRecordType aMode, params object[] anArgs)
		{
			anArgs = (object[])anArgs.Clone();
			TfmRecordSelect dlg = new TfmRecordSelect(this);
			TGEDCOMRecord Result;
			try
			{
				dlg.Mode = aMode;
				int args_cnt = ((anArgs != null) ? anArgs.Length : 0) - 1 + 1;
				if (args_cnt > 0)
				{
					dlg.edFastFilter.Text = (anArgs[0] as string);
				}
				if (GKL.fmGEDKeeper.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					Result = dlg.ResultRecord;
				}
				else
				{
					Result = null;
				}
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
		public void SelectRecordByXRef(string XRef)
		{
			TGEDCOMRecord rec = this.FTree.XRefIndex_Find(XRef);
			if (rec != null)
			{
				switch (rec.RecordType)
				{
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListPersons, rec, 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListFamilies, rec, 1);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListNotes, rec, 2);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListMultimedia, rec, 3);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListSources, rec, 4);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListRepositories, rec, 5);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListGroups, rec, 6);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListResearches, rec, 7);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListTasks, rec, 8);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListCommunications, rec, 9);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListLocations, rec, 10);
						break;
					}
				}
			}
		}
		public void SetFilter()
		{
			TfmFilter fmFilter = new TfmFilter(this);
			try
			{
				GKL.fmGEDKeeper.ShowModalEx(fmFilter, this, false);
			}
			finally
			{
				TObjectHelper.Free(fmFilter);
			}
		}
		public void ShowMap()
		{
			TfmMaps frm_maps = new TfmMaps(this.FTree, this.ListPersons.ContentList);
			frm_maps.MdiParent = GKL.fmGEDKeeper;
			frm_maps.Show();
		}
		public void ShowMedia(TGEDCOMMultimediaRecord aMediaRec)
		{
			TfmMediaView fmMediaView = new TfmMediaView(this);
			try
			{
				fmMediaView.FileRef = aMediaRec.GetFileReference(0);
				if (!fmMediaView.Extern)
				{
					GKL.fmGEDKeeper.ShowModalEx(fmMediaView, GKL.fmGEDKeeper, false);
				}
			}
			finally
			{
				TObjectHelper.Free(fmMediaView);
			}
		}
		public void ShowOrganizer()
		{
			TfmOrganizer dlg = new TfmOrganizer(this);
			try
			{
				GKL.fmGEDKeeper.ShowModalEx(dlg, this, false);
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
		}
		public void ShowRecordInfo(TGEDCOMRecord aRecord)
		{
			if (aRecord != null)
			{
				try
				{
					switch (aRecord.RecordType)
					{
						case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
						{
							this.ShowPersonInfo(aRecord as TGEDCOMIndividualRecord, this.mPersonSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
						{
							this.ShowFamilyInfo(aRecord as TGEDCOMFamilyRecord, this.mFamilySummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
						{
							this.ShowNoteInfo(aRecord as TGEDCOMNoteRecord, this.mNoteSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
						{
							this.ShowMultimediaInfo(aRecord as TGEDCOMMultimediaRecord, this.mMediaSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
						{
							this.ShowSourceInfo(aRecord as TGEDCOMSourceRecord, this.mSourceSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
						{
							this.ShowRepositoryInfo(aRecord as TGEDCOMRepositoryRecord, this.mRepositorySummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
						{
							this.ShowGroupInfo(aRecord as TGEDCOMGroupRecord, this.mGroupSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
						{
							this.ShowResearchInfo(aRecord as TGEDCOMResearchRecord, this.mResearchSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
						{
							this.ShowTaskInfo(aRecord as TGEDCOMTaskRecord, this.mTaskSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
						{
							this.ShowCommunicationInfo(aRecord as TGEDCOMCommunicationRecord, this.mCommunicationSummary.Lines);
							break;
						}
						case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
						{
							this.ShowLocationInfo(aRecord as TGEDCOMLocationRecord, this.mLocationSummary.Lines);
							break;
						}
					}
				}
				catch (Exception E)
				{
					TGKSys.LogWrite("GKBase.ShowRecordInfo(): " + E.Message);
				}
			}
		}
		public void ShowScriptDaemon()
		{
			TfmScriptDaemon dmn = new TfmScriptDaemon(this);
			try
			{
				GKL.fmGEDKeeper.ShowModalEx(dmn, this, false);
			}
			finally
			{
				TObjectHelper.Free(dmn);
			}
		}
		public void ShowStats()
		{
			TfmStats fmStats = new TfmStats(this);
			fmStats.Show();
		}
		public void ShowTips()
		{
			try
			{
				if (!GKL.fmGEDKeeper.Options.ShowTips)
				{
				}
				else
				{
					TStringList birth_days = new TStringList();
					try
					{
						int arg_32_0 = 0;
						int num = this.FTree.RecordsCount - 1;
						int i = arg_32_0;
						if (num >= i)
						{
							num++;
							do
							{
								TGEDCOMRecord rec = this.FTree.GetRecord(i);
								if (rec is TGEDCOMIndividualRecord)
								{
									TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
									string nm = TGenEngine.GetNameStr(i_rec, true, false);
									string days = TGenEngine.GetDaysForBirth(i_rec);
									if (BDSSystem.WStrCmp(days, "") != 0 && int.Parse(days) < 3)
									{
										birth_days.Add(string.Format(GKL.LSList[248], new object[]
										{
											nm, 
											days
										}));
									}
								}
								i++;
							}
							while (i != num);
						}
						if (birth_days.Count > 0)
						{
							GKL.fmGEDKeeper.Options.ShowTips = TfmTipsDialog.ShowTipsEx(GKL.LSList[247], GKL.fmGEDKeeper.Options.ShowTips, birth_days);
						}
					}
					finally
					{
						birth_days.Free();
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowTips(): " + E.Message);
			}
		}

		public void ShowTreeAncestors()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TAncestryChartBox.TChartKind.ckAncestors))
			{
				TfmChart fmChart = new TfmChart(this);
				fmChart.Base = this;
				fmChart.Tree = this.FTree;
				fmChart.Person = this.GetSelectedPerson();
				fmChart.ChartKind = TAncestryChartBox.TChartKind.ckAncestors;
				fmChart.FileName = Path.GetFileName(this.FileName);
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeDescendants()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TAncestryChartBox.TChartKind.ckDescendants))
			{
				TfmChart fmChart = new TfmChart(this);
				fmChart.Base = this;
				fmChart.Tree = this.FTree;
				fmChart.Person = this.GetSelectedPerson();
				fmChart.ChartKind = TAncestryChartBox.TChartKind.ckDescendants;
				fmChart.FileName = Path.GetFileName(this.FileName);
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeBoth()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TAncestryChartBox.TChartKind.ckBoth))
			{
				TfmChart fmChart = new TfmChart(this);
				fmChart.Base = this;
				fmChart.Tree = this.FTree;
				fmChart.Person = this.GetSelectedPerson();
				fmChart.ChartKind = TAncestryChartBox.TChartKind.ckBoth;
				fmChart.FileName = Path.GetFileName(this.FileName);
				fmChart.GenChart(true);
			}
		}
		public void TreeTools()
		{
			TfmTreeTools fmTreeTools = new TfmTreeTools(this);
			try
			{
				GKL.fmGEDKeeper.ShowModalEx(fmTreeTools, this, false);
			}
			finally
			{
				TObjectHelper.Free(fmTreeTools);
			}
		}
		public bool IsAdvanced()
		{
			return this.FTree.Header.FindTag("_ADVANCED", 0) != null;
		}
		public bool ModifyName(ref TNamesTable.TName aName)
		{
			TfmNameEdit dlg = new TfmNameEdit();
			bool Result;
			try
			{
				dlg.IName = aName;
				Result = (GKL.fmGEDKeeper.ShowModalEx(dlg, this, false) == DialogResult.OK);
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
		public string DefinePatronymic(string aName, TGEDCOMObject.TGEDCOMSex aSex, bool aConfirm)
		{
			string Result = "";
			TNamesTable.TName i = GKL.fmGEDKeeper.NamesTable.FindName(aName);
			if (i == null)
			{
				if (!aConfirm)
				{
					return Result;
				}
				i = GKL.fmGEDKeeper.NamesTable.AddName(aName);
			}
			if (aSex == TGEDCOMObject.TGEDCOMSex.svMale)
			{
				Result = i.M_Patronymic;
				goto IL_48;
			}
			if (aSex != TGEDCOMObject.TGEDCOMSex.svFemale)
			{
				goto IL_48;
			}
			Result = i.F_Patronymic;
			goto IL_48;
			return Result;
			IL_48:
			if (BDSSystem.WStrCmp(Result, "") == 0)
			{
				if (!aConfirm)
				{
					return Result;
				}
				this.ModifyName(ref i);
			}
			if (aSex == TGEDCOMObject.TGEDCOMSex.svMale)
			{
				Result = i.M_Patronymic;
				return Result;
			}
			if (aSex != TGEDCOMObject.TGEDCOMSex.svFemale)
			{
				return Result;
			}
			Result = i.F_Patronymic;
			return Result;
		}
		public bool DeleteFamilyRecord(TGEDCOMFamilyRecord aFamily, bool aConfirm)
		{
			bool Result = false;
			if (aFamily != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[72], new object[]
			{
				TGenEngine.GetFamilyStr(aFamily)
			})) != DialogResult.No))
			{
				this.FEngine.CleanFamily(aFamily);
				this.RecordNotify(aFamily, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(aFamily));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteGroupRecord(TGEDCOMGroupRecord groupRec, bool aConfirm)
		{
			bool Result = false;
			if (groupRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[77], new object[]
			{
				groupRec.Name
			})) != DialogResult.No))
			{
				int arg_43_0 = 0;
				int num = groupRec.GetMembersCount() - 1;
				int i = arg_43_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMIndividualRecord member = groupRec.GetMember(i).Value as TGEDCOMIndividualRecord;
						member.DeleteGroup(member.IndexOfGroup(groupRec));
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(groupRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(groupRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteIndividualRecord(TGEDCOMIndividualRecord iRec, bool aConfirm)
		{
			bool Result = false;
			if (iRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[71], new object[]
			{
				TGenEngine.GetNameStr(iRec, true, false)
			})) != DialogResult.No))
			{
				int i = iRec.ChildToFamilyLinksCount - 1;
				if (i >= 0)
				{
					do
					{
						TGEDCOMFamilyRecord family = iRec.GetChildToFamilyLink(i).Family;
						family.DeleteChild(iRec);
						i--;
					}
					while (i != -1);
				}
				i = iRec.SpouseToFamilyLinksCount - 1;
				if (i >= 0)
				{
					do
					{
						TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(i).Family;
						this.FEngine.RemoveFamilySpouse(family, iRec);
						i--;
					}
					while (i != -1);
				}
				this.RecordNotify(iRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(iRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteMediaRecord(TGEDCOMMultimediaRecord mRec, bool aConfirm)
		{
			bool Result = false;
			if (mRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[75], new object[]
			{
				mRec.StringValue
			})) != DialogResult.No))
			{
				int arg_4C_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_4C_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						int j = rec.GetMultimediaLinksCount() - 1;
						if (j >= 0)
						{
							do
							{
								if (object.Equals(rec.GetMultimediaLink(j).Value, mRec))
								{
									rec.DeleteMultimediaLink(j);
								}
								j--;
							}
							while (j != -1);
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(mRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(mRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteNoteRecord(TGEDCOMNoteRecord nRec, bool aConfirm)
		{
			bool Result = false;
			if (nRec != null && (!aConfirm || TGKSys.ShowQuestion(GKL.LSList[73]) != DialogResult.No))
			{
				int arg_33_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_33_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						int j = rec.GetNotesCount() - 1;
						if (j >= 0)
						{
							do
							{
								if (object.Equals(rec.GetNote(j).Value, nRec))
								{
									rec.DeleteNotes(j);
								}
								j--;
							}
							while (j != -1);
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(nRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(nRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteRepositoryRecord(TGEDCOMRepositoryRecord repRec, bool aConfirm)
		{
			bool Result = false;
			if (repRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[76], new object[]
			{
				repRec.RepositoryName
			})) != DialogResult.No))
			{
				int arg_4C_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_4C_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMSourceRecord)
						{
							TGEDCOMSourceRecord srcRec = (TGEDCOMSourceRecord)rec;
							int j = srcRec.GetRepositoryCitationsCount() - 1;
							if (j >= 0)
							{
								do
								{
									if (object.Equals(srcRec.GetRepositoryCitation(j).Value, repRec))
									{
										srcRec.DeleteRepositoryCitation(srcRec.GetRepositoryCitation(j));
									}
									j--;
								}
								while (j != -1);
							}
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(repRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(repRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteResearchRecord(TGEDCOMResearchRecord resRec, bool aConfirm)
		{
			bool Result = false;
			if (resRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[78], new object[]
			{
				resRec.Name
			})) != DialogResult.No))
			{
				this.RecordNotify(resRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(resRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteSourceRecord(TGEDCOMSourceRecord srcRec, bool aConfirm)
		{
			bool Result = false;
			if (srcRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[74], new object[]
			{
				srcRec.FiledByEntry
			})) != DialogResult.No))
			{
				int arg_4C_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_4C_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						int j = rec.GetSourceCitationsCount() - 1;
						if (j >= 0)
						{
							do
							{
								if (object.Equals(rec.GetSourceCitation(j).Value, srcRec))
								{
									rec.DeleteSourceCitation(j);
								}
								j--;
							}
							while (j != -1);
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(srcRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(srcRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteTaskRecord(TGEDCOMTaskRecord TaskRec, bool aConfirm)
		{
			bool Result = false;
			if (TaskRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[79], new object[]
			{
				TGenEngine.GetTaskGoalStr(this.FTree, TaskRec)
			})) != DialogResult.No))
			{
				int arg_57_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_57_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMResearchRecord)
						{
							TGEDCOMResearchRecord resRec = (TGEDCOMResearchRecord)rec;
							int j = resRec.TasksCount - 1;
							if (j >= 0)
							{
								do
								{
									if (object.Equals(resRec.GetTask(j).Value, TaskRec))
									{
										resRec.DeleteTask(j);
									}
									j--;
								}
								while (j != -1);
							}
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(TaskRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(TaskRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteCommunicationRecord(TGEDCOMCommunicationRecord ComRec, bool aConfirm)
		{
			bool Result = false;
			if (ComRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[80], new object[]
			{
				ComRec.Name
			})) != DialogResult.No))
			{
				int arg_4C_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_4C_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMResearchRecord)
						{
							TGEDCOMResearchRecord resRec = (TGEDCOMResearchRecord)rec;
							int j = resRec.CommunicationsCount - 1;
							if (j >= 0)
							{
								do
								{
									if (object.Equals(resRec.GetCommunication(j).Value, ComRec))
									{
										resRec.DeleteCommunication(j);
									}
									j--;
								}
								while (j != -1);
							}
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(ComRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(ComRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteLocationRecord(TGEDCOMLocationRecord LocRec, bool aConfirm)
		{
			bool Result = false;
			if (LocRec != null && (!aConfirm || TGKSys.ShowQuestion(string.Format(GKL.LSList[81], new object[]
			{
				LocRec.Name
			})) != DialogResult.No))
			{
				int arg_4D_0 = 0;
				int num = this.FTree.RecordsCount - 1;
				int i = arg_4D_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
							int j = iRec.IndividualEventsCount - 1;
							if (j >= 0)
							{
								do
								{
									TGEDCOMCustomEvent ev = iRec.GetIndividualEvent(j);
									if (object.Equals(ev.Detail.Place.Location.Value, LocRec))
									{
										ev.Detail.Place.DeleteTag("_LOC");
									}
									j--;
								}
								while (j != -1);
							}
						}
						else
						{
							if (rec is TGEDCOMFamilyRecord)
							{
								TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)rec;
								int j = fRec.GetFamilyEventCount() - 1;
								if (j >= 0)
								{
									do
									{
										TGEDCOMCustomEvent ev = fRec.GetFamilyEvent(j);
										if (object.Equals(ev.Detail.Place.Location.Value, LocRec))
										{
											ev.Detail.Place.DeleteTag("_LOC");
										}
										j--;
									}
									while (j != -1);
								}
							}
						}
						i++;
					}
					while (i != num);
				}
				this.RecordNotify(LocRec, TfmBase.TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(LocRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}
		public bool DeleteRecord(TGEDCOMRecord aRecord, bool aConfirm)
		{
			bool Result = false;
			if (aRecord != null)
			{
				switch (aRecord.RecordType)
				{
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						Result = this.DeleteIndividualRecord((TGEDCOMIndividualRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						Result = this.DeleteFamilyRecord((TGEDCOMFamilyRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
					{
						Result = this.DeleteNoteRecord((TGEDCOMNoteRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						Result = this.DeleteMediaRecord((TGEDCOMMultimediaRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						Result = this.DeleteSourceRecord((TGEDCOMSourceRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						Result = this.DeleteRepositoryRecord((TGEDCOMRepositoryRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
					{
						Result = this.DeleteGroupRecord((TGEDCOMGroupRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
					{
						Result = this.DeleteResearchRecord((TGEDCOMResearchRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
					{
						Result = this.DeleteTaskRecord((TGEDCOMTaskRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
					{
						Result = this.DeleteCommunicationRecord((TGEDCOMCommunicationRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
					{
						Result = this.DeleteLocationRecord((TGEDCOMLocationRecord)aRecord, aConfirm);
						break;
					}
				}
			}
			return Result;
		}
		public bool RecordIsFiltered(TGEDCOMRecord aRecord)
		{
			bool Result = false;
			if (aRecord != null)
			{
				switch (aRecord.RecordType)
				{
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						Result = (this.ListPersons.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						Result = (this.ListFamilies.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
					{
						Result = (this.ListNotes.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						Result = (this.ListMultimedia.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						Result = (this.ListSources.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						Result = (this.ListRepositories.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
					{
						Result = (this.ListGroups.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
					{
						Result = (this.ListResearches.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
					{
						Result = (this.ListTasks.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
					{
						Result = (this.ListCommunications.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
					{
						Result = (this.ListLocations.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
				}
			}
			return Result;
		}
		public void DoUndo()
		{
			this.FUndoman.CmdUndo();
			this.ListsRefresh(false);
			GKL.fmGEDKeeper.UpdateControls(false);
		}
		public void DoRedo()
		{
			this.FUndoman.CmdRedo();
			this.ListsRefresh(false);
			GKL.fmGEDKeeper.UpdateControls(false);
		}
		public void DoPersonChangeSex(TGEDCOMIndividualRecord aPerson, TGEDCOMObject.TGEDCOMSex NewSex)
		{
			if (aPerson.Sex != NewSex)
			{
				this.FUndoman.CmdDo(new TCmdPersonChangeSex(this.FUndoman, aPerson, NewSex));
				this.FUndoman.Commit();
			}
		}
		public void DoPersonChangePatriarch(TGEDCOMIndividualRecord aPerson, bool NewValue)
		{
			if (aPerson.Patriarch != NewValue)
			{
				this.FUndoman.CmdDo(new TCmdPersonChangePatriarch(this.FUndoman, aPerson, NewValue));
				this.FUndoman.Commit();
			}
		}
		public bool ModifyPerson(ref TGEDCOMIndividualRecord aIndivRec)
		{
			bool Result = false;
			if (aIndivRec != null)
			{
				TfmPersonEdit dlg = new TfmPersonEdit(this);
				try
				{
					dlg.Person = aIndivRec;
					Result = (GKL.fmGEDKeeper.ShowModalEx(dlg, this, false) == DialogResult.OK);
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			return Result;
		}
		public bool ModifyFamily(ref TGEDCOMFamilyRecord aFamilyRec, TGenEngine.TFamilyTarget aTarget, TGEDCOMIndividualRecord aPerson)
		{
			bool Result = false;
			if (aTarget == TGenEngine.TFamilyTarget.ftSpouse && aPerson != null)
			{
				TGEDCOMObject.TGEDCOMSex sex = aPerson.Sex;
				if (sex < TGEDCOMObject.TGEDCOMSex.svMale || sex >= TGEDCOMObject.TGEDCOMSex.svUndetermined)
				{
					TGKSys.ShowError(GKL.LSList[210]);
					return Result;
				}
			}
			TfmFamilyEdit fmFamEdit = new TfmFamilyEdit(this);
			try
			{
				bool exists = aFamilyRec != null;
				if (!exists)
				{
					aFamilyRec = new TGEDCOMFamilyRecord(this.FTree, this.FTree, "", "");
					aFamilyRec.InitNew();
				}
				if (aTarget != TGenEngine.TFamilyTarget.ftSpouse)
				{
					if (aTarget == TGenEngine.TFamilyTarget.ftChild)
					{
						if (aPerson != null)
						{
							this.FEngine.AddFamilyChild(aFamilyRec, aPerson);
						}
					}
				}
				else
				{
					if (aPerson != null)
					{
						this.FEngine.AddFamilySpouse(aFamilyRec, aPerson);
					}
				}
				fmFamEdit.Family = aFamilyRec;
				Result = (GKL.fmGEDKeeper.ShowModalEx(fmFamEdit, this, false) == DialogResult.OK);
				if (Result)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aFamilyRec);
					}
				}
				else
				{
					if (!exists)
					{
						this.FEngine.CleanFamily(aFamilyRec);
						object obj = aFamilyRec;
						VCLUtils.FreeAndNil(ref obj);
						aFamilyRec = (obj as TGEDCOMFamilyRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmFamEdit);
			}
			return Result;
		}
		public bool ModifyNote(ref TGEDCOMNoteRecord aNoteRec)
		{
			bool Result = false;
			TfmNoteEdit fmNoteEdit = new TfmNoteEdit(this);
			try
			{
				bool exists = aNoteRec != null;
				if (!exists)
				{
					aNoteRec = new TGEDCOMNoteRecord(this.FTree, this.FTree, "", "");
					aNoteRec.InitNew();
				}
				fmNoteEdit.NoteRecord = aNoteRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmNoteEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aNoteRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aNoteRec;
						VCLUtils.FreeAndNil(ref obj);
						aNoteRec = (obj as TGEDCOMNoteRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmNoteEdit);
			}
			return Result;
		}
		public bool ModifyMedia(ref TGEDCOMMultimediaRecord aMediaRec)
		{
			bool Result = false;
			TfmMediaEdit fmMediaEdit = new TfmMediaEdit(this);
			try
			{
				bool exists = aMediaRec != null;
				if (!exists)
				{
					aMediaRec = new TGEDCOMMultimediaRecord(this.FTree, this.FTree, "", "");
					aMediaRec.AddFileReference(new TGEDCOMFileReferenceWithTitle(this.FTree, aMediaRec, "", ""));
					aMediaRec.InitNew();
				}
				fmMediaEdit.MediaRec = aMediaRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmMediaEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aMediaRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aMediaRec;
						VCLUtils.FreeAndNil(ref obj);
						aMediaRec = (obj as TGEDCOMMultimediaRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmMediaEdit);
			}
			return Result;
		}
		public bool ModifySource(ref TGEDCOMSourceRecord aSourceRec)
		{
			bool Result = false;
			TfmSourceEdit fmSrcEdit = new TfmSourceEdit(this);
			try
			{
				bool exists = aSourceRec != null;
				if (!exists)
				{
					aSourceRec = new TGEDCOMSourceRecord(this.FTree, this.FTree, "", "");
					aSourceRec.InitNew();
				}
				fmSrcEdit.SourceRecord = aSourceRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmSrcEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aSourceRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aSourceRec;
						VCLUtils.FreeAndNil(ref obj);
						aSourceRec = (obj as TGEDCOMSourceRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmSrcEdit);
			}
			return Result;
		}
		public bool ModifyRepository(ref TGEDCOMRepositoryRecord aRepRec)
		{
			bool Result = false;
			TfmRepositoryEdit fmRepEdit = new TfmRepositoryEdit(this);
			try
			{
				bool exists = aRepRec != null;
				if (!exists)
				{
					aRepRec = new TGEDCOMRepositoryRecord(this.FTree, this.FTree, "", "");
					aRepRec.InitNew();
				}
				fmRepEdit.Repository = aRepRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmRepEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aRepRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aRepRec;
						VCLUtils.FreeAndNil(ref obj);
						aRepRec = (obj as TGEDCOMRepositoryRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmRepEdit);
			}
			return Result;
		}
		public bool ModifyGroup(ref TGEDCOMGroupRecord aGroupRec)
		{
			bool Result = false;
			TfmGroupEdit fmGrpEdit = new TfmGroupEdit(this);
			try
			{
				bool exists = aGroupRec != null;
				if (!exists)
				{
					aGroupRec = new TGEDCOMGroupRecord(this.FTree, this.FTree, "", "");
					aGroupRec.InitNew();
				}
				fmGrpEdit.Group = aGroupRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmGrpEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aGroupRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aGroupRec;
						VCLUtils.FreeAndNil(ref obj);
						aGroupRec = (obj as TGEDCOMGroupRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmGrpEdit);
			}
			return Result;
		}
		public bool ModifyResearch(ref TGEDCOMResearchRecord aResearchRec)
		{
			bool Result = false;
			TfmResearchEdit fmResEdit = new TfmResearchEdit(this);
			try
			{
				bool exists = aResearchRec != null;
				if (!exists)
				{
					aResearchRec = new TGEDCOMResearchRecord(this.FTree, this.FTree, "", "");
					aResearchRec.InitNew();
				}
				fmResEdit.Research = aResearchRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmResEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aResearchRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aResearchRec;
						VCLUtils.FreeAndNil(ref obj);
						aResearchRec = (obj as TGEDCOMResearchRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmResEdit);
			}
			return Result;
		}
		public bool ModifyTask(ref TGEDCOMTaskRecord aTaskRec)
		{
			bool Result = false;
			TfmTaskEdit fmTaskEdit = new TfmTaskEdit(this);
			try
			{
				bool exists = aTaskRec != null;
				if (!exists)
				{
					aTaskRec = new TGEDCOMTaskRecord(this.FTree, this.FTree, "", "");
					aTaskRec.InitNew();
				}
				fmTaskEdit.Task = aTaskRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmTaskEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aTaskRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aTaskRec;
						VCLUtils.FreeAndNil(ref obj);
						aTaskRec = (obj as TGEDCOMTaskRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmTaskEdit);
			}
			return Result;
		}
		public bool ModifyCommunication(ref TGEDCOMCommunicationRecord aCommunicationRec)
		{
			bool Result = false;
			TfmCommunicationEdit fmCorrEdit = new TfmCommunicationEdit(this);
			try
			{
				bool exists = aCommunicationRec != null;
				if (!exists)
				{
					aCommunicationRec = new TGEDCOMCommunicationRecord(this.FTree, this.FTree, "", "");
					aCommunicationRec.InitNew();
				}
				fmCorrEdit.Communication = aCommunicationRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmCorrEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aCommunicationRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aCommunicationRec;
						VCLUtils.FreeAndNil(ref obj);
						aCommunicationRec = (obj as TGEDCOMCommunicationRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmCorrEdit);
			}
			return Result;
		}
		public bool ModifyLocation(ref TGEDCOMLocationRecord aLocationRec)
		{
			bool Result = false;
			TfmLocationEdit fmLocEdit = new TfmLocationEdit(this);
			try
			{
				bool exists = aLocationRec != null;
				if (!exists)
				{
					aLocationRec = new TGEDCOMLocationRecord(this.FTree, this.FTree, "", "");
					aLocationRec.InitNew();
				}
				fmLocEdit.LocationRecord = aLocationRec;
				if (GKL.fmGEDKeeper.ShowModalEx(fmLocEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aLocationRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						object obj = aLocationRec;
						VCLUtils.FreeAndNil(ref obj);
						aLocationRec = (obj as TGEDCOMLocationRecord);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmLocEdit);
			}
			return Result;
		}
		public bool ModifyAddress(Form aSender, TGEDCOMAddress anAddress)
		{
			TfmAddressEdit dlg = new TfmAddressEdit();
			bool Result;
			try
			{
				dlg.Address = anAddress;
				Result = (GKL.fmGEDKeeper.ShowModalEx(dlg, aSender, false) == DialogResult.OK);
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
		public bool ModifyRecAssociation(Form aSender, TGEDCOMIndividualRecord aRecord, TGEDCOMAssociation aAssociation, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[243]) != DialogResult.No)
				{
					aRecord.DeleteAssociation(aAssociation);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmAssociationEdit fmAstEdit = new TfmAssociationEdit(this);
				try
				{
					TGEDCOMAssociation ast;
					if (anAction == TGenEngine.TRecAction.raEdit && aAssociation != null)
					{
						ast = aAssociation;
					}
					else
					{
						ast = new TGEDCOMAssociation(this.FTree, aRecord, "", "");
					}
					fmAstEdit.Association = ast;
					DialogResult res = GKL.fmGEDKeeper.ShowModalEx(fmAstEdit, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.AddAssociation(ast);
						}
						else
						{
							ast.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					TObjectHelper.Free(fmAstEdit);
				}
			}
			return Result;
		}
		public bool ModifyRecEvent(Form aSender, TGEDCOMRecord aRecord, TGEDCOMCustomEvent aEvent, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[239]) != DialogResult.No)
				{
					if (aRecord is TGEDCOMIndividualRecord)
					{
						(aRecord as TGEDCOMIndividualRecord).DeleteIndividualEvent(aEvent);
					}
					else
					{
						(aRecord as TGEDCOMFamilyRecord).DeleteFamilyEvent(aEvent as TGEDCOMFamilyEvent);
					}
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmEventEdit fmEventEdit = new TfmEventEdit(this);
				try
				{
					TGEDCOMCustomEvent @event;
					if (aEvent != null)
					{
						@event = aEvent;
					}
					else
					{
						if (aRecord is TGEDCOMIndividualRecord)
						{
							@event = new TGEDCOMIndividualEvent(this.FTree, aRecord, "", "");
						}
						else
						{
							@event = new TGEDCOMFamilyEvent(this.FTree, aRecord, "", "");
						}
					}
					fmEventEdit.Event = @event;
					DialogResult dialogResult = GKL.fmGEDKeeper.ShowModalEx(fmEventEdit, aSender, true);
					if (dialogResult != DialogResult.OK)
					{
						if (dialogResult == DialogResult.Cancel)
						{
							if (aEvent == null)
							{
								@event.Dispose();
							}
						}
					}
					else
					{
						if (aEvent == null)
						{
							@event = fmEventEdit.Event;
							if (aRecord is TGEDCOMIndividualRecord)
							{
								(aRecord as TGEDCOMIndividualRecord).AddIndividualEvent(@event);
							}
							else
							{
								(aRecord as TGEDCOMFamilyRecord).AddFamilyEvent(@event as TGEDCOMFamilyEvent);
							}
						}
						else
						{
							if (aRecord is TGEDCOMIndividualRecord && !object.Equals(fmEventEdit.Event, aEvent))
							{
								(aRecord as TGEDCOMIndividualRecord).DeleteIndividualEvent(aEvent);
								@event = fmEventEdit.Event;
								(aRecord as TGEDCOMIndividualRecord).AddIndividualEvent(@event);
							}
						}
						Result = true;
						this.Modified = true;
					}
				}
				finally
				{
					TObjectHelper.Free(fmEventEdit);
				}
			}
			return Result;
		}
		public bool ModifyRecMultimedia(Form aSender, TGEDCOMRecord aRecord, TGEDCOMMultimediaLink aLink, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[241]) != DialogResult.No)
				{
					aRecord.DeleteMultimediaLink(aLink);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit && aLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = aLink.Value as TGEDCOMMultimediaRecord;
					Result = this.ModifyMedia(ref mmRec);
				}
				else
				{
					TGEDCOMRecord.TGEDCOMRecordType arg_5C_1 = TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia;
					object[] anArgs = new object[0];
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(arg_5C_1, anArgs) as TGEDCOMMultimediaRecord;
					if (mmRec != null)
					{
						TGEDCOMMultimediaLink mmLink = new TGEDCOMMultimediaLink(this.FTree, aRecord, "", "");
						mmLink.Value = mmRec;
						aRecord.AddMultimediaLink(mmLink);
						Result = true;
					}
				}
			}
			return Result;
		}
		public bool ModifyRecNote(Form aSender, TGEDCOMRecord aRecord, TGEDCOMNotes aNote, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[240]) != DialogResult.No)
				{
					aRecord.DeleteNotes(aNote);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit && aNote != null)
				{
					TGEDCOMNoteRecord noteRec = aNote.Value as TGEDCOMNoteRecord;
					Result = this.ModifyNote(ref noteRec);
				}
				else
				{
					TGEDCOMRecord.TGEDCOMRecordType arg_5C_1 = TGEDCOMRecord.TGEDCOMRecordType.rtNote;
					object[] anArgs = new object[0];
					TGEDCOMNoteRecord noteRec = this.SelectRecord(arg_5C_1, anArgs) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, aRecord, "", "");
						note.Value = noteRec;
						aRecord.AddNotes(note);
						Result = true;
					}
				}
			}
			return Result;
		}
		public bool ModifyRecSource(Form aSender, TGEDCOMRecord aRecord, TGEDCOMSourceCitation aCit, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[242]) != DialogResult.No)
				{
					aRecord.DeleteSourceCitation(aCit);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmSourceCitEdit fmSrcCitEdit = new TfmSourceCitEdit(this);
				try
				{
					TGEDCOMSourceCitation cit;
					if (anAction == TGenEngine.TRecAction.raEdit && aCit != null)
					{
						cit = aCit;
					}
					else
					{
						cit = new TGEDCOMSourceCitation(this.FTree, aRecord, "", "");
					}
					fmSrcCitEdit.SourceCitation = cit;
					DialogResult res = GKL.fmGEDKeeper.ShowModalEx(fmSrcCitEdit, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.AddSourceCitation(cit);
						}
						else
						{
							cit.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					TObjectHelper.Free(fmSrcCitEdit);
				}
			}
			return Result;
		}
		public bool ModifyRecUserRef(Form aSender, TGEDCOMRecord aRecord, TGEDCOMUserReference aUserRef, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[244]) != DialogResult.No)
				{
					aRecord.DeleteUserReference(aUserRef);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmUserRefEdit dlg = new TfmUserRefEdit(this);
				try
				{
					TGEDCOMUserReference @ref;
					if (anAction == TGenEngine.TRecAction.raEdit && aUserRef != null)
					{
						@ref = aUserRef;
					}
					else
					{
						@ref = new TGEDCOMUserReference(this.FTree, aRecord, "", "");
					}
					dlg.UserRef = @ref;
					DialogResult res = GKL.fmGEDKeeper.ShowModalEx(dlg, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.AddUserReference(@ref);
						}
						else
						{
							@ref.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			return Result;
		}
		public bool ModifyTagMultimedia(TGEDCOMTagWithLists aTag, TGEDCOMMultimediaLink aLink, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[241]) != DialogResult.No)
				{
					aTag.DeleteMultimediaLink(aLink);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit)
				{
					if (aLink != null)
					{
						TGEDCOMMultimediaRecord mmRec = aLink.Value as TGEDCOMMultimediaRecord;
						Result = this.ModifyMedia(ref mmRec);
						this.Modified |= Result;
					}
				}
				else
				{
					TGEDCOMRecord.TGEDCOMRecordType arg_6B_1 = TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia;
					object[] anArgs = new object[0];
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(arg_6B_1, anArgs) as TGEDCOMMultimediaRecord;
					if (mmRec != null)
					{
						TGEDCOMMultimediaLink mmLink = new TGEDCOMMultimediaLink(this.FTree, aTag, "", "");
						mmLink.Value = mmRec;
						aTag.AddMultimediaLink(mmLink);
						this.Modified = true;
						Result = true;
					}
				}
			}
			return Result;
		}
		public bool ModifyTagNote(TGEDCOMTagWithLists aTag, TGEDCOMNotes aNote, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[240]) != DialogResult.No)
				{
					aTag.DeleteNotes(aNote);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit)
				{
					if (aNote != null)
					{
						TGEDCOMNoteRecord noteRec = aNote.Value as TGEDCOMNoteRecord;
						Result = this.ModifyNote(ref noteRec);
					}
				}
				else
				{
					TGEDCOMRecord.TGEDCOMRecordType arg_5A_1 = TGEDCOMRecord.TGEDCOMRecordType.rtNote;
					object[] anArgs = new object[0];
					TGEDCOMNoteRecord noteRec = this.SelectRecord(arg_5A_1, anArgs) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, aTag, "", "");
						note.Value = noteRec;
						aTag.AddNotes(note);
						Result = true;
					}
				}
			}
			return Result;
		}
		public bool ModifyTagSource(TGEDCOMTagWithLists aTag, TGEDCOMSourceCitation aCit, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGKSys.ShowQuestion(GKL.LSList[242]) != DialogResult.No)
				{
					aTag.DeleteSourceCitation(aCit);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				TfmSourceCitEdit fmSrcCitEdit = new TfmSourceCitEdit(this);
				try
				{
					TGEDCOMSourceCitation cit;
					if (anAction == TGenEngine.TRecAction.raEdit && aCit != null)
					{
						cit = aCit;
					}
					else
					{
						cit = new TGEDCOMSourceCitation(this.FTree, aTag, "", "");
					}
					fmSrcCitEdit.SourceCitation = cit;
					DialogResult res = GKL.fmGEDKeeper.ShowModalEx(fmSrcCitEdit, GKL.fmGEDKeeper, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aTag.AddSourceCitation(cit);
						}
						else
						{
							cit.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					TObjectHelper.Free(fmSrcCitEdit);
				}
			}
			return Result;
		}
		public void RecListAssociationsRefresh(TGEDCOMIndividualRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.AssociationsCount != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[154] + ":");
					}
					int arg_51_0 = 0;
					int num = aRecord.AssociationsCount - 1;
					int idx = arg_51_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMAssociation ast = aRecord.GetAssociation(idx);
							string nm = TGenEngine.GetNameStr(ast.Individual, true, false);
							if (aList == null && aSummary != null)
							{
								aSummary.Add("    " + ast.Relation + " " + TGenEngine.HyperLink(ast.Individual.XRef, nm, 0));
							}
							if (aList != null)
							{
								TExtListItem item = aList.AddItem(ast.Relation, ast);
								item.SubItems.Add(nm);
							}
							idx++;
						}
						while (idx != num);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListAssociationsRefresh(): " + E.Message);
			}
		}
		public void RecListFamilyEventsRefresh(TGEDCOMFamilyRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.GetFamilyEventCount() != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[83] + ":");
					}
					int arg_4C_0 = 0;
					int num = aRecord.GetFamilyEventCount() - 1;
					int idx = arg_4C_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMFamilyEvent @event = aRecord.GetFamilyEvent(idx);
							int ev = TGenEngine.GetFamilyEventIndex(@event.Name);
							string st;
							if (ev == 0)
							{
								st = @event.Detail.Classification;
							}
							else
							{
								if (ev > 0)
								{
									st = GKL.LSList[(int)TGenEngine.FamilyEvents[ev].Name - 1];
								}
								else
								{
									st = @event.Name;
								}
							}
							if (aSummary != null)
							{
								aSummary.Add(st + ": " + TGenEngine.GetEventDesc(@event.Detail));
								this.ShowDetailCause(@event.Detail, aSummary);
							}
							this.ShowDetailInfo(@event.Detail, aSummary);
							if (aList != null)
							{
								TExtListItem item = aList.AddItem(Convert.ToString(idx + 1), @event);
								item.SubItems.Add(st);
								item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, GKL.fmGEDKeeper.Options.DefDateFormat, false));
								item.SubItems.Add(@event.Detail.Place.StringValue);
								item.SubItems.Add(TGenEngine.GetEventCause(@event.Detail));
							}
							idx++;
						}
						while (idx != num);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListFamilyEventsRefresh(): " + E.Message);
			}
		}
		public void RecListGroupsRefresh(TGEDCOMIndividualRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.GroupsCount != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[58] + ":");
					}
					int arg_4E_0 = 0;
					int num = aRecord.GroupsCount - 1;
					int idx = arg_4E_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMPointer ptr = aRecord.GetGroup(idx);
							TGEDCOMGroupRecord grp = ptr.Value as TGEDCOMGroupRecord;
							if (grp != null)
							{
								if (aList == null && aSummary != null)
								{
									aSummary.Add("    " + TGenEngine.HyperLink(grp.XRef, grp.Name, 0));
								}
								if (aList != null)
								{
									TExtListItem item = aList.AddItem(grp.Name, grp);
								}
							}
							idx++;
						}
						while (idx != num);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListGroupsRefresh(): " + E.Message);
			}
		}
		public void RecListIndividualEventsRefresh(TGEDCOMIndividualRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.IndividualEventsCount != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[83] + ":");
					}
					int arg_4C_0 = 0;
					int num = aRecord.IndividualEventsCount - 1;
					int idx = arg_4C_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMCustomEvent @event = aRecord.GetIndividualEvent(idx);
							int ev = TGenEngine.GetPersonEventIndex(@event.Name);
							string st;
							if (ev == 0)
							{
								st = @event.Detail.Classification;
							}
							else
							{
								if (ev > 0)
								{
									st = GKL.LSList[(int)TGenEngine.PersonEvents[ev].Name - 1];
								}
								else
								{
									st = @event.Name;
								}
							}
							if (aSummary != null)
							{
								aSummary.Add(st + ": " + TGenEngine.GetEventDesc(@event.Detail));
								if (BDSSystem.WStrCmp(@event.StringValue, "") != 0)
								{
									aSummary.Add("    " + @event.StringValue);
								}
								this.ShowDetailCause(@event.Detail, aSummary);
								this.ShowAddress(@event.Detail.Address, aSummary);
								this.ShowDetailInfo(@event.Detail, aSummary);
							}
							if (aList != null)
							{
								TExtListItem item = aList.AddItem(Convert.ToString(idx + 1), @event);
								item.SubItems.Add(st);
								item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, GKL.fmGEDKeeper.Options.DefDateFormat, false));
								st = @event.Detail.Place.StringValue;
								if (BDSSystem.WStrCmp(@event.StringValue, "") != 0)
								{
									st = st + " [" + @event.StringValue + "]";
								}
								item.SubItems.Add(st);
								item.SubItems.Add(TGenEngine.GetEventCause(@event.Detail));
							}
							idx++;
						}
						while (idx != num);
					}
					if (aList != null)
					{
						aList.ResizeColumn(2);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListIndividualEventsRefresh(): " + E.Message);
			}
		}
		public void RecListMediaRefresh(TGEDCOMRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.GetMultimediaLinksCount() != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[55] + " (" + aRecord.GetMultimediaLinksCount().ToString() + "):");
					}
					int arg_62_0 = 0;
					int num = aRecord.GetMultimediaLinksCount() - 1;
					int idx = arg_62_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMMultimediaLink mmLink = aRecord.GetMultimediaLink(idx);
							TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
							if (mmRec != null && mmRec.GetFileReferencesCount() != 0)
							{
								string st = mmRec.GetFileReference(0).Title;
								if (aList == null && aSummary != null)
								{
									aSummary.Add(string.Concat(new string[]
									{
										"  ", 
										TGenEngine.HyperLink(mmRec.XRef, st, 0), 
										" (", 
										TGenEngine.HyperLink("view_" + mmRec.XRef, "просмотр", 0), 
										")"
									}));
								}
								if (aList != null)
								{
									aList.AddItem(st, mmLink);
								}
							}
							idx++;
						}
						while (idx != num);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListMediaRefresh(): " + E.Message);
			}
		}
		public void RecListNotesRefresh(TGEDCOMRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.GetNotesCount() != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[54] + " (" + aRecord.GetNotesCount().ToString() + "):");
					}
					int arg_60_0 = 0;
					int num = aRecord.GetNotesCount() - 1;
					int idx = arg_60_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMNotes note = aRecord.GetNote(idx);
							if (aSummary != null)
							{
								int arg_89_0 = 0;
								int num2 = note.Notes.Count - 1;
								int i = arg_89_0;
								if (num2 >= i)
								{
									num2++;
									do
									{
										string st = note.Notes[i];
										aSummary.Add(st);
										i++;
									}
									while (i != num2);
								}
								if (idx < aRecord.GetNotesCount() - 1)
								{
									aSummary.Add("");
								}
							}
							if (aList != null)
							{
								aList.AddItem(note.Notes.Text.Trim(), note);
							}
							idx++;
						}
						while (idx != num);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListNotesRefresh(): " + E.Message);
			}
		}
		public void RecListSourcesRefresh(TGEDCOMRecord aRecord, TGKListView aList, TStrings aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.GetSourceCitationsCount() != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[56] + " (" + aRecord.GetSourceCitationsCount().ToString() + "):");
					}
					int arg_62_0 = 0;
					int num = aRecord.GetSourceCitationsCount() - 1;
					int idx = arg_62_0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMSourceCitation cit = aRecord.GetSourceCitation(idx);
							TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
							if (sourceRec != null)
							{
								string nm = "\"" + sourceRec.FiledByEntry + "\"";
								if (BDSSystem.WStrCmp(cit.Page, "") != 0)
								{
									nm = nm + ", " + cit.Page;
								}
								if (aList == null && aSummary != null)
								{
									aSummary.Add("  " + TGenEngine.HyperLink(sourceRec.XRef, nm, 0));
								}
								if (aList != null)
								{
									TExtListItem item = aList.AddItem(sourceRec.Originator.Text.Trim(), cit);
									item.SubItems.Add(nm);
								}
							}
							idx++;
						}
						while (idx != num);
					}
					if (aList != null)
					{
						aList.ResizeColumn(1);
					}
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.RecListSourcesRefresh(): " + E.Message);
			}
		}
		public void SetupRecEventsList(TSheetList aList, bool PersonsMode)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn("№", 25, false);
			aList.AddColumn(GKL.LSList[203], 90, false);
			aList.AddColumn(GKL.LSList[139], 80, false);
			if (!PersonsMode)
			{
				aList.AddColumn(GKL.LSList[204], 200, false);
			}
			else
			{
				aList.AddColumn(GKL.LSList[235], 200, false);
			}
			aList.AddColumn(GKL.LSList[205], 130, false);
			aList.Columns_EndUpdate();
		}
		public void SetupRecMediaList(TSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(GKL.LSList[55], 300, false);
			aList.Columns_EndUpdate();
		}
		public void SetupRecNotesList(TSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(GKL.LSList[108], 300, false);
			aList.Columns_EndUpdate();
		}
		public void SetupRecSourcesList(TSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(GKL.LSList[142], 120, false);
			aList.AddColumn(GKL.LSList[125], 180, false);
			aList.Columns_EndUpdate();
		}
		public void ShowDetailCause(TGEDCOMEventDetail aDetail, TStrings aSummary)
		{
			string cause = TGenEngine.GetEventCause(aDetail);
			if (aSummary != null && BDSSystem.WStrCmp(cause, "") != 0)
			{
				aSummary.Add("    " + cause);
			}
		}
		public void ShowDetailInfo(TGEDCOMEventDetail aDetail, TStrings aSummary)
		{
			if (aSummary != null && aDetail.GetSourceCitationsCount() != 0)
			{
				aSummary.Add(string.Concat(new string[]
				{
					"    ", 
					GKL.LSList[56], 
					" (", 
					aDetail.GetSourceCitationsCount().ToString(),
					"):"
				}));
				int arg_62_0 = 0;
				int num = aDetail.GetSourceCitationsCount() - 1;
				int idx = arg_62_0;
				if (num >= idx)
				{
					num++;
					do
					{
						TGEDCOMSourceCitation cit = aDetail.GetSourceCitation(idx);
						TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
						if (sourceRec != null)
						{
							string nm = "\"" + sourceRec.FiledByEntry + "\"";
							if (BDSSystem.WStrCmp(cit.Page, "") != 0)
							{
								nm = nm + ", " + cit.Page;
							}
							aSummary.Add("      " + TGenEngine.HyperLink(sourceRec.XRef, nm, 0));
						}
						idx++;
					}
					while (idx != num);
				}
			}
		}
		public void ShowFamilyInfo(TGEDCOMFamilyRecord aFamily, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aFamily != null)
					{
						aSummary.Add("");
						TGEDCOMIndividualRecord irec = aFamily.Husband.Value as TGEDCOMIndividualRecord;
						string st;
						if (irec != null)
						{
							st = TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0);
						}
						else
						{
							st = GKL.LSList[64];
						}
						aSummary.Add(GKL.LSList[115] + ": " + st + TGenEngine.GetLifeStr(irec));
						irec = (aFamily.Wife.Value as TGEDCOMIndividualRecord);
						if (irec != null)
						{
							st = TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0);
						}
						else
						{
							st = GKL.LSList[63];
						}
						aSummary.Add(GKL.LSList[116] + ": " + st + TGenEngine.GetLifeStr(irec));
						aSummary.Add("");
						if (aFamily.ChildrenCount != 0)
						{
							aSummary.Add(GKL.LSList[118] + ":");
						}
						int arg_121_0 = 0;
						int num = aFamily.ChildrenCount - 1;
						int i = arg_121_0;
						if (num >= i)
						{
							num++;
							do
							{
								irec = (aFamily.GetChildren(i).Value as TGEDCOMIndividualRecord);
								aSummary.Add("    " + TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0) + TGenEngine.GetLifeStr(irec));
								i++;
							}
							while (i != num);
						}
						aSummary.Add("");
						this.RecListFamilyEventsRefresh(aFamily, null, aSummary);
						this.RecListNotesRefresh(aFamily, null, aSummary);
						this.RecListMediaRefresh(aFamily, null, aSummary);
						this.RecListSourcesRefresh(aFamily, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowFamilyInfo(): " + E.Message);
			}
		}
		public void ShowGroupInfo(TGEDCOMGroupRecord aGroup, TStrings aSummary)
		{
			try
			{
				TStringList mbrList = new TStringList();
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aGroup != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aGroup.Name + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[126] + " (" + aGroup.GetMembersCount().ToString() + "):");
						int arg_83_0 = 0;
						int num = aGroup.GetMembersCount() - 1;
						int i = arg_83_0;
						if (num >= i)
						{
							num++;
							do
							{
								TGEDCOMIndividualRecord member = aGroup.GetMember(i).Value as TGEDCOMIndividualRecord;
								mbrList.AddObject(TGenEngine.GetNameStr(member, true, false), member);
								i++;
							}
							while (i != num);
						}
						mbrList.Sort();
						int arg_CC_0 = 0;
						int num2 = mbrList.Count - 1;
						i = arg_CC_0;
						if (num2 >= i)
						{
							num2++;
							do
							{
								TGEDCOMIndividualRecord member = mbrList.GetObject(i) as TGEDCOMIndividualRecord;
								aSummary.Add("    " + TGenEngine.HyperLink(member.XRef, mbrList[i], i + 1));
								i++;
							}
							while (i != num2);
						}
						this.RecListNotesRefresh(aGroup, null, aSummary);
						this.RecListMediaRefresh(aGroup, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
					mbrList.Free();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowGroupInfo(): " + E.Message);
			}
		}
		public void ShowMultimediaInfo(TGEDCOMMultimediaRecord aMultimediaRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aMultimediaRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aMultimediaRec.GetFileReference(0).Title + "~bu-1~");
						aSummary.Add("");
						aSummary.Add("[ " + TGenEngine.HyperLink("view_" + aMultimediaRec.XRef, GKL.LSList[148], 0) + " ]");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[234] + ":");
						int arg_BF_0 = 0;
						int num = this.FTree.RecordsCount - 1;
						int i = arg_BF_0;
						if (num >= i)
						{
							num++;
							do
							{
								this.SearchSubjectLinks(this.FTree.GetRecord(i), aMultimediaRec, aSummary);
								i++;
							}
							while (i != num);
						}
						this.RecListNotesRefresh(aMultimediaRec, null, aSummary);
						this.RecListSourcesRefresh(aMultimediaRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowMultimediaInfo(): " + E.Message);
			}
		}
		public void ShowNoteInfo(TGEDCOMNoteRecord aNoteRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aNoteRec != null)
					{
						aSummary.Add("");
						aSummary.AddStrings(aNoteRec.Notes);
						aSummary.Add("");
						aSummary.Add(GKL.LSList[234] + ":");
						int arg_5E_0 = 0;
						int num = this.FTree.RecordsCount - 1;
						int i = arg_5E_0;
						if (num >= i)
						{
							num++;
							do
							{
								this.SearchSubjectLinks(this.FTree.GetRecord(i), aNoteRec, aSummary);
								i++;
							}
							while (i != num);
						}
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowNoteInfo(): " + E.Message);
			}
		}
		public void ShowPersonInfo(TGEDCOMIndividualRecord iRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				aSummary.Clear();
				try
				{
					if (iRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + TGenEngine.GetNameStr(iRec, true, true) + "~bu-1~");
						aSummary.Add(GKL.LSList[87] + ": " + TGenEngine.SexStr(iRec.Sex));
						try
						{
							if (iRec.ChildToFamilyLinksCount != 0)
							{
								aSummary.Add("");
								aSummary.Add(GKL.LSList[152] + ":");
								TGEDCOMFamilyRecord family = iRec.GetChildToFamilyLink(0).Family;
								TGEDCOMIndividualRecord rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
								string st;
								if (rel_person != null)
								{
									st = TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0);
								}
								else
								{
									st = GKL.LSList[64];
								}
								aSummary.Add(string.Concat(new string[]
								{
									"  ", 
									GKL.LSList[150], 
									": ", 
									st, 
									TGenEngine.GetLifeStr(rel_person)
								}));
								rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
								if (rel_person != null)
								{
									st = TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0);
								}
								else
								{
									st = GKL.LSList[63];
								}
								aSummary.Add(string.Concat(new string[]
								{
									"  ", 
									GKL.LSList[151], 
									": ", 
									st, 
									TGenEngine.GetLifeStr(rel_person)
								}));
							}
						}
						catch (Exception E)
						{
							TGKSys.LogWrite("GKBase.ShowPersonInfo().Parents(): " + E.Message);
						}
						try
						{
							int arg_1EA_0 = 0;
							int num = iRec.SpouseToFamilyLinksCount - 1;
							int idx = arg_1EA_0;
							if (num >= idx)
							{
								num++;
								do
								{
									TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(idx).Family;
									if (family == null)
									{
										TGKSys.LogWrite(string.Concat(new string[]
										{
											"File (", 
											this.FileName, 
											"), iRec (", 
											iRec.XRef, 
											"): empty family entry"
										}));
									}
									else
									{
										if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
										{
											string st;
											TGEDCOMPointer sp;
											string unk;
											if (iRec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
											{
												sp = family.Wife;
												st = GKL.LSList[116] + ": ";
												unk = GKL.LSList[63];
											}
											else
											{
												sp = family.Husband;
												st = GKL.LSList[115] + ": ";
												unk = GKL.LSList[64];
											}
											string marr = TGenEngine.GetMarriageDate(family, TGenEngine.TDateFormat.dfDD_MM_YYYY);
											if (BDSSystem.WStrCmp(marr, "") != 0)
											{
												marr = GKL.LSList[236] + " " + marr;
											}
											else
											{
												marr = GKL.LSList[237];
											}
											TGEDCOMIndividualRecord rel_person = sp.Value as TGEDCOMIndividualRecord;
											aSummary.Add("");
											if (rel_person != null)
											{
												st = string.Concat(new string[]
												{
													st, 
													TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0), 
													" (", 
													TGenEngine.HyperLink(family.XRef, marr, 0), 
													")"
												});
											}
											else
											{
												st = string.Concat(new string[]
												{
													st, 
													unk, 
													" (", 
													TGenEngine.HyperLink(family.XRef, marr, 0), 
													")"
												});
											}
											aSummary.Add(st);
											if (family.ChildrenCount != 0)
											{
												aSummary.Add("");
												aSummary.Add(GKL.LSList[118] + ":");
											}
											int arg_3FA_0 = 0;
											int num2 = family.ChildrenCount - 1;
											int i = arg_3FA_0;
											if (num2 >= i)
											{
												num2++;
												do
												{
													rel_person = (family.GetChildren(i).Value as TGEDCOMIndividualRecord);
													aSummary.Add("    " + TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0) + TGenEngine.GetLifeStr(rel_person));
													i++;
												}
												while (i != num2);
											}
										}
									}
									idx++;
								}
								while (idx != num);
							}
						}
						catch (Exception E)
						{
							TGKSys.LogWrite("GKBase.ShowPersonInfo().Families(): " + E.Message);
						}
						this.RecListIndividualEventsRefresh(iRec, null, aSummary);
						this.RecListNotesRefresh(iRec, null, aSummary);
						this.RecListMediaRefresh(iRec, null, aSummary);
						this.RecListSourcesRefresh(iRec, null, aSummary);
						this.RecListAssociationsRefresh(iRec, null, aSummary);
						this.RecListGroupsRefresh(iRec, null, aSummary);
						TStringList namesakes = new TStringList();
						try
						{
							string st = TGenEngine.GetNameStr(iRec, true, false);
							int arg_4F1_0 = 0;
							int num3 = this.FTree.RecordsCount - 1;
							int i = arg_4F1_0;
							if (num3 >= i)
							{
								num3++;
								do
								{
									TGEDCOMRecord rec = this.FTree.GetRecord(i);
									if (rec is TGEDCOMIndividualRecord && !object.Equals(rec, iRec))
									{
										TGEDCOMIndividualRecord rel_person = (TGEDCOMIndividualRecord)rec;
										string unk = TGenEngine.GetNameStr(rel_person, true, false);
										if (BDSSystem.WStrCmp(st, unk) == 0)
										{
											namesakes.AddObject(unk + TGenEngine.GetLifeStr(rel_person), rel_person);
										}
									}
									i++;
								}
								while (i != num3);
							}
							if (namesakes.Count > 0)
							{
								aSummary.Add("");
								aSummary.Add(GKL.LSList[238] + ":");
								int arg_5A5_0 = 0;
								int num4 = namesakes.Count - 1;
								i = arg_5A5_0;
								if (num4 >= i)
								{
									num4++;
									do
									{
										TGEDCOMIndividualRecord rel_person = (TGEDCOMIndividualRecord)namesakes.GetObject(i);
										aSummary.Add("    " + TGenEngine.HyperLink(rel_person.XRef, namesakes[i], 0));
										i++;
									}
									while (i != num4);
								}
							}
						}
						finally
						{
							namesakes.Free();
						}
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowPersonInfo(): " + E.Message);
			}
		}
		public void ShowSourceInfo(TGEDCOMSourceRecord aSourceRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				TStringList link_list = new TStringList();
				try
				{
					aSummary.Clear();
					if (aSourceRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aSourceRec.FiledByEntry + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[142] + ": " + aSourceRec.Originator.Text.Trim());
						aSummary.Add(GKL.LSList[125] + ": \"" + aSourceRec.Title.Text.Trim() + "\"");
						aSummary.Add(GKL.LSList[143] + ": \"" + aSourceRec.Publication.Text.Trim() + "\"");
						if (aSourceRec.GetRepositoryCitationsCount() > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[57] + ":");
							int arg_113_0 = 0;
							int num = aSourceRec.GetRepositoryCitationsCount() - 1;
							int i = arg_113_0;
							if (num >= i)
							{
								num++;
								do
								{
									TGEDCOMRepositoryRecord rep = aSourceRec.GetRepositoryCitation(i).Value as TGEDCOMRepositoryRecord;
									aSummary.Add("    " + TGenEngine.HyperLink(rep.XRef, rep.RepositoryName, 0));
									i++;
								}
								while (i != num);
							}
						}
						aSummary.Add("");
						aSummary.Add(GKL.LSList[234] + ":");
						int arg_1A2_0 = 0;
						int num2 = this.FTree.RecordsCount - 1;
						int j = arg_1A2_0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								this.SearchSubjectLinks(this.FTree.GetRecord(j), aSourceRec, link_list);
								j++;
							}
							while (j != num2);
						}
						link_list.Sort();
						int arg_1D7_0 = 0;
						int num3 = link_list.Count - 1;
						j = arg_1D7_0;
						if (num3 >= j)
						{
							num3++;
							do
							{
								aSummary.Add(link_list[j]);
								j++;
							}
							while (j != num3);
						}
						this.RecListNotesRefresh(aSourceRec, null, aSummary);
						this.RecListMediaRefresh(aSourceRec, null, aSummary);
					}
				}
				finally
				{
					link_list.Free();
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowSourceInfo(): " + E.Message);
			}
		}
		public void ShowRepositoryInfo(TGEDCOMRepositoryRecord aRepositoryRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aRepositoryRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aRepositoryRec.RepositoryName.Trim() + "~bu-1~");
						aSummary.Add("");
						this.ShowAddress(aRepositoryRec.Address, aSummary);
						aSummary.Add("");
						aSummary.Add(GKL.LSList[56] + ":");
						int arg_8D_0 = 0;
						int num = this.FTree.RecordsCount - 1;
						int i = arg_8D_0;
						if (num >= i)
						{
							num++;
							do
							{
								TGEDCOMRecord rec = this.FTree.GetRecord(i);
								if (rec is TGEDCOMSourceRecord)
								{
									TGEDCOMSourceRecord srcRec = (TGEDCOMSourceRecord)rec;
									int arg_C5_0 = 0;
									int num2 = srcRec.GetRepositoryCitationsCount() - 1;
									int j = arg_C5_0;
									if (num2 >= j)
									{
										num2++;
										do
										{
											if (object.Equals(srcRec.GetRepositoryCitation(j).Value, aRepositoryRec))
											{
												aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, srcRec, false));
											}
											j++;
										}
										while (j != num2);
									}
								}
								i++;
							}
							while (i != num);
						}
						this.RecListNotesRefresh(aRepositoryRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowRepositoryInfo(): " + E.Message);
			}
		}
		public void ShowResearchInfo(TGEDCOMResearchRecord aResearchRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aResearchRec != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[125] + ": \"~ub+1~" + aResearchRec.Name.Trim() + "~bu-1~\"");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[178] + ": " + GKL.LSList[(int)TGenEngine.PriorityNames[(int)aResearchRec.Priority] - 1]);
						aSummary.Add(string.Concat(new string[]
						{
							GKL.LSList[117], 
							": ", 
							GKL.LSList[(int)TGenEngine.StatusNames[(int)aResearchRec.Status] - 1], 
							" (", 
							aResearchRec.Percent.ToString(), 
							"%)"
						}));
						aSummary.Add(GKL.LSList[180] + ": " + TGenEngine.GEDCOMDateToStr(aResearchRec.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						aSummary.Add(GKL.LSList[181] + ": " + TGenEngine.GEDCOMDateToStr(aResearchRec.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						if (aResearchRec.TasksCount > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[60] + ":");
							int arg_177_0 = 0;
							int num = aResearchRec.TasksCount - 1;
							int i = arg_177_0;
							if (num >= i)
							{
								num++;
								do
								{
									TGEDCOMTaskRecord taskRec = aResearchRec.GetTask(i).Value as TGEDCOMTaskRecord;
									aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, taskRec, false));
									i++;
								}
								while (i != num);
							}
						}
						if (aResearchRec.CommunicationsCount > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[61] + ":");
							int arg_1F7_0 = 0;
							int num2 = aResearchRec.CommunicationsCount - 1;
							int i = arg_1F7_0;
							if (num2 >= i)
							{
								num2++;
								do
								{
									TGEDCOMCommunicationRecord corrRec = aResearchRec.GetCommunication(i).Value as TGEDCOMCommunicationRecord;
									aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, corrRec, false));
									i++;
								}
								while (i != num2);
							}
						}
						if (aResearchRec.GroupsCount != 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[58] + ":");
							int arg_277_0 = 0;
							int num3 = aResearchRec.GroupsCount - 1;
							int i = arg_277_0;
							if (num3 >= i)
							{
								num3++;
								do
								{
									TGEDCOMGroupRecord grp = aResearchRec.GetGroup(i).Value as TGEDCOMGroupRecord;
									aSummary.Add("    " + TGenEngine.HyperLink(grp.XRef, grp.Name, 0));
									i++;
								}
								while (i != num3);
							}
						}
						this.RecListNotesRefresh(aResearchRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowResearchInfo(): " + E.Message);
			}
		}
		public void ShowTaskInfo(TGEDCOMTaskRecord aTaskRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aTaskRec != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[182] + ": ~ub+1~" + TGenEngine.GetTaskGoalStr(this.FTree, aTaskRec) + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[178] + ": " + GKL.LSList[(int)TGenEngine.PriorityNames[(int)aTaskRec.Priority] - 1]);
						aSummary.Add(GKL.LSList[180] + ": " + TGenEngine.GEDCOMDateToStr(aTaskRec.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						aSummary.Add(GKL.LSList[181] + ": " + TGenEngine.GEDCOMDateToStr(aTaskRec.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						this.RecListNotesRefresh(aTaskRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowTaskInfo(): " + E.Message);
			}
		}
		public void ShowCommunicationInfo(TGEDCOMCommunicationRecord aCommunicationRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aCommunicationRec != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[183] + ": \"~ub+1~" + aCommunicationRec.Name.Trim() + "~bu-1~\"");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[184] + ": " + TGenEngine.GetCorresponderStr(this.FTree, aCommunicationRec, true));
						aSummary.Add(GKL.LSList[113] + ": " + GKL.LSList[(int)TGenEngine.CommunicationNames[(int)aCommunicationRec.CommunicationType] - 1]);
						aSummary.Add(GKL.LSList[139] + ": " + TGenEngine.GEDCOMDateToStr(aCommunicationRec.Date, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						this.RecListNotesRefresh(aCommunicationRec, null, aSummary);
						this.RecListMediaRefresh(aCommunicationRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowCommunicationInfo(): " + E.Message);
			}
		}
		public void ShowLocationInfo(TGEDCOMLocationRecord aLocationRec, TStrings aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				TStringList link_list = new TStringList();
				try
				{
					aSummary.Clear();
					if (aLocationRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aLocationRec.Name.Trim() + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[171] + ": " + aLocationRec.Map.Lati);
						aSummary.Add(GKL.LSList[172] + ": " + aLocationRec.Map.Long);
						TGenEngine.GetLocationLinks(this.FTree, aLocationRec, ref link_list);
						link_list.Sort();
						if (link_list.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[234] + ":");
							int arg_F3_0 = 0;
							int num = link_list.Count - 1;
							int i = arg_F3_0;
							if (num >= i)
							{
								num++;
								do
								{
									aSummary.Add("    " + link_list[i]);
									i++;
								}
								while (i != num);
							}
						}
						this.RecListNotesRefresh(aLocationRec, null, aSummary);
						this.RecListMediaRefresh(aLocationRec, null, aSummary);
					}
				}
				finally
				{
					link_list.Free();
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKBase.ShowLocationInfo(): " + E.Message);
			}
		}
		void ILocalization.SetLang()
		{
			this.PageRecords.TabPages[0].Text = GKL.LSList[52];
			this.PageRecords.TabPages[1].Text = GKL.LSList[53];
			this.PageRecords.TabPages[2].Text = GKL.LSList[54];
			this.PageRecords.TabPages[3].Text = GKL.LSList[55];
			this.PageRecords.TabPages[4].Text = GKL.LSList[56];
			this.PageRecords.TabPages[5].Text = GKL.LSList[57];
			this.PageRecords.TabPages[6].Text = GKL.LSList[58];
			this.PageRecords.TabPages[7].Text = GKL.LSList[59];
			this.PageRecords.TabPages[8].Text = GKL.LSList[60];
			this.PageRecords.TabPages[9].Text = GKL.LSList[61];
			this.PageRecords.TabPages[10].Text = GKL.LSList[62];
		}
		public void TimeLine_Init()
		{
			this.FXFilter.LifeMode = TGenEngine.TLifeMode.lmTimeLine;
		}
		public void TimeLine_Done()
		{
			this.FXFilter.LifeMode = TGenEngine.TLifeMode.lmAll;
			this.FXFilter.TimeLineYear = -1;
			this.ApplyFilter();
		}
		public int TimeLine_GetYear()
		{
			return this.FXFilter.TimeLineYear;
		}

		public void TimeLine_SetYear(int aYear)
		{
			this.FXFilter.TimeLineYear = aYear;
			this.ApplyFilter();
		}

		private static void _SelectRecordByXRef_SelectItemByRec([In] TfmBase Self, TRecordsView aList, TGEDCOMRecord aRec, int aTab)
		{
			Self.PageRecords.SelectedIndex = aTab;
			Self.PageRecords_SelectedIndexChanged(null, null);
			Self.ActiveControl = aList;
			aList.SelectItemByRec(aRec);
		}
	}
}
