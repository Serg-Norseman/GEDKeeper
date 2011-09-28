using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Commands;
using GKCore.Sys;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Lists;

namespace GKUI
{
	public partial class TfmBase : Form, ILocalization
	{

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
				if ((aExt as TGEDCOMSourceCitation).Page != "")
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
				int num = @event.Detail.Notes.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						if (object.Equals(@event.Detail.Notes[i].Value, aSubject))
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
					int num2 = @event.Detail.MultimediaLinks.Count - 1;
					int i = 0;
					if (num2 >= i)
					{
						num2++;
						do
						{
							if (object.Equals(@event.Detail.MultimediaLinks[i].Value, aSubject))
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
						int num3 = @event.Detail.SourceCitations.Count - 1;
						int i = 0;
						if (num3 >= i)
						{
							num3++;
							do
							{
								if (object.Equals(@event.Detail.SourceCitations[i].Value, aSubject))
								{
									this.OutLink(aSubject, aToList, aRec, @event, @event.Detail.SourceCitations[i]);
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
							if (SysUtils.ShowQuestion(msg) == DialogResult.Yes)
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

			int rt = (int)aRecView.RecordType;
			this.FCounts[rt].Total = aRecView.TotalCount;
			this.FCounts[rt].Filtered = aRecView.FilteredCount;
		}

		private bool IsMainList(TGEDCOMRecordType aRecType, TGKListView aList)
		{
			bool Result = false;
			switch (aRecType)
			{
				case TGEDCOMRecordType.rtIndividual:
				{
					Result = object.Equals(aList, this.ListPersons);
					break;
				}
				case TGEDCOMRecordType.rtFamily:
				{
					Result = object.Equals(aList, this.ListFamilies);
					break;
				}
				case TGEDCOMRecordType.rtNote:
				{
					Result = object.Equals(aList, this.ListNotes);
					break;
				}
				case TGEDCOMRecordType.rtMultimedia:
				{
					Result = object.Equals(aList, this.ListMultimedia);
					break;
				}
				case TGEDCOMRecordType.rtSource:
				{
					Result = object.Equals(aList, this.ListSources);
					break;
				}
				case TGEDCOMRecordType.rtRepository:
				{
					Result = object.Equals(aList, this.ListRepositories);
					break;
				}
				case TGEDCOMRecordType.rtGroup:
				{
					Result = object.Equals(aList, this.ListGroups);
					break;
				}
				case TGEDCOMRecordType.rtResearch:
				{
					Result = object.Equals(aList, this.ListResearches);
					break;
				}
				case TGEDCOMRecordType.rtTask:
				{
					Result = object.Equals(aList, this.ListTasks);
					break;
				}
				case TGEDCOMRecordType.rtCommunication:
				{
					Result = object.Equals(aList, this.ListCommunications);
					break;
				}
				case TGEDCOMRecordType.rtLocation:
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
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
		}

		private void SetFileName([In] string Value)
		{
			this.FEngine.FileName = Value;
			this.SetMainTitle();
			GKUI.TfmGEDKeeper.Instance.Options.LastDir = SysUtils.ExtractFilePath(this.FEngine.FileName);
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
				for (int i = 0; i <= num; i++)
				{
					aSummary.Add("    " + anAddress.GetPhoneNumber(i));
				}

				int num2 = anAddress.GetEmailAddressesCount() - 1;
				for (int i = 0; i <= num2; i++)
				{
					aSummary.Add("    " + anAddress.GetEmailAddress(i));
				}

				int num3 = anAddress.GetWebPagesCount() - 1;
				for (int i = 0; i <= num3; i++)
				{
					aSummary.Add("    " + anAddress.GetWebPage(i));
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
			if (SysUtils.Pos("view_", LinkName) > 0)
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
			GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			if (GKUI.TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				GKUI.TfmGEDKeeper.Instance.fmTimeLine.CheckTimeWin(this);
			}
		}

		private void FormDeactivate(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.UpdateControls(true);
			if (TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				TfmGEDKeeper.Instance.fmTimeLine.CheckTimeWin(null);
			}
		}

		private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.UpdateControls(false);
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
				for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
				{
					this.FChangedRecords[(int)rt].Free();
				}
			}
			base.Dispose(Disposing);
		}

		public TfmBase()
		{
			this.InitializeComponent();
			for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
			{
				this.FChangedRecords[(int)rt] = new TList();
			}
			this.FEngine = new TGenEngine();
			this.FTree = this.FEngine.Tree;
			this.FXFilter = new TPersonsFilter();
			this.FLockedRecords = new TList();
			this.FBackman = new TBackManager();
			this.FUndoman = new TUndoManager(this.FTree, TUndoManager.TUndoManType.manualCommit);
			this.CreatePage(GKL.LSList[52], TGEDCOMRecordType.rtIndividual, ref this.ListPersons, ref this.mPersonSummary);
			this.CreatePage(GKL.LSList[53], TGEDCOMRecordType.rtFamily, ref this.ListFamilies, ref this.mFamilySummary);
			this.CreatePage(GKL.LSList[54], TGEDCOMRecordType.rtNote, ref this.ListNotes, ref this.mNoteSummary);
			this.CreatePage(GKL.LSList[55], TGEDCOMRecordType.rtMultimedia, ref this.ListMultimedia, ref this.mMediaSummary);
			this.CreatePage(GKL.LSList[56], TGEDCOMRecordType.rtSource, ref this.ListSources, ref this.mSourceSummary);
			this.CreatePage(GKL.LSList[57], TGEDCOMRecordType.rtRepository, ref this.ListRepositories, ref this.mRepositorySummary);
			this.CreatePage(GKL.LSList[58], TGEDCOMRecordType.rtGroup, ref this.ListGroups, ref this.mGroupSummary);
			this.CreatePage(GKL.LSList[59], TGEDCOMRecordType.rtResearch, ref this.ListResearches, ref this.mResearchSummary);
			this.CreatePage(GKL.LSList[60], TGEDCOMRecordType.rtTask, ref this.ListTasks, ref this.mTaskSummary);
			this.CreatePage(GKL.LSList[61], TGEDCOMRecordType.rtCommunication, ref this.ListCommunications, ref this.mCommunicationSummary);
			this.CreatePage(GKL.LSList[62], TGEDCOMRecordType.rtLocation, ref this.ListLocations, ref this.mLocationSummary);
			this.PageRecords.SelectedIndex = 0;
		}

		public void ApplyFilter()
		{
			if (this.FTree.RecordsCount > 0)
			{
				this.ListsRefresh(false);
			}
		}

		public void ChangeRecord(TGEDCOMRecord record)
		{
			if (record != null)
			{
				int rt = (int)record.RecordType;
				this.FChangedRecords[rt].Add(record);
				record.ChangeDate.ChangeDateTime = DateTime.Now;
				this.Modified = true;
			}
		}

		public void ChangesClear()
		{
			for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
			{
				this.FChangedRecords[(int)rt].Clear();
			}
		}

		public bool CheckModified()
		{
			bool Result = true;
			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(GKL.LSList[69], "GEDKeeper2", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
				if (dialogResult != DialogResult.Cancel)
				{
					if (dialogResult == DialogResult.Yes)
					{
						GKUI.TfmGEDKeeper.Instance.miFileSaveClick(null, null);
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

		public void CreatePage(string aPageText, TGEDCOMRecordType aRecType, ref TRecordsView aList, ref TGKHyperView aSummary)
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

		public TGEDCOMIndividualRecord CreatePersonDialog(TGEDCOMIndividualRecord aTarget, TGenEngine.TTargetMode aTargetMode, TGEDCOMSex aNeedSex)
		{
			TGEDCOMIndividualRecord Result = null;
			TfmPersonNew dlg = new TfmPersonNew();
			try
			{
				dlg.EditSex.SelectedIndex = (int)aNeedSex;
				dlg.TargetMode = aTargetMode;
				dlg.Target = aTarget;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					Result = TGenEngine.CreatePersonEx(this.FTree, dlg.edName.Text, dlg.edPatronymic.Text, dlg.edFamily.Text, (TGEDCOMSex)dlg.EditSex.SelectedIndex, true);
					this.ChangeRecord(Result);
				}
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}

		public void CreateRecordsView(Control aParent, TGEDCOMRecordType aRecordType, ref TRecordsView aList)
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
				ex_exp.Options = GKUI.TfmGEDKeeper.Instance.Options;
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
			//	SysUtils.LogWrite("GKBase.FileLoad().TreeLoad(): " + E.Message);
			//	SysUtils.ShowError(GKL.LSList[245]);
			//}

			try
			{
				TGenEngine.CheckGEDCOMFormat(this.FTree);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.FileLoad().CheckFormat(): " + E.Message);
				SysUtils.ShowError(GKL.LSList[246]);
			}

			this.FileName = aFileName;
			this.Modified = false;

			TfmGEDKeeper.Instance.NamesTable.ImportNames(this.FTree);
			TfmGEDKeeper.Instance.AddMRU(aFileName);

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
				fmFileProps.PageControl1.SelectedIndex = (int)aMode;
				Result = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmFileProps, this, false);
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
			this.FTree.Header.CharacterSet = GKUI.TfmGEDKeeper.Instance.Options.DefCharacterSet;
			this.FTree.Header.Language = "Russian";
			this.FTree.Header.GEDCOMVersion = "5.5";
			this.FTree.Header.GEDCOMForm = "LINEAGE-LINKED";
			this.FTree.Header.FileName = Path.GetFileName(aFileName);
			this.FTree.Header.TransmissionDate.Date = DateTime.Now;
			if (subm != "")
			{
				this.FTree.Header.SetTagStringValue("SUBM", subm);
			}
			if (is_advanced && ext_name != "")
			{
				this.FTree.Header.AddTag("_ADVANCED", "", null);
				this.FTree.Header.AddTag("_EXT_NAME", ext_name, null);
			}
			this.FTree.Pack();

			StreamWriter fs = new StreamWriter(aFileName, false, TGEDCOMObject.GetEncodingByCharacterSet(this.FTree.Header.CharacterSet));
			try
			{
				this.FTree.SaveToStream(fs);
				this.FTree.Header.CharacterSet = TGEDCOMCharacterSet.csASCII;
			}
			finally
			{
				TObjectHelper.Free(fs);
			}

			this.FileName = aFileName;
			GKUI.TfmGEDKeeper.Instance.AddMRU(aFileName);
			this.Modified = false;
		}

		public void GenPedigree_dAboville()
		{
			TPedigree p = new TPedigree(this.FEngine, this.GetCurFileTempPath());
			try
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKUI.TfmGEDKeeper.Instance.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = TPedigree.TPedigreeKind.pk_dAboville;
				p.Generate();
			}
			finally
			{
				p.Dispose();
			}
		}

		public void GenPedigree_Konovalov()
		{
			TPedigree p = new TPedigree(this.FEngine, this.GetCurFileTempPath());
			try
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKUI.TfmGEDKeeper.Instance.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = TPedigree.TPedigreeKind.pk_Konovalov;
				p.Generate();
			}
			finally
			{
				p.Dispose();
			}
		}

		public TGEDCOMFamilyRecord GetChildFamily(TGEDCOMIndividualRecord iChild, bool aCanCreate, TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord Result = null;
			if (iChild != null)
			{
				if (iChild.ChildToFamilyLinks.Count != 0)
				{
					Result = iChild.ChildToFamilyLinks[0].Family;
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
			return SysUtils.ExtractFilePath(this.FileName) + "~temp\\";
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

			if (GKUI.TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				GKUI.TfmGEDKeeper.Instance.fmTimeLine.CheckTimeWin(this);
			}
		}

		public void NavNext()
		{
			this.FBackman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FBackman.Next() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
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
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
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
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmPersonScan, this, false);
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
					rec = this.CreatePersonDialog(null, TGenEngine.TTargetMode.tmParent, TGEDCOMSex.svNone);
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
					case TGEDCOMRecordType.rtIndividual:
					{
						list = this.ListPersons;
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						list = this.ListFamilies;
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						list = this.ListNotes;
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						list = this.ListMultimedia;
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						list = this.ListSources;
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						list = this.ListRepositories;
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						list = this.ListGroups;
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						list = this.ListResearches;
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						list = this.ListTasks;
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						list = this.ListCommunications;
						break;
					}
					case TGEDCOMRecordType.rtLocation:
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
				int num;

				if (aSubject is TGEDCOMNoteRecord) {
					num = aInRecord.Notes.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.Notes[i].Value, aSubject)) {
							this.OutLink(aSubject, aToList, aInRecord, null, null);
						}
					}
				} else if (aSubject is TGEDCOMMultimediaRecord) {
					num = aInRecord.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.MultimediaLinks[i].Value, aSubject)) {
							this.OutLink(aSubject, aToList, aInRecord, null, null);
						}
					}
				} else if (aSubject is TGEDCOMSourceRecord) {
					num = aInRecord.SourceCitations.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.SourceCitations[i].Value, aSubject)) {
							this.OutLink(aSubject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
						}
					}
				}

				if (aInRecord is TGEDCOMIndividualRecord) {
					TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)aInRecord;
					num = i_rec.IndividualEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						this.PrepareEvent(aSubject, aToList, i_rec, i_rec.IndividualEvents[i]);
					}
				} else if (aInRecord is TGEDCOMFamilyRecord) {
					TGEDCOMFamilyRecord f_rec = (TGEDCOMFamilyRecord)aInRecord;
					num = f_rec.FamilyEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						this.PrepareEvent(aSubject, aToList, f_rec, f_rec.FamilyEvents[i]);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SearchSubjectLinks(): " + E.Message);
			}
		}

		public TGEDCOMFamilyRecord SelectFamily(TGEDCOMIndividualRecord aTarget)
		{
			TGEDCOMFamilyRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.FTarget = aTarget;
					dlg.FNeedSex = TGEDCOMSex.svNone;
					dlg.TargetMode = TGenEngine.TTargetMode.tmChildToFamily;
					dlg.Mode = TGEDCOMRecordType.rtFamily;
					if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
					{
						Result = (dlg.ResultRecord as TGEDCOMFamilyRecord);
					} else {
						Result = null;
					}
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectFamily(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public TGEDCOMIndividualRecord SelectPerson(TGEDCOMIndividualRecord aTarget, TGenEngine.TTargetMode aTargetMode, TGEDCOMSex aNeedSex)
		{
			TGEDCOMIndividualRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.FTarget = aTarget;
					dlg.FNeedSex = aNeedSex;
					dlg.TargetMode = aTargetMode;
					dlg.Mode = TGEDCOMRecordType.rtIndividual;
					if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
					{
						Result = (dlg.ResultRecord as TGEDCOMIndividualRecord);
					} else {
						Result = null;
					}
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectPerson(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public TGEDCOMRecord SelectRecord(TGEDCOMRecordType aMode, params object[] anArgs)
		{
			TGEDCOMRecord Result;
			try
			{
				anArgs = (object[])anArgs.Clone();
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.Mode = aMode;
					int args_cnt = ((anArgs != null) ? anArgs.Length : 0);
					if (args_cnt > 0) {
						dlg.edFastFilter.Text = (anArgs[0] as string);
					}

					if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
					{
						Result = dlg.ResultRecord;
					} else {
						Result = null;
					}
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectRecord(): " + E.Message);
				Result = null;
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
					case TGEDCOMRecordType.rtIndividual:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListPersons, rec, 0);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListFamilies, rec, 1);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListNotes, rec, 2);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListMultimedia, rec, 3);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListSources, rec, 4);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListRepositories, rec, 5);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListGroups, rec, 6);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListResearches, rec, 7);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListTasks, rec, 8);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListCommunications, rec, 9);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
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
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmFilter, this, false);
			}
			finally
			{
				TObjectHelper.Free(fmFilter);
			}
		}

		public void ShowMap()
		{
			TfmMaps frm_maps = new TfmMaps(this.FTree, this.ListPersons.ContentList);
			frm_maps.MdiParent = GKUI.TfmGEDKeeper.Instance;
			frm_maps.Show();
		}

		public void ShowMedia(TGEDCOMMultimediaRecord aMediaRec)
		{
			TfmMediaView fmMediaView = new TfmMediaView(this);
			try
			{
				fmMediaView.FileRef = aMediaRec.FileReferences[0];
				if (!fmMediaView.Extern)
				{
					GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmMediaView, GKUI.TfmGEDKeeper.Instance, false);
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
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false);
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
		}

		public void ShowScriptDaemon()
		{
			TfmScriptDaemon dmn = new TfmScriptDaemon(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(dmn, this, false);
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
				if (GKUI.TfmGEDKeeper.Instance.Options.ShowTips)
				{
					TStringList birth_days = new TStringList();
					try
					{
						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord rec = this.FTree.GetRecord(i);
							if (rec is TGEDCOMIndividualRecord)
							{
								TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
								string nm = TGenEngine.GetNameStr(i_rec, true, false);
								string days = TGenEngine.GetDaysForBirth(i_rec);

								if (days != "" && int.Parse(days) < 3)
								{
									birth_days.Add(string.Format(GKL.LSList[248], new object[]
									{ nm, days }));
								}
							}
						}
						if (birth_days.Count > 0)
						{
							GKUI.TfmGEDKeeper.Instance.Options.ShowTips = TfmTipsDialog.ShowTipsEx(GKL.LSList[247], GKUI.TfmGEDKeeper.Instance.Options.ShowTips, birth_days);
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
				SysUtils.LogWrite("GKBase.ShowTips(): " + E.Message);
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
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmTreeTools, this, false);
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
				Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK);
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}

		public string DefinePatronymic(string aName, TGEDCOMSex aSex, bool aConfirm)
		{
			string Result = "";

			TNamesTable.TName n = GKUI.TfmGEDKeeper.Instance.NamesTable.FindName(aName);
			if (n == null) {
				if (!aConfirm) {
					return Result;
				} else {
					n = GKUI.TfmGEDKeeper.Instance.NamesTable.AddName(aName);
				}
			}

			switch (aSex) {
				case TGEDCOMSex.svMale: {
					Result = n.M_Patronymic;
					break;
				}
				case TGEDCOMSex.svFemale: {
					Result = n.F_Patronymic;
					break;
				}
			}
			
			if (Result == "") {
				if (!aConfirm) {
					return Result;
				} else {
					this.ModifyName(ref n);
				}
			}

			switch (aSex) {
				case TGEDCOMSex.svMale: {
					Result = n.M_Patronymic;
					break;
				}
				case TGEDCOMSex.svFemale: {
					Result = n.F_Patronymic;
					break;
				}
			}
			
			return Result;
		}

		public bool DeleteFamilyRecord(TGEDCOMFamilyRecord aFamily, bool aConfirm)
		{
			bool Result = false;
			if (aFamily != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[72], new object[]
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
			if (groupRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[77], new object[]
			{
				groupRec.GroupName
			})) != DialogResult.No))
			{
				int num = groupRec.Members.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMIndividualRecord member = groupRec.Members[i].Value as TGEDCOMIndividualRecord;
						member.Groups.Delete(member.IndexOfGroup(groupRec));
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
			if (iRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[71], new object[]
			{
				TGenEngine.GetNameStr(iRec, true, false)
			})) != DialogResult.No))
			{
				for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--)
				{
					TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[i].Family;
					family.DeleteChild(iRec);
				}

				for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
					this.FEngine.RemoveFamilySpouse(family, iRec);
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
			if (mRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[75], new object[]
			{
				mRec.StringValue
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--)
					{
						if (rec.MultimediaLinks[j].Value == mRec)
						{
							rec.MultimediaLinks.Delete(j);
						}
					}
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
			if (nRec != null && (!aConfirm || SysUtils.ShowQuestion(GKL.LSList[73]) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					for (int j = rec.Notes.Count - 1; j >= 0; j--)
					{
						if (rec.Notes[j].Value == nRec)
							rec.Notes.Delete(j);
					}
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
			if (repRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[76], new object[]
			{
				repRec.RepositoryName
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMSourceRecord)
						{
							TGEDCOMSourceRecord srcRec = (TGEDCOMSourceRecord)rec;
							for (int j = srcRec.RepositoryCitations.Count - 1; j >= 0; j--)
							{
								if (srcRec.RepositoryCitations[j].Value == repRec)
								{
									srcRec.RepositoryCitations.DeleteObject(srcRec.RepositoryCitations[j]);
								}
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
			if (resRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[78], new object[]
			{
				resRec.ResearchName
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
			if (srcRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[74], new object[]
			{
				srcRec.FiledByEntry
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					for (int j = rec.SourceCitations.Count - 1; j >= 0; j--)
					{
						if (rec.SourceCitations[j].Value == srcRec)
						{
							rec.SourceCitations.Delete(j);
						}
					}
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
			if (TaskRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[79], new object[]
			{
				TGenEngine.GetTaskGoalStr(this.FTree, TaskRec)
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMResearchRecord)
					{
						TGEDCOMResearchRecord resRec = (TGEDCOMResearchRecord)rec;
						for (int j = resRec.Tasks.Count - 1; j >= 0; j--)
						{
							if (resRec.Tasks[j].Value == TaskRec)
							{
								resRec.Tasks.Delete(j);
							}
						}
					}
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
			if (ComRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[80], new object[]
			{
				ComRec.CommName
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMResearchRecord)
					{
						TGEDCOMResearchRecord resRec = (TGEDCOMResearchRecord)rec;
						for (int j = resRec.Communications.Count - 1; j >= 0; j--)
						{
							if (resRec.Communications[j].Value == ComRec)
							{
								resRec.Communications.Delete(j);
							}
						}
					}
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
			if (LocRec != null && (!aConfirm || SysUtils.ShowQuestion(string.Format(GKL.LSList[81], new object[]
			{
				LocRec.LocationName
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
							int j = iRec.IndividualEvents.Count - 1;
							if (j >= 0)
							{
								do
								{
									TGEDCOMCustomEvent ev = iRec.IndividualEvents[j];
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
								int j = fRec.FamilyEvents.Count - 1;
								if (j >= 0)
								{
									do
									{
										TGEDCOMCustomEvent ev = fRec.FamilyEvents[j];
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
					case TGEDCOMRecordType.rtIndividual:
					{
						Result = this.DeleteIndividualRecord((TGEDCOMIndividualRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						Result = this.DeleteFamilyRecord((TGEDCOMFamilyRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						Result = this.DeleteNoteRecord((TGEDCOMNoteRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						Result = this.DeleteMediaRecord((TGEDCOMMultimediaRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						Result = this.DeleteSourceRecord((TGEDCOMSourceRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						Result = this.DeleteRepositoryRecord((TGEDCOMRepositoryRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						Result = this.DeleteGroupRecord((TGEDCOMGroupRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						Result = this.DeleteResearchRecord((TGEDCOMResearchRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						Result = this.DeleteTaskRecord((TGEDCOMTaskRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						Result = this.DeleteCommunicationRecord((TGEDCOMCommunicationRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
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
					case TGEDCOMRecordType.rtIndividual:
					{
						Result = (this.ListPersons.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						Result = (this.ListFamilies.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						Result = (this.ListNotes.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						Result = (this.ListMultimedia.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						Result = (this.ListSources.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						Result = (this.ListRepositories.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						Result = (this.ListGroups.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						Result = (this.ListResearches.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						Result = (this.ListTasks.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						Result = (this.ListCommunications.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
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
			GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
		}

		public void DoRedo()
		{
			this.FUndoman.CmdRedo();
			this.ListsRefresh(false);
			GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
		}

		public void DoPersonChangeSex(TGEDCOMIndividualRecord aPerson, TGEDCOMSex NewSex)
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
					Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK);
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
				TGEDCOMSex sex = aPerson.Sex;
				if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
				{
					SysUtils.ShowError(GKL.LSList[210]);
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
				Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmFamEdit, this, false) == DialogResult.OK);
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmNoteEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
					aMediaRec.FileReferences.Add(new TGEDCOMFileReferenceWithTitle(this.FTree, aMediaRec, "", ""));
					aMediaRec.InitNew();
				}
				fmMediaEdit.MediaRec = aMediaRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmMediaEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmRepEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmGrpEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmResEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmTaskEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmCorrEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmLocEdit, this, false) == DialogResult.OK)
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
						SysUtils.FreeAndNil(ref obj);
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
				Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, aSender, false) == DialogResult.OK);
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
				if (SysUtils.ShowQuestion(GKL.LSList[243]) != DialogResult.No)
				{
					aRecord.Associations.DeleteObject(aAssociation);
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
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmAstEdit, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.Associations.Add(ast);
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
			try
			{
				bool Result = false;
				if (anAction == TGenEngine.TRecAction.raDelete)
				{
					if (SysUtils.ShowQuestion(GKL.LSList[239]) != DialogResult.No)
					{
						if (aRecord is TGEDCOMIndividualRecord)
						{
							(aRecord as TGEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
						}
						else
						{
							(aRecord as TGEDCOMFamilyRecord).FamilyEvents.DeleteObject(aEvent as TGEDCOMFamilyEvent);
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
						DialogResult dialogResult = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmEventEdit, aSender, true);
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
									(aRecord as TGEDCOMFamilyRecord).FamilyEvents.Add(@event as TGEDCOMFamilyEvent);
								}
							}
							else
							{
								if (aRecord is TGEDCOMIndividualRecord && !object.Equals(fmEventEdit.Event, aEvent))
								{
									(aRecord as TGEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
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
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ModifyRecEvent(): " + E.Message);
				return false;
			}
		}

		public bool ModifyRecMultimedia(Form aSender, TGEDCOMRecord aRecord, TGEDCOMMultimediaLink aLink, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (SysUtils.ShowQuestion(GKL.LSList[241]) != DialogResult.No)
				{
					aRecord.MultimediaLinks.DeleteObject(aLink);
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
					object[] anArgs = new object[0];
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(TGEDCOMRecordType.rtMultimedia, anArgs) as TGEDCOMMultimediaRecord;
					if (mmRec != null)
					{
						TGEDCOMMultimediaLink mmLink = new TGEDCOMMultimediaLink(this.FTree, aRecord, "", "");
						mmLink.Value = mmRec;
						aRecord.MultimediaLinks.Add(mmLink);
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
				if (SysUtils.ShowQuestion(GKL.LSList[240]) != DialogResult.No)
				{
					aRecord.Notes.DeleteObject(aNote);
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
					TGEDCOMRecordType arg_5C_1 = TGEDCOMRecordType.rtNote;
					object[] anArgs = new object[0];
					TGEDCOMNoteRecord noteRec = this.SelectRecord(arg_5C_1, anArgs) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, aRecord, "", "");
						note.Value = noteRec;
						aRecord.Notes.Add(note);
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
				if (SysUtils.ShowQuestion(GKL.LSList[242]) != DialogResult.No)
				{
					aRecord.SourceCitations.DeleteObject(aCit);
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
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.SourceCitations.Add(cit);
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
				if (SysUtils.ShowQuestion(GKL.LSList[244]) != DialogResult.No)
				{
					aRecord.UserReferences.DeleteObject(aUserRef);
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
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.UserReferences.Add(@ref);
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
				if (SysUtils.ShowQuestion(GKL.LSList[241]) != DialogResult.No)
				{
					aTag.MultimediaLinks.DeleteObject(aLink);
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
					TGEDCOMRecordType arg_6B_1 = TGEDCOMRecordType.rtMultimedia;
					object[] anArgs = new object[0];
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(arg_6B_1, anArgs) as TGEDCOMMultimediaRecord;
					if (mmRec != null)
					{
						TGEDCOMMultimediaLink mmLink = new TGEDCOMMultimediaLink(this.FTree, aTag, "", "");
						mmLink.Value = mmRec;
						aTag.MultimediaLinks.Add(mmLink);
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
				if (SysUtils.ShowQuestion(GKL.LSList[240]) != DialogResult.No)
				{
					aTag.Notes.DeleteObject(aNote);
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
					TGEDCOMRecordType arg_5A_1 = TGEDCOMRecordType.rtNote;
					object[] anArgs = new object[0];
					TGEDCOMNoteRecord noteRec = this.SelectRecord(arg_5A_1, anArgs) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, aTag, "", "");
						note.Value = noteRec;
						aTag.Notes.Add(note);
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
				if (SysUtils.ShowQuestion(GKL.LSList[242]) != DialogResult.No)
				{
					aTag.SourceCitations.DeleteObject(aCit);
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
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, GKUI.TfmGEDKeeper.Instance, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aTag.SourceCitations.Add(cit);
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
				if (aRecord.Associations.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[154] + ":");
					}

					int num = aRecord.Associations.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMAssociation ast = aRecord.Associations[idx];
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
				SysUtils.LogWrite("GKBase.RecListAssociationsRefresh(): " + E.Message);
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
				if (aRecord.FamilyEvents.Count != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[83] + ":");
					}

					int num = aRecord.FamilyEvents.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMFamilyEvent @event = aRecord.FamilyEvents[idx];
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
								item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
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
				SysUtils.LogWrite("GKBase.RecListFamilyEventsRefresh(): " + E.Message);
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
				if (aRecord.Groups.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[58] + ":");
					}

					int num = aRecord.Groups.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMPointer ptr = aRecord.Groups[idx];
							TGEDCOMGroupRecord grp = ptr.Value as TGEDCOMGroupRecord;
							if (grp != null)
							{
								if (aList == null && aSummary != null)
								{
									aSummary.Add("    " + TGenEngine.HyperLink(grp.XRef, grp.GroupName, 0));
								}
								if (aList != null)
								{
									TExtListItem item = aList.AddItem(grp.GroupName, grp);
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
				SysUtils.LogWrite("GKBase.RecListGroupsRefresh(): " + E.Message);
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
				if (aRecord.IndividualEvents.Count != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[83] + ":");
					}

					int num = aRecord.IndividualEvents.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMCustomEvent @event = aRecord.IndividualEvents[idx];
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
								if (@event.StringValue != "")
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
								item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
								st = @event.Detail.Place.StringValue;
								if (@event.StringValue != "")
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
				SysUtils.LogWrite("GKBase.RecListIndividualEventsRefresh(): " + E.Message);
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
				if (aRecord.MultimediaLinks.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[55] + " (" + aRecord.MultimediaLinks.Count.ToString() + "):");
					}

					int num = aRecord.MultimediaLinks.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMMultimediaLink mmLink = aRecord.MultimediaLinks[idx];
							TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
							if (mmRec != null && mmRec.FileReferences.Count != 0)
							{
								string st = mmRec.FileReferences[0].Title;
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
				SysUtils.LogWrite("GKBase.RecListMediaRefresh(): " + E.Message);
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
				if (aRecord.Notes.Count != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[54] + " (" + aRecord.Notes.Count.ToString() + "):");
					}

					int num = aRecord.Notes.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMNotes note = aRecord.Notes[idx];
							if (aSummary != null)
							{
								int num2 = note.Notes.Count - 1;
								int i = 0;
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
								if (idx < aRecord.Notes.Count - 1)
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
				SysUtils.LogWrite("GKBase.RecListNotesRefresh(): " + E.Message);
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
				if (aRecord.SourceCitations.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(GKL.LSList[56] + " (" + aRecord.SourceCitations.Count.ToString() + "):");
					}

					int num = aRecord.SourceCitations.Count - 1;
					int idx = 0;
					if (num >= idx)
					{
						num++;
						do
						{
							TGEDCOMSourceCitation cit = aRecord.SourceCitations[idx];
							TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
							if (sourceRec != null)
							{
								string nm = "\"" + sourceRec.FiledByEntry + "\"";
								if (cit.Page != "")
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
				SysUtils.LogWrite("GKBase.RecListSourcesRefresh(): " + E.Message);
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
			if (aSummary != null && cause != "")
			{
				aSummary.Add("    " + cause);
			}
		}

		public void ShowDetailInfo(TGEDCOMEventDetail aDetail, TStrings aSummary)
		{
			if (aSummary != null && aDetail.SourceCitations.Count != 0)
			{
				aSummary.Add(string.Concat(new string[]
				{
					"    ", 
					GKL.LSList[56], 
					" (", 
					aDetail.SourceCitations.Count.ToString(),
					"):"
				}));

				int num = aDetail.SourceCitations.Count - 1;
				for (int idx = 0; idx <= num; idx++)
				{
					TGEDCOMSourceCitation cit = aDetail.SourceCitations[idx];
					TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
					if (sourceRec != null)
					{
						string nm = "\"" + sourceRec.FiledByEntry + "\"";
						if (cit.Page != "")
						{
							nm = nm + ", " + cit.Page;
						}
						aSummary.Add("      " + TGenEngine.HyperLink(sourceRec.XRef, nm, 0));
					}
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
						if (aFamily.Childrens.Count != 0)
						{
							aSummary.Add(GKL.LSList[118] + ":");
						}

						int num = aFamily.Childrens.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							irec = (aFamily.Childrens[i].Value as TGEDCOMIndividualRecord);
							aSummary.Add("    " + TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0) + TGenEngine.GetLifeStr(irec));
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
				SysUtils.LogWrite("GKBase.ShowFamilyInfo(): " + E.Message);
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
						aSummary.Add("~ub+1~" + aGroup.GroupName + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[126] + " (" + aGroup.Members.Count.ToString() + "):");

						int num = aGroup.Members.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMIndividualRecord member = aGroup.Members[i].Value as TGEDCOMIndividualRecord;
							mbrList.AddObject(TGenEngine.GetNameStr(member, true, false), member);
						}
						mbrList.Sort();

						int num2 = mbrList.Count - 1;
						for (int i = 0; i <= num2; i++)
						{
							TGEDCOMIndividualRecord member = mbrList.GetObject(i) as TGEDCOMIndividualRecord;
							aSummary.Add("    " + TGenEngine.HyperLink(member.XRef, mbrList[i], i + 1));
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
				SysUtils.LogWrite("GKBase.ShowGroupInfo(): " + E.Message);
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
						aSummary.Add("~ub+1~" + aMultimediaRec.FileReferences[0].Title + "~bu-1~");
						aSummary.Add("");
						aSummary.Add("[ " + TGenEngine.HyperLink("view_" + aMultimediaRec.XRef, GKL.LSList[148], 0) + " ]");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[234] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							this.SearchSubjectLinks(this.FTree.GetRecord(i), aMultimediaRec, aSummary);
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
				SysUtils.LogWrite("GKBase.ShowMultimediaInfo(): " + E.Message);
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
						aSummary.AddStrings(aNoteRec.Note);
						aSummary.Add("");
						aSummary.Add(GKL.LSList[234] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							this.SearchSubjectLinks(this.FTree.GetRecord(i), aNoteRec, aSummary);
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
				SysUtils.LogWrite("GKBase.ShowNoteInfo(): " + E.Message);
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
							if (iRec.ChildToFamilyLinks.Count != 0)
							{
								aSummary.Add("");
								aSummary.Add(GKL.LSList[152] + ":");
								TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
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
							SysUtils.LogWrite("GKBase.ShowPersonInfo().Parents(): " + E.Message);
						}
						try
						{
							int num = iRec.SpouseToFamilyLinks.Count - 1;
							int idx = 0;
							if (num >= idx)
							{
								num++;
								do
								{
									TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[idx].Family;
									if (family == null)
									{
										SysUtils.LogWrite(string.Concat(new string[]
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
											if (iRec.Sex == TGEDCOMSex.svMale)
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
											if (marr != "")
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
											if (family.Childrens.Count != 0)
											{
												aSummary.Add("");
												aSummary.Add(GKL.LSList[118] + ":");
											}

											int num2 = family.Childrens.Count - 1;
											int i = 0;
											if (num2 >= i)
											{
												num2++;
												do
												{
													rel_person = (family.Childrens[i].Value as TGEDCOMIndividualRecord);
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
							SysUtils.LogWrite("GKBase.ShowPersonInfo().Families(): " + E.Message);
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

							int num3 = this.FTree.RecordsCount - 1;
							for (int i = 0; i <= num3; i++)
							{
								TGEDCOMRecord rec = this.FTree.GetRecord(i);
								if (rec is TGEDCOMIndividualRecord && !object.Equals(rec, iRec))
								{
									TGEDCOMIndividualRecord rel_person = (TGEDCOMIndividualRecord)rec;
									string unk = TGenEngine.GetNameStr(rel_person, true, false);
									if (st == unk)
									{
										namesakes.AddObject(unk + TGenEngine.GetLifeStr(rel_person), rel_person);
									}
								}
							}

							if (namesakes.Count > 0)
							{
								aSummary.Add("");
								aSummary.Add(GKL.LSList[238] + ":");

								int num4 = namesakes.Count - 1;
								for (int i = 0; i <= num4; i++)
								{
									TGEDCOMIndividualRecord rel_person = (TGEDCOMIndividualRecord)namesakes.GetObject(i);
									aSummary.Add("    " + TGenEngine.HyperLink(rel_person.XRef, namesakes[i], 0));
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
				SysUtils.LogWrite("GKBase.ShowPersonInfo(): " + E.Message);
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

						if (aSourceRec.RepositoryCitations.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[57] + ":");

							int num = aSourceRec.RepositoryCitations.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMRepositoryRecord rep = aSourceRec.RepositoryCitations[i].Value as TGEDCOMRepositoryRecord;
								aSummary.Add("    " + TGenEngine.HyperLink(rep.XRef, rep.RepositoryName, 0));
							}
						}

						aSummary.Add("");
						aSummary.Add(GKL.LSList[234] + ":");

						int num2 = this.FTree.RecordsCount - 1;
						for (int j = 0; j <= num2; j++)
						{
							this.SearchSubjectLinks(this.FTree.GetRecord(j), aSourceRec, link_list);
						}

						link_list.Sort();

						int num3 = link_list.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							aSummary.Add(link_list[j]);
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
				SysUtils.LogWrite("GKBase.ShowSourceInfo(): " + E.Message);
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

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord rec = this.FTree.GetRecord(i);

							if (rec is TGEDCOMSourceRecord)
							{
								TGEDCOMSourceRecord srcRec = (TGEDCOMSourceRecord)rec;

								int num2 = srcRec.RepositoryCitations.Count - 1;
								for (int j = 0; j <= num2; j++)
								{
									if (object.Equals(srcRec.RepositoryCitations[j].Value, aRepositoryRec))
									{
										aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, srcRec, false));
									}
								}
							}
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
				SysUtils.LogWrite("GKBase.ShowRepositoryInfo(): " + E.Message);
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
						aSummary.Add(GKL.LSList[125] + ": \"~ub+1~" + aResearchRec.ResearchName.Trim() + "~bu-1~\"");
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
						if (aResearchRec.Tasks.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[60] + ":");

							int num = aResearchRec.Tasks.Count - 1;
							int i = 0;
							if (num >= i)
							{
								num++;
								do
								{
									TGEDCOMTaskRecord taskRec = aResearchRec.Tasks[i].Value as TGEDCOMTaskRecord;
									aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, taskRec, false));
									i++;
								}
								while (i != num);
							}
						}
						if (aResearchRec.Communications.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[61] + ":");

							int num2 = aResearchRec.Communications.Count - 1;
							int i = 0;
							if (num2 >= i)
							{
								num2++;
								do
								{
									TGEDCOMCommunicationRecord corrRec = aResearchRec.Communications[i].Value as TGEDCOMCommunicationRecord;
									aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, corrRec, false));
									i++;
								}
								while (i != num2);
							}
						}
						if (aResearchRec.Groups.Count != 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[58] + ":");

							int num3 = aResearchRec.Groups.Count - 1;
							int i = 0;
							if (num3 >= i)
							{
								num3++;
								do
								{
									TGEDCOMGroupRecord grp = aResearchRec.Groups[i].Value as TGEDCOMGroupRecord;
									aSummary.Add("    " + TGenEngine.HyperLink(grp.XRef, grp.GroupName, 0));
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
				SysUtils.LogWrite("GKBase.ShowResearchInfo(): " + E.Message);
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
				SysUtils.LogWrite("GKBase.ShowTaskInfo(): " + E.Message);
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
						aSummary.Add(GKL.LSList[183] + ": \"~ub+1~" + aCommunicationRec.CommName.Trim() + "~bu-1~\"");
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
				SysUtils.LogWrite("GKBase.ShowCommunicationInfo(): " + E.Message);
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
						aSummary.Add("~ub+1~" + aLocationRec.LocationName.Trim() + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(GKL.LSList[171] + ": " + aLocationRec.Map.Lati);
						aSummary.Add(GKL.LSList[172] + ": " + aLocationRec.Map.Long);
						TGenEngine.GetLocationLinks(this.FTree, aLocationRec, ref link_list);
						link_list.Sort();
						if (link_list.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(GKL.LSList[234] + ":");

							int num = link_list.Count - 1;
							int i = 0;
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
				SysUtils.LogWrite("GKBase.ShowLocationInfo(): " + E.Message);
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

		public void ShowRecordInfo(TGEDCOMRecord aRecord)
		{
			if (aRecord != null)
			{
				try
				{
					switch (aRecord.RecordType)
					{
						case TGEDCOMRecordType.rtIndividual:
						{
							this.ShowPersonInfo(aRecord as TGEDCOMIndividualRecord, this.mPersonSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtFamily:
						{
							this.ShowFamilyInfo(aRecord as TGEDCOMFamilyRecord, this.mFamilySummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtNote:
						{
							this.ShowNoteInfo(aRecord as TGEDCOMNoteRecord, this.mNoteSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtMultimedia:
						{
							this.ShowMultimediaInfo(aRecord as TGEDCOMMultimediaRecord, this.mMediaSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtSource:
						{
							this.ShowSourceInfo(aRecord as TGEDCOMSourceRecord, this.mSourceSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtRepository:
						{
							this.ShowRepositoryInfo(aRecord as TGEDCOMRepositoryRecord, this.mRepositorySummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtGroup:
						{
							this.ShowGroupInfo(aRecord as TGEDCOMGroupRecord, this.mGroupSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtResearch:
						{
							this.ShowResearchInfo(aRecord as TGEDCOMResearchRecord, this.mResearchSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtTask:
						{
							this.ShowTaskInfo(aRecord as TGEDCOMTaskRecord, this.mTaskSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtCommunication:
						{
							this.ShowCommunicationInfo(aRecord as TGEDCOMCommunicationRecord, this.mCommunicationSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtLocation:
						{
							this.ShowLocationInfo(aRecord as TGEDCOMLocationRecord, this.mLocationSummary.Lines);
							break;
						}
					}
				}
				catch (Exception E)
				{
					SysUtils.LogWrite("GKBase.ShowRecordInfo(): " + E.Message);
				}
			}
		}

		public TStrings GetRecordContext(TGEDCOMRecord aRecord)
		{
			//string res = "";
			TStrings ctx = new TStringList();
			if (aRecord != null)
			{
				try
				{
					//TStringList ctx = new TStringList();
					try
					{
						switch (aRecord.RecordType)
						{
							case TGEDCOMRecordType.rtIndividual:
								{
									this.ShowPersonInfo(aRecord as TGEDCOMIndividualRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtFamily:
								{
									this.ShowFamilyInfo(aRecord as TGEDCOMFamilyRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtNote:
								{
									this.ShowNoteInfo(aRecord as TGEDCOMNoteRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtMultimedia:
								{
									this.ShowMultimediaInfo(aRecord as TGEDCOMMultimediaRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtSource:
								{
									this.ShowSourceInfo(aRecord as TGEDCOMSourceRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtRepository:
								{
									this.ShowRepositoryInfo(aRecord as TGEDCOMRepositoryRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtGroup:
								{
									this.ShowGroupInfo(aRecord as TGEDCOMGroupRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtResearch:
								{
									this.ShowResearchInfo(aRecord as TGEDCOMResearchRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtTask:
								{
									this.ShowTaskInfo(aRecord as TGEDCOMTaskRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtCommunication:
								{
									this.ShowCommunicationInfo(aRecord as TGEDCOMCommunicationRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtLocation:
								{
									this.ShowLocationInfo(aRecord as TGEDCOMLocationRecord, ctx);
									break;
								}
						}
						//res = ctx.Text;
					}
					finally
					{
						//ctx.Free();
					}
				}
				catch (Exception E)
				{
					SysUtils.LogWrite("GKBase.GetRecordContext(): " + E.Message);
				}
			}
			return ctx;
		}
		
	}
}
