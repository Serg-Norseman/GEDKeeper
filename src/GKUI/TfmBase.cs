using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKCore.Export;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
	public partial class TfmBase : Form, ILocalization, IProgressController
	{
		private NavManager FNavman;
		private TList[] FChangedRecords = new TList[14];
		private TList FLockedRecords;
		private bool FModified;
		private TShieldState FShieldState;
		private TGEDCOMTree FTree;
		private MediaManager FMedia;
		private ValuesCollection fValuesCollection = new ValuesCollection();

		public TabControl PageRecords;
		public GKRecordsView ListPersons;
		public GKHyperView mPersonSummary;
		public GKRecordsView ListFamilies;
		public GKHyperView mFamilySummary;
		public GKRecordsView ListNotes;
		public GKHyperView mNoteSummary;
		public GKRecordsView ListMultimedia;
		public GKHyperView mMediaSummary;
		public GKRecordsView ListSources;
		public GKHyperView mSourceSummary;
		public GKRecordsView ListRepositories;
		public GKHyperView mRepositorySummary;
		public GKRecordsView ListGroups;
		public GKHyperView mGroupSummary;
		public GKRecordsView ListResearches;
		public GKHyperView mResearchSummary;
		public GKRecordsView ListTasks;
		public GKHyperView mTaskSummary;
		public GKRecordsView ListCommunications;
		public GKHyperView mCommunicationSummary;
		public GKRecordsView ListLocations;
		public GKHyperView mLocationSummary;


		public NavManager Navman
		{
			get { return this.FNavman; }
		}

		public MediaManager Media
		{
			get { return this.FMedia; }
		}

		public ValuesCollection ValuesCollection
		{
			get {
				return this.fValuesCollection;
			}
		}

		public void ChangeFileName()
		{
			this.SetMainTitle();
			GKUI.TfmGEDKeeper.Instance.Options.LastDir = Path.GetDirectoryName(this.FTree.FileName);
		}
		
		public bool Modified
		{
			get {
				return this.FModified;
			}
			set {
				this.FModified = value;
				this.SetMainTitle();
			}
		}

		public TShieldState ShieldState
		{
			get {
				return this.FShieldState;
			}
			set {
				bool up = (this.FShieldState != TShieldState.ssNone && value == TShieldState.ssNone) || (this.FShieldState == TShieldState.ssNone && value != TShieldState.ssNone);
				this.FShieldState = value;
				if (up)
				{
					this.ListsRefresh(false);
				}
			}
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
		}

		void ILocalization.SetLang()
		{
			this.PageRecords.TabPages[ 0].Text = LangMan.LS(LSID.LSID_RPIndividuals);
			this.PageRecords.TabPages[ 1].Text = LangMan.LS(LSID.LSID_RPFamilies);
			this.PageRecords.TabPages[ 2].Text = LangMan.LS(LSID.LSID_RPNotes);
			this.PageRecords.TabPages[ 3].Text = LangMan.LS(LSID.LSID_RPMultimedia);
			this.PageRecords.TabPages[ 4].Text = LangMan.LS(LSID.LSID_RPSources);
			this.PageRecords.TabPages[ 5].Text = LangMan.LS(LSID.LSID_RPRepositories);
			this.PageRecords.TabPages[ 6].Text = LangMan.LS(LSID.LSID_RPGroups);
			this.PageRecords.TabPages[ 7].Text = LangMan.LS(LSID.LSID_RPResearches);
			this.PageRecords.TabPages[ 8].Text = LangMan.LS(LSID.LSID_RPTasks);
			this.PageRecords.TabPages[ 9].Text = LangMan.LS(LSID.LSID_RPCommunications);
			this.PageRecords.TabPages[10].Text = LangMan.LS(LSID.LSID_RPLocations);
		}

		public string GetStatusString()
		{
			string res = "";

			TGEDCOMRecordType rt = (TGEDCOMRecordType)(this.PageRecords.SelectedIndex + 1);
			GKRecordsView rView = this.GetRecordsViewByType(rt);

			if (rView != null)
			{
				res = LangMan.LSList[50] + ": " + rView.TotalCount.ToString();
				res = res + ", " + LangMan.LSList[51] + ": " + rView.FilteredCount.ToString();
			}

			return res;
		}

		private TGEDCOMFamilyRecord GetFamilyBySpouse(TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord result = null;

			int num = this.FTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.FTree[i];

				if (rec is TGEDCOMFamilyRecord)
				{
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					TGEDCOMIndividualRecord husb = fam.Husband.Value as TGEDCOMIndividualRecord;
					TGEDCOMIndividualRecord wife = fam.Wife.Value as TGEDCOMIndividualRecord;
					if (husb == aNewParent || wife == aNewParent)
					{
						string msg = string.Format(LangMan.LSList[70], GKUtils.aux_GetFamilyStr(fam));
						if (GKUtils.ShowQuestion(msg) == DialogResult.Yes)
						{
							result = fam;
							break;
						}
					}
				}
			}

			return result;
		}

		private void List_SelectedIndexChanged(object sender, EventArgs e)
		{
			TGEDCOMRecord rec = (sender as GKRecordsView).GetSelectedRecord();
			if (rec != null)
			{
				this.NavAdd(rec);
			}
			this.ShowRecordInfo(rec);
		}

		private void NavAdd(TGEDCOMRecord aRec)
		{
			if (aRec != null && !this.FNavman.Busy)
			{
				this.FNavman.Current = aRec;
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
		}

		private void SetMainTitle()
		{
			this.Text = Path.GetFileName(this.Tree.FileName);
			if (this.FModified)
			{
				this.Text = "* " + this.Text;
			}
		}

		private void mPersonSummaryLink(object sender, string LinkName)
		{
			if (LinkName.StartsWith("view_"))
			{
				string xref = LinkName.Remove(0, 5);
				TGEDCOMRecord rec = this.FTree.XRefIndex_Find(xref);
				if (rec != null)
				{
					this.ShowMedia(rec as TGEDCOMMultimediaRecord, false);
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
				GKUI.TfmGEDKeeper.Instance.fmTimeLine.CheckTimeLine(this);
			}
		}

		private void FormDeactivate(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.UpdateControls(true);
			if (TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				TfmGEDKeeper.Instance.fmTimeLine.CheckTimeLine(null);
			}
		}

		private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.UpdateControls(false);
		}

		private void TfmBase_Closing(object sender, CancelEventArgs e)
		{
			e.Cancel = !this.CheckModified();

			if (!e.Cancel)
			{
				GKUI.TfmGEDKeeper.Instance.CheckMRUWin(this.Tree.FileName, this);
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FNavman.Dispose();
				this.FLockedRecords.Dispose();
				this.FTree.Dispose();
				this.FTree = null;
				for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
				{
					this.FChangedRecords[(int)rt].Dispose();
				}

                if (components != null) components.Dispose();
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

			this.FTree = new TGEDCOMTree();
			this.FMedia = new MediaManager(this.FTree);

			this.FLockedRecords = new TList();
			this.FNavman = new NavManager();

			this.CreatePage(LangMan.LSList[52], TGEDCOMRecordType.rtIndividual, ref this.ListPersons, ref this.mPersonSummary);
			this.CreatePage(LangMan.LSList[53], TGEDCOMRecordType.rtFamily, ref this.ListFamilies, ref this.mFamilySummary);
			this.CreatePage(LangMan.LSList[54], TGEDCOMRecordType.rtNote, ref this.ListNotes, ref this.mNoteSummary);
			this.CreatePage(LangMan.LSList[55], TGEDCOMRecordType.rtMultimedia, ref this.ListMultimedia, ref this.mMediaSummary);
			this.CreatePage(LangMan.LSList[56], TGEDCOMRecordType.rtSource, ref this.ListSources, ref this.mSourceSummary);
			this.CreatePage(LangMan.LSList[57], TGEDCOMRecordType.rtRepository, ref this.ListRepositories, ref this.mRepositorySummary);
			this.CreatePage(LangMan.LSList[58], TGEDCOMRecordType.rtGroup, ref this.ListGroups, ref this.mGroupSummary);
			this.CreatePage(LangMan.LSList[59], TGEDCOMRecordType.rtResearch, ref this.ListResearches, ref this.mResearchSummary);
			this.CreatePage(LangMan.LSList[60], TGEDCOMRecordType.rtTask, ref this.ListTasks, ref this.mTaskSummary);
			this.CreatePage(LangMan.LSList[61], TGEDCOMRecordType.rtCommunication, ref this.ListCommunications, ref this.mCommunicationSummary);
			this.CreatePage(LangMan.LSList[62], TGEDCOMRecordType.rtLocation, ref this.ListLocations, ref this.mLocationSummary);
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

				this.FTree.Header.TransmissionDateTime = DateTime.Now;

				TfmGEDKeeper.Instance.SearchMan.UpdateRecord(this, record);
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
			bool result = true;

			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(LangMan.LSList[69], "GEDKeeper2", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);

				switch (dialogResult) {
					case DialogResult.Yes:
						GKUI.TfmGEDKeeper.Instance.miFileSaveClick(null, null);
						break;
					case DialogResult.No:
						break;
					case DialogResult.Cancel:
						result = false;
						break;
				}
			}

			return result;
		}

		public void Clear()
		{
			this.FTree.Clear();
			this.FNavman.Clear();
		}

		public void CreateListView(Control aOwner, out GKListView aList)
		{
			aList = new GKListView();
			aList.HideSelection = false;
			aList.LabelEdit = false;
			aList.FullRowSelect = true;
			aList.View = View.Details;
			aList.Dock = DockStyle.Fill;
			aOwner.Controls.Add(aList);
		}

		private void CreatePage(string aPageText, TGEDCOMRecordType aRecType, ref GKRecordsView aList, ref GKHyperView aSummary)
		{
			this.PageRecords.SuspendLayout();

			TabPage sheet = new TabPage();
			sheet.Text = aPageText;
			this.PageRecords.Controls.Add(sheet);
			this.PageRecords.ResumeLayout(false);

			aSummary = new GKHyperView();
			aSummary.BorderWidth = 4;
			aSummary.Dock = DockStyle.Right;
			aSummary.Size = new Size(300, 290);
			aSummary.OnLink += new GKHyperView.TLinkEvent(this.mPersonSummaryLink);

			Splitter spl = new Splitter();
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;

			sheet.Controls.Add(aSummary);
			sheet.Controls.Add(spl);

			this.CreateRecordsView(sheet, aRecType, ref aList);
			aList.IsMainList = true;
			aList.DoubleClick += new EventHandler(this.RecordEdit);
			aList.SelectedIndexChanged += new EventHandler(this.List_SelectedIndexChanged);
			aList.UpdateTitles();

			sheet.Controls.SetChildIndex(spl, 1);
			sheet.Controls.SetChildIndex(aSummary, 2);
		}

		void TfmBaseKeyDown(object sender, KeyEventArgs e)
		{
			if (e.Control)
			{
				switch (e.KeyCode) {
					/*case Keys.I:
						this.ItemAdd();
						break;
					case Keys.D:
						this.ItemDelete();
						break;*/

					case Keys.Return:
						this.RecordEdit(null, null);
						break;
				}
			}
		}

		public TGEDCOMIndividualRecord CreatePersonDialog(TGEDCOMIndividualRecord aTarget, TTargetMode aTargetMode, TGEDCOMSex aNeedSex)
		{
			TGEDCOMIndividualRecord result = null;

			using (TfmPersonNew dlg = new TfmPersonNew())
			{
				dlg.EditSex.SelectedIndex = (int)aNeedSex;
				dlg.TargetMode = aTargetMode;
				dlg.Target = aTarget;

				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					result = GKUtils.CreatePersonEx(this.FTree, dlg.edName.Text, dlg.edPatronymic.Text, dlg.edFamily.Text, (TGEDCOMSex)dlg.EditSex.SelectedIndex, true);
					this.ChangeRecord(result);
					
					TfmGEDKeeper.Instance.NamesTable.ImportNames(result);
					
					TIndividualListFilter iFilter = (TIndividualListFilter)this.ListPersons.ListMan.Filter;

					if (iFilter.SourceMode == CustomFilter.TGroupMode.gmSelected) {
						TGEDCOMSourceRecord src = this.FTree.XRefIndex_Find(iFilter.SourceRef) as TGEDCOMSourceRecord;
						if (GKUtils.ShowQuestion("Установлен фильтр по источнику. Внести источник в новую персональную запись?") == DialogResult.Yes) {
							result.aux_AddSource(src, "", 0);
						}
 					}

					if (iFilter.GroupMode == CustomFilter.TGroupMode.gmSelected) {
						TGEDCOMGroupRecord grp = this.FTree.XRefIndex_Find(iFilter.GroupRef) as TGEDCOMGroupRecord;
						if (GKUtils.ShowQuestion("Установлен фильтр по группе. Внести группу в новую персональную запись?") == DialogResult.Yes) {
							grp.aux_AddMember(result);
						}
 					}
				}
			}

			return result;
		}

		public void CreateRecordsView(Control aParent, TGEDCOMRecordType aRecordType, ref GKRecordsView aList)
		{
			aList = new GKRecordsView();
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

		public string GetRestoreFilename(string fileName)
		{
			string rfn = Path.ChangeExtension(fileName, ".restore");
			return rfn;
		}

		private void LoadProgress(object sender, int progress)
		{
			TfmProgress.ProgressStep(progress);
		}

		public void FileNew()
		{
			this.ChangesClear();
			this.Clear();
			this.ListsRefresh(false);
			this.ShowPersonInfo(null, this.mPersonSummary.Lines);
			this.FTree.SetFileName(LangMan.LSList[165]);
			this.Modified = false;
		}

		public void FileLoad(string fileName)
		{
			this.ChangesClear();
			this.Clear();

			//try
			//{
			TfmProgress.ProgressInit(100, LangMan.LS(LSID.LSID_Loading));
			try
			{
				this.FTree.OnProgress += LoadProgress;
				this.FTree.LoadFromFile(fileName);
				this.FTree.OnProgress -= LoadProgress;
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
			//}
			//catch (Exception E)
			//{
			//	SysUtils.LogWrite("GKBase.FileLoad().TreeLoad(): " + E.Message);
			//	SysUtils.ShowError(LangMan.LSList[245]);
			//}

			TreeTools.CheckGEDCOMFormat(this.FTree, this.fValuesCollection, this);

			this.ChangeFileName(); //FileName = fileName;
			this.Modified = false;

			TfmGEDKeeper.Instance.AddMRU(fileName);

			this.ListsRefresh(false);
			this.ShowTips();
		}

		public void FileSave(string fileName)
		{
			try
			{
				if (GKUI.TfmGEDKeeper.Instance.Options.RevisionsBackup)
				{
					int rev = this.Tree.Header.FileRevision;
					if (File.Exists(fileName)) 
					{
						string bak_path = Path.GetDirectoryName(fileName) + "\\__history\\";
						string bak_file = Path.GetFileName(fileName) + "." + SysUtils.NumUpdate(rev, 3);

						if (!Directory.Exists(bak_path)) Directory.CreateDirectory(bak_path);
						File.Move(fileName, bak_path + bak_file);
					}

					// FIXME: обязательная чистка файлов истории, установить границу в 2-5 кб
				}

				// проверка наличия архива и хранилища, перемещение их, если файл изменил местоположение
				CheckMediaContainers(this.Tree.FileName, fileName);

				this.FTree.SaveToFile(fileName, GKUI.TfmGEDKeeper.Instance.Options.DefCharacterSet);

				this.ChangeFileName(); //FileName = fileName;

				GKUI.TfmGEDKeeper.Instance.AddMRU(fileName);
				this.Modified = false;
			}
			catch (UnauthorizedAccessException)
			{
				GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": доступ закрыт" }));
			}
			catch (Exception E)
			{
				GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
				SysUtils.LogWrite("GKBase.FileSave(): " + E.Message);
			}
		}

		public DialogResult FileProperties()
		{
			DialogResult result;

			using (TfmFileProperties dlgFileProps = new TfmFileProperties(this))
			{
				result = GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlgFileProps, this, false);
			}

			return result;
		}

		public void CheckMediaContainers(string oldFileName, string newFileName)
		{
			// ничего не делать, если имя файла не изменилось
			if (string.Equals(oldFileName, newFileName)) return;

			bool has_arc = File.Exists(this.FMedia.GetArcFileName());
			bool has_stg = Directory.Exists(this.FMedia.GetStgFolder(false));

			string new_path = Path.GetDirectoryName(newFileName);
			string new_name = Path.GetFileName(newFileName);

			// переместить архив и хранилище
			if (has_arc) {
				string new_arc = new_path + "\\" + this.FMedia.GetContainerName(new_name, true);
				File.Move(this.FMedia.GetArcFileName(), new_arc);
			}

			if (has_stg) {
				string new_stg = new_path + "\\" + this.FMedia.GetContainerName(new_name, false);
				Directory.Move(this.FMedia.GetStgFolder(false), new_stg);
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
							fam = this.FTree.aux_CreateFamily();
						}
						fam.aux_AddChild(iChild);
						Result = fam;
					}
				}
			}
			return Result;
		}

		public string GetCurFileTempPath()
		{
			return Path.GetDirectoryName(this.Tree.FileName) + "\\~temp\\";
		}

		public TGEDCOMIndividualRecord GetSelectedPerson()
		{
			return this.ListPersons.GetSelectedRecord() as TGEDCOMIndividualRecord;
		}

		private void IntUpdate(GKRecordsView aRecView, int ASCol, bool aTitles)
		{
			TGEDCOMRecord rec = aRecView.GetSelectedRecord();
			aRecView.UpdateContents(this.FShieldState, aTitles, ASCol);
			if (rec != null) aRecView.SelectItemByRec(rec);
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
				GKUI.TfmGEDKeeper.Instance.fmTimeLine.UpdateControls();
			}
		}

		public void RecordNotify(TGEDCOMRecord record, TRecNotify notify)
		{
			if (record != null)
			{
				GKRecordsView list = GetRecordsViewByType(record.RecordType);

				if (list != null && notify == TRecNotify.rnDelete)
				{
					list.DeleteRecord(record);
				}
			}
		}

		public TGEDCOMFamilyRecord SelectFamily(TGEDCOMIndividualRecord target)
		{
			TGEDCOMFamilyRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.FTarget = target;
					dlg.FNeedSex = TGEDCOMSex.svNone;
					dlg.TargetMode = TTargetMode.tmChildToFamily;
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
					dlg.Dispose();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectFamily(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public TGEDCOMIndividualRecord SelectPerson(TGEDCOMIndividualRecord target, TTargetMode targetMode, TGEDCOMSex needSex)
		{
			TGEDCOMIndividualRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.FTarget = target;
					dlg.FNeedSex = needSex;
					dlg.TargetMode = targetMode;
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
					dlg.Dispose();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectPerson(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public TGEDCOMRecord SelectRecord(TGEDCOMRecordType mode, params object[] args)
		{
			TGEDCOMRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.Mode = mode;
					int args_cnt = ((args != null) ? args.Length : 0);
					if (args_cnt > 0) {
						dlg.edFastFilter.Text = (args[0] as string);
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
					dlg.Dispose();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectRecord(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public void SetFilter()
		{
			TGEDCOMRecordType rt = (TGEDCOMRecordType)(this.PageRecords.SelectedIndex + 1);
			GKRecordsView rView = this.GetRecordsViewByType(rt);

			switch (rt) {
				case TGEDCOMRecordType.rtIndividual:
					using (TfmPersonsFilter fmFilter = new TfmPersonsFilter(this, rView.ListMan)) {
						DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmFilter, this, false);
						if (res == DialogResult.OK) this.ApplyFilter();
					}
					break;
				case TGEDCOMRecordType.rtFamily:
				case TGEDCOMRecordType.rtNote:
				case TGEDCOMRecordType.rtMultimedia:
				case TGEDCOMRecordType.rtSource:
				case TGEDCOMRecordType.rtRepository:
				case TGEDCOMRecordType.rtGroup:
				case TGEDCOMRecordType.rtResearch:
				case TGEDCOMRecordType.rtTask:
				case TGEDCOMRecordType.rtCommunication:
				case TGEDCOMRecordType.rtLocation:
					using (TfmComFilter fmComFilter = new TfmComFilter(this, rView.ListMan)) {
						DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmComFilter, this, false);
						if (res == DialogResult.OK) this.ApplyFilter();
					}
					break;
			}
		}

		public string DefinePatronymic(string name, TGEDCOMSex sex, bool confirm)
		{
			string Result = "";

			NamesTable.NameEntry n = GKUI.TfmGEDKeeper.Instance.NamesTable.FindName(name);
			if (n == null) {
				if (!confirm) {
					return Result;
				} else {
					n = GKUI.TfmGEDKeeper.Instance.NamesTable.AddName(name);
				}
			}

			switch (sex) {
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
				if (!confirm) {
					return Result;
				} else {
					this.ModifyName(ref n);
				}
			}

			switch (sex) {
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

		#region Record Management

		public void RecordAdd()
		{
			TGEDCOMRecord rec = null;
			bool res = false;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					rec = this.CreatePersonDialog(null, TTargetMode.tmParent, TGEDCOMSex.svNone);
					res = (rec != null);
					break;
				}
				case 1:
				{
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					res = this.ModifyFamily(ref fam, TFamilyTarget.ftNone, null);
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
					res = this.ModifyFamily(ref fam, TFamilyTarget.ftNone, null);
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

		public GKRecordsView GetRecordsViewByType(TGEDCOMRecordType recType)
		{
			GKRecordsView list = null;
			switch (recType) {
				case TGEDCOMRecordType.rtIndividual:
					list = this.ListPersons;
					break;
				case TGEDCOMRecordType.rtFamily:
					list = this.ListFamilies;
					break;
				case TGEDCOMRecordType.rtNote:
					list = this.ListNotes;
					break;
				case TGEDCOMRecordType.rtMultimedia:
					list = this.ListMultimedia;
					break;
				case TGEDCOMRecordType.rtSource:
					list = this.ListSources;
					break;
				case TGEDCOMRecordType.rtRepository:
					list = this.ListRepositories;
					break;
				case TGEDCOMRecordType.rtGroup:
					list = this.ListGroups;
					break;
				case TGEDCOMRecordType.rtResearch:
					list = this.ListResearches;
					break;
				case TGEDCOMRecordType.rtTask:
					list = this.ListTasks;
					break;
				case TGEDCOMRecordType.rtCommunication:
					list = this.ListCommunications;
					break;
				case TGEDCOMRecordType.rtLocation:
					list = this.ListLocations;
					break;
			}
			return list;
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
						this.SelectItemByRec(this.ListPersons, rec, 0);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						this.SelectItemByRec(this.ListFamilies, rec, 1);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						this.SelectItemByRec(this.ListNotes, rec, 2);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						this.SelectItemByRec(this.ListMultimedia, rec, 3);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						this.SelectItemByRec(this.ListSources, rec, 4);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						this.SelectItemByRec(this.ListRepositories, rec, 5);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						this.SelectItemByRec(this.ListGroups, rec, 6);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						this.SelectItemByRec(this.ListResearches, rec, 7);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						this.SelectItemByRec(this.ListTasks, rec, 8);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						this.SelectItemByRec(this.ListCommunications, rec, 9);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
					{
						this.SelectItemByRec(this.ListLocations, rec, 10);
						break;
					}
				}
			}
		}

		private void SelectItemByRec(GKRecordsView aList, TGEDCOMRecord aRec, int aTab)
		{
			this.PageRecords.SelectedIndex = aTab;
			this.PageRecords_SelectedIndexChanged(null, null);
			this.ActiveControl = aList;
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

		public StringList GetRecordContext(TGEDCOMRecord aRecord)
		{
			StringList ctx = new StringList();

            if (aRecord != null)
			{
				try
				{
						switch (aRecord.RecordType)
						{
							case TGEDCOMRecordType.rtIndividual:
								this.ShowPersonInfo(aRecord as TGEDCOMIndividualRecord, ctx);
								break;

							case TGEDCOMRecordType.rtFamily:
								this.ShowFamilyInfo(aRecord as TGEDCOMFamilyRecord, ctx);
								break;

							case TGEDCOMRecordType.rtNote:
								this.ShowNoteInfo(aRecord as TGEDCOMNoteRecord, ctx);
								break;

							case TGEDCOMRecordType.rtMultimedia:
								this.ShowMultimediaInfo(aRecord as TGEDCOMMultimediaRecord, ctx);
								break;

							case TGEDCOMRecordType.rtSource:
								this.ShowSourceInfo(aRecord as TGEDCOMSourceRecord, ctx);
								break;

							case TGEDCOMRecordType.rtRepository:
								this.ShowRepositoryInfo(aRecord as TGEDCOMRepositoryRecord, ctx);
								break;

							case TGEDCOMRecordType.rtGroup:
								this.ShowGroupInfo(aRecord as TGEDCOMGroupRecord, ctx);
								break;

							case TGEDCOMRecordType.rtResearch:
								this.ShowResearchInfo(aRecord as TGEDCOMResearchRecord, ctx);
								break;

							case TGEDCOMRecordType.rtTask:
								this.ShowTaskInfo(aRecord as TGEDCOMTaskRecord, ctx);
								break;

							case TGEDCOMRecordType.rtCommunication:
								this.ShowCommunicationInfo(aRecord as TGEDCOMCommunicationRecord, ctx);
								break;

							case TGEDCOMRecordType.rtLocation:
								this.ShowLocationInfo(aRecord as TGEDCOMLocationRecord, ctx);
								break;
						}
				}
				catch (Exception E)
				{
					SysUtils.LogWrite("GKBase.GetRecordContext(): " + E.Message);
				}
			}

            return ctx;
		}

		public bool RecordIsFiltered(TGEDCOMRecord record)
		{
			bool Result = false;
			if (record != null)
			{
				switch (record.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						Result = (this.ListPersons.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						Result = (this.ListFamilies.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						Result = (this.ListNotes.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						Result = (this.ListMultimedia.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						Result = (this.ListSources.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						Result = (this.ListRepositories.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						Result = (this.ListGroups.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						Result = (this.ListResearches.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						Result = (this.ListTasks.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						Result = (this.ListCommunications.ContentList.IndexOf(record) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
					{
						Result = (this.ListLocations.ContentList.IndexOf(record) >= 0);
						break;
					}
				}
			}
			return Result;
		}

		#endregion

		#region UI Actions

		public void GenPedigree_dAboville()
		{
			using (PedigreeExporter p = new PedigreeExporter(this.FTree))
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKUI.TfmGEDKeeper.Instance.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = PedigreeExporter.TPedigreeKind.pk_dAboville;
				p.Generate(true);
			}
		}

		public void GenPedigree_Konovalov()
		{
			using (PedigreeExporter p = new PedigreeExporter(this.FTree))
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKUI.TfmGEDKeeper.Instance.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = PedigreeExporter.TPedigreeKind.pk_Konovalov;
				p.Generate(true);
			}
		}

		public void ExportToExcel(bool appmode)
		{
			using (ExcelExporter ex_exp = new ExcelExporter(this.FTree))
			{
				ex_exp.Options = GKUI.TfmGEDKeeper.Instance.Options;
				ex_exp.SelectedRecords = this.ListPersons.ContentList;
				ex_exp.AppMode = appmode;
				ex_exp.Generate(true);
			}
		}

		public void ExportToFamilyBook()
		{
			using (FamilyBookExporter fb = new FamilyBookExporter(this.FTree))
			{
				fb.Generate(true);
			}
		}

		public void NavNext()
		{
			this.FNavman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FNavman.Next() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
			finally
			{
				this.FNavman.EndNav();
			}
		}

		public void NavPrev()
		{
			this.FNavman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FNavman.Back() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
			finally
			{
				this.FNavman.EndNav();
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
				fmPersonScan.Dispose();
			}
		}

		public void ShowMap()
		{
			TfmMaps frm_maps = new TfmMaps(this.FTree, this.ListPersons.ContentList);
			frm_maps.MdiParent = GKUI.TfmGEDKeeper.Instance;
			frm_maps.Show();
		}

		public void ShowMedia(TGEDCOMMultimediaRecord mediaRec, bool modal)
		{
			TfmMediaView fmMediaView = new TfmMediaView(this);
			try
			{
				fmMediaView.FileRef = mediaRec.FileReferences[0];
				if (!fmMediaView.Extern)
				{
					if (modal) {
						fmMediaView.ShowDialog();
					} else {
						fmMediaView.ShowInTaskbar = true;
						fmMediaView.Show();
					}
					//GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmMediaView, GKUI.TfmGEDKeeper.Instance, false);
				}
			}
			finally
			{
				if (modal) fmMediaView.Dispose();
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
				dlg.Dispose();
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
				dmn.Dispose();
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
					StringList birth_days = new StringList();
					try
					{
						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord rec = this.FTree[i];
							if (rec is TGEDCOMIndividualRecord)
							{
								TGEDCOMIndividualRecord i_rec = rec as TGEDCOMIndividualRecord;
								string nm = i_rec.aux_GetNameStr(true, false);
								string days = GKUtils.GetDaysForBirth(i_rec);

								if (days != "" && int.Parse(days) < 3)
								{
									birth_days.Add(string.Format(LangMan.LSList[248], nm, days));
								}
							}
						}
						if (birth_days.Count > 0)
						{
							GKUI.TfmGEDKeeper.Instance.Options.ShowTips = TfmTipsDialog.ShowTipsEx(LangMan.LSList[247], GKUI.TfmGEDKeeper.Instance.Options.ShowTips, birth_days);
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
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TTreeChartBox.TChartKind.ckAncestors))
			{
				TfmChart fmChart = new TfmChart(this, this.GetSelectedPerson());
				fmChart.ChartKind = TTreeChartBox.TChartKind.ckAncestors;
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeDescendants()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TTreeChartBox.TChartKind.ckDescendants))
			{
				TfmChart fmChart = new TfmChart(this, this.GetSelectedPerson());
				fmChart.ChartKind = TTreeChartBox.TChartKind.ckDescendants;
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeBoth()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TTreeChartBox.TChartKind.ckBoth))
			{
				TfmChart fmChart = new TfmChart(this, this.GetSelectedPerson());
				fmChart.ChartKind = TTreeChartBox.TChartKind.ckBoth;
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeTools()
		{
			TfmTreeTools fmTreeTools = new TfmTreeTools(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmTreeTools, this, false);
			}
			finally
			{
				fmTreeTools.Dispose();
			}
		}

		#endregion

		#region Delete records

		public bool DeleteFamilyRecord(TGEDCOMFamilyRecord family, bool confirm)
		{
			bool Result = false;
			if (family != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[72], GKUtils.aux_GetFamilyStr(family))) != DialogResult.No))
			{
				GKUtils.CleanFamily(family);
				this.RecordNotify(family, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(family));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteGroupRecord(TGEDCOMGroupRecord groupRec, bool confirm)
		{
			bool Result = false;
			if (groupRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[77], groupRec.GroupName)) != DialogResult.No))
			{
				for (int i = groupRec.Members.Count - 1; i >= 0; i--)
				{
					TGEDCOMIndividualRecord member = groupRec.Members[i].Value as TGEDCOMIndividualRecord;
					groupRec.aux_RemoveMember(member);
				}

				this.RecordNotify(groupRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(groupRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteIndividualRecord(TGEDCOMIndividualRecord iRec, bool confirm)
		{
			bool Result = false;
			if (iRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[71], iRec.aux_GetNameStr(true, false))) != DialogResult.No))
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

				this.RecordNotify(iRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(iRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteMediaRecord(TGEDCOMMultimediaRecord mRec, bool confirm)
		{
			bool Result = false;
			if (mRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[75], mRec.StringValue)) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--)
					{
						if (rec.MultimediaLinks[j].Value == mRec)
						{
							rec.MultimediaLinks.Delete(j);
						}
					}
				}
				this.RecordNotify(mRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(mRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteNoteRecord(TGEDCOMNoteRecord nRec, bool confirm)
		{
			bool Result = false;
			if (nRec != null && (!confirm || GKUtils.ShowQuestion(LangMan.LSList[73]) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					for (int j = rec.Notes.Count - 1; j >= 0; j--)
					{
						if (rec.Notes[j].Value == nRec)
							rec.Notes.Delete(j);
					}
				}

				this.RecordNotify(nRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(nRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteRepositoryRecord(TGEDCOMRepositoryRecord repRec, bool confirm)
		{
			bool Result = false;
			if (repRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[76], repRec.RepositoryName)) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
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

				this.RecordNotify(repRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(repRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteResearchRecord(TGEDCOMResearchRecord resRec, bool confirm)
		{
			bool Result = false;
			if (resRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[78], resRec.ResearchName)) != DialogResult.No))
			{
				this.RecordNotify(resRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(resRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteSourceRecord(TGEDCOMSourceRecord srcRec, bool confirm)
		{
			bool Result = false;
			if (srcRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[74], srcRec.FiledByEntry)) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					for (int j = rec.SourceCitations.Count - 1; j >= 0; j--)
					{
						if (rec.SourceCitations[j].Value == srcRec)
						{
							rec.SourceCitations.Delete(j);
						}
					}
				}
				this.RecordNotify(srcRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(srcRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteTaskRecord(TGEDCOMTaskRecord taskRec, bool confirm)
		{
			bool Result = false;
			if (taskRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[79], GKUtils.GetTaskGoalStr(taskRec))) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
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
				this.RecordNotify(taskRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(taskRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteCommunicationRecord(TGEDCOMCommunicationRecord commRec, bool confirm)
		{
			bool Result = false;
			if (commRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[80], commRec.CommName)) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
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
				this.RecordNotify(commRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(commRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteLocationRecord(TGEDCOMLocationRecord locRec, bool confirm)
		{
			bool Result = false;
			if (locRec != null && (!confirm || GKUtils.ShowQuestion(string.Format(LangMan.LSList[81], locRec.LocationName)) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
						for (int j = iRec.IndividualEvents.Count - 1; j >= 0; j--)
						{
							TGEDCOMCustomEvent ev = iRec.IndividualEvents[j];
							if (object.Equals(ev.Detail.Place.Location.Value, locRec))
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
								if (object.Equals(ev.Detail.Place.Location.Value, locRec))
								{
									ev.Detail.Place.DeleteTag("_LOC");
								}
							}
						}
					}
				}
				this.RecordNotify(locRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(locRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteRecord(TGEDCOMRecord record, bool confirm)
		{
			bool res = false;

			if (record != null)
			{
				string xref = record.XRef;

				switch (record.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
						res = this.DeleteIndividualRecord(record as TGEDCOMIndividualRecord, confirm);
						break;

					case TGEDCOMRecordType.rtFamily:
						res = this.DeleteFamilyRecord(record as TGEDCOMFamilyRecord, confirm);
						break;

					case TGEDCOMRecordType.rtNote:
						res = this.DeleteNoteRecord(record as TGEDCOMNoteRecord, confirm);
						break;

					case TGEDCOMRecordType.rtMultimedia:
						res = this.DeleteMediaRecord(record as TGEDCOMMultimediaRecord, confirm);
						break;

					case TGEDCOMRecordType.rtSource:
						res = this.DeleteSourceRecord(record as TGEDCOMSourceRecord, confirm);
						break;

					case TGEDCOMRecordType.rtRepository:
						res = this.DeleteRepositoryRecord(record as TGEDCOMRepositoryRecord, confirm);
						break;

					case TGEDCOMRecordType.rtGroup:
						res = this.DeleteGroupRecord(record as TGEDCOMGroupRecord, confirm);
						break;

					case TGEDCOMRecordType.rtResearch:
						res = this.DeleteResearchRecord(record as TGEDCOMResearchRecord, confirm);
						break;

					case TGEDCOMRecordType.rtTask:
						res = this.DeleteTaskRecord(record as TGEDCOMTaskRecord, confirm);
						break;

					case TGEDCOMRecordType.rtCommunication:
						res = this.DeleteCommunicationRecord(record as TGEDCOMCommunicationRecord, confirm);
						break;

					case TGEDCOMRecordType.rtLocation:
						res = this.DeleteLocationRecord(record as TGEDCOMLocationRecord, confirm);
						break;
				}

				if (res) {
					this.FTree.Header.TransmissionDateTime = DateTime.Now;
					TfmGEDKeeper.Instance.SearchMan.DeleteRecord(this, xref);
				}
			}

			return res;
		}

		#endregion

		#region Modification objects

		public bool ModifyName(ref NamesTable.NameEntry aName)
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
				dlg.Dispose();
			}
			return Result;
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
					dlg.Dispose();
				}
			}
			return Result;
		}

		public bool ModifyFamily(ref TGEDCOMFamilyRecord familyRec, TFamilyTarget target, TGEDCOMIndividualRecord person)
		{
			bool result = false;

			if (target == TFamilyTarget.ftSpouse && person != null) {
				TGEDCOMSex sex = person.Sex;
				if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined) {
					GKUtils.ShowError(LangMan.LSList[210]);
					return result;
				}
			}

			using (TfmFamilyEdit dlg = new TfmFamilyEdit(this))
			{
				bool exists = (familyRec != null);
				if (!exists) {
					familyRec = new TGEDCOMFamilyRecord(this.FTree, this.FTree, "", "");
					familyRec.InitNew();
				}

				if (target == TFamilyTarget.ftSpouse) {
					if (person != null) familyRec.aux_AddSpouse(person);
				} else if (target == TFamilyTarget.ftChild) {
					if (person != null) familyRec.aux_AddChild(person);
				}

				dlg.Family = familyRec;
				result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK);

				if (result) {
					if (!exists) {
						this.FTree.AddRecord(familyRec);
					}
				} else {
					if (!exists) {
						GKUtils.CleanFamily(familyRec);
						familyRec.Dispose();
						familyRec = null;
					}
				}
			}

			return result;
		}

		public bool ModifyNote(ref TGEDCOMNoteRecord noteRec)
		{
			bool result = false;

			TfmNoteEdit dlg = new TfmNoteEdit(this);
			try
			{
				bool exists = noteRec != null;
				if (!exists)
				{
					noteRec = new TGEDCOMNoteRecord(this.FTree, this.FTree, "", "");
					noteRec.InitNew();
				}

				dlg.NoteRecord = noteRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(noteRec);
					}
					result = true;
				}
				else
				{
					if (!exists)
					{
						noteRec.Dispose();
						noteRec = null;
					}
				}
			}
			finally
			{
				dlg.Dispose();
			}

			return result;
		}

		public bool ModifyMedia(ref TGEDCOMMultimediaRecord mediaRec)
		{
			bool result = false;

			TfmMediaEdit dlg = new TfmMediaEdit(this);
			try
			{
				bool exists = mediaRec != null;
				if (!exists)
				{
					mediaRec = new TGEDCOMMultimediaRecord(this.FTree, this.FTree, "", "");
					mediaRec.FileReferences.Add(new TGEDCOMFileReferenceWithTitle(this.FTree, mediaRec, "", ""));
					mediaRec.InitNew();
				}

				dlg.MediaRec = mediaRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(mediaRec);
					}
					result = true;
				}
				else
				{
					if (!exists)
					{
						mediaRec.Dispose();
						mediaRec = null;
					}
				}
			}
			finally
			{
				dlg.Dispose();
			}

			return result;
		}

		public bool ModifySource(ref TGEDCOMSourceRecord sourceRec)
		{
			bool Result = false;
			TfmSourceEdit fmSrcEdit = new TfmSourceEdit(this);
			try
			{
				bool exists = sourceRec != null;
				if (!exists)
				{
					sourceRec = new TGEDCOMSourceRecord(this.FTree, this.FTree, "", "");
					sourceRec.InitNew();
				}
				fmSrcEdit.SourceRecord = sourceRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(sourceRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						sourceRec.Dispose();
						sourceRec = null;
					}
				}
			}
			finally
			{
				fmSrcEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyRepository(ref TGEDCOMRepositoryRecord repRec)
		{
			bool Result = false;
			TfmRepositoryEdit fmRepEdit = new TfmRepositoryEdit(this);
			try
			{
				bool exists = repRec != null;
				if (!exists)
				{
					repRec = new TGEDCOMRepositoryRecord(this.FTree, this.FTree, "", "");
					repRec.InitNew();
				}
				fmRepEdit.Repository = repRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmRepEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(repRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						repRec.Dispose();
						repRec = null;
					}
				}
			}
			finally
			{
				fmRepEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyGroup(ref TGEDCOMGroupRecord groupRec)
		{
			bool Result = false;
			TfmGroupEdit fmGrpEdit = new TfmGroupEdit(this);
			try
			{
				bool exists = groupRec != null;
				if (!exists)
				{
					groupRec = new TGEDCOMGroupRecord(this.FTree, this.FTree, "", "");
					groupRec.InitNew();
				}
				fmGrpEdit.Group = groupRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmGrpEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(groupRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						groupRec.Dispose();
						groupRec = null;
					}
				}
			}
			finally
			{
				fmGrpEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyResearch(ref TGEDCOMResearchRecord researchRec)
		{
			bool Result = false;
			TfmResearchEdit fmResEdit = new TfmResearchEdit(this);
			try
			{
				bool exists = researchRec != null;
				if (!exists)
				{
					researchRec = new TGEDCOMResearchRecord(this.FTree, this.FTree, "", "");
					researchRec.InitNew();
				}
				fmResEdit.Research = researchRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmResEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(researchRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						researchRec.Dispose();
						researchRec = null;
					}
				}
			}
			finally
			{
				fmResEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyTask(ref TGEDCOMTaskRecord taskRec)
		{
			bool Result = false;
			TfmTaskEdit fmTaskEdit = new TfmTaskEdit(this);
			try
			{
				bool exists = taskRec != null;
				if (!exists)
				{
					taskRec = new TGEDCOMTaskRecord(this.FTree, this.FTree, "", "");
					taskRec.InitNew();
				}
				fmTaskEdit.Task = taskRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmTaskEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(taskRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						taskRec.Dispose();
						taskRec = null;
					}
				}
			}
			finally
			{
				fmTaskEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyCommunication(ref TGEDCOMCommunicationRecord commRec)
		{
			bool Result = false;
			TfmCommunicationEdit fmCorrEdit = new TfmCommunicationEdit(this);
			try
			{
				bool exists = commRec != null;
				if (!exists)
				{
					commRec = new TGEDCOMCommunicationRecord(this.FTree, this.FTree, "", "");
					commRec.InitNew();
				}
				fmCorrEdit.Communication = commRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmCorrEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(commRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						commRec.Dispose();
						commRec = null;
					}
				}
			}
			finally
			{
				fmCorrEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyLocation(ref TGEDCOMLocationRecord locRec)
		{
			bool Result = false;
			TfmLocationEdit fmLocEdit = new TfmLocationEdit(this);
			try
			{
				bool exists = locRec != null;
				if (!exists)
				{
					locRec = new TGEDCOMLocationRecord(this.FTree, this.FTree, "", "");
					locRec.InitNew();
				}
				fmLocEdit.LocationRecord = locRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmLocEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(locRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						locRec.Dispose();
						locRec = null;
					}
				}
			}
			finally
			{
				fmLocEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyAddress(Form sender, TGEDCOMAddress address)
		{
			bool res;

            using (TfmAddressEdit dlg = new TfmAddressEdit()) {
				dlg.Address = address;
				res = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, sender, false) == DialogResult.OK);
			}

            return res;
		}

		public bool ModifyRecGroup(Form sender, TGEDCOMIndividualRecord iRec, TGEDCOMGroupRecord groupRec, TRecAction action)
		{
			bool res = false;

			switch (action)
			{
				case TRecAction.raAdd:
					groupRec = this.SelectRecord(TGEDCOMRecordType.rtGroup, null) as TGEDCOMGroupRecord;
					res = (groupRec != null && groupRec.aux_AddMember(iRec));
					break;

				case TRecAction.raDelete:
					res = (GKUtils.ShowQuestion(LangMan.LSList[188]) != DialogResult.No && groupRec.aux_RemoveMember(iRec));
					break;
			}

            return res;
		}

		public bool ModifyRecAssociation(Form sender, TGEDCOMIndividualRecord iRec, TGEDCOMAssociation aAssociation, TRecAction action)
		{
			bool Result = false;

            if (action == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[243]) != DialogResult.No)
				{
					iRec.Associations.DeleteObject(aAssociation);
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
					if (action == TRecAction.raEdit && aAssociation != null)
					{
						ast = aAssociation;
					}
					else
					{
						ast = new TGEDCOMAssociation(this.FTree, iRec, "", "");
					}
					fmAstEdit.Association = ast;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmAstEdit, sender, false);
					if (action == TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							iRec.Associations.Add(ast);
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
					fmAstEdit.Dispose();
				}
			}

            return Result;
		}

		public bool ModifyRecEvent(Form sender, TGEDCOMRecord record, ref TGEDCOMCustomEvent aEvent, TRecAction action)
		{
            bool result = false;

			try
			{
				if (action == TRecAction.raDelete)
				{
					if (GKUtils.ShowQuestion(LangMan.LSList[239]) != DialogResult.No)
					{
						if (record is TGEDCOMIndividualRecord) {
							(record as TGEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
						} else {
							(record as TGEDCOMFamilyRecord).FamilyEvents.DeleteObject(aEvent as TGEDCOMFamilyEvent);
						}

						aEvent = null;
						result = true;
					}
				}
				else
				{
					using (TfmEventEdit fmEventEdit = new TfmEventEdit(this))
					{
						TGEDCOMCustomEvent newEvent;
						if (aEvent != null) {
							newEvent = aEvent;
						} else {
							if (record is TGEDCOMIndividualRecord) {
								newEvent = new TGEDCOMIndividualEvent(this.FTree, record, "", "");
							} else {
								newEvent = new TGEDCOMFamilyEvent(this.FTree, record, "", "");
							}
						}

						fmEventEdit.Event = newEvent;
						DialogResult dialogResult = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmEventEdit, sender, true);

						if (dialogResult != DialogResult.OK)
						{
							if (dialogResult == DialogResult.Cancel)
							{
								if (aEvent == null)
								{
									newEvent.Dispose();
								}
							}
						}
						else
						{
							newEvent = fmEventEdit.Event;

							if (aEvent == null)
							{
								if (record is TGEDCOMIndividualRecord) {
									(record as TGEDCOMIndividualRecord).AddIndividualEvent(newEvent);
								} else {
									(record as TGEDCOMFamilyRecord).FamilyEvents.Add(newEvent as TGEDCOMFamilyEvent);
								}
							}
							else
							{
								if (record is TGEDCOMIndividualRecord && !object.Equals(newEvent, aEvent))
								{
									(record as TGEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
									(record as TGEDCOMIndividualRecord).AddIndividualEvent(newEvent);
								}
							}

							aEvent = newEvent;
							result = true;
						}
					}
				}

				if (result) this.Modified = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ModifyRecEvent(): " + E.Message);
				return false;
			}

            return result;
        }

		public bool ModifyRecMultimedia(Form sender, TGEDCOMRecord record, TGEDCOMMultimediaLink mmLink, TRecAction action)
		{
			bool res = false;
			TGEDCOMMultimediaRecord mmRec;

			switch (action) {
				case TRecAction.raAdd:
					mmRec = this.SelectRecord(TGEDCOMRecordType.rtMultimedia, new object[0]) as TGEDCOMMultimediaRecord;
					res = (record.aux_AddMultimedia(mmRec) != null);
					break;

				case TRecAction.raEdit:
					if (mmLink != null) {
						mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
						res = this.ModifyMedia(ref mmRec);
					}
					break;

				case TRecAction.raDelete:
					if (GKUtils.ShowQuestion(LangMan.LSList[241]) != DialogResult.No) {
						record.MultimediaLinks.DeleteObject(mmLink);
						res = true;
					}
					break;
			}
			
			if (res) this.Modified = true;
			
			return res;
		}

		public bool ModifyRecNote(Form sender, TGEDCOMRecord record, TGEDCOMNotes aNote, TRecAction action)
		{
			bool result = false;

			if (action == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[240]) != DialogResult.No)
				{
					record.Notes.DeleteObject(aNote);
					result = true;
					this.Modified = true;
				}
			}
			else
			{
				if (action == TRecAction.raEdit && aNote != null)
				{
					TGEDCOMNoteRecord noteRec = aNote.Value as TGEDCOMNoteRecord;
					result = this.ModifyNote(ref noteRec);
				}
				else
				{
					TGEDCOMNoteRecord noteRec = this.SelectRecord(TGEDCOMRecordType.rtNote, null) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, record, "", "");
						note.Value = noteRec;
						record.Notes.Add(note);
						result = true;
					}
				}
			}

			return result;
		}

		public bool ModifyRecSource(Form sender, TGEDCOMRecord record, TGEDCOMSourceCitation aCit, TRecAction action)
		{
			bool Result = false;
			if (action == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[242]) != DialogResult.No)
				{
					record.SourceCitations.DeleteObject(aCit);
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
					if (action == TRecAction.raEdit && aCit != null)
					{
						cit = aCit;
					}
					else
					{
						cit = new TGEDCOMSourceCitation(this.FTree, record, "", "");
					}
					fmSrcCitEdit.SourceCitation = cit;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, sender, false);
					if (action == TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							record.SourceCitations.Add(cit);
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
					fmSrcCitEdit.Dispose();
				}
			}
			return Result;
		}

		public bool ModifyRecUserRef(Form sender, TGEDCOMRecord record, TGEDCOMUserReference aUserRef, TRecAction action)
		{
			bool Result = false;
			if (action == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[244]) != DialogResult.No)
				{
					record.UserReferences.DeleteObject(aUserRef);
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
					if (action == TRecAction.raEdit && aUserRef != null)
					{
						@ref = aUserRef;
					}
					else
					{
						@ref = new TGEDCOMUserReference(this.FTree, record, "", "");
					}
					dlg.UserRef = @ref;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, sender, false);
					if (action == TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							record.UserReferences.Add(@ref);
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
					dlg.Dispose();
				}
			}
			return Result;
		}

		public bool ModifyTagMultimedia(TGEDCOMTagWithLists aTag, TGEDCOMMultimediaLink aLink, TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[241]) != DialogResult.No)
				{
					aTag.MultimediaLinks.DeleteObject(aLink);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				if (anAction == TRecAction.raEdit)
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
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(TGEDCOMRecordType.rtMultimedia, new object[0]) as TGEDCOMMultimediaRecord;
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

		public bool ModifyTagNote(TGEDCOMTagWithLists aTag, TGEDCOMNotes aNote, TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[240]) != DialogResult.No)
				{
					aTag.Notes.DeleteObject(aNote);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				if (anAction == TRecAction.raEdit)
				{
					if (aNote != null)
					{
						TGEDCOMNoteRecord noteRec = aNote.Value as TGEDCOMNoteRecord;
						Result = this.ModifyNote(ref noteRec);
					}
				}
				else
				{
					TGEDCOMNoteRecord noteRec = this.SelectRecord(TGEDCOMRecordType.rtNote, new object[0]) as TGEDCOMNoteRecord;
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

		public bool ModifyTagSource(TGEDCOMTagWithLists aTag, TGEDCOMSourceCitation aCit, TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TRecAction.raDelete)
			{
				if (GKUtils.ShowQuestion(LangMan.LSList[242]) != DialogResult.No)
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
					if (anAction == TRecAction.raEdit && aCit != null)
					{
						cit = aCit;
					}
					else
					{
						cit = new TGEDCOMSourceCitation(this.FTree, aTag, "", "");
					}
					fmSrcCitEdit.SourceCitation = cit;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, GKUI.TfmGEDKeeper.Instance, false);
					if (anAction == TRecAction.raAdd)
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
					fmSrcCitEdit.Dispose();
				}
			}
			return Result;
		}

		#endregion

		#region RecLists refresh

		public void RecListAssociationsRefresh(TGEDCOMIndividualRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null)
				{
					list.Items.Clear();
				}
				if (record.Associations.Count != 0)
				{
					if (list == null && summary != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[154] + ":");
					}

					int num = record.Associations.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMAssociation ast = record.Associations[idx];
						string nm = ((ast.Individual == null) ? "" : ast.Individual.aux_GetNameStr(true, false));
						if (list == null && summary != null)
						{
							summary.Add("    " + ast.Relation + " " + GKUtils.HyperLink(ast.Individual.XRef, nm, 0));
						}
						if (list != null)
						{
							GKListItem item = list.AddItem(ast.Relation, ast);
							item.SubItems.Add(nm);
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListAssociationsRefresh(): " + E.Message);
			}
		}

		public void RecListFamilyEventsRefresh(TGEDCOMFamilyRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null)
				{
					list.Items.Clear();
				}

				if (record.FamilyEvents.Count != 0)
				{
					if (summary != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[83] + ":");
					}

					int num = record.FamilyEvents.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMFamilyEvent evt = record.FamilyEvents[idx];
						int ev = GKUtils.GetFamilyEventIndex(evt.Name);
						string st;
						if (ev == 0)
						{
							st = evt.Detail.Classification;
						}
						else
						{
							if (ev > 0)
							{
								st = LangMan.LSList[(int)GKData.FamilyEvents[ev].Name - 1];
							}
							else
							{
								st = evt.Name;
							}
						}
						if (summary != null)
						{
							summary.Add(st + ": " + GKUtils.GetEventDesc(evt.Detail));

							UIUtils.ShowDetailCause(evt.Detail, summary);
						}

						UIUtils.ShowDetailInfo(evt.Detail, summary);

						if (list != null)
						{
							GKListItem item = list.AddItem(Convert.ToString(idx + 1), evt);
							item.SubItems.Add(st);
							item.SubItems.Add(GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
							item.SubItems.Add(evt.Detail.Place.StringValue);
							item.SubItems.Add(GKUtils.GetEventCause(evt.Detail));
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListFamilyEventsRefresh(): " + E.Message);
			}
		}

		public void RecListGroupsRefresh(TGEDCOMIndividualRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null) {
					list.Items.Clear();
				}

                if (record.Groups.Count != 0) {
					if (list == null && summary != null) {
						summary.Add("");
						summary.Add(LangMan.LSList[58] + ":");
					}

					int num = record.Groups.Count - 1;
					for (int idx = 0; idx <= num; idx++) {
						TGEDCOMPointer ptr = record.Groups[idx];
						TGEDCOMGroupRecord grp = ptr.Value as TGEDCOMGroupRecord;
						if (grp != null) {
							if (list == null && summary != null) {
								summary.Add("    " + GKUtils.HyperLink(grp.XRef, grp.GroupName, 0));
							}
							if (list != null) {
								list.AddItem(grp.GroupName, grp);
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListGroupsRefresh(): " + E.Message);
			}
		}

		public void RecListIndividualEventsRefresh(TGEDCOMIndividualRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null)
				{
					list.Items.Clear();
				}

				if (record.IndividualEvents.Count != 0)
				{
					if (summary != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[83] + ":");
					}

					int num = record.IndividualEvents.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMCustomEvent evt = record.IndividualEvents[idx];
						int ev = GKUtils.GetPersonEventIndex(evt.Name);
						string st;
						if (ev == 0)
						{
							st = evt.Detail.Classification;
						}
						else
						{
							if (ev > 0)
							{
								st = LangMan.LSList[(int)GKData.PersonEvents[ev].Name - 1];
							}
							else
							{
								st = evt.Name;
							}
						}

						if (summary != null)
						{
							string sv = "";
							if (evt.StringValue != "") {
								sv = evt.StringValue + ", ";
							}
							summary.Add(st + ": " + sv + GKUtils.GetEventDesc(evt.Detail));

							UIUtils.ShowDetailCause(evt.Detail, summary);
							UIUtils.ShowAddressSummary(evt.Detail.Address, summary);
							UIUtils.ShowDetailInfo(evt.Detail, summary);
						}

						if (list != null)
						{
							GKListItem item = list.AddItem(Convert.ToString(idx + 1), evt);
							item.SubItems.Add(st);
							item.SubItems.Add(GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
							st = evt.Detail.Place.StringValue;
							if (evt.StringValue != "")
							{
								st = st + " [" + evt.StringValue + "]";
							}
							item.SubItems.Add(st);
							item.SubItems.Add(GKUtils.GetEventCause(evt.Detail));
						}
					}

					if (list != null) list.ResizeColumn(2);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListIndividualEventsRefresh(): " + E.Message);
			}
		}

		public void RecListMediaRefresh(TGEDCOMRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null) {
					list.Items.Clear();
				}

                if (record.MultimediaLinks.Count != 0) {
					if (list == null && summary != null) {
						summary.Add("");
						summary.Add(LangMan.LSList[55] + " (" + record.MultimediaLinks.Count.ToString() + "):");
					}

					int num = record.MultimediaLinks.Count - 1;
					for (int idx = 0; idx <= num; idx++) {
						TGEDCOMMultimediaLink mmLink = record.MultimediaLinks[idx];
						TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
						if (mmRec != null && mmRec.FileReferences.Count != 0) {
							string st = mmRec.FileReferences[0].Title;
							if (list == null && summary != null) {
								summary.Add("  " + GKUtils.HyperLink(mmRec.XRef, st, 0) + " (" + GKUtils.HyperLink("view_" + mmRec.XRef, "просмотр", 0) + ")");
							}
							if (list != null) {
								list.AddItem(st, mmLink);
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListMediaRefresh(): " + E.Message);
			}
		}

		public void RecListNotesRefresh(TGEDCOMRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null)
				{
					list.Items.Clear();
				}

				if (record.Notes.Count != 0)
				{
					if (summary != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[54] + " (" + record.Notes.Count.ToString() + "):");
					}

					int num = record.Notes.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMNotes note = record.Notes[idx];

						if (summary != null)
						{
							int num2 = note.Notes.Count - 1;
							for (int i = 0; i <= num2; i++)
							{
								string st = note.Notes[i];
								summary.Add(st);
							}

							if (idx < record.Notes.Count - 1)
							{
								summary.Add("");
							}
						}

						if (list != null)
						{
							list.AddItem(note.Notes.Text.Trim(), note);
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListNotesRefresh(): " + E.Message);
			}
		}

		public void RecListSourcesRefresh(TGEDCOMRecord record, GKListView list, StringList summary)
		{
			try
			{
				if (list != null)
				{
					list.Items.Clear();
				}
				if (record.SourceCitations.Count != 0)
				{
					if (list == null && summary != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[56] + " (" + record.SourceCitations.Count.ToString() + "):");
					}

					int num = record.SourceCitations.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMSourceCitation cit = record.SourceCitations[idx];
						TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
						if (sourceRec != null)
						{
							string nm = "\"" + sourceRec.FiledByEntry + "\"";
							if (cit.Page != "")
							{
								nm = nm + ", " + cit.Page;
							}
							if (list == null && summary != null)
							{
								summary.Add("  " + GKUtils.HyperLink(sourceRec.XRef, nm, 0));
							}
							if (list != null)
							{
								GKListItem item = list.AddItem(sourceRec.Originator.Text.Trim(), cit);
								item.SubItems.Add(nm);
							}
						}
					}
					if (list != null)
					{
						list.ResizeColumn(1);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListSourcesRefresh(): " + E.Message);
			}
		}

		#endregion

		#region Lists titles setup

		public void SetupRecEventsList(GKSheetList aList, bool PersonsMode)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn("№", 25, false);
			aList.AddColumn(LangMan.LSList[203], 90, false);
			aList.AddColumn(LangMan.LSList[139], 80, false);
			if (!PersonsMode)
			{
				aList.AddColumn(LangMan.LSList[204], 200, false);
			}
			else
			{
				aList.AddColumn(LangMan.LSList[235], 200, false);
			}
			aList.AddColumn(LangMan.LSList[205], 130, false);
			aList.Columns_EndUpdate();
		}

		public void SetupRecMediaList(GKSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(LangMan.LSList[55], 300, false);
			aList.Columns_EndUpdate();
		}

		public void SetupRecNotesList(GKSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(LangMan.LSList[108], 300, false);
			aList.Columns_EndUpdate();
		}

		public void SetupRecSourcesList(GKSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(LangMan.LSList[142], 120, false);
			aList.AddColumn(LangMan.LSList[125], 180, false);
			aList.Columns_EndUpdate();
		}

		#endregion

		#region Show objects information

		public void ShowFamilyInfo(TGEDCOMFamilyRecord familyRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (familyRec != null)
					{
						summary.Add("");

						TGEDCOMIndividualRecord irec = familyRec.Husband.Value as TGEDCOMIndividualRecord;
						string st = ((irec == null) ? LangMan.LSList[64] : GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0));
						summary.Add(LangMan.LSList[115] + ": " + st + GKUtils.GetLifeStr(irec));

						irec = (familyRec.Wife.Value as TGEDCOMIndividualRecord);
						st = ((irec == null) ? LangMan.LSList[63] : GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0));
						summary.Add(LangMan.LSList[116] + ": " + st + GKUtils.GetLifeStr(irec));

						summary.Add("");
						if (familyRec.Childrens.Count != 0)
						{
							summary.Add(LangMan.LSList[118] + ":");
						}

						int num = familyRec.Childrens.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							irec = (familyRec.Childrens[i].Value as TGEDCOMIndividualRecord);
							summary.Add("    " + GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0) + GKUtils.GetLifeStr(irec));
						}
						summary.Add("");

						this.RecListFamilyEventsRefresh(familyRec, null, summary);
						this.RecListNotesRefresh(familyRec, null, summary);
						this.RecListMediaRefresh(familyRec, null, summary);
						this.RecListSourcesRefresh(familyRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowFamilyInfo(): " + E.Message);
			}
		}

		public void ShowGroupInfo(TGEDCOMGroupRecord groupRec, StringList summary)
		{
			try
			{
				StringList mbrList = new StringList();
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (groupRec != null)
					{
						summary.Add("");
						summary.Add("~ub+1~" + groupRec.GroupName + "~bu-1~");
						summary.Add("");
						summary.Add(LangMan.LSList[126] + " (" + groupRec.Members.Count.ToString() + "):");

						int num = groupRec.Members.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMPointer ptr = groupRec.Members[i];
							TGEDCOMIndividualRecord member = ptr.Value as TGEDCOMIndividualRecord;
							mbrList.AddObject(member.aux_GetNameStr(true, false), member);
						}
						mbrList.Sort();

						int num2 = mbrList.Count - 1;
						for (int i = 0; i <= num2; i++)
						{
							TGEDCOMIndividualRecord member = mbrList.GetObject(i) as TGEDCOMIndividualRecord;
							summary.Add("    " + GKUtils.HyperLink(member.XRef, mbrList[i], i + 1));
						}

						this.RecListNotesRefresh(groupRec, null, summary);
						this.RecListMediaRefresh(groupRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
					mbrList.Free();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowGroupInfo(): " + E.Message);
			}
		}

		public void ShowMultimediaInfo(TGEDCOMMultimediaRecord mediaRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (mediaRec != null)
					{
						summary.Add("");
						summary.Add("~ub+1~" + mediaRec.FileReferences[0].Title + "~bu-1~");
						summary.Add("");
						summary.Add("[ " + GKUtils.HyperLink("view_" + mediaRec.XRef, LangMan.LSList[148], 0) + " ]");
						summary.Add("");
						summary.Add(LangMan.LSList[234] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							UIUtils.ShowSubjectLinks(this.FTree[i], mediaRec, summary);
						}

						this.RecListNotesRefresh(mediaRec, null, summary);
						this.RecListSourcesRefresh(mediaRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowMultimediaInfo(): " + E.Message);
			}
		}

		public void ShowNoteInfo(TGEDCOMNoteRecord noteRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (noteRec != null)
					{
						summary.Add("");
						summary.AddStrings(noteRec.Note);
						summary.Add("");
						summary.Add(LangMan.LSList[234] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							UIUtils.ShowSubjectLinks(this.FTree[i], noteRec, summary);
						}
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowNoteInfo(): " + E.Message);
			}
		}

		public void ShowPersonInfo(TGEDCOMIndividualRecord iRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				summary.Clear();
				try
				{
					if (iRec != null)
					{
						summary.Add("");
						summary.Add("~ub+1~" + iRec.aux_GetNameStr(true, true) + "~bu-1~");
						summary.Add(LangMan.LSList[87] + ": " + GKUtils.SexStr(iRec.Sex));
						try
						{
							if (iRec.ChildToFamilyLinks.Count != 0)
							{
								summary.Add("");
								summary.Add(LangMan.LSList[152] + ":");
								TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
								TGEDCOMIndividualRecord rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
								string st;
								if (rel_person != null)
								{
									st = GKUtils.HyperLink(rel_person.XRef, rel_person.aux_GetNameStr(true, false), 0);
								}
								else
								{
									st = LangMan.LSList[64];
								}
								summary.Add("  " + LangMan.LSList[150] + ": " + st + GKUtils.GetLifeStr(rel_person));
								rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
								if (rel_person != null)
								{
									st = GKUtils.HyperLink(rel_person.XRef, rel_person.aux_GetNameStr(true, false), 0);
								}
								else
								{
									st = LangMan.LSList[63];
								}
								summary.Add("  " + LangMan.LSList[151] + ": " + st + GKUtils.GetLifeStr(rel_person));
							}
						}
						catch (Exception E)
						{
							SysUtils.LogWrite("GKBase.ShowPersonInfo().Parents(): " + E.Message);
						}

						try
						{
							int num = iRec.SpouseToFamilyLinks.Count - 1;
							for (int idx = 0; idx <= num; idx++)
							{
								TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[idx].Family;
								if (family == null)
								{
									SysUtils.LogWrite("File (" + this.Tree.FileName + "), iRec (" + iRec.XRef + "): empty family entry");
								}
								else
								{
									if (GKUtils.IsRecordAccess(family.Restriction, this.FShieldState))
									{
										string st;
										TGEDCOMPointer sp;
										string unk;
										if (iRec.Sex == TGEDCOMSex.svMale)
										{
											sp = family.Wife;
											st = LangMan.LSList[116] + ": ";
											unk = LangMan.LSList[63];
										}
										else
										{
											sp = family.Husband;
											st = LangMan.LSList[115] + ": ";
											unk = LangMan.LSList[64];
										}
										string marr = GKUtils.GetMarriageDate(family, TDateFormat.dfDD_MM_YYYY);
										if (marr != "")
										{
											marr = LangMan.LSList[236] + " " + marr;
										}
										else
										{
											marr = LangMan.LSList[237];
										}
										TGEDCOMIndividualRecord rel_person = sp.Value as TGEDCOMIndividualRecord;
										summary.Add("");
										if (rel_person != null)
										{
											st = st + GKUtils.HyperLink(rel_person.XRef, rel_person.aux_GetNameStr(true, false), 0) + " (" + GKUtils.HyperLink(family.XRef, marr, 0) + ")";
										}
										else
										{
											st = st + unk + " (" + GKUtils.HyperLink(family.XRef, marr, 0) + ")";
										}
										summary.Add(st);
										if (family.Childrens.Count != 0)
										{
											summary.Add("");
											summary.Add(LangMan.LSList[118] + ":");
										}

										int num2 = family.Childrens.Count - 1;
										for (int i = 0; i <= num2; i++)
										{
											rel_person = (family.Childrens[i].Value as TGEDCOMIndividualRecord);
											summary.Add("    " + GKUtils.HyperLink(rel_person.XRef, rel_person.aux_GetNameStr(true, false), 0) + GKUtils.GetLifeStr(rel_person));
										}
									}
								}
							}
						}
						catch (Exception E)
						{
							SysUtils.LogWrite("GKBase.ShowPersonInfo().Families(): " + E.Message);
						}

						this.RecListIndividualEventsRefresh(iRec, null, summary);
						this.RecListNotesRefresh(iRec, null, summary);
						this.RecListMediaRefresh(iRec, null, summary);
						this.RecListSourcesRefresh(iRec, null, summary);
						this.RecListAssociationsRefresh(iRec, null, summary);
						this.RecListGroupsRefresh(iRec, null, summary);

                        UIUtils.ShowPersonNamesakes(this.FTree, iRec, summary);
                        UIUtils.ShowPersonExtInfo(this.FTree, iRec, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowPersonInfo(): " + E.Message);
			}
		}

		public void ShowSourceInfo(TGEDCOMSourceRecord sourceRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				StringList link_list = new StringList();
				try
				{
					summary.Clear();
					if (sourceRec != null)
					{
						summary.Add("");
						summary.Add("~ub+1~" + sourceRec.FiledByEntry + "~bu-1~");
						summary.Add("");
						summary.Add(LangMan.LSList[142] + ": " + sourceRec.Originator.Text.Trim());
						summary.Add(LangMan.LSList[125] + ": \"" + sourceRec.Title.Text.Trim() + "\"");
						summary.Add(LangMan.LSList[143] + ": \"" + sourceRec.Publication.Text.Trim() + "\"");

						if (sourceRec.RepositoryCitations.Count > 0)
						{
							summary.Add("");
							summary.Add(LangMan.LSList[57] + ":");

							int num = sourceRec.RepositoryCitations.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMRepositoryRecord rep = sourceRec.RepositoryCitations[i].Value as TGEDCOMRepositoryRecord;
								summary.Add("    " + GKUtils.HyperLink(rep.XRef, rep.RepositoryName, 0));
							}
						}

						summary.Add("");
						summary.Add(LangMan.LSList[234] + ":");

						int num2 = this.FTree.RecordsCount - 1;
						for (int j = 0; j <= num2; j++)
						{
							UIUtils.ShowSubjectLinks(this.FTree[j], sourceRec, link_list);
						}

						link_list.Sort();

						int num3 = link_list.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							summary.Add(link_list[j]);
						}

						this.RecListNotesRefresh(sourceRec, null, summary);
						this.RecListMediaRefresh(sourceRec, null, summary);
					}
				}
				finally
				{
					link_list.Free();
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowSourceInfo(): " + E.Message);
			}
		}

		public void ShowRepositoryInfo(TGEDCOMRepositoryRecord repositoryRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (repositoryRec != null)
					{
						summary.Add("");
						summary.Add("~ub+1~" + repositoryRec.RepositoryName.Trim() + "~bu-1~");
						summary.Add("");

						UIUtils.ShowAddressSummary(repositoryRec.Address, summary);

						summary.Add("");
						summary.Add(LangMan.LSList[56] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord rec = this.FTree[i];

							if (rec is TGEDCOMSourceRecord)
							{
								TGEDCOMSourceRecord srcRec = rec as TGEDCOMSourceRecord;

								int num2 = srcRec.RepositoryCitations.Count - 1;
								for (int j = 0; j <= num2; j++)
								{
									if (object.Equals(srcRec.RepositoryCitations[j].Value, repositoryRec)) {
										summary.Add("    " + GKUtils.GenRecordLink(srcRec, false));
									}
								}
							}
						}

						this.RecListNotesRefresh(repositoryRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowRepositoryInfo(): " + E.Message);
			}
		}

		public void ShowResearchInfo(TGEDCOMResearchRecord researchRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (researchRec != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[125] + ": \"~ub+1~" + researchRec.ResearchName.Trim() + "~bu-1~\"");
						summary.Add("");
						summary.Add(LangMan.LSList[178] + ": " + LangMan.LSList[(int)GKData.PriorityNames[(int)researchRec.Priority] - 1]);
						summary.Add(LangMan.LSList[117] + ": " + LangMan.LSList[(int)GKData.StatusNames[(int)researchRec.Status] - 1] + " (" + researchRec.Percent.ToString() + "%)");
						summary.Add(LangMan.LSList[180] + ": " + GKUtils.GEDCOMDateToStr(researchRec.StartDate, TDateFormat.dfDD_MM_YYYY));
						summary.Add(LangMan.LSList[181] + ": " + GKUtils.GEDCOMDateToStr(researchRec.StopDate, TDateFormat.dfDD_MM_YYYY));

						if (researchRec.Tasks.Count > 0)
						{
							summary.Add("");
							summary.Add(LangMan.LSList[60] + ":");

							int num = researchRec.Tasks.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMTaskRecord taskRec = researchRec.Tasks[i].Value as TGEDCOMTaskRecord;
								summary.Add("    " + GKUtils.GenRecordLink(taskRec, false));
							}
						}

						if (researchRec.Communications.Count > 0)
						{
							summary.Add("");
							summary.Add(LangMan.LSList[61] + ":");

							int num2 = researchRec.Communications.Count - 1;
							for (int i = 0; i <= num2; i++)
							{
								TGEDCOMCommunicationRecord corrRec = researchRec.Communications[i].Value as TGEDCOMCommunicationRecord;
								summary.Add("    " + GKUtils.GenRecordLink(corrRec, false));
							}
						}

						if (researchRec.Groups.Count != 0)
						{
							summary.Add("");
							summary.Add(LangMan.LSList[58] + ":");

							int num3 = researchRec.Groups.Count - 1;
							for (int i = 0; i <= num3; i++)
							{
								TGEDCOMGroupRecord grp = researchRec.Groups[i].Value as TGEDCOMGroupRecord;
								summary.Add("    " + GKUtils.HyperLink(grp.XRef, grp.GroupName, 0));
							}
						}

						this.RecListNotesRefresh(researchRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowResearchInfo(): " + E.Message);
			}
		}

		public void ShowTaskInfo(TGEDCOMTaskRecord taskRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (taskRec != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[182] + ": ~ub+1~" + GKUtils.GetTaskGoalStr(taskRec) + "~bu-1~");
						summary.Add("");
						summary.Add(LangMan.LSList[178] + ": " + LangMan.LSList[(int)GKData.PriorityNames[(int)taskRec.Priority] - 1]);
						summary.Add(LangMan.LSList[180] + ": " + GKUtils.GEDCOMDateToStr(taskRec.StartDate, TDateFormat.dfDD_MM_YYYY));
						summary.Add(LangMan.LSList[181] + ": " + GKUtils.GEDCOMDateToStr(taskRec.StopDate, TDateFormat.dfDD_MM_YYYY));

						this.RecListNotesRefresh(taskRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowTaskInfo(): " + E.Message);
			}
		}

		public void ShowCommunicationInfo(TGEDCOMCommunicationRecord commRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				try
				{
					summary.Clear();
					if (commRec != null)
					{
						summary.Add("");
						summary.Add(LangMan.LSList[183] + ": \"~ub+1~" + commRec.CommName.Trim() + "~bu-1~\"");
						summary.Add("");
						summary.Add(LangMan.LSList[184] + ": " + GKUtils.GetCorresponderStr(this.FTree, commRec, true));
						summary.Add(LangMan.LSList[113] + ": " + LangMan.LSList[(int)GKData.CommunicationNames[(int)commRec.CommunicationType] - 1]);
						summary.Add(LangMan.LSList[139] + ": " + GKUtils.GEDCOMDateToStr(commRec.Date, TDateFormat.dfDD_MM_YYYY));

						this.RecListNotesRefresh(commRec, null, summary);
						this.RecListMediaRefresh(commRec, null, summary);
					}
				}
				finally
				{
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowCommunicationInfo(): " + E.Message);
			}
		}

		public void ShowLocationInfo(TGEDCOMLocationRecord locRec, StringList summary)
		{
			try
			{
				summary.BeginUpdate();
				StringList link_list = new StringList();
				try
				{
					summary.Clear();
					if (locRec != null)
					{
						summary.Add("");
						summary.Add("~ub+1~" + locRec.LocationName.Trim() + "~bu-1~");
						summary.Add("");
						summary.Add(LangMan.LSList[171] + ": " + locRec.Map.Lati);
						summary.Add(LangMan.LSList[172] + ": " + locRec.Map.Long);

						GKUtils.GetLocationLinks(this.FTree, locRec, ref link_list);

						if (link_list.Count > 0)
						{
							link_list.Sort();

							summary.Add("");
							summary.Add(LangMan.LSList[234] + ":");

							int num = link_list.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								summary.Add("    " + link_list[i]);
							}
						}

						this.RecListNotesRefresh(locRec, null, summary);
						this.RecListMediaRefresh(locRec, null, summary);
					}
				}
				finally
				{
					link_list.Free();
					summary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowLocationInfo(): " + E.Message);
			}
		}

		#endregion

		#region TimeLine

		public void TimeLine_Init()
		{
			((TIndividualListFilter)this.ListPersons.ListMan.Filter).LifeMode = TLifeMode.lmTimeLine;
		}

		public void TimeLine_Done()
		{
			TIndividualListFilter iFilter = ((TIndividualListFilter)this.ListPersons.ListMan.Filter);
			
			iFilter.LifeMode = TLifeMode.lmAll;
			iFilter.TimeLineYear = -1;
			this.ApplyFilter();
		}

		public int TimeLineYear
		{
			get
			{
				return ((TIndividualListFilter)this.ListPersons.ListMan.Filter).TimeLineYear;
			}
			set
			{
				((TIndividualListFilter)this.ListPersons.ListMan.Filter).TimeLineYear = value;
				this.ApplyFilter();
			}
		}

		#endregion

		#region Progress

		void IProgressController.ProgressInit(int aMax, string aTitle)
		{
			TfmProgress.ProgressInit(aMax, aTitle);
		}

		void IProgressController.ProgressDone()
		{
			TfmProgress.ProgressDone();
		}

		void IProgressController.ProgressStep()
		{
			TfmProgress.ProgressStep();
		}

		void IProgressController.ProgressStep(int value)
		{
			TfmProgress.ProgressStep(value);
		}

		#endregion
	}
}
