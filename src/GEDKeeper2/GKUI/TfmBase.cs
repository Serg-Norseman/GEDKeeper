using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Windows.Forms;

using ExtUtils;
using ExtUtils.Controls;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using GKUI.Dialogs;
using GKUI.Lists;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
    public sealed partial class TfmBase : Form, IBase, ILocalization
	{
    	#region Private fields
    	
        /*private readonly ExtList[] fChangedRecords;*/

        private readonly ExtList<TGEDCOMRecord> fLockedRecords;
        private readonly NavigationStack fNavman;
        private readonly ValuesCollection fValuesCollection;

		private IBaseContext fContext;
		private bool fModified;
		private ShieldState fShieldState;
		private TGEDCOMTree fTree;

        private readonly GKRecordsView ListPersons;
        private readonly GKRecordsView ListFamilies;
        private readonly GKRecordsView ListNotes;
        private readonly GKRecordsView ListMultimedia;
        private readonly GKRecordsView ListSources;
        private readonly GKRecordsView ListRepositories;
        private readonly GKRecordsView ListGroups;
        private readonly GKRecordsView ListResearches;
        private readonly GKRecordsView ListTasks;
        private readonly GKRecordsView ListCommunications;
        private readonly GKRecordsView ListLocations;

		private readonly HyperView mPersonSummary;
		private readonly HyperView mFamilySummary;
		private readonly HyperView mNoteSummary;
		private readonly HyperView mMediaSummary;
		private readonly HyperView mSourceSummary;
		private readonly HyperView mRepositorySummary;
		private readonly HyperView mGroupSummary;
		private readonly HyperView mResearchSummary;
		private readonly HyperView mTaskSummary;
		private readonly HyperView mCommunicationSummary;
		private readonly HyperView mLocationSummary;

		#endregion

		#region Public properties

		public IHost Host
		{ 
			get { return TfmGEDKeeper.Instance; }
		}

		public IBaseContext Context
		{
			get { return this.fContext; }
		}
		
		public NavigationStack Navman
		{
			get { return this.fNavman; }
		}

		public ValuesCollection ValuesCollection
		{
			get {
				return this.fValuesCollection;
			}
		}

		public bool Modified
		{
			get {
				return this.fModified;
			}
			set {
				this.fModified = value;
				this.SetMainTitle();
			}
		}

		public ShieldState ShieldState
		{
			get {
				return this.fShieldState;
			}
			set {
				bool up = (this.fShieldState != ShieldState.ssNone && value == ShieldState.ssNone) || (this.fShieldState == ShieldState.ssNone && value != ShieldState.ssNone);
				this.fShieldState = value;
				if (up)
				{
					this.RefreshLists(false);
				}
			}
		}

		public TGEDCOMTree Tree
		{
			get { return this.fTree; }
		}

		#endregion
		
		#region Instance control
		
        public TfmBase()
        {
            this.InitializeComponent();
            base.MdiParent = TfmGEDKeeper.Instance;

            /*this.fChangedRecords = new ExtList[14];
            for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
            {
                this.fChangedRecords[(int)rt] = new ExtList();
            }*/

            this.fTree = new TGEDCOMTree();
            this.fContext = new BaseContext(this.fTree, this);

            this.fLockedRecords = new ExtList<TGEDCOMRecord>();
            this.fNavman = new NavigationStack();
            this.fValuesCollection = new ValuesCollection();

            this.CreatePage(LangMan.LS(LSID.LSID_RPIndividuals), TGEDCOMRecordType.rtIndividual, out this.ListPersons, out this.mPersonSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPFamilies), TGEDCOMRecordType.rtFamily, out this.ListFamilies, out this.mFamilySummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPNotes), TGEDCOMRecordType.rtNote, out this.ListNotes, out this.mNoteSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPMultimedia), TGEDCOMRecordType.rtMultimedia, out this.ListMultimedia, out this.mMediaSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPSources), TGEDCOMRecordType.rtSource, out this.ListSources, out this.mSourceSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPRepositories), TGEDCOMRecordType.rtRepository, out this.ListRepositories, out this.mRepositorySummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPGroups), TGEDCOMRecordType.rtGroup, out this.ListGroups, out this.mGroupSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPResearches), TGEDCOMRecordType.rtResearch, out this.ListResearches, out this.mResearchSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPTasks), TGEDCOMRecordType.rtTask, out this.ListTasks, out this.mTaskSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPCommunications), TGEDCOMRecordType.rtCommunication, out this.ListCommunications, out this.mCommunicationSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPLocations), TGEDCOMRecordType.rtLocation, out this.ListLocations, out this.mLocationSummary);
            this.PageRecords.SelectedIndex = 0;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fNavman.Dispose();
                this.fLockedRecords.Dispose();
                this.fTree.Dispose();
                this.fTree = null;
                
                /*for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
                {
                    this.fChangedRecords[(int)rt].Dispose();
                }*/

                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        #endregion
        
        #region Form handlers

        private void Form_Activated(object sender, EventArgs e)
        {
            TfmGEDKeeper.Instance.UpdateControls(false);
            TfmGEDKeeper.Instance.BaseChanged(this);
        }

        private void Form_Deactivate(object sender, EventArgs e)
        {
            TfmGEDKeeper.Instance.BaseChanged(null);
            TfmGEDKeeper.Instance.UpdateControls(true);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            //
        }

        private void Form_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !this.CheckModified();

            if (!e.Cancel)
            {
            	TfmGEDKeeper.Instance.BaseChanged(null);
                TfmGEDKeeper.Instance.CheckMRUWin(this.fTree.FileName, this);
            }
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
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

        #endregion
        
        #region Basic function
        
		private void ChangeFileName()
		{
			this.SetMainTitle();
			TfmGEDKeeper.Instance.Options.LastDir = Path.GetDirectoryName(this.fTree.FileName);
		}

		public TGEDCOMRecordType GetSelectedRecordType()
		{
			return (TGEDCOMRecordType)(this.PageRecords.SelectedIndex + 1);
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

		public IListManager GetRecordsListManByType(TGEDCOMRecordType recType)
		{
			GKRecordsView rView = this.GetRecordsViewByType(recType);
			if (rView == null) return null;

			return rView.ListMan;
		}

		public TGEDCOMRecord GetSelectedRecordEx()
		{
			TGEDCOMRecordType rt = this.GetSelectedRecordType();
			GKRecordsView rView = this.GetRecordsViewByType(rt);

			if (rView != null) {
				TGEDCOMRecord record = rView.GetSelectedRecord();
				return record;
			}

			return null;
		}

		private void List_SelectedIndexChanged(object sender, EventArgs e)
		{
		    if (sender == null) return;

			TGEDCOMRecord rec = (sender as GKRecordsView).GetSelectedRecord();
			if (rec != null)
			{
				this.NavAdd(rec);
			}
			this.ShowRecordInfo(rec);
		}

		// FIXME
		public ExtList<TGEDCOMRecord> GetContentList(TGEDCOMRecordType recType)
		{
			GKRecordsView rView = this.GetRecordsViewByType(recType);
			if (rView == null) return null;
			
			return rView.ContentList;
		}

		private void SetMainTitle()
		{
			this.Text = Path.GetFileName(this.Tree.FileName);
			if (this.fModified)
			{
				this.Text = "* " + this.Text;
			}
		}

		private void mPersonSummaryLink(object sender, string linkName)
		{
			if (linkName.StartsWith("view_"))
			{
				string xref = linkName.Remove(0, 5);
				TGEDCOMRecord rec = this.fTree.XRefIndex_Find(xref);
				if (rec != null)
				{
					this.ShowMedia(rec as TGEDCOMMultimediaRecord, false);
				}
			}
			else
			{
				this.SelectRecordByXRef(linkName);
			}
		}

        private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
        {
            TfmGEDKeeper.Instance.UpdateControls(false);
        }

        public void ApplyFilter()
		{
			if (this.fTree.RecordsCount > 0)
			{
				this.RefreshLists(false);
			}
		}

		public void ChangeRecord(TGEDCOMRecord record)
		{
			if (record != null)
			{
				/*int rt = (int)record.RecordType;
				this.fChangedRecords[rt].Add(record);*/

				record.ChangeDate.ChangeDateTime = DateTime.Now;

				this.Modified = true;
				this.fTree.Header.TransmissionDateTime = DateTime.Now;

				TfmGEDKeeper.Instance.NotifyRecord(this, record, RecordAction.raEdit);
			}
		}

		public void ChangesClear()
		{
			/*for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
			{
				this.fChangedRecords[(int)rt].Clear();
			}*/
		}

		public bool CheckModified()
		{
			bool result = true;

			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.AppTitle, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);

				switch (dialogResult) {
					case DialogResult.Yes:
						TfmGEDKeeper.Instance.miFileSaveClick(null, null);
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
			this.fTree.Clear();
			this.fNavman.Clear();
		}

		private void CreatePage(string pageText, TGEDCOMRecordType recType, out GKRecordsView recView, out HyperView summary)
		{
			this.PageRecords.SuspendLayout();

			TabPage sheet = new TabPage();
			sheet.Text = pageText;
			this.PageRecords.Controls.Add(sheet);
			this.PageRecords.ResumeLayout(false);

			summary = new HyperView();
			summary.BorderWidth = 4;
			summary.Dock = DockStyle.Right;
			summary.Size = new Size(300, 290);
			summary.OnLink += this.mPersonSummaryLink;

			Splitter spl = new Splitter();
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;

			sheet.Controls.Add(summary);
			sheet.Controls.Add(spl);

			GKUtils.CreateRecordsView(sheet, this.fTree, recType, out recView);
			recView.IsMainList = true;
			recView.DoubleClick += this.RecordEdit;
			recView.SelectedIndexChanged += this.List_SelectedIndexChanged;
			recView.UpdateTitles();

			sheet.Controls.SetChildIndex(spl, 1);
			sheet.Controls.SetChildIndex(summary, 2);
		}

        // FIXME
		private void LoadProgress(object sender, int progress)
		{
			TfmProgress.ProgressStep(progress);
		}

		public void FileNew()
		{
			this.ChangesClear();
			this.Clear();
			this.RefreshLists(false);
            GKUtils.ShowPersonInfo(null, this.mPersonSummary.Lines, this.fShieldState);
            this.fTree.SetFileName(LangMan.LS(LSID.LSID_Unknown));
			this.Modified = false;
		}

		public void FileLoad(string fileName)
		{
			this.ChangesClear();
			this.Clear();

			//try
			//{
			TfmProgress.ProgressInit(LangMan.LS(LSID.LSID_Loading), 100);
			try
			{
				this.fTree.OnProgress += LoadProgress;
				this.fTree.LoadFromFile(fileName);
				this.fTree.OnProgress -= LoadProgress;
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
			//}
			//catch (Exception ex)
			//{
			//	this.Host.LogWrite("TfmBase.FileLoad().TreeLoad(): " + E.Message);
			//	.ShowError(LangMan.LS(LSID.245]);
			//}

			TreeTools.CheckGEDCOMFormat(this.fTree, this.fValuesCollection, this);

			this.ChangeFileName();
			this.Modified = false;

			TfmGEDKeeper.Instance.AddMRU(fileName);

			this.RefreshLists(false);
			this.ShowTips();
		}

		public void FileSave(string fileName)
		{
			try
			{
				if (TfmGEDKeeper.Instance.Options.RevisionsBackup)
				{
					int rev = this.Tree.Header.FileRevision;
					if (File.Exists(fileName)) 
					{
						string bak_path = Path.GetDirectoryName(fileName) + "\\__history\\";
						string bak_file = Path.GetFileName(fileName) + "." + SysUtils.NumUpdate(rev, 3);

						if (!Directory.Exists(bak_path)) Directory.CreateDirectory(bak_path);
						File.Move(fileName, bak_path + bak_file);
					}
				}

				// проверка наличия архива и хранилища, перемещение их, если файл изменил местоположение
				this.MoveMediaContainers(this.Tree.FileName, fileName);

				this.fTree.SaveToFile(fileName, TfmGEDKeeper.Instance.Options.DefCharacterSet);

				this.ChangeFileName();

				TfmGEDKeeper.Instance.AddMRU(fileName);
				this.Modified = false;
			}
			catch (UnauthorizedAccessException)
			{
				GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": доступ закрыт" }));
			}
			catch (Exception ex)
			{
				GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
				this.Host.LogWrite("TfmBase.FileSave(): " + ex.Message);
			}
		}

		private TGEDCOMFamilyRecord GetFamilyBySpouse(TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord result = null;

			int num = this.fTree.RecordsCount;
			for (int i = 0; i < num; i++)
			{
				TGEDCOMRecord rec = this.fTree[i];

				if (rec is TGEDCOMFamilyRecord)
				{
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					TGEDCOMIndividualRecord husb = fam.Husband.Value as TGEDCOMIndividualRecord;
					TGEDCOMIndividualRecord wife = fam.Wife.Value as TGEDCOMIndividualRecord;
					if (husb == aNewParent || wife == aNewParent)
					{
						string msg = string.Format(LangMan.LS(LSID.LSID_ParentsQuery), GKUtils.aux_GetFamilyStr(fam));
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

		public TGEDCOMFamilyRecord GetChildFamily(TGEDCOMIndividualRecord iChild, bool aCanCreate, TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord result = null;

			if (iChild != null)
			{
				if (iChild.ChildToFamilyLinks.Count != 0)
				{
					result = iChild.ChildToFamilyLinks[0].Family;
				}
				else
				{
					if (aCanCreate)
					{
						TGEDCOMFamilyRecord fam = this.GetFamilyBySpouse(aNewParent);
						if (fam == null)
						{
							fam = this.fTree.aux_CreateFamily();
						}
						fam.aux_AddChild(iChild);
						result = fam;
					}
				}
			}

			return result;
		}

		// FIXME
		public TGEDCOMIndividualRecord GetSelectedPerson()
		{
			return this.ListPersons.GetSelectedRecord() as TGEDCOMIndividualRecord;
		}

		public void RefreshLists(bool titles)
		{
			this.ListPersons.UpdateContents(this.fShieldState, titles, 2);
			this.ListFamilies.UpdateContents(this.fShieldState, titles, 1);
			this.ListNotes.UpdateContents(this.fShieldState, titles, -1);
			this.ListMultimedia.UpdateContents(this.fShieldState, titles, 1);
			this.ListSources.UpdateContents(this.fShieldState, titles, 1);
			this.ListRepositories.UpdateContents(this.fShieldState, titles, 1);
			this.ListGroups.UpdateContents(this.fShieldState, titles, 1);
			this.ListResearches.UpdateContents(this.fShieldState, titles, 1);
			this.ListTasks.UpdateContents(this.fShieldState, titles, 1);
			this.ListCommunications.UpdateContents(this.fShieldState, titles, 1);
			this.ListLocations.UpdateContents(this.fShieldState, titles, 1);

			this.PageRecords_SelectedIndexChanged(null, null);
		}

		/*public void RefreshRecordsView(TGEDCOMRecordType recType)
		{
			GKRecordsView rView = this.GetRecordsViewByType(recType);
			if (rView == null) return;

			rView.UpdateContents(this.fShieldState, false, -1);
		}*/
		
		public void RecordNotify(TGEDCOMRecord record, RecordAction notify)
		{
			if (record != null)
			{
				GKRecordsView rView = GetRecordsViewByType(record.RecordType);

				if (rView != null && notify == RecordAction.raDelete)
				{
					rView.DeleteRecord(record);
				}

				TfmGEDKeeper.Instance.NotifyRecord(this, record, RecordAction.raDelete);
			}
		}

		public TGEDCOMFamilyRecord SelectFamily(TGEDCOMIndividualRecord target)
		{
			TGEDCOMFamilyRecord result;

			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.Target = target;
					dlg.NeedSex = TGEDCOMSex.svNone;
					dlg.TargetMode = TargetMode.tmChildToFamily;
					dlg.Mode = TGEDCOMRecordType.rtFamily;
					if (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
						result = (dlg.ResultRecord as TGEDCOMFamilyRecord);
					} else {
						result = null;
					}
				}
				finally
				{
					dlg.Dispose();
				}
			}
			catch (Exception ex)
			{
				this.Host.LogWrite("TfmBase.SelectFamily(): " + ex.Message);
				result = null;
			}

			return result;
		}

		public TGEDCOMIndividualRecord SelectPerson(TGEDCOMIndividualRecord target, TargetMode targetMode, TGEDCOMSex needSex)
		{
			TGEDCOMIndividualRecord result;

			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.Target = target;
					dlg.NeedSex = needSex;
					dlg.TargetMode = targetMode;
					dlg.Mode = TGEDCOMRecordType.rtIndividual;
					if (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
						result = (dlg.ResultRecord as TGEDCOMIndividualRecord);
					} else {
						result = null;
					}
				}
				finally
				{
					dlg.Dispose();
				}
			}
			catch (Exception ex)
			{
				this.Host.LogWrite("TfmBase.SelectPerson(): " + ex.Message);
				result = null;
			}

			return result;
		}

		public TGEDCOMRecord SelectRecord(TGEDCOMRecordType mode, params object[] args)
		{
			TGEDCOMRecord result;

			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.Mode = mode;
					int argsCnt = ((args != null) ? args.Length : 0);
					if (argsCnt > 0) {
						dlg.edFastFilter.Text = (args[0] as string);
					}

					if (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
						result = dlg.ResultRecord;
					} else {
						result = null;
					}
				}
				finally
				{
					dlg.Dispose();
				}
			}
			catch (Exception ex)
			{
				this.Host.LogWrite("TfmBase.SelectRecord(): " + ex.Message);
				result = null;
			}

			return result;
		}

		public void SetFilter()
		{
			TGEDCOMRecordType rt = this.GetSelectedRecordType();
			GKRecordsView rView = this.GetRecordsViewByType(rt);

			switch (rt) {
				case TGEDCOMRecordType.rtIndividual:
					using (TfmPersonsFilter fmFilter = new TfmPersonsFilter(this, rView.ListMan)) {
						DialogResult res = TfmGEDKeeper.Instance.ShowModalEx(fmFilter, false);
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
						DialogResult res = TfmGEDKeeper.Instance.ShowModalEx(fmComFilter, false);
						if (res == DialogResult.OK) this.ApplyFilter();
					}
					break;
			}
		}

		private void NavAdd(TGEDCOMRecord aRec)
		{
			if (aRec != null && !this.fNavman.Busy)
			{
				this.fNavman.Current = aRec;
				TfmGEDKeeper.Instance.UpdateControls(false);
			}
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
				}
			}
			finally
			{
				if (modal) fmMediaView.Dispose();
			}
		}

		public void ShowTips()
		{
			try
			{
				if (TfmGEDKeeper.Instance.Options.ShowTips)
				{
					StringList birthDays = new StringList();
					try
					{
						int num = this.fTree.RecordsCount;
						for (int i = 0; i < num; i++)
						{
							TGEDCOMRecord rec = this.fTree[i];
							if (rec is TGEDCOMIndividualRecord) {
								TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
								string nm = iRec.aux_GetNameStr(true, false);
								string days = GKUtils.GetDaysForBirth(iRec);

								if (days != "" && int.Parse(days) < 3) {
									birthDays.Add(string.Format(LangMan.LS(LSID.LSID_DaysRemained), nm, days));
								}
							}
						}

						if (birthDays.Count > 0) {
							TfmGEDKeeper.Instance.Options.ShowTips = TfmTipsDialog.ShowTipsEx(LangMan.LS(LSID.LSID_BirthDays), TfmGEDKeeper.Instance.Options.ShowTips, birthDays);
						}
					}
					finally
					{
                        birthDays.Dispose();
					}
				}
			}
			catch (Exception ex)
			{
				this.Host.LogWrite("TfmBase.ShowTips(): " + ex.Message);
			}
		}

		#endregion
		
		#region Name and sex functions

		public string DefinePatronymic(string name, TGEDCOMSex sex, bool confirm)
		{
			string result = "";

			NamesTable.NameEntry n = TfmGEDKeeper.Instance.NamesTable.FindName(name);
			if (n == null) {
			    if (!confirm) {
					return result;
				}

                n = TfmGEDKeeper.Instance.NamesTable.AddName(name);
			}

		    switch (sex) {
				case TGEDCOMSex.svMale:
					result = n.M_Patronymic;
					break;

				case TGEDCOMSex.svFemale:
					result = n.F_Patronymic;
					break;
			}
			
			if (result == "") {
			    if (!confirm) {
					return result;
				}

                this.ModifyName(ref n);
			}

		    switch (sex) {
				case TGEDCOMSex.svMale: {
					result = n.M_Patronymic;
					break;
				}
				case TGEDCOMSex.svFemale: {
					result = n.F_Patronymic;
					break;
				}
			}
			
			return result;
		}

		public TGEDCOMSex DefineSex(string iName, string iPatr)
		{
			NamesTable namesTable = TfmGEDKeeper.Instance.NamesTable;
			TGEDCOMSex result = namesTable.GetSexByName(iName);

			if (result == TGEDCOMSex.svNone)
			{
				TfmSexCheck dlg = new TfmSexCheck();
				try
				{
					dlg.IndividualName = iName + " " + iPatr;
					result = NamesTable.GetSex(iName, iPatr, false);

					dlg.Sex = result;
					if (dlg.ShowDialog() == DialogResult.OK)
					{
						result = dlg.Sex;

						if (result != TGEDCOMSex.svNone)
						{
							namesTable.SetNameSex(iName, result);
						}
					}
				}
				finally
				{
					dlg.Dispose();
				}
			}

			return result;
		}

		public void CheckPersonSex(TGEDCOMIndividualRecord iRec)
		{
			if (iRec.Sex == TGEDCOMSex.svNone || iRec.Sex == TGEDCOMSex.svUndetermined)
			{
				string f_fam, f_name, f_patr;
				iRec.aux_GetNameParts(out f_fam, out f_name, out f_patr);
				iRec.Sex = this.DefineSex(f_name, f_patr);
			}
		}

		#endregion
		
		#region ILocalization implementation
		
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
        
        #endregion

		#region IProgressController implementation
		
		void IProgressController.ProgressInit(string title, int max)
		{
			TfmProgress.ProgressInit(title, max);
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

		#region IWorkWindow implementation
		
		string IWorkWindow.GetStatusString()
		{
			string res = "";

			TGEDCOMRecordType rt = this.GetSelectedRecordType();
			GKRecordsView rView = this.GetRecordsViewByType(rt);

			if (rView != null)
			{
				res = LangMan.LS(LSID.LSID_SBRecords) + ": " + rView.TotalCount.ToString();
				res = res + ", " + LangMan.LS(LSID.LSID_SBFiltered) + ": " + rView.FilteredCount.ToString();
			}

			return res;
		}

		void IWorkWindow.NavNext()
		{
			this.fNavman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.fNavman.Next() as TGEDCOMRecord;
                if (rec != null)
                {
                    this.SelectRecordByXRef(rec.XRef);
                    TfmGEDKeeper.Instance.UpdateControls(false);
                }
			}
			finally
			{
				this.fNavman.EndNav();
			}
		}

		void IWorkWindow.NavPrev()
		{
			this.fNavman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.fNavman.Back() as TGEDCOMRecord;
                if (rec != null)
                {
                    this.SelectRecordByXRef(rec.XRef);
                    TfmGEDKeeper.Instance.UpdateControls(false);
                }
			}
			finally
			{
				this.fNavman.EndNav();
			}
		}

		bool IWorkWindow.NavCanBackward()
		{
			return this.fNavman.CanBackward();
		}

		bool IWorkWindow.NavCanForward()
		{
			return this.fNavman.CanForward();
		}
		
		#endregion
		
		#region Record Management

		public void RecordAdd()
		{
			TGEDCOMRecord rec = null;
			bool result = false;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					rec = this.CreatePersonDialog(null, TargetMode.tmParent, TGEDCOMSex.svNone);
					result = (rec != null);
					break;
				}
				case 1:
				{
					TGEDCOMFamilyRecord fam = null;
					result = this.ModifyFamily(ref fam, FamilyTarget.ftNone, null);
					rec = fam;
					break;
				}
				case 2:
				{
					TGEDCOMNoteRecord note = null;
                    result = this.ModifyNote(ref note);
					rec = note;
					break;
				}
				case 3:
				{
					TGEDCOMMultimediaRecord mm_rec = null;
                    result = this.ModifyMedia(ref mm_rec);
					rec = mm_rec;
					break;
				}
				case 4:
				{
					TGEDCOMSourceRecord src = null;
                    result = this.ModifySource(ref src);
					rec = src;
					break;
				}
				case 5:
				{
					TGEDCOMRepositoryRecord rep = null;
                    result = this.ModifyRepository(ref rep);
					rec = rep;
					break;
				}
				case 6:
				{
					TGEDCOMGroupRecord grp = null;
                    result = this.ModifyGroup(ref grp);
					rec = grp;
					break;
				}
				case 7:
				{
					TGEDCOMResearchRecord rsr = null;
                    result = this.ModifyResearch(ref rsr);
					rec = rsr;
					break;
				}
				case 8:
				{
					TGEDCOMTaskRecord tsk = null;
                    result = this.ModifyTask(ref tsk);
					rec = tsk;
					break;
				}
				case 9:
				{
					TGEDCOMCommunicationRecord comm = null;
                    result = this.ModifyCommunication(ref comm);
					rec = comm;
					break;
				}
				case 10:
				{
					TGEDCOMLocationRecord loc = null;
                    result = this.ModifyLocation(ref loc);
					rec = loc;
					break;
				}
			}

            if (result) {
				this.RefreshLists(false);
				this.SelectRecordByXRef(rec.XRef);
			}
		}

		public bool RecordDelete(TGEDCOMRecord record, bool confirm)
		{
			bool result = false;

			if (record != null)
			{
				string xref = record.XRef;
				string msg = "";

				switch (record.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
						msg = string.Format(LangMan.LS(LSID.LSID_PersonDeleteQuery), (record as TGEDCOMIndividualRecord).aux_GetNameStr(true, false));
						break;

					case TGEDCOMRecordType.rtFamily:
						msg = string.Format(LangMan.LS(LSID.LSID_FamilyDeleteQuery), GKUtils.aux_GetFamilyStr(record as TGEDCOMFamilyRecord));
						break;

					case TGEDCOMRecordType.rtNote:
						msg = LangMan.LS(LSID.LSID_NoteDeleteQuery);
						break;

					case TGEDCOMRecordType.rtMultimedia:
						msg = string.Format(LangMan.LS(LSID.LSID_MediaDeleteQuery), (record as TGEDCOMMultimediaRecord).aux_GetTitle());
						break;

					case TGEDCOMRecordType.rtSource:
						msg = string.Format(LangMan.LS(LSID.LSID_SourceDeleteQuery), (record as TGEDCOMSourceRecord).FiledByEntry);
						break;

					case TGEDCOMRecordType.rtRepository:
						msg = string.Format(LangMan.LS(LSID.LSID_RepositoryDeleteQuery), (record as TGEDCOMRepositoryRecord).RepositoryName);
						break;

					case TGEDCOMRecordType.rtGroup:
						msg = string.Format(LangMan.LS(LSID.LSID_GroupDeleteQuery), (record as TGEDCOMGroupRecord).GroupName);
						break;

					case TGEDCOMRecordType.rtResearch:
						msg = string.Format(LangMan.LS(LSID.LSID_ResearchDeleteQuery), (record as TGEDCOMResearchRecord).ResearchName);
						break;

					case TGEDCOMRecordType.rtTask:
						msg = string.Format(LangMan.LS(LSID.LSID_TaskDeleteQuery), GKUtils.GetTaskGoalStr(record as TGEDCOMTaskRecord));
						break;

					case TGEDCOMRecordType.rtCommunication:
						msg = string.Format(LangMan.LS(LSID.LSID_CommunicationDeleteQuery), (record as TGEDCOMCommunicationRecord).CommName);
						break;

					case TGEDCOMRecordType.rtLocation:
						msg = string.Format(LangMan.LS(LSID.LSID_LocationDeleteQuery), (record as TGEDCOMLocationRecord).LocationName);
						break;
				}

				if (confirm && GKUtils.ShowQuestion(msg) != DialogResult.Yes) return false;
				this.RecordNotify(record, RecordAction.raDelete);

				switch (record.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
						result = this.fTree.aux_DeleteIndividualRecord(record as TGEDCOMIndividualRecord);
						break;

					case TGEDCOMRecordType.rtFamily:
						result = this.fTree.aux_DeleteFamilyRecord(record as TGEDCOMFamilyRecord);
						break;

					case TGEDCOMRecordType.rtNote:
						result = this.fTree.aux_DeleteNoteRecord(record as TGEDCOMNoteRecord);
						break;

					case TGEDCOMRecordType.rtMultimedia:
						result = this.fTree.aux_DeleteMediaRecord(record as TGEDCOMMultimediaRecord);
						break;

					case TGEDCOMRecordType.rtSource:
						result = this.fTree.aux_DeleteSourceRecord(record as TGEDCOMSourceRecord);
						break;

					case TGEDCOMRecordType.rtRepository:
						result = this.fTree.aux_DeleteRepositoryRecord(record as TGEDCOMRepositoryRecord);
						break;

					case TGEDCOMRecordType.rtGroup:
						result = this.fTree.aux_DeleteGroupRecord(record as TGEDCOMGroupRecord);
						break;

					case TGEDCOMRecordType.rtResearch:
						result = this.fTree.aux_DeleteResearchRecord(record as TGEDCOMResearchRecord);
						break;

					case TGEDCOMRecordType.rtTask:
						result = this.fTree.aux_DeleteTaskRecord(record as TGEDCOMTaskRecord);
						break;

					case TGEDCOMRecordType.rtCommunication:
						result = this.fTree.aux_DeleteCommunicationRecord(record as TGEDCOMCommunicationRecord);
						break;

					case TGEDCOMRecordType.rtLocation:
						result = this.fTree.aux_DeleteLocationRecord(record as TGEDCOMLocationRecord);
						break;
				}

				if (result) {
					this.Modified = true;
					this.fTree.Header.TransmissionDateTime = DateTime.Now;
				}
			}

			return result;
		}

		public void RecordDelete()
		{
			TGEDCOMRecord record = this.GetSelectedRecordEx();
			if (record == null) return;

			bool result = this.RecordDelete(record, true);

			if (result) {
				this.RefreshLists(false);
			}
		}

		public void RecordEdit(object sender, EventArgs e)
		{
			TGEDCOMRecord rec = this.GetSelectedRecordEx();
			if (rec == null) return;

			bool result = false;

			switch (rec.RecordType) {
				case TGEDCOMRecordType.rtIndividual:
					TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
					result = this.ModifyPerson(ref ind);
					break;

				case TGEDCOMRecordType.rtFamily:
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
                    result = this.ModifyFamily(ref fam, FamilyTarget.ftNone, null);
					break;

				case TGEDCOMRecordType.rtNote:
					TGEDCOMNoteRecord note = rec as TGEDCOMNoteRecord;
                    result = this.ModifyNote(ref note);
					break;

				case TGEDCOMRecordType.rtMultimedia:
					TGEDCOMMultimediaRecord mm_rec = rec as TGEDCOMMultimediaRecord;
                    result = this.ModifyMedia(ref mm_rec);
					break;

				case TGEDCOMRecordType.rtSource:
					TGEDCOMSourceRecord src = rec as TGEDCOMSourceRecord;
                    result = this.ModifySource(ref src);
					break;

				case TGEDCOMRecordType.rtRepository:
					TGEDCOMRepositoryRecord rep = rec as TGEDCOMRepositoryRecord;
                    result = this.ModifyRepository(ref rep);
					break;

				case TGEDCOMRecordType.rtGroup:
					TGEDCOMGroupRecord grp = rec as TGEDCOMGroupRecord;
                    result = this.ModifyGroup(ref grp);
					break;

				case TGEDCOMRecordType.rtResearch:
					TGEDCOMResearchRecord rsr = rec as TGEDCOMResearchRecord;
                    result = this.ModifyResearch(ref rsr);
					break;

				case TGEDCOMRecordType.rtTask:
					TGEDCOMTaskRecord tsk = rec as TGEDCOMTaskRecord;
                    result = this.ModifyTask(ref tsk);
					break;

				case TGEDCOMRecordType.rtCommunication:
					TGEDCOMCommunicationRecord comm = rec as TGEDCOMCommunicationRecord;
                    result = this.ModifyCommunication(ref comm);
					break;

				case TGEDCOMRecordType.rtLocation:
					TGEDCOMLocationRecord loc = rec as TGEDCOMLocationRecord;
                    result = this.ModifyLocation(ref loc);
					break;
			}

            if (result)
			{
				this.RefreshLists(false);
				this.ShowRecordInfo(rec);
			}
		}

		public void SelectRecordByXRef(string XRef)
		{
			TGEDCOMRecord record = this.fTree.XRefIndex_Find(XRef);
			if (record == null) return;

			GKRecordsView rView = this.GetRecordsViewByType(record.RecordType);
			if (rView == null) return;

			this.PageRecords.SelectedIndex = (int)record.RecordType - 1;
			this.PageRecords_SelectedIndexChanged(null, null);
			this.ActiveControl = rView;
		    //aList.Focus();
			rView.SelectItemByRec(record);
		}

		public void ShowRecordInfo(TGEDCOMRecord record)
		{
			if (record == null) return;

			try
			{
				switch (record.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
						GKUtils.ShowPersonInfo(record as TGEDCOMIndividualRecord, this.mPersonSummary.Lines, this.fShieldState);
						break;

					case TGEDCOMRecordType.rtFamily:
						GKUtils.ShowFamilyInfo(record as TGEDCOMFamilyRecord, this.mFamilySummary.Lines, this.fShieldState);
						break;

					case TGEDCOMRecordType.rtNote:
						GKUtils.ShowNoteInfo(record as TGEDCOMNoteRecord, this.mNoteSummary.Lines);
						break;

					case TGEDCOMRecordType.rtMultimedia:
						GKUtils.ShowMultimediaInfo(record as TGEDCOMMultimediaRecord, this.mMediaSummary.Lines);
						break;

					case TGEDCOMRecordType.rtSource:
						GKUtils.ShowSourceInfo(record as TGEDCOMSourceRecord, this.mSourceSummary.Lines);
						break;

					case TGEDCOMRecordType.rtRepository:
						GKUtils.ShowRepositoryInfo(record as TGEDCOMRepositoryRecord, this.mRepositorySummary.Lines);
						break;

					case TGEDCOMRecordType.rtGroup:
						GKUtils.ShowGroupInfo(record as TGEDCOMGroupRecord, this.mGroupSummary.Lines);
						break;

					case TGEDCOMRecordType.rtResearch:
						GKUtils.ShowResearchInfo(record as TGEDCOMResearchRecord, this.mResearchSummary.Lines);
						break;

					case TGEDCOMRecordType.rtTask:
						GKUtils.ShowTaskInfo(record as TGEDCOMTaskRecord, this.mTaskSummary.Lines);
						break;

					case TGEDCOMRecordType.rtCommunication:
						GKUtils.ShowCommunicationInfo(record as TGEDCOMCommunicationRecord, this.mCommunicationSummary.Lines);
						break;

					case TGEDCOMRecordType.rtLocation:
						GKUtils.ShowLocationInfo(record as TGEDCOMLocationRecord, this.mLocationSummary.Lines);
						break;
				}
			}
			catch (Exception ex)
			{
				this.Host.LogWrite("TfmBase.ShowRecordInfo(): " + ex.Message);
			}
		}

		public StringList GetRecordContent(TGEDCOMRecord record)
		{
			StringList ctx = new StringList();

            if (record != null)
			{
				try
				{
						switch (record.RecordType)
						{
							case TGEDCOMRecordType.rtIndividual:
                                GKUtils.ShowPersonInfo(record as TGEDCOMIndividualRecord, ctx, this.fShieldState);
								break;

							case TGEDCOMRecordType.rtFamily:
                                GKUtils.ShowFamilyInfo(record as TGEDCOMFamilyRecord, ctx, this.fShieldState);
								break;

							case TGEDCOMRecordType.rtNote:
                                GKUtils.ShowNoteInfo(record as TGEDCOMNoteRecord, ctx);
								break;

							case TGEDCOMRecordType.rtMultimedia:
                                GKUtils.ShowMultimediaInfo(record as TGEDCOMMultimediaRecord, ctx);
								break;

							case TGEDCOMRecordType.rtSource:
                                GKUtils.ShowSourceInfo(record as TGEDCOMSourceRecord, ctx);
								break;

							case TGEDCOMRecordType.rtRepository:
                                GKUtils.ShowRepositoryInfo(record as TGEDCOMRepositoryRecord, ctx);
								break;

							case TGEDCOMRecordType.rtGroup:
                                GKUtils.ShowGroupInfo(record as TGEDCOMGroupRecord, ctx);
								break;

							case TGEDCOMRecordType.rtResearch:
                                GKUtils.ShowResearchInfo(record as TGEDCOMResearchRecord, ctx);
								break;

							case TGEDCOMRecordType.rtTask:
                                GKUtils.ShowTaskInfo(record as TGEDCOMTaskRecord, ctx);
								break;

							case TGEDCOMRecordType.rtCommunication:
                                GKUtils.ShowCommunicationInfo(record as TGEDCOMCommunicationRecord, ctx);
								break;

							case TGEDCOMRecordType.rtLocation:
								GKUtils.ShowLocationInfo(record as TGEDCOMLocationRecord, ctx);
								break;
						}
				}
				catch (Exception ex)
				{
					this.Host.LogWrite("TfmBase.GetRecordContext(): " + ex.Message);
				}
			}

            return ctx;
		}

		public bool RecordIsFiltered(TGEDCOMRecord record)
		{
			bool result = false;
			if (record != null)
			{
				GKRecordsView rView = this.GetRecordsViewByType(record.RecordType);
				result = (rView.ContentList.IndexOf(record) >= 0);
			}
			return result;
		}

		#endregion

		#region Modify routines

		public bool ModifyMedia(ref TGEDCOMMultimediaRecord mediaRec)
		{
			bool result = false;

			TfmMediaEdit dlg = new TfmMediaEdit(this);
			try {
				bool exists = mediaRec != null;
				if (!exists) {
					mediaRec = new TGEDCOMMultimediaRecord(this.fTree, this.fTree, "", "");
					mediaRec.FileReferences.Add(new TGEDCOMFileReferenceWithTitle(this.fTree, mediaRec, "", ""));
					mediaRec.InitNew();
				}

				dlg.MediaRec = mediaRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(mediaRec);
					}
					result = true;
				} else {
					if (!exists) {
						mediaRec.Dispose();
						mediaRec = null;
					}
				}
			} finally {
				dlg.Dispose();
			}

			return result;
		}

		public bool ModifyNote(ref TGEDCOMNoteRecord noteRec)
		{
			bool result = false;

			TfmNoteEdit dlg = new TfmNoteEdit(this);
			try {
				bool exists = noteRec != null;
				if (!exists) {
					noteRec = new TGEDCOMNoteRecord(this.fTree, this.fTree, "", "");
					noteRec.InitNew();
				}

				dlg.NoteRecord = noteRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(noteRec);
					}
					result = true;
				} else {
					if (!exists) {
						noteRec.Dispose();
						noteRec = null;
					}
				}
			} finally {
				dlg.Dispose();
			}

			return result;
		}

		public bool ModifySource(ref TGEDCOMSourceRecord sourceRec)
		{
			bool result = false;

			TfmSourceEdit fmSrcEdit = new TfmSourceEdit(this);
			try {
				bool exists = sourceRec != null;
				if (!exists) {
					sourceRec = new TGEDCOMSourceRecord(this.fTree, this.fTree, "", "");
					sourceRec.InitNew();
				}

				fmSrcEdit.SourceRecord = sourceRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmSrcEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(sourceRec);
					}
					result = true;
				} else {
					if (!exists) {
						sourceRec.Dispose();
						sourceRec = null;
					}
				}
			} finally {
				fmSrcEdit.Dispose();
			}

			return result;
		}

		public bool ModifyRepository(ref TGEDCOMRepositoryRecord repRec)
		{
			bool result = false;

			TfmRepositoryEdit fmRepEdit = new TfmRepositoryEdit(this);
			try {
				bool exists = repRec != null;
				if (!exists) {
					repRec = new TGEDCOMRepositoryRecord(this.fTree, this.fTree, "", "");
					repRec.InitNew();
				}
				fmRepEdit.Repository = repRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmRepEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(repRec);
					}
					result = true;
				} else {
					if (!exists) {
						repRec.Dispose();
						repRec = null;
					}
				}
			} finally {
				fmRepEdit.Dispose();
			}

			return result;
		}

		public bool ModifyGroup(ref TGEDCOMGroupRecord groupRec)
		{
			bool result = false;

			TfmGroupEdit fmGrpEdit = new TfmGroupEdit(this);
			try {
				bool exists = groupRec != null;
				if (!exists) {
					groupRec = new TGEDCOMGroupRecord(this.fTree, this.fTree, "", "");
					groupRec.InitNew();
				}
				fmGrpEdit.Group = groupRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmGrpEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(groupRec);
					}
					result = true;
				} else {
					if (!exists) {
						groupRec.Dispose();
						groupRec = null;
					}
				}
			} finally {
				fmGrpEdit.Dispose();
			}

			return result;
		}

		public bool ModifyResearch(ref TGEDCOMResearchRecord researchRec)
		{
			bool result = false;

			TfmResearchEdit fmResEdit = new TfmResearchEdit(this);
			try {
				bool exists = researchRec != null;
				if (!exists) {
					researchRec = new TGEDCOMResearchRecord(this.fTree, this.fTree, "", "");
					researchRec.InitNew();
				}
				fmResEdit.Research = researchRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmResEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(researchRec);
					}
					result = true;
				} else {
					if (!exists) {
						researchRec.Dispose();
						researchRec = null;
					}
				}
			} finally {
				fmResEdit.Dispose();
			}

			return result;
		}

		public bool ModifyTask(ref TGEDCOMTaskRecord taskRec)
		{
			bool result = false;

			TfmTaskEdit fmTaskEdit = new TfmTaskEdit(this);
			try {
				bool exists = taskRec != null;
				if (!exists) {
					taskRec = new TGEDCOMTaskRecord(this.fTree, this.fTree, "", "");
					taskRec.InitNew();
				}
				fmTaskEdit.Task = taskRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmTaskEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(taskRec);
					}
					result = true;
				} else {
					if (!exists) {
						taskRec.Dispose();
						taskRec = null;
					}
				}
			} finally {
				fmTaskEdit.Dispose();
			}

			return result;
		}

		public bool ModifyCommunication(ref TGEDCOMCommunicationRecord commRec)
		{
			bool result = false;

			TfmCommunicationEdit fmCorrEdit = new TfmCommunicationEdit(this);
			try {
				bool exists = commRec != null;
				if (!exists) {
					commRec = new TGEDCOMCommunicationRecord(this.fTree, this.fTree, "", "");
					commRec.InitNew();
				}
				fmCorrEdit.Communication = commRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmCorrEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(commRec);
					}
					result = true;
				} else {
					if (!exists) {
						commRec.Dispose();
						commRec = null;
					}
				}
			} finally {
				fmCorrEdit.Dispose();
			}

			return result;
		}

		public bool ModifyLocation(ref TGEDCOMLocationRecord locRec)
		{
			bool result = false;

			TfmLocationEdit fmLocEdit = new TfmLocationEdit(this);
			try {
				bool exists = locRec != null;
				if (!exists) {
					locRec = new TGEDCOMLocationRecord(this.fTree, this.fTree, "", "");
					locRec.InitNew();
				}
				fmLocEdit.LocationRecord = locRec;
				if (TfmGEDKeeper.Instance.ShowModalEx(fmLocEdit, false) == DialogResult.OK) {
					if (!exists) {
						this.fTree.AddRecord(locRec);
					}
					result = true;
				} else {
					if (!exists) {
						locRec.Dispose();
						locRec = null;
					}
				}
			} finally {
				fmLocEdit.Dispose();
			}

			return result;
		}

		public bool ModifyName(ref NamesTable.NameEntry aName)
		{
			bool result;

			using (TfmNameEdit dlg = new TfmNameEdit(this)) {
				dlg.IName = aName;
				result = (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
			}

			return result;
		}

		public TGEDCOMIndividualRecord CreatePersonDialog(TGEDCOMIndividualRecord target, TargetMode targetMode, TGEDCOMSex needSex)
		{
			TGEDCOMIndividualRecord result = null;

			using (TfmPersonNew dlg = new TfmPersonNew(this))
			{
				dlg.EditSex.SelectedIndex = (int)needSex;
				dlg.TargetMode = targetMode;
				dlg.Target = target;

				if (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK)
				{
					result = this.fContext.CreatePersonEx(dlg.edName.Text, dlg.edPatronymic.Text, dlg.edFamily.Text, (TGEDCOMSex)dlg.EditSex.SelectedIndex, true);
					this.ChangeRecord(result);
					
					TfmGEDKeeper.Instance.NamesTable.ImportNames(result);
					
					TIndividualListFilter iFilter = (TIndividualListFilter)this.ListPersons.ListMan.Filter;

					if (iFilter.SourceMode == TGroupMode.gmSelected)
                    {
						TGEDCOMSourceRecord src = this.fTree.XRefIndex_Find(iFilter.SourceRef) as TGEDCOMSourceRecord;
                        if (src != null && GKUtils.ShowQuestion("Установлен фильтр по источнику. Внести источник в новую персональную запись?") == DialogResult.Yes)
                        {
							result.aux_AddSource(src, "", 0);
						}
 					}

					if (iFilter.GroupMode == TGroupMode.gmSelected)
                    {
						TGEDCOMGroupRecord grp = this.fTree.XRefIndex_Find(iFilter.GroupRef) as TGEDCOMGroupRecord;
                        if (grp != null && GKUtils.ShowQuestion("Установлен фильтр по группе. Внести группу в новую персональную запись?") == DialogResult.Yes)
                        {
							grp.aux_AddMember(result);
						}
 					}
				}
			}

			return result;
		}

		public bool ModifyPerson(ref TGEDCOMIndividualRecord indivRec)
		{
			bool result = false;

			if (indivRec != null) {
				using (TfmPersonEdit dlg = new TfmPersonEdit(this)) {
					dlg.Person = indivRec;
					result = (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
				}
			}

			return result;
		}

		public bool ModifyFamily(ref TGEDCOMFamilyRecord familyRec, FamilyTarget target, TGEDCOMIndividualRecord person)
		{
			bool result = false;

			if (target == FamilyTarget.ftSpouse && person != null) {
				TGEDCOMSex sex = person.Sex;
				if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined) {
					GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
					return false;
				}
			}

			using (TfmFamilyEdit dlg = new TfmFamilyEdit(this)) {
				bool exists = (familyRec != null);
				if (!exists) {
					familyRec = new TGEDCOMFamilyRecord(this.fTree, this.fTree, "", "");
					familyRec.InitNew();
				}

				if (target == FamilyTarget.ftSpouse) {
					if (person != null)
						familyRec.aux_AddSpouse(person);
				} else if (target == FamilyTarget.ftChild) {
					if (person != null)
						familyRec.aux_AddChild(person);
				}

				dlg.Family = familyRec;
				result = (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK);

				if (result) {
					if (!exists) {
						this.fTree.AddRecord(familyRec);
					}
				} else {
					if (!exists) {
						this.fTree.aux_CleanFamily(familyRec);
						familyRec.Dispose();
						familyRec = null;
					}
				}
			}

			return result;
		}

		public bool ModifyAddress(TGEDCOMAddress address)
		{
			bool result;

			using (TfmAddressEdit dlg = new TfmAddressEdit(this)) {
				dlg.Address = address;
				result = (TfmGEDKeeper.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
			}

			return result;
		}

		#endregion

		#region Private media support

		private string GetArcFileName()
		{
			string treeName = this.fTree.FileName;
			string result = Path.GetDirectoryName(treeName) + "\\" + Path.GetFileNameWithoutExtension(treeName) + ".zip";
			return result;
		}

		private string GetStgFolder(bool create)
		{
			string treeName = this.fTree.FileName;
			string result = Path.GetDirectoryName(treeName) + "\\" + Path.GetFileNameWithoutExtension(treeName) + "\\";
			if (!Directory.Exists(result) && create) Directory.CreateDirectory(result);
			return result;
		}

		private void ArcFileLoad(string target_fn, Stream toStream)
		{
			// http://www.icsharpcode.net/OpenSource/SharpZipLib/ - slow, but high compression ratio
			// http://dotnetzip.codeplex.com/ - fast, but low compression ratio

			target_fn = target_fn.Replace('\\', '/');

			using (ZipStorer zip = ZipStorer.Open(this.GetArcFileName(), FileAccess.Read))
			{
				List<ZipStorer.ZipFileEntry> dir = zip.ReadCentralDir();
				foreach (ZipStorer.ZipFileEntry entry in dir)
				{
					if (entry.FilenameInZip.Equals(target_fn)) {
						zip.ExtractFile(entry, toStream);
						break;
					}
				}
			}
		}

		private void ArcFileSave(string fileName, string sfn)
		{
			string arc_fn = this.GetArcFileName();
			ZipStorer zip = null;

			try
			{
				if (File.Exists(arc_fn)) {
					zip = ZipStorer.Open(arc_fn, FileAccess.ReadWrite);
				} else {
					zip = ZipStorer.Create(arc_fn, "");
				}
				zip.AddFile(ZipStorer.Compression.Deflate, fileName, sfn, null);
			}
			finally
			{
				if (zip != null) zip.Dispose();
			}
		}

		private void MoveMediaContainers(string oldFileName, string newFileName)
		{
			// ничего не делать, если имя файла не изменилось
			if (string.Equals(oldFileName, newFileName)) return;

			bool has_arc = File.Exists(this.GetArcFileName());
			bool has_stg = Directory.Exists(this.GetStgFolder(false));

			string new_path = Path.GetDirectoryName(newFileName);
			string new_name = Path.GetFileName(newFileName);

			// переместить архив и хранилище
			if (has_arc) {
				string new_arc = new_path + "\\" + GKUtils.GetContainerName(new_name, true);
				File.Move(this.GetArcFileName(), new_arc);
			}

			if (has_stg) {
				string new_stg = new_path + "\\" + GKUtils.GetContainerName(new_name, false);
				Directory.Move(this.GetStgFolder(false), new_stg);
			}
		}

		#endregion

		#region Public media support
		
		public bool CheckBasePath()
		{
			string path = Path.GetDirectoryName(this.fTree.FileName);

			bool result = (!string.IsNullOrEmpty(path));
			if (!result)
			{
				GKUtils.ShowError("Для типов хранения \"архив\" и \"хранилище\" новый файл БД нужно предварительно сохранить");
			}
			return result;
		}

		public MediaStoreType GetStoreType(TGEDCOMFileReference fileReference, ref string fileName)
		{
			string fileRef = fileReference.StringValue;
			
			fileName = fileRef;
			MediaStoreType result;

			if (fileRef.IndexOf(GKData.GKStoreTypes[2].Sign) == 0)
			{
				result = MediaStoreType.mstArchive;
				fileName = fileName.Remove(0, 4);
			}
			else
			{
				if (fileRef.IndexOf(GKData.GKStoreTypes[1].Sign) == 0)
				{
					result = MediaStoreType.mstStorage;
					fileName = fileName.Remove(0, 4);
				}
				else
				{
					result = MediaStoreType.mstReference;
				}
			}

			return result;
		}

		public void MediaLoad(TGEDCOMFileReference fileReference, out Stream aStream, bool throwException)
		{
			aStream = null;
			if (fileReference == null) return;
			
			string target_fn = "";
			MediaStoreType gst = this.GetStoreType(fileReference, ref target_fn);

			switch (gst) {
				case MediaStoreType.mstStorage:
					target_fn = this.GetStgFolder(false) + target_fn;
					if (!File.Exists(target_fn)) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
						}
					} else {
						aStream = new FileStream(target_fn, FileMode.Open);
					}
					break;

				case MediaStoreType.mstArchive:
					aStream = new MemoryStream();
					if (!File.Exists(this.GetArcFileName())) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
						}
					} else {
						this.ArcFileLoad(target_fn, aStream);
						aStream.Seek((long)0, SeekOrigin.Begin);
					}
					break;

				case MediaStoreType.mstReference:
					aStream = new FileStream(target_fn, FileMode.Open);
					break;
			}
		}

		public void MediaLoad(TGEDCOMFileReference fileReference, ref string fileName)
		{
			if (fileReference == null) return;
			
			try
			{
				string target_fn = "";
				MediaStoreType gst = this.GetStoreType(fileReference, ref target_fn);

				switch (gst) {
					case MediaStoreType.mstStorage:
						fileName = this.GetStgFolder(false) + target_fn;
						break;

					case MediaStoreType.mstArchive:
						fileName = GKUtils.GetTempDir() + "\\" + Path.GetFileName(target_fn);
						FileStream fs = new FileStream(fileName, FileMode.Create);
						try
						{
							if (!File.Exists(this.GetArcFileName())) {
								GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
							} else {
								target_fn = target_fn.Replace("\\", "/");
								this.ArcFileLoad(target_fn, fs);
							}
						}
						finally
						{
							fs.Close();
							fs.Dispose();
						}
						break;

					case MediaStoreType.mstReference:
						fileName = target_fn;
						break;
				}
			}
			catch (Exception ex)
			{
                this.Host.LogWrite("TfmBase.MediaLoad_fn(): " + ex.Message);
				fileName = "";
			}
		}

		public bool MediaSave(TGEDCOMFileReference fileReference, string fileName, MediaStoreType storeType)
		{
			if (fileReference == null) return false;

			bool result = true;

			string storeFile = Path.GetFileName(fileName);
			string storePath = "";
			string refPath = "";

			switch (TGEDCOMFileReference.RecognizeFormat(fileName))
			{
				case TGEDCOMMultimediaFormat.mfNone:
				case TGEDCOMMultimediaFormat.mfOLE:
				case TGEDCOMMultimediaFormat.mfUnknown:
					storePath = "unknown\\";
					break;

				case TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMMultimediaFormat.mfPNG:
					storePath = "images\\";
					break;

				case TGEDCOMMultimediaFormat.mfWAV:
					storePath = "audio\\";
					break;

				case TGEDCOMMultimediaFormat.mfTXT:
				case TGEDCOMMultimediaFormat.mfRTF:
				case TGEDCOMMultimediaFormat.mfHTM:
					storePath = "texts\\";
					break;

				case TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMMultimediaFormat.mfMPG:
					storePath = "video\\";
					break;
			}

			switch (storeType) {
				case MediaStoreType.mstReference:
					refPath = fileName;
					break;

				case MediaStoreType.mstArchive:
					refPath = GKData.GKStoreTypes[(int)storeType].Sign + storePath + storeFile;
					this.ArcFileSave(fileName, storeFile);
					break;

				case MediaStoreType.mstStorage:
					refPath = GKData.GKStoreTypes[(int)storeType].Sign + storePath + storeFile;
					try
					{
						string target_dir = this.GetStgFolder(true) + storePath;
						if (!Directory.Exists(target_dir)) Directory.CreateDirectory(target_dir);

						string target_fn = target_dir + storeFile;
						File.Copy(fileName, target_fn, false);
					}
					catch (IOException)
					{
						GKUtils.ShowError("Файл с таким именем уже есть в хранилище");
						result = false;
					}
					break;
			}

			if (result) {
				fileReference.LinkFile(refPath);
			}

			return result;
		}

		public Bitmap BitmapLoad(TGEDCOMFileReference fileReference, int thumbWidth, int thumbHeight, bool throwException)
		{
			if (fileReference == null) return null;
			
			Bitmap result = null;
			try
			{
				Stream stm;

				this.MediaLoad(fileReference, out stm, throwException);

				if (stm != null)
				{
					if (stm.Length != 0) {
						using (Bitmap bmp = new Bitmap(stm))
						{
							int new_width;
							int new_height;

							if (thumbWidth > 0 && thumbHeight > 0)
							{
								int maxSize_src = ((bmp.Height > bmp.Width) ? bmp.Height : bmp.Width);
								int minSize_dst = ((thumbHeight < thumbWidth) ? thumbHeight : thumbWidth);
								double ratio = (double)minSize_dst / maxSize_src;
								new_width = (int)(bmp.Width * ratio);
								new_height = (int)(bmp.Height * ratio);
							} else {
								new_width = bmp.Width;
								new_height = bmp.Height;
							}

							Bitmap new_image = new Bitmap(new_width, new_height, PixelFormat.Format24bppRgb);
							Graphics graphic = Graphics.FromImage(new_image);
							graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
							graphic.SmoothingMode = SmoothingMode.HighQuality;
							graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
							graphic.CompositingQuality = CompositingQuality.HighQuality;
							graphic.DrawImage(bmp, 0, 0, new_width, new_height);

							result = new_image;
						}
					}
					stm.Dispose();
				}
			}
			catch (MediaFileNotFoundException)
			{
				throw;
			}
			catch (Exception ex)
			{
                this.Host.LogWrite("TfmBase.BitmapLoad(): " + ex.Message);
				result = null;
			}
			return result;
		}

		public Bitmap GetPrimaryBitmap(TGEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException)
		{
		    if (iRec == null) return null;

			Bitmap result = null;
			try
			{
				TGEDCOMMultimediaLink mmLink = iRec.aux_GetPrimaryMultimediaLink();
				if (mmLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					result = this.BitmapLoad(mmRec.FileReferences[0], thumbWidth, thumbHeight, throwException);
				}
			}
			catch (MediaFileNotFoundException)
			{
				throw;
			}
			catch (Exception ex)
			{
                this.Host.LogWrite("TfmBase.GetPrimaryBitmap(): " + ex.Message);
				result = null;
			}
			return result;
		}
		
		#endregion

    }
}
