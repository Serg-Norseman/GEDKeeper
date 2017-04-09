/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Dialogs;
using GKUI.Forms;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class BaseWin : MdiChildFormEx, IBaseWindow
    {
        #region Private fields

        private readonly List<GEDCOMRecord> fLockedRecords;

        private readonly NavigationStack fNavman;
        private readonly IBaseContext fContext;

        private bool fModified;
        private ShieldState fShieldState;
        private GEDCOMTree fTree;

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
            get { return MainWin.Instance; }
        }

        public IBaseContext Context
        {
            get { return fContext; }
        }

        public NavigationStack Navman
        {
            get { return fNavman; }
        }


        public bool Modified
        {
            get {
                return fModified;
            }
            set {
                fModified = value;
                SetMainTitle();
            }
        }

        public ShieldState ShieldState
        {
            get {
                return fShieldState;
            }
            set {
                if (fShieldState == value) return;
                fShieldState = value;
                RefreshLists(false);
            }
        }

        public GEDCOMTree Tree
        {
            get { return fTree; }
        }

        #endregion

        #region Instance control

        public BaseWin()
        {
            InitializeComponent();

            fLockedRecords = new List<GEDCOMRecord>();

            fTree = new GEDCOMTree();
            fContext = new BaseContext(fTree, this);

            fNavman = new NavigationStack();

            CreatePage(LangMan.LS(LSID.LSID_RPIndividuals), GEDCOMRecordType.rtIndividual, out ListPersons, out mPersonSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPFamilies), GEDCOMRecordType.rtFamily, out ListFamilies, out mFamilySummary);
            CreatePage(LangMan.LS(LSID.LSID_RPNotes), GEDCOMRecordType.rtNote, out ListNotes, out mNoteSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPMultimedia), GEDCOMRecordType.rtMultimedia, out ListMultimedia, out mMediaSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPSources), GEDCOMRecordType.rtSource, out ListSources, out mSourceSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPRepositories), GEDCOMRecordType.rtRepository, out ListRepositories, out mRepositorySummary);
            CreatePage(LangMan.LS(LSID.LSID_RPGroups), GEDCOMRecordType.rtGroup, out ListGroups, out mGroupSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPResearches), GEDCOMRecordType.rtResearch, out ListResearches, out mResearchSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPTasks), GEDCOMRecordType.rtTask, out ListTasks, out mTaskSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPCommunications), GEDCOMRecordType.rtCommunication, out ListCommunications, out mCommunicationSummary);
            CreatePage(LangMan.LS(LSID.LSID_RPLocations), GEDCOMRecordType.rtLocation, out ListLocations, out mLocationSummary);
            tabsRecords.SelectedIndex = 0;

            (this as ILocalization).SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                #if !__MonoCS__
                fNavman.Dispose();

                if (fTree != null) {
                    fTree.Dispose();
                    fTree = null;
                }

                /*fLockedRecords.Dispose();*/

                if (components != null) components.Dispose();
                #endif
            }
            base.Dispose(disposing);
        }

        #endregion

        #region Form handlers

        private void Form_Activated(object sender, EventArgs e)
        {
            MainWin.Instance.UpdateControls(false);
            MainWin.Instance.BaseChanged(this);
        }

        private void Form_Deactivate(object sender, EventArgs e)
        {
            MainWin.Instance.BaseChanged(null);
            MainWin.Instance.UpdateControls(true);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            ((IWorkWindow)this).UpdateView();
        }

        private void Form_Closing(object sender, FormClosingEventArgs e)
        {
            e.Cancel = !CheckModified();
            if (e.Cancel) return;

            IListManager listMan = GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
            if (listMan != null) {
                listMan.ListColumns.CopyTo(GlobalOptions.Instance.IndividualListColumns);
            }

            MainWin.Instance.BaseClosed(this);
            MainWin.Instance.CheckMRUWin(fTree.FileName, this);
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            // Attention: Does not receive control when executing in Mono
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                    /*case Keys.I:
						ItemAdd();
						break;
					case Keys.D:
						ItemDelete();
						break;*/

                case Keys.Return:
                    if (e.Control) {
                        RecordEdit(null, null);
                    }
                    break;

                    /*case Keys.F12:
                    throw new NotSupportedException(); // debug!*/

                    /*case Keys.F:
        			if (e.Control) {
        				QuickFind();
        			}
        			break;*/
            }
        }

        private void contextMenu_Opening(object sender, CancelEventArgs e)
        {
            GKRecordsView recView = contextMenu.SourceControl as GKRecordsView;

            miRecordDuplicate.Visible = (recView == ListPersons);
        }

        private void miRecordAdd_Click(object sender, EventArgs e)
        {
            RecordAdd();
        }
        
        private void miRecordEdit_Click(object sender, EventArgs e)
        {
            RecordEdit(null, null);
        }

        private void miRecordDelete_Click(object sender, EventArgs e)
        {
            RecordDelete();
        }

        private void miRecordDuplicate_Click(object sender, EventArgs e)
        {
            RecordDuplicate();
        }

        #endregion

        #region Basic function

        public GEDCOMRecordType GetSelectedRecordType()
        {
            return (GEDCOMRecordType)(tabsRecords.SelectedIndex + 1);
        }

        public GKRecordsView GetRecordsViewByType(GEDCOMRecordType recType)
        {
            GKRecordsView list = null;

            switch (recType) {
                case GEDCOMRecordType.rtIndividual:
                    list = ListPersons;
                    break;

                case GEDCOMRecordType.rtFamily:
                    list = ListFamilies;
                    break;

                case GEDCOMRecordType.rtNote:
                    list = ListNotes;
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    list = ListMultimedia;
                    break;

                case GEDCOMRecordType.rtSource:
                    list = ListSources;
                    break;

                case GEDCOMRecordType.rtRepository:
                    list = ListRepositories;
                    break;

                case GEDCOMRecordType.rtGroup:
                    list = ListGroups;
                    break;

                case GEDCOMRecordType.rtResearch:
                    list = ListResearches;
                    break;

                case GEDCOMRecordType.rtTask:
                    list = ListTasks;
                    break;

                case GEDCOMRecordType.rtCommunication:
                    list = ListCommunications;
                    break;

                case GEDCOMRecordType.rtLocation:
                    list = ListLocations;
                    break;
            }

            return list;
        }

        ///-----------------------------------------------------------------------------
        /// <summary>
        /// Gets a hyper-view control for the specified record type.
        /// </summary>
        /// <param name="recType">Record type for which a hyper view control is
        /// required.</param>
        /// <returns>Hyper view control.</returns>
        ///-----------------------------------------------------------------------------
        public HyperView GetHyperViewByType(GEDCOMRecordType recType)
        {
            HyperView view = null;

            switch (recType)
            {
                case GEDCOMRecordType.rtIndividual:
                    view = mPersonSummary;
                    break;

                case GEDCOMRecordType.rtFamily:
                    view = mFamilySummary;
                    break;

                case GEDCOMRecordType.rtNote:
                    view = mNoteSummary;
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    view = mMediaSummary;
                    break;

                case GEDCOMRecordType.rtSource:
                    view = mSourceSummary;
                    break;

                case GEDCOMRecordType.rtRepository:
                    view = mRepositorySummary;
                    break;

                case GEDCOMRecordType.rtGroup:
                    view = mGroupSummary;
                    break;

                case GEDCOMRecordType.rtResearch:
                    view = mResearchSummary;
                    break;

                case GEDCOMRecordType.rtTask:
                    view = mTaskSummary;
                    break;

                case GEDCOMRecordType.rtCommunication:
                    view = mCommunicationSummary;
                    break;

                case GEDCOMRecordType.rtLocation:
                    view = mLocationSummary;
                    break;
            }

            return view;
        }

        public IListManager GetRecordsListManByType(GEDCOMRecordType recType)
        {
            GKRecordsView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.ListMan;
        }

        public GEDCOMRecord GetSelectedRecordEx()
        {
            GEDCOMRecordType rt = GetSelectedRecordType();
            GKRecordsView rView = GetRecordsViewByType(rt);
            return (rView == null) ? null : rView.GetSelectedRecord();
        }

        public GEDCOMIndividualRecord GetSelectedPerson()
        {
            return ListPersons.GetSelectedRecord() as GEDCOMIndividualRecord;
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (sender == null) return;

            GEDCOMRecord rec = ((GKRecordsView) sender).GetSelectedRecord();
            if (rec != null)
            {
                NavAdd(rec);
            }
            ShowRecordInfo(rec);
        }

        public List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType)
        {
            GKRecordsView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.GetContentList();
        }

        private void SetMainTitle()
        {
            Text = Path.GetFileName(fTree.FileName);
            if (fModified)
            {
                Text = @"* " + Text;
            }
        }

        private void mPersonSummaryLink(object sender, string linkName)
        {
            if (linkName.StartsWith("view_"))
            {
                string xref = linkName.Remove(0, 5);
                GEDCOMMultimediaRecord mmRec = fTree.XRefIndex_Find(xref) as GEDCOMMultimediaRecord;
                if (mmRec != null)
                {
                    ShowMedia(mmRec, false);
                }
            }
            else
            {
                SelectRecordByXRef(linkName);
            }
        }

        private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
        {
            MainWin.Instance.UpdateControls(false);
        }

        public void ApplyFilter()
        {
            if (fTree.RecordsCount > 0)
            {
                RefreshLists(false);
            }
        }

        public void ApplyFilter(GEDCOMRecordType recType)
        {
            if (fTree.RecordsCount > 0)
            {
                RefreshRecordsView(recType);
            }
        }

        public void ChangeRecord(GEDCOMRecord record)
        {
            if (record == null) return;

            record.ChangeDate.ChangeDateTime = DateTime.Now;

            Modified = true;
            fTree.Header.TransmissionDateTime = DateTime.Now;

            MainWin.Instance.NotifyRecord(this, record, RecordAction.raEdit);
        }

        public bool CheckModified()
        {
            bool result = true;
            if (!Modified) return result;

            DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
            switch (dialogResult) {
                case DialogResult.Yes:
                    MainWin.Instance.miFileSave_Click(null, null);
                    break;
                case DialogResult.No:
                    break;
                case DialogResult.Cancel:
                    result = false;
                    break;
            }

            return result;
        }

        private void CreatePage(string pageText, GEDCOMRecordType recType, out GKRecordsView recView, out HyperView summary)
        {
            tabsRecords.SuspendLayout();
            TabPage sheet = new TabPage(pageText);
            tabsRecords.Controls.Add(sheet);
            tabsRecords.ResumeLayout(false);

            summary = new HyperView();
            summary.BorderWidth = 4;
            summary.Dock = DockStyle.Right;
            summary.Size = new Size(300, 290);
            summary.OnLink += mPersonSummaryLink;

            Splitter spl = new Splitter();
            spl.Dock = DockStyle.Right;
            spl.Size = new Size(4, 290);
            spl.MinExtra = 100;
            spl.MinSize = 100;

            sheet.Controls.Add(summary);
            sheet.Controls.Add(spl);

            recView = (GKRecordsView)AppHub.UIHelper.CreateRecordsView(sheet, fTree, recType);
            recView.IsMainList = true;
            recView.DoubleClick += RecordEdit;
            recView.SelectedIndexChanged += List_SelectedIndexChanged;
            recView.UpdateTitles();
            recView.ContextMenuStrip = contextMenu;

            sheet.Controls.SetChildIndex(spl, 1);
            sheet.Controls.SetChildIndex(summary, 2);
        }

        private void ChangeFileName()
        {
            SetMainTitle();
            MainWin.Instance.Options.LastDir = Path.GetDirectoryName(fTree.FileName);
            MainWin.Instance.AddMRU(fTree.FileName);
        }

        public void Clear()
        {
            fNavman.Clear();
            fContext.Clear();
        }

        public bool IsUnknown()
        {
            string fileName = fTree.FileName;

            return string.IsNullOrEmpty(fileName) || !File.Exists(fileName);
        }

        public void FileNew()
        {
            Clear();
            RefreshLists(false);
            GKUtils.ShowPersonInfo(null, mPersonSummary.Lines, fShieldState);
            fTree.SetFileName(LangMan.LS(LSID.LSID_Unknown));
            fTree.Header.Language.Value = MainWin.Instance.Options.GetCurrentItfLang();
            Modified = false;
        }

        public void FileLoad(string fileName)
        {
            Clear();

            string pw = null;
            string ext = SysUtils.GetFileExtension(fileName);
            if (ext == ".geds") {
                if (!AppHub.StdDialogs.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                    AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                    return;
                }
            }

            IProgressController progress = AppHub.Progress;

            try
            {
                progress.ProgressInit(LangMan.LS(LSID.LSID_Loading), 100);
                fTree.OnProgress += LoadProgress;
                try
                {
                    fContext.FileLoad(fileName, pw);
                }
                finally
                {
                    fTree.OnProgress -= LoadProgress;
                    progress.ProgressDone();
                }

                TreeTools.CheckGEDCOMFormat(fTree, fContext.ValuesCollection, progress);
                Modified = false;
            }
            catch (Exception ex)
            {
                Host.LogWrite("BaseWin.FileLoad(): " + ex.Message);
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_LoadGedComFailed));
            }

            ChangeFileName();
            RefreshLists(false);
        }

        public void FileSave(string fileName)
        {
            try
            {
                string pw = null;
                string ext = SysUtils.GetFileExtension(fileName);
                if (ext == ".geds") {
                    if (!AppHub.StdDialogs.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                        return;
                    }
                }

                fContext.FileSave(fileName, pw);
                Modified = false;

                ChangeFileName();
            }
            catch (UnauthorizedAccessException)
            {
                AppHub.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": access denied" }));
            }
            catch (Exception ex)
            {
                AppHub.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
                Host.LogWrite("BaseWin.FileSave(): " + ex.Message);
            }
        }

        public void CriticalSave()
        {
            try
            {
                string rfn = Path.ChangeExtension(fTree.FileName, ".restore");
                // TODO: PrepareHeader or not?
                fTree.SaveToFile(rfn, GlobalOptions.Instance.DefCharacterSet);
            } catch (Exception ex) {
                Host.LogWrite("BaseWin.CriticalSave(): " + ex.Message);
            }
        }

        public void UpdateListsSettings()
        {
            IListManager listMan = GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
            if (listMan != null) {
                GlobalOptions.Instance.IndividualListColumns.CopyTo(listMan.ListColumns);
            }
        }

        public void RefreshLists(bool titles)
        {
            ListPersons.UpdateContents(fShieldState, titles, -1/*2*/);
            ListFamilies.UpdateContents(fShieldState, titles, 1);
            ListNotes.UpdateContents(fShieldState, titles, -1);
            ListMultimedia.UpdateContents(fShieldState, titles, 1);
            ListSources.UpdateContents(fShieldState, titles, 1);
            ListRepositories.UpdateContents(fShieldState, titles, 1);
            ListGroups.UpdateContents(fShieldState, titles, 1);
            ListResearches.UpdateContents(fShieldState, titles, 1);
            ListTasks.UpdateContents(fShieldState, titles, 1);
            ListCommunications.UpdateContents(fShieldState, titles, 1);
            ListLocations.UpdateContents(fShieldState, titles, 1);

            PageRecords_SelectedIndexChanged(null, null);
        }

        public void RefreshRecordsView(GEDCOMRecordType recType)
        {
            GKRecordsView rView = GetRecordsViewByType(recType);
            if (rView == null) return;

            rView.UpdateContents(fShieldState, false, -1);

            PageRecords_SelectedIndexChanged(null, null);
        }

        public void RecordNotify(GEDCOMRecord record, RecordAction action)
        {
            if (record == null) return;

            GKRecordsView rView = GetRecordsViewByType(record.RecordType);

            if (rView != null && action == RecordAction.raDelete)
            {
                rView.DeleteRecord(record);
                HyperView hView = GetHyperViewByType(record.RecordType);
                if ((null != hView) && (0 == rView.FilteredCount))
                {
                    hView.Lines.Clear();
                }
            }

            MainWin.Instance.NotifyRecord(this, record, action);
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            GEDCOMRecordType rt = GetSelectedRecordType();
            IListManager listMan = GetRecordsListManByType(rt);
            if (listMan == null) return;

            switch (rt) {
                case GEDCOMRecordType.rtIndividual:
                    using (PersonsFilterDlg fmFilter = new PersonsFilterDlg(this, listMan)) {
                        DialogResult res = MainWin.Instance.ShowModalEx(fmFilter, false);
                        if (res == DialogResult.OK) ApplyFilter(rt);
                    }
                    break;

                case GEDCOMRecordType.rtFamily:
                case GEDCOMRecordType.rtNote:
                case GEDCOMRecordType.rtMultimedia:
                case GEDCOMRecordType.rtSource:
                case GEDCOMRecordType.rtRepository:
                case GEDCOMRecordType.rtGroup:
                case GEDCOMRecordType.rtResearch:
                case GEDCOMRecordType.rtTask:
                case GEDCOMRecordType.rtCommunication:
                case GEDCOMRecordType.rtLocation:
                    using (CommonFilterDlg fmComFilter = new CommonFilterDlg(this, listMan)) {
                        DialogResult res = MainWin.Instance.ShowModalEx(fmComFilter, false);
                        if (res == DialogResult.OK) ApplyFilter(rt);
                    }
                    break;
            }
        }

        private void NavAdd(GEDCOMRecord aRec)
        {
            if (aRec == null || fNavman.Busy) return;

            fNavman.Current = aRec;
            MainWin.Instance.UpdateControls(false);
        }

        public void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal)
        {
            if (mediaRec == null)
                throw new ArgumentNullException("mediaRec");

            GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
            bool externalViewer = !GlobalOptions.Instance.EmbeddedMediaPlayer &&
                ((mmKind == MultimediaKind.mkAudio || mmKind == MultimediaKind.mkVideo));

            if (externalViewer) {
                string targetFile = "";
                fContext.MediaLoad(fileRef, ref targetFile);
                SysUtils.LoadExtFile(targetFile);
            } else {
                MediaViewerWin mediaViewer = new MediaViewerWin(this);
                try
                {
                    try
                    {
                        mediaViewer.FileRef = fileRef;
                        if (modal) {
                            mediaViewer.ShowDialog();
                        } else {
                            mediaViewer.ShowInTaskbar = true;
                            mediaViewer.Show();
                        }
                    }
                    finally
                    {
                        if (modal) mediaViewer.Dispose();
                    }
                }
                catch (Exception ex)
                {
                    if (mediaViewer != null) mediaViewer.Dispose();
                    Host.LogWrite("BaseWin.ShowMedia(): " + ex.Message);
                }
            }
        }

        public void CollectTips(StringList tipsList)
        {
            if (tipsList == null)
                throw new ArgumentNullException("tipsList");

            if (!MainWin.Instance.Options.ShowTips) return;

            try
            {
                try
                {
                    bool firstTip = true;
                    int num = fTree.RecordsCount;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMRecord rec = fTree[i];
                        if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                        GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord) rec;

                        int days;
                        if (GKUtils.GetDaysForBirth(iRec, out days))
                        {
                            string nm = GKUtils.GetNameString(iRec, true, false);
                            nm = fContext.Culture.GetPossessiveName(nm);

                            if (days >= 0 && 3 > days) {
                                string tip;

                                if (firstTip) {
                                    tipsList.Add("#" + LangMan.LS(LSID.LSID_BirthDays));
                                    firstTip = false;
                                }

                                if (0 == days)
                                {
                                    tip = string.Format(
                                        LangMan.LS(LSID.LSID_BirthdayToday), nm);
                                }
                                else if (1 == days)
                                {
                                    tip = string.Format(
                                        LangMan.LS(LSID.LSID_BirthdayTomorrow), nm);
                                }
                                else
                                {
                                    tip = string.Format(
                                        LangMan.LS(LSID.LSID_DaysRemained),
                                        nm, days);
                                }

                                tipsList.Add(tip);
                            }
                        }
                    }
                }
                finally
                {
                    // temp stub, remove try/finally here?
                }
            }
            catch (Exception ex)
            {
                Host.LogWrite("BaseWin.ShowTips(): " + ex.Message);
            }
        }

        private void LoadProgress(object sender, int progress)
        {
            AppHub.Progress.ProgressStep(progress);
        }

        #endregion

        #region ILocalization implementation

        void ILocalization.SetLang()
        {
            tabsRecords.TabPages[ 0].Text = LangMan.LS(LSID.LSID_RPIndividuals);
            tabsRecords.TabPages[ 1].Text = LangMan.LS(LSID.LSID_RPFamilies);
            tabsRecords.TabPages[ 2].Text = LangMan.LS(LSID.LSID_RPNotes);
            tabsRecords.TabPages[ 3].Text = LangMan.LS(LSID.LSID_RPMultimedia);
            tabsRecords.TabPages[ 4].Text = LangMan.LS(LSID.LSID_RPSources);
            tabsRecords.TabPages[ 5].Text = LangMan.LS(LSID.LSID_RPRepositories);
            tabsRecords.TabPages[ 6].Text = LangMan.LS(LSID.LSID_RPGroups);
            tabsRecords.TabPages[ 7].Text = LangMan.LS(LSID.LSID_RPResearches);
            tabsRecords.TabPages[ 8].Text = LangMan.LS(LSID.LSID_RPTasks);
            tabsRecords.TabPages[ 9].Text = LangMan.LS(LSID.LSID_RPCommunications);
            tabsRecords.TabPages[10].Text = LangMan.LS(LSID.LSID_RPLocations);

            miRecordAdd.Text = LangMan.LS(LSID.LSID_MIRecordAdd);
            miRecordEdit.Text = LangMan.LS(LSID.LSID_MIRecordEdit);
            miRecordDelete.Text = LangMan.LS(LSID.LSID_MIRecordDelete);
            miRecordDuplicate.Text = LangMan.LS(LSID.LSID_RecordDuplicate);
        }

        #endregion

        #region IWorkWindow implementation
        
        string IWorkWindow.GetStatusString()
        {
            string res = "";

            GEDCOMRecordType rt = GetSelectedRecordType();
            GKRecordsView rView = GetRecordsViewByType(rt);

            if (rView != null)
            {
                res = LangMan.LS(LSID.LSID_SBRecords) + ": " + rView.TotalCount.ToString();
                res = res + ", " + LangMan.LS(LSID.LSID_SBFiltered) + ": " + rView.FilteredCount.ToString();
            }

            return res;
        }

        void IWorkWindow.UpdateView()
        {
            UpdateListsSettings();
            RefreshLists(true);
        }

        void IWorkWindow.NavNext()
        {
            fNavman.BeginNav();
            try
            {
                GEDCOMRecord rec = fNavman.Next() as GEDCOMRecord;
                if (rec != null)
                {
                    SelectRecordByXRef(rec.XRef);
                    MainWin.Instance.UpdateControls(false);
                }
            }
            finally
            {
                fNavman.EndNav();
            }
        }

        void IWorkWindow.NavPrev()
        {
            fNavman.BeginNav();
            try
            {
                GEDCOMRecord rec = fNavman.Back() as GEDCOMRecord;
                if (rec != null)
                {
                    SelectRecordByXRef(rec.XRef);
                    MainWin.Instance.UpdateControls(false);
                }
            }
            finally
            {
                fNavman.EndNav();
            }
        }

        bool IWorkWindow.NavCanBackward()
        {
            return fNavman.CanBackward();
        }

        bool IWorkWindow.NavCanForward()
        {
            return fNavman.CanForward();
        }

        public bool AllowQuickSearch()
        {
            return true;
        }

        IList<ISearchResult> IWorkWindow.FindAll(string searchPattern)
        {
            List<ISearchResult> result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = fTree[i];

                if (rec.RecordType == GEDCOMRecordType.rtIndividual) {
                    GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;

                    string fullname = GKUtils.GetNameString(iRec, true, false);
                    if (GKUtils.MatchesRegex(fullname, regex)) {
                        //yield return new SearchResult(iRec);
                        result.Add(new SearchResult(iRec));
                    }
                }
            }

            return result;
        }

        void IWorkWindow.SelectByRec(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            // platform: In Windows works without it
            #if __MonoCS__
            Activate();
            #endif

            SelectRecordByXRef(iRec.XRef);
        }

        void IWorkWindow.QuickSearch()
        {
            QuickSearchDlg qsDlg = new QuickSearchDlg(this);

            Rectangle client = ClientRectangle;
            Point pt = PointToScreen(new Point(client.Left, client.Bottom - qsDlg.Height));
            qsDlg.Location = pt;

            qsDlg.Show();
        }

        #endregion

        #region Record Management

        public void RecordDuplicate()
        {
            GEDCOMRecord source = GetSelectedRecordEx();
            if (source == null) return;
            if (source.RecordType != GEDCOMRecordType.rtIndividual) return;

            AppHub.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_DuplicateWarning));

            GEDCOMIndividualRecord target;
            try {
                fContext.BeginUpdate();

                target = fContext.Tree.CreateIndividual();
                target.Assign(source);

                ChangeRecord(target);
            } finally {
                fContext.EndUpdate();
            }

            RefreshLists(false);
            SelectRecordByXRef(target.XRef);
        }

        public void RecordAdd()
        {
            bool result = false;

            GEDCOMRecord rec = null;
            GEDCOMRecordType rt = GetSelectedRecordType();

            switch (rt)
            {
                case GEDCOMRecordType.rtIndividual:
                    {
                        GEDCOMIndividualRecord indivRec = null;
                        result = AppHub.BaseController.ModifyIndividual(this, ref indivRec, null, TargetMode.tmParent, GEDCOMSex.svNone);
                        rec = indivRec;
                        break;
                    }

                case GEDCOMRecordType.rtFamily:
                    {
                        GEDCOMFamilyRecord fam = null;
                        result = AppHub.BaseController.ModifyFamily(this, ref fam, FamilyTarget.None, null);
                        rec = fam;
                        break;
                    }
                case GEDCOMRecordType.rtNote:
                    {
                        GEDCOMNoteRecord note = null;
                        result = AppHub.BaseController.ModifyNote(this, ref note);
                        rec = note;
                        break;
                    }
                case GEDCOMRecordType.rtMultimedia:
                    {
                        GEDCOMMultimediaRecord mmRec = null;
                        result = AppHub.BaseController.ModifyMedia(this, ref mmRec);
                        rec = mmRec;
                        break;
                    }
                case GEDCOMRecordType.rtSource:
                    {
                        GEDCOMSourceRecord src = null;
                        result = AppHub.BaseController.ModifySource(this, ref src);
                        rec = src;
                        break;
                    }
                case GEDCOMRecordType.rtRepository:
                    {
                        GEDCOMRepositoryRecord rep = null;
                        result = AppHub.BaseController.ModifyRepository(this, ref rep);
                        rec = rep;
                        break;
                    }
                case GEDCOMRecordType.rtGroup:
                    {
                        GEDCOMGroupRecord grp = null;
                        result = AppHub.BaseController.ModifyGroup(this, ref grp);
                        rec = grp;
                        break;
                    }
                case GEDCOMRecordType.rtResearch:
                    {
                        GEDCOMResearchRecord rsr = null;
                        result = AppHub.BaseController.ModifyResearch(this, ref rsr);
                        rec = rsr;
                        break;
                    }
                case GEDCOMRecordType.rtTask:
                    {
                        GEDCOMTaskRecord tsk = null;
                        result = AppHub.BaseController.ModifyTask(this, ref tsk);
                        rec = tsk;
                        break;
                    }
                case GEDCOMRecordType.rtCommunication:
                    {
                        GEDCOMCommunicationRecord comm = null;
                        result = AppHub.BaseController.ModifyCommunication(this, ref comm);
                        rec = comm;
                        break;
                    }
                case GEDCOMRecordType.rtLocation:
                    {
                        GEDCOMLocationRecord loc = null;
                        result = AppHub.BaseController.ModifyLocation(this, ref loc);
                        rec = loc;
                        break;
                    }
            }

            if (result) {
                RefreshLists(false);
                SelectRecordByXRef(rec.XRef);
            }
        }

        public bool RecordDelete(GEDCOMRecord record, bool confirm)
        {
            bool result = false;

            if (record != null)
            {
                //string xref = record.XRef;
                string msg = "";
                switch (record.RecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        msg = string.Format(LangMan.LS(LSID.LSID_PersonDeleteQuery), GKUtils.GetNameString(((GEDCOMIndividualRecord)record), true, false));
                        break;

                    case GEDCOMRecordType.rtFamily:
                        msg = string.Format(LangMan.LS(LSID.LSID_FamilyDeleteQuery), GKUtils.GetFamilyString((GEDCOMFamilyRecord)record));
                        break;

                    case GEDCOMRecordType.rtNote:
                        {
                            string value = GKUtils.TruncateStrings(((GEDCOMNoteRecord) (record)).Note, GKData.NOTE_NAME_MAX_LENGTH);
                            if (string.IsNullOrEmpty(value))
                            {
                                value = string.Format("#{0}", record.GetId().ToString());
                            }
                            msg = string.Format(LangMan.LS(LSID.LSID_NoteDeleteQuery), value);
                            break;
                        }

                    case GEDCOMRecordType.rtMultimedia:
                        msg = string.Format(LangMan.LS(LSID.LSID_MediaDeleteQuery), ((GEDCOMMultimediaRecord)record).GetFileTitle());
                        break;

                    case GEDCOMRecordType.rtSource:
                        msg = string.Format(LangMan.LS(LSID.LSID_SourceDeleteQuery), ((GEDCOMSourceRecord)record).FiledByEntry);
                        break;

                    case GEDCOMRecordType.rtRepository:
                        msg = string.Format(LangMan.LS(LSID.LSID_RepositoryDeleteQuery), ((GEDCOMRepositoryRecord)record).RepositoryName);
                        break;

                    case GEDCOMRecordType.rtGroup:
                        msg = string.Format(LangMan.LS(LSID.LSID_GroupDeleteQuery), ((GEDCOMGroupRecord)record).GroupName);
                        break;

                    case GEDCOMRecordType.rtResearch:
                        msg = string.Format(LangMan.LS(LSID.LSID_ResearchDeleteQuery), ((GEDCOMResearchRecord)record).ResearchName);
                        break;

                    case GEDCOMRecordType.rtTask:
                        msg = string.Format(LangMan.LS(LSID.LSID_TaskDeleteQuery), GKUtils.GetTaskGoalStr((GEDCOMTaskRecord)record));
                        break;

                    case GEDCOMRecordType.rtCommunication:
                        msg = string.Format(LangMan.LS(LSID.LSID_CommunicationDeleteQuery), ((GEDCOMCommunicationRecord)record).CommName);
                        break;

                    case GEDCOMRecordType.rtLocation:
                        msg = string.Format(LangMan.LS(LSID.LSID_LocationDeleteQuery), ((GEDCOMLocationRecord)record).LocationName);
                        break;
                }

                if (confirm && AppHub.StdDialogs.ShowQuestionYN(msg) != true)
                    return false;

                RecordNotify(record, RecordAction.raDelete);

                result = fContext.DeleteRecord(record);

                if (result) {
                    Modified = true;
                    fTree.Header.TransmissionDateTime = DateTime.Now;
                }
            }

            return result;
        }

        public void RecordDelete()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record == null) return;

            bool result = RecordDelete(record, true);

            if (result) {
                RefreshLists(false);
            }
        }

        public void RecordEdit(object sender, EventArgs e)
        {
            GEDCOMRecord rec = GetSelectedRecordEx();
            if (rec == null) return;

            bool result = false;

            switch (rec.RecordType) {
                case GEDCOMRecordType.rtIndividual:
                    GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
                    result = AppHub.BaseController.ModifyIndividual(this, ref ind, null, TargetMode.tmNone, GEDCOMSex.svNone);
                    break;

                case GEDCOMRecordType.rtFamily:
                    GEDCOMFamilyRecord fam = rec as GEDCOMFamilyRecord;
                    result = AppHub.BaseController.ModifyFamily(this, ref fam, FamilyTarget.None, null);
                    break;

                case GEDCOMRecordType.rtNote:
                    GEDCOMNoteRecord note = rec as GEDCOMNoteRecord;
                    result = AppHub.BaseController.ModifyNote(this, ref note);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    GEDCOMMultimediaRecord mmRec = rec as GEDCOMMultimediaRecord;
                    result = AppHub.BaseController.ModifyMedia(this, ref mmRec);
                    break;

                case GEDCOMRecordType.rtSource:
                    GEDCOMSourceRecord src = rec as GEDCOMSourceRecord;
                    result = AppHub.BaseController.ModifySource(this, ref src);
                    break;

                case GEDCOMRecordType.rtRepository:
                    GEDCOMRepositoryRecord rep = rec as GEDCOMRepositoryRecord;
                    result = AppHub.BaseController.ModifyRepository(this, ref rep);
                    break;

                case GEDCOMRecordType.rtGroup:
                    GEDCOMGroupRecord grp = rec as GEDCOMGroupRecord;
                    result = AppHub.BaseController.ModifyGroup(this, ref grp);
                    break;

                case GEDCOMRecordType.rtResearch:
                    GEDCOMResearchRecord rsr = rec as GEDCOMResearchRecord;
                    result = AppHub.BaseController.ModifyResearch(this, ref rsr);
                    break;

                case GEDCOMRecordType.rtTask:
                    GEDCOMTaskRecord tsk = rec as GEDCOMTaskRecord;
                    result = AppHub.BaseController.ModifyTask(this, ref tsk);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    GEDCOMCommunicationRecord comm = rec as GEDCOMCommunicationRecord;
                    result = AppHub.BaseController.ModifyCommunication(this, ref comm);
                    break;

                case GEDCOMRecordType.rtLocation:
                    GEDCOMLocationRecord loc = rec as GEDCOMLocationRecord;
                    result = AppHub.BaseController.ModifyLocation(this, ref loc);
                    break;
            }

            if (result)
            {
                RefreshLists(false);
                ShowRecordInfo(rec);
            }
        }

        public void ShowRecordsTab(GEDCOMRecordType recType)
        {
            tabsRecords.SelectedIndex = (int)recType - 1;
            PageRecords_SelectedIndexChanged(null, null);
        }

        public void SelectRecordByXRef(string xref)
        {
            GEDCOMRecord record = fTree.XRefIndex_Find(xref);
            if (record == null) return;

            GKRecordsView rView = GetRecordsViewByType(record.RecordType);
            if (rView == null) return;

            ShowRecordsTab(record.RecordType);
            ActiveControl = rView;
            //aList.Focus();
            rView.SelectItemByRec(record);
        }

        public void ShowRecordInfo(GEDCOMRecord record)
        {
            if (record == null) return;

            try
            {
                switch (record.RecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        GKUtils.ShowPersonInfo(record as GEDCOMIndividualRecord, mPersonSummary.Lines, fShieldState);
                        break;

                    case GEDCOMRecordType.rtFamily:
                        GKUtils.ShowFamilyInfo(record as GEDCOMFamilyRecord, mFamilySummary.Lines, fShieldState);
                        break;

                    case GEDCOMRecordType.rtNote:
                        GKUtils.ShowNoteInfo(record as GEDCOMNoteRecord, mNoteSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtMultimedia:
                        GKUtils.ShowMultimediaInfo(record as GEDCOMMultimediaRecord, mMediaSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtSource:
                        GKUtils.ShowSourceInfo(record as GEDCOMSourceRecord, mSourceSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtRepository:
                        GKUtils.ShowRepositoryInfo(record as GEDCOMRepositoryRecord, mRepositorySummary.Lines);
                        break;

                    case GEDCOMRecordType.rtGroup:
                        GKUtils.ShowGroupInfo(record as GEDCOMGroupRecord, mGroupSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtResearch:
                        GKUtils.ShowResearchInfo(record as GEDCOMResearchRecord, mResearchSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtTask:
                        GKUtils.ShowTaskInfo(record as GEDCOMTaskRecord, mTaskSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtCommunication:
                        GKUtils.ShowCommunicationInfo(record as GEDCOMCommunicationRecord, mCommunicationSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtLocation:
                        GKUtils.ShowLocationInfo(record as GEDCOMLocationRecord, mLocationSummary.Lines);
                        break;
                }
            }
            catch (Exception ex)
            {
                Host.LogWrite("BaseWin.ShowRecordInfo(): " + ex.Message);
            }
        }

        public StringList GetRecordContent(GEDCOMRecord record)
        {
            StringList ctx = new StringList();

            if (record != null)
            {
                try
                {
                    switch (record.RecordType)
                    {
                        case GEDCOMRecordType.rtIndividual:
                            GKUtils.ShowPersonInfo(record as GEDCOMIndividualRecord, ctx, fShieldState);
                            break;

                        case GEDCOMRecordType.rtFamily:
                            GKUtils.ShowFamilyInfo(record as GEDCOMFamilyRecord, ctx, fShieldState);
                            break;

                        case GEDCOMRecordType.rtNote:
                            GKUtils.ShowNoteInfo(record as GEDCOMNoteRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtMultimedia:
                            GKUtils.ShowMultimediaInfo(record as GEDCOMMultimediaRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtSource:
                            GKUtils.ShowSourceInfo(record as GEDCOMSourceRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtRepository:
                            GKUtils.ShowRepositoryInfo(record as GEDCOMRepositoryRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtGroup:
                            GKUtils.ShowGroupInfo(record as GEDCOMGroupRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtResearch:
                            GKUtils.ShowResearchInfo(record as GEDCOMResearchRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtTask:
                            GKUtils.ShowTaskInfo(record as GEDCOMTaskRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtCommunication:
                            GKUtils.ShowCommunicationInfo(record as GEDCOMCommunicationRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtLocation:
                            GKUtils.ShowLocationInfo(record as GEDCOMLocationRecord, ctx);
                            break;
                    }
                }
                catch (Exception ex)
                {
                    Host.LogWrite("BaseWin.GetRecordContext(): " + ex.Message);
                }
            }

            return ctx;
        }

        public string GetRecordName(GEDCOMRecord record, bool signed)
        {
            return GKUtils.GetRecordName(record, signed);
        }

        public bool RecordIsFiltered(GEDCOMRecord record)
        {
            bool result = false;
            if (record == null) return result;

            GKRecordsView rView = GetRecordsViewByType(record.RecordType);
            result = (rView != null && rView.IndexOfRecord(record) >= 0);
            return result;
        }

        #endregion

        #region Modify routines

        /// <summary>
        /// This method performs a basic locking of the records for their
        /// editors.
        /// 
        /// The original idea was to call the methods Lock/Unlock records,
        /// in the edit dialogs of the records. However, it would be unsafe,
        /// because in the case of a failure of dialogue, the record would
        /// remain locked. Therefore, the locking and unlocking of records
        /// must take on a methods that controls the dialog.
        /// </summary>
        /// <param name="record"></param>
        public void LockRecord(GEDCOMRecord record)
        {
            fLockedRecords.Add(record);
        }

        public void UnlockRecord(GEDCOMRecord record)
        {
            fLockedRecords.Remove(record);
        }

        public bool IsAvailableRecord(GEDCOMRecord record)
        {
            bool result = fLockedRecords.IndexOf(record) < 0;

            if (!result) {
                // message, for exclude of duplication
                AppHub.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_RecordIsLocked));
            }

            return result;
        }

        #endregion
    }
}
