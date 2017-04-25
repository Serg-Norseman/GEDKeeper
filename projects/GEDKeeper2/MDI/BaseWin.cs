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
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;
using GKUI.Dialogs;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class BaseWin : MdiChildFormEx, IBaseWindow
    {
        #region Private fields

        private readonly IBaseContext fContext;
        private readonly NavigationStack fNavman;

        private bool fModified;

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

        #endregion

        #region Instance control

        public BaseWin()
        {
            InitializeComponent();

            fContext = new BaseContext(this);
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
                fContext.Dispose();

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
            MainWin.Instance.CheckMRUWin(fContext.FileName, this);
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
                        EditRecord();
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
            AddRecord();
        }

        private void miRecordEdit_Click(object sender, EventArgs e)
        {
            EditRecord();
        }

        private void miRecordDelete_Click(object sender, EventArgs e)
        {
            DeleteRecord();
        }

        private void miRecordDuplicate_Click(object sender, EventArgs e)
        {
            DuplicateRecord();
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


        /// <summary>
        /// Gets a hyper-view control for the specified record type.
        /// </summary>
        /// <param name="recType">Record type for which a hyper view control is
        /// required.</param>
        /// <returns>Hyper view control.</returns>
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
            GEDCOMRecordType recType = GetSelectedRecordType();
            GKRecordsView rView = GetRecordsViewByType(recType);
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
            Text = Path.GetFileName(fContext.FileName);
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
                GEDCOMMultimediaRecord mmRec = fContext.Tree.XRefIndex_Find(xref) as GEDCOMMultimediaRecord;
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

        public void ApplyFilter(GEDCOMRecordType recType = GEDCOMRecordType.rtNone)
        {
            if (fContext.Tree.RecordsCount > 0)
            {
                if (recType == GEDCOMRecordType.rtNone) {
                    RefreshLists(false);
                } else {
                    RefreshRecordsView(recType);
                }
            }
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

            recView = UIHelper.CreateRecordsView(sheet, fContext.Tree, recType);
            recView.IsMainList = true;
            recView.DoubleClick += miRecordEdit_Click;
            recView.SelectedIndexChanged += List_SelectedIndexChanged;
            recView.UpdateTitles();
            recView.ContextMenuStrip = contextMenu;

            sheet.Controls.SetChildIndex(spl, 1);
            sheet.Controls.SetChildIndex(summary, 2);
        }

        private void ChangeFileName()
        {
            SetMainTitle();
            GlobalOptions.Instance.LastDir = Path.GetDirectoryName(fContext.FileName);
            MainWin.Instance.AddMRU(fContext.FileName);
        }

        public void Clear()
        {
            fNavman.Clear();
            fContext.Clear();
        }

        public void CreateNewFile()
        {
            Clear();
            RefreshLists(false);
            GKUtils.ShowPersonInfo(null, mPersonSummary.Lines, fContext.ShieldState);
            fContext.SetFileName(LangMan.LS(LSID.LSID_Unknown));
            fContext.Tree.Header.Language.Value = GlobalOptions.Instance.GetCurrentItfLang();
            Modified = false;
        }

        public void LoadFile(string fileName)
        {
            Clear();

            if (fContext.FileLoad(fileName)) {
                Modified = false;
                ChangeFileName();
                RefreshLists(false);
            }
        }

        public void SaveFile(string fileName)
        {
            if (fContext.FileSave(fileName)) {
                Modified = false;
                ChangeFileName();
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
            ShieldState shieldState = Context.ShieldState;

            ListPersons.UpdateContents(shieldState, titles, -1/*2*/);
            ListFamilies.UpdateContents(shieldState, titles, 1);
            ListNotes.UpdateContents(shieldState, titles, -1);
            ListMultimedia.UpdateContents(shieldState, titles, 1);
            ListSources.UpdateContents(shieldState, titles, 1);
            ListRepositories.UpdateContents(shieldState, titles, 1);
            ListGroups.UpdateContents(shieldState, titles, 1);
            ListResearches.UpdateContents(shieldState, titles, 1);
            ListTasks.UpdateContents(shieldState, titles, 1);
            ListCommunications.UpdateContents(shieldState, titles, 1);
            ListLocations.UpdateContents(shieldState, titles, 1);

            PageRecords_SelectedIndexChanged(null, null);
        }

        public void RefreshRecordsView(GEDCOMRecordType recType)
        {
            GKRecordsView rView = GetRecordsViewByType(recType);
            if (rView != null) {
                rView.UpdateContents(fContext.ShieldState, false, -1);
                PageRecords_SelectedIndexChanged(null, null);
            }
        }

        public void NotifyRecord(GEDCOMRecord record, RecordAction action)
        {
            if (record == null) return;

            DateTime dtNow = DateTime.Now;

            switch (action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    record.ChangeDate.ChangeDateTime = dtNow;
                    break;

                case RecordAction.raDelete:
                    {
                        GKRecordsView rView = GetRecordsViewByType(record.RecordType);
                        if (rView != null) {
                            rView.DeleteRecord(record);

                            HyperView hView = GetHyperViewByType(record.RecordType);
                            if ((hView != null) && (rView.FilteredCount == 0)) {
                                hView.Lines.Clear();
                            }
                        }
                    }
                    break;

                case RecordAction.raJump:
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    break;
            }

            if (action != RecordAction.raJump) {
                fContext.Tree.Header.TransmissionDateTime = dtNow;
                Modified = true;

                MainWin.Instance.NotifyRecord(this, record, action);
            }
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
                    Logger.LogWrite("BaseWin.ShowMedia(): " + ex.Message);
                }
            }
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

            GEDCOMRecordType recType = GetSelectedRecordType();
            GKRecordsView rView = GetRecordsViewByType(recType);

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
            GEDCOMRecordType rt = GetSelectedRecordType();
            IList<ISearchResult> result = fContext.FindAll(rt, searchPattern);
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

        public void DuplicateRecord()
        {
            GEDCOMRecord original = GetSelectedRecordEx();
            if (original == null) return;
            if (original.RecordType != GEDCOMRecordType.rtIndividual) return;

            AppHub.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_DuplicateWarning));

            GEDCOMIndividualRecord target;
            try {
                fContext.BeginUpdate();

                target = fContext.Tree.CreateIndividual();
                target.Assign(original);

                NotifyRecord(target, RecordAction.raAdd);
            } finally {
                fContext.EndUpdate();
            }

            RefreshLists(false);
            SelectRecordByXRef(target.XRef);
        }

        public void AddRecord()
        {
            GEDCOMRecordType rt = GetSelectedRecordType();

            GEDCOMRecord rec;
            if (AppHub.BaseController.AddRecord(this, rt, null, out rec)) {
                RefreshLists(false);
                SelectRecordByXRef(rec.XRef);
            }
        }

        public void EditRecord()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record == null) return;

            if (AppHub.BaseController.EditRecord(this, record)) {
                RefreshLists(false);
                ShowRecordInfo(record);
            }
        }

        public void DeleteRecord()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record == null) return;

            if (AppHub.BaseController.DeleteRecord(this, record, true)) {
                RefreshLists(false);
            }
        }

        public void ShowRecordsTab(GEDCOMRecordType recType)
        {
            tabsRecords.SelectedIndex = (int)recType - 1;
            PageRecords_SelectedIndexChanged(null, null);
        }

        public void SelectRecordByXRef(string xref)
        {
            GEDCOMRecord record = fContext.Tree.XRefIndex_Find(xref);
            if (record == null) return;

            GKRecordsView rView = GetRecordsViewByType(record.RecordType);
            if (rView == null) return;

            ShowRecordsTab(record.RecordType);
            ActiveControl = rView;
            rView.SelectItemByRec(record);
        }

        public void ShowRecordInfo(GEDCOMRecord record)
        {
            if (record == null) return;

            try
            {
                HyperView hyperView = GetHyperViewByType(record.RecordType);
                if (hyperView != null) {
                    GKUtils.GetRecordContent(record, fContext.ShieldState, hyperView.Lines);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseWin.ShowRecordInfo(): " + ex.Message);
            }
        }

        public StringList GetRecordContent(GEDCOMRecord record)
        {
            StringList ctx = new StringList();
            GKUtils.GetRecordContent(record, fContext.ShieldState, ctx);
            return ctx;
        }

        public string GetRecordName(GEDCOMRecord record, bool signed)
        {
            return GKUtils.GetRecordName(record, signed);
        }

        public bool RecordIsFiltered(GEDCOMRecord record)
        {
            bool result = false;
            if (record != null) {
                GKRecordsView rView = GetRecordsViewByType(record.RecordType);
                result = (rView != null && rView.IndexOfRecord(record) >= 0);
            }
            return result;
        }

        #endregion
    }
}
