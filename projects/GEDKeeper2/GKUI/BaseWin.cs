/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Dialogs;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class BaseWin : Form, IBaseWindow
    {
        private static readonly int NoteNameMaxLength = 64;

        #region Private fields
        
        /*private readonly ExtList<GEDCOMRecord> fLockedRecords;*/

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
            get { return this.fContext; }
        }

        public NavigationStack Navman
        {
            get { return this.fNavman; }
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
                if (this.fShieldState != value) {
                    this.fShieldState = value;
                    this.RefreshLists(false);
                }
            }
        }

        public GEDCOMTree Tree
        {
            get { return this.fTree; }
        }

        #endregion

        #region Instance control

        public BaseWin()
        {
            this.InitializeComponent();
            base.MdiParent = MainWin.Instance;

            /*this.fLockedRecords = new ExtList<GEDCOMRecord>();*/

            this.fTree = new GEDCOMTree();
            this.fContext = new BaseContext(this.fTree, this);

            this.fNavman = new NavigationStack();

            this.CreatePage(LangMan.LS(LSID.LSID_RPIndividuals), GEDCOMRecordType.rtIndividual, out this.ListPersons, out this.mPersonSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPFamilies), GEDCOMRecordType.rtFamily, out this.ListFamilies, out this.mFamilySummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPNotes), GEDCOMRecordType.rtNote, out this.ListNotes, out this.mNoteSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPMultimedia), GEDCOMRecordType.rtMultimedia, out this.ListMultimedia, out this.mMediaSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPSources), GEDCOMRecordType.rtSource, out this.ListSources, out this.mSourceSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPRepositories), GEDCOMRecordType.rtRepository, out this.ListRepositories, out this.mRepositorySummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPGroups), GEDCOMRecordType.rtGroup, out this.ListGroups, out this.mGroupSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPResearches), GEDCOMRecordType.rtResearch, out this.ListResearches, out this.mResearchSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPTasks), GEDCOMRecordType.rtTask, out this.ListTasks, out this.mTaskSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPCommunications), GEDCOMRecordType.rtCommunication, out this.ListCommunications, out this.mCommunicationSummary);
            this.CreatePage(LangMan.LS(LSID.LSID_RPLocations), GEDCOMRecordType.rtLocation, out this.ListLocations, out this.mLocationSummary);
            this.tabsRecords.SelectedIndex = 0;

            (this as ILocalization).SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                #if !__MonoCS__
                this.fNavman.Dispose();
                this.fTree.Dispose();
                this.fTree = null;

                /*this.fLockedRecords.Dispose();*/

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
            //
        }

        private void Form_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !this.CheckModified();

            if (!e.Cancel)
            {
                MainWin.Instance.BaseChanged(null);
                MainWin.Instance.CheckMRUWin(this.fTree.FileName, this);
            }
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                    /*case Keys.I:
						this.ItemAdd();
						break;
					case Keys.D:
						this.ItemDelete();
						break;*/

                case Keys.Return:
                    if (e.Control) {
                        this.RecordEdit(null, null);
                    }
                    break;

                    /*case Keys.F12:
                    throw new NotSupportedException(); // debug!*/

                    /*case Keys.F:
        			if (e.Control) {
        				this.QuickFind();
        			}
        			break;*/
            }
        }

        private void contextMenu_Opening(object sender, CancelEventArgs e)
        {
            GKRecordsView recView = contextMenu.SourceControl as GKRecordsView;

            this.miRecordDuplicate.Visible = (recView == this.ListPersons);
        }

        private void miRecordEdit_Click(object sender, EventArgs e)
        {
            this.RecordEdit(null, null);
        }

        private void miRecordDelete_Click(object sender, EventArgs e)
        {
            this.RecordDelete();
        }

        private void miRecordDuplicate_Click(object sender, EventArgs e)
        {
            this.RecordDuplicate();
        }

        #endregion

        #region Basic function

        public GEDCOMRecordType GetSelectedRecordType()
        {
            return (GEDCOMRecordType)(this.tabsRecords.SelectedIndex + 1);
        }

        public GKRecordsView GetRecordsViewByType(GEDCOMRecordType recType)
        {
            GKRecordsView list = null;

            switch (recType) {
                case GEDCOMRecordType.rtIndividual:
                    list = this.ListPersons;
                    break;

                case GEDCOMRecordType.rtFamily:
                    list = this.ListFamilies;
                    break;

                case GEDCOMRecordType.rtNote:
                    list = this.ListNotes;
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    list = this.ListMultimedia;
                    break;

                case GEDCOMRecordType.rtSource:
                    list = this.ListSources;
                    break;

                case GEDCOMRecordType.rtRepository:
                    list = this.ListRepositories;
                    break;

                case GEDCOMRecordType.rtGroup:
                    list = this.ListGroups;
                    break;

                case GEDCOMRecordType.rtResearch:
                    list = this.ListResearches;
                    break;

                case GEDCOMRecordType.rtTask:
                    list = this.ListTasks;
                    break;

                case GEDCOMRecordType.rtCommunication:
                    list = this.ListCommunications;
                    break;

                case GEDCOMRecordType.rtLocation:
                    list = this.ListLocations;
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
            GKRecordsView rView = this.GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.ListMan;
        }

        public GEDCOMRecord GetSelectedRecordEx()
        {
            GEDCOMRecordType rt = this.GetSelectedRecordType();
            GKRecordsView rView = this.GetRecordsViewByType(rt);
            return (rView == null) ? null : rView.GetSelectedRecord();
        }

        public GEDCOMIndividualRecord GetSelectedPerson()
        {
            return this.ListPersons.GetSelectedRecord() as GEDCOMIndividualRecord;
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (sender == null) return;

            GEDCOMRecord rec = ((GKRecordsView) sender).GetSelectedRecord();
            if (rec != null)
            {
                this.NavAdd(rec);
            }
            this.ShowRecordInfo(rec);
        }

        public List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType)
        {
            GKRecordsView rView = this.GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.GetContentList();
        }

        private void SetMainTitle()
        {
            this.Text = Path.GetFileName(this.Tree.FileName);
            if (this.fModified)
            {
                this.Text = @"* " + this.Text;
            }
        }

        private void mPersonSummaryLink(object sender, string linkName)
        {
            if (linkName.StartsWith("view_"))
            {
                string xref = linkName.Remove(0, 5);
                GEDCOMMultimediaRecord mmRec = this.fTree.XRefIndex_Find(xref) as GEDCOMMultimediaRecord;
                if (mmRec != null)
                {
                    this.ShowMedia(mmRec, false);
                }
            }
            else
            {
                this.SelectRecordByXRef(linkName);
            }
        }

        private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
        {
            MainWin.Instance.UpdateControls(false);
        }

        public void ApplyFilter()
        {
            if (this.fTree.RecordsCount > 0)
            {
                this.RefreshLists(false);
            }
        }

        public void ApplyFilter(GEDCOMRecordType recType)
        {
            if (this.fTree.RecordsCount > 0)
            {
                this.RefreshRecordsView(recType);
            }
        }

        public void ChangeRecord(GEDCOMRecord record)
        {
            if (record == null) return;

            record.ChangeDate.ChangeDateTime = DateTime.Now;

            this.Modified = true;
            this.fTree.Header.TransmissionDateTime = DateTime.Now;

            MainWin.Instance.NotifyRecord(this, record, RecordAction.raEdit);
        }

        public bool CheckModified()
        {
            bool result = true;

            if (this.Modified)
            {
                DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);

                switch (dialogResult) {
                    case DialogResult.Yes:
                        MainWin.Instance.miFileSaveClick(null, null);
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

        private void CreatePage(string pageText, GEDCOMRecordType recType, out GKRecordsView recView, out HyperView summary)
        {
            this.tabsRecords.SuspendLayout();
            TabPage sheet = new TabPage(pageText);
            this.tabsRecords.Controls.Add(sheet);
            this.tabsRecords.ResumeLayout(false);

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

            recView = GKUtils.CreateRecordsView(sheet, this.fTree, recType);
            recView.IsMainList = true;
            recView.DoubleClick += this.RecordEdit;
            recView.SelectedIndexChanged += this.List_SelectedIndexChanged;
            recView.UpdateTitles();
            recView.ContextMenuStrip = this.contextMenu;

            sheet.Controls.SetChildIndex(spl, 1);
            sheet.Controls.SetChildIndex(summary, 2);
        }

        private void ChangeFileName()
        {
            this.SetMainTitle();
            MainWin.Instance.Options.LastDir = Path.GetDirectoryName(this.fTree.FileName);
            MainWin.Instance.AddMRU(this.fTree.FileName);
        }

        public void Clear()
        {
            this.fNavman.Clear();
            this.fContext.Clear();
        }

        public bool IsUnknown()
        {
            string fileName = this.fTree.FileName;

            return string.IsNullOrEmpty(fileName) || !File.Exists(fileName);
        }

        public void FileNew()
        {
            this.Clear();
            this.RefreshLists(false);
            GKUtils.ShowPersonInfo(null, this.mPersonSummary.Lines, this.fShieldState);
            this.fTree.SetFileName(LangMan.LS(LSID.LSID_Unknown));
            this.Modified = false;
        }

        public void FileLoad(string fileName)
        {
            this.Clear();

            string pw = null;
            string ext = FileHelper.GetFileExtension(fileName);
            if (ext == ".geds") {
                if (!GKUtils.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                    GKUtils.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                    return;
                }
            }

            try
            {
                this.ProgressInit(LangMan.LS(LSID.LSID_Loading), 100);
                this.fTree.OnProgress += LoadProgress;
                try
                {
                    this.fContext.FileLoad(fileName, pw);
                }
                finally
                {
                    this.fTree.OnProgress -= LoadProgress;
                    this.ProgressDone();
                }

                TreeTools.CheckGEDCOMFormat(this.fTree, this.fContext.ValuesCollection, this);
                this.Modified = false;
            }
            catch (Exception ex)
            {
                this.Host.LogWrite("BaseWin.FileLoad(): " + ex.Message);
                GKUtils.ShowError(LangMan.LS(LSID.LSID_LoadGedComFailed));
            }

            this.ChangeFileName();
            this.RefreshLists(false);
            this.ShowTips();
        }

        public void FileSave(string fileName)
        {
            try
            {
                string pw = null;
                string ext = FileHelper.GetFileExtension(fileName);
                if (ext == ".geds") {
                    if (!GKUtils.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                        return;
                    }
                }

                this.fContext.FileSave(fileName, pw);
                this.Modified = false;

                this.ChangeFileName();
            }
            catch (UnauthorizedAccessException)
            {
                GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": access denied" }));
            }
            catch (Exception ex)
            {
                GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
                this.Host.LogWrite("BaseWin.FileSave(): " + ex.Message);
            }
        }

        public void CriticalSave()
        {
            try
            {
                string rfn = Path.ChangeExtension(this.fTree.FileName, ".restore");
                this.fTree.SaveToFile(rfn, GlobalOptions.Instance.DefCharacterSet);
            } catch (Exception ex) {
                this.Host.LogWrite("BaseWin.CriticalSave(): " + ex.Message);
            }
        }

        private GEDCOMFamilyRecord GetFamilyBySpouse(GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            int num = this.fTree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this.fTree[i];

                if (rec is GEDCOMFamilyRecord)
                {
                    GEDCOMFamilyRecord fam = rec as GEDCOMFamilyRecord;
                    GEDCOMIndividualRecord husb = fam.GetHusband();
                    GEDCOMIndividualRecord wife = fam.GetWife();
                    if (husb == newParent || wife == newParent)
                    {
                        string msg = string.Format(LangMan.LS(LSID.LSID_ParentsQuery), GKUtils.GetFamilyString(fam));
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

        public GEDCOMFamilyRecord GetChildFamily(GEDCOMIndividualRecord iChild, bool canCreate, GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            if (iChild != null)
            {
                if (iChild.ChildToFamilyLinks.Count != 0)
                {
                    result = iChild.ChildToFamilyLinks[0].Family;
                }
                else
                {
                    if (canCreate)
                    {
                        GEDCOMFamilyRecord fam = this.GetFamilyBySpouse(newParent);
                        if (fam == null)
                        {
                            fam = this.fTree.CreateFamily();
                        }
                        fam.AddChild(iChild);
                        result = fam;
                    }
                }
            }

            return result;
        }

        public GEDCOMFamilyRecord AddFamilyForSpouse(GEDCOMIndividualRecord spouse)
        {
            if (spouse == null)
                throw new ArgumentNullException("spouse");

            GEDCOMSex sex = spouse.Sex;
            if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined)
            {
                GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                return null;
            }

            GEDCOMFamilyRecord family = this.fTree.CreateFamily();
            family.AddSpouse(spouse);
            return family;
        }

        public GEDCOMIndividualRecord AddChildForParent(GEDCOMIndividualRecord parent, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord resultChild = null;

            if (parent != null)
            {
                if (parent.SpouseToFamilyLinks.Count > 1)
                {
                    GKUtils.ShowError(LangMan.LS(LSID.LSID_ThisPersonHasSeveralFamilies));
                }
                else
                {
                    GEDCOMFamilyRecord family;

                    if (parent.SpouseToFamilyLinks.Count == 0)
                    {
                        //GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotFamilies));

                        family = this.AddFamilyForSpouse(parent);
                        if (family == null) {
                            return null;
                        }
                    } else {
                        family = parent.SpouseToFamilyLinks[0].Family;
                    }

                    GEDCOMIndividualRecord child = this.SelectPerson(family.GetHusband(), TargetMode.tmParent, needSex);

                    if (child != null && family.AddChild(child))
                    {
                        // this repetition necessary, because the call of CreatePersonDialog only works if person already has a father,
                        // what to call AddChild () is no; all this is necessary in order to in the namebook were correct patronymics.
                        MainWin.Instance.NamesTable.ImportNames(child);

                        resultChild = child;
                    }
                }
            }

            return resultChild;
        }

        public GEDCOMIndividualRecord SelectSpouseFor(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GEDCOMSex needSex;
            switch (iRec.Sex) {
                case GEDCOMSex.svMale:
                    needSex = GEDCOMSex.svFemale;
                    break;
                case GEDCOMSex.svFemale:
                    needSex = GEDCOMSex.svMale;
                    break;
                default:
                    GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                    return null;
            }

            GEDCOMIndividualRecord target = null;
            TargetMode targetMode = TargetMode.tmNone;
            if (needSex == GEDCOMSex.svFemale) {
                target = iRec;
                targetMode = TargetMode.tmWife;
            }

            GEDCOMIndividualRecord result = this.SelectPerson(target, targetMode, needSex);
            return result;
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

        public void RefreshRecordsView(GEDCOMRecordType recType)
        {
            GKRecordsView rView = this.GetRecordsViewByType(recType);
            if (rView == null) return;

            rView.UpdateContents(this.fShieldState, false, -1);

            this.PageRecords_SelectedIndexChanged(null, null);
        }

        public void RecordNotify(GEDCOMRecord record, RecordAction notify)
        {
            if (record == null) return;
            
            GKRecordsView rView = GetRecordsViewByType(record.RecordType);

            if (rView != null && notify == RecordAction.raDelete)
            {
                rView.DeleteRecord(record);
                HyperView hView = GetHyperViewByType(record.RecordType);
                if ((null != hView) && (0 == rView.FilteredCount))
                {
                    hView.Lines.Clear();
                }
            }

            MainWin.Instance.NotifyRecord(this, record, notify);
        }

        public GEDCOMFamilyRecord SelectFamily(GEDCOMIndividualRecord target)
        {
            GEDCOMFamilyRecord result;

            try
            {
                using (RecordSelectDlg dlg = new RecordSelectDlg(this))
                {
                    dlg.Target = target;
                    dlg.NeedSex = GEDCOMSex.svNone;
                    dlg.TargetMode = TargetMode.tmChildToFamily;
                    dlg.Mode = GEDCOMRecordType.rtFamily;
                    if (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
                        result = (dlg.ResultRecord as GEDCOMFamilyRecord);
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                this.Host.LogWrite("BaseWin.SelectFamily(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public GEDCOMIndividualRecord SelectPerson(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord result;

            try
            {
                using (RecordSelectDlg dlg = new RecordSelectDlg(this))
                {
                    dlg.Target = target;
                    dlg.NeedSex = needSex;
                    dlg.TargetMode = targetMode;
                    dlg.Mode = GEDCOMRecordType.rtIndividual;
                    if (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
                        result = (dlg.ResultRecord as GEDCOMIndividualRecord);
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                this.Host.LogWrite("BaseWin.SelectPerson(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public GEDCOMRecord SelectRecord(GEDCOMRecordType mode, params object[] args)
        {
            GEDCOMRecord result;

            try
            {
                using (RecordSelectDlg dlg = new RecordSelectDlg(this))
                {
                    dlg.Mode = mode;

                    if (args != null && args.Length > 0) {
                        dlg.txtFastFilter.Text = (args[0] as string);
                    }

                    if (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK) {
                        result = dlg.ResultRecord;
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                this.Host.LogWrite("BaseWin.SelectRecord(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            GEDCOMRecordType rt = this.GetSelectedRecordType();
            IListManager listMan = this.GetRecordsListManByType(rt);
            if (listMan == null) return;

            switch (rt) {
                case GEDCOMRecordType.rtIndividual:
                    using (PersonsFilterDlg fmFilter = new PersonsFilterDlg(this, listMan)) {
                        DialogResult res = MainWin.Instance.ShowModalEx(fmFilter, false);
                        if (res == DialogResult.OK) this.ApplyFilter(rt);
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
                        if (res == DialogResult.OK) this.ApplyFilter(rt);
                    }
                    break;
            }
        }

        private void NavAdd(GEDCOMRecord aRec)
        {
            if (aRec != null && !this.fNavman.Busy)
            {
                this.fNavman.Current = aRec;
                MainWin.Instance.UpdateControls(false);
            }
        }

        public void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal)
        {
            if (mediaRec == null)
                throw new ArgumentNullException("mediaRec");

            GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);

            if (mmKind == MultimediaKind.mkAudio || mmKind == MultimediaKind.mkVideo) {
                // external files
                string targetFile = "";
                this.fContext.MediaLoad(fileRef, ref targetFile);
                SysUtils.LoadExtFile(targetFile);
            } else {
                MediaViewerWin mediaViewer = new MediaViewerWin(this);
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
        }

        public void ShowTips()
        {
            try
            {
                if (MainWin.Instance.Options.ShowTips)
                {
                    StringList birthDays = new StringList();
                    try
                    {
                        int num = this.fTree.RecordsCount;
                        for (int i = 0; i < num; i++)
                        {
                            GEDCOMRecord rec = this.fTree[i];
                            if (rec is GEDCOMIndividualRecord) {
                                GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
                                string nm = iRec.GetNameString(true, false);
                                string days = GKUtils.GetDaysForBirth(iRec);
                                // `days` always contains non-negative number.
                                if (!string.IsNullOrEmpty(days))
                                {
                                    uint daysBefore = uint.Parse(days);
                                    if (0 == daysBefore)
                                    {
                                        birthDays.Add(string.Format(
                                            LangMan.LS(LSID.LSID_BirthdayToday),
                                            nm));
                                    }
                                    else if (3 > daysBefore)
                                    {
                                        birthDays.Add(string.Format(
                                            LangMan.LS(LSID.LSID_DaysRemained),
                                            nm, days));
                                    }
                                }
                            }
                        }

                        if (birthDays.Count > 0) {
                            MainWin.Instance.Options.ShowTips = DayTipsDlg.ShowTipsEx(LangMan.LS(LSID.LSID_BirthDays), MainWin.Instance.Options.ShowTips, birthDays);
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
                this.Host.LogWrite("BaseWin.ShowTips(): " + ex.Message);
            }
        }

        #endregion
        
        #region Name and sex functions

        public string DefinePatronymic(string name, GEDCOMSex sex, bool confirm)
        {
            string result = "";

            NameEntry n = MainWin.Instance.NamesTable.FindName(name);
            if (n == null) {
                if (!confirm) {
                    return result;
                }

                n = MainWin.Instance.NamesTable.AddName(name);
            }

            switch (sex) {
                case GEDCOMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GEDCOMSex.svFemale:
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
                    case GEDCOMSex.svMale: {
                        result = n.M_Patronymic;
                        break;
                    }
                    case GEDCOMSex.svFemale: {
                        result = n.F_Patronymic;
                        break;
                    }
            }
            
            return result;
        }

        public GEDCOMSex DefineSex(string iName, string iPatr)
        {
            INamesTable namesTable = MainWin.Instance.NamesTable;
            GEDCOMSex result = namesTable.GetSexByName(iName);

            if (result == GEDCOMSex.svNone)
            {
                using (SexCheckDlg dlg = new SexCheckDlg())
                {
                    dlg.IndividualName = iName + " " + iPatr;
                    result = GlobalOptions.CurrentCulture.GetSex(iName, iPatr, false);

                    dlg.Sex = result;
                    if (dlg.ShowDialog() == DialogResult.OK)
                    {
                        result = dlg.Sex;

                        if (result != GEDCOMSex.svNone)
                        {
                            namesTable.SetNameSex(iName, result);
                        }
                    }
                }
            }

            return result;
        }

        public void CheckPersonSex(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            try {
                this.fContext.BeginUpdate();

                if (iRec.Sex == GEDCOMSex.svNone || iRec.Sex == GEDCOMSex.svUndetermined)
                {
                    string fFam, fName, fPatr;
                    GKUtils.GetNameParts(iRec, out fFam, out fName, out fPatr);
                    iRec.Sex = this.DefineSex(fName, fPatr);
                }
            } finally {
                this.fContext.EndUpdate();
            }
        }

        #endregion

        #region ILocalization implementation
        
        void ILocalization.SetLang()
        {
            this.tabsRecords.TabPages[ 0].Text = LangMan.LS(LSID.LSID_RPIndividuals);
            this.tabsRecords.TabPages[ 1].Text = LangMan.LS(LSID.LSID_RPFamilies);
            this.tabsRecords.TabPages[ 2].Text = LangMan.LS(LSID.LSID_RPNotes);
            this.tabsRecords.TabPages[ 3].Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.tabsRecords.TabPages[ 4].Text = LangMan.LS(LSID.LSID_RPSources);
            this.tabsRecords.TabPages[ 5].Text = LangMan.LS(LSID.LSID_RPRepositories);
            this.tabsRecords.TabPages[ 6].Text = LangMan.LS(LSID.LSID_RPGroups);
            this.tabsRecords.TabPages[ 7].Text = LangMan.LS(LSID.LSID_RPResearches);
            this.tabsRecords.TabPages[ 8].Text = LangMan.LS(LSID.LSID_RPTasks);
            this.tabsRecords.TabPages[ 9].Text = LangMan.LS(LSID.LSID_RPCommunications);
            this.tabsRecords.TabPages[10].Text = LangMan.LS(LSID.LSID_RPLocations);

            this.miRecordEdit.Text = LangMan.LS(LSID.LSID_MIRecordEdit);
            this.miRecordDelete.Text = LangMan.LS(LSID.LSID_MIRecordDelete);
            this.miRecordDuplicate.Text = LangMan.LS(LSID.LSID_RecordDuplicate);
        }
        
        #endregion

        #region IProgressController implementation
        
        public void ProgressInit(string title, int max)
        {
            ProgressController.ProgressInit(title, max);
        }

        public void ProgressDone()
        {
            ProgressController.ProgressDone();
        }

        public void ProgressStep()
        {
            ProgressController.ProgressStep();
        }

        public void ProgressStep(int value)
        {
            ProgressController.ProgressStep(value);
        }

        private void LoadProgress(object sender, int progress)
        {
            ProgressController.ProgressStep(progress);
        }

        #endregion

        #region IWorkWindow implementation
        
        string IWorkWindow.GetStatusString()
        {
            string res = "";

            GEDCOMRecordType rt = this.GetSelectedRecordType();
            GKRecordsView rView = this.GetRecordsViewByType(rt);

            if (rView != null)
            {
                res = LangMan.LS(LSID.LSID_SBRecords) + ": " + rView.TotalCount.ToString();
                res = res + ", " + LangMan.LS(LSID.LSID_SBFiltered) + ": " + rView.FilteredCount.ToString();
            }

            return res;
        }

        void IWorkWindow.UpdateView()
        {
            this.RefreshLists(true);
        }

        void IWorkWindow.NavNext()
        {
            this.fNavman.BeginNav();
            try
            {
                GEDCOMRecord rec = this.fNavman.Next() as GEDCOMRecord;
                if (rec != null)
                {
                    this.SelectRecordByXRef(rec.XRef);
                    MainWin.Instance.UpdateControls(false);
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
                GEDCOMRecord rec = this.fNavman.Back() as GEDCOMRecord;
                if (rec != null)
                {
                    this.SelectRecordByXRef(rec.XRef);
                    MainWin.Instance.UpdateControls(false);
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

        public bool AllowQuickFind()
        {
            return true;
        }

        IList<ISearchResult> IWorkWindow.FindAll(string searchPattern)
        {
            List<ISearchResult> result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = this.fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = this.fTree[i];

                if (rec.RecordType == GEDCOMRecordType.rtIndividual) {
                    GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;

                    string fullname = iRec.GetNameString(true, false);
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
            if (iRec == null) {
                throw new ArgumentNullException("iRec");
            }

            // platform: In Windows works without it
            #if __MonoCS__
            this.Activate();
            #endif

            this.SelectRecordByXRef(iRec.XRef);
        }

        void IWorkWindow.QuickFind()
        {
            SearchPanel panel = new SearchPanel(this);

            Rectangle client = this.ClientRectangle;
            Point pt = this.PointToScreen(new Point(client.Left, client.Bottom - panel.Height));
            panel.Location = pt;

            panel.Show();
        }

        #endregion

        #region Record Management

        public void RecordDuplicate()
        {
            GEDCOMRecord source = this.GetSelectedRecordEx();
            if (source == null) return;
            if (source.RecordType != GEDCOMRecordType.rtIndividual) return;

            GKUtils.ShowWarning(LangMan.LS(LSID.LSID_DuplicateWarning));

            GEDCOMIndividualRecord target;
            try {
                this.fContext.BeginUpdate();

                target = this.fContext.Tree.CreateIndividual();
                target.Assign(source);

                this.ChangeRecord(target);
            } finally {
                this.fContext.EndUpdate();
            }

            this.RefreshLists(false);
            this.SelectRecordByXRef(target.XRef);
        }

        public void RecordAdd()
        {
            bool result = false;

            GEDCOMRecord rec = null;
            GEDCOMRecordType rt = this.GetSelectedRecordType();

            switch (rt)
            {
                case GEDCOMRecordType.rtIndividual:
                    {
                        rec = this.CreatePersonDialog(null, TargetMode.tmParent, GEDCOMSex.svNone);
                        result = (rec != null);
                        break;
                    }
                case GEDCOMRecordType.rtFamily:
                    {
                        GEDCOMFamilyRecord fam = null;
                        result = this.ModifyFamily(ref fam, FamilyTarget.None, null);
                        rec = fam;
                        break;
                    }
                case GEDCOMRecordType.rtNote:
                    {
                        GEDCOMNoteRecord note = null;
                        result = this.ModifyNote(ref note);
                        rec = note;
                        break;
                    }
                case GEDCOMRecordType.rtMultimedia:
                    {
                        GEDCOMMultimediaRecord mmRec = null;
                        result = this.ModifyMedia(ref mmRec);
                        rec = mmRec;
                        break;
                    }
                case GEDCOMRecordType.rtSource:
                    {
                        GEDCOMSourceRecord src = null;
                        result = this.ModifySource(ref src);
                        rec = src;
                        break;
                    }
                case GEDCOMRecordType.rtRepository:
                    {
                        GEDCOMRepositoryRecord rep = null;
                        result = this.ModifyRepository(ref rep);
                        rec = rep;
                        break;
                    }
                case GEDCOMRecordType.rtGroup:
                    {
                        GEDCOMGroupRecord grp = null;
                        result = this.ModifyGroup(ref grp);
                        rec = grp;
                        break;
                    }
                case GEDCOMRecordType.rtResearch:
                    {
                        GEDCOMResearchRecord rsr = null;
                        result = this.ModifyResearch(ref rsr);
                        rec = rsr;
                        break;
                    }
                case GEDCOMRecordType.rtTask:
                    {
                        GEDCOMTaskRecord tsk = null;
                        result = this.ModifyTask(ref tsk);
                        rec = tsk;
                        break;
                    }
                case GEDCOMRecordType.rtCommunication:
                    {
                        GEDCOMCommunicationRecord comm = null;
                        result = this.ModifyCommunication(ref comm);
                        rec = comm;
                        break;
                    }
                case GEDCOMRecordType.rtLocation:
                    {
                        GEDCOMLocationRecord loc = null;
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
                        msg = string.Format(LangMan.LS(LSID.LSID_PersonDeleteQuery), ((GEDCOMIndividualRecord)record).GetNameString(true, false));
                        break;

                    case GEDCOMRecordType.rtFamily:
                        msg = string.Format(LangMan.LS(LSID.LSID_FamilyDeleteQuery), GKUtils.GetFamilyString((GEDCOMFamilyRecord)record));
                        break;

                    case GEDCOMRecordType.rtNote:
                        {
                            string value = GKUtils.TruncateStrings(((GEDCOMNoteRecord) (record)).Note, NoteNameMaxLength);
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

                if (confirm && GKUtils.ShowQuestion(msg) != DialogResult.Yes)
                    return false;

                this.RecordNotify(record, RecordAction.raDelete);

                result = this.fContext.DeleteRecord(record);

                if (result) {
                    this.Modified = true;
                    this.fTree.Header.TransmissionDateTime = DateTime.Now;
                }
            }

            return result;
        }

        public void RecordDelete()
        {
            GEDCOMRecord record = this.GetSelectedRecordEx();
            if (record == null) return;

            bool result = this.RecordDelete(record, true);

            if (result) {
                this.RefreshLists(false);
            }
        }

        public void RecordEdit(object sender, EventArgs e)
        {
            GEDCOMRecord rec = this.GetSelectedRecordEx();
            if (rec == null) return;

            bool result = false;

            switch (rec.RecordType) {
                case GEDCOMRecordType.rtIndividual:
                    GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
                    result = this.ModifyPerson(ref ind);
                    break;

                case GEDCOMRecordType.rtFamily:
                    GEDCOMFamilyRecord fam = rec as GEDCOMFamilyRecord;
                    result = this.ModifyFamily(ref fam, FamilyTarget.None, null);
                    break;

                case GEDCOMRecordType.rtNote:
                    GEDCOMNoteRecord note = rec as GEDCOMNoteRecord;
                    result = this.ModifyNote(ref note);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    GEDCOMMultimediaRecord mmRec = rec as GEDCOMMultimediaRecord;
                    result = this.ModifyMedia(ref mmRec);
                    break;

                case GEDCOMRecordType.rtSource:
                    GEDCOMSourceRecord src = rec as GEDCOMSourceRecord;
                    result = this.ModifySource(ref src);
                    break;

                case GEDCOMRecordType.rtRepository:
                    GEDCOMRepositoryRecord rep = rec as GEDCOMRepositoryRecord;
                    result = this.ModifyRepository(ref rep);
                    break;

                case GEDCOMRecordType.rtGroup:
                    GEDCOMGroupRecord grp = rec as GEDCOMGroupRecord;
                    result = this.ModifyGroup(ref grp);
                    break;

                case GEDCOMRecordType.rtResearch:
                    GEDCOMResearchRecord rsr = rec as GEDCOMResearchRecord;
                    result = this.ModifyResearch(ref rsr);
                    break;

                case GEDCOMRecordType.rtTask:
                    GEDCOMTaskRecord tsk = rec as GEDCOMTaskRecord;
                    result = this.ModifyTask(ref tsk);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    GEDCOMCommunicationRecord comm = rec as GEDCOMCommunicationRecord;
                    result = this.ModifyCommunication(ref comm);
                    break;

                case GEDCOMRecordType.rtLocation:
                    GEDCOMLocationRecord loc = rec as GEDCOMLocationRecord;
                    result = this.ModifyLocation(ref loc);
                    break;
            }

            if (result)
            {
                this.RefreshLists(false);
                this.ShowRecordInfo(rec);
            }
        }

        public void ShowRecordsTab(GEDCOMRecordType recType)
        {
            this.tabsRecords.SelectedIndex = (int)recType - 1;
            this.PageRecords_SelectedIndexChanged(null, null);
        }

        public void SelectRecordByXRef(string xref)
        {
            GEDCOMRecord record = this.fTree.XRefIndex_Find(xref);
            if (record == null) return;

            GKRecordsView rView = this.GetRecordsViewByType(record.RecordType);
            if (rView == null) return;

            this.ShowRecordsTab(record.RecordType);
            this.ActiveControl = rView;
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
                        GKUtils.ShowPersonInfo(record as GEDCOMIndividualRecord, this.mPersonSummary.Lines, this.fShieldState);
                        break;

                    case GEDCOMRecordType.rtFamily:
                        GKUtils.ShowFamilyInfo(record as GEDCOMFamilyRecord, this.mFamilySummary.Lines, this.fShieldState);
                        break;

                    case GEDCOMRecordType.rtNote:
                        GKUtils.ShowNoteInfo(record as GEDCOMNoteRecord, this.mNoteSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtMultimedia:
                        GKUtils.ShowMultimediaInfo(record as GEDCOMMultimediaRecord, this.mMediaSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtSource:
                        GKUtils.ShowSourceInfo(record as GEDCOMSourceRecord, this.mSourceSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtRepository:
                        GKUtils.ShowRepositoryInfo(record as GEDCOMRepositoryRecord, this.mRepositorySummary.Lines);
                        break;

                    case GEDCOMRecordType.rtGroup:
                        GKUtils.ShowGroupInfo(record as GEDCOMGroupRecord, this.mGroupSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtResearch:
                        GKUtils.ShowResearchInfo(record as GEDCOMResearchRecord, this.mResearchSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtTask:
                        GKUtils.ShowTaskInfo(record as GEDCOMTaskRecord, this.mTaskSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtCommunication:
                        GKUtils.ShowCommunicationInfo(record as GEDCOMCommunicationRecord, this.mCommunicationSummary.Lines);
                        break;

                    case GEDCOMRecordType.rtLocation:
                        GKUtils.ShowLocationInfo(record as GEDCOMLocationRecord, this.mLocationSummary.Lines);
                        break;
                }
            }
            catch (Exception ex)
            {
                this.Host.LogWrite("BaseWin.ShowRecordInfo(): " + ex.Message);
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
                            GKUtils.ShowPersonInfo(record as GEDCOMIndividualRecord, ctx, this.fShieldState);
                            break;

                        case GEDCOMRecordType.rtFamily:
                            GKUtils.ShowFamilyInfo(record as GEDCOMFamilyRecord, ctx, this.fShieldState);
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
                    this.Host.LogWrite("BaseWin.GetRecordContext(): " + ex.Message);
                }
            }

            return ctx;
        }

        public bool RecordIsFiltered(GEDCOMRecord record)
        {
            bool result = false;
            if (record != null)
            {
                GKRecordsView rView = this.GetRecordsViewByType(record.RecordType);
                result = (rView != null && rView.IndexOfRecord(record) >= 0);
            }
            return result;
        }

        #endregion

        #region Modify routines

        public bool ModifyMedia(ref GEDCOMMultimediaRecord mediaRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (MediaEditDlg dlg = new MediaEditDlg(this))
                {
                    bool exists = mediaRec != null;
                    if (!exists) {
                        mediaRec = new GEDCOMMultimediaRecord(this.fTree, this.fTree, "", "");
                        mediaRec.FileReferences.Add(new GEDCOMFileReferenceWithTitle(this.fTree, mediaRec, "", ""));
                        mediaRec.InitNew();
                    }

                    dlg.MediaRec = mediaRec;

                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(mediaRec);
                        } else {
                            mediaRec.Dispose();
                            mediaRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyNote(ref GEDCOMNoteRecord noteRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (NoteEditDlg dlg = new NoteEditDlg(this))
                {
                    bool exists = noteRec != null;
                    if (!exists) {
                        noteRec = new GEDCOMNoteRecord(this.fTree, this.fTree, "", "");
                        noteRec.InitNew();
                    }

                    dlg.NoteRecord = noteRec;

                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(noteRec);
                        } else {
                            noteRec.Dispose();
                            noteRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifySource(ref GEDCOMSourceRecord sourceRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (SourceEditDlg fmSrcEdit = new SourceEditDlg(this))
                {
                    bool exists = sourceRec != null;
                    if (!exists) {
                        sourceRec = new GEDCOMSourceRecord(this.fTree, this.fTree, "", "");
                        sourceRec.InitNew();
                    }

                    fmSrcEdit.SourceRecord = sourceRec;

                    result = (MainWin.Instance.ShowModalEx(fmSrcEdit, false) == DialogResult.OK);
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(sourceRec);
                        } else {
                            sourceRec.Dispose();
                            sourceRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifySourceCitation(IGEDCOMStructWithLists _struct, ref GEDCOMSourceCitation cit)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (SourceCitEditDlg fmSrcCitEdit = new SourceCitEditDlg(this))
                {
                    bool exists = cit != null;
                    if (!exists) {
                        cit = new GEDCOMSourceCitation(this.Tree, _struct as GEDCOMObject, "", "");
                    }

                    fmSrcCitEdit.SourceCitation = cit;

                    result = MainWin.Instance.ShowModalEx(fmSrcCitEdit, false) == DialogResult.OK;
                    if (!exists) {
                        if (result) {
                            _struct.SourceCitations.Add(cit);
                        } else {
                            cit.Dispose();
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyRepository(ref GEDCOMRepositoryRecord repRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (RepositoryEditDlg fmRepEdit = new RepositoryEditDlg(this))
                {
                    bool exists = repRec != null;
                    if (!exists) {
                        repRec = new GEDCOMRepositoryRecord(this.fTree, this.fTree, "", "");
                        repRec.InitNew();
                    }

                    fmRepEdit.Repository = repRec;

                    result = MainWin.Instance.ShowModalEx(fmRepEdit, false) == DialogResult.OK;
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(repRec);
                        } else {
                            repRec.Dispose();
                            repRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyGroup(ref GEDCOMGroupRecord groupRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (GroupEditDlg fmGrpEdit = new GroupEditDlg(this))
                {
                    bool exists = groupRec != null;
                    if (!exists) {
                        groupRec = new GEDCOMGroupRecord(this.fTree, this.fTree, "", "");
                        groupRec.InitNew();
                    }

                    fmGrpEdit.Group = groupRec;

                    result = (MainWin.Instance.ShowModalEx(fmGrpEdit, false) == DialogResult.OK);
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(groupRec);
                        } else {
                            groupRec.Dispose();
                            groupRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyResearch(ref GEDCOMResearchRecord researchRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (ResearchEditDlg fmResEdit = new ResearchEditDlg(this))
                {
                    bool exists = researchRec != null;
                    if (!exists) {
                        researchRec = new GEDCOMResearchRecord(this.fTree, this.fTree, "", "");
                        researchRec.InitNew();
                    }

                    fmResEdit.Research = researchRec;

                    result = MainWin.Instance.ShowModalEx(fmResEdit, false) == DialogResult.OK;
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(researchRec);
                        } else {
                            researchRec.Dispose();
                            researchRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyTask(ref GEDCOMTaskRecord taskRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (TaskEditDlg fmTaskEdit = new TaskEditDlg(this))
                {
                    bool exists = taskRec != null;
                    if (!exists) {
                        taskRec = new GEDCOMTaskRecord(this.fTree, this.fTree, "", "");
                        taskRec.InitNew();
                    }

                    fmTaskEdit.Task = taskRec;

                    result = MainWin.Instance.ShowModalEx(fmTaskEdit, false) == DialogResult.OK;
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(taskRec);
                        } else {
                            taskRec.Dispose();
                            taskRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyCommunication(ref GEDCOMCommunicationRecord commRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (CommunicationEditDlg fmCorrEdit = new CommunicationEditDlg(this))
                {
                    bool exists = commRec != null;
                    if (!exists) {
                        commRec = new GEDCOMCommunicationRecord(this.fTree, this.fTree, "", "");
                        commRec.InitNew();
                    }

                    fmCorrEdit.Communication = commRec;

                    result = MainWin.Instance.ShowModalEx(fmCorrEdit, false) == DialogResult.OK;
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(commRec);
                        } else {
                            commRec.Dispose();
                            commRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyLocation(ref GEDCOMLocationRecord locRec)
        {
            bool result = false;

            try {
                this.fContext.BeginUpdate();

                using (LocationEditDlg fmLocEdit = new LocationEditDlg(this))
                {
                    bool exists = locRec != null;
                    if (!exists) {
                        locRec = new GEDCOMLocationRecord(this.fTree, this.fTree, "", "");
                        locRec.InitNew();
                    }

                    fmLocEdit.LocationRecord = locRec;

                    result = MainWin.Instance.ShowModalEx(fmLocEdit, false) == DialogResult.OK;
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(locRec);
                        } else {
                            locRec.Dispose();
                            locRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyName(ref NameEntry aName)
        {
            bool result;

            try {
                this.fContext.BeginUpdate();

                using (NameEditDlg dlg = new NameEditDlg(this)) {
                    dlg.IName = aName;
                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public GEDCOMIndividualRecord CreatePersonDialog(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord result = null;

            try {
                this.fContext.BeginUpdate();

                using (PersonNewDlg dlg = new PersonNewDlg(this))
                {
                    dlg.cmbSex.SelectedIndex = (int)needSex;
                    dlg.TargetMode = targetMode;
                    dlg.Target = target;

                    if (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK)
                    {
                        result = this.fContext.CreatePersonEx(dlg.txtName.Text, dlg.cmbPatronymic.Text, dlg.txtSurname.Text, (GEDCOMSex)dlg.cmbSex.SelectedIndex, true);
                        this.ChangeRecord(result);

                        MainWin.Instance.NamesTable.ImportNames(result);

                        IndividualListFilter iFilter = (IndividualListFilter)this.ListPersons.ListMan.Filter;

                        if (iFilter.SourceMode == FilterGroupMode.Selected)
                        {
                            GEDCOMSourceRecord src = this.fTree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                            if (src != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_IncludedSourceFilter)) == DialogResult.Yes)
                            {
                                result.AddSource(src, "", 0);
                            }
                        }

                        if (iFilter.FilterGroupMode == FilterGroupMode.Selected)
                        {
                            GEDCOMGroupRecord grp = this.fTree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                            if (grp != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_IncludedGroupFilter)) == DialogResult.Yes)
                            {
                                grp.AddMember(result);
                            }
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyPerson(ref GEDCOMIndividualRecord indivRec)
        {
            bool result = false;

            if (indivRec != null)
            {
                try {
                    this.fContext.BeginUpdate();

                    using (PersonEditDlg dlg = new PersonEditDlg(this)) {
                        dlg.Person = indivRec;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    }
                } finally {
                    this.fContext.EndUpdate();
                }
            }

            return result;
        }

        public bool ModifyFamily(ref GEDCOMFamilyRecord familyRec, FamilyTarget target, GEDCOMIndividualRecord person)
        {
            bool result;

            try {
                this.fContext.BeginUpdate();

                if (target == FamilyTarget.Spouse && person != null) {
                    GEDCOMSex sex = person.Sex;
                    if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined) {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                        return false;
                    }
                }

                using (FamilyEditDlg dlg = new FamilyEditDlg(this)) {
                    bool exists = (familyRec != null);
                    if (!exists) {
                        familyRec = new GEDCOMFamilyRecord(this.fTree, this.fTree, "", "");
                        familyRec.InitNew();
                    }

                    if (target == FamilyTarget.Spouse && person != null) {
                        familyRec.AddSpouse(person);
                    } else if (target == FamilyTarget.Child && person != null) {
                        familyRec.AddChild(person);
                    }

                    dlg.Family = familyRec;

                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    if (!exists) {
                        if (result) {
                            this.fTree.AddRecord(familyRec);
                        } else {
                            GEDCOMUtils.CleanFamily(familyRec);
                            familyRec.Dispose();
                            familyRec = null;
                        }
                    }
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyAddress(GEDCOMAddress address)
        {
            bool result;

            try {
                this.fContext.BeginUpdate();

                using (AddressEditDlg dlg = new AddressEditDlg(this)) {
                    dlg.Address = address;
                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                }
            } finally {
                this.fContext.EndUpdate();
            }

            return result;
        }

        #endregion
    }
}
