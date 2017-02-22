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

using Externals;
using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
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
            //
        }

        private void Form_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !CheckModified();
            if (e.Cancel) return;

            MainWin.Instance.BaseClosed(this);
            MainWin.Instance.CheckMRUWin(fTree.FileName, this);
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

            recView = GKUtils.CreateRecordsView(sheet, fTree, recType);
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
                if (!GKUtils.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                    GKUtils.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                    return;
                }
            }

            try
            {
                ProgressInit(LangMan.LS(LSID.LSID_Loading), 100);
                fTree.OnProgress += LoadProgress;
                try
                {
                    fContext.FileLoad(fileName, pw);
                }
                finally
                {
                    fTree.OnProgress -= LoadProgress;
                    ProgressDone();
                }

                TreeTools.CheckGEDCOMFormat(fTree, fContext.ValuesCollection, this);
                Modified = false;
            }
            catch (Exception ex)
            {
                Host.LogWrite("BaseWin.FileLoad(): " + ex.Message);
                GKUtils.ShowError(LangMan.LS(LSID.LSID_LoadGedComFailed));
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
                    if (!GKUtils.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                        return;
                    }
                }

                fContext.FileSave(fileName, pw);
                Modified = false;

                ChangeFileName();
            }
            catch (UnauthorizedAccessException)
            {
                GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": access denied" }));
            }
            catch (Exception ex)
            {
                GKUtils.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
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

        private GEDCOMFamilyRecord GetFamilyBySpouse(GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fTree[i];

                if (rec.RecordType == GEDCOMRecordType.rtFamily)
                {
                    GEDCOMFamilyRecord fam = (GEDCOMFamilyRecord) rec;
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
                        GEDCOMFamilyRecord fam = GetFamilyBySpouse(newParent);
                        if (fam == null)
                        {
                            fam = fTree.CreateFamily();
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

            GEDCOMFamilyRecord family = fTree.CreateFamily();
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

                        family = AddFamilyForSpouse(parent);
                        if (family == null) {
                            return null;
                        }
                    } else {
                        family = parent.SpouseToFamilyLinks[0].Family;
                    }

                    GEDCOMIndividualRecord child = SelectPerson(family.GetHusband(), TargetMode.tmParent, needSex);

                    if (child != null && family.AddChild(child))
                    {
                        // this repetition necessary, because the call of CreatePersonDialog only works if person already has a father,
                        // what to call AddChild () is no; all this is necessary in order to in the namebook were correct patronymics.
                        ImportNames(child);

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

            GEDCOMIndividualRecord result = SelectPerson(target, targetMode, needSex);
            return result;
        }

        public void RefreshLists(bool titles)
        {
            ListPersons.UpdateContents(fShieldState, titles, 2);
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
                Host.LogWrite("BaseWin.SelectFamily(): " + ex.Message);
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
                Host.LogWrite("BaseWin.SelectPerson(): " + ex.Message);
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
                Host.LogWrite("BaseWin.SelectRecord(): " + ex.Message);
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

                        uint days;
                        if (GKUtils.GetDaysForBirth(iRec, out days))
                        {
                            string nm = GKUtils.GetNameString(iRec, true, false);
                            nm = fContext.Culture.GetPossessiveName(nm);

                            if (3 > days) {
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

        #endregion

        #region Name and sex functions

        // may be move to host (MainWin)?
        public void ImportNames(GEDCOMIndividualRecord iRec)
        {
            if (MainWin.Instance == null) return;

            INamesTable namesTable = MainWin.Instance.NamesTable;
            if (namesTable == null) return;

            namesTable.ImportNames(iRec);
        }

        public string DefinePatronymic(string name, GEDCOMSex sex, bool confirm)
        {
            ICulture culture = fContext.Culture;
            if (!culture.HasPatronymic()) return string.Empty;

            string result = "";

            INamesTable namesTable = MainWin.Instance.NamesTable;

            NameEntry n = namesTable.FindName(name);
            if (n == null) {
                if (!confirm) {
                    return result;
                }

                n = namesTable.AddName(name);
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

                ModifyName(ref n);
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
            //ICulture culture = fContext.Culture;
            INamesTable namesTable = MainWin.Instance.NamesTable;

            GEDCOMSex result = namesTable.GetSexByName(iName);

            if (result == GEDCOMSex.svNone)
            {
                using (SexCheckDlg dlg = new SexCheckDlg())
                {
                    dlg.IndividualName = iName + " " + iPatr;
                    result = fContext.Culture.GetSex(iName, iPatr, false);

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
                fContext.BeginUpdate();

                if (iRec.Sex == GEDCOMSex.svNone || iRec.Sex == GEDCOMSex.svUndetermined)
                {
                    string fFam, fName, fPatr;
                    GKUtils.GetNameParts(iRec, out fFam, out fName, out fPatr);
                    iRec.Sex = DefineSex(fName, fPatr);
                }
            } finally {
                fContext.EndUpdate();
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

            GKUtils.ShowWarning(LangMan.LS(LSID.LSID_DuplicateWarning));

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
                        result = ModifyPerson(ref indivRec, null, TargetMode.tmParent, GEDCOMSex.svNone);
                        rec = indivRec;
                        break;
                    }

                case GEDCOMRecordType.rtFamily:
                    {
                        GEDCOMFamilyRecord fam = null;
                        result = ModifyFamily(ref fam, FamilyTarget.None, null);
                        rec = fam;
                        break;
                    }
                case GEDCOMRecordType.rtNote:
                    {
                        GEDCOMNoteRecord note = null;
                        result = ModifyNote(ref note);
                        rec = note;
                        break;
                    }
                case GEDCOMRecordType.rtMultimedia:
                    {
                        GEDCOMMultimediaRecord mmRec = null;
                        result = ModifyMedia(ref mmRec);
                        rec = mmRec;
                        break;
                    }
                case GEDCOMRecordType.rtSource:
                    {
                        GEDCOMSourceRecord src = null;
                        result = ModifySource(ref src);
                        rec = src;
                        break;
                    }
                case GEDCOMRecordType.rtRepository:
                    {
                        GEDCOMRepositoryRecord rep = null;
                        result = ModifyRepository(ref rep);
                        rec = rep;
                        break;
                    }
                case GEDCOMRecordType.rtGroup:
                    {
                        GEDCOMGroupRecord grp = null;
                        result = ModifyGroup(ref grp);
                        rec = grp;
                        break;
                    }
                case GEDCOMRecordType.rtResearch:
                    {
                        GEDCOMResearchRecord rsr = null;
                        result = ModifyResearch(ref rsr);
                        rec = rsr;
                        break;
                    }
                case GEDCOMRecordType.rtTask:
                    {
                        GEDCOMTaskRecord tsk = null;
                        result = ModifyTask(ref tsk);
                        rec = tsk;
                        break;
                    }
                case GEDCOMRecordType.rtCommunication:
                    {
                        GEDCOMCommunicationRecord comm = null;
                        result = ModifyCommunication(ref comm);
                        rec = comm;
                        break;
                    }
                case GEDCOMRecordType.rtLocation:
                    {
                        GEDCOMLocationRecord loc = null;
                        result = ModifyLocation(ref loc);
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

                if (confirm && GKUtils.ShowQuestion(msg) != DialogResult.Yes)
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
                    result = ModifyPerson(ref ind, null, TargetMode.tmNone, GEDCOMSex.svNone);
                    break;

                case GEDCOMRecordType.rtFamily:
                    GEDCOMFamilyRecord fam = rec as GEDCOMFamilyRecord;
                    result = ModifyFamily(ref fam, FamilyTarget.None, null);
                    break;

                case GEDCOMRecordType.rtNote:
                    GEDCOMNoteRecord note = rec as GEDCOMNoteRecord;
                    result = ModifyNote(ref note);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    GEDCOMMultimediaRecord mmRec = rec as GEDCOMMultimediaRecord;
                    result = ModifyMedia(ref mmRec);
                    break;

                case GEDCOMRecordType.rtSource:
                    GEDCOMSourceRecord src = rec as GEDCOMSourceRecord;
                    result = ModifySource(ref src);
                    break;

                case GEDCOMRecordType.rtRepository:
                    GEDCOMRepositoryRecord rep = rec as GEDCOMRepositoryRecord;
                    result = ModifyRepository(ref rep);
                    break;

                case GEDCOMRecordType.rtGroup:
                    GEDCOMGroupRecord grp = rec as GEDCOMGroupRecord;
                    result = ModifyGroup(ref grp);
                    break;

                case GEDCOMRecordType.rtResearch:
                    GEDCOMResearchRecord rsr = rec as GEDCOMResearchRecord;
                    result = ModifyResearch(ref rsr);
                    break;

                case GEDCOMRecordType.rtTask:
                    GEDCOMTaskRecord tsk = rec as GEDCOMTaskRecord;
                    result = ModifyTask(ref tsk);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    GEDCOMCommunicationRecord comm = rec as GEDCOMCommunicationRecord;
                    result = ModifyCommunication(ref comm);
                    break;

                case GEDCOMRecordType.rtLocation:
                    GEDCOMLocationRecord loc = rec as GEDCOMLocationRecord;
                    result = ModifyLocation(ref loc);
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
                GKUtils.ShowWarning(LangMan.LS(LSID.LSID_RecordIsLocked));
            }

            return result;
        }

        public bool ModifyMedia(ref GEDCOMMultimediaRecord mediaRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (MediaEditDlg dlg = new MediaEditDlg(this))
                {
                    bool exists = mediaRec != null;
                    if (!exists) {
                        mediaRec = new GEDCOMMultimediaRecord(fTree, fTree, "", "");
                        mediaRec.FileReferences.Add(new GEDCOMFileReferenceWithTitle(fTree, mediaRec, "", ""));
                        mediaRec.InitNew();
                    }

                    try {
                        LockRecord(mediaRec);

                        dlg.MediaRec = mediaRec;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    } finally {
                        UnlockRecord(mediaRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(mediaRec);
                        } else {
                            mediaRec.Dispose();
                            mediaRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyNote(ref GEDCOMNoteRecord noteRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (NoteEditDlg dlg = new NoteEditDlg(this))
                {
                    bool exists = noteRec != null;
                    if (!exists) {
                        noteRec = new GEDCOMNoteRecord(fTree, fTree, "", "");
                        noteRec.InitNew();
                    }

                    try {
                        LockRecord(noteRec);

                        dlg.NoteRecord = noteRec;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    } finally {
                        UnlockRecord(noteRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(noteRec);
                        } else {
                            noteRec.Dispose();
                            noteRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifySource(ref GEDCOMSourceRecord sourceRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (SourceEditDlg fmSrcEdit = new SourceEditDlg(this))
                {
                    bool exists = sourceRec != null;
                    if (!exists) {
                        sourceRec = new GEDCOMSourceRecord(fTree, fTree, "", "");
                        sourceRec.InitNew();
                    }

                    try {
                        LockRecord(sourceRec);

                        fmSrcEdit.SourceRecord = sourceRec;
                        result = (MainWin.Instance.ShowModalEx(fmSrcEdit, false) == DialogResult.OK);
                    } finally {
                        UnlockRecord(sourceRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(sourceRec);
                        } else {
                            sourceRec.Dispose();
                            sourceRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifySourceCitation(ChangeTracker undoman, IGEDCOMStructWithLists _struct, ref GEDCOMSourceCitation cit)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (SourceCitEditDlg fmSrcCitEdit = new SourceCitEditDlg(this))
                {
                    bool exists = cit != null;
                    if (!exists) {
                        cit = new GEDCOMSourceCitation(Tree, _struct as GEDCOMObject, "", "");
                    }

                    fmSrcCitEdit.SourceCitation = cit;
                    result = MainWin.Instance.ShowModalEx(fmSrcCitEdit, false) == DialogResult.OK;

                    if (!exists) {
                        if (result) {
                            //_struct.SourceCitations.Add(cit);
                            result = undoman.DoOrdinaryOperation(OperationType.otRecordSourceCitAdd, (GEDCOMObject)_struct, cit);
                        } else {
                            cit.Dispose();
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyRepository(ref GEDCOMRepositoryRecord repRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (RepositoryEditDlg fmRepEdit = new RepositoryEditDlg(this))
                {
                    bool exists = repRec != null;
                    if (!exists) {
                        repRec = new GEDCOMRepositoryRecord(fTree, fTree, "", "");
                        repRec.InitNew();
                    }

                    try {
                        LockRecord(repRec);

                        fmRepEdit.Repository = repRec;
                        result = MainWin.Instance.ShowModalEx(fmRepEdit, false) == DialogResult.OK;
                    } finally {
                        UnlockRecord(repRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(repRec);
                        } else {
                            repRec.Dispose();
                            repRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyGroup(ref GEDCOMGroupRecord groupRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (GroupEditDlg fmGrpEdit = new GroupEditDlg(this))
                {
                    bool exists = groupRec != null;
                    if (!exists) {
                        groupRec = new GEDCOMGroupRecord(fTree, fTree, "", "");
                        groupRec.InitNew();
                    }

                    try {
                        LockRecord(groupRec);

                        fmGrpEdit.Group = groupRec;
                        result = (MainWin.Instance.ShowModalEx(fmGrpEdit, false) == DialogResult.OK);
                    } finally {
                        UnlockRecord(groupRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(groupRec);
                        } else {
                            groupRec.Dispose();
                            groupRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyResearch(ref GEDCOMResearchRecord researchRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (ResearchEditDlg fmResEdit = new ResearchEditDlg(this))
                {
                    bool exists = researchRec != null;
                    if (!exists) {
                        researchRec = new GEDCOMResearchRecord(fTree, fTree, "", "");
                        researchRec.InitNew();
                    }

                    try {
                        LockRecord(researchRec);

                        fmResEdit.Research = researchRec;
                        result = MainWin.Instance.ShowModalEx(fmResEdit, false) == DialogResult.OK;
                    } finally {
                        UnlockRecord(researchRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(researchRec);
                        } else {
                            researchRec.Dispose();
                            researchRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyTask(ref GEDCOMTaskRecord taskRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (TaskEditDlg fmTaskEdit = new TaskEditDlg(this))
                {
                    bool exists = taskRec != null;
                    if (!exists) {
                        taskRec = new GEDCOMTaskRecord(fTree, fTree, "", "");
                        taskRec.InitNew();
                    }

                    try {
                        LockRecord(taskRec);

                        fmTaskEdit.Task = taskRec;
                        result = MainWin.Instance.ShowModalEx(fmTaskEdit, false) == DialogResult.OK;
                    } finally {
                        UnlockRecord(taskRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(taskRec);
                        } else {
                            taskRec.Dispose();
                            taskRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyCommunication(ref GEDCOMCommunicationRecord commRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (CommunicationEditDlg fmCorrEdit = new CommunicationEditDlg(this))
                {
                    bool exists = commRec != null;
                    if (!exists) {
                        commRec = new GEDCOMCommunicationRecord(fTree, fTree, "", "");
                        commRec.InitNew();
                    }

                    try {
                        LockRecord(commRec);

                        fmCorrEdit.Communication = commRec;
                        result = MainWin.Instance.ShowModalEx(fmCorrEdit, false) == DialogResult.OK;
                    } finally {
                        UnlockRecord(commRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(commRec);
                        } else {
                            commRec.Dispose();
                            commRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyLocation(ref GEDCOMLocationRecord locRec)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (LocationEditDlg fmLocEdit = new LocationEditDlg(this))
                {
                    bool exists = locRec != null;
                    if (!exists) {
                        locRec = new GEDCOMLocationRecord(fTree, fTree, "", "");
                        locRec.InitNew();
                    }

                    try {
                        LockRecord(locRec);

                        fmLocEdit.LocationRecord = locRec;
                        result = MainWin.Instance.ShowModalEx(fmLocEdit, false) == DialogResult.OK;
                    } finally {
                        UnlockRecord(locRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(locRec);
                        } else {
                            locRec.Dispose();
                            locRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyName(ref NameEntry nameEntry)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (NameEditDlg dlg = new NameEditDlg(this)) {
                    dlg.IName = nameEntry;
                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        private void PostProcessPerson(GEDCOMIndividualRecord indivRec)
        {
            ImportNames(indivRec);

            IndividualListFilter iFilter = (IndividualListFilter)ListPersons.ListMan.Filter;

            if (iFilter.SourceMode == FilterGroupMode.Selected)
            {
                GEDCOMSourceRecord src = fTree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                if (src != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_IncludedSourceFilter)) == DialogResult.Yes)
                {
                    indivRec.AddSource(src, "", 0);
                }
            }

            if (iFilter.FilterGroupMode == FilterGroupMode.Selected)
            {
                GEDCOMGroupRecord grp = fTree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                if (grp != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_IncludedGroupFilter)) == DialogResult.Yes)
                {
                    grp.AddMember(indivRec);
                }
            }
        }

        public bool ModifyPerson(ref GEDCOMIndividualRecord indivRec,
                                 GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (PersonEditDlg dlg = new PersonEditDlg(this)) {
                    bool exists = (indivRec != null);
                    if (!exists) {
                        indivRec = new GEDCOMIndividualRecord(fTree, fTree, "", "");
                        indivRec.InitNew();

                        indivRec.AddPersonalName(new GEDCOMPersonalName(fTree, indivRec, "", ""));
                        fContext.CreateEventEx(indivRec, "BIRT", "", "");
                    }

                    try {
                        LockRecord(indivRec);

                        dlg.Person = indivRec;

                        if (targetMode != TargetMode.tmNone) {
                            if (needSex == GEDCOMSex.svMale || needSex == GEDCOMSex.svFemale) {
                                dlg.cmbSex.SelectedIndex = (int)needSex;
                            }
                            dlg.TargetMode = targetMode;
                            dlg.Target = target;
                        }

                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    } finally {
                        UnlockRecord(indivRec);
                    }

                    if (!exists) {
                        if (result) {
                            PostProcessPerson(indivRec);

                            fTree.AddRecord(indivRec);
                        } else {
                            GEDCOMUtils.CleanIndividual(indivRec);
                            indivRec.Dispose();
                            indivRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyFamily(ref GEDCOMFamilyRecord familyRec, FamilyTarget targetType, GEDCOMIndividualRecord target)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                if (targetType == FamilyTarget.Spouse && target != null) {
                    GEDCOMSex sex = target.Sex;
                    if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined) {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                        return false;
                    }
                }

                using (FamilyEditDlg dlg = new FamilyEditDlg(this)) {
                    bool exists = (familyRec != null);
                    if (!exists) {
                        familyRec = new GEDCOMFamilyRecord(fTree, fTree, "", "");
                        familyRec.InitNew();
                    }

                    try {
                        LockRecord(familyRec);

                        dlg.Family = familyRec;

                        if (targetType != FamilyTarget.None && target != null) {
                            dlg.SetTarget(targetType, target);
                        }

                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                    } finally {
                        UnlockRecord(familyRec);
                    }

                    if (!exists) {
                        if (result) {
                            fTree.AddRecord(familyRec);
                        } else {
                            GEDCOMUtils.CleanFamily(familyRec);
                            familyRec.Dispose();
                            familyRec = null;
                        }
                    }
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        public bool ModifyAddress(GEDCOMAddress address)
        {
            bool result;

            try {
                fContext.BeginUpdate();

                using (AddressEditDlg dlg = new AddressEditDlg(this)) {
                    dlg.Address = address;
                    result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);
                }
            } finally {
                fContext.EndUpdate();
            }

            return result;
        }

        #endregion
    }
}
