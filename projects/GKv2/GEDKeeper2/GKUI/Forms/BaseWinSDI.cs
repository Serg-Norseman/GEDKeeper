/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Security.Permissions;
using System.Windows.Forms;

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class BaseWinSDI : Form, IBaseWindow
    {
        private sealed class TabParts
        {
            public readonly GKListView ListView;
            public readonly HyperView Summary;

            public TabParts(GKListView listView, HyperView summary)
            {
                ListView = listView;
                Summary = summary;
            }
        }

        #region Private fields

        private readonly IBaseContext fContext;
        private readonly NavigationStack<GEDCOMRecord> fNavman;
        private readonly TabParts[] fTabParts;

        #endregion

        #region Public properties

        public IBaseContext Context
        {
            get { return fContext; }
        }

        public NavigationStack<GEDCOMRecord> Navman
        {
            get { return fNavman; }
        }

        #endregion

        #region Instance control

        public BaseWinSDI()
        {
            InitializeComponent();

            Icon = new Icon(GKUtils.LoadResourceStream("Resources.icon_gedkeeper.ico"));
            tbFileNew.Image = UIHelper.LoadResourceImage("Resources.btn_create_new.gif");
            tbFileLoad.Image = UIHelper.LoadResourceImage("Resources.btn_load.gif");
            tbFileSave.Image = UIHelper.LoadResourceImage("Resources.btn_save.gif");
            tbRecordAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            tbRecordEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            tbRecordDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            tbFilter.Image = UIHelper.LoadResourceImage("Resources.btn_filter.gif");
            tbTreeAncestors.Image = UIHelper.LoadResourceImage("Resources.btn_tree_ancestry.gif");
            tbTreeDescendants.Image = UIHelper.LoadResourceImage("Resources.btn_tree_descendants.gif");
            tbTreeBoth.Image = UIHelper.LoadResourceImage("Resources.btn_tree_both.gif");
            tbPedigree.Image = UIHelper.LoadResourceImage("Resources.btn_scroll.gif");
            tbStats.Image = UIHelper.LoadResourceImage("Resources.btn_table.gif");
            tbPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            tbNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");
            tbDocPreview.Image = UIHelper.LoadResourceImage("Resources.btn_preview.gif");
            tbDocPrint.Image = UIHelper.LoadResourceImage("Resources.btn_print.gif");

            tbDocPrint.Visible = false;
            tbDocPreview.Visible = false;

            AppHost.Instance.LoadWindow(this);

            fContext = new BaseContext(this);
            ((BaseContext)fContext).ModifiedChanged += BaseContext_ModifiedChanged;

            fNavman = new NavigationStack<GEDCOMRecord>();

            fTabParts = new TabParts[(int)GEDCOMRecordType.rtLast + 1];
            CreatePage(LangMan.LS(LSID.LSID_RPIndividuals), GEDCOMRecordType.rtIndividual);
            CreatePage(LangMan.LS(LSID.LSID_RPFamilies), GEDCOMRecordType.rtFamily);
            CreatePage(LangMan.LS(LSID.LSID_RPNotes), GEDCOMRecordType.rtNote);
            CreatePage(LangMan.LS(LSID.LSID_RPMultimedia), GEDCOMRecordType.rtMultimedia);
            CreatePage(LangMan.LS(LSID.LSID_RPSources), GEDCOMRecordType.rtSource);
            CreatePage(LangMan.LS(LSID.LSID_RPRepositories), GEDCOMRecordType.rtRepository);
            CreatePage(LangMan.LS(LSID.LSID_RPGroups), GEDCOMRecordType.rtGroup);
            CreatePage(LangMan.LS(LSID.LSID_RPResearches), GEDCOMRecordType.rtResearch);
            CreatePage(LangMan.LS(LSID.LSID_RPTasks), GEDCOMRecordType.rtTask);
            CreatePage(LangMan.LS(LSID.LSID_RPCommunications), GEDCOMRecordType.rtCommunication);
            CreatePage(LangMan.LS(LSID.LSID_RPLocations), GEDCOMRecordType.rtLocation);
            tabsRecords.SelectedIndex = 0;

            SetLang();
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

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            SetMainTitle();
        }

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
        {
            base.WndProc(ref m);

            if (m.Msg == GKUI.Components.NativeMethods.WM_KEEPMODELESS) {
                AppHost.Instance.WidgetsEnable();
            }
        }

        #endregion

        #region Form handlers

        private void Form_Activated(object sender, EventArgs e)
        {
            AppHost.Instance.BaseChanged(this);
        }

        private void Form_Deactivate(object sender, EventArgs e)
        {
            AppHost.Instance.BaseChanged(null);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            try
            {
                ((IWorkWindow)this).UpdateView();

                UpdatePluginsItems();
                UpdateMRU();
                UpdateControls(false);
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.Form_Load(): " + ex.Message);
            }
        }

        private void Form_Show(object sender, EventArgs e)
        {
        }

        private void Form_Closing(object sender, FormClosingEventArgs e)
        {
            e.Cancel = !CheckModified();
            if (e.Cancel) return;

            IListManager listMan = GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
            if (listMan != null) {
                listMan.ListColumns.CopyTo(GlobalOptions.Instance.IndividualListColumns);
            }

            AppHost.Instance.BaseClosed(this);
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            AppHost.Instance.CloseWindow(this);
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

                case Keys.F12:
                    break;

                    /*case Keys.F:
        			if (e.Control) {
        				QuickFind();
        			}
        			break;*/
            }
        }

        private void contextMenu_Opening(object sender, CancelEventArgs e)
        {
            GKListView recView = contextMenu.SourceControl as GKListView;

            miRecordDuplicate.Visible = (recView == fTabParts[(int)GEDCOMRecordType.rtIndividual].ListView);
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

        public GKListView GetRecordsViewByType(GEDCOMRecordType recType)
        {
            GKListView list = fTabParts[(int)recType].ListView;
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
            HyperView view = fTabParts[(int)recType].Summary;
            return view;
        }

        public IListManager GetRecordsListManByType(GEDCOMRecordType recType)
        {
            GKListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : (IListManager)rView.ListMan;
        }

        public GEDCOMRecord GetSelectedRecordEx()
        {
            GEDCOMRecordType recType = GetSelectedRecordType();
            GKListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : (rView.GetSelectedData() as GEDCOMRecord);
        }

        public GEDCOMIndividualRecord GetSelectedPerson()
        {
            return GetSelectedRecordEx() as GEDCOMIndividualRecord;
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (sender == null) return;

            GEDCOMRecord rec = ((GKListView) sender).GetSelectedData() as GEDCOMRecord;
            if (rec != null)
            {
                NavAdd(rec);
            }
            ShowRecordInfo(rec);
        }

        public List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType)
        {
            GKListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.ListMan.GetRecordsList();
        }

        private void SetMainTitle()
        {
            Text = Path.GetFileName(fContext.FileName);
            if (fContext.Modified) {
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
            AppHost.Instance.UpdateControls(false);
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

        public void SaveFileEx(bool saveAs)
        {
            if (!fContext.IsUnknown() && !saveAs) {
                SaveFile(fContext.FileName);
            } else {
                string homePath = AppHost.Instance.GetUserFilesPath(Path.GetDirectoryName(fContext.FileName));
                string fileName = AppHost.StdDialogs.GetSaveFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, fContext.FileName, false);
                if (!string.IsNullOrEmpty(fileName)) {
                    SaveFile(fileName);
                }
            }
        }

        public void CheckAutosave()
        {
            // file is modified, isn't updated now, and isn't now created (exists)
            if (fContext.Modified && !fContext.IsUpdated() && !fContext.IsUnknown()) {
                // TODO: if file is new and not exists - don't save it, but hint to user
                SaveFile(fContext.FileName);
            }
        }

        public bool CheckModified()
        {
            bool result = true;
            if (!fContext.Modified) return result;

            DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
            switch (dialogResult) {
                case DialogResult.Yes:
                    SaveFileEx(false);
                    break;
                case DialogResult.No:
                    break;
                case DialogResult.Cancel:
                    result = false;
                    break;
            }

            return result;
        }

        private void CreatePage(string pageText, GEDCOMRecordType recType)
        {
            tabsRecords.SuspendLayout();
            TabPage sheet = new TabPage(pageText);
            tabsRecords.Controls.Add(sheet);
            tabsRecords.ResumeLayout(false);

            var summary = new HyperView();
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

            var recView = UIHelper.CreateRecordsView(sheet, fContext, recType);
            recView.DoubleClick += miRecordEdit_Click;
            recView.SelectedIndexChanged += List_SelectedIndexChanged;
            recView.UpdateContents();
            recView.ContextMenuStrip = contextMenu;

            sheet.Controls.SetChildIndex(spl, 1);
            sheet.Controls.SetChildIndex(summary, 2);

            fTabParts[(int)recType] = new TabParts(recView, summary);
        }

        private void ChangeFileName()
        {
            SetMainTitle();
            GlobalOptions.Instance.LastDir = Path.GetDirectoryName(fContext.FileName);
            AppHost.Instance.AddMRU(fContext.FileName);
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
            ClearSummaries();
            fContext.SetFileName(LangMan.LS(LSID.LSID_Unknown));
            fContext.Tree.Header.Language.Value = GlobalOptions.Instance.GetCurrentItfLang();
            fContext.Modified = false;
        }

        public void LoadFile(string fileName)
        {
            Clear();

            if (fContext.FileLoad(fileName)) {
                fContext.Modified = false;
                ChangeFileName();
                RefreshLists(false);
            }
        }

        public void SaveFile(string fileName)
        {
            if (fContext.FileSave(fileName)) {
                fContext.Modified = false;
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

        public void ClearSummaries()
        {
            for (var rt = GEDCOMRecordType.rtIndividual; rt <= GEDCOMRecordType.rtLocation; rt++) {
                HyperView summary = fTabParts[(int)rt].Summary;
                if (summary != null) {
                    summary.Lines.Clear();
                }
            }
        }

        public void RefreshLists(bool columnsChanged)
        {
            for (var rt = GEDCOMRecordType.rtIndividual; rt <= GEDCOMRecordType.rtLocation; rt++) {
                GKListView listview = fTabParts[(int)rt].ListView;
                if (listview != null) {
                    listview.UpdateContents(columnsChanged);
                }
            }

            PageRecords_SelectedIndexChanged(null, null);
        }

        public void RefreshRecordsView(GEDCOMRecordType recType)
        {
            GKListView rView = GetRecordsViewByType(recType);
            if (rView != null) {
                rView.UpdateContents();
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
                        GKListView rView = GetRecordsViewByType(record.RecordType);
                        if (rView != null) {
                            rView.DeleteRecord(record);

                            HyperView hView = GetHyperViewByType(record.RecordType);
                            if ((hView != null) && (rView.ListMan.FilteredCount == 0)) {
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
                fContext.Modified = true;

                AppHost.Instance.NotifyRecord(this, record, action);
            }
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            if (!AllowFilter()) return;

            GEDCOMRecordType rt = GetSelectedRecordType();
            IListManager listMan = GetRecordsListManByType(rt);
            if (listMan == null) return;

            switch (rt) {
                case GEDCOMRecordType.rtIndividual:
                    using (PersonsFilterDlg fmFilter = new PersonsFilterDlg(this, listMan)) {
                        if (AppHost.Instance.ShowModalX(fmFilter, false)) {
                            ApplyFilter(rt);
                        }
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
                        if (AppHost.Instance.ShowModalX(fmComFilter, false)) {
                            ApplyFilter(rt);
                        }
                    }
                    break;
            }
        }

        private void NavAdd(GEDCOMRecord aRec)
        {
            if (aRec == null || fNavman.Busy) return;

            fNavman.Current = aRec;
            AppHost.Instance.UpdateControls(false);
        }

        public void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal)
        {
            if (mediaRec == null)
                throw new ArgumentNullException("mediaRec");

            GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
            if (mmKind == MultimediaKind.mkNone) {
                return;
            }

            bool externalViewer = !GlobalOptions.Instance.EmbeddedMediaPlayer &&
                ((mmKind == MultimediaKind.mkAudio || mmKind == MultimediaKind.mkVideo));

            if (externalViewer) {
                string targetFile = fContext.MediaLoad(fileRef);
                GKUtils.LoadExtFile(targetFile);
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
                    Logger.LogWrite("BaseWinSDI.ShowMedia(): " + ex.Message);
                }
            }
        }

        #endregion

        #region ILocalization implementation

        public void SetLang()
        {
            miFile.Text = LangMan.LS(LSID.LSID_MIFile);
            miEdit.Text = LangMan.LS(LSID.LSID_MIEdit);
            miPedigree.Text = LangMan.LS(LSID.LSID_MIPedigree);
            miService.Text = LangMan.LS(LSID.LSID_MIService);
            //miWindow.Text = LangMan.LS(LSID.LSID_MIWindow);
            miHelp.Text = LangMan.LS(LSID.LSID_MIHelp);

            miFileNew.Text = LangMan.LS(LSID.LSID_MIFileNew);
            miFileLoad.Text = LangMan.LS(LSID.LSID_MIFileLoad);
            miMRUFiles.Text = LangMan.LS(LSID.LSID_MIMRUFiles);
            miFileSave.Text = LangMan.LS(LSID.LSID_MIFileSave);
            miFileSaveAs.Text = LangMan.LS(LSID.LSID_MIFileSaveAs);
            miFileClose.Text = LangMan.LS(LSID.LSID_MIFileClose);
            miFileProperties.Text = LangMan.LS(LSID.LSID_MIFileProperties) + @"...";
            miExport.Text = LangMan.LS(LSID.LSID_MIExport);
            miExportToFamilyBook.Text = LangMan.LS(LSID.LSID_MIExportToFamilyBook);
            miExportToTreesAlbum.Text = LangMan.LS(LSID.LSID_TreesAlbum);
            miExportToExcelFile.Text = LangMan.LS(LSID.LSID_MIExportToExcelFile);
            miExit.Text = LangMan.LS(LSID.LSID_MIExit);

            miRecordAdd.Text = LangMan.LS(LSID.LSID_MIRecordAdd);
            miRecordEdit.Text = LangMan.LS(LSID.LSID_MIRecordEdit);
            miRecordDelete.Text = LangMan.LS(LSID.LSID_MIRecordDelete);

            miTreeAncestors.Text = LangMan.LS(LSID.LSID_MITreeAncestors);
            miTreeDescendants.Text = LangMan.LS(LSID.LSID_MITreeDescendants);
            miTreeBoth.Text = LangMan.LS(LSID.LSID_MITreeBoth);
            miPedigreeAscend.Text = LangMan.LS(LSID.LSID_MIPedigreeAscend);
            miPedigree_dAboville.Text = LangMan.LS(LSID.LSID_MIPedigree_dAboville);
            miPedigree_Konovalov.Text = LangMan.LS(LSID.LSID_MIPedigree_Konovalov);

            miMap.Text = LangMan.LS(LSID.LSID_MIMap) + @"...";
            miStats.Text = LangMan.LS(LSID.LSID_MIStats) + @"...";
            miSearch.Text = LangMan.LS(LSID.LSID_Search);
            miAncestorsCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            miDescendantsCircle.Text = LangMan.LS(LSID.LSID_DescendantsCircle);
            miRelationshipCalculator.Text = LangMan.LS(LSID.LSID_RelationshipCalculator);

            miOrganizer.Text = LangMan.LS(LSID.LSID_MIOrganizer) + @"...";
            miSlideshow.Text = LangMan.LS(LSID.LSID_Slideshow) + @"...";
            miScripts.Text = LangMan.LS(LSID.LSID_MIScripts);
            miTreeTools.Text = LangMan.LS(LSID.LSID_MITreeTools);
            miFilter.Text = LangMan.LS(LSID.LSID_MIFilter) + @"...";
            miOptions.Text = LangMan.LS(LSID.LSID_MIOptions) + @"...";

            //miWinCascade.Text = LangMan.LS(LSID.LSID_MIWinCascade);
            //miWinHTile.Text = LangMan.LS(LSID.LSID_MIWinHTile);
            //miWinVTile.Text = LangMan.LS(LSID.LSID_MIWinVTile);
            //miWinMinimize.Text = LangMan.LS(LSID.LSID_MIWinMinimize);
            //miWinArrange.Text = LangMan.LS(LSID.LSID_MIWinArrange);

            miContext.Text = LangMan.LS(LSID.LSID_MIContext);
            miAbout.Text = LangMan.LS(LSID.LSID_MIAbout) + @"...";
            miLogSend.Text = LangMan.LS(LSID.LSID_LogSend);
            miLogView.Text = LangMan.LS(LSID.LSID_LogView);
            miPlugins.Text = LangMan.LS(LSID.LSID_Plugins);
            miReports.Text = LangMan.LS(LSID.LSID_Reports);

            tbFileNew.ToolTipText = LangMan.LS(LSID.LSID_FileNewTip);
            tbFileLoad.ToolTipText = LangMan.LS(LSID.LSID_FileLoadTip);
            tbFileSave.ToolTipText = LangMan.LS(LSID.LSID_FileSaveTip);
            tbRecordAdd.ToolTipText = LangMan.LS(LSID.LSID_RecordAddTip);
            tbRecordEdit.ToolTipText = LangMan.LS(LSID.LSID_RecordEditTip);
            tbRecordDelete.ToolTipText = LangMan.LS(LSID.LSID_RecordDeleteTip);
            tbFilter.ToolTipText = LangMan.LS(LSID.LSID_FilterTip);
            tbTreeAncestors.ToolTipText = LangMan.LS(LSID.LSID_TreeAncestorsTip);
            tbTreeDescendants.ToolTipText = LangMan.LS(LSID.LSID_TreeDescendantsTip);
            tbTreeBoth.ToolTipText = LangMan.LS(LSID.LSID_TreeBothTip);
            tbPedigree.ToolTipText = LangMan.LS(LSID.LSID_PedigreeTip);
            miPedigree_dAboville2.Text = LangMan.LS(LSID.LSID_Pedigree_dAbovilleTip);
            miPedigree_Konovalov2.Text = LangMan.LS(LSID.LSID_Pedigree_KonovalovTip);
            tbStats.ToolTipText = LangMan.LS(LSID.LSID_StatsTip);

            tbDocPrint.ToolTipText = LangMan.LS(LSID.LSID_DocPrint);
            tbDocPreview.ToolTipText = LangMan.LS(LSID.LSID_DocPreview);

            tbPrev.ToolTipText = LangMan.LS(LSID.LSID_PrevRec);
            tbNext.ToolTipText = LangMan.LS(LSID.LSID_NextRec);

            int num = miPlugins.DropDownItems.Count;
            for (int i = 0; i < num; i++) {
                ToolStripItem mi = miPlugins.DropDownItems[i];
                IPlugin plugin = (IPlugin)mi.Tag;
                mi.Text = plugin.DisplayName;
            }

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

            miContRecordAdd.Text = LangMan.LS(LSID.LSID_MIRecordAdd);
            miContRecordEdit.Text = LangMan.LS(LSID.LSID_MIRecordEdit);
            miContRecordDelete.Text = LangMan.LS(LSID.LSID_MIRecordDelete);
            miRecordDuplicate.Text = LangMan.LS(LSID.LSID_RecordDuplicate);
        }

        #endregion

        #region IWorkWindow implementation
        
        string IWorkWindow.GetStatusString()
        {
            string res = "";

            GEDCOMRecordType recType = GetSelectedRecordType();
            GKListView rView = GetRecordsViewByType(recType);

            if (rView != null)
            {
                var listMan = rView.ListMan;
                res = LangMan.LS(LSID.LSID_SBRecords) + ": " + listMan.TotalCount.ToString();
                res = res + ", " + LangMan.LS(LSID.LSID_SBFiltered) + ": " + listMan.FilteredCount.ToString();
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
                    AppHost.Instance.UpdateControls(false);
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
                    AppHost.Instance.UpdateControls(false);
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
            if (!AllowQuickSearch()) return;

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
            if (original == null || original.RecordType != GEDCOMRecordType.rtIndividual) return;

            AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_DuplicateWarning));

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

            GEDCOMRecord rec = BaseController.AddRecord(this, rt, null);
            if (rec != null) {
                RefreshLists(false);
                SelectRecordByXRef(rec.XRef);
            }
        }

        public void EditRecord()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.EditRecord(this, record)) {
                RefreshLists(false);
                ShowRecordInfo(record);
            }
        }

        public void DeleteRecord()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.DeleteRecord(this, record, true)) {
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
            GKListView rView = (record == null) ? null : GetRecordsViewByType(record.RecordType);

            if (rView != null) {
                ShowRecordsTab(record.RecordType);
                ActiveControl = rView;
                rView.SelectItemByData(record);
            }
        }

        public void ShowRecordInfo(GEDCOMRecord record)
        {
            if (record == null) return;

            try
            {
                HyperView hyperView = GetHyperViewByType(record.RecordType);
                if (hyperView != null) {
                    GKUtils.GetRecordContent(fContext, record, hyperView.Lines);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseWinSDI.ShowRecordInfo(): " + ex.Message);
            }
        }

        public StringList GetRecordContent(GEDCOMRecord record)
        {
            StringList ctx = new StringList();
            GKUtils.GetRecordContent(fContext, record, ctx);
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
                GKListView rView = GetRecordsViewByType(record.RecordType);
                result = (rView != null && rView.ListMan.IndexOfRecord(record) >= 0);
            }
            return result;
        }

        #endregion

        #region From MainWin

        // FIXME
        private void tbDocPrint_Click(object sender, EventArgs e)
        {
            /*IChartWindow chartWin = AppHost.Instance.GetWorkWindow() as IChartWindow;
            if (chartWin != null && chartWin.AllowPrint()) {
                chartWin.DoPrint();
            }*/
        }

        // FIXME
        private void tbDocPreview_Click(object sender, EventArgs e)
        {
            /*IChartWindow chartWin = AppHost.Instance.GetWorkWindow() as IChartWindow;
            if (chartWin != null && chartWin.AllowPrint()) {
                chartWin.DoPrintPreview();
            }*/
        }

        private void Form_Resize(object sender, EventArgs e)
        {
            StatusBar.Panels[0].Width = Width - 50;
        }

        public void Restore()
        {
            if (WindowState == FormWindowState.Minimized) {
                WindowState = FormWindowState.Normal;
            }
        }

        private void Form_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = e.Data.GetDataPresent(DataFormats.FileDrop) ? DragDropEffects.Copy : DragDropEffects.None;
        }

        private void Form_DragDrop(object sender, DragEventArgs e)
        {
            try {
                try {
                    AppHost.Instance.BeginLoading();

                    Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
                    if (a == null) return;

                    for (int i = 0; i < a.Length; i++) {
                        string fn = a.GetValue(i).ToString();
                        AppHost.Instance.LoadBase(this, fn);
                    }
                } finally {
                    AppHost.Instance.EndLoading();
                }
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.Form_DragDrop(): " + ex.Message);
            }
        }

        private void StatusBar_DrawItem(object sender, StatusBarDrawItemEventArgs sbdevent)
        {
            Bitmap pic = null;
            switch (fContext.ShieldState)
            {
                case ShieldState.None:
                    pic = (Bitmap)UIHelper.LoadResourceImage("Resources.rg_shield_none.gif");
                    break;
                case ShieldState.Middle:
                    pic = (Bitmap)UIHelper.LoadResourceImage("Resources.rg_shield_mid.gif");
                    break;
                case ShieldState.Maximum:
                    pic = (Bitmap)UIHelper.LoadResourceImage("Resources.rg_shield_max.gif");
                    break;
            }

            if (pic != null) {
                pic.MakeTransparent(pic.GetPixel(0, 0));
                sbdevent.Graphics.DrawImage(pic, sbdevent.Bounds.Left, sbdevent.Bounds.Top);
            }
        }

        private void StatusBar_PanelClick(object sender, StatusBarPanelClickEventArgs e)
        {
            if (e.StatusBarPanel == StatusBarPanel2 && e.Clicks == 2) {
                fContext.SwitchShieldState();
                StatusBar.Invalidate();
            }
        }

        private void MRUFileClick(object sender, EventArgs e)
        {
            int idx = (int)((GKToolStripMenuItem)sender).Tag;
            AppHost.Instance.LoadBase(this, AppHost.Options.MRUFiles[idx].FileName);
        }

        public void UpdateMRU()
        {
            try {
                miMRUFiles.Enabled = (AppHost.Options.MRUFiles.Count > 0);
                miMRUFiles.DropDownItems.Clear();
                MenuMRU.Items.Clear();

                int num = AppHost.Options.MRUFiles.Count;
                for (int i = 0; i < num; i++) {
                    string fn = AppHost.Options.MRUFiles[i].FileName;

                    GKToolStripMenuItem mi = new GKToolStripMenuItem(fn, i);
                    mi.Click += MRUFileClick;
                    miMRUFiles.DropDownItems.Add(mi);

                    GKToolStripMenuItem tsmi = new GKToolStripMenuItem(fn, i);
                    tsmi.Click += MRUFileClick;
                    MenuMRU.Items.Add(tsmi);
                }
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.UpdateMRU(): " + ex.Message);
            }
        }

        public void UpdateNavControls()
        {
            try
            {
                IWorkWindow workWin = this as IWorkWindow;

                tbPrev.Enabled = (workWin != null && workWin.NavCanBackward());
                tbNext.Enabled = (workWin != null && workWin.NavCanForward());
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.UpdateNavControls(): " + ex.Message);
            }
        }

        public void UpdateControls(bool forceDeactivate)
        {
            try
            {
                IWorkWindow workWin = AppHost.Instance.GetWorkWindow();
                IBaseWindow curBase = ((forceDeactivate) ? null : AppHost.Instance.GetCurrentFile());
                IChartWindow curChart = ((workWin is IChartWindow) ? ((IChartWindow) workWin) : null);

                GEDCOMRecordType rt = (curBase == null) ? GEDCOMRecordType.rtNone : curBase.GetSelectedRecordType();
                bool baseEn = (rt != GEDCOMRecordType.rtNone);

                miFileSave.Enabled = baseEn || (curChart != null);
                miFileSaveAs.Enabled = miFileSave.Enabled;
                tbFileSave.Enabled = miFileSave.Enabled;

                miRecordAdd.Enabled = baseEn;
                tbRecordAdd.Enabled = miRecordAdd.Enabled;
                miRecordEdit.Enabled = baseEn;
                tbRecordEdit.Enabled = miRecordEdit.Enabled;
                miRecordDelete.Enabled = baseEn;
                tbRecordDelete.Enabled = miRecordDelete.Enabled;
                miStats.Enabled = baseEn;
                tbStats.Enabled = miStats.Enabled;

                miFilter.Enabled = (workWin != null && workWin.AllowFilter());
                tbFilter.Enabled = miFilter.Enabled;

                miSearch.Enabled = (workWin != null && workWin.AllowQuickSearch());

                tbDocPrint.Enabled = (curChart != null && curChart.AllowPrint());
                tbDocPreview.Enabled = (curChart != null && curChart.AllowPrint());

                miTreeTools.Enabled = baseEn;
                miExportToFamilyBook.Enabled = baseEn;
                miExportToTreesAlbum.Enabled = baseEn;
                miExportToExcelFile.Enabled = baseEn;
                miFileClose.Enabled = baseEn;
                miFileProperties.Enabled = baseEn;
                miOrganizer.Enabled = baseEn;
                miSlideshow.Enabled = baseEn;
                miScripts.Enabled = baseEn;

                bool indivEn = baseEn && rt == GEDCOMRecordType.rtIndividual;

                miTreeAncestors.Enabled = indivEn;
                tbTreeAncestors.Enabled = miTreeAncestors.Enabled;
                miTreeDescendants.Enabled = indivEn;
                tbTreeDescendants.Enabled = miTreeDescendants.Enabled;
                miTreeBoth.Enabled = indivEn;
                tbTreeBoth.Enabled = miTreeBoth.Enabled;
                miPedigree.Enabled = indivEn;
                tbPedigree.Enabled = miPedigree.Enabled;
                miPedigree_dAboville.Enabled = indivEn;
                miPedigree_Konovalov.Enabled = indivEn;

                if (workWin != null) {
                    StatusBar.Panels[0].Text = workWin.GetStatusString();
                }

                UpdateNavControls();

                StatusBar.Invalidate();
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.UpdateControls(): " + ex.Message);
            }
        }

        private void miExit_Click(object sender, EventArgs e)
        {
            // FIXME: Controversial issue...
            //AppHost.Instance.SaveLastBases();
            Application.Exit();
        }

        private void miUndo_Click(object sender, EventArgs e)
        {
            fContext.DoUndo();
        }

        private void miRedo_Click(object sender, EventArgs e)
        {
            fContext.DoRedo();
        }

        private void miExportToFamilyBook_Click(object sender, EventArgs e)
        {
            //#if __MonoCS__
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            //#endif

            using (FamilyBookExporter fb = new FamilyBookExporter(this)) {
                fb.Generate(true);
            }
        }

        private void miExportToTreesAlbum_Click(object sender, EventArgs e)
        {
            //#if __MonoCS__
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            //#endif

            AppHost.StdDialogs.ShowWarning(@"This function is experimental and not completed. Only for PDF!");
            using (TreesAlbumExporter ta = new TreesAlbumExporter(this)) {
                ta.Generate(true);
            }
        }

        private void miExportToExcelFile_Click(object sender, EventArgs e)
        {
            using (ExcelExporter exExp = new ExcelExporter(this)) {
                exExp.Options = AppHost.Options;
                exExp.Generate(true);
            }
        }

        private void miFileProperties_Click(object sender, EventArgs e)
        {
            try {
                fContext.BeginUpdate();

                using (var dlg = new FilePropertiesDlg()) {
                    dlg.InitDialog(this);
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        private void miScripts_Click(object sender, EventArgs e)
        {
            try {
                fContext.BeginUpdate();

                using (ScriptEditWin scriptWin = new ScriptEditWin(this)) {
                    scriptWin.ShowDialog();
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        private void miTreeTools_Click(object sender, EventArgs e)
        {
            try {
                fContext.BeginUpdate();

                using (TreeToolsWin fmTreeTools = new TreeToolsWin(this)) {
                    fmTreeTools.ShowDialog();
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        // FIXME: implement button of options into charts windows
        private void miOptions_Click(object sender, EventArgs e)
        {
            using (OptionsDlg dlgOptions = new OptionsDlg(AppHost.Instance))
            {
                IWindow activeWin = AppHost.Instance.GetActiveWindow();
                if (activeWin is IBaseWindow) dlgOptions.SetPage(OptionsPage.opInterface);
                if (activeWin is IChartWindow) {
                    if (activeWin is CircleChartWin) {
                        dlgOptions.SetPage(OptionsPage.opAncestorsCircle);
                    } else {
                        dlgOptions.SetPage(OptionsPage.opTreeChart);
                    }
                }

                if (dlgOptions.ShowDialog() == DialogResult.OK) {
                    AppHost.Instance.ApplyOptions();
                }
            }
        }

        private void miFileClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void miFileNew_Click(object sender, EventArgs e)
        {
            AppHost.Instance.CreateBase("");
        }

        private void miFileLoad_Click(object sender, EventArgs e)
        {
            string homePath = AppHost.Instance.GetUserFilesPath("");

            string fileName = AppHost.StdDialogs.GetOpenFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                AppHost.Instance.LoadBase(this, fileName);
            }
        }

        private void miFileSaveAs_Click(object sender, EventArgs e)
        {
            SaveFileEx(true);
        }

        private void miFileSave_Click(object sender, EventArgs e)
        {
            SaveFileEx(false);
        }

        private void miSearch_Click(object sender, EventArgs e)
        {
            (this as IWorkWindow).QuickSearch();
        }

        private void miFilter_Click(object sender, EventArgs e)
        {
            (this as IWorkWindow).SetFilter();
        }

        private void tbPrev_Click(object sender, EventArgs e)
        {
            (this as IWorkWindow).NavPrev();
        }

        private void tbNext_Click(object sender, EventArgs e)
        {
            (this as IWorkWindow).NavNext();
        }

        private void miMap_Click(object sender, EventArgs e)
        {
            //#if __MonoCS__
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            MapsViewerWin mapsWin = new MapsViewerWin(this);
            mapsWin.ProcessMap();
            //#endif
        }

        private void miOrganizer_Click(object sender, EventArgs e)
        {
            using (OrganizerWin dlg = new OrganizerWin(this)) {
                dlg.ShowDialog();
            }
        }

        private void miRelationshipCalculator_Click(object sender, EventArgs e)
        {
            using (RelationshipCalculatorDlg relCalc = new RelationshipCalculatorDlg(this)) {
                relCalc.ShowDialog();
            }
        }

        private void miSlideshow_Click(object sender, EventArgs e)
        {
            SlideshowWin win = new SlideshowWin(this);
            AppHost.Instance.ShowWindow(win);
        }

        private void miStats_Click(object sender, EventArgs e)
        {
            List<GEDCOMRecord> selectedRecords = GetContentList(GEDCOMRecordType.rtIndividual);

            StatisticsWin win = new StatisticsWin(this, selectedRecords);
            AppHost.Instance.ShowWindow(win);
        }

        private void GeneratePedigree(PedigreeExporter.PedigreeKind kind)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            using (var p = new PedigreeExporter(this, selPerson)) {
                p.Options = AppHost.Options;
                p.Kind = kind;
                p.Generate(true);
            }
        }

        private void miPedigreeAscend_Click(object sender, EventArgs e)
        {
            GeneratePedigree(PedigreeExporter.PedigreeKind.pkAscend);
        }

        private void miPedigree_dAbovilleClick(object sender, EventArgs e)
        {
            GeneratePedigree(PedigreeExporter.PedigreeKind.pkDescend_dAboville);
        }

        private void miPedigree_KonovalovClick(object sender, EventArgs e)
        {
            GeneratePedigree(PedigreeExporter.PedigreeKind.pkDescend_Konovalov);
        }

        private void miTreeAncestors_Click(object sender, EventArgs e)
        {
            ShowTreeChart(TreeChartKind.ckAncestors);
        }

        private void miTreeDescendants_Click(object sender, EventArgs e)
        {
            ShowTreeChart(TreeChartKind.ckDescendants);
        }

        private void miTreeBoth_Click(object sender, EventArgs e)
        {
            ShowTreeChart(TreeChartKind.ckBoth);
        }

        private void ShowTreeChart(TreeChartKind chartKind)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) return;

            if (TreeChartModel.CheckTreeChartSize(fContext.Tree, selPerson, chartKind)) {
                TreeChartWin fmChart = new TreeChartWin(this, selPerson);
                fmChart.ChartKind = chartKind;
                fmChart.GenChart();
                AppHost.Instance.ShowWindow(fmChart);
            }
        }

        private void miAncestorsCircle_Click(object sender, EventArgs e)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) return;

            CircleChartWin fmChart = new CircleChartWin(this, selPerson, CircleChartType.Ancestors);
            AppHost.Instance.ShowWindow(fmChart);
        }

        private void miDescendantsCircle_Click(object sender, EventArgs e)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) return;

            CircleChartWin fmChart = new CircleChartWin(this, selPerson, CircleChartType.Descendants);
            AppHost.Instance.ShowWindow(fmChart);
        }

        private void miLogSend_Click(object sender, EventArgs e)
        {
            SysUtils.SendMail(GKData.APP_MAIL, "GEDKeeper: feedback", "This automatic notification of error.", AppHost.GetLogFilename());
        }

        private void miLogView_Click(object sender, EventArgs e)
        {
            GKUtils.LoadExtFile(AppHost.GetLogFilename());
        }

        private void miAbout_Click(object sender, EventArgs e)
        {
            using (AboutDlg dlg = new AboutDlg()) {
                dlg.ShowDialog();
            }
        }

        private void miContext_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowHelpTopic("");
        }

        private static void Plugin_Click(object sender, EventArgs e)
        {
            ToolStripMenuItem item = sender as ToolStripMenuItem;
            if (item == null) return;

            IPlugin plugin = item.Tag as IPlugin;
            if (plugin == null) return;

            plugin.Execute();
        }

        private void UpdatePluginsItems()
        {
            try {
                miPlugins.DropDownItems.Clear();
                miReports.DropDownItems.Clear();

                AppHost.Instance.ActiveWidgets.Clear();

                int num = AppHost.Plugins.Count;
                for (int i = 0; i < num; i++) {
                    IPlugin plugin = AppHost.Plugins[i];
                    string dispName = plugin.DisplayName;
                    ImageHandler hIcon = plugin.Icon as ImageHandler;

                    ToolStripMenuItemEx mi = new ToolStripMenuItemEx(dispName/*, i*/);
                    mi.Click += Plugin_Click;
                    mi.Tag = plugin;
                    mi.Image = (hIcon == null) ? null : hIcon.Handle;

                    if (plugin.Category == PluginCategory.Report) {
                        miReports.DropDownItems.Add(mi);
                    } else {
                        miPlugins.DropDownItems.Add(mi);
                    }

                    if (plugin is IWidget) {
                        WidgetInfo widInfo = new WidgetInfo();
                        widInfo.Widget = (plugin as IWidget);
                        widInfo.MenuItem = mi;
                        AppHost.Instance.ActiveWidgets.Add(widInfo);

                        (plugin as IWidget).WidgetInit(AppHost.Instance);
                    }
                }

                miReports.Visible = (miReports.DropDownItems.Count > 0);
                miPlugins.Visible = (miPlugins.DropDownItems.Count > 0);
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.UpdatePluginsItems(): " + ex.Message);
            }
        }

        #endregion
    }
}
