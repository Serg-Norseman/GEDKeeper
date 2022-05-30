/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Design.MVP.Controls;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public sealed partial class BaseWinSDI : CommonWindow, IBaseWindowView
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TableLayout StatusBar;
        private Label panStatusText;
        private Eto.Forms.ImageView panStatusShieldImage;
        private ToolBar ToolBar1;
        private ButtonToolItem tbFileNew;
        private ButtonToolItem tbFileLoad;
        private ButtonToolItem tbFileSave;
        private ButtonToolItem tbRecordAdd;
        private ButtonToolItem tbRecordEdit;
        private ButtonToolItem tbRecordDelete;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbTreeAncestors;
        private ButtonToolItem tbTreeDescendants;
        private ButtonToolItem tbPedigree;
        private ButtonToolItem tbStats;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbSendMail;
        private MenuBar MainMenu1;
        private ButtonMenuItem miFile;
        private ButtonMenuItem miFileNew;
        private ButtonMenuItem miFileLoad;
        private ButtonMenuItem miMRUFiles;
        private ButtonMenuItem miFileSave;
        private ButtonMenuItem miFileSaveAs;
        private ButtonMenuItem miFileClose;
        private ButtonMenuItem miFileProperties;
        private ButtonMenuItem miExportToExcelFile;
        private ButtonMenuItem miExportToFamilyBook;
        private ButtonMenuItem miExportToTreesAlbum;
        private ButtonMenuItem miTreeTools;
        private ButtonMenuItem miExit;
        private ButtonMenuItem miEdit;
        private ButtonMenuItem miRecordAdd;
        private ButtonMenuItem miRecordEdit;
        private ButtonMenuItem miRecordDelete;
        private ButtonMenuItem miSearch;
        private ButtonMenuItem miFilter;
        private ButtonMenuItem miOptions;
        private ButtonMenuItem miPedigree;
        private ButtonMenuItem miTreeAncestors;
        private ButtonMenuItem miTreeDescendants;
        private ButtonMenuItem miPedigree_dAboville;
        private ButtonMenuItem miPedigree_Konovalov;
        private ButtonMenuItem miMap;
        private ButtonMenuItem miStats;
        private ButtonMenuItem miHelp;
        private ButtonMenuItem miContext;
        private ButtonMenuItem miLogSend;
        private ButtonMenuItem miLogView;
        private ButtonMenuItem miAbout;
        private ContextMenu MenuMRU;
        private ContextMenu MenuPedigree;
        private ButtonMenuItem miPedigree_dAboville2;
        private ButtonMenuItem miPedigree_Konovalov2;
        private ButtonMenuItem miOrganizer;
        private ButtonMenuItem miService;
        private ButtonMenuItem miScripts;
        private ButtonMenuItem miExport;
        private ButtonMenuItem miTreeBoth;
        private ButtonMenuItem miAncestorsCircle;
        private ButtonToolItem tbTreeBoth;
        private ButtonMenuItem miReports;
        private ButtonMenuItem miPlugins;
        private ButtonMenuItem miSlideshow;
        private ButtonToolItem tbLoadMRU;
        private ButtonMenuItem miPedigreeAscend;
        private ButtonMenuItem miDescendantsCircle;
        private ButtonMenuItem miRelationshipCalculator;
        private TabControl tabsRecords;
        private ButtonMenuItem miContRecordDuplicate;
        private ButtonMenuItem miContRecordDelete;
        private ButtonMenuItem miContRecordEdit;
        private ButtonMenuItem miContRecordMerge;
        private ContextMenu contextMenu;
        private ButtonMenuItem miContRecordAdd;
        private ButtonMenuItem miTreeCompare;
        private ButtonMenuItem miTreeMerge;
        private ButtonMenuItem miTreeSplit;
        private ButtonMenuItem miRecMerge;
        private ButtonMenuItem miFamilyGroups;
        private ButtonMenuItem miTreeCheck;
        private ButtonMenuItem miPatSearch;
        private ButtonMenuItem miPlacesManager;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region Private fields

        private readonly BaseWinController fController;

        private readonly IBaseContext fContext;

        #endregion

        #region Public properties

        public IBaseContext Context
        {
            get { return fContext; }
        }

        #endregion

        #region View Interface

        ITabControl IBaseWindowView.RecordTabs
        {
            get { return GetControlHandler<ITabControl>(tabsRecords); }
        }

        IMenuItem IBaseWindowView.ReportsItem
        {
            get { return GetControlHandler<IMenuItem>(miReports); }
        }

        IMenuItem IBaseWindowView.PluginsItem
        {
            get { return GetControlHandler<IMenuItem>(miPlugins); }
        }

        #endregion

        #region Instance control

        public BaseWinSDI()
        {
            XamlReader.Load(this);
            InitializeComponent();

            AppHost.Instance.LoadWindow(this);

            fController = new BaseWinController(this);
            fContext = fController.Context;
            ((BaseContext)fContext).ModifiedChanged += BaseContext_ModifiedChanged;

            tabsRecords.SuspendLayout();
            CreatePage(LangMan.LS(LSID.LSID_RPIndividuals), GDMRecordType.rtIndividual);
            CreatePage(LangMan.LS(LSID.LSID_RPFamilies), GDMRecordType.rtFamily);
            CreatePage(LangMan.LS(LSID.LSID_RPNotes), GDMRecordType.rtNote);
            CreatePage(LangMan.LS(LSID.LSID_RPMultimedia), GDMRecordType.rtMultimedia);
            CreatePage(LangMan.LS(LSID.LSID_RPSources), GDMRecordType.rtSource);
            CreatePage(LangMan.LS(LSID.LSID_RPRepositories), GDMRecordType.rtRepository);
            CreatePage(LangMan.LS(LSID.LSID_RPGroups), GDMRecordType.rtGroup);
            CreatePage(LangMan.LS(LSID.LSID_RPResearches), GDMRecordType.rtResearch);
            CreatePage(LangMan.LS(LSID.LSID_RPTasks), GDMRecordType.rtTask);
            CreatePage(LangMan.LS(LSID.LSID_RPCommunications), GDMRecordType.rtCommunication);
            CreatePage(LangMan.LS(LSID.LSID_RPLocations), GDMRecordType.rtLocation);
            tabsRecords.ResumeLayout();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fController.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            MenuMRU = new ContextMenu();

            miPedigree_dAboville2 = new ButtonMenuItem(miPedigree_dAbovilleClick);
            miPedigree_Konovalov2 = new ButtonMenuItem(miPedigree_KonovalovClick);
            MenuPedigree = new ContextMenu();
            MenuPedigree.Items.AddRange(new MenuItem[] { miPedigree_dAboville2, miPedigree_Konovalov2 });

            miContRecordAdd = new ButtonMenuItem(miRecordAdd_Click);
            miContRecordEdit = new ButtonMenuItem(miRecordEdit_Click);
            miContRecordDelete = new ButtonMenuItem(miRecordDelete_Click);
            miContRecordDuplicate = new ButtonMenuItem(miRecordDuplicate_Click);
            miContRecordMerge = new ButtonMenuItem(miRecordMerge_Click);
            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] { miContRecordAdd, miContRecordEdit, miContRecordDelete, miContRecordDuplicate, miContRecordMerge });
            contextMenu.Opening += contextMenu_Opening;
        }

        private void CreatePage(string pageText, GDMRecordType recType)
        {
            var summary = new HyperView();
            summary.BorderWidth = 4;
            summary.OnLink += mPersonSummaryLink;

            var recView = new GKListView();
            recView.AllowMultipleSelection = true;
            recView.MouseDoubleClick += miRecordEdit_Click;
            recView.SelectedItemsChanged += List_SelectedIndexChanged;
            recView.UpdateContents();
            recView.ContextMenu = contextMenu;
            recView.ListMan = ListManager.Create(fContext, recType);

            Splitter spl = new Splitter();
            spl.Panel1 = recView;
            spl.Panel2 = summary;
            spl.RelativePosition = 300;
            spl.Orientation = Orientation.Horizontal;
            spl.FixedPanel = SplitterFixedPanel.Panel2;

            TabPage tabPage = new TabPage();
            tabPage.Text = pageText;
            tabPage.Content = spl;
            tabsRecords.Pages.Add(tabPage);

            fController.SetTabPart(recType, recView, summary);
        }

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            fController.SetMainTitle();
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
            try {
                ((IWorkWindow)this).UpdateSettings();

                fController.UpdatePluginsItems();
                UpdateMRU();
                UpdateControls(false);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.Form_Load()", ex);
            }
        }

        private void Form_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !CheckModified();
            if (e.Cancel) return;

            fController.SaveListsSettings();

            AppHost.Instance.BaseClosed(this);
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            AppHost.Instance.CloseWindow(this);
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.Key) {
                    /*case Keys.I:
						ItemAdd();
						break;
					case Keys.D:
						ItemDelete();
						break;*/

                case Keys.Enter:
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

        private void contextMenu_Opening(object sender, EventArgs e)
        {
            IListView recView = GetRecordsViewByType(GetSelectedRecordType());

            miContRecordDuplicate.Enabled = (recView == fController.GetRecordsViewByType(GDMRecordType.rtIndividual));
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

        private void miRecordMerge_Click(object sender, EventArgs e)
        {
            var recView = GetRecordsViewByType(GetSelectedRecordType()) as GKListView;
            if (recView != null) {
                var items = recView.GetSelectedItems();
                fController.ShowRecMerge(
                    items.Count > 0 ? items[0] as GDMRecord : null,
                    items.Count > 1 ? items[1] as GDMRecord : null
                );
            }
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (sender != null) {
                fController.ChangeListItem((IListViewEx)sender);
                AppHost.Instance.SelectedIndexChanged(this);
            }
        }

        private void tabsRecords_SelectedIndexChanged(object sender, EventArgs e)
        {
            AppHost.Instance.UpdateControls(false);
            AppHost.Instance.TabChanged(this);
        }

        private void mPersonSummaryLink(object sender, string linkName)
        {
            fController.SelectSummaryLink(linkName);
        }

        private void tbLoadMRU_Click(object sender, EventArgs e)
        {
            if (MenuMRU.Items.Count > 0) {
                MenuMRU.Show(this);
            }
        }

        private void tbPedigree_Click(object sender, EventArgs e)
        {
            MenuPedigree.Show(this);
        }

        #endregion

        #region Basic function

        public GDMRecordType GetSelectedRecordType()
        {
            return fController.GetSelectedRecordType();
        }

        public IListViewEx GetRecordsViewByType(GDMRecordType recType)
        {
            return fController.GetRecordsViewByType(recType);
        }

        public IHyperView GetHyperViewByType(GDMRecordType recType)
        {
            return fController.GetHyperViewByType(recType);
        }

        public IListManager GetRecordsListManByType(GDMRecordType recType)
        {
            return fController.GetRecordsListManByType(recType);
        }

        public GDMRecord GetSelectedRecordEx()
        {
            return fController.GetSelectedRecordEx();
        }

        public GDMIndividualRecord GetSelectedPerson()
        {
            return fController.GetSelectedPerson();
        }

        public List<GDMRecord> GetContentList(GDMRecordType recType)
        {
            return fController.GetContentList(recType);
        }

        public void ApplyFilter(GDMRecordType recType = GDMRecordType.rtNone)
        {
            fController.ApplyFilter(recType);
        }

        public void SetExternalFilter(ExternalFilterHandler filterHandler,
                                      GDMRecordType recType = GDMRecordType.rtNone)
        {
            fController.SetExternalFilter(filterHandler, recType);
        }

        public void SaveFileEx(bool saveAs)
        {
            fController.SaveFileEx(saveAs);
        }

        public void CheckAutosave()
        {
            fController.CheckAutosave();
        }

        public bool CheckModified()
        {
            bool result = true;
            if (!fContext.Modified) return result;

            DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxType.Warning);
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

        public void Clear()
        {
            fController.Clear();
        }

        public void CreateNewFile()
        {
            fController.CreateNewFile();
        }

        public void LoadFile(string fileName)
        {
            fController.LoadFile(fileName);
        }

        public void SaveFile(string fileName)
        {
            fController.SaveFile(fileName);
        }

        public void RefreshLists(bool columnsChanged)
        {
            fController.RefreshLists(columnsChanged);
        }

        public void RefreshRecordsView(GDMRecordType recType)
        {
            fController.RefreshRecordsView(recType);
        }

        public void UpdateChangedRecords(GDMRecord select = null)
        {
            fController.UpdateChangedRecords(select);
        }

        public void NotifyRecord(GDMRecord record, RecordAction action)
        {
            fController.NotifyRecord(record, action);
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            fController.SetFilter();
        }

        public void ShowMedia(GDMMultimediaRecord mediaRec, bool modal)
        {
            if (mediaRec == null)
                throw new ArgumentNullException("mediaRec");

            GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            if (fileRef == null) return;

            if (!GKUtils.UseEmbeddedViewer(fileRef.MultimediaFormat)) {
                string targetFile = fContext.MediaLoad(fileRef);
                GKUtils.LoadExtFile(targetFile);
            } else {
                //var mediaViewer = AppHost.Container.Resolve<IMediaViewerWin>(this);
                MediaViewerWin mediaViewer = new MediaViewerWin(this);
                try {
                    try {
                        mediaViewer.MultimediaRecord = mediaRec;
                        mediaViewer.FileReference = fileRef;
                        if (modal) {
                            mediaViewer.Show();
                        } else {
                            mediaViewer.ShowInTaskbar = true;
                            mediaViewer.Show();
                        }
                    } finally {
                        if (modal) mediaViewer.Dispose();
                    }
                } catch (Exception ex) {
                    if (mediaViewer != null) mediaViewer.Dispose();
                    Logger.WriteError("BaseWinSDI.ShowMedia()", ex);
                }
            }
        }

        #endregion

        #region ILocalizable implementation

        public override void SetLocale()
        {
            fController.SetLocale();

            int num = miPlugins.Items.Count;
            for (int i = 0; i < num; i++) {
                MenuItem mi = miPlugins.Items[i];
                IPlugin plugin = (IPlugin)mi.Tag;
                mi.Text = plugin.DisplayName;
            }

            tabsRecords.Pages[ 0].Text = LangMan.LS(LSID.LSID_RPIndividuals);
            tabsRecords.Pages[ 1].Text = LangMan.LS(LSID.LSID_RPFamilies);
            tabsRecords.Pages[ 2].Text = LangMan.LS(LSID.LSID_RPNotes);
            tabsRecords.Pages[ 3].Text = LangMan.LS(LSID.LSID_RPMultimedia);
            tabsRecords.Pages[ 4].Text = LangMan.LS(LSID.LSID_RPSources);
            tabsRecords.Pages[ 5].Text = LangMan.LS(LSID.LSID_RPRepositories);
            tabsRecords.Pages[ 6].Text = LangMan.LS(LSID.LSID_RPGroups);
            tabsRecords.Pages[ 7].Text = LangMan.LS(LSID.LSID_RPResearches);
            tabsRecords.Pages[ 8].Text = LangMan.LS(LSID.LSID_RPTasks);
            tabsRecords.Pages[ 9].Text = LangMan.LS(LSID.LSID_RPCommunications);
            tabsRecords.Pages[10].Text = LangMan.LS(LSID.LSID_RPLocations);
        }

        #endregion

        #region IWorkWindow implementation

        void IWorkWindow.UpdateControls()
        {
            string statusLine = "";
            GDMRecordType recType = GetSelectedRecordType();
            IListViewEx rView = GetRecordsViewByType(recType);
            if (rView != null) {
                var listMan = rView.ListMan;
                statusLine = LangMan.LS(LSID.LSID_SBRecords) + ": " + listMan.TotalCount.ToString();
                statusLine = statusLine + ", " + LangMan.LS(LSID.LSID_SBFiltered) + ": " + listMan.FilteredCount.ToString();
            }

            panStatusText.Text = statusLine;
        }

        void IWorkWindow.UpdateSettings()
        {
            fController.UpdateSettings();
        }

        void IWorkWindow.NavNext()
        {
            fController.NavNext();
        }

        void IWorkWindow.NavPrev()
        {
            fController.NavPrev();
        }

        bool IWorkWindow.NavCanBackward()
        {
            return fController.NavCanBackward();
        }

        bool IWorkWindow.NavCanForward()
        {
            return fController.NavCanForward();
        }

        public bool AllowQuickSearch()
        {
            return true;
        }

        IList<ISearchResult> IWorkWindow.FindAll(string searchPattern)
        {
            return fController.FindAll(searchPattern);
        }

        void IWorkWindow.SelectByRec(GDMRecord record)
        {
            fController.SelectByRec(record);
        }

        void IWorkWindow.QuickSearch()
        {
            if (!AllowQuickSearch()) return;

            QuickSearchDlg qsDlg = new QuickSearchDlg(this);

            Rectangle client = Bounds;
            qsDlg.Location = new Point(client.Left, client.Bottom - qsDlg.Height);

            qsDlg.Show();
        }

        #endregion

        #region Record Management

        public void DuplicateRecord()
        {
            fController.DuplicateRecord();
        }

        public void AddRecord()
        {
            fController.AddRecord();
        }

        public void EditRecord()
        {
            fController.EditRecord();
        }

        public void DeleteRecord()
        {
            fController.DeleteRecord();
        }

        public void ShowRecordsTab(GDMRecordType recType)
        {
            tabsRecords.SelectedIndex = (int)recType - 1;
        }

        public void SelectRecordByXRef(string xref, bool delayedTransition = false)
        {
            fController.SelectRecordByXRef(xref, delayedTransition);
        }

        public StringList GetRecordContent(GDMRecord record)
        {
            return fController.GetRecordContent(record);
        }

        public bool RecordIsFiltered(GDMRecord record)
        {
            return fController.RecordIsFiltered(record);
        }

        #endregion

        #region From MainWin

        public void Restore()
        {
            if (WindowState == Eto.Forms.WindowState.Minimized) {
                WindowState = Eto.Forms.WindowState.Normal;
            }
        }

        private void Form_DragEnter(object sender, DragEventArgs e)
        {
            var files = e.Data.Uris;
            e.Effects = (files != null) ? DragEffects.Copy : DragEffects.None;
        }

        private void Form_DragDrop(object sender, DragEventArgs e)
        {
            try {
                try {
                    AppHost.Instance.BeginLoading();

                    var files = e.Data.Uris;
                    if (files == null) return;

                    for (int i = 0; i < files.Length; i++) {
                        var uri = files[i];
                        if (!uri.IsFile) continue;
                        string fn = uri.AbsolutePath;
                        AppHost.Instance.LoadBase(this, fn);
                    }
                } finally {
                    AppHost.Instance.EndLoading();
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.Form_DragDrop()", ex);
            }
        }

        void IBaseWindowView.LoadBase(string fileName)
        {
            Application.Instance.Invoke(delegate () {
                AppHost.Instance.LoadBase(this, fileName);
            });
        }

        private void UpdateShieldState()
        {
            Bitmap img = (Bitmap)((ImageHandler)fController.GetShieldImage()).Handle;
            if (img != null) {
                panStatusShieldImage.Image = img;
            }
        }

        private void StatusBar_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            fContext.SwitchShieldState();
            UpdateShieldState();
            e.Handled = true;
        }

        private void MRUFileClick(object sender, EventArgs e)
        {
            int idx = (int)((ButtonMenuItem)sender).Tag;
            AppHost.Instance.LoadBase(this, AppHost.Options.MRUFiles[idx].FileName);
        }

        public void UpdateMRU()
        {
            try {
                miMRUFiles.Enabled = (AppHost.Options.MRUFiles.Count > 0);
                miMRUFiles.Items.Clear();
                MenuMRU.Items.Clear();

                int num = AppHost.Options.MRUFiles.Count;
                for (int i = 0; i < num; i++) {
                    string fn = AppHost.Options.MRUFiles[i].FileName;

                    MenuItemEx mi = new MenuItemEx(fn, i);
                    mi.Click += MRUFileClick;
                    miMRUFiles.Items.Add(mi);

                    MenuItemEx tsmi = new MenuItemEx(fn, i);
                    tsmi.Click += MRUFileClick;
                    MenuMRU.Items.Add(tsmi);
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.UpdateMRU()", ex);
            }
        }

        public void UpdateNavControls()
        {
            try {
                IWorkWindow workWin = this;

                tbPrev.Enabled = (workWin != null && workWin.NavCanBackward());
                tbNext.Enabled = (workWin != null && workWin.NavCanForward());
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.UpdateNavControls()", ex);
            }
        }

        public void UpdateControls(bool forceDeactivate, bool blockDependent = false)
        {
            try {
                IWorkWindow workWin = AppHost.Instance.GetWorkWindow();
                IBaseWindow curBase = ((forceDeactivate) ? null : AppHost.Instance.GetCurrentFile());
                IChartWindow curChart = ((workWin is IChartWindow) ? ((IChartWindow) workWin) : null);

                GDMRecordType rt = (curBase == null) ? GDMRecordType.rtNone : curBase.GetSelectedRecordType();
                bool baseEn = (rt != GDMRecordType.rtNone);

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

                miTreeTools.Enabled = baseEn;
                miExportToFamilyBook.Enabled = baseEn;
                miExportToTreesAlbum.Enabled = baseEn;
                miExportToExcelFile.Enabled = baseEn;
                miFileClose.Enabled = baseEn;
                miFileProperties.Enabled = baseEn;
                miOrganizer.Enabled = baseEn;
                miSlideshow.Enabled = baseEn;
                miScripts.Enabled = baseEn;

                bool indivEn = baseEn && rt == GDMRecordType.rtIndividual;

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

                UpdateNavControls();

                if (workWin != null && !blockDependent) {
                    workWin.UpdateControls();
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.UpdateControls()", ex);
            }
        }

        private void miExit_Click(object sender, EventArgs e)
        {
            AppHost.Instance.Quit();
        }

        private void miUndo_Click(object sender, EventArgs e)
        {
            fController.Undo();
        }

        private void miRedo_Click(object sender, EventArgs e)
        {
            fController.Redo();
        }

        private void miExportToFamilyBook_Click(object sender, EventArgs e)
        {
            fController.ExportToFamilyBook();
        }

        private void miExportToTreesAlbum_Click(object sender, EventArgs e)
        {
            fController.ExportToTreesAlbum();
        }

        private void miExportToExcelFile_Click(object sender, EventArgs e)
        {
            fController.ExportToExcelFile();
        }

        private void miFileProperties_Click(object sender, EventArgs e)
        {
            fController.ShowFileProperties();
        }

        private void miScripts_Click(object sender, EventArgs e)
        {
            fController.ShowScripts();
        }

        private void miTTTreeSplit_Click(object sender, EventArgs e)
        {
            fController.ShowTreeSplit();
        }

        private void miTTTreeMerge_Click(object sender, EventArgs e)
        {
            fController.ShowTreeMerge();
        }

        private void miTTTreeCompare_Click(object sender, EventArgs e)
        {
            fController.ShowTreeCompare();
        }

        private void miTTTreeCheck_Click(object sender, EventArgs e)
        {
            fController.ShowTreeCheck();
        }

        private void miTTRecMerge_Click(object sender, EventArgs e)
        {
            fController.ShowRecMerge(null, null);
        }

        private void miTTPlacesManager_Click(object sender, EventArgs e)
        {
            fController.ShowPlacesManager();
        }

        private void miTTPatSearch_Click(object sender, EventArgs e)
        {
            fController.ShowPatSearch();
        }

        private void miTTFamilyGroups_Click(object sender, EventArgs e)
        {
            fController.ShowFamilyGroups();
        }

        private void miOptions_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowOptions();
        }

        private void miFileClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void miFileNew_Click(object sender, EventArgs e)
        {
            fController.NewFile();
        }

        private void miFileLoad_Click(object sender, EventArgs e)
        {
            fController.LoadFileEx();
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
            fController.SetFilter();
        }

        private void tbPrev_Click(object sender, EventArgs e)
        {
            fController.NavPrev();
        }

        private void tbNext_Click(object sender, EventArgs e)
        {
            fController.NavNext();
        }

        private void tbSendMail_Click(object sender, EventArgs e)
        {
            fController.SendMail();
        }

        private void miMap_Click(object sender, EventArgs e)
        {
            fController.ShowMap();
        }

        private void miOrganizer_Click(object sender, EventArgs e)
        {
            fController.ShowOrganizer();
        }

        private void miRelationshipCalculator_Click(object sender, EventArgs e)
        {
            fController.ShowRelationshipCalculator();
        }

        private void miSlideshow_Click(object sender, EventArgs e)
        {
            fController.ShowSlideshow();
        }

        private void miStats_Click(object sender, EventArgs e)
        {
            fController.ShowStats();
        }

        private void miPedigreeAscend_Click(object sender, EventArgs e)
        {
            fController.GeneratePedigree(PedigreeExporter.PedigreeKind.Ascend);
        }

        private void miPedigree_dAbovilleClick(object sender, EventArgs e)
        {
            fController.GeneratePedigree(PedigreeExporter.PedigreeKind.Descend_dAboville);
        }

        private void miPedigree_KonovalovClick(object sender, EventArgs e)
        {
            fController.GeneratePedigree(PedigreeExporter.PedigreeKind.Descend_Konovalov);
        }

        private void miTreeAncestors_Click(object sender, EventArgs e)
        {
            fController.ShowTreeChart(TreeChartKind.ckAncestors);
        }

        private void miTreeDescendants_Click(object sender, EventArgs e)
        {
            fController.ShowTreeChart(TreeChartKind.ckDescendants);
        }

        private void miTreeBoth_Click(object sender, EventArgs e)
        {
            fController.ShowTreeChart(TreeChartKind.ckBoth);
        }

        private void miAncestorsCircle_Click(object sender, EventArgs e)
        {
            fController.ShowCircleChart(CircleChartType.Ancestors);
        }

        private void miDescendantsCircle_Click(object sender, EventArgs e)
        {
            fController.ShowCircleChart(CircleChartType.Descendants);
        }

        private void miLogSend_Click(object sender, EventArgs e)
        {
            fController.SendLog();
        }

        private void miLogView_Click(object sender, EventArgs e)
        {
            fController.ShowLog();
        }

        private void miAbout_Click(object sender, EventArgs e)
        {
            fController.ShowAbout();
        }

        private void miContext_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowHelpTopic("");
        }

        #endregion
    }
}
