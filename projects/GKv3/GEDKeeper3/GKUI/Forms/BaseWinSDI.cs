/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;
using GKUI.Platform;
using GKUI.Platform.Handlers;
using GKUI.Themes;

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
        private GKDropDownToolItem tbPedigree;
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
        private ButtonMenuItem miExportTable;
        private ButtonMenuItem miExportToFamilyBook;
        private ButtonMenuItem miExportToTreesAlbum;
        private ButtonMenuItem miTreeTools;
        private ButtonMenuItem miExit;
        private ButtonMenuItem miEdit;
        private ButtonMenuItem miRecordAdd;
        private ButtonMenuItem miRecordEdit;
        private ButtonMenuItem miRecordDelete;
        private ButtonMenuItem miSearch;
        private ButtonMenuItem miFindAndReplace;
        private ButtonMenuItem miFilter;
        private ButtonMenuItem miOptions;
        private ButtonMenuItem miPedigree;
        private ButtonMenuItem miTreeAncestors;
        private ButtonMenuItem miTreeDescendants;
        private ButtonMenuItem miPedigreeAscend;
        private ButtonMenuItem miPedigreeDescend;
        private ButtonMenuItem miMap;
        private ButtonMenuItem miStats;
        private ButtonMenuItem miHelp;
        private ButtonMenuItem miContext;
        private ButtonMenuItem miLogSend;
        private ButtonMenuItem miLogView;
        private ButtonMenuItem miAbout;
        private ContextMenu MenuMRU;
        private ContextMenu MenuPedigree;
        private ButtonMenuItem miPedigreeAscend2;
        private ButtonMenuItem miPedigreeDescend2;
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
        private GKDropDownToolItem tbLoadMRU;
        private ButtonMenuItem miDescendantsCircle;
        private ButtonMenuItem miRelationshipCalculator;
        private TabControl tabsRecords;
        private ButtonMenuItem miContRecordDuplicate;
        private ButtonMenuItem miContRecordDelete;
        private ButtonMenuItem miContRecordEdit;
        private ButtonMenuItem miContRecordMerge;
        private ButtonMenuItem miContMediaMoveFile2Abs;
        private ButtonMenuItem miContMediaMoveFile2Rel;
        private ButtonMenuItem miContMediaMoveFile2Arc;
        private ButtonMenuItem miContMediaMoveFile2Stg;
        private ButtonMenuItem miContMediaMoveFile;
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
        private ButtonMenuItem miThemes;
        private ButtonMenuItem miWindow;
        private ButtonMenuItem miWinCascade;
        private ButtonMenuItem miWinHTile;
        private ButtonMenuItem miWinVTile;
        private ButtonMenuItem miWinMinimize;
        private ContextMenu summaryMenu;
        private ButtonMenuItem miCopyContent;
        private ButtonMenuItem miPhotosBatchAdding;
        private ButtonMenuItem miCleanImagesCache;
        private ButtonToolItem tbPartialView;

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

        public NavigationStack<GDMRecord> Navman
        {
            get { return fController.Navman; }
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

            fController = new BaseWinController(this, true);
            fContext = fController.Context;
            ((BaseContext)fContext).ModifiedChanged += BaseContext_ModifiedChanged;

            tabsRecords.SuspendLayout();
            CreatePage("Individuals", GDMRecordType.rtIndividual);
            CreatePage("Families", GDMRecordType.rtFamily);
            CreatePage("Notes", GDMRecordType.rtNote);
            CreatePage("Multimedia", GDMRecordType.rtMultimedia);
            CreatePage("Sources", GDMRecordType.rtSource);
            CreatePage("Repositories", GDMRecordType.rtRepository);
            CreatePage("Groups", GDMRecordType.rtGroup);
            CreatePage("Researches", GDMRecordType.rtResearch);
            CreatePage("Tasks", GDMRecordType.rtTask);
            CreatePage("Communications", GDMRecordType.rtCommunication);
            CreatePage("Locations", GDMRecordType.rtLocation);
            tabsRecords.ResumeLayout();
            tabsRecords.SelectedIndex = 0;

            fController.SetLocale();
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
            miContMediaMoveFile2Abs = new ButtonMenuItem(miContMediaMoveFile_Click);
            miContMediaMoveFile2Rel = new ButtonMenuItem(miContMediaMoveFile_Click);
            miContMediaMoveFile2Arc = new ButtonMenuItem(miContMediaMoveFile_Click);
            miContMediaMoveFile2Stg = new ButtonMenuItem(miContMediaMoveFile_Click);
            miContMediaMoveFile = new ButtonMenuItem();
            miContMediaMoveFile.Items.AddRange(new MenuItem[] { miContMediaMoveFile2Abs, miContMediaMoveFile2Rel, miContMediaMoveFile2Arc, miContMediaMoveFile2Stg });

            miContRecordAdd = new ButtonMenuItem(miRecordAdd_Click);
            miContRecordEdit = new ButtonMenuItem(miRecordEdit_Click);
            miContRecordDelete = new ButtonMenuItem(miRecordDelete_Click);
            miContRecordDuplicate = new ButtonMenuItem(miRecordDuplicate_Click);
            miContRecordMerge = new ButtonMenuItem(miRecordMerge_Click);
            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] { miContRecordAdd, miContRecordEdit, miContRecordDelete, miContRecordDuplicate, miContRecordMerge, miContMediaMoveFile });
            contextMenu.Opening += contextMenu_Opening;

            miCopyContent = new ButtonMenuItem(miCopyContent_Click);
            summaryMenu = new ContextMenu();
            summaryMenu.Items.AddRange(new MenuItem[] { miCopyContent });
        }

        private void CreatePage(string pageText, GDMRecordType recType)
        {
            var summary = new HyperView();
            summary.BorderWidth = 4;
            summary.OnLink += mPersonSummaryLink;
            summary.ContextMenu = summaryMenu;

            var recView = new GKListView();
            recView.AllowMultipleSelection = true;
            recView.MouseDoubleClick += miRecordEdit_Click;
            recView.SelectedItemsChanged += List_SelectedIndexChanged;
            recView.KeyDown += Form_KeyDown;
            recView.ContextMenu = contextMenu;
            recView.ListMan = RecordsListModel<GDMRecord>.Create(fContext, recType, false);
            recView.UpdateContents();

            var spl = new Splitter();
            spl.ID = "splitter" + ((int)recType).ToString();
            spl.Panel1 = recView;
            spl.Panel2 = summary;
            spl.RelativePosition = 300;
            spl.Orientation = Orientation.Horizontal;
            spl.FixedPanel = SplitterFixedPanel.Panel2;
            spl.PositionChanged += Spl_PositionChanged;

            var tabPage = new TabPage();
            tabPage.Text = pageText;
            tabPage.Content = spl;
            tabsRecords.Pages.Add(tabPage);

            fController.SetTabPart(recType, recView, spl.ID, summary);
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

                UpdateShieldState();
                fController.UpdatePluginsItems();
                UpdateThemesItems();
                UpdateMRU();
                fController.UpdateControls(false);
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
            AppHost.Instance.CloseDependentWindows(this);
        }

        private void Spl_PositionChanged(object sender, EventArgs e)
        {
            fController.SetSummaryWidth(true);
        }

        void IBaseWindowView.EnableSplitterEvent(object controlHandler, bool enable)
        {
            var handler = controlHandler as SplitterHandler;
            if (handler == null) return;

            var splitter = handler.Control as Splitter;
            if (splitter != null) {
                if (enable) {
                    splitter.PositionChanged += Spl_PositionChanged;
                } else {
                    splitter.PositionChanged -= Spl_PositionChanged;
                }
            }
        }

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            fController.SetMainTitle();
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
                        e.Handled = true;
                    }
                    break;

                case Keys.Home:
                case Keys.End:
                    if (sender is GKListView) {
                        var listView = sender as GKListView;
                        if (e.Key == Keys.Home) {
                            listView.SelectedIndex = 0;
                        } else {
                            listView.SelectedIndex = -1;
                        }
                        e.Handled = true;
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
            var recType = GetSelectedRecordType();
            miContRecordDuplicate.Enabled = (recType == GDMRecordType.rtIndividual || recType == GDMRecordType.rtLocation);

            miContMediaMoveFile.Visible = (recType == GDMRecordType.rtMultimedia);
            miContMediaMoveFile2Abs.Enabled = false;
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
                BaseController.ShowRecMerge(this, this,
                    items.Count > 0 ? items[0] as GDMRecord : null,
                    items.Count > 1 ? items[1] as GDMRecord : null
                );
            }
        }

        private void miContMediaMoveFile_Click(object sender, EventArgs e)
        {
            MediaStoreType storeType;
            if (sender == miContMediaMoveFile2Abs) {
                storeType = MediaStoreType.mstReference;
            } else if (sender == miContMediaMoveFile2Rel) {
                storeType = MediaStoreType.mstRelativeReference;
            } else if (sender == miContMediaMoveFile2Arc) {
                storeType = MediaStoreType.mstArchive;
            } else if (sender == miContMediaMoveFile2Stg) {
                storeType = MediaStoreType.mstStorage;
            } else {
                return;
            }

            var recView = GetRecordsViewByType(GetSelectedRecordType()) as GKListView;
            if (recView != null) {
                var items = recView.GetSelectedItems();
                fController.MoveMediaFiles(items, storeType);
            }
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (sender != null) {
                fController.ChangeListItem((IListView)sender);
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
            fController.SelectSummaryLink((IHyperView)sender, linkName);
        }

        private void miCopyContent_Click(object sender, EventArgs e)
        {
            fController.CopyContent();
        }

        #endregion

        #region Basic function

        public GDMRecordType GetSelectedRecordType()
        {
            return fController.GetSelectedRecordType();
        }

        public IListView GetRecordsViewByType(GDMRecordType recType)
        {
            return fController.GetRecordsViewByType(recType);
        }

        public IHyperView GetHyperViewByType(GDMRecordType recType)
        {
            return fController.GetHyperViewByType(recType);
        }

        public IRecordsListModel GetRecordsListManByType(GDMRecordType recType)
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

            DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxType.Warning);
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
            BaseController.ShowMedia(this, mediaRec, modal);
        }

        #endregion

        #region ILocalizable implementation

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        #endregion

        #region IThemedView implementation

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        #endregion

        #region IWorkWindow implementation

        void IWorkWindow.UpdateControls()
        {
            string statusLine = "";
            GDMRecordType recType = GetSelectedRecordType();
            IListView rView = GetRecordsViewByType(recType);
            if (rView != null) {
                var listMan = rView.ListMan;
                statusLine = LangMan.LS(LSID.SBRecords) + ": " + listMan.TotalCount.ToString();
                statusLine = statusLine + ", " + LangMan.LS(LSID.SBFiltered) + ": " + listMan.FilteredCount.ToString();
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

        public StringList GetRecordContent(GDMRecord record, RecordContentType contentType)
        {
            return fController.GetRecordContent(record, contentType);
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

        private void ThemeClick(object sender, EventArgs e)
        {
            var mItem = (CheckMenuItem)sender;
            foreach (CheckMenuItem mi in ((ButtonMenuItem)mItem.Parent).Items) {
                mi.Checked = mItem == mi;
            }

            var theme = (Theme)mItem.Tag;
            AppHost.Instance.ApplyTheme(theme.Name);
        }

        private void UpdateThemesItems()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes))
                return;

            try {
                string curTheme = GlobalOptions.Instance.Theme;

                miThemes.Items.Clear();

                var themes = AppHost.ThemeManager.Themes;
                int num = themes.Count;
                for (int i = 0; i < num; i++) {
                    var theme = themes[i];

                    var mi = new CheckMenuItem();
                    mi.Text = theme.Name;
                    mi.Tag = theme;
                    mi.Click += ThemeClick;
                    miThemes.Items.Add(mi);

                    if ((i == 0 && string.IsNullOrEmpty(curTheme)) || (theme.Name == curTheme)) {
                        mi.Checked = true;
                    }
                }

                miThemes.Enabled = (miThemes.Items.Count > 0);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.UpdateThemesItems()", ex);
            }
        }

        public void UpdateControls(bool forceDeactivate, bool blockDependent = false)
        {
            fController.UpdateControls(forceDeactivate, blockDependent);
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

        private void miExportTable_Click(object sender, EventArgs e)
        {
            fController.ExportTable();
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
            BaseController.ShowRecMerge(this, this, null, null);
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

        private void miPhotosBatchAdding_Click(object sender, EventArgs e)
        {
            fController.ShowPhotosBatchAdding();
        }

        private void miCleanImagesCache_Click(object sender, EventArgs e)
        {
            AppHost.CleanImagesCache();
        }

        private void miOptions_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowOptions(this);
        }

        private void miFileClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void miFileNew_Click(object sender, EventArgs e)
        {
            fController.NewFile();
        }

        private async void miFileLoad_Click(object sender, EventArgs e)
        {
            await fController.LoadFileEx();
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

        private void miFindAndReplace_Click(object sender, EventArgs e)
        {
            fController.FindAndReplace();
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

        private void tbPartialView_Click(object sender, EventArgs e)
        {
            fController.ShowPartialView();
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
            fController.GeneratePedigree(PedigreeType.Ascend);
        }

        private void miPedigreeDescend_Click(object sender, EventArgs e)
        {
            fController.GeneratePedigree(PedigreeType.Descend);
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
            BaseController.ShowCircleChart(this, CircleChartType.Ancestors);
        }

        private void miDescendantsCircle_Click(object sender, EventArgs e)
        {
            BaseController.ShowCircleChart(this, CircleChartType.Descendants);
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

        private void miWinCascade_Click(object sender, EventArgs e)
        {
            AppHost.Instance.LayoutWindows(WinLayout.Cascade);
        }

        private void miWinHTile_Click(object sender, EventArgs e)
        {
            AppHost.Instance.LayoutWindows(WinLayout.TileHorizontal);
        }

        private void miWinVTile_Click(object sender, EventArgs e)
        {
            AppHost.Instance.LayoutWindows(WinLayout.TileVertical);
        }

        private void miWinMinimize_Click(object sender, EventArgs e)
        {
            AppHost.Instance.LayoutWindows(WinLayout.Minimize);
        }

        #endregion
    }
}
