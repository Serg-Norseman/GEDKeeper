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
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    public sealed partial class BaseWinSDI : CommonWindow, IBaseWindowView
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private StatusBar StatusBar;
        private Label panStatusText;
        private MenuItem tbPrev; // tb->menu
        private MenuItem tbNext; // tb->menu
        private MenuItem tbSendMail; // tb->menu
        private MenuBar MainMenu1;
        private MenuBarItem miFile;
        private MenuItem miFileNew;
        private MenuItem miFileLoad;
        private MenuItem miMRUFiles;
        private MenuItem miFileSave;
        private MenuItem miFileSaveAs;
        private MenuItem miFileClose;
        private MenuItem miFileProperties;
        private MenuItem miExportToExcelFile;
        private MenuItem miExportToFamilyBook;
        private MenuItem miExportToTreesAlbum;
        private MenuItem miTreeTools;
        private MenuItem miExit;
        private MenuBarItem miEdit;
        private MenuItem miRecordAdd;
        private MenuItem miRecordEdit;
        private MenuItem miRecordDelete;
        private MenuItem miSearch;
        private MenuItem miFilter;
        private MenuItem miOptions;
        private MenuBarItem miPedigree;
        private MenuItem miTreeAncestors;
        private MenuItem miTreeDescendants;
        private MenuItem miPedigree_dAboville;
        private MenuItem miPedigree_Konovalov;
        private MenuItem miMap;
        private MenuItem miStats;
        private MenuBarItem miHelp;
        private MenuItem miContext;
        private MenuItem miLogSend;
        private MenuItem miLogView;
        private MenuItem miAbout;
        private ContextMenu MenuMRU;
        private ContextMenu MenuPedigree;
        private MenuItem miPedigree_dAboville2;
        private MenuItem miPedigree_Konovalov2;
        private MenuItem miOrganizer;
        private MenuBarItem miService;
        private MenuItem miScripts;
        private MenuItem miExport;
        private MenuItem miTreeBoth;
        private MenuItem miAncestorsCircle;
        private MenuBarItem miReports;
        private MenuBarItem miPlugins;
        private MenuItem miSlideshow;
        private MenuItem miPedigreeAscend;
        private MenuItem miDescendantsCircle;
        private MenuItem miRelationshipCalculator;
        private TabView tabsRecords;
        private MenuItem miContRecordDuplicate;
        private MenuItem miContRecordDelete;
        private MenuItem miContRecordEdit;
        private MenuItem miContRecordMerge;
        private ContextMenu contextMenu;
        private MenuItem miContRecordAdd;
        private MenuItem miTreeCompare;
        private MenuItem miTreeMerge;
        private MenuItem miTreeSplit;
        private MenuItem miRecMerge;
        private MenuItem miFamilyGroups;
        private MenuItem miTreeCheck;
        private MenuItem miPatSearch;
        private MenuItem miPlacesManager;

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
            InitializeComponent();

            fController = new BaseWinController(this, false);
            fContext = fController.Context;
            ((BaseContext)fContext).ModifiedChanged += BaseContext_ModifiedChanged;

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
        }

        private void LoadFile()
        {
            var fileName = AppHost.StdDialogs.GetOpenFile("GEDCOM File", GlobalOptions.Instance.LastDir, "GEDCOM files|*.ged", 0, "");
            if (!string.IsNullOrEmpty(fileName)) {
                fContext.FileLoad(fileName, false);
                var listView = GetRecordsViewByType(GDMRecordType.rtIndividual) as ListView;

                var indiList = new List<string>();
                foreach (var indiRec in fContext.Tree.GetRecords<GDMIndividualRecord>()) {
                    indiList.Add(GKUtils.GetNameString(indiRec, true, false));
                }

                listView.SetSource(indiList);
                AppHost.StdDialogs.ShowMessage("All ok");
            }
        }

        private void CreatePage(string pageText, GDMRecordType recType)
        {
            var recView = new GKListView();
            //recView.Y = 0;
            recView.Height = Dim.Fill();
            recView.Width = Dim./*Percent(70);// */Fill();
            recView.AllowsMarking = false;
            recView.AllowsMultipleSelection = true;
            /*recView.MouseDoubleClick += miRecordEdit_Click;
            recView.SelectedItemsChanged += List_SelectedIndexChanged;
            recView.UpdateContents();
            recView.ContextMenu = contextMenu;
            recView.ListMan = RecordsListModel<GDMRecord>.Create(fContext, recType);*/

            var summary = new HyperView();
            //summary.X = Pos.Right(recView) + 1;
            summary.Height = Dim.Fill();
            summary.Width = Dim.Fill();
            /*summary.BorderWidth = 4;
            summary.OnLink += mPersonSummaryLink;*/

            var left = new FrameView() {
                X = 0,
                Height = Dim.Fill(),
                Width = Dim.Percent(70)
            };
            left.Add(recView);

            var right = new FrameView() {
                X = Pos.Right(left) + 1,
                Height = Dim.Fill(),
                Width = Dim.Percent(30)
            };
            right.Add(summary);

            var container = new View();
            container.Height = Dim.Fill();
            container.Width = Dim.Fill();
            container.Add(left);
            container.Add(right);

            /*Splitter spl = new Splitter();
            spl.Panel1 = recView;
            spl.Panel2 = summary;
            spl.RelativePosition = 300;
            spl.Orientation = Orientation.Horizontal;
            spl.FixedPanel = SplitterFixedPanel.Panel2;*/

            TabView.Tab tabPage = new TabView.Tab(pageText, container);
            tabsRecords.AddTab(tabPage, false);

            fController.SetTabPart(recType, recView, summary);
        }

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            fController.SetMainTitle();
        }

        #endregion

        #region Form handlers

        private void Form_Activated(Toplevel top)
        {
            AppHost.Instance.BaseChanged(this);
        }

        private void Form_Deactivate(Toplevel top)
        {
            AppHost.Instance.BaseChanged(null);
        }

        private void Form_Load()
        {
            try {
                ((IWorkWindow)this).UpdateSettings();

                fController.UpdatePluginsItems();
                UpdateMRU();
                fController.UpdateControls(false);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.Form_Load()", ex);
            }
        }

        private void Form_Closing(ToplevelClosingEventArgs e)
        {
            e.Cancel = !CheckModified();
            if (e.Cancel) return;

            fController.SaveListsSettings();

            AppHost.Instance.BaseClosed(this);
        }

        private void Form_Closed(Toplevel top)
        {
        }

        /*private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.Key) {
                    /*case Keys.I:
						ItemAdd();
						break;
					case Keys.D:
						ItemDelete();
						break;*//*

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
        			break;*//*
            }
        }*/

        private void contextMenu_Opening(object sender, EventArgs e)
        {
            IListView recView = GetRecordsViewByType(GetSelectedRecordType());

            //miContRecordDuplicate.Enabled = (recView == fController.GetRecordsViewByType(GDMRecordType.rtIndividual));
        }

        private void miRecordAdd_Click()
        {
            AddRecord();
        }

        private void miRecordEdit_Click()
        {
            EditRecord();
        }

        private void miRecordDelete_Click()
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
                /*var items = recView.GetSelectedItems();
                fController.ShowRecMerge(
                    items.Count > 0 ? items[0] as GDMRecord : null,
                    items.Count > 1 ? items[1] as GDMRecord : null
                );*/
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
            /*if (!fContext.Modified) return result;

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
            }*/

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
                /*MediaViewerWin mediaViewer = new MediaViewerWin(this);
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
                }*/
            }
        }

        #endregion

        #region ILocalizable implementation

        public override void SetLocale()
        {
            /*fController.SetLocale();

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
            tabsRecords.Pages[10].Text = LangMan.LS(LSID.LSID_RPLocations);*/
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

            //panStatusText.Text = statusLine;
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
            /*if (!AllowQuickSearch()) return;

            QuickSearchDlg qsDlg = new QuickSearchDlg(this);

            Rectangle client = Bounds;
            qsDlg.Location = new Point(client.Left, client.Bottom - qsDlg.Height);

            qsDlg.Show();*/
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
            //tabsRecords.SelectedIndex = (int)recType - 1;
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
            /*if (WindowState == Eto.Forms.WindowState.Minimized) {
                WindowState = Eto.Forms.WindowState.Normal;
            }*/
        }

        /*private void Form_DragEnter(object sender, DragEventArgs e)
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
        }*/

        void IBaseWindowView.LoadBase(string fileName)
        {
            /*Application.Instance.Invoke(delegate () {
                AppHost.Instance.LoadBase(this, fileName);
            });*/
        }

        private void UpdateShieldState()
        {
            /*Bitmap img = (Bitmap)((ImageHandler)fController.GetShieldImage()).Handle;
            if (img != null) {
                panStatusShieldImage.Image = img;
            }*/
        }

        private void StatusBar_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            fContext.SwitchShieldState();
            UpdateShieldState();
            e.Handled = true;
        }

        private void MRUFileClick(object sender, EventArgs e)
        {
            /*int idx = (int)((ButtonMenuItem)sender).Tag;
            AppHost.Instance.LoadBase(this, AppHost.Options.MRUFiles[idx].FileName);*/
        }

        public void UpdateMRU()
        {
            /*try {
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
            }*/
        }

        public void UpdateControls(bool forceDeactivate, bool blockDependent = false)
        {
            fController.UpdateControls(forceDeactivate, blockDependent);
        }

        private void miExit_Click()
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

        private void miExportToFamilyBook_Click()
        {
            fController.ExportToFamilyBook();
        }

        private void miExportToTreesAlbum_Click()
        {
            fController.ExportToTreesAlbum();
        }

        private void miExportToExcelFile_Click()
        {
            fController.ExportToExcelFile();
        }

        private void miFileProperties_Click()
        {
            fController.ShowFileProperties();
        }

        private void miScripts_Click()
        {
            fController.ShowScripts();
        }

        private void miTTTreeSplit_Click()
        {
            fController.ShowTreeSplit();
        }

        private void miTTTreeMerge_Click()
        {
            fController.ShowTreeMerge();
        }

        private void miTTTreeCompare_Click()
        {
            fController.ShowTreeCompare();
        }

        private void miTTTreeCheck_Click()
        {
            fController.ShowTreeCheck();
        }

        private void miTTRecMerge_Click()
        {
            fController.ShowRecMerge(null, null);
        }

        private void miTTPlacesManager_Click()
        {
            fController.ShowPlacesManager();
        }

        private void miTTPatSearch_Click()
        {
            fController.ShowPatSearch();
        }

        private void miTTFamilyGroups_Click()
        {
            fController.ShowFamilyGroups();
        }

        private void miOptions_Click()
        {
            AppHost.Instance.ShowOptions();
        }

        private void miFileClose_Click()
        {
            Close();
        }

        private void miFileNew_Click()
        {
            fController.NewFile();
        }

        private void miFileLoad_Click()
        {
            LoadFile();
            //fController.LoadFileEx();
        }

        private void miFileSaveAs_Click()
        {
            SaveFileEx(true);
        }

        private void miFileSave_Click()
        {
            SaveFileEx(false);
        }

        private void miSearch_Click()
        {
            (this as IWorkWindow).QuickSearch();
        }

        private void miFilter_Click()
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

        private void miMap_Click()
        {
            fController.ShowMap();
        }

        private void miOrganizer_Click()
        {
            fController.ShowOrganizer();
        }

        private void miRelationshipCalculator_Click()
        {
            fController.ShowRelationshipCalculator();
        }

        private void miSlideshow_Click()
        {
            fController.ShowSlideshow();
        }

        private void miStats_Click()
        {
            fController.ShowStats();
        }

        private void miPedigreeAscend_Click()
        {
            fController.GeneratePedigree(PedigreeExporter.PedigreeKind.Ascend);
        }

        private void miPedigree_dAbovilleClick()
        {
            fController.GeneratePedigree(PedigreeExporter.PedigreeKind.Descend_dAboville);
        }

        private void miPedigree_KonovalovClick()
        {
            fController.GeneratePedigree(PedigreeExporter.PedigreeKind.Descend_Konovalov);
        }

        private void miTreeAncestors_Click()
        {
            fController.ShowTreeChart(TreeChartKind.ckAncestors);
        }

        private void miTreeDescendants_Click()
        {
            fController.ShowTreeChart(TreeChartKind.ckDescendants);
        }

        private void miTreeBoth_Click()
        {
            fController.ShowTreeChart(TreeChartKind.ckBoth);
        }

        private void miAncestorsCircle_Click()
        {
            fController.ShowCircleChart(CircleChartType.Ancestors);
        }

        private void miDescendantsCircle_Click()
        {
            fController.ShowCircleChart(CircleChartType.Descendants);
        }

        private void miLogSend_Click()
        {
            fController.SendLog();
        }

        private void miLogView_Click()
        {
            fController.ShowLog();
        }

        private void miAbout_Click()
        {
            fController.ShowAbout();
        }

        private void miContext_Click()
        {
            AppHost.Instance.ShowHelpTopic("");
        }

        #endregion
    }
}
