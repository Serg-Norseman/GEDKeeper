/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Search;
using GKUI.Components;
using Terminal.Gui.App;
using Terminal.Gui.Drawing;
using Terminal.Gui.Input;
using Terminal.Gui.ViewBase;
using Terminal.Gui.Views;

namespace GKUI.Forms
{
    public sealed partial class BaseWinSDI : CommonWindow, IBaseWindowView
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private StatusBar StatusBar;
        private Label lblStatusLine;
        private MenuItem tbPrev; // tb->menu
        private MenuItem tbNext; // tb->menu
        private MenuItem tbSendMail; // tb->menu
        private MenuBar MainMenu1;
        private MenuBarItem miFile;
        private MenuItem miFileNew;
        private MenuItem miFileLoad;
        private MenuItem miFileReload;
        private MenuItem miMRUFiles;
        private MenuItem miFileSave;
        private MenuItem miFileSaveAs;
        private MenuItem miFileClose;
        private MenuItem miFileProperties;
        private MenuItem miExportTable;
        private MenuItem miExportToFamilyBook;
        private MenuItem miExportToTreesAlbum;
        private MenuItem miTreeTools;
        private MenuItem miExit;
        private MenuBarItem miEdit;
        private MenuItem miRecordAdd;
        private MenuItem miRecordEdit;
        private MenuItem miRecordDelete;
        private MenuItem miSearch;
        private MenuItem miFindAndReplace;
        private MenuItem miFilter;
        private MenuItem miOptions;
        private MenuBarItem miPedigree;
        private MenuItem miTreeAncestors;
        private MenuItem miTreeDescendants;
        private MenuItem miPedigreeDescend;
        //private MenuItem miMap;
        private MenuItem miStats;
        private MenuBarItem miHelp;
        private MenuItem miContext;
        private MenuItem miLogSend;
        private MenuItem miLogView;
        private MenuItem miAbout;
        //private ContextMenu MenuMRU;
        //private ContextMenu MenuPedigree;
        private MenuItem miPedigree_dAboville2;
        private MenuItem miPedigree_Konovalov2;
        private MenuItem miOrganizer;
        private MenuBarItem miService;
        private MenuItem miScripts;
        private MenuItem miExport;
        private MenuItem miTreeBoth;
        //private MenuItem miAncestorsCircle;
        private MenuBarItem miReports;
        private MenuBarItem miPlugins;
        //private MenuItem miSlideshow;
        private MenuItem miPedigreeAscend;
        //private MenuItem miDescendantsCircle;
        private MenuItem miRelationshipCalculator;
        private TabView tabsRecords;
        private MenuItem miContRecordDuplicate;
        private MenuItem miContRecordDelete;
        private MenuItem miContRecordEdit;
        private MenuItem miContRecordMerge;
        private MenuItem miContMediaMoveFile2Abs;
        private MenuItem miContMediaMoveFile2Rel;
        private MenuItem miContMediaMoveFile2Arc;
        private MenuItem miContMediaMoveFile;
        private PopoverMenu contextMenu;
        private MenuItem miContRecordAdd;
        private MenuItem miTreeCompare;
        private MenuItem miTreeMerge;
        private MenuItem miTreeSplit;
        private MenuItem miRecMerge;
        private MenuItem miFamilyGroups;
        private MenuItem miTreeCheck;
        private MenuItem miPatSearch;
        private MenuItem miPlacesManager;
        private PopoverMenu summaryMenu;
        private MenuItem miCopyContent;
        private MenuItem miExportToStrictGEDCOM;
        private MenuItem miChronicle;

        private Tab tab1;
        private Tab tab2;
        private Tab tab3;
        private Tab tab4;
        private Tab tab5;
        private Tab tab6;
        private Tab tab7;
        private Tab tab8;
        private Tab tab9;
        private Tab tab10;
        private Tab tab11;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region Private fields

        private readonly BaseWinController fController;

        private readonly BaseContext fContext;

        #endregion

        #region Public properties

        public BaseContext Context
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
            fContext.ModifiedChanged += BaseContext_ModifiedChanged;

            tab1 = CreatePage("Individuals", GDMRecordType.rtIndividual);
            tab2 = CreatePage("Families", GDMRecordType.rtFamily);
            tab3 = CreatePage("Notes", GDMRecordType.rtNote);
            tab4 = CreatePage("Multimedia", GDMRecordType.rtMultimedia);
            tab5 = CreatePage("Sources", GDMRecordType.rtSource);
            tab6 = CreatePage("Repositories", GDMRecordType.rtRepository);
            tab7 = CreatePage("Groups", GDMRecordType.rtGroup);
            tab8 = CreatePage("Researches", GDMRecordType.rtResearch);
            tab9 = CreatePage("Tasks", GDMRecordType.rtTask);
            tab10 = CreatePage("Communications", GDMRecordType.rtCommunication);
            tab11 = CreatePage("Locations", GDMRecordType.rtLocation);

            fController.SetLocale();
        }

        private Tab CreatePage(string pageText, GDMRecordType recType)
        {
            var summary = new HyperView();
            summary.Border.BorderStyle = LineStyle.Single;
            summary.Height = Dim.Fill();
            summary.Width = Dim.Fill();
            //summary.OnLink += mPersonSummaryLink;

            //summary.ContextMenu = summaryMenu;
            summary.MouseEvent += (sender, args) => {
                if (args.Flags.HasFlag(MouseFlags.RightButtonReleased)) {
                    summaryMenu.SetPosition(args.Position);
                    Application.Popover.Show(summaryMenu);
                }
            };

            var recView = new GKListView();
            recView.Height = Dim.Fill();
            recView.Width = Dim.Fill();
            //recView.AllowsMarking = false;
            recView.MultiSelect = true;
            //recView.MouseDoubleClick += miRecordEdit_Click;
            recView.SelectedCellChanged += List_SelectedIndexChanged;
            recView.UpdateContents();
            recView.ListMan = RecordsListModel<GDMRecord>.Create(fContext, recType, false);

            //recView.ContextMenu = contextMenu;
            recView.MouseEvent += (sender, args) => {
                if (args.Flags.HasFlag(MouseFlags.RightButtonReleased)) {
                    contextMenu.SetPosition(args.Position);
                    Application.Popover.Show(contextMenu);
                }
            };

            var left = new FrameView() {
                X = 0,
                Height = Dim.Fill(),
                Width = Dim.Percent(70),
            };
            left.Add(recView);

            var right = new FrameView() {
                X = Pos.Right(left) + 1,
                Height = Dim.Fill(),
                Width = Dim.Percent(30)
            };
            right.Add(summary);

            var container = new View();
            container.Border.BorderStyle = LineStyle.None;
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

            var tabPage = new Tab();
            tabPage.DisplayText = pageText;
            tabPage.View = container;
            tabsRecords.AddTab(tabPage, false);

            fController.SetTabPart(recType, recView, recType.ToString(), summary);

            return tabPage;
        }

        #endregion

        #region Form handlers

        /*private void Form_Activated(Toplevel top)
        {
            // FIXME
            AppHost.Instance.BaseChanged(this);
        }

        private void Form_Deactivate(Toplevel top)
        {
            // FIXME
            AppHost.Instance.BaseChanged(null);
        }*/

        protected override void OnLoading()
        {
            try {
                ((IWorkWindow)this).UpdateSettings();

                fController.UpdatePluginsItems();
                UpdateMRU();
                fController.UpdateControls(false);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.Form_Load()", ex);
            }

            base.OnLoading();
        }

        protected override bool OnClosing()
        {
            bool cancel = !CheckModified();
            if (cancel) return true;

            fController.SaveListsSettings();
            AppHost.Instance.BaseClosed(this);

            return base.OnClosing();
        }

        /*private void Form_Closed(Toplevel top)
        {
        }*/

        void IBaseWindowView.EnableSplitterEvent(object controlHandler, bool enable)
        {
            // dummy
        }

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            fController.SetMainTitle();
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

        private void miRecordDuplicate_Click()
        {
            DuplicateRecord();
        }

        private void miRecordMerge_Click()
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

        private void miContMediaMoveFile_Click()
        {
            /*MediaStoreType storeType;
            if (sender == miContMediaMoveFile2Abs) {
                storeType = MediaStoreType.mstReference;
            } else if (sender == miContMediaMoveFile2Rel) {
                storeType = MediaStoreType.mstRelativeReference;
            } else if (sender == miContMediaMoveFile2Arc) {
                storeType = MediaStoreType.mstArchive;
            } else {
                return;
            }

            var recView = GetRecordsViewByType(GetSelectedRecordType()) as GKListView;
            if (recView != null) {
                var items = recView.GetSelectedItems();
                fController.MoveMediaFiles(items, storeType);
            }*/
        }

        private void List_SelectedIndexChanged(object sender, SelectedCellChangedEventArgs e)
        {
            if (sender is GKListView listView) {
                fController.ChangeListItem(listView);
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

        private void miCopyContent_Click()
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

            var dialogResult = MessageBox.Query(Application.Instance, GKData.APP_TITLE, LangMan.LS(LSID.FileSaveQuery), "Yes", "No", LangMan.LS(LSID.DlgCancel));
            switch (dialogResult) {
                case 0: // Yes
                    SaveFileEx(false);
                    break;
                case 1: // No
                    break;
                case 2: // Cancel
                    result = false;
                    break;
            }

            return result;
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

        public void ShowMedia(string link, bool modal)
        {
            BaseController.ShowMedia(this, link, modal);
        }

        public void ShowMedia(GDMMultimediaRecord mediaRec, int fileNum, bool modal)
        {
            BaseController.ShowMedia(this, mediaRec, fileNum, modal);
        }

        #endregion

        #region ILocalizable implementation

        public override void SetLocale()
        {
            fController.SetLocale();
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

            //StatusBar.Items[0].Title = statusLine;
            lblStatusLine.Text = statusLine;
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
            var tabs = tabsRecords.Tabs.ToList();
            var selTab = tabs[(int)recType - 1];
            tabsRecords.SelectedTab = selTab;
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
            // not supported
        }

        /*private void Form_DragEnter(object sender, DragEventArgs e)
        {
            // not supported
        }

        private void Form_DragDrop(object sender, DragEventArgs e)
        {
            // not supported
        }*/

        void IBaseWindowView.LoadBase(string fileName)
        {
            /*Application.Instance.Invoke(delegate () {
                AppHost.Instance.LoadBase(this, fileName);
            });*/
        }

        private void UpdateShieldState()
        {
            // not supported
        }

        private void StatusBar_MouseDoubleClick(object sender, CommandEventArgs e)
        {
            fContext.SwitchShieldState();
            UpdateShieldState();
            e.Handled = true;
        }

        public void UpdateMRU()
        {
            try {
                int num = AppHost.Options.MRUFiles.Count;
                var subItems = new MenuItem[num];
                for (int i = 0; i < num; i++) {
                    string fn = AppHost.Options.MRUFiles[i].FileName;

                    int idx = i;
                    var mi = new MenuItem(fn, "", () => {
                        AppHost.Instance.LoadBase(this, AppHost.Options.MRUFiles[idx].FileName);
                    });
                    subItems[i] = mi;
                }
                miMRUFiles.Enabled = (num > 0);
                miMRUFiles.SubMenu = new Menu(subItems);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.UpdateMRU()", ex);
            }
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

        private void miExportTable_Click()
        {
            fController.ExportTable();
        }

        private void miExportToStrictGEDCOM_Click()
        {
            fController.ExportToStrictGEDCOM();
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
            BaseController.ShowRecMerge(this, this, null, null);
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

        private void miPhotosBatchAdding_Click()
        {
            fController.ShowPhotosBatchAdding();
        }

        private void miCleanImagesCache_Click()
        {
            AppHost.CleanImagesCache();
        }

        private void miOptions_Click()
        {
            AppHost.Instance.ShowOptions(this);
        }

        private void miFileClose_Click()
        {
            Close();
        }

        private void miFileNew_Click()
        {
            fController.NewFile();
        }

        private async void miFileLoad_Click()
        {
            //LoadFile();
            await fController.LoadFileEx();
        }

        private async void miFileReload_Click()
        {
            await fController.ReloadFile();
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

        private void miFindAndReplace_Click()
        {
            fController.FindAndReplace();
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

        private void tbPartialView_Click()
        {
            // not supported
            fController.ShowPartialView();
        }

        /*private void miMap_Click()
        {
            // not supported
            fController.ShowMap();
        }*/

        private void miChronicle_Click()
        {
            fController.ShowChronicle();
        }

        private void miOrganizer_Click()
        {
            fController.ShowOrganizer();
        }

        private void miRelationshipCalculator_Click()
        {
            fController.ShowRelationshipCalculator();
        }

        /*private void miSlideshow_Click()
        {
            // not supported
            fController.ShowSlideshow();
        }*/

        private void miStats_Click()
        {
            fController.ShowStats();
        }

        private void miPedigreeAscend_Click()
        {
            fController.GeneratePedigree(PedigreeType.Ascend);
        }

        private void miPedigreeDescend_Click()
        {
            fController.GeneratePedigree(PedigreeType.Descend);
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
            BaseController.ShowCircleChart(this, CircleChartType.Ancestors);
        }

        private void miDescendantsCircle_Click()
        {
            BaseController.ShowCircleChart(this, CircleChartType.Descendants);
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
