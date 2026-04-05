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
using Terminal.Gui;

namespace GKUI.Forms
{
    public sealed partial class BaseWinSDI : CommonWindow, IBaseWindowView
    {
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

        private TabPage CreatePage(string pageText, GDMRecordType recType)
        {
            var summary = new HyperView();
            summary.Border = new Border() { BorderStyle = BorderStyle.Single };
            summary.Height = Dim.Fill();
            summary.Width = Dim.Fill();
            summary.OnLink += mPersonSummaryLink;
            summary.ContextMenu = summaryMenu;

            var recView = new GKListView();
            recView.Height = Dim.Fill();
            recView.Width = Dim.Fill();
            recView.MultiSelect = true;
            recView.DoubleClick += miRecordEdit_Click;
            recView.SelectedCellChanged += List_SelectedIndexChanged;
            recView.UpdateContents();
            recView.ListMan = RecordsListModel<GDMRecord>.Create(fContext, recType, false);
            recView.KeyDown += Form_KeyDown;
            recView.ContextMenu = contextMenu;

            var strRecType = ((int)recType).ToString();

            var spl = new SplitterContainer(Orientation.Vertical, 70);
            spl.Panel1.Add(recView);
            spl.Panel2.Add(summary);
            spl.Id = "splitter" + strRecType;

            // works only after adding to some container
            recView.SetupScroll();

            var tabPage = new TabPage();
            tabPage.Text = pageText;
            tabPage.View.Add(spl);
            tabsRecords.AddTab(tabPage, false);

            fController.SetTabPart(recType, recView, spl.Id, summary);

            return tabPage;
        }

        #endregion

        #region Form handlers

        private void Form_Activated(object sender, Toplevel top)
        {
            AppHost.Instance.BaseChanged(this);
        }

        private void Form_Deactivate(object sender, Toplevel top)
        {
            AppHost.Instance.BaseChanged(null);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            // FIXME: dirty hack
            // This main form is created independently and added to `Top`, instead of Application.Run(new BaseWinSDI()).
            // Therefore, Running is not true for it. Because of this, RequestStop does not work properly when closing.
            Running = true;

            try {
                ((IWorkWindow)this).UpdateSettings();

                fController.UpdatePluginsItems();
                UpdateMRU();
                fController.UpdateControls(false);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.Form_Load()", ex);
            }
        }

        private void Form_Closing(object sender, ToplevelClosingEventArgs e)
        {
            e.Cancel = !CheckModified();
            if (e.Cancel) return;

            fController.SaveListsSettings();

            AppHost.Instance.BaseClosed(this);
        }

        private void Form_Closed(object sender, Toplevel top)
        {
        }

        void IBaseWindowView.EnableSplitterEvent(object controlHandler, bool enable)
        {
            // dummy
        }

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            fController.SetMainTitle();
        }

        private void Form_KeyDown(object sender, KeyEventEventArgs e)
        {
            if (e.KeyEvent.IsCtrl) {
                var key = e.KeyEvent.Key & ~Key.CtrlMask;
                switch (key) {
                    case Key.I:
                        AddRecord();
                        e.Handled = true;
                        break;

                    case Key.L:
                        DeleteRecord();
                        e.Handled = true;
                        break;

                    case Key.Enter:
                        EditRecord();
                        e.Handled = true;
                        break;

                    case Key.F12:
                        break;

                    case Key.F:
                        (this as IWorkWindow).QuickSearch();
                        e.Handled = true;
                        break;
                }
            }
        }

        private void contextMenu_Opening(object sender, EventArgs e)
        {
            IListView recView = GetRecordsViewByType(GetSelectedRecordType());

            //miContRecordDuplicate.Enabled = (recView == fController.GetRecordsViewByType(GDMRecordType.rtIndividual));
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
            var recView = GetRecordsViewByType(GetSelectedRecordType());
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

        private void List_SelectedIndexChanged(object sender, TableView.SelectedCellChangedEventArgs e)
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

            var dialogResult = MessageBox.Query(GKData.APP_TITLE, LangMan.LS(LSID.FileSaveQuery), "Yes", "No", LangMan.LS(LSID.DlgCancel));
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

            stbMain.Items[0].Title = statusLine;
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
            var client = Bounds;
            qsDlg.Location = new Point(client.Left, client.Bottom + 1 - qsDlg.Bounds.Height);
            Application.Run(qsDlg);
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
            var tabs = tabsRecords.Tabs.ToArray();
            var selTab = tabs[(int)recType - 1];
            //tabsRecords.TabIndex = (int)recType - 1;
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

        private void StatusBar_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            fContext.SwitchShieldState();
            UpdateShieldState();
            e.Handled = true;
        }

        public void UpdateMRU()
        {
            try {
                //miMRUFiles.Enabled = (AppHost.Options.MRUFiles.Count > 0);
                int num = AppHost.Options.MRUFiles.Count;
                for (int i = 0; i < num; i++) {
                    string fn = AppHost.Options.MRUFiles[i].FileName;

                    int idx = i;
                    var mi = new MenuItem(fn, "", (sender, e) => {
                        AppHost.Instance.LoadBase(this, AppHost.Options.MRUFiles[idx].FileName);
                    });
                    miMRUFiles.Children.Add(mi);
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.UpdateMRU()", ex);
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

        private void miExportToStrictGEDCOM_Click(object sender, EventArgs e)
        {
            fController.ExportToStrictGEDCOM();
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

        private async void miFileReload_Click(object sender, EventArgs e)
        {
            await fController.ReloadFile();
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

        private void miChronicle_Click(object sender, EventArgs e)
        {
            fController.ShowChronicle();
        }

        private void miOrganizer_Click(object sender, EventArgs e)
        {
            fController.ShowOrganizer();
        }

        private void miRelationshipCalculator_Click(object sender, EventArgs e)
        {
            fController.ShowRelationshipCalculator();
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
