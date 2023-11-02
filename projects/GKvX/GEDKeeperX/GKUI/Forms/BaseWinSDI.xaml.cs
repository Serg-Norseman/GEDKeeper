/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Components;
using Xam.Plugin.TabView;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class BaseWinSDI : CommonWindow, IBaseWindowView, IProgressController
    {
        #region Private fields

        private readonly BaseWinController fController;

        private readonly IBaseContext fContext;

        #endregion

        #region Public properties

        public IBaseContext Context
        {
            get { return fContext; }
        }

        public BaseWinController Controller
        {
            get { return fController; }
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
            get { return /*GetControlHandler<IMenuItem>(miReports)*/ null; }
        }

        IMenuItem IBaseWindowView.PluginsItem
        {
            get { return /*GetControlHandler<IMenuItem>(miPlugins) */ null; }
        }

        #endregion

        #region Instance control

        public BaseWinSDI()
        {
            InitializeComponent();

            tabsRecords.PositionChanged += tabsRecords_SelectedIndexChanged;

            fController = new BaseWinController(this, true);
            fContext = fController.Context;
            ((BaseContext)fContext).ModifiedChanged += BaseContext_ModifiedChanged;

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

            /*fController.SetLocale();*/

            // FIXME: temp debug
            ((IBaseWindow)this).UpdateControls();
            UpdateShieldState();
        }

        private void CreatePage(string pageText, GDMRecordType recType)
        {
            var summary = new HyperView();
            //summary.BorderWidth = 4;
            summary.OnLink += mPersonSummaryLink;
            //summary.ContextMenu = summaryMenu;

            var recView = new GKListView();
            //recView.AllowMultipleSelection = true;
            recView.MouseDoubleClick += miRecordEdit_Click;
            recView.ItemSelected += List_SelectedIndexChanged;
            //recView.ContextMenu = contextMenu;
            recView.ListMan = RecordsListModel<GDMRecord>.Create(fContext, recType, false);
            recView.UpdateContents();

            //var spl = new StackLayout();
            string splID = "splitter" + ((int)recType).ToString();
            /*spl.Panel1 = recView;
            spl.Panel2 = summary;
            spl.RelativePosition = 300;
            spl.Orientation = Orientation.Horizontal;
            spl.FixedPanel = SplitterFixedPanel.Panel2;
            spl.PositionChanged += Spl_PositionChanged;*/
            var spl = new Grid {
                HorizontalOptions = LayoutOptions.FillAndExpand,
                ColumnDefinitions = new ColumnDefinitionCollection() {
                    new ColumnDefinition() { Width = new GridLength(1, GridUnitType.Star) },
                    new ColumnDefinition() { Width = new GridLength(300, GridUnitType.Absolute) },
                },
                RowDefinitions = new RowDefinitionCollection() {
                    new RowDefinition() { Height = GridLength.Auto }
                }
            };
            spl.Children.Add(new Frame() { Content = recView, Padding = 1, BorderColor = Color.Red }, 0, 0);
            spl.Children.Add(new Frame() { Content = summary, Padding = 1, BorderColor = Color.Red }, 1, 0);

            var tabPage = new TabItem(); //ContentPage();
            tabPage.HeaderText = pageText;
            tabPage.Content = spl;
            tabsRecords.AddTab(tabPage);

            fController.SetTabPart(recType, recView, splID, summary);
        }

        #endregion

        #region Form handlers

        void IBaseWindowView.EnableSplitterEvent(object controlHandler, bool enable)
        {
        }

        private void BaseContext_ModifiedChanged(object sender, EventArgs e)
        {
            fController.SetMainTitle();
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
            fController.SelectSummaryLink(linkName);
        }

        private void miCopyContent_Click(object sender, EventArgs e)
        {
            var hyperView = GetHyperViewByType(GetSelectedRecordType());
            fController.CopyContent(hyperView);
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

            /*DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxType.Warning);
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
                        mediaViewer.ShowInTaskbar = true;
                        mediaViewer.Show();
                    } finally {
                        //if (modal) mediaViewer.Dispose();
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
            /*if (!AllowQuickSearch()) return;

            QuickSearchDlg qsDlg = new QuickSearchDlg(this);
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
            tabsRecords.SelectedTabIndex = (int)recType - 1;
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
            var img = ((XFImageHandler)fController.GetShieldImage()).Handle;
            if (img != null) {
                panStatusShieldImage.Source = img;
            }
        }

        private void StatusBar_MouseDoubleClick(object sender, EventArgs e)
        {
            fContext.SwitchShieldState();
            UpdateShieldState();
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

        private void miOptions_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowOptions(this);
        }

        private void miFileNew_Click(object sender, EventArgs e)
        {
            fController.NewFile();
        }

        private void miFileLoad_Click(object sender, EventArgs e)
        {
            fController.LoadFileAsync();
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

        #region IProgressController

        private int fMaximum;
        private int fVal;

        bool IProgressController.IsCanceled
        {
            get { return false; }
        }

        private void UpdateProgress()
        {
            progressBar.Progress = (fMaximum == 0) ? 0 : fVal / (float)fMaximum;
        }

        void IProgressController.Begin(int maximum, bool cancelable)
        {
            fMaximum = maximum;
            fVal = 0;
            Device.BeginInvokeOnMainThread(() => {
                progressBar.IsVisible = true;
                UpdateProgress();
            });
        }

        void IProgressController.Begin(string title, int maximum, bool cancelable)
        {
            fMaximum = maximum;
            fVal = 0;
            Device.BeginInvokeOnMainThread(() => {
                progressBar.IsVisible = true;
                UpdateProgress();
            });
        }

        void IProgressController.End()
        {
            Device.BeginInvokeOnMainThread(() => {
                progressBar.IsVisible = false;
            });
        }

        void IProgressController.End(ThreadError threadError)
        {
            Device.BeginInvokeOnMainThread(() => {
                progressBar.IsVisible = false;
            });
        }

        void IProgressController.SetText(string text)
        {
        }

        void IProgressController.StepTo(int value)
        {
            fVal = value;
            Device.BeginInvokeOnMainThread(() => {
                UpdateProgress();
            });
        }

        void IProgressController.Increment(int value)
        {
            fVal += value;
            Device.BeginInvokeOnMainThread(() => {
                UpdateProgress();
            });
        }

        void IProgressController.InvokeEx(Action action)
        {
            Device.BeginInvokeOnMainThread(action);
        }

        #endregion
    }
}
