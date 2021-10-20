/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

#define GEDML_SUPPORT
#define FAMX_SUPPORT

using System;
using System.Collections.Generic;
using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Charts;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Controllers
{
    public sealed class TabParts
    {
        public readonly IListViewEx ListView;
        public readonly IHyperView Summary;

        public TabParts(IListViewEx listView, IHyperView summary)
        {
            ListView = listView;
            Summary = summary;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class BaseWinController : Controller<IBaseWindowView>
    {
        private readonly List<GDMRecord> fChangedRecords;
        private readonly IBaseContext fContext;
        private GDMRecord fDelayedTransitionRecord;
        private readonly NavigationStack<GDMRecord> fNavman;
        private readonly TabParts[] fTabParts;

        public IBaseContext Context
        {
            get { return fContext; }
        }

        public NavigationStack<GDMRecord> Navman
        {
            get { return fNavman; }
        }


        public BaseWinController(IBaseWindowView view) : base(view)
        {
            fContext = new BaseContext(view);
            fChangedRecords = new List<GDMRecord>();
            fNavman = new NavigationStack<GDMRecord>();
            fTabParts = new TabParts[(int)GDMRecordType.rtLast + 1];
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //fNavman.Dispose();
                fContext.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Context

        public void Clear()
        {
            fNavman.Clear();
            fContext.Clear();
        }

        // FIXME: Identify and test the need for this method
        public void CreateNewFile()
        {
            Clear();
            RefreshLists(false);
            ClearSummaries();
            fContext.SetFileName(LangMan.LS(LSID.LSID_Unknown));
            fContext.Tree.Header.Language = GlobalOptions.Instance.GetCurrentItfLang();
            fContext.Modified = false;
        }

        public void NewFile()
        {
            AppHost.Instance.CreateBase("");
        }

        public void LoadFile(string fileName)
        {
            Clear();

            if (fContext.FileLoad(fileName)) {
                fContext.Modified = false;
                ChangeFileName();
                RefreshLists(false);
                fView.Activate();
            }
        }

        public void LoadFileEx()
        {
            string homePath = AppHost.Instance.GetUserFilesPath("");

            string filters = LangMan.LS(LSID.LSID_GEDCOMFilter);

            #if GEDML_SUPPORT
            filters += "|" + LangMan.LS(LSID.LSID_GedMLFilter);
            #endif

            #if FAMX_SUPPORT
            filters += "|" + "Family.Show files (*.familyx)|*.familyx";
            #endif

            string fileName = AppHost.StdDialogs.GetOpenFile("", homePath, filters, 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                AppHost.Instance.LoadBase(fView, fileName);
            }
        }

        public void SaveFile(string fileName)
        {
            if (fContext.FileSave(fileName)) {
                fContext.Modified = false;
                ChangeFileName();
            }
        }

        public void SaveFileEx(bool saveAs)
        {
            string oldFileName = fContext.FileName;
            bool isUnknown = fContext.IsUnknown();

            if (!isUnknown && !saveAs) {
                SaveFile(oldFileName);
            } else {
                string homePath = AppHost.Instance.GetUserFilesPath(Path.GetDirectoryName(oldFileName));
                string newFileName = AppHost.StdDialogs.GetSaveFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, oldFileName, false);
                if (!string.IsNullOrEmpty(newFileName)) {
                    SaveFile(newFileName);
                    if (!isUnknown && !string.Equals(oldFileName, newFileName)) {
                        AppHost.Instance.BaseRenamed(fView, oldFileName, newFileName);
                    }
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

        public void ApplyFilter(GDMRecordType recType = GDMRecordType.rtNone)
        {
            if (fContext.Tree.RecordsCount > 0) {
                if (recType == GDMRecordType.rtNone) {
                    RefreshLists(false);
                } else {
                    RefreshRecordsView(recType);
                }
            }
        }

        public void SetExternalFilter(ExternalFilterHandler filterHandler, 
                                      GDMRecordType recType = GDMRecordType.rtNone)
        {
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                if (recType != GDMRecordType.rtNone && recType != rt) continue;

                IListViewEx listview = fTabParts[(int)rt].ListView;
                if (listview != null) {
                    listview.ListMan.ExternalFilter = filterHandler;
                }
            }
        }

        public void NotifyRecord(GDMRecord record, RecordAction action)
        {
            if (record == null) return;

            DateTime dtNow = DateTime.Now;

            switch (action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    record.ChangeDate.ChangeDateTime = dtNow;
                    CheckChangedRecord(record, true);
                    break;

                case RecordAction.raDelete:
                    {
                        CheckChangedRecord(record, false);

                        IListViewEx rView = GetRecordsViewByType(record.RecordType);
                        if (rView != null) {
                            rView.DeleteRecord(record);

                            IHyperView hView = GetHyperViewByType(record.RecordType);
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

                AppHost.Instance.NotifyRecord(fView, record, action);
            }
        }

        public void DuplicateRecord()
        {
            GDMRecord original = GetSelectedRecordEx();
            if (original == null || original.RecordType != GDMRecordType.rtIndividual) return;

            AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_DuplicateWarning));

            GDMIndividualRecord target;
            try {
                fContext.BeginUpdate();

                target = fContext.Tree.CreateIndividual();
                target.Assign(original);

                NotifyRecord(target, RecordAction.raAdd);
            } finally {
                fContext.EndUpdate();
            }

            RefreshLists(false);
            fView.SelectRecordByXRef(target.XRef);
        }

        public void AddRecord()
        {
            GDMRecordType rt = GetSelectedRecordType();

            GDMRecord record = BaseController.AddRecord(fView, rt, null);
            if (record != null) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public void EditRecord()
        {
            GDMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.EditRecord(fView, record)) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public void DeleteRecord()
        {
            GDMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.DeleteRecord(fView, record, true)) {
                RefreshLists(false);
            }
        }

        public void ShowRecordInfo(GDMRecord record)
        {
            if (record == null) return;

            try {
                IHyperView hyperView = GetHyperViewByType(record.RecordType);
                if (hyperView != null) {
                    GKUtils.GetRecordContent(fContext, record, hyperView.Lines);
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.ShowRecordInfo()", ex);
            }
        }

        public void ChangeListItem(IListViewEx sender)
        {
            GDMRecord rec = sender.GetSelectedData() as GDMRecord;
            if (rec != null) {
                NavAdd(rec);
            }
            ShowRecordInfo(rec);
        }

        public void SelectSummaryLink(string linkName)
        {
            if (linkName.StartsWith("http")) {
                GKUtils.LoadExtFile(linkName);
                return;
            }

            if (linkName.StartsWith("view_")) {
                string xref = linkName.Remove(0, 5);
                var mmRec = fContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
                if (mmRec != null) {
                    fView.ShowMedia(mmRec, false);
                }
            } else {
                SelectRecordByXRef(linkName);
            }
        }

        public void SelectByRec(GDMRecord record)
        {
            if (record == null)
                throw new ArgumentNullException("record");

            fView.Activate();
            SelectRecordByXRef(record.XRef);
        }

        public void SelectRecordByXRef(string xref, bool delayedTransition = false)
        {
            GDMRecord record = fContext.Tree.XRefIndex_Find(xref);

            if (delayedTransition) {
                fDelayedTransitionRecord = record;
                return;
            }

            if (fDelayedTransitionRecord != null) {
                record = fDelayedTransitionRecord;
                fDelayedTransitionRecord = null;
            }

            IListViewEx rView = (record == null) ? null : GetRecordsViewByType(record.RecordType);
            if (rView != null) {
                fView.ShowRecordsTab(record.RecordType);
                rView.Activate();
                rView.SelectItem(record);
            }
        }

        public StringList GetRecordContent(GDMRecord record)
        {
            StringList ctx = new StringList();
            GKUtils.GetRecordContent(fContext, record, ctx);
            return ctx;
        }

        public bool RecordIsFiltered(GDMRecord record)
        {
            bool result = false;
            if (record != null) {
                IListViewEx rView = GetRecordsViewByType(record.RecordType);
                result = (rView != null && rView.ListMan.IndexOfRecord(record) >= 0);
            }
            return result;
        }

        public void Undo()
        {
            fContext.DoUndo();
        }

        public void Redo()
        {
            fContext.DoRedo();
        }

        #endregion

        #region UI

        public void SetTabPart(GDMRecordType recType, IListViewEx listView, IHyperView summary)
        {
            fTabParts[(int)recType] = new TabParts(listView, summary);
        }

        public GDMRecordType GetSelectedRecordType()
        {
            return (GDMRecordType)(fView.RecordTabs.SelectedIndex + 1);
        }

        public IListViewEx GetRecordsViewByType(GDMRecordType recType)
        {
            int rt = (int)recType;
            TabParts tabPart = (rt < 0 || rt >= fTabParts.Length) ? null : fTabParts[rt];
            return (tabPart == null) ? null : tabPart.ListView;
        }

        /// <summary>
        /// Gets a hyper-view control for the specified record type.
        /// </summary>
        /// <param name="recType">Record type for which a hyper view control is
        /// required.</param>
        /// <returns>Hyper view control.</returns>
        public IHyperView GetHyperViewByType(GDMRecordType recType)
        {
            IHyperView view = fTabParts[(int)recType].Summary;
            return view;
        }

        public IListManager GetRecordsListManByType(GDMRecordType recType)
        {
            IListViewEx rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.ListMan;
        }

        public GDMRecord GetSelectedRecordEx()
        {
            GDMRecordType recType = GetSelectedRecordType();
            IListViewEx rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : (rView.GetSelectedData() as GDMRecord);
        }

        public GDMIndividualRecord GetSelectedPerson()
        {
            return GetSelectedRecordEx() as GDMIndividualRecord;
        }

        public void ClearSummaries()
        {
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                IHyperView summary = fTabParts[(int)rt].Summary;
                if (summary != null) {
                    summary.Lines.Clear();
                }
            }
        }

        public void RefreshLists(bool columnsChanged)
        {
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                IListViewEx listview = fTabParts[(int)rt].ListView;
                if (listview != null) {
                    listview.UpdateContents(columnsChanged);
                }
            }

            AppHost.Instance.UpdateControls(false);
        }

        public List<GDMRecord> GetContentList(GDMRecordType recType)
        {
            IListViewEx rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.ListMan.GetRecordsList();
        }

        public void RestoreListsSettings()
        {
            var globOptions = GlobalOptions.Instance;
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                IListViewEx rView = fTabParts[(int)rt].ListView;
                if (rView != null) {
                    rView.SetSortColumn(globOptions.ListOptions[rt].SortColumn, false);
                    if (rt == GDMRecordType.rtIndividual) {
                        globOptions.IndividualListColumns.CopyTo(rView.ListMan.ListColumns);
                    }
                }
            }
        }

        public void SaveListsSettings()
        {
            var globOptions = GlobalOptions.Instance;
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                IListViewEx rView = fTabParts[(int)rt].ListView;
                if (rView != null) {
                    globOptions.ListOptions[rt].SortColumn = rView.SortColumn;
                    if (rt == GDMRecordType.rtIndividual) {
                        rView.ListMan.ListColumns.CopyTo(globOptions.IndividualListColumns);
                    }
                }
            }
        }

        public void RefreshRecordsView(GDMRecordType recType)
        {
            IListViewEx rView = GetRecordsViewByType(recType);
            if (rView != null) {
                rView.UpdateContents();

                AppHost.Instance.UpdateControls(false);
            }
        }

        public void UpdateChangedRecords(GDMRecord select = null)
        {
            for (int i = fChangedRecords.Count - 1; i >= 0; i--) {
                var record = fChangedRecords[i];

                RefreshRecordsView(record.RecordType);
            }

            if (select != null) {
                fView.SelectRecordByXRef(select.XRef);
            }
        }

        public void CheckChangedRecord(GDMRecord record, bool active)
        {
            int idx = fChangedRecords.IndexOf(record);
            if (active) {
                if (idx < 0) {
                    fChangedRecords.Add(record);
                }
            } else {
                if (idx >= 0) {
                    fChangedRecords.RemoveAt(idx);
                }
            }
        }

        public void UpdateSettings()
        {
            RestoreListsSettings();
            RefreshLists(true);
        }

        public void NavAdd(GDMRecord aRec)
        {
            if (aRec == null) return;

            fNavman.Current = aRec;
            AppHost.Instance.UpdateControls(false);
        }

        public void NavNext()
        {
            GDMRecord rec = fNavman.Next();
            if (rec != null) {
                fView.SelectRecordByXRef(rec.XRef);
                AppHost.Instance.UpdateControls(false);
            }
        }

        public void NavPrev()
        {
            GDMRecord rec = fNavman.Back();
            if (rec != null) {
                fView.SelectRecordByXRef(rec.XRef);
                AppHost.Instance.UpdateControls(false);
            }
        }

        public bool NavCanBackward()
        {
            return fNavman.CanBackward();
        }

        public bool NavCanForward()
        {
            return fNavman.CanForward();
        }

        public void SetMainTitle()
        {
            string caption = Path.GetFileName(fContext.FileName);
            if (fContext.Modified) {
                caption = @"* " + caption;
            }
            fView.Title = caption;
        }

        public void ChangeFileName()
        {
            SetMainTitle();
            GlobalOptions.Instance.LastDir = Path.GetDirectoryName(fContext.FileName);
            AppHost.Instance.AddMRU(fContext.FileName);
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            GDMRecordType rt = GetSelectedRecordType();
            IListManager listMan = GetRecordsListManByType(rt);
            IList<ISearchResult> result = listMan.FindAll(searchPattern);
            //IList<ISearchResult> result = fContext.FindAll(rt, searchPattern);
            return result;
        }

        public void SetFilter()
        {
            if (!fView.AllowFilter()) return;

            GDMRecordType rt = GetSelectedRecordType();
            IListManager listMan = GetRecordsListManByType(rt);
            if (listMan == null) return;

            switch (rt) {
                case GDMRecordType.rtIndividual:
                    ShowPersonsFilter(rt, listMan);
                    break;

                case GDMRecordType.rtFamily:
                case GDMRecordType.rtNote:
                case GDMRecordType.rtMultimedia:
                case GDMRecordType.rtSource:
                case GDMRecordType.rtRepository:
                case GDMRecordType.rtGroup:
                case GDMRecordType.rtResearch:
                case GDMRecordType.rtTask:
                case GDMRecordType.rtCommunication:
                case GDMRecordType.rtLocation:
                    ShowCommonFilter(rt, listMan);
                    break;
            }
        }

        public IImage GetShieldImage()
        {
            IImage img = null;
            var gfxProvider = AppHost.GfxProvider;
            switch (fContext.ShieldState) {
                case ShieldState.None:
                    img = gfxProvider.LoadResourceImage("rg_shield_none.gif", true);
                    break;
                case ShieldState.Middle:
                    img = gfxProvider.LoadResourceImage("rg_shield_mid.gif", true);
                    break;
                case ShieldState.Maximum:
                    img = gfxProvider.LoadResourceImage("rg_shield_max.gif", true);
                    break;
            }
            return img;
        }

        #endregion

        public override void UpdateView()
        {
        }

        #region Dialogs

        private void ShowCommonFilter(GDMRecordType rt, IListManager listMan)
        {
            using (var dlg = AppHost.Container.Resolve<ICommonFilterDlg>(fView, listMan)) {
                if (AppHost.Instance.ShowModalX(dlg, false)) {
                    ApplyFilter(rt);
                }
            }
        }

        private void ShowPersonsFilter(GDMRecordType rt, IListManager listMan)
        {
            using (var dlg = AppHost.Container.Resolve<IPersonsFilterDlg>(fView, listMan)) {
                if (AppHost.Instance.ShowModalX(dlg, false)) {
                    ApplyFilter(rt);
                }
            }
        }

        public void ExportToFamilyBook()
        {
            //#if MONO
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            //#endif

            using (FamilyBookExporter fb = new FamilyBookExporter(fView)) {
                fb.Generate(true);
            }
        }

        public void ExportToTreesAlbum()
        {
            //#if MONO
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            //#endif

            AppHost.StdDialogs.ShowWarning(@"This function is experimental and not completed. Only for PDF!");

            using (TreesAlbumExporter ta = new TreesAlbumExporter(fView)) {
                ta.Generate(true);
            }
        }

        public void ExportToExcelFile()
        {
            using (ExcelExporter exExp = new ExcelExporter(fView)) {
                exExp.Options = AppHost.Options;
                exExp.Generate(true);
            }
        }

        public void ShowFileProperties()
        {
            try {
                fContext.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<IFilePropertiesDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowScripts()
        {
            try {
                fContext.BeginUpdate();

                using (var dlg = AppHost.Container.Resolve<IScriptEditWin>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowTreeSplit()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<ITreeSplitDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowTreeMerge()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<ITreeMergeDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowTreeCompare()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<ITreeCompareDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowTreeCheck()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<ITreeCheckDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowRecMerge(GDMRecord rec1, GDMRecord rec2)
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IRecMergeDlg>(fView)) {
                    dlg.MergeCtl.SetRec1(rec1);
                    dlg.MergeCtl.SetRec2(rec2);
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowPlacesManager()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IPlacesManagerDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowPatSearch()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IPatriarchsSearchDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowFamilyGroups()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IFragmentSearchDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void SendMail()
        {
            if (fView.CheckModified()) {
                string fileName = Path.GetFileName(fContext.FileName);
                SysUtils.SendMail("?", fileName, "?", fContext.FileName);
            }
        }

        public void ShowMap()
        {
            //#if MONO
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            var mapsWin = AppHost.Container.Resolve<IMapsViewerWin>(fView);
            AppHost.Instance.ShowWindow(mapsWin);
            //#endif
        }

        public void ShowOrganizer()
        {
            using (var dlg = AppHost.Container.Resolve<IOrganizerWin>(fView)) {
                AppHost.Instance.ShowModalX(dlg, false);
            }
        }

        public void ShowRelationshipCalculator()
        {
            using (var dlg = AppHost.Container.Resolve<IRelationshipCalculatorDlg>(fView)) {
                AppHost.Instance.ShowModalX(dlg, false);
            }
        }

        public void ShowSlideshow()
        {
            var win = AppHost.Container.Resolve<ISlideshowWin>(fView);
            AppHost.Instance.ShowWindow(win);
        }

        public void ShowStats()
        {
            List<GDMRecord> selectedRecords = GetContentList(GDMRecordType.rtIndividual);

            var win = AppHost.Container.Resolve<IStatisticsWin>(fView, selectedRecords);
            AppHost.Instance.ShowWindow(win);
        }

        public void GeneratePedigree(PedigreeExporter.PedigreeKind kind)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            if (BaseController.DetectCycle(fContext.Tree, selPerson)) return;

            using (var p = new PedigreeExporter(fView, selPerson)) {
                p.Options = AppHost.Options;
                p.Kind = kind;
                p.Generate(true);
            }
        }

        public void ShowTreeChart(TreeChartKind chartKind)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) return;

            if (BaseController.DetectCycle(fContext.Tree, selPerson)) return;

            if (TreeChartModel.CheckTreeChartSize(fContext.Tree, selPerson, chartKind)) {
                var fmChart = AppHost.Container.Resolve<ITreeChartWin>(fView, selPerson);
                fmChart.GenChart(chartKind);
                AppHost.Instance.ShowWindow(fmChart);
            }
        }

        public void ShowCircleChart(CircleChartType chartKind)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) return;

            if (BaseController.DetectCycle(fContext.Tree, selPerson)) return;

            var fmChart = AppHost.Container.Resolve<ICircleChartWin>(fView, selPerson, chartKind);
            AppHost.Instance.ShowWindow(fmChart);
        }

        public void SendLog()
        {
            SysUtils.SendMail(GKData.APP_MAIL, "GEDKeeper: feedback", "This automatic notification of error.", AppHost.GetLogFilename());
        }

        public void ShowLog()
        {
            GKUtils.LoadExtFile(AppHost.GetLogFilename());
        }

        public void ShowAbout()
        {
            using (var dlg = AppHost.Container.Resolve<IAboutDlg>()) {
                AppHost.Instance.ShowModalX(dlg, false);
            }
        }

        private static void Plugin_Click(IMenuItem sender)
        {
            if (sender == null) return;

            IPlugin plugin = sender.Tag as IPlugin;
            if (plugin == null) return;

            plugin.Execute();
        }

        public void UpdatePluginsItems()
        {
            try {
                fView.PluginsItem.ClearItems();
                fView.ReportsItem.ClearItems();

                AppHost.Instance.ActiveWidgets.Clear();

                int num = AppHost.Plugins.Count;
                for (int i = 0; i < num; i++) {
                    IPlugin plugin = AppHost.Plugins[i];

                    if (plugin is IDialogReplacement || plugin.Category == PluginCategory.DialogReplacement) {
                        continue;
                    }

                    IMenuItem ownerItem = (plugin.Category == PluginCategory.Report) ? fView.ReportsItem : fView.PluginsItem;
                    IMenuItem mi = ownerItem.AddItem(plugin.DisplayName, plugin, plugin.Icon, Plugin_Click);

                    var widget = plugin as IWidget;
                    if (widget != null) {
                        var widInfo = new WidgetInfo(widget, mi);
                        AppHost.Instance.ActiveWidgets.Add(widInfo);
                        widget.WidgetInit(AppHost.Instance);
                    }
                }

                fView.ReportsItem.Enabled = (fView.ReportsItem.ItemsCount > 0);
                fView.PluginsItem.Enabled = (fView.PluginsItem.ItemsCount > 0);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinController.UpdatePluginsItems()", ex);
            }
        }

        #endregion
    }
}
