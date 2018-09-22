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
using System.IO;
using BSLib;
using GKCommon.GEDCOM;
using GKCore.Charts;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Controllers
{
    public sealed class TabParts
    {
        public readonly IListView ListView;
        public readonly IHyperView Summary;

        public TabParts(IListView listView, IHyperView summary)
        {
            ListView = listView;
            Summary = summary;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class BaseWinController : DialogController<IBaseWindowView>
    {
        private readonly List<GEDCOMRecord> fChangedRecords;
        private readonly IBaseContext fContext;
        private readonly NavigationStack<GEDCOMRecord> fNavman;
        private readonly TabParts[] fTabParts;

        public IBaseContext Context
        {
            get { return fContext; }
        }

        public NavigationStack<GEDCOMRecord> Navman
        {
            get { return fNavman; }
        }


        public BaseWinController(IBaseWindowView view) : base(view)
        {
            fContext = new BaseContext(view);
            fChangedRecords = new List<GEDCOMRecord>();
            fNavman = new NavigationStack<GEDCOMRecord>();
            fTabParts = new TabParts[(int)GEDCOMRecordType.rtLast + 1];
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fNavman.Dispose();
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

        public void LoadFileEx()
        {
            string homePath = AppHost.Instance.GetUserFilesPath("");

            string fileName = AppHost.StdDialogs.GetOpenFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
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

        public void ApplyFilter(GEDCOMRecordType recType = GEDCOMRecordType.rtNone)
        {
            if (fContext.Tree.RecordsCount > 0) {
                if (recType == GEDCOMRecordType.rtNone) {
                    RefreshLists(false);
                } else {
                    RefreshRecordsView(recType);
                }
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
                    CheckChangedRecord(record, true);
                    break;

                case RecordAction.raDelete:
                    {
                        CheckChangedRecord(record, false);

                        IListView rView = GetRecordsViewByType(record.RecordType);
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
            fView.SelectRecordByXRef(target.XRef);
        }

        public void AddRecord()
        {
            GEDCOMRecordType rt = GetSelectedRecordType();

            GEDCOMRecord record = BaseController.AddRecord(fView, rt, null);
            if (record != null) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public void EditRecord()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.EditRecord(fView, record)) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public void DeleteRecord()
        {
            GEDCOMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.DeleteRecord(fView, record, true)) {
                RefreshLists(false);
            }
        }

        public void ShowRecordInfo(GEDCOMRecord record)
        {
            if (record == null) return;

            try {
                IHyperView hyperView = GetHyperViewByType(record.RecordType);
                if (hyperView != null) {
                    GKUtils.GetRecordContent(fContext, record, hyperView.Lines);
                }
            } catch (Exception ex) {
                Logger.LogWrite("BaseWinSDI.ShowRecordInfo(): " + ex.Message);
            }
        }

        public void ChangeListItem(IListView sender)
        {
            GEDCOMRecord rec = sender.GetSelectedData() as GEDCOMRecord;
            if (rec != null) {
                NavAdd(rec);
            }
            ShowRecordInfo(rec);
        }

        public void SelectSummaryLink(string linkName)
        {
            if (linkName.StartsWith("view_")) {
                string xref = linkName.Remove(0, 5);
                GEDCOMMultimediaRecord mmRec = fContext.Tree.XRefIndex_Find(xref) as GEDCOMMultimediaRecord;
                if (mmRec != null) {
                    fView.ShowMedia(mmRec, false);
                }
            } else {
                SelectRecordByXRef(linkName);
            }
        }

        public void SelectByRec(GEDCOMRecord record)
        {
            if (record == null)
                throw new ArgumentNullException("record");

            fView.Activate();
            SelectRecordByXRef(record.XRef);
        }

        public void SelectRecordByXRef(string xref)
        {
            GEDCOMRecord record = fContext.Tree.XRefIndex_Find(xref);
            IListView rView = (record == null) ? null : GetRecordsViewByType(record.RecordType);

            if (rView != null) {
                fView.ShowRecordsTab(record.RecordType);
                rView.Activate();
                rView.SelectItem(record);
            }
        }

        public StringList GetRecordContent(GEDCOMRecord record)
        {
            StringList ctx = new StringList();
            GKUtils.GetRecordContent(fContext, record, ctx);
            return ctx;
        }

        public bool RecordIsFiltered(GEDCOMRecord record)
        {
            bool result = false;
            if (record != null) {
                IListView rView = GetRecordsViewByType(record.RecordType);
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

        public void SetTabPart(GEDCOMRecordType recType, IListView listView, IHyperView summary)
        {
            fTabParts[(int)recType] = new TabParts(listView, summary);
        }

        public GEDCOMRecordType GetSelectedRecordType()
        {
            return (GEDCOMRecordType)(fView.RecordTabs.SelectedIndex + 1);
        }

        public IListView GetRecordsViewByType(GEDCOMRecordType recType)
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
        public IHyperView GetHyperViewByType(GEDCOMRecordType recType)
        {
            IHyperView view = fTabParts[(int)recType].Summary;
            return view;
        }

        public IListManager GetRecordsListManByType(GEDCOMRecordType recType)
        {
            IListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : (IListManager)rView.ListMan;
        }

        public GEDCOMRecord GetSelectedRecordEx()
        {
            GEDCOMRecordType recType = GetSelectedRecordType();
            IListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : (rView.GetSelectedData() as GEDCOMRecord);
        }

        public GEDCOMIndividualRecord GetSelectedPerson()
        {
            return GetSelectedRecordEx() as GEDCOMIndividualRecord;
        }

        public void ClearSummaries()
        {
            for (var rt = GEDCOMRecordType.rtIndividual; rt <= GEDCOMRecordType.rtLocation; rt++) {
                IHyperView summary = fTabParts[(int)rt].Summary;
                if (summary != null) {
                    summary.Lines.Clear();
                }
            }
        }

        public void RefreshLists(bool columnsChanged)
        {
            for (var rt = GEDCOMRecordType.rtIndividual; rt <= GEDCOMRecordType.rtLocation; rt++) {
                IListView listview = fTabParts[(int)rt].ListView;
                if (listview != null) {
                    listview.UpdateContents(columnsChanged);
                }
            }

            AppHost.Instance.UpdateControls(false);
        }

        public List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType)
        {
            IListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : rView.ListMan.GetRecordsList();
        }

        public void UpdateListsSettings()
        {
            IListManager listMan = GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
            if (listMan != null) {
                GlobalOptions.Instance.IndividualListColumns.CopyTo(listMan.ListColumns);
            }
        }

        public void RefreshRecordsView(GEDCOMRecordType recType)
        {
            IListView rView = GetRecordsViewByType(recType);
            if (rView != null) {
                rView.UpdateContents();

                AppHost.Instance.UpdateControls(false);
            }
        }

        public void UpdateChangedRecords(GEDCOMRecord select = null)
        {
            for (int i = fChangedRecords.Count - 1; i >= 0; i--) {
                var record = fChangedRecords[i];

                RefreshRecordsView(record.RecordType);
            }

            if (select != null) {
                fView.SelectRecordByXRef(select.XRef);
            }
        }

        public void CheckChangedRecord(GEDCOMRecord record, bool active)
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
            UpdateListsSettings();
            RefreshLists(true);
        }

        public void NavAdd(GEDCOMRecord aRec)
        {
            if (aRec == null || fNavman.Busy) return;

            fNavman.Current = aRec;
            AppHost.Instance.UpdateControls(false);
        }

        public void NavNext()
        {
            fNavman.BeginNav();
            try {
                GEDCOMRecord rec = fNavman.Next() as GEDCOMRecord;
                if (rec != null) {
                    fView.SelectRecordByXRef(rec.XRef);
                    AppHost.Instance.UpdateControls(false);
                }
            } finally {
                fNavman.EndNav();
            }
        }

        public void NavPrev()
        {
            fNavman.BeginNav();
            try {
                GEDCOMRecord rec = fNavman.Back() as GEDCOMRecord;
                if (rec != null) {
                    fView.SelectRecordByXRef(rec.XRef);
                    AppHost.Instance.UpdateControls(false);
                }
            } finally {
                fNavman.EndNav();
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
            fView.Caption = caption;
        }

        public void ChangeFileName()
        {
            SetMainTitle();
            GlobalOptions.Instance.LastDir = Path.GetDirectoryName(fContext.FileName);
            AppHost.Instance.AddMRU(fContext.FileName);
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            GEDCOMRecordType rt = GetSelectedRecordType();
            IList<ISearchResult> result = fContext.FindAll(rt, searchPattern);
            return result;
        }

        public void SetFilter()
        {
            if (!fView.AllowFilter()) return;

            GEDCOMRecordType rt = GetSelectedRecordType();
            IListManager listMan = GetRecordsListManByType(rt);
            if (listMan == null) return;

            switch (rt) {
                case GEDCOMRecordType.rtIndividual:
                    ShowPersonsFilter(rt, listMan);
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

        private void ShowCommonFilter(GEDCOMRecordType rt, IListManager listMan)
        {
            using (var dlg = AppHost.Container.Resolve<ICommonFilterDlg>(fView, listMan)) {
                if (AppHost.Instance.ShowModalX(dlg, false)) {
                    ApplyFilter(rt);
                }
            }
        }

        private void ShowPersonsFilter(GEDCOMRecordType rt, IListManager listMan)
        {
            using (var dlg = AppHost.Container.Resolve<IPersonsFilterDlg>(fView, listMan)) {
                if (AppHost.Instance.ShowModalX(dlg, false)) {
                    ApplyFilter(rt);
                }
            }
        }

        public void ExportToFamilyBook()
        {
            //#if __MonoCS__
            //AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            //#else
            //#endif

            using (FamilyBookExporter fb = new FamilyBookExporter(fView)) {
                fb.Generate(true);
            }
        }

        public void ExportToTreesAlbum()
        {
            //#if __MonoCS__
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

                using (var dlg = AppHost.Container.Resolve<IFilePropertiesDlg>()) {
                    dlg.InitDialog(fView);
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

        public void ShowRecMerge()
        {
            try {
                fContext.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IRecMergeDlg>(fView)) {
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

        public void FileNew()
        {
            AppHost.Instance.CreateBase("");
        }

        public void FileLoad()
        {
            string homePath = AppHost.Instance.GetUserFilesPath("");

            string fileName = AppHost.StdDialogs.GetOpenFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                AppHost.Instance.LoadBase(fView, fileName);
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
            //#if __MonoCS__
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
            List<GEDCOMRecord> selectedRecords = GetContentList(GEDCOMRecordType.rtIndividual);

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

            if (BaseController.DetectCycle(selPerson)) return;

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

            if (BaseController.DetectCycle(selPerson)) return;

            if (TreeChartModel.CheckTreeChartSize(fContext.Tree, selPerson, chartKind)) {
                var fmChart = AppHost.Container.Resolve<ITreeChartWin>(fView, selPerson);
                fmChart.ChartKind = chartKind;
                fmChart.GenChart();
                AppHost.Instance.ShowWindow(fmChart);
            }
        }

        public void ShowCircleChart(CircleChartType chartKind)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) return;

            if (BaseController.DetectCycle(selPerson)) return;

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

        #endregion
    }
}
