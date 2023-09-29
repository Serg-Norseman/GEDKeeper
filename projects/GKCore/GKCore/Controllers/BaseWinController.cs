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

#define GEDML_SUPPORT
#define FAMX_SUPPORT

using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public sealed class TabParts
    {
        public readonly IListView ListView;
        public readonly string SplitterName;
        public readonly IHyperView Summary;

        public TabParts(IListView listView, string splitterName, IHyperView summary)
        {
            ListView = listView;
            SplitterName = splitterName;
            Summary = summary;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class BaseWinController : FormController<IBaseWindowView>
    {
        private readonly List<GDMRecord> fChangedRecords;
        private readonly IBaseContext fContext;
        private GDMRecord fDelayedTransitionRecord;
        private bool fHasToolbar;
        private readonly NavigationStack<GDMRecord> fNavman;
        private readonly TabParts[] fTabParts;

        public IBaseContext Context
        {
            get { return fContext; }
        }

        public bool HasToolbar
        {
            get { return fHasToolbar; }
            set { fHasToolbar = value; }
        }

        public NavigationStack<GDMRecord> Navman
        {
            get { return fNavman; }
        }


        public BaseWinController(IBaseWindowView view, bool hasToolbar) : base(view)
        {
            fContext = new BaseContext(view);
            fChangedRecords = new List<GDMRecord>();
            fNavman = new NavigationStack<GDMRecord>();
            fTabParts = new TabParts[(int)GDMRecordType.rtLast + 1];
            fHasToolbar = hasToolbar;
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
            fChangedRecords.Clear();
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
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                AppHost.Instance.CreateBase("");
            } else {
                CreateNewFile();
            }
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

        private void PrepareLoadFile(out string homePath, out string filters)
        {
            homePath = AppHost.Instance.GetUserFilesPath("");

            filters = LangMan.LS(LSID.LSID_GEDCOMFilter);

#if GEDML_SUPPORT
            filters += "|" + LangMan.LS(LSID.LSID_GedMLFilter);
#endif

#if FAMX_SUPPORT
            filters += "|" + "Family.Show files (*.familyx)|*.familyx";
#endif
        }

        public void LoadFileEx()
        {
            string homePath, filters;
            PrepareLoadFile(out homePath, out filters);

            string fileName = AppHost.StdDialogs.GetOpenFile("", homePath, filters, 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                AppHost.Instance.LoadBase(fView, fileName);
            }
        }

        public async void LoadFileAsync()
        {
            string homePath, filters;
            PrepareLoadFile(out homePath, out filters);

            string fileName = await AppHost.StdDialogs.GetOpenFileAsync("", homePath, filters, 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                AppHost.Instance.LoadBase(fView, fileName);
            }
        }

        public void SaveFile(string fileName)
        {
            if (fContext.FileSave(fileName)) {
                fContext.Modified = false;
                ChangeFileName();
                AppHost.Instance.BaseSaved(fView, fileName);
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
                string newFileName = AppHost.StdDialogs.GetSaveFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, oldFileName, GlobalOptions.Instance.FilesOverwriteWarn);
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

        private void ApplyFilter(GDMRecordType rt, IRecordsListModel listMan)
        {
            AppHost.Instance.NotifyFilter(fView, rt, listMan, listMan.Filter);
            ApplyFilter(rt);
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

                IListView listview = fTabParts[(int)rt].ListView;
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

            GDMRecord record = BaseController.AddRecord(fView, fView, rt, null);
            if (record != null) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public void EditRecord()
        {
            GDMRecord record = GetSelectedRecordEx();
            if (record != null && BaseController.EditRecord(fView, fView, record)) {
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

        public void ChangeListItem(IListView sender)
        {
            GDMRecord rec = sender.GetSelectedData() as GDMRecord;
            if (rec != null) {
                NavAdd(rec);
            }
            ShowRecordInfo(rec);
        }

        public void SelectSummaryLink(string linkName)
        {
            if (linkName.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                GKUtils.LoadExtFile(linkName);
                return;
            }

            if (linkName.StartsWith(GKData.INFO_HREF_VIEW)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_VIEW.Length);
                var mmRec = fContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
                if (mmRec != null) {
                    fView.ShowMedia(mmRec, false);
                }
            } else if (linkName.StartsWith(GKData.INFO_HREF_FILTER_INDI)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_FILTER_INDI.Length);
                var rec = fContext.Tree.FindXRef<GDMRecord>(xref);
                if (rec is GDMSourceRecord) {
                    var listMan = GetRecordsListManByType(GDMRecordType.rtIndividual);
                    IndividualListFilter iFilter = (IndividualListFilter)listMan.Filter;
                    iFilter.SourceMode = FilterGroupMode.Selected;
                    iFilter.SourceRef = rec.XRef;
                    ApplyFilter(GDMRecordType.rtIndividual, listMan);
                    fView.ShowRecordsTab(GDMRecordType.rtIndividual);
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

            IListView rView = (record == null) ? null : GetRecordsViewByType(record.RecordType);
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
                IListView rView = GetRecordsViewByType(record.RecordType);
                result = (rView != null && rView.ListMan.IndexOfItem(record) >= 0);
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

        public void CopyContent(IHyperView hyperView)
        {
            if (hyperView == null) return;

            var text = Regex.Replace(hyperView.Lines.Text, @"\[.*?\]", string.Empty);
            AppHost.Instance.SetClipboardText(text);
        }

        #endregion

        #region UI

        public void SetTabPart(GDMRecordType recType, IListView listView, string splitterName, IHyperView summary)
        {
            fTabParts[(int)recType] = new TabParts(listView, splitterName, summary);
        }

        public void SetSummaryWidth(bool uiAction)
        {
            if (!GlobalOptions.Instance.KeepInfoPansOverallSize)
                return;

            try {
                int currentTab, splitterPos;

                if (uiAction) {
                    currentTab = fView.RecordTabs.SelectedIndex + 1;
                    splitterPos = GetControl<ISplitter>(fTabParts[currentTab].SplitterName).Position;
                    GlobalOptions.Instance.InfoPansOverallSize = splitterPos;
                } else {
                    currentTab = 0;
                    splitterPos = GlobalOptions.Instance.InfoPansOverallSize;
                    if (splitterPos <= 0) splitterPos = 300;
                }

                for (int i = 0; i < fTabParts.Length; i++) {
                    var tab = fTabParts[i];
                    if (i != currentTab && tab != null) {
                        var splitterHandler = GetControl<ISplitter>(tab.SplitterName);
                        fView.EnableSplitterEvent(splitterHandler, false);
                        splitterHandler.Position = splitterPos;
                        fView.EnableSplitterEvent(splitterHandler, true);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinController.SetSummaryWidth()", ex);
            }
        }

        public GDMRecordType GetSelectedRecordType()
        {
            return (GDMRecordType)(fView.RecordTabs.SelectedIndex + 1);
        }

        public IListView GetRecordsViewByType(GDMRecordType recType)
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

        public IRecordsListModel GetRecordsListManByType(GDMRecordType recType)
        {
            IListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : (IRecordsListModel)rView.ListMan;
        }

        public GDMRecord GetSelectedRecordEx()
        {
            GDMRecordType recType = GetSelectedRecordType();
            IListView rView = GetRecordsViewByType(recType);
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
                IListView listview = fTabParts[(int)rt].ListView;
                if (listview != null) {
                    listview.UpdateContents(columnsChanged);
                }
            }

            AppHost.Instance.UpdateControls(false);
        }

        public List<GDMRecord> GetContentList(GDMRecordType recType)
        {
            IListView rView = GetRecordsViewByType(recType);
            return (rView == null) ? null : ((IRecordsListModel)rView.ListMan).GetRecordsList();
        }

        public void RestoreListsSettings()
        {
            var globOptions = GlobalOptions.Instance;
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                IListView rView = fTabParts[(int)rt].ListView;
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
                IListView rView = fTabParts[(int)rt].ListView;
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
            IListView rView = GetRecordsViewByType(recType);
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
            SetSummaryWidth(false);
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
            IRecordsListModel listMan = GetRecordsListManByType(rt);
            IList<ISearchResult> result = (listMan == null) ? new List<ISearchResult>() : listMan.FindAll(searchPattern);
            return result;
        }

        public void SetFilter()
        {
            if (!fView.AllowFilter()) return;

            GDMRecordType rt = GetSelectedRecordType();
            IRecordsListModel listMan = GetRecordsListManByType(rt);
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
                    img = gfxProvider.LoadResourceImage("Resources.rg_shield_none.gif", true);
                    break;
                case ShieldState.Middle:
                    img = gfxProvider.LoadResourceImage("Resources.rg_shield_mid.gif", true);
                    break;
                case ShieldState.Maximum:
                    img = gfxProvider.LoadResourceImage("Resources.rg_shield_max.gif", true);
                    break;
            }
            return img;
        }

        #endregion

        public override void UpdateView()
        {
        }

        public void UpdateNavControls()
        {
            try {
                if (fHasToolbar) {
                    GetControl<IToolItem>("tbPrev").Enabled = NavCanBackward();
                    GetControl<IToolItem>("tbNext").Enabled = NavCanForward();
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinController.UpdateNavControls()", ex);
            }
        }

        public void UpdateControls(bool forceDeactivate, bool blockDependent = false)
        {
            try {
                IWorkWindow workWin = AppHost.Instance.GetWorkWindow();
                IBaseWindow curBase = ((forceDeactivate) ? null : AppHost.Instance.GetCurrentFile());
                IChartWindow curChart = ((workWin is IChartWindow) ? ((IChartWindow)workWin) : null);

                GDMRecordType rt = (curBase == null) ? GDMRecordType.rtNone : curBase.GetSelectedRecordType();
                bool baseEn = (rt != GDMRecordType.rtNone);
                bool indivEn = baseEn && rt == GDMRecordType.rtIndividual;
                bool ifEn = baseEn && (rt == GDMRecordType.rtIndividual || rt == GDMRecordType.rtFamily);

                GetControl<IMenuItem>("miFileSave").Enabled = baseEn || (curChart != null);
                GetControl<IMenuItem>("miFileSaveAs").Enabled = GetControl<IMenuItem>("miFileSave").Enabled;
                GetControl<IMenuItem>("miFileClose").Enabled = baseEn;
                GetControl<IMenuItem>("miFileProperties").Enabled = baseEn;
                GetControl<IMenuItem>("miExportTable").Enabled = baseEn;

                GetControl<IMenuItem>("miRecordAdd").Enabled = baseEn;
                GetControl<IMenuItem>("miRecordEdit").Enabled = baseEn;
                GetControl<IMenuItem>("miRecordDelete").Enabled = baseEn;
                GetControl<IMenuItem>("miFilter").Enabled = (workWin != null && workWin.AllowFilter());
                GetControl<IMenuItem>("miSearch").Enabled = (workWin != null && workWin.AllowQuickSearch());

                GetControl<IMenuItem>("miPedigree").Enabled = indivEn;
                GetControl<IMenuItem>("miTreeAncestors").Enabled = ifEn;
                GetControl<IMenuItem>("miTreeDescendants").Enabled = ifEn;
                GetControl<IMenuItem>("miTreeBoth").Enabled = ifEn;

                GetControl<IMenuItem>("miPedigreeAscend").Enabled = indivEn;
                GetControl<IMenuItem>("miPedigreeDescend").Enabled = indivEn;

                GetControl<IMenuItem>("miStats").Enabled = baseEn;
                GetControl<IMenuItem>("miExportToFamilyBook").Enabled = baseEn;
                GetControl<IMenuItem>("miExportToTreesAlbum").Enabled = baseEn;

                GetControl<IMenuItem>("miTreeTools").Enabled = baseEn;
                GetControl<IMenuItem>("miOrganizer").Enabled = baseEn;
                GetControl<IMenuItem>("miSlideshow").Enabled = baseEn;
                GetControl<IMenuItem>("miScripts").Enabled = baseEn;

                if (fHasToolbar) {
                    GetControl<IToolItem>("tbFileSave").Enabled = GetControl<IMenuItem>("miFileSave").Enabled;
                    GetControl<IToolItem>("tbRecordAdd").Enabled = GetControl<IMenuItem>("miRecordAdd").Enabled;
                    GetControl<IToolItem>("tbRecordEdit").Enabled = GetControl<IMenuItem>("miRecordEdit").Enabled;
                    GetControl<IToolItem>("tbRecordDelete").Enabled = GetControl<IMenuItem>("miRecordDelete").Enabled;
                    GetControl<IToolItem>("tbStats").Enabled = GetControl<IMenuItem>("miStats").Enabled;
                    GetControl<IToolItem>("tbFilter").Enabled = GetControl<IMenuItem>("miFilter").Enabled;
                    GetControl<IToolItem>("tbTreeAncestors").Enabled = GetControl<IMenuItem>("miTreeAncestors").Enabled;
                    GetControl<IToolItem>("tbTreeDescendants").Enabled = GetControl<IMenuItem>("miTreeDescendants").Enabled;
                    GetControl<IToolItem>("tbTreeBoth").Enabled = GetControl<IMenuItem>("miTreeBoth").Enabled;

                    GetControl<IToolItem>("tbPedigree").Enabled = GetControl<IMenuItem>("miPedigree").Enabled;
                    GetControl<IMenuItem>("miPedigreeAscend2").Enabled = indivEn;
                    GetControl<IMenuItem>("miPedigreeDescend2").Enabled = indivEn;
                }

                UpdateNavControls();

                if (workWin != null && !blockDependent) {
                    workWin.UpdateControls();
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinController.UpdateControls()", ex);
            }
        }

        public override void SetLocale()
        {
            try {
                GetControl<IMenuItem>("miFile").Text = LangMan.LS(LSID.LSID_MIFile);
                GetControl<IMenuItem>("miEdit").Text = LangMan.LS(LSID.LSID_MIEdit);
                GetControl<IMenuItem>("miPedigree").Text = LangMan.LS(LSID.LSID_MIPedigree);
                GetControl<IMenuItem>("miService").Text = LangMan.LS(LSID.LSID_MIService);
                GetControl<IMenuItem>("miReports").Text = LangMan.LS(LSID.LSID_Reports);
                GetControl<IMenuItem>("miPlugins").Text = LangMan.LS(LSID.LSID_Plugins);
                GetControl<IMenuItem>("miHelp").Text = LangMan.LS(LSID.LSID_MIHelp);

                GetControl<IMenuItem>("miFileNew").Text = LangMan.LS(LSID.LSID_MIFileNew);
                GetControl<IMenuItem>("miFileLoad").Text = LangMan.LS(LSID.LSID_MIFileLoad);
                GetControl<IMenuItem>("miMRUFiles").Text = LangMan.LS(LSID.LSID_MIMRUFiles);
                GetControl<IMenuItem>("miFileSave").Text = LangMan.LS(LSID.LSID_MIFileSave);
                GetControl<IMenuItem>("miFileSaveAs").Text = LangMan.LS(LSID.LSID_MIFileSaveAs);
                GetControl<IMenuItem>("miFileClose").Text = LangMan.LS(LSID.LSID_MIFileClose);
                GetControl<IMenuItem>("miFileProperties").Text = LangMan.LS(LSID.LSID_MIFileProperties) + @"...";
                GetControl<IMenuItem>("miExport").Text = LangMan.LS(LSID.LSID_MIExport);
                GetControl<IMenuItem>("miExportToFamilyBook").Text = LangMan.LS(LSID.LSID_MIExportToFamilyBook);
                GetControl<IMenuItem>("miExportToTreesAlbum").Text = LangMan.LS(LSID.LSID_TreesAlbum);
                GetControl<IMenuItem>("miExportTable").Text = LangMan.LS(LSID.LSID_ExportTable);
                GetControl<IMenuItem>("miExit").Text = LangMan.LS(LSID.LSID_MIExit);

                GetControl<IMenuItem>("miRecordAdd").Text = LangMan.LS(LSID.LSID_MIRecordAdd);
                GetControl<IMenuItem>("miRecordEdit").Text = LangMan.LS(LSID.LSID_MIRecordEdit);
                GetControl<IMenuItem>("miRecordDelete").Text = LangMan.LS(LSID.LSID_MIRecordDelete);
                GetControl<IMenuItem>("miSearch").Text = LangMan.LS(LSID.LSID_Search);
                GetControl<IMenuItem>("miFindAndReplace").Text = LangMan.LS(LSID.LSID_FindAndReplace);
                GetControl<IMenuItem>("miFilter").Text = LangMan.LS(LSID.LSID_MIFilter) + @"...";

                GetControl<IMenuItem>("miTreeAncestors").Text = LangMan.LS(LSID.LSID_MITreeAncestors);
                GetControl<IMenuItem>("miTreeDescendants").Text = LangMan.LS(LSID.LSID_MITreeDescendants);
                GetControl<IMenuItem>("miTreeBoth").Text = LangMan.LS(LSID.LSID_MITreeBoth);
                GetControl<IMenuItem>("miPedigreeAscend").Text = LangMan.LS(LSID.LSID_MIPedigreeAscend);
                GetControl<IMenuItem>("miPedigreeDescend").Text = LangMan.LS(LSID.LSID_MIPedigreeDescend);
                GetControl<IMenuItem>("miMap").Text = LangMan.LS(LSID.LSID_MIMap) + @"...";
                GetControl<IMenuItem>("miStats").Text = LangMan.LS(LSID.LSID_MIStats) + @"...";
                GetControl<IMenuItem>("miAncestorsCircle").Text = LangMan.LS(LSID.LSID_AncestorsCircle);
                GetControl<IMenuItem>("miDescendantsCircle").Text = LangMan.LS(LSID.LSID_DescendantsCircle);
                GetControl<IMenuItem>("miRelationshipCalculator").Text = LangMan.LS(LSID.LSID_RelationshipCalculator);

                GetControl<IMenuItem>("miOrganizer").Text = LangMan.LS(LSID.LSID_MIOrganizer) + @"...";
                GetControl<IMenuItem>("miSlideshow").Text = LangMan.LS(LSID.LSID_Slideshow) + @"...";
                GetControl<IMenuItem>("miScripts").Text = LangMan.LS(LSID.LSID_MIScripts);
                GetControl<IMenuItem>("miTreeTools").Text = LangMan.LS(LSID.LSID_MITreeTools);
                GetControl<IMenuItem>("miOptions").Text = LangMan.LS(LSID.LSID_MIOptions) + @"...";

                GetControl<IMenuItem>("miTreeCompare").Text = LangMan.LS(LSID.LSID_ToolOp_1);
                GetControl<IMenuItem>("miTreeMerge").Text = LangMan.LS(LSID.LSID_ToolOp_2);
                GetControl<IMenuItem>("miTreeSplit").Text = LangMan.LS(LSID.LSID_ToolOp_3);
                GetControl<IMenuItem>("miRecMerge").Text = LangMan.LS(LSID.LSID_MergeDuplicates);
                GetControl<IMenuItem>("miFamilyGroups").Text = LangMan.LS(LSID.LSID_ToolOp_6);
                GetControl<IMenuItem>("miTreeCheck").Text = LangMan.LS(LSID.LSID_ToolOp_7);
                GetControl<IMenuItem>("miPatSearch").Text = LangMan.LS(LSID.LSID_ToolOp_8);
                GetControl<IMenuItem>("miPlacesManager").Text = LangMan.LS(LSID.LSID_ToolOp_9);

                GetControl<IMenuItem>("miContext").Text = LangMan.LS(LSID.LSID_MIContext);
                GetControl<IMenuItem>("miAbout").Text = LangMan.LS(LSID.LSID_MIAbout) + @"...";
                GetControl<IMenuItem>("miLogSend").Text = LangMan.LS(LSID.LSID_LogSend);
                GetControl<IMenuItem>("miLogView").Text = LangMan.LS(LSID.LSID_LogView);

                GetControl<IMenuItem>("miWindow").Text = LangMan.LS(LSID.LSID_MIWindow);
                GetControl<IMenuItem>("miWinCascade").Text = LangMan.LS(LSID.LSID_MIWinCascade);
                GetControl<IMenuItem>("miWinHTile").Text = LangMan.LS(LSID.LSID_MIWinHTile);
                GetControl<IMenuItem>("miWinVTile").Text = LangMan.LS(LSID.LSID_MIWinVTile);
                GetControl<IMenuItem>("miWinMinimize").Text = LangMan.LS(LSID.LSID_MIWinMinimize);

                if (fHasToolbar) {
                    SetToolTip("tbFileNew", LangMan.LS(LSID.LSID_FileNewTip));
                    SetToolTip("tbFileLoad", LangMan.LS(LSID.LSID_FileLoadTip));
                    SetToolTip("tbFileSave", LangMan.LS(LSID.LSID_FileSaveTip));
                    SetToolTip("tbRecordAdd", LangMan.LS(LSID.LSID_RecordAddTip));
                    SetToolTip("tbRecordEdit", LangMan.LS(LSID.LSID_RecordEditTip));
                    SetToolTip("tbRecordDelete", LangMan.LS(LSID.LSID_RecordDeleteTip));
                    SetToolTip("tbFilter", LangMan.LS(LSID.LSID_FilterTip));
                    SetToolTip("tbTreeAncestors", LangMan.LS(LSID.LSID_TreeAncestorsTip));
                    SetToolTip("tbTreeDescendants", LangMan.LS(LSID.LSID_TreeDescendantsTip));
                    SetToolTip("tbTreeBoth", LangMan.LS(LSID.LSID_TreeBothTip));
                    SetToolTip("tbPedigree", LangMan.LS(LSID.LSID_PedigreeTip));
                    SetToolTip("tbStats", LangMan.LS(LSID.LSID_StatsTip));
                    SetToolTip("tbPrev", LangMan.LS(LSID.LSID_PrevRec));
                    SetToolTip("tbNext", LangMan.LS(LSID.LSID_NextRec));

                    GetControl<IMenuItem>("miPedigreeAscend2").Text = LangMan.LS(LSID.LSID_MIPedigreeAscend);
                    GetControl<IMenuItem>("miPedigreeDescend2").Text = LangMan.LS(LSID.LSID_MIPedigreeDescend);
                }

                GetControl<IMenuItem>("miContRecordAdd").Text = LangMan.LS(LSID.LSID_MIRecordAdd);
                GetControl<IMenuItem>("miContRecordEdit").Text = LangMan.LS(LSID.LSID_MIRecordEdit);
                GetControl<IMenuItem>("miContRecordDelete").Text = LangMan.LS(LSID.LSID_MIRecordDelete);
                GetControl<IMenuItem>("miContRecordDuplicate").Text = LangMan.LS(LSID.LSID_RecordDuplicate);
                GetControl<IMenuItem>("miContRecordMerge").Text = LangMan.LS(LSID.LSID_MergeDuplicates);

                GetControl<IMenuItem>("miCopyContent").Text = LangMan.LS(LSID.LSID_Copy);

                var tabControl = GetControl<ITabControl>("tabsRecords");
                if (tabControl.Pages.Count >= 11) {
                    tabControl.Pages[0].Text = LangMan.LS(LSID.LSID_RPIndividuals);
                    tabControl.Pages[1].Text = LangMan.LS(LSID.LSID_RPFamilies);
                    tabControl.Pages[2].Text = LangMan.LS(LSID.LSID_RPNotes);
                    tabControl.Pages[3].Text = LangMan.LS(LSID.LSID_RPMultimedia);
                    tabControl.Pages[4].Text = LangMan.LS(LSID.LSID_RPSources);
                    tabControl.Pages[5].Text = LangMan.LS(LSID.LSID_RPRepositories);
                    tabControl.Pages[6].Text = LangMan.LS(LSID.LSID_RPGroups);
                    tabControl.Pages[7].Text = LangMan.LS(LSID.LSID_RPResearches);
                    tabControl.Pages[8].Text = LangMan.LS(LSID.LSID_RPTasks);
                    tabControl.Pages[9].Text = LangMan.LS(LSID.LSID_RPCommunications);
                    tabControl.Pages[10].Text = LangMan.LS(LSID.LSID_RPLocations);
                }

                var miPlugins = GetControl<IMenuItem>("miPlugins");
                int num = miPlugins.SubItems.Count;
                for (int i = 0; i < num; i++) {
                    var mi = miPlugins.SubItems[i];
                    IPlugin plugin = (IPlugin)mi.Tag;
                    mi.Text = plugin.DisplayName;
                }

                var miThemes = GetControl<IMenuItem>("miThemes");
                miThemes.Text = LangMan.LS(LSID.LSID_Themes);
                if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) {
                    miThemes.Enabled = false;
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseWinSDI.SetLocale()", ex);
            }
        }

        public override void ApplyTheme()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.Themes)) {
                // menu
                GetControl<IMenuItem>("miFileNew").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileNew);
                GetControl<IMenuItem>("miFileLoad").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileLoad);
                GetControl<IMenuItem>("miFileSave").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileSave);
                GetControl<IMenuItem>("miFileProperties").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileProperties);
                GetControl<IMenuItem>("miExport").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Export);
                GetControl<IMenuItem>("miExportTable").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ExportTable);
                GetControl<IMenuItem>("miExit").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Exit);

                GetControl<IMenuItem>("miRecordAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordAdd);
                GetControl<IMenuItem>("miRecordEdit").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordEdit);
                GetControl<IMenuItem>("miRecordDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordDelete);
                GetControl<IMenuItem>("miSearch").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Search);
                GetControl<IMenuItem>("miFilter").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Filter);

                GetControl<IMenuItem>("miTreeAncestors").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeAncestors);
                GetControl<IMenuItem>("miTreeDescendants").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeDescendants);
                GetControl<IMenuItem>("miTreeBoth").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeBoth);
                GetControl<IMenuItem>("miPedigreeAscend").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Pedigree);
                GetControl<IMenuItem>("miPedigreeDescend").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Pedigree);
                GetControl<IMenuItem>("miMap").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Maps);
                GetControl<IMenuItem>("miStats").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Stats);

                GetControl<IMenuItem>("miOrganizer").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Organizer);
                GetControl<IMenuItem>("miSlideshow").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Slideshow);
                GetControl<IMenuItem>("miOptions").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Settings);

                GetControl<IMenuItem>("miContext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Help);
                GetControl<IMenuItem>("miAbout").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_About);

                // toolbar
                //GetControl<IMenuItem>("tbFileNew").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileNew);
                //GetControl<IMenuItem>("tbFileLoad").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileLoad);
                //GetControl<IMenuItem>("tbFileSave").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileSave);
                //GetControl<IMenuItem>("tbRecordAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordAdd);
                //GetControl<IMenuItem>("tbRecordEdit").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordEdit);
                //GetControl<IMenuItem>("tbRecordDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordDelete);
                //GetControl<IMenuItem>("tbFilter").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Filter);
                //GetControl<IMenuItem>("tbTreeAncestors").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeAncestors);
                //GetControl<IMenuItem>("tbTreeDescendants").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeDescendants);
                //GetControl<IMenuItem>("tbTreeBoth").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeBoth);
                //GetControl<IMenuItem>("tbPedigree").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Pedigree);
                //GetControl<IMenuItem>("tbStats").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Stats);
                //GetControl<IMenuItem>("tbPrev").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev);
                //GetControl<IMenuItem>("tbNext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next);
                //GetControl<IMenuItem>("tbSendMail").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_SendMail);
            }
        }

        #region Dialogs

        private void ShowCommonFilter(GDMRecordType rt, IRecordsListModel listMan)
        {
            using (var dlg = AppHost.Container.Resolve<ICommonFilterDlg>(fView, listMan)) {
                if (AppHost.Instance.ShowModalX(dlg, fView, false)) {
                    AppHost.Instance.NotifyFilter(fView, rt, listMan, listMan.Filter);
                    ApplyFilter(rt);
                }
            }
        }

        private void ShowPersonsFilter(GDMRecordType rt, IRecordsListModel listMan)
        {
            using (var dlg = AppHost.Container.Resolve<IPersonsFilterDlg>(fView, listMan)) {
                if (AppHost.Instance.ShowModalX(dlg, fView, false)) {
                    ApplyFilter(rt, listMan);
                }
            }
        }

        public void ExportToFamilyBook()
        {
            using (FamilyBookExporter fb = new FamilyBookExporter(fView)) {
                fb.Generate(true);
            }
        }

        public void ExportToTreesAlbum()
        {
            AppHost.StdDialogs.ShowWarning(@"This function is experimental and not completed. Only for PDF!");

            using (TreesAlbumExporter ta = new TreesAlbumExporter(fView)) {
                ta.Generate(true);
            }
        }

        public void ExportTable()
        {
            using (TableExporter exExp = new TableExporter(fView)) {
                exExp.Options = AppHost.Options;
                exExp.Generate(true);
            }
        }

        public void ShowFileProperties()
        {
            try {
                fContext.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<IFilePropertiesDlg>(fView)) {
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
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
                    AppHost.Instance.ShowModalX(dlg, fView, false);
                }
            } finally {
                fContext.EndUpdate();
            }
        }

        public void ShowFamilyGroups()
        {
            try {
                var dlg = AppHost.Container.Resolve<IFragmentSearchDlg>(fView);
                AppHost.Instance.ShowWindow(dlg);
            } finally {
            }
        }

        public void SendMail()
        {
            if (fView.CheckModified()) {
                string fileName = Path.GetFileName(fContext.FileName);
                SysUtils.SendMail("", fileName, "?", fContext.FileName);
            }
        }

        public void ShowMap()
        {
            var mapsWin = AppHost.Container.Resolve<IMapsViewerWin>(fView);
            AppHost.Instance.ShowWindow(mapsWin);
        }

        public void ShowOrganizer()
        {
            using (var dlg = AppHost.Container.Resolve<IOrganizerWin>(fView)) {
                AppHost.Instance.ShowModalX(dlg, fView, false);
            }
        }

        public void ShowRelationshipCalculator()
        {
            using (var dlg = AppHost.Container.Resolve<IRelationshipCalculatorDlg>(fView)) {
                AppHost.Instance.ShowModalX(dlg, fView, false);
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

        public void GeneratePedigree(PedigreeType type)
        {
            var selPerson = GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            if (BaseController.DetectCycle(fContext.Tree, selPerson)) return;

            using (var p = new PedigreeExporter(fView, selPerson)) {
                p.Options = AppHost.Options;
                p.Type = type;
                p.Generate(true);
            }
        }

        private GDMIndividualRecord GetSelectedPersonVar()
        {
            var selRec = GetSelectedRecordEx();

            if (selRec is GDMFamilyRecord) {
                var famRec = (GDMFamilyRecord)selRec;
                var tree = fContext.Tree;

                selRec = tree.GetPtrValue(famRec.Husband);
                if (selRec == null) {
                    selRec = tree.GetPtrValue(famRec.Wife);
                }
            }

            return selRec as GDMIndividualRecord;
        }

        public void ShowTreeChart(TreeChartKind chartKind)
        {
            BaseController.ShowTreeChart(fView, GetSelectedPersonVar(), chartKind);
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
                AppHost.Instance.ShowModalX(dlg, fView, false);
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

                fView.ReportsItem.Enabled = (fView.ReportsItem.SubItems.Count > 0);
                fView.PluginsItem.Enabled = (fView.PluginsItem.SubItems.Count > 0);
            } catch (Exception ex) {
                Logger.WriteError("BaseWinController.UpdatePluginsItems()", ex);
            }
        }

        public void FindAndReplace()
        {
            var win = AppHost.Container.Resolve<IFARDlg>(fView);
            AppHost.Instance.ShowWindow(win);
        }

        #endregion
    }
}
