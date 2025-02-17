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
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PartialViewController : FormController<IPartialView>
    {
        private readonly List<GDMRecord> fChangedRecords;
        private GDMRecord fDelayedTransitionRecord;
        private readonly NavigationStack<GDMRecord> fNavman;
        private readonly GDMRecordType fRecordType;

        private IListView fListView;
        private string fSplitterName;
        private IHyperView fSummary;


        public GDMRecordType RecordType
        {
            get { return fRecordType; }
        }


        public PartialViewController(IPartialView view, IBaseWindow baseWin, GDMRecordType recordType) : base(view)
        {
            fChangedRecords = new List<GDMRecord>();
            fNavman = new NavigationStack<GDMRecord>();

            Init(baseWin);
            fRecordType = recordType;
        }

        #region Context

        private void ApplyFilter(GDMRecordType rt, IRecordsListModel listMan)
        {
            AppHost.Instance.NotifyFilter(fBase, rt, listMan, listMan.Filter);
            ApplyFilter(rt);
        }

        public void ApplyFilter(GDMRecordType recType = GDMRecordType.rtNone)
        {
            if (fBase.Context.Tree.RecordsCount > 0) {
                if (recType == GDMRecordType.rtNone) {
                    RefreshLists(false);
                } else {
                    RefreshRecordsView(recType);
                }
            }
        }

        public void NotifyRecord(GDMRecord record, RecordAction action)
        {
            if (record == null) return;

            switch (action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    CheckChangedRecord(record, true);
                    break;

                case RecordAction.raDelete: {
                        CheckChangedRecord(record, false);
                        fListView.DeleteRecord(record);
                        if (fListView.ListMan.FilteredCount == 0) {
                            fSummary.Lines.Clear();
                        }
                    }
                    break;

                case RecordAction.raJump:
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    break;
            }

            BaseController.NotifyRecord(fBase, record, action);
        }

        public void DuplicateRecord()
        {
            GDMRecord original = GetSelectedRecordEx();
            GDMRecord target = BaseController.DuplicateRecord(fBase.Context, original);
            if (target != null) {
                NotifyRecord(target, RecordAction.raAdd);
                RefreshLists(false);
                SelectRecordByXRef(target.XRef);
            }
        }

        public async void AddRecord()
        {
            GDMRecord record = await BaseController.AddRecord(fView, fBase, fRecordType, null);
            if (record != null) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public async void EditRecord()
        {
            GDMRecord record = GetSelectedRecordEx();
            if (record != null && await BaseController.EditRecord(fView, fBase, record)) {
                RefreshLists(false);
            }

            UpdateChangedRecords(record);
        }

        public async void DeleteRecord()
        {
            GDMRecord record = GetSelectedRecordEx();
            if (record != null && await BaseController.DeleteRecord(fBase, record, true)) {
                RefreshLists(false);
            }
        }

        public void ShowRecordInfo(GDMRecord record)
        {
            if (record == null) return;

            try {
                GKUtils.GetRecordContent(fBase.Context, record, fSummary.Lines, RecordContentType.Full);
            } catch (Exception ex) {
                Logger.WriteError("PartialViewController.ShowRecordInfo()", ex);
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

        public void SelectSummaryLink(IHyperView sender, string linkName)
        {
            if (linkName.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                GKUtils.LoadExtFile(linkName);
                return;
            }

            if (linkName.StartsWith(GKData.INFO_HREF_VIEW)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_VIEW.Length);
                var mmRec = fBase.Context.Tree.FindXRef<GDMMultimediaRecord>(xref);
                if (mmRec != null) {
                    fBase.ShowMedia(mmRec, false);
                }
            } else if (linkName.StartsWith(GKData.INFO_HREF_FILTER_INDI)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_FILTER_INDI.Length);
                var rec = fBase.Context.Tree.FindXRef<GDMRecord>(xref);
                if (rec is GDMSourceRecord) {
                    var listMan = GetRecordsListManByType();
                    IndividualListFilter iFilter = (IndividualListFilter)listMan.Filter;
                    iFilter.SourceMode = FilterGroupMode.Selected;
                    iFilter.SourceRef = rec.XRef;
                    ApplyFilter(GDMRecordType.rtIndividual, listMan);
                }
            } else if (linkName.StartsWith(GKData.INFO_HREF_LOC_SUB)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_LOC_SUB.Length);
                var locRec = fBase.Context.Tree.FindXRef<GDMLocationRecord>(xref);
                if (locRec != null) BaseController.ShowMap_Sub(fBase, locRec);
            } else if (linkName.StartsWith(GKData.INFO_HREF_LOC_INDI)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_LOC_INDI.Length);
                var locRec = fBase.Context.Tree.FindXRef<GDMLocationRecord>(xref);
                if (locRec != null) BaseController.ShowMap_Indi(fBase, locRec);
            } else if (linkName.StartsWith(GKData.INFO_HREF_EXPAND_ASSO)) {
                GKUtils.ExpandExtInfo(fBase.Context, sender, linkName);
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
            GDMRecord record = fBase.Context.Tree.XRefIndex_Find(xref);

            if (delayedTransition) {
                fDelayedTransitionRecord = record;
                return;
            }

            if (fDelayedTransitionRecord != null) {
                record = fDelayedTransitionRecord;
                fDelayedTransitionRecord = null;
            }

            if (record != null && record.RecordType == fRecordType) {
                fListView.Activate();
                fListView.SelectItem(record);
            }
        }

        public void CopyContent()
        {
            CopyContent(fSummary);
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
            fListView = listView;
            fSummary = summary;
            fSplitterName = splitterName;
        }

        /// <summary>
        /// Sets tab splitter positions in response to interface events.
        /// </summary>
        /// <param name="userChange">true - if user change event (SplitterMoved), false - settings change event</param>
        public void SetSummaryWidth(bool userChange)
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile))
                return;

            try {
                GlobalOptions globOpts = GlobalOptions.Instance;

                if (userChange) {
                    int splitterPos = GetControl<ISplitter>(fSplitterName).Position;

                    if (globOpts.KeepInfoPansOverallSize) {
                        globOpts.InfoPansOverallSize = splitterPos;
                    } else {
                        globOpts.ListOptions[fRecordType].SplitterPosition = splitterPos;
                    }
                }

                if (globOpts.KeepInfoPansOverallSize) {
                    SetSplitterPos(globOpts.InfoPansOverallSize);
                } else {
                    if (!userChange) {
                        SetSplitterPos(globOpts.ListOptions[fRecordType].SplitterPosition);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("PartialViewController.SetSummaryWidth()", ex);
            }
        }

        private void SetSplitterPos(int splitterPos)
        {
            if (splitterPos <= 0) splitterPos = 300;

            var splitterHandler = GetControl<ISplitter>(fSplitterName);
            splitterHandler.Position = splitterPos;
        }

        public IListView GetRecordsViewByType()
        {
            return fListView;
        }

        public IRecordsListModel GetRecordsListManByType()
        {
            return (IRecordsListModel)fListView.ListMan;
        }

        public GDMRecord GetSelectedRecordEx()
        {
            return (fListView.GetSelectedData() as GDMRecord);
        }

        public void RefreshLists(bool columnsChanged)
        {
            fListView.UpdateContents(columnsChanged);
            AppHost.Instance.UpdateControls(false);
        }

        public void RestoreListsSettings()
        {
            var globOptions = GlobalOptions.Instance;

            var columnOpts = globOptions.ListOptions[fRecordType];
            fListView.SetSortColumn(columnOpts.SortColumn, false);
            columnOpts.Columns.CopyTo(fListView.ListMan.ListColumns);
        }

        public void SaveListsSettings()
        {
            var globOptions = GlobalOptions.Instance;

            var columnOpts = globOptions.ListOptions[fRecordType];
            columnOpts.SortColumn = fListView.SortColumn;
            fListView.ListMan.ListColumns.CopyTo(columnOpts.Columns);
        }

        public void RefreshRecordsView(GDMRecordType recType)
        {
            fListView.UpdateContents();
            AppHost.Instance.UpdateControls(false);
        }

        public void UpdateChangedRecords(GDMRecord select = null)
        {
            for (int i = fChangedRecords.Count - 1; i >= 0; i--) {
                var record = fChangedRecords[i];

                RefreshRecordsView(record.RecordType);
            }

            if (select != null) {
                SelectRecordByXRef(select.XRef);
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
            UpdateControls(false);
        }

        public void NavNext()
        {
            GDMRecord rec = fNavman.Next();
            if (rec != null) {
                SelectRecordByXRef(rec.XRef);
                UpdateControls(false);
            }
        }

        public void NavPrev()
        {
            GDMRecord rec = fNavman.Back();
            if (rec != null) {
                SelectRecordByXRef(rec.XRef);
                UpdateControls(false);
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

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            IRecordsListModel listMan = GetRecordsListManByType();
            IList<ISearchResult> result = (listMan == null) ? new List<ISearchResult>() : listMan.FindAll(searchPattern);
            return result;
        }

        public void SetFilter()
        {
            if (!fView.AllowFilter()) return;

            IRecordsListModel listMan = GetRecordsListManByType();
            if (listMan == null) return;

            switch (fRecordType) {
                case GDMRecordType.rtIndividual:
                    ShowPersonsFilter(listMan);
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
                    ShowCommonFilter(listMan);
                    break;
            }
        }

        #endregion

        public override void UpdateView()
        {
        }

        public void UpdateNavControls()
        {
            try {
                GetControl<IToolItem>("tbPrev").Enabled = NavCanBackward();
                GetControl<IToolItem>("tbNext").Enabled = NavCanForward();
            } catch (Exception ex) {
                Logger.WriteError("PartialViewController.UpdateNavControls()", ex);
            }
        }

        public void UpdateControls(bool forceDeactivate, bool blockDependent = false)
        {
            try {
                IWorkWindow workWin = fView;
                IBaseWindow curBase = (forceDeactivate) ? null : AppHost.Instance.GetCurrentFile();
                IChartWindow curChart = (workWin is IChartWindow) ? (IChartWindow)workWin : null;

                bool baseEn = (fRecordType != GDMRecordType.rtNone);
                bool indivEn = baseEn && fRecordType == GDMRecordType.rtIndividual;
                bool ifEn = baseEn && (fRecordType == GDMRecordType.rtIndividual || fRecordType == GDMRecordType.rtFamily);
                bool canSave = baseEn || (curChart != null);
                bool canFlt = workWin.AllowFilter();

                GetControl<IToolItem>("tbFileSave").Enabled = canSave;
                GetControl<IToolItem>("tbRecordAdd").Enabled = baseEn;
                GetControl<IToolItem>("tbRecordEdit").Enabled = baseEn;
                GetControl<IToolItem>("tbRecordDelete").Enabled = baseEn;
                GetControl<IToolItem>("tbFilter").Enabled = canFlt;
                GetControl<IToolItem>("tbTreeAncestors").Enabled = ifEn;
                GetControl<IToolItem>("tbTreeDescendants").Enabled = ifEn;
                GetControl<IToolItem>("tbTreeBoth").Enabled = ifEn;

                UpdateNavControls();

                if (!blockDependent) {
                    workWin.UpdateControls();
                }
            } catch (Exception ex) {
                Logger.WriteError("PartialViewController.UpdateControls()", ex);
            }
        }

        public override void SetLocale()
        {
            try {
                if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                    SetToolTip("tbFileSave", LangMan.LS(LSID.FileSaveTip));
                    SetToolTip("tbRecordAdd", LangMan.LS(LSID.RecordAddTip));
                    SetToolTip("tbRecordEdit", LangMan.LS(LSID.RecordEditTip));
                    SetToolTip("tbRecordDelete", LangMan.LS(LSID.RecordDeleteTip));
                    SetToolTip("tbFilter", LangMan.LS(LSID.FilterTip));
                    SetToolTip("tbTreeAncestors", LangMan.LS(LSID.TreeAncestorsTip));
                    SetToolTip("tbTreeDescendants", LangMan.LS(LSID.TreeDescendantsTip));
                    SetToolTip("tbTreeBoth", LangMan.LS(LSID.TreeBothTip));
                    SetToolTip("tbPrev", LangMan.LS(LSID.PrevRec));
                    SetToolTip("tbNext", LangMan.LS(LSID.NextRec));

                    GetControl<IMenuItem>("miContRecordAdd").Text = LangMan.LS(LSID.MIRecordAdd);
                    GetControl<IMenuItem>("miContRecordEdit").Text = LangMan.LS(LSID.MIRecordEdit);
                    GetControl<IMenuItem>("miContRecordDelete").Text = LangMan.LS(LSID.MIRecordDelete);
                    GetControl<IMenuItem>("miContRecordDuplicate").Text = LangMan.LS(LSID.RecordDuplicate);
                    GetControl<IMenuItem>("miContRecordMerge").Text = LangMan.LS(LSID.MergeDuplicates);

                    GetControl<IMenuItem>("miCopyContent").Text = LangMan.LS(LSID.Copy);
                }
            } catch (Exception ex) {
                Logger.WriteError("PartialViewController.SetLocale()", ex);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            // toolbar
            GetControl<IToolItem>("tbFileSave").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileSave, true);
            GetControl<IToolItem>("tbRecordAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordAdd, true);
            GetControl<IToolItem>("tbRecordEdit").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordEdit, true);
            GetControl<IToolItem>("tbRecordDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_RecordDelete, true);
            GetControl<IToolItem>("tbFilter").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Filter, true);
            GetControl<IToolItem>("tbTreeAncestors").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeAncestors, true);
            GetControl<IToolItem>("tbTreeDescendants").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeDescendants, true);
            GetControl<IToolItem>("tbTreeBoth").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_TreeBoth, true);
            GetControl<IToolItem>("tbPrev").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev, true);
            GetControl<IToolItem>("tbNext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next, true);
        }

        #region Dialogs

        private async void ShowCommonFilter(IRecordsListModel listMan)
        {
            using (var dlg = AppHost.Container.Resolve<ICommonFilterDlg>(fBase, listMan)) {
                if (await AppHost.Instance.ShowModalAsync(dlg, fView, false)) {
                    AppHost.Instance.NotifyFilter(fBase, fRecordType, listMan, listMan.Filter);
                    ApplyFilter(fRecordType);
                }
            }
        }

        private async void ShowPersonsFilter(IRecordsListModel listMan)
        {
            using (var dlg = AppHost.Container.Resolve<IPersonsFilterDlg>(fBase, listMan)) {
                if (await AppHost.Instance.ShowModalAsync(dlg, fView, false)) {
                    ApplyFilter(fRecordType, listMan);
                }
            }
        }

        private GDMIndividualRecord GetSelectedPersonVar()
        {
            var selRec = GetSelectedRecordEx();

            if (selRec is GDMFamilyRecord) {
                var famRec = (GDMFamilyRecord)selRec;
                var tree = fBase.Context.Tree;

                selRec = tree.GetPtrValue(famRec.Husband);
                if (selRec == null) {
                    selRec = tree.GetPtrValue(famRec.Wife);
                }
            }

            return selRec as GDMIndividualRecord;
        }

        public void ShowTreeChart(TreeChartKind chartKind)
        {
            BaseController.ShowTreeChart(fBase, GetSelectedPersonVar(), chartKind);
        }

        #endregion
    }
}
