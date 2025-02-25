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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKNavigatorPlugin
{
    public enum DataCategory
    {
        Root,
        RecentActivity,
        JumpHistory,
        PotencialProblems,
        Filters,
        Bookmarks,
        Records,
        Languages,
        Associations,
        WebLinks,
    }


    public sealed class RecordInfo
    {
        public readonly GDMRecordType Type;
        public readonly RecordAction Action;
        public readonly string XRef;
        public readonly string Name;
        public readonly GDMRecord Record; // null for deleted records
        public readonly DateTime Time;

        public RecordInfo(RecordAction action, string xref, string name, GDMRecordType type, GDMRecord record)
        {
            Action = action;
            XRef = xref;
            Name = name;
            Type = type;
            Record = record;
            Time = DateTime.Now;
        }
    }


    public sealed class FilterInfo
    {
        public readonly GDMRecordType RecType;
        public readonly IListSource ListSource;
        public readonly string FilterContent;
        public readonly string FilterView;

        public FilterInfo(GDMRecordType recType, IListSource listSource, IListFilter filter)
        {
            RecType = recType;
            ListSource = listSource;
            FilterContent = filter.Serialize();
            FilterView = filter.ToString(listSource);
        }
    }


    public sealed class BaseData
    {
        private readonly List<FilterInfo> fChangedFilters;
        private readonly List<RecordInfo> fChangedRecords;

        public List<FilterInfo> ChangedFilters
        {
            get { return fChangedFilters; }
        }

        public List<RecordInfo> ChangedRecords
        {
            get { return fChangedRecords; }
        }

        public BaseData()
        {
            fChangedFilters = new List<FilterInfo>();
            fChangedRecords = new List<RecordInfo>();
        }

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            GDMRecord gRecord = record as GDMRecord;
            if (baseWin == null || gRecord == null) return;

            try {
                string recName = GKUtils.GetRecordName(baseWin.Context.Tree, gRecord, false);

                int index = Find(gRecord);
                if (index >= 0) {
                    fChangedRecords.RemoveAt(index);
                }

                switch (action) {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit:
                        fChangedRecords.Add(new RecordInfo(action, gRecord.XRef, recName, gRecord.RecordType, gRecord));
                        break;

                    case RecordAction.raDelete:
                        fChangedRecords.Add(new RecordInfo(action, gRecord.XRef, recName, gRecord.RecordType, null));
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseData.NotifyRecord()", ex);
            }
        }

        public int Find(GDMRecord record)
        {
            for (int i = 0; i < fChangedRecords.Count; i++) {
                RecordInfo recInfo = fChangedRecords[i];
                if (recInfo.Record == record) {
                    return i;
                }
            }
            return -1;
        }

        public void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, IListFilter filter)
        {
            if (listSource == null || filter == null) return;

            fChangedFilters.Add(new FilterInfo(recType, listSource, filter));
        }
    }


    public sealed class NavigatorData
    {
        private readonly Dictionary<string, BaseData> fBases;
        private readonly Plugin fPlugin;

        public BaseData this[string name]
        {
            get {
                BaseData result;
                if (!fBases.TryGetValue(name, out result)) {
                    result = new BaseData();
                    fBases.Add(name, result);
                }
                return result;
            }
        }

        public NavigatorData(Plugin plugin)
        {
            fPlugin = plugin;
            fBases = new Dictionary<string, BaseData>();
        }

        public void CloseBase(string name)
        {
            BaseData b_data;
            if (!fBases.TryGetValue(name, out b_data)) {
                return; // base not exists
            }

            fBases.Remove(name);
        }

        public void RenameBase(string oldName, string newName)
        {
            BaseData b_data;
            if (!fBases.TryGetValue(oldName, out b_data)) {
                return; // base not exists
            }

            fBases.Remove(oldName);
            fBases.Add(newName, b_data);
        }

        public void ShowItem(IBaseWindow baseWin, object tag, IListView listView)
        {
            if (tag is GDMRecordType recType) {
                ShowData(baseWin, DataCategory.Records, recType, listView);
            } else if (tag is DataCategory dataCat) {
                ShowData(baseWin, dataCat, GDMRecordType.rtNone, listView);
            }
        }

        public void ShowData(IBaseWindow baseWin, DataCategory category, GDMRecordType recordType, IListView listView)
        {
            switch (category) {
                case DataCategory.RecentActivity:
                    listView.Clear();
                    break;

                case DataCategory.JumpHistory:
                    ShowJumpHistory(baseWin, listView);
                    break;

                case DataCategory.PotencialProblems:
                    listView.Clear();
                    break;

                case DataCategory.Filters:
                    ShowFilters(baseWin, listView);
                    break;

                case DataCategory.Bookmarks:
                    ShowBookmarks(baseWin, listView);
                    break;

                case DataCategory.Records:
                    ShowRecordsData(baseWin, recordType, listView);
                    break;

                case DataCategory.Languages:
                    ShowLanguages(baseWin, listView);
                    break;

                case DataCategory.Associations:
                    ShowAssociations(baseWin, listView);
                    break;

                case DataCategory.WebLinks:
                    //ShowWebLinks(baseWin);
                    break;
            }
        }

        public void SelectItem(IBaseWindow baseWin, object tag, object itemData)
        {
            if (tag == null) return;
            if (itemData == null) return;

            if (tag is GDMRecordType) {
                SelectRecordInfo(baseWin, (RecordInfo)itemData);
            } else if (tag is DataCategory) {
                var dataCat = (DataCategory)tag;

                switch (dataCat) {
                    case DataCategory.JumpHistory:
                        SelectRecord(baseWin, (GDMRecord)itemData);
                        break;

                    case DataCategory.Filters:
                        SelectFilter(baseWin, (FilterInfo)itemData);
                        break;

                    case DataCategory.Bookmarks:
                        SelectRecord(baseWin, (GDMRecord)itemData);
                        break;

                    case DataCategory.Languages:
                        SelectLanguage(baseWin, (GDMLanguageID)itemData);
                        break;

                    case DataCategory.Associations:
                        SelectRecord(baseWin, (GDMRecord)itemData);
                        break;
                }
            }
        }

        private void SelectRecord(IBaseWindow baseWin, GDMRecord iRec)
        {
            baseWin.SelectByRec(iRec);
        }

        #region Records Data

        private void ShowRecordsData(IBaseWindow baseWin, GDMRecordType recordType, IListView listView)
        {
            baseWin.ShowRecordsTab(recordType);

            listView.BeginUpdate();
            try {
                listView.Clear();
                listView.AddColumn(fPlugin.LangMan.LS(PLS.Action), 20, true);
                listView.AddColumn("XRef", 20, true);
                listView.AddColumn(fPlugin.LangMan.LS(PLS.Name), 20, true);
                listView.AddColumn(fPlugin.LangMan.LS(PLS.Time), 20, true);

                BaseData baseData = fPlugin.Data[baseWin.Context.FileName];
                if (baseData == null) return;

                foreach (var recordInfo in baseData.ChangedRecords) {
                    if (recordInfo.Type != recordType) continue;

                    string act = "";
                    switch (recordInfo.Action) {
                        case RecordAction.raAdd:
                            act = "+";
                            break;
                        case RecordAction.raEdit:
                            act = "*";
                            break;
                        case RecordAction.raDelete:
                            act = "-";
                            break;
                    }

                    listView.AddItem(recordInfo, new object[] { act, recordInfo.XRef, recordInfo.Name, recordInfo.Time.ToString() });
                }

                listView.ResizeColumns();
            } finally {
                listView.EndUpdate();
            }
        }

        private void SelectRecordInfo(IBaseWindow baseWin, RecordInfo recInfo)
        {
            if (recInfo.Action != RecordAction.raDelete)
                SelectRecord(baseWin, recInfo.Record);
        }

        #endregion

        #region JumpHistory

        private void ShowJumpHistory(IBaseWindow baseWin, IListView listView)
        {
            var tree = baseWin.Context.Tree;
            var navArray = baseWin.Navman.FullArray;

            listView.BeginUpdate();
            try {
                listView.Clear();
                listView.AddColumn(LangMan.LS(LSID.Record), 400, true);

                foreach (var rec in navArray) {
                    listView.AddItem(rec, new object[] { GKUtils.GetRecordName(tree, rec, true) });
                }

                listView.ResizeColumn(0);
            } finally {
                listView.EndUpdate();
            }
        }

        #endregion

        #region Filters

        private void ShowFilters(IBaseWindow baseWin, IListView listView)
        {
            baseWin.ShowRecordsTab(GDMRecordType.rtIndividual);

            listView.BeginUpdate();
            try {
                listView.Clear();
                listView.AddColumn(LangMan.LS(LSID.MIFilter), 400, true);

                BaseData baseData = fPlugin.Data[baseWin.Context.FileName];
                if (baseData == null) return;

                foreach (var filterInfo in baseData.ChangedFilters) {
                    listView.AddItem(filterInfo, new object[] { filterInfo.FilterView });
                }

                listView.ResizeColumn(0);
            } finally {
                listView.EndUpdate();
            }
        }

        private void SelectFilter(IBaseWindow baseWin, FilterInfo filterInfo)
        {
            try {
                baseWin.ShowRecordsTab(filterInfo.RecType);
                filterInfo.ListSource.Filter.Deserialize(filterInfo.FilterContent);
                baseWin.ApplyFilter(filterInfo.RecType);
            } catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }

        #endregion

        #region Bookmarks

        private void ShowBookmarks(IBaseWindow baseWin, IListView listView)
        {
            baseWin.ShowRecordsTab(GDMRecordType.rtIndividual);

            var bookmarks = fPlugin.Data.SearchBookmarks(baseWin.Context);

            listView.BeginUpdate();
            try {
                listView.Clear();
                listView.AddColumn(LangMan.LS(LSID.Person), 400, true);

                foreach (var iRec in bookmarks) {
                    listView.AddItem(iRec, new object[] { GKUtils.GetNameString(iRec, false) });
                }

                listView.ResizeColumn(0);
            } finally {
                listView.EndUpdate();
            }
        }

        public IList<GDMIndividualRecord> SearchBookmarks(IBaseContext baseContext)
        {
            var result = new List<GDMIndividualRecord>();

            AppHost.Instance.ExecuteWork((controller) => {
                var tree = baseContext.Tree;
                int num = tree.RecordsCount;

                controller.Begin("PatSearch", num);

                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];

                    if (rec.RecordType == GDMRecordType.rtIndividual) {
                        GDMIndividualRecord iRec = rec as GDMIndividualRecord;

                        if (iRec.Bookmark) {
                            result.Add(iRec);
                        }
                    }

                    controller.Increment();
                }

                controller.End();
            });

            return result;
        }

        #endregion

        #region Languages

        private void ShowLanguages(IBaseWindow baseWin, IListView listView)
        {
            listView.BeginUpdate();
            try {
                listView.Clear();
                listView.AddColumn(LangMan.LS(LSID.Language), 200, true);

                var langsList = baseWin.Context.LangStats.ToList();
                foreach (var lang in langsList) {
                    listView.AddItem(lang, new object[] { GEDCOMUtils.GetLanguageStr(lang) });
                }

                listView.ResizeColumn(0);
            } finally {
                listView.EndUpdate();
            }
        }

        public void SelectLanguage(IBaseWindow baseWin, GDMLanguageID lang)
        {
            baseWin.Context.DefaultLanguage = lang;
            baseWin.ShowRecordsTab(GDMRecordType.rtIndividual);
            baseWin.RefreshRecordsView(GDMRecordType.rtIndividual);
        }

        #endregion

        #region Associations

        private void ShowAssociations(IBaseWindow baseWin, IListView listView)
        {
            listView.BeginUpdate();
            try {
                listView.Clear();
                listView.AddColumn(LangMan.LS(LSID.Person), 400, false);
                listView.AddColumn(LangMan.LS(LSID.Relation), 400, false);

                var tree = baseWin.Context.Tree;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    var rec = tree[i];
                    if (rec.RecordType == GDMRecordType.rtIndividual) {
                        var iRec = (GDMIndividualRecord)rec;
                        if (iRec.HasAssociations) {
                            string pnm = GKUtils.GetNameString(iRec, false);
                            int num2 = iRec.Associations.Count;
                            for (int k = 0; k < num2; k++) {
                                GDMAssociation ast = iRec.Associations[k];
                                var relIndi = tree.GetPtrValue(ast);
                                string rnm = ((relIndi == null) ? string.Empty : GKUtils.GetNameString(relIndi, false));
                                listView.AddItem(iRec, new object[] { pnm, ast.Relation + " " + rnm });
                            }
                        }
                    }
                }

                listView.ResizeColumn(0);
            } finally {
                listView.EndUpdate();
            }
        }

        #endregion
    }
}
