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

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using BSLib;
using BSLib.Calendar;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Search;
using GKCore.Types;

using BSDColors = BSLib.Design.BSDConsts.Colors;

namespace GKCore.Lists
{
    /// <summary>
    ///
    /// </summary>
    public abstract class ListSource : IListSource
    {
        protected sealed class MapColumnRec
        {
            public byte ColType;
            public byte ColSubtype;

            public MapColumnRec(byte colType, byte colSubtype)
            {
                ColType = colType;
                ColSubtype = colSubtype;
            }
        }

        private EnumSet<RecordAction> fAllowedActions;
        private bool fColumnsHaveBeenChanged;

        protected readonly IBaseContext fBaseContext;
        protected readonly List<MapColumnRec> fColumnsMap;
        protected readonly ListColumns fListColumns;


        public EnumSet<RecordAction> AllowedActions
        {
            get { return fAllowedActions; }
            set { fAllowedActions = value; }
        }

        public IBaseContext BaseContext
        {
            get { return fBaseContext; }
        }

        public bool ColumnsHaveBeenChanged
        {
            get { return fColumnsHaveBeenChanged; }
            set { fColumnsHaveBeenChanged = value; }
        }

        public IListColumns ListColumns
        {
            get { return fListColumns; }
        }


        protected ListSource(IBaseContext baseContext, ListColumns defaultListColumns)
        {
            fAllowedActions = new EnumSet<RecordAction>();
            fBaseContext = baseContext;
            fColumnsMap = new List<MapColumnRec>();
            fListColumns = defaultListColumns;
        }

        protected void AddColumn(IListViewEx list, string caption, int width, bool autoSize, byte colType, byte colSubtype)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            list.AddColumn(caption, width, autoSize);
            fColumnsMap.Add(new MapColumnRec(colType, colSubtype));
        }

        protected void ColumnsMap_Clear()
        {
            fColumnsMap.Clear();
        }

        public abstract void UpdateColumns(IListViewEx listView);

        public abstract void UpdateContents();
    }

    /// <summary>
    ///
    /// </summary>
    public abstract class ListManager : ListSource, IListManager
    {
        public sealed class ValItem
        {
            public readonly GDMRecord Record;
            public object ColumnValue;

            public ValItem(GDMRecord record)
            {
                Record = record;
                ColumnValue = null;
            }
        }

        protected ListFilter fFilter;
        protected ExternalFilterHandler fExternalFilter;

        private readonly List<ValItem> fContentList;
        private readonly GDMRecordType fRecordType;
        private int fXSortFactor;
        private int fTotalCount;
        private string fQuickFilter = "*";

        private string fMask;
        private Regex fRegexMask;
        private string fSimpleMask;


        public List<ValItem> ContentList
        {
            get { return fContentList; }
        }

        public ExternalFilterHandler ExternalFilter
        {
            get { return fExternalFilter; }
            set { fExternalFilter = value; }
        }

        public IListFilter Filter
        {
            get { return fFilter; }
        }

        public int FilteredCount
        {
            get { return fContentList.Count; }
        }

        public GDMRecordType RecordType
        {
            get { return fRecordType; }
        }

        public int TotalCount
        {
            get { return fTotalCount; }
        }

        public string QuickFilter
        {
            get { return fQuickFilter; }
            set { fQuickFilter = value; }
        }


        protected ListManager(IBaseContext baseContext, ListColumns defaultListColumns, GDMRecordType recordType) :
            base(baseContext, defaultListColumns)
        {
            fContentList = new List<ValItem>();
            fRecordType = recordType;

            CreateFilter();
        }

        protected virtual void CreateFilter()
        {
            fFilter = new ListFilter();
        }

        protected bool IsMatchesMask(string str, string mask)
        {
            if (string.IsNullOrEmpty(mask) || mask == "*") {
                return true;
            }

            if (string.IsNullOrEmpty(str)) {
                return false;
            }

            if (fMask != mask) {
                fMask = mask;
                fSimpleMask = GetSimpleMask(fMask);
                if (fSimpleMask == null) {
                    fRegexMask = new Regex(GKUtils.PrepareMask(fMask), GKUtils.RegexOpts);
                }
            }

            if (fSimpleMask != null) {
                return str.IndexOf(fSimpleMask, StringComparison.OrdinalIgnoreCase) >= 0;
            } else {
                return fRegexMask.IsMatch(str, 0);
            }
        }

        protected bool IsMatchesMask(GDMLines strList, string mask)
        {
            if (strList == null || strList.IsEmpty() || string.IsNullOrEmpty(mask)) {
                return false;
            }

            if (mask == "*") {
                return true;
            }

            if (fMask != mask) {
                fMask = mask;
                fSimpleMask = GetSimpleMask(fMask);
                if (fSimpleMask == null) {
                    fRegexMask = new Regex(GKUtils.PrepareMask(fMask), GKUtils.RegexOpts);
                }
            }

            for (int i = 0; i < strList.Count; i++) {
                string str = strList[i];

                bool res;
                if (fSimpleMask != null) {
                    res = str.IndexOf(fSimpleMask, StringComparison.OrdinalIgnoreCase) >= 0;
                } else {
                    res = fRegexMask.IsMatch(str, 0);
                }

                if (res) return true;
            }

            return false;
        }

        private static readonly char[] SpecialChars = new char[] { '*', '?', '|' };

        private static string GetSimpleMask(string mask)
        {
            int len = mask.Length;
            if (len > 2 && mask[0] == '*' && mask[len - 1] == '*') {
                string subStr = mask.Substring(1, len - 2);
                return (subStr.IndexOfAny(SpecialChars) >= 0) ? null : subStr;
            } else {
                return null;
            }
        }

        public virtual bool CheckFilter()
        {
            return true;
        }

        public abstract void Fetch(GDMRecord aRec);

        protected static object GetDateValue(GDMCustomEvent evt, bool isVisible)
        {
            if (evt == null) {
                return (isVisible) ? null : (object)UDN.CreateEmpty();
            }

            return GetDateValue(evt.Date.Value, isVisible);
        }

        protected static object GetDateValue(GDMCustomDate date, bool isVisible)
        {
            object result;

            if (date == null) {
                result = (isVisible) ? null : (object)UDN.CreateEmpty();
            } else {
                if (isVisible) {
                    GlobalOptions glob = GlobalOptions.Instance;
                    result = date.GetDisplayStringExt(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
                } else {
                    result = date.GetUDN();
                }
            }

            return result;
        }

        public object GetColumnInternalValue(int colIndex)
        {
            // col_index - from 1
            MapColumnRec colrec = fColumnsMap[colIndex];
            return GetColumnValueEx(colrec.ColType, colrec.ColSubtype, false);
        }

        protected virtual object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            return null;
        }

        public virtual void PrepareFilter()
        {
        }

        public virtual object[] GetItemData(object rowData)
        {
            GDMRecord rec = rowData as GDMRecord;
            if (rec == null) return null;

            Fetch(rec);

            object[] result = new object[fColumnsMap.Count];
            result[0] = rec.GetXRefNum();

            int num = fColumnsMap.Count;
            for (int i = 1; i < num; i++)
            {
                MapColumnRec colrec = fColumnsMap[i];

                // aColIndex - from 1
                ListColumn cs = fListColumns[colrec.ColType];
                object val = GetColumnValueEx(colrec.ColType, colrec.ColSubtype, true);
                string res = ConvertColumnValue(val, cs);
                result[i] = res;
            }

            return result;
        }

        public virtual void UpdateItemProps(IListItem item, object rowData)
        {
        }

        public virtual void UpdateItem(int itemIndex, IListItem item, object rowData)
        {
            GDMRecord rec = rowData as GDMRecord;
            if (item == null || rec == null) return;

            Fetch(rec);

            int num = fColumnsMap.Count;
            for (int i = 1; i < num; i++)
            {
                MapColumnRec colrec = fColumnsMap[i];

                // aColIndex - from 1
                ListColumn cs = fListColumns[colrec.ColType];
                object val = GetColumnValueEx(colrec.ColType, colrec.ColSubtype, true);
                string res = ConvertColumnValue(val, cs);

                item.AddSubItem(res);
            }

            if (GlobalOptions.Instance.ReadabilityHighlightRows && MathHelper.IsOdd(itemIndex)) {
                item.SetBackColor(ChartRenderer.GetColor(BSDColors.LightGray));
            }
        }

        public override void UpdateColumns(IListViewEx listView)
        {
            if (listView == null) return;

            ColumnsMap_Clear();
            AddColumn(listView, "№", 50, false, 0, 0);

            int num = fListColumns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn cs = fListColumns.OrderedColumns[i];

                AddColumn(listView, LangMan.LS(cs.ColName), cs.CurWidth, false, cs.Id, 0);
            }

            ColumnsHaveBeenChanged = false;
        }

        public string GetColumnName(int columnId)
        {
            if (columnId >= 0 && columnId < fListColumns.Count) {
                return LangMan.LS(fListColumns[columnId].ColName);
            }

            return "<?>";
        }

        public DataType GetColumnDataType(int columnId)
        {
            if (columnId >= 0 && columnId < fListColumns.Count) {
                return fListColumns[columnId].DataType;
            }

            return DataType.dtString;
        }

        // used only in UpdateItem
        private static string ConvertColumnValue(object val, ListColumn cs)
        {
            if (val == null) return string.Empty;

            switch (cs.DataType) {
                case DataType.dtString:
                    return val.ToString();

                case DataType.dtInteger:
                    return val.ToString();

                case DataType.dtFloat:
                    return ((double)val).ToString(cs.Format, cs.NumFmt);

                case DataType.dtDateTime:
                    DateTime dtx = ((DateTime)val);
                    return ((dtx.Ticks == 0) ? "" : dtx.ToString("yyyy.MM.dd HH:mm:ss", null));

                case DataType.dtGEDCOMDate:
                    return val.ToString();

                default:
                    return val.ToString();
            }
        }

        private static object ConvertColumnStr(string val, DataType type)
        {
            switch (type) {
                case DataType.dtString:
                    return val;

                case DataType.dtInteger:
                    return ConvertHelper.ParseInt(val, 0);

                case DataType.dtFloat:
                    return ConvertHelper.ParseFloat(val, 0.0);

                case DataType.dtDateTime:
                    return DateTime.Parse(val);

                case DataType.dtGEDCOMDate:
                    return GDMDate.GetUDNByFormattedStr(val, GDMCalendar.dcGregorian);
            }

            return val;
        }

        public void AddCondition(byte columnId, ConditionKind condition, string value)
        {
            object condValue = ConvertColumnStr(value, GetColumnDataType(columnId));

            FilterCondition fltCond = new FilterCondition(columnId, condition, condValue);
            fFilter.Conditions.Add(fltCond);
        }

        private bool CheckCondition(FilterCondition fcond)
        {
            bool res = true;

            try {
                object dataval = GetColumnValueEx(fcond.ColumnIndex, -1, false);
                if (dataval == null) return true;

                int compRes = 0;
                if (fcond.Condition != ConditionKind.ck_Contains) {
                    compRes = ((IComparable)dataval).CompareTo(fcond.Value);
                }

                switch (fcond.Condition) {
                    case ConditionKind.ck_NotEq:
                        res = compRes != 0;
                        break;

                    case ConditionKind.ck_LT:
                        res = compRes < 0;
                        break;

                    case ConditionKind.ck_LET:
                        res = compRes <= 0;
                        break;

                    case ConditionKind.ck_Eq:
                        res = compRes == 0;
                        break;

                    case ConditionKind.ck_GET:
                        res = compRes >= 0;
                        break;

                    case ConditionKind.ck_GT:
                        res = compRes > 0;
                        break;

                    case ConditionKind.ck_Contains:
                        res = GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value + "*");
                        break;

                    case ConditionKind.ck_NotContains:
                        res = !GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value + "*");
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("ListManager.CheckCondition()", ex);
                res = true;
            }

            return res;
        }

        protected bool CheckCommonFilter()
        {
            bool res = true;

            try {
                int num = Filter.Conditions.Count;
                for (int i = 0; i < num; i++) {
                    FilterCondition fcond = Filter.Conditions[i];
                    res = res && CheckCondition(fcond);
                }
            } catch (Exception ex) {
                Logger.WriteError("ListManager.CheckCommonFilter()", ex);
                res = true;
            }

            return res;
        }

        protected bool CheckExternalFilter(GDMRecord rec)
        {
            bool res = true;
            if (fExternalFilter != null) {
                res = res && fExternalFilter(rec);
            }
            return res;
        }

        private ListColumn FindColumnProps(int colType)
        {
            int num = fListColumns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn props = fListColumns[i];

                if (props.Id == colType) {
                    return props;
                }
            }

            return null;
        }

        public void ChangeColumnWidth(int colIndex, int colWidth)
        {
            if (colIndex <= 0) return;

            MapColumnRec colrec = fColumnsMap[colIndex];
            ListColumn props = FindColumnProps(colrec.ColType);

            if (props != null) {
                props.CurWidth = colWidth;
            }
        }

        public bool IsColumnAutosize(int colIndex)
        {
            if (colIndex <= 0) return false;

            MapColumnRec colrec = fColumnsMap[colIndex];
            ListColumn props = FindColumnProps(colrec.ColType);

            if (props != null) {
                return props.Autosize;
            } else {
                return false;
            }
        }

        private int CompareItems(ValItem item1, ValItem item2)
        {
            int compRes;
            object cv1 = item1.ColumnValue;
            object cv2 = item2.ColumnValue;

            if (cv1 != null && cv2 != null) {
                compRes = ((IComparable)cv1).CompareTo(cv2);
            } else if (cv1 != null) {
                compRes = -1;
            } else if (cv2 != null) {
                compRes = 1;
            } else {
                compRes = 0;
            }

            return compRes * fXSortFactor;
        }

        public void SortContents(int sortColumn, bool sortAscending)
        {
            try {
                int num = fContentList.Count;
                for (int i = 0; i < num; i++) {
                    ValItem valItem = fContentList[i];
                    GDMRecord rec = valItem.Record;

                    if (sortColumn == 0) {
                        valItem.ColumnValue = rec.GetId();
                    } else {
                        Fetch(rec);
                        valItem.ColumnValue = GetColumnInternalValue(sortColumn);
                    }
                }

                fXSortFactor = (sortAscending ? 1 : -1);
                ListTimSort<ValItem>.Sort(fContentList, CompareItems);
            } catch (Exception ex) {
                Logger.WriteError("ListManager.SortContents()", ex);
            }
        }

        public override void UpdateContents()
        {
            fTotalCount = 0;

            PrepareFilter();

            int contentSize = fBaseContext.Tree.RecordsCount;

            fContentList.Clear();
            fContentList.Capacity = contentSize;

            for (int i = 0; i < contentSize; i++) {
                GDMRecord rec = fBaseContext.Tree[i];

                if (rec.RecordType == fRecordType) {
                    fTotalCount++;

                    Fetch(rec);
                    if (CheckFilter()) {
                        fContentList.Add(new ValItem(rec));
                    } else {
                        // filter's debug
                    }
                }
            }
        }

        public List<GDMRecord> GetRecordsList()
        {
            int size = fContentList.Count;
            var result = new List<GDMRecord>(size);

            for (int i = 0; i < size; i++) {
                result.Add(fContentList[i].Record);
            }

            return result;
        }

        public IListItem CreateListItem(object rowData, CreateListItemHandler handler)
        {
            GDMRecord record = rowData as GDMRecord;
            if (record == null || handler == null) return null;

            return handler(record.GetXRefNum(), record);
        }

        public GDMRecord GetContentItem(int itemIndex)
        {
            GDMRecord result;
            if (itemIndex < 0 || itemIndex >= fContentList.Count) {
                result = null;
            } else {
                result = fContentList[itemIndex].Record;
            }
            return result;
        }

        public int IndexOfRecord(object data)
        {
            int result = -1;

            int num = fContentList.Count;
            for (int i = 0; i < num; i++) {
                if (fContentList[i].Record == data) {
                    result = i;
                    break;
                }
            }

            return result;
        }

        public bool DeleteRecord(object data)
        {
            int idx = IndexOfRecord(data);
            if (idx >= 0) {
                fContentList.RemoveAt(idx);
                return true;
            }
            return false;
        }

        public static ListManager Create(IBaseContext baseContext, GDMRecordType recType)
        {
            ListManager result = null;

            switch (recType) {
                case GDMRecordType.rtIndividual:
                    result = new IndividualListMan(baseContext);
                    break;

                case GDMRecordType.rtFamily:
                    result = new FamilyListMan(baseContext);
                    break;

                case GDMRecordType.rtNote:
                    result = new NoteListMan(baseContext);
                    break;

                case GDMRecordType.rtMultimedia:
                    result = new MultimediaListMan(baseContext);
                    break;

                case GDMRecordType.rtSource:
                    result = new SourceListMan(baseContext);
                    break;

                case GDMRecordType.rtRepository:
                    result = new RepositoryListMan(baseContext);
                    break;

                case GDMRecordType.rtGroup:
                    result = new GroupListMan(baseContext);
                    break;

                case GDMRecordType.rtResearch:
                    result = new ResearchListMan(baseContext);
                    break;

                case GDMRecordType.rtTask:
                    result = new TaskListMan(baseContext);
                    break;

                case GDMRecordType.rtCommunication:
                    result = new CommunicationListMan(baseContext);
                    break;

                case GDMRecordType.rtLocation:
                    result = new LocationListMan(baseContext);
                    break;

                case GDMRecordType.rtSubmission:
                    result = null;
                    break;

                case GDMRecordType.rtSubmitter:
                    result = null;
                    break;
            }

            return result;
        }

        public string[] CreateFields()
        {
            ListColumns listColumns = (ListColumns)ListColumns;
            string[] fields = new string[listColumns.Count + 1]; // +empty item
            fields[0] = "";

            for (int idx = 0; idx < listColumns.Count; idx++) {
                var cs = listColumns[idx];
                fields[idx + 1] = GetColumnName(cs.Id);
            }

            return fields;
        }

        public ConditionKind GetCondByName(string condName)
        {
            ConditionKind res = ConditionKind.ck_NotEq;

            for (ConditionKind pl = ConditionKind.ck_NotEq; pl <= ConditionKind.ck_NotContains; pl++) {
                if (GKData.CondSigns[(int)pl] == condName) {
                    res = pl;
                    break;
                }
            }

            return res;
        }

        public int GetFieldColumnId(string[] fields, string fieldName)
        {
            int idx = -1;
            for (int i = 0; i < fields.Length; i++) {
                if (fields[i] == fieldName) {
                    idx = i - 1; // exclude empty item
                    break;
                }
            }

            return idx;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            List<ISearchResult> result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = fContentList.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fContentList[i].Record;

                string recName = GKUtils.GetRecordName(fBaseContext.Tree, rec, false);
                if (GKUtils.MatchesRegex(recName, regex)) {
                    result.Add(new SearchResult(rec));
                }
            }

            return result;
        }
    }
}
