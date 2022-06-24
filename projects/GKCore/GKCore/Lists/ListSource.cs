﻿/*
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
using System.Text.RegularExpressions;
using BSLib;
using BSLib.Calendar;
using BSLib.Design.Graphics;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    ///
    /// </summary>
    public abstract class ListSource<T> : IListSource<T>
        where T : GDMTag
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


        private bool fColumnsHaveBeenChanged;
        private readonly ExtObservableList<ContentItem> fContentList;
        private int fTotalCount;
        private string fQuickFilter = "*";
        private int fXSortFactor;

        private string fMask;
        private Regex fRegexMask;
        private string fSimpleMask;

        protected ExternalFilterHandler fExternalFilter;
        protected T fFetchedRec;
        protected ListFilter fFilter;

        protected readonly IBaseContext fBaseContext;
        protected readonly List<MapColumnRec> fColumnsMap;
        protected readonly ListColumns<T> fListColumns;


        public IBaseContext BaseContext
        {
            get { return fBaseContext; }
        }

        public bool ColumnsHaveBeenChanged
        {
            get { return fColumnsHaveBeenChanged; }
            set { fColumnsHaveBeenChanged = value; }
        }

        public ExtObservableList<ContentItem> ContentList
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

        public IListColumns ListColumns
        {
            get { return fListColumns; }
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


        protected ListSource(IBaseContext baseContext, ListColumns<T> defaultListColumns)
        {
            fBaseContext = baseContext;
            fColumnsMap = new List<MapColumnRec>();
            fListColumns = defaultListColumns;
            fContentList = new ExtObservableList<ContentItem>();

            CreateFilter();
        }

        #region Columns

        protected void AddColumn(IListViewEx list, string caption, int width, bool autoSize, byte colType, byte colSubtype)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            list.AddColumn(caption, width, autoSize);
            fColumnsMap.Add(new MapColumnRec(colType, colSubtype));
        }

        public virtual void UpdateColumns(IListViewEx listView)
        {
            if (listView == null) return;

            fColumnsMap.Clear();

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

        public virtual void ChangeColumnWidth(int colIndex, int colWidth)
        {
            if (colIndex < 0)
                return;

            MapColumnRec colrec = fColumnsMap[colIndex];
            ListColumn props = FindColumnProps(colrec.ColType);

            if (props != null) {
                props.CurWidth = colWidth;
            }
        }

        protected ListColumn FindColumnProps(int colType)
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

        public bool IsColumnAutosize(int colIndex)
        {
            if (colIndex < 0)
                return false;

            MapColumnRec colrec = fColumnsMap[colIndex];
            ListColumn props = FindColumnProps(colrec.ColType);

            if (props != null) {
                return props.Autosize;
            } else {
                return false;
            }
        }

        #endregion

        #region Mask processing

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

                if (res)
                    return true;
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

        #endregion

        #region Filters

        public virtual void PrepareFilter()
        {
        }

        public virtual bool CheckFilter()
        {
            return true;
        }

        protected virtual void CreateFilter()
        {
            fFilter = new ListFilter();
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
                if (dataval == null)
                    return true;

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
                Logger.WriteError("ListSource.CheckCondition()", ex);
                res = true;
            }

            return res;
        }

        protected bool CheckCommonFilter()
        {
            bool res = true;

            try {
                var conditions = fFilter.Conditions;
                for (int i = 0, num = conditions.Count; i < num; i++) {
                    FilterCondition fcond = conditions[i];
                    res = res && CheckCondition(fcond);
                    if (!res) break;
                }
            } catch (Exception ex) {
                Logger.WriteError("ListSource.CheckCommonFilter()", ex);
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

        public string[] CreateFields()
        {
            ListColumns<T> listColumns = (ListColumns<T>)ListColumns;
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

        #endregion

        #region Cell values

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

        protected static string ConvertColumnValue(object val, ListColumn cs)
        {
            if (val == null)
                return string.Empty;

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

        protected static object ConvertColumnStr(string val, DataType type)
        {
            switch (type) {
                case DataType.dtString:
                    return val;

                case DataType.dtInteger:
                    return ConvertHelper.ParseLong(val, 0);

                case DataType.dtFloat:
                    return ConvertHelper.ParseFloat(val, 0.0);

                case DataType.dtDateTime:
                    return DateTime.Parse(val);

                case DataType.dtGEDCOMDate:
                    return GDMDate.GetUDNByFormattedStr(val, GDMCalendar.dcGregorian);
            }

            return val;
        }

        public object GetColumnValue(int colIndex, bool isVisible)
        {
            MapColumnRec colrec = fColumnsMap[colIndex];
            object val = GetColumnValueEx(colrec.ColType, colrec.ColSubtype, isVisible);

            if (isVisible) {
                ListColumn cs = fListColumns[colrec.ColType];
                return ConvertColumnValue(val, cs);
            } else {
                return val;
            }
        }

        /// <summary>
        /// Getting the value of a column cell.
        /// </summary>
        /// <param name="colType">Basic column type.</param>
        /// <param name="colSubtype">Column subtype.</param>
        /// <param name="isVisible">Value target sign - for display or sorting functions.</param>
        /// <returns></returns>
        protected virtual object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            return null;
        }

        #endregion

        #region Items and content processing

        private object[] fItemData;
        private T fVirtualFetchedRecord;

        public string GetColumnExternalValue(ContentItem contentItem, int colIndex)
        {
            var queryRec = (T)contentItem.Record;

            // "super fetch"
            if (fVirtualFetchedRecord != queryRec) {
                fItemData = GetItemData(queryRec);
                fVirtualFetchedRecord = queryRec;
            }

            return (fItemData == null) ? string.Empty : (string)fItemData[colIndex];
        }

        protected object[] GetItemData(object rowData)
        {
            T rec = rowData as T;
            if (rec == null)
                return null;

            Fetch(rec);

            int num = fColumnsMap.Count;
            object[] result = new object[num];
            for (int i = 0; i < num; i++) {
                result[i] = GetColumnValue(i, true);
            }

            return result;
        }

        public virtual IListItem CreateListItem(int itemIndex, object rowData, CreateListItemHandler handler)
        {
            if (rowData == null || handler == null)
                return null;

            object[] columnValues = GetItemData(rowData);

            IListItem item = handler(rowData, columnValues);

            var backColor = GetBackgroundColor(itemIndex, fFetchedRec);
            if (backColor != null) {
                item.SetBackColor(backColor);
            }

            return item;
        }

        public virtual IColor GetBackgroundColor(int itemIndex, object rowData)
        {
            return null;
        }

        public object GetContentItem(int itemIndex)
        {
            object result;
            if (itemIndex < 0 || itemIndex >= fContentList.Count) {
                result = null;
            } else {
                result = fContentList[itemIndex].Record;
            }
            return result;
        }

        protected void InitContent(int contentSize)
        {
            fTotalCount = 0;

            fContentList.BeginUpdate();
            fContentList.Clear();
            fContentList.Capacity = contentSize;

            PrepareFilter();
        }

        protected void DoneContent()
        {
            fContentList.EndUpdate();
        }

        protected void AddFilteredContent(T rec)
        {
            fTotalCount++;

            Fetch(rec);

            if (CheckFilter()) {
                fContentList.Add(new ContentItem(rec));
            }
        }

        public abstract void UpdateContents();

        public virtual void Fetch(T aRec)
        {
            fFetchedRec = aRec;
        }

        public int IndexOfItem(object data)
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

        public bool DeleteItem(object data)
        {
            int idx = IndexOfItem(data);
            if (idx >= 0) {
                fContentList.RemoveAt(idx);
                return true;
            }
            return false;
        }

        #endregion

        #region Sort support

        private int CompareItems(ContentItem item1, ContentItem item2)
        {
            int compRes;
            object cv1 = item1.SortValue;
            object cv2 = item2.SortValue;

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
                fContentList.BeginUpdate();

                int num = fContentList.Count;
                for (int i = 0; i < num; i++) {
                    ContentItem valItem = fContentList[i];
                    Fetch((T)valItem.Record);
                    valItem.SortValue = GetColumnValue(sortColumn, false);
                }

                fXSortFactor = (sortAscending ? 1 : -1);
                ListTimSort<ContentItem>.Sort(fContentList, CompareItems);

                fContentList.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("ListSource.SortContents()", ex);
            }
        }

        #endregion
    }
}
