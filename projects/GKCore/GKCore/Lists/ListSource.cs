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
using GKCore.Calendar;
using GKCore.Charts;
using GKCore.Cultures;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Filters;
using GKCore.Options;
using GKCore.Utilities;
using SGCulture = System.Globalization.CultureInfo;

namespace GKCore.Lists
{
    /// <summary>
    ///
    /// </summary>
    public abstract class ListSource<T> : IListSource
        where T : class
    {
        public const string ErrorValue = "#";


        private readonly ExtObservableList<ContentItem> fContentList;
        private SGCulture fSysCulture;
        private int fTotalCount;
        private int fXSortFactor;

        private string fMask;
        private Regex fRegexMask;
        private string fSimpleMask;

        protected ExternalFilterHandler fExternalFilter;
        protected T fFetchedRec;
        protected ListFilter fFilter;
        protected MatchPatternMethod fFilterMethod;

        protected readonly BaseContext fBaseContext;
        protected readonly List<MapColumnRec> fColumnsMap;
        protected readonly ListColumns fListColumns;

        private int fSortColumn;
        private GKSortOrder fSortOrder;


        public List<MapColumnRec> ColumnsMap
        {
            get { return fColumnsMap; }
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

        public ListFilter Filter
        {
            get { return fFilter; }
        }

        public int FilteredCount
        {
            get { return fContentList.Count; }
        }

        public ListColumns ListColumns
        {
            get { return fListColumns; }
        }

        public int SortColumn
        {
            get { return fSortColumn; }
            set { fSortColumn = value; }
        }

        public GKSortOrder SortOrder
        {
            get { return fSortOrder; }
            set { fSortOrder = value; }
        }

        public int TotalCount
        {
            get { return fTotalCount; }
        }


        protected ListSource()
        {
            fBaseContext = null;
            fColumnsMap = new List<MapColumnRec>();
            fContentList = new ExtObservableList<ContentItem>();
            fListColumns = new ListColumns(GKListType.ltNone);

            fSortColumn = 0;
            fSortOrder = GKSortOrder.Ascending;

            CreateFilter();
        }

        protected ListSource(BaseContext baseContext, ListColumns defaultListColumns)
        {
            fBaseContext = baseContext;
            fColumnsMap = new List<MapColumnRec>();
            fContentList = new ExtObservableList<ContentItem>();

            if (defaultListColumns != null) {
                fListColumns = defaultListColumns;
                fListColumns.ResetDefaults();

                RestoreSettings();
            } else {
                fListColumns = new ListColumns(GKListType.ltNone);
            }

            fSortColumn = 0;
            fSortOrder = GKSortOrder.Ascending;

            CreateFilter();
        }

        public virtual void Clear()
        {
            fContentList.Clear();
            fTotalCount = 0;

            fMask = string.Empty;
            fRegexMask = null;
            fSimpleMask = string.Empty;
            fExternalFilter = null;
            fFilter.Clear();

            fFetchedRec = default;

            fColumnsMap.Clear();
            fListColumns.Clear();
        }

        public void RestoreSettings()
        {
            var listType = fListColumns.ListType;
            if (listType == GKListType.ltNone) return;

            var columnOpts = GlobalOptions.Instance.ListOptions[listType];
            //rView.SetSortColumn(columnOpts.SortColumn, false);
            columnOpts.Columns.CopyTo(fListColumns);
        }

        public void SaveSettings()
        {
            var listType = fListColumns.ListType;
            if (listType == GKListType.ltNone) return;

            var columnOpts = GlobalOptions.Instance.ListOptions[listType];
            //columnOpts.SortColumn = rView.SortColumn;
            fListColumns.CopyTo(columnOpts.Columns);
        }

        #region Columns

        protected virtual void UpdateColumnsMap()
        {
            fColumnsMap.Clear();

            int num = fListColumns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn cs = fListColumns.OrderedColumns[i];
                AddColumn(cs.ColName, cs.CurWidth, false, cs.Id, 0, cs.DataType);
            }
        }

        protected void AddColumn(string caption, int width, bool autoSize, byte colType, byte colSubtype, DataType dataType = DataType.dtString)
        {
            fColumnsMap.Add(new MapColumnRec(caption, width, autoSize, colType, colSubtype, dataType));
        }

        public void UpdateColumns(IListView listView)
        {
            UpdateColumnsMap();

            var defDateFormat = GlobalOptions.Instance.DefDateFormat;

            if (listView != null) {
                listView.ClearColumns();

                int num = fColumnsMap.Count;
                for (int i = 0; i < num; i++) {
                    var cm = fColumnsMap[i];

                    switch (cm.DataType) {
                        case DataType.dtBool:
                            listView.AddCheckedColumn(cm.Caption, cm.Width, cm.AutoSize);
                            break;

                        case DataType.dtGEDCOMDate:
                            var textAlign = (defDateFormat != DateFormat.dfDD_MM_YYYY) ? GKHorizontalAlignment.Left : GKHorizontalAlignment.Right;
                            listView.AddColumn(cm.Caption, cm.Width, cm.AutoSize, textAlign);
                            break;

                        default:
                            listView.AddColumn(cm.Caption, cm.Width, cm.AutoSize);
                            break;
                    }
                }
            }
        }

        public string GetColumnName(int columnId)
        {
            return (columnId >= 0 && columnId < fListColumns.Count) ? fListColumns[columnId].ColName : "<?>";
        }

        public DataType GetColumnDataType(int columnId)
        {
            return (columnId >= 0 && columnId < fListColumns.Count) ? fListColumns[columnId].DataType : DataType.dtString;
        }

        public virtual void ChangeColumnWidth(int colIndex, int colWidth)
        {
            if (colIndex < 0 || colIndex >= fColumnsMap.Count) return;

            MapColumnRec colrec = fColumnsMap[colIndex];
            ListColumn props = FindColumnProps(colrec.ColType);

            if (props != null) {
                props.CurWidth = colWidth;
                props.Autosize = false;
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

        public int GetColumnIndex(int columnId)
        {
            int result = -1;
            for (int i = 0, num = fListColumns.Count; i < num; i++) {
                ListColumn columnProps = fListColumns.OrderedColumns[i];
                if (columnProps.CurActive && columnProps.Id == columnId) {
                    result = i;
                    break;
                }
            }
            return result;
        }

        #endregion

        #region Mask processing

        /// <summary>
        /// Tests on working database, filtering 691 from 12174 records, pattern `*xxxx*xxx*`.
        ///     RegEx -> 394.4 ms -> x1
        ///     FastIgnoreCase -> 142.9 ms -> x2.8
        ///     Fast -> 37.9 ms -> x10.4
        /// </summary>
        protected bool IsMatchesMask(string str, string mask)
        {
            if (fFilterMethod != MatchPatternMethod.RegEx) {
                bool ignoreCase = (fFilterMethod == MatchPatternMethod.FastIgnoreCase);

                // This method of processing name matching with a pattern mask compared to using RegEx:
                //   4.6 times faster if without unsafe operations (601 -> 129 ms)
                //   and 11.5 times faster if with unsafe operations (601 -> 52 ms).
                return SysUtils.MatchPattern(mask, str, ignoreCase);
            } else {
                bool any = false;
                if (string.IsNullOrEmpty(mask) || (any = mask.Equals("*"))) {
                    return true;
                }

                if (string.IsNullOrEmpty(str)) {
                    return any;
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

        private Func<T, bool> fFilterFunc;

        public virtual void PrepareFilter()
        {
            fFilterMethod = GlobalOptions.Instance.MatchPatternMethod;

            var filterExpression = new FilterExpression(this, LogicalOperator.And);
            if (fFilter.Conditions.Count > 0) {
                filterExpression.Conditions.AddRange(fFilter.Conditions);
                fFilterFunc = filterExpression.GenerateFilterExpression<T>();
            } else {
                fFilterFunc = null;
            }
        }

        public virtual bool CheckFilter()
        {
            return true;
        }

        protected virtual void CreateFilter()
        {
            fFilter = new ListFilter();
        }

        public void AddCondition(byte columnId, ConditionOperator condition, string value)
        {
            object condValue = ConvertColumnStr(value, GetColumnDataType(columnId));

            ColumnConditionExpression fltCond = new ColumnConditionExpression(columnId, condition, condValue);
            fFilter.Conditions.Add(fltCond);
        }

        protected virtual bool CheckCommonCondition(ColumnConditionExpression fcond)
        {
            object dataval;
            try {
                dataval = GetColumnValueEx(fcond.ColumnIndex, -1, false);
            } catch (Exception ex) {
                Logger.WriteError("ListSource.CheckCommonCondition()", ex);
                dataval = null;
            }

            return ListFilter.CheckCondition(fcond, dataval);
        }

        protected bool CheckCommonFilter(GDMRecord rec)
        {
            bool res;
            try {
                // check external filter
                res = (fExternalFilter == null || fExternalFilter(rec));

                // check user conditions
                res = res && (fFilterFunc == null || fFilterFunc(rec as T));

                /*var conditions = fFilter.Conditions;
                for (int i = 0, num = conditions.Count; i < num; i++) {
                    ColumnConditionExpression fcond = conditions[i];
                    res = res && CheckCommonCondition(fcond);
                    if (!res) break;
                }*/
            } catch (Exception ex) {
                Logger.WriteError("ListSource.CheckCommonFilter()", ex);
                res = true;
            }
            return res;
        }

        public string[] CreateFields()
        {
            var listColumns = ListColumns;
            string[] fields = new string[listColumns.Count + 1]; // +empty item
            fields[0] = "";

            for (int idx = 0; idx < listColumns.Count; idx++) {
                var cs = listColumns[idx];
                fields[idx + 1] = GetColumnName(cs.Id);
            }

            return fields;
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

        private static readonly object UnknownUDNObject = (object)UDN.Unknown;

        protected static object GetDateValue(GDMCustomEvent evt, bool isVisible)
        {
            if (evt == null) {
                return (isVisible) ? null : UnknownUDNObject;
            }

            return GetDateValue(evt.Date.Value, isVisible);
        }

        protected static object GetDateValue(GDMCustomDate date, bool isVisible)
        {
            object result;

            if (date == null) {
                result = (isVisible) ? null : UnknownUDNObject;
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

        protected static object ConvertColumnValue(object val, ListColumn cs)
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
                    DateTime dtx = (DateTime)val;
                    return (dtx.Ticks == 0) ? "" : dtx.ToString("yyyy.MM.dd HH:mm:ss", null);

                case DataType.dtGEDCOMDate:
                    return val.ToString();

                case DataType.dtBool:
                    return val;

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

                case DataType.dtBool:
                    return bool.Parse(val);
            }

            return val;
        }

        public object GetColumnValue(int colIndex, bool isVisible)
        {
            object val;
            try {
                if (colIndex < 0 || colIndex >= fColumnsMap.Count) {
                    return ErrorValue;
                }

                MapColumnRec colrec = fColumnsMap[colIndex];
                val = GetColumnValueEx(colrec.ColType, colrec.ColSubtype, isVisible);

                if (isVisible) {
                    ListColumn cs = fListColumns[colrec.ColType];
                    val = ConvertColumnValue(val, cs);
                }
            } catch (Exception ex) {
                Logger.WriteError("ListSource.GetColumnValue()", ex);
                val = ErrorValue;
            }
            return val;
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

        /// <summary>
        /// Set a new column value. Currently only applicable for column 0 for lists in virtual mode (Checked flag items).
        /// </summary>
        /// <param name="rowIndex">rowIndex comes in the current sort order of the virtual list</param>
        /// <param name="colIndex"></param>
        /// <param name="value"></param>
        public virtual void SetColumnValue(int rowIndex, int colIndex, object value)
        {
            if (rowIndex >= 0 && rowIndex < fContentList.Count) {
                var item = fContentList[rowIndex].Record;
                SetColumnValueEx(item as T, colIndex, value);
            }
        }

        protected virtual void SetColumnValueEx(T item, int colIndex, object value)
        {
            // dummy
        }

        #endregion

        #region Items and content processing

        private object[] fItemData;
        private ContentItem fVirtualFetchedItem;

        public object GetColumnExternalValue(ContentItem contentItem, int colIndex)
        {
            // "super fetch"
            if (fVirtualFetchedItem != contentItem) {
                fItemData = GetItemData(contentItem.Record);
                fVirtualFetchedItem = contentItem;
            }

            return (fItemData == null || colIndex >= fItemData.Length) ? null : fItemData[colIndex];
        }

        // Minimizing memory allocations
        private object[] fDataBuffer;

        public object[] GetItemData(object rowData)
        {
            if (rowData is T rec) {
                Fetch(rec);

                int num = fColumnsMap.Count;

                if (fDataBuffer == null || fDataBuffer.Length != num)
                    fDataBuffer = new object[num];

                for (int i = 0; i < num; i++) {
                    fDataBuffer[i] = GetColumnValue(i, true);
                }

                return fDataBuffer;
            } else {
                return null;
            }
        }

        public virtual IColor GetBackgroundColor(int itemIndex, object rowData)
        {
            if (GlobalOptions.Instance.ReadabilityHighlightRows && MathHelper.IsOdd(itemIndex)) {
                return ChartRenderer.GetColor(GKData.HighlightReadabilityRows);
            }
            return null;
        }

        public object GetContentItem(int itemIndex)
        {
            object result = (itemIndex < 0 || itemIndex >= fContentList.Count) ? null : fContentList[itemIndex].Record;
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
                ContentItem contentItem = new ContentItem(this, rec);

                fContentList.Add(contentItem);

                contentItem.SortValue = GetColumnValue(fSortColumn, false);
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

            for (int i = 0, num = fContentList.Count; i < num; i++) {
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

        public virtual void OnItemSelected(int itemIndex, object rowData)
        {
        }

        #endregion

        #region Sort support

        private int CompareItems(ContentItem item1, ContentItem item2)
        {
            int compRes;
            object cv1 = item1.SortValue;
            object cv2 = item2.SortValue;

            if (cv1 != null) {
                if (cv2 != null) {
                    if (cv1 is string cv1str && cv2 is string cv2str) {
                        compRes = string.Compare(cv1str, cv2str, false, fSysCulture);
                    } else {
                        compRes = ((IComparable)cv1).CompareTo(cv2);
                    }
                } else {
                    compRes = -1;
                }
            } else if (cv2 != null) {
                compRes = 1;
            } else {
                compRes = 0;
            }

            return compRes * fXSortFactor;
        }

        public void SortContents(bool uiChange)
        {
            int recsCount = fContentList.Count;
            if (recsCount == 0) return;

            try {
                fSysCulture = (fBaseContext != null) ? CulturesPool.GetSystemCulture(fBaseContext.Culture) : SGCulture.CurrentCulture;

                fContentList.BeginUpdate();

                if (uiChange) {
                    for (int i = 0; i < recsCount; i++) {
                        ContentItem valItem = fContentList[i];
                        Fetch((T)valItem.Record);
                        valItem.SortValue = GetColumnValue(fSortColumn, false);
                    }
                }

                fXSortFactor = (fSortOrder == GKSortOrder.Ascending ? 1 : -1);
                ListTimSort<ContentItem>.Sort(fContentList, CompareItems);

                fContentList.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("ListSource.SortContents()", ex);
            }
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            int prevColumn = fSortColumn;
            if (prevColumn == sortColumn && checkOrder) {
                var prevOrder = (fSortColumn == sortColumn) ? fSortOrder : GKSortOrder.None;
                fSortOrder = (prevOrder == GKSortOrder.Ascending) ? GKSortOrder.Descending : GKSortOrder.Ascending;
            } else {
                fSortOrder = GKSortOrder.Ascending;
            }

            fSortColumn = sortColumn;

            if (fSortOrder != GKSortOrder.None)
                SortContents(true);
        }

        #endregion
    }
}
