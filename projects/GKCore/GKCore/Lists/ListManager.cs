/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Externals;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ListSource : BaseObject, IListSource
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

        protected void AddColumn(IListView list, string caption, int width, bool autoSize, byte colType, byte colSubtype)
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

        public abstract void UpdateColumns(IListView listView);

        public abstract void UpdateContents();
    }

    /// <summary>
    /// 
    /// </summary>
    public abstract class ListManager : ListSource, IListManager
    {
        public sealed class ValItem
        {
            public readonly GEDCOMRecord Record;
            public object ColumnValue;

            public ValItem(GEDCOMRecord record)
            {
                Record = record;
                ColumnValue = null;
            }
        }

        protected ListFilter fFilter;
        protected ExternalFilterHandler fExternalFilter;

        private readonly List<ValItem> fContentList;
        private readonly GEDCOMRecordType fRecordType;
        private int fXSortFactor;
        private int fTotalCount;
        private string fQuickFilter = "*";


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

        public GEDCOMRecordType RecordType
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


        protected ListManager(IBaseContext baseContext, ListColumns defaultListColumns, GEDCOMRecordType recordType) :
            base(baseContext, defaultListColumns)
        {
            fContentList = new List<ValItem>();
            fRecordType = recordType;

            CreateFilter();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                // dummy
            }
            base.Dispose(disposing);
        }

        protected virtual void CreateFilter()
        {
            fFilter = new ListFilter();
        }

        protected static bool IsMatchesMask(string str, string mask)
        {
            bool result = false;
            if (string.IsNullOrEmpty(str) || string.IsNullOrEmpty(mask)) return result;

            string stx = str.ToLower();
            string[] masks = mask.ToLower().Split('|');

            int num = masks.Length;
            for (int i = 0; i < num; i++)
            {
                result = result || SysUtils.MatchesMask(stx, masks[i]);
            }

            return result;
        }

        public virtual bool CheckFilter()
        {
            return true;
        }

        public abstract void Fetch(GEDCOMRecord aRec);

        protected static object GetDateValue(GEDCOMCustomEvent evt, bool isVisible)
        {
            if (evt == null) {
                return (isVisible) ? null : (object)UDN.CreateEmpty();
            }

            return GetDateValue(evt.Date.Value, isVisible);
        }

        protected static object GetDateValue(GEDCOMCustomDate date, bool isVisible)
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
            GEDCOMRecord rec = rowData as GEDCOMRecord;
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

        public virtual void UpdateItem(IListItem item, object rowData)
        {
            GEDCOMRecord rec = rowData as GEDCOMRecord;
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
        }

        public override void UpdateColumns(IListView listView)
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
                    return SysUtils.ParseInt(val, 0);

                case DataType.dtFloat:
                    return SysUtils.ParseFloat(val, 0.0);

                case DataType.dtDateTime:
                    return DateTime.Parse(val);

                case DataType.dtGEDCOMDate:
                    return GEDCOMDate.GetUDNByFormattedStr(val, GEDCOMCalendar.dcGregorian);
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

            try
            {
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
                        res = SysUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value + "*");
                        break;

                    case ConditionKind.ck_NotContains:
                        res = !SysUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value + "*");
                        break;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("ListManager.CheckCondition(): " + ex.Message);
                res = true;
            }

            return res;
        }

        protected bool CheckCommonFilter()
        {
            bool res = true;

            try
            {
                int num = Filter.Conditions.Count;
                for (int i = 0; i < num; i++) {
                    FilterCondition fcond = Filter.Conditions[i];
                    res = res && CheckCondition(fcond);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("ListManager.CheckCommonFilter(): " + ex.Message);
                res = true;
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
                    GEDCOMRecord rec = valItem.Record;

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
                Logger.LogWrite("ListManager.SortContents(): " + ex.Message);
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
                GEDCOMRecord rec = fBaseContext.Tree[i];

                if (rec.RecordType == fRecordType) {
                    fTotalCount++;

                    Fetch(rec);
                    if (CheckFilter()) {
                        fContentList.Add(new ValItem(rec));
                    }
                }
            }
        }

        public List<GEDCOMRecord> GetRecordsList()
        {
            int size = fContentList.Count;
            var result = new List<GEDCOMRecord>(size);

            for (int i = 0; i < size; i++) {
                result.Add(fContentList[i].Record);
            }

            return result;
        }

        public IListItem CreateListItem(object rowData, CreateListItemHandler handler)
        {
            GEDCOMRecord record = rowData as GEDCOMRecord;
            if (record == null || handler == null) return null;

            return handler(record.GetXRefNum(), record);
        }

        public GEDCOMRecord GetContentItem(int itemIndex)
        {
            GEDCOMRecord result;
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

        public static ListManager Create(IBaseContext baseContext, GEDCOMRecordType recType)
        {
            ListManager result = null;

            switch (recType) {
                case GEDCOMRecordType.rtIndividual:
                    result = new IndividualListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtFamily:
                    result = new FamilyListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtNote:
                    result = new NoteListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    result = new MultimediaListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtSource:
                    result = new SourceListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtRepository:
                    result = new RepositoryListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtGroup:
                    result = new GroupListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtResearch:
                    result = new ResearchListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtTask:
                    result = new TaskListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    result = new CommunicationListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtLocation:
                    result = new LocationListMan(baseContext);
                    break;

                case GEDCOMRecordType.rtSubmission:
                    result = null;
                    break;

                case GEDCOMRecordType.rtSubmitter:
                    result = null;
                    break;
            }

            return result;
        }
    }
}
