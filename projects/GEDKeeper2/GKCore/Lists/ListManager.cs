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
using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ListManager : BaseObject, IListManager
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

        protected ListFilter fFilter;
        protected GEDCOMTree fTree;
        protected ExternalFilterHandler fExternalFilter;

        private readonly ListColumns fListColumns;
        private readonly List<MapColumnRec> fColumnsMap;

        public string QuickFilter = "*";

        public ExternalFilterHandler ExternalFilter
        {
            get { return fExternalFilter; }
            set { fExternalFilter = value; }
        }

        public IListFilter Filter
        {
            get { return fFilter; }
        }

        public IListColumns ListColumns
        {
            get { return fListColumns; }
        }

        protected ListManager(GEDCOMTree tree, ListColumns defaultListColumns)
        {
            fTree = tree;
            fListColumns = defaultListColumns;
            fColumnsMap = new List<MapColumnRec>();

            CreateFilter();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                // dummy
            }
            base.Dispose(disposing);
        }

        protected void AddListColumn(IListView list, string caption, int width, bool autoSize, byte colType, byte colSubtype)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            list.AddListColumn(caption, width, autoSize);
            fColumnsMap.Add(new MapColumnRec(colType, colSubtype));
        }

        protected void ColumnsMap_Clear()
        {
            fColumnsMap.Clear();
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
                result = result || GKUtils.MatchesMask(stx, masks[i]);
            }

            return result;
        }

        public abstract bool CheckFilter(ShieldState shieldState);
        public abstract void Fetch(GEDCOMRecord aRec);

        protected static object GetDateValue(GEDCOMCustomEvent evt, bool isVisible)
        {
            if (evt == null) {
                return (isVisible) ? null : (object)UDN.CreateEmpty();
            }

            return GetDateValue(evt.Detail.Date.Value, isVisible);
        }

        protected static object GetDateValue(GEDCOMCustomDate date, bool isVisible)
        {
            object result;

            if (date == null) {
                result = (isVisible) ? null : (object)UDN.CreateEmpty();
            } else {
                if (isVisible) {
                    GlobalOptions glob = GlobalOptions.Instance;
                    result = GKUtils.GetCustomDateFmtString(date, glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
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

        public virtual void InitFilter()
        {
        }

        public virtual void UpdateItem(GKListItem item, bool isMain)
        {
            if (item == null) return;

            int num = fColumnsMap.Count;
            for (int i = 1; i < num; i++)
            {
                MapColumnRec colrec = fColumnsMap[i];

                // aColIndex - from 1
                ListColumn cs = fListColumns[colrec.ColType];
                object val = GetColumnValueEx(colrec.ColType, colrec.ColSubtype, true);
                string res = ConvertColumnValue(val, cs);

                item.SubItems.Add(res);
            }
        }

        public virtual void UpdateColumns(IListView listView, bool isMain)
        {
            if (listView == null) return;

            ColumnsMap_Clear();
            AddListColumn(listView, "№", 50, false, 0, 0);

            int num = fListColumns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn cs = fListColumns.OrderedColumns[i];

                AddListColumn(listView, LangMan.LS(cs.ColName), cs.DefWidth, false, (byte)i, 0);
            }
        }

        public string GetColumnName(byte columnId)
        {
            if (columnId >= 0 && columnId < fListColumns.Count) {
                return LangMan.LS(fListColumns[columnId].ColName);
            }

            return "<?>";
        }

        public DataType GetColumnDataType(int columnId)
        {
            int col = columnId/* - 1*/;

            if (col >= 0 && col < fListColumns.Count) {
                return fListColumns[col].DataType;
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
                    return GEDCOMUtils.GetUDN(val);
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
                        res = GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value + "*");
                        break;

                    case ConditionKind.ck_NotContains:
                        res = !GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value + "*");
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

        // FIXME
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

        public void WidthChanged(int colIndex, int newWidth)
        {
            if (colIndex <= 0) return;

            MapColumnRec colrec = fColumnsMap[colIndex];
            ListColumn props = FindColumnProps(colrec.ColType);

            if (props != null) {
                props.CurWidth = newWidth;
            }
        }
    }
}
