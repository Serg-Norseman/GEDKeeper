/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Filters;
using GKCore.Utilities;

namespace GKCore.Lists
{
    public sealed class ContentItem
    {
        private readonly IListSource ListSource;

        public readonly object Record;
        public object SortValue;

        /// <summary>
        /// The property of supporting grids with the absence of bindings to arbitrary getters/setters.
        /// </summary>
        public object[] Values
        {
            get { return ListSource.GetItemData(Record); }
        }

        public ContentItem(IListSource listSource, object record)
        {
            ListSource = listSource;
            Record = record;
        }
    }


    public sealed class MapColumnRec
    {
        public string Caption;
        public int Width;
        public bool AutoSize;
        public byte ColType;
        public byte ColSubtype;
        public DataType DataType;

        public MapColumnRec(string caption, int width, bool autoSize, byte colType, byte colSubtype, DataType dataType)
        {
            Caption = caption;
            Width = width;
            AutoSize = autoSize;
            ColType = colType;
            ColSubtype = colSubtype;
            DataType = dataType;
        }
    }


    public interface IListSource
    {
        List<MapColumnRec> ColumnsMap { get; }
        ExtObservableList<ContentItem> ContentList { get; }
        ExternalFilterHandler ExternalFilter { get; set; }
        ListFilter Filter { get; }
        int FilteredCount { get; }
        ListColumns ListColumns { get; }
        int SortColumn { get; set; }
        GKSortOrder SortOrder { get; set; }
        int TotalCount { get; }

        void Clear();
        void AddCondition(byte columnId, ConditionOperator condition, string value);
        void ChangeColumnWidth(int colIndex, int colWidth);
        string[] CreateFields();
        bool DeleteItem(object data);
        DataType GetColumnDataType(int columnId);
        string GetColumnName(int columnId);

        /// <summary>
        /// Getting internal (raw) or display values for columns.
        /// </summary>
        /// <param name="colIndex">Column index</param>
        /// <param name="isVisible">Flag for display values</param>
        /// <returns></returns>
        object GetColumnValue(int colIndex, bool isVisible);

        /// <summary>
        /// Set a new column value. Currently only applicable for column 0 for lists in virtual mode (Checked flag items).
        /// </summary>
        /// <param name="rowIndex">rowIndex comes in the current sort order of the virtual list</param>
        /// <param name="colIndex"></param>
        /// <param name="value"></param>
        void SetColumnValue(int rowIndex, int colIndex, object value);

        object[] GetItemData(object rowData);

        /// <summary>
        /// Getting display values for binding columns of lists with virtualization (caching).
        /// </summary>
        /// <param name="contentItem">Internal intermediate item of list</param>
        /// <param name="colIndex">Column index</param>
        /// <returns></returns>
        object GetColumnExternalValue(ContentItem contentItem, int colIndex);

        IColor GetBackgroundColor(int itemIndex, object rowData);

        int GetColumnIndex(int columnId);
        object GetContentItem(int itemIndex);
        int GetFieldColumnId(string[] fields, string fieldName);
        int IndexOfItem(object data);
        bool IsColumnAutosize(int colIndex);
        void OnItemSelected(int itemIndex, object rowData);
        void RestoreSettings();
        void SaveSettings();
        void SetSortColumn(int sortColumn, bool checkOrder = true);
        void SortContents(bool uiChange);
        void UpdateColumns(IListView listView);
        void UpdateContents();
    }
}
