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

using System.Collections.Generic;
using GDModel;
using GKCore.Design.Graphics;
using GKCore.Design.MVP.Controls;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(GDMRecord record);

    public delegate IListItem CreateListItemHandler(object data, object[] columnValues);


    public sealed class ContentItem
    {
        public readonly object Record;
        public object SortValue;

        public ContentItem(object record)
        {
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

        public MapColumnRec(string caption, int width, bool autoSize, byte colType, byte colSubtype)
        {
            Caption = caption;
            Width = width;
            AutoSize = autoSize;
            ColType = colType;
            ColSubtype = colSubtype;
        }
    }


    public interface IListSource
    {
        IBaseContext BaseContext { get; }
        bool ColumnsHaveBeenChanged { get; set; }
        List<MapColumnRec> ColumnsMap { get; }
        ExtObservableList<ContentItem> ContentList { get; }
        ExternalFilterHandler ExternalFilter { get; set; }
        IListFilter Filter { get; }
        int FilteredCount { get; }
        IListColumns ListColumns { get; }
        int TotalCount { get; }
        string QuickFilter { get; set; }

        void AddCondition(byte columnId, ConditionKind condition, string value);
        void ChangeColumnWidth(int colIndex, int colWidth);
        string[] CreateFields();
        IListItem CreateListItem(int itemIndex, object rowData, CreateListItemHandler handler);
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

        object[] GetItemData(object rowData);

        /// <summary>
        /// Getting display values for binding columns of lists with virtualization (caching).
        /// </summary>
        /// <param name="contentItem">Internal intermediate item of list</param>
        /// <param name="colIndex">Column index</param>
        /// <returns></returns>
        string GetColumnExternalValue(ContentItem contentItem, int colIndex);

        IColor GetBackgroundColor(int itemIndex, object rowData);

        ConditionKind GetCondByName(string condName);
        object GetContentItem(int itemIndex);
        int GetFieldColumnId(string[] fields, string fieldName);
        int IndexOfItem(object data);
        bool IsColumnAutosize(int colIndex);
        void SortContents(int sortColumn, bool sortAscending);
        void UpdateColumns(IListViewEx listView);
        void UpdateContents();
    }
}
