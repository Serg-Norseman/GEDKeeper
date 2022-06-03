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
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Types;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(GDMRecord record);

    public delegate IListItem CreateListItemHandler(object itemValue, object data);


    public interface IListSource
    {
        EnumSet<RecordAction> AllowedActions { get; }
        IBaseContext BaseContext { get; }
        bool ColumnsHaveBeenChanged { get; set; }
        IListColumns ListColumns { get; }

        void UpdateColumns(IListViewEx listView);
        void UpdateContents();
    }


    public interface IListManager : IListSource
    {
        ExternalFilterHandler ExternalFilter { get; set; }
        IListFilter Filter { get; }
        int FilteredCount { get; }
        string QuickFilter { get; set; }
        GDMRecordType RecordType { get; }
        int TotalCount { get; }

        void AddCondition(byte columnId, ConditionKind condition, string value);
        DataType GetColumnDataType(int columnId);
        string GetColumnName(int columnId);

        void ChangeColumnWidth(int colIndex, int colWidth);
        IListItem CreateListItem(object rowData, CreateListItemHandler handler);
        bool DeleteRecord(object data);
        GDMRecord GetContentItem(int itemIndex);
        object[] GetItemData(object rowData);
        List<GDMRecord> GetRecordsList();
        int IndexOfRecord(object data);
        bool IsColumnAutosize(int colIndex);
        void SortContents(int sortColumn, bool sortAscending);
        void UpdateItem(int itemIndex, IListItem item, object rowData);

        void UpdateItemProps(IListItem item, object rowData);

        string[] CreateFields();
        ConditionKind GetCondByName(string condName);
        int GetFieldColumnId(string[] fields, string fieldName);

        IList<ISearchResult> FindAll(string searchPattern);
    }
}
