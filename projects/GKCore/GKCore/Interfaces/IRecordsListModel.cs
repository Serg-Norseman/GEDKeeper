/*
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

using System.Collections.Generic;
using BSLib.Design.MVP.Controls;
using GDModel;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(GDMRecord record);

    public delegate IListItem CreateListItemHandler(object data, object[] columnValues);


    public interface IListSource
    {
        IBaseContext BaseContext { get; }
        bool ColumnsHaveBeenChanged { get; set; }
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
        ConditionKind GetCondByName(string condName);
        object GetContentItem(int itemIndex);
        int GetFieldColumnId(string[] fields, string fieldName);
        int IndexOfItem(object data);
        bool IsColumnAutosize(int colIndex);
        void SortContents(int sortColumn, bool sortAscending);
        void UpdateColumns(IListViewEx listView);
        void UpdateContents();
    }


    public interface IListSource<T> : IListSource
        where T : GDMTag
    {
    }


    public interface IRecordsListModel : IListSource
    {
        GDMRecordType RecordType { get; }

        List<GDMRecord> GetRecordsList();

        IList<ISearchResult> FindAll(string searchPattern);
    }


    public interface IRecordsListModel<T> : IRecordsListModel, IListSource<T>
        where T : GDMRecord
    {
    }
}
