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
using GKCommon.GEDCOM;
using GKCore.Types;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(GEDCOMRecord record);

    public delegate IListItem CreateListItemHandler(object itemValue, object data);


    public interface IListSource : IDisposable
    {
        EnumSet<RecordAction> AllowedActions { get; }
        IBaseContext BaseContext { get; }
        bool ColumnsHaveBeenChanged { get; set; }
        IListColumns ListColumns { get; }

        void UpdateColumns(IListView listView);
        void UpdateContents();
    }


    public interface IListManager : IListSource
    {
        void AddCondition(byte columnId, ConditionKind condition, string value);
        DataType GetColumnDataType(int index);
        string GetColumnName(byte columnId);

        ExternalFilterHandler ExternalFilter { get; set; }
        IListFilter Filter { get; }
        int FilteredCount { get; }
        string QuickFilter { get; set; }
        GEDCOMRecordType RecordType { get; }
        int TotalCount { get; }

        void ChangeColumnWidth(int colIndex, int colWidth);
        IListItem CreateListItem(object rowData, CreateListItemHandler handler);
        bool DeleteRecord(object data);
        GEDCOMRecord GetContentItem(int itemIndex);
        List<GEDCOMRecord> GetRecordsList();
        int IndexOfRecord(object data);
        bool IsColumnAutosize(int colIndex);
        void SortContents(int sortColumn, bool sortAscending);
        void UpdateItem(IListItem item, object rowData);
    }
}
