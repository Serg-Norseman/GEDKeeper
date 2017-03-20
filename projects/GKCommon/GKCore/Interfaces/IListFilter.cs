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
using GKCore.Types;

namespace GKCore.Interfaces
{
    public enum DataType
    {
        dtString,
        dtInteger,
        dtFloat,
        dtDateTime,
        dtGEDCOMDate
    }

    public enum ConditionKind
    {
        ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Contains, ck_NotContains
    }

    public sealed class FilterCondition
    {
        public int ColumnIndex;
        public ConditionKind Condition;
        public object Value;
        
        public FilterCondition(int columnIndex, ConditionKind condition, object value)
        {
            ColumnIndex = columnIndex;
            Condition = condition;
            Value = value;
        }
    }

    public interface IListFilter
    {
        List<FilterCondition> Conditions { get; }
        void Clear();
    }

    public interface IIndividualListFilter : IListFilter
    {
        FilterLifeMode FilterLifeMode { get; set; }
    }

    public interface IListColumns
    {
        void CopyTo(IListColumns columns);
        bool MoveColumn(int idx, bool up);
        void ResetDefaults();
        void UpdateOrders();
    }
}
