/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore.Lists;
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


    public enum ConditionKind : int
    {
        ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT,
        ck_Contains, ck_NotContains,
        ck_ContainsMask, ck_NotContainsMask,

        ck_Last = ck_NotContainsMask
    }


    public class FilterCondition
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

        void Assign(IListFilter other);
        void Clear();
        string ToString(IListSource listSource);

        void Deserialize(string value);
        string Serialize();
    }


    public interface IIndividualListFilter : IListFilter
    {
        FilterLifeMode FilterLifeMode { get; set; }
    }


    public interface IListColumns
    {
        int Count { get; }
        ListColumn this[int index] { get; }
        IList<ListColumn> OrderedColumns { get; }

        void Clear();
        void CopyTo(IListColumns target);
        bool MoveColumn(int idx, bool up);
        void ResetDefaults();
        void UpdateOrders();

        void LoadFromFile(IniFile iniFile, string section, int optsVersion);
        void SaveToFile(IniFile iniFile, string section, int optsVersion);
    }
}
