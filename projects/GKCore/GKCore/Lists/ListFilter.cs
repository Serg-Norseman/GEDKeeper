﻿/*
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
using System.Text;
using GKCore.Utilities;

namespace GKCore.Lists
{
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


    /// <summary>
    /// 
    /// </summary>
    public class ListFilter : IListFilter
    {
        private readonly List<FilterCondition> fConditions;

        public List<FilterCondition> Conditions
        {
            get { return fConditions; }
        }

        public ListFilter()
        {
            fConditions = new List<FilterCondition>();
        }

        public virtual void Clear()
        {
            fConditions.Clear();
        }

        public virtual string ToString(IListSource listSource)
        {
            if (listSource == null)
                return string.Empty;

            var fields = listSource.CreateFields();

            var sb = new StringBuilder();
            foreach (var cond in fConditions) {
                if (sb.Length != 0) sb.Append(", ");

                int condIndex = ((IConvertible)cond.Condition).ToByte(null);
                sb.Append(string.Format("{0} {1} `{2}`", fields[cond.ColumnIndex + 1], GKData.CondSigns[condIndex], cond.Value.ToString()));
            }
            return sb.ToString();
        }

        public virtual void Assign(IListFilter other)
        {
            var otherFilter = other as ListFilter;
            if (otherFilter == null)
                throw new ArgumentNullException("other");

            fConditions.Clear();
            fConditions.AddRange(otherFilter.fConditions);
        }

        public virtual void Deserialize(string value)
        {
            var instance = JsonHelper.Deserialize<ListFilter>(value);
            Assign(instance);
        }

        public string Serialize()
        {
            return JsonHelper.Serialize(this);
        }
    }
}
