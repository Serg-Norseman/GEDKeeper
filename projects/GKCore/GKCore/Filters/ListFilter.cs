/*
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
using GDModel;

namespace GKCore.Filters
{
    public delegate bool ExternalFilterHandler(GDMRecord record);


    public enum MatchPatternMethod
    {
        RegEx, FastIgnoreCase, Fast
    }


    public enum MatchType
    {
        REMask,
        Indistinct
    }


    public class QuickFilterParams
    {
        public string Value;
        public MatchType Type;
        public float IndistinctThreshold;

        public QuickFilterParams()
        {
            Value = "*";
            Type = MatchType.REMask;
            IndistinctThreshold = 1.00f;
        }
    }


    public interface IListFilter
    {
        List<ColumnConditionExpression> Conditions { get; }

        void Assign(IListFilter other);
        void Clear();
    }


    /// <summary>
    /// 
    /// </summary>
    public class ListFilter : IListFilter
    {
        private readonly List<ColumnConditionExpression> fConditions;

        public List<ColumnConditionExpression> Conditions
        {
            get { return fConditions; }
        }

        public ListFilter()
        {
            fConditions = new List<ColumnConditionExpression>();
        }

        public virtual void Clear()
        {
            fConditions.Clear();
        }

        public virtual void Assign(IListFilter other)
        {
            var otherFilter = other as ListFilter;
            if (otherFilter == null)
                throw new ArgumentNullException("other");

            fConditions.Clear();
            fConditions.AddRange(otherFilter.fConditions);
        }

        internal static bool CheckCondition(ColumnConditionExpression fcond, object dataval)
        {
            bool res = true;

            try {
                if (dataval == null)
                    return true;

                int compRes = 0;
                if (fcond.Operator < ConditionOperator.Contains) {
                    compRes = ((IComparable)dataval).CompareTo(fcond.Value);
                }

                switch (fcond.Operator) {
                    case ConditionOperator.NotEqual:
                        res = compRes != 0;
                        break;

                    case ConditionOperator.LessThan:
                        res = compRes < 0;
                        break;

                    case ConditionOperator.LessThanOrEqual:
                        res = compRes <= 0;
                        break;

                    case ConditionOperator.Equal:
                        res = compRes == 0;
                        break;

                    case ConditionOperator.GreaterThanOrEqual:
                        res = compRes >= 0;
                        break;

                    case ConditionOperator.GreaterThan:
                        res = compRes > 0;
                        break;

                    case ConditionOperator.Contains:
                        res = dataval.ToString().Contains((string)fcond.Value);
                        break;

                    case ConditionOperator.NotContains:
                        res = !dataval.ToString().Contains((string)fcond.Value);
                        break;

                    case ConditionOperator.ContainsMask:
                        res = GKUtils.MatchesMask(dataval.ToString(), (string)fcond.Value);
                        break;

                    case ConditionOperator.NotContainsMask:
                        res = !GKUtils.MatchesMask(dataval.ToString(), (string)fcond.Value);
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("ListFilter.CheckCondition()", ex);
                res = true;
            }

            return res;
        }

        public static ConditionOperator GetCondByName(string condName)
        {
            ConditionOperator res = ConditionOperator.NotEqual;

            for (ConditionOperator pl = ConditionOperator.NotEqual; pl <= ConditionOperator.Last; pl++) {
                if (GKData.CondSigns[(int)pl] == condName) {
                    res = pl;
                    break;
                }
            }

            return res;
        }
    }
}
