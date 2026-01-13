/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        private const string AnyPattern = "*";

        public string Value;
        public MatchType Type;
        public float IndistinctThreshold;

        public bool IsEmpty { get { return string.Equals(Value, AnyPattern); } }

        public QuickFilterParams()
        {
            Value = AnyPattern;
            Type = MatchType.REMask;
            IndistinctThreshold = 1.00f;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class ListFilter
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

        public virtual void Assign(ListFilter other)
        {
            if (other == null)
                throw new ArgumentNullException(nameof(other));

            fConditions.Clear();
            fConditions.AddRange(other.fConditions);
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
