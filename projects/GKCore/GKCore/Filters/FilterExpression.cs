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
using System.Linq.Expressions;
using System.Reflection;

namespace GKCore.Filters
{
    public enum ConditionOperator : int
    {
        NotEqual,
        LessThan,
        LessThanOrEqual,
        Equal,
        GreaterThanOrEqual,
        GreaterThan,

        Contains,
        NotContains,

        ContainsMask,
        NotContainsMask,

        Last = NotContainsMask
    }


    public enum LogicalOperator
    {
        And,
        Or
    }


    public abstract class BaseExpression
    {
    }


    public class ConditionExpression : BaseExpression
    {
        public string FieldName { get; set; }
        public object Value { get; set; }
        public ConditionOperator Operator { get; set; }

        public ConditionExpression()
        {
        }
    }


    public class FilterExpression : BaseExpression
    {
        public LogicalOperator Operator { get; private set; }
        public List<BaseExpression> Conditions { get; private set; }

        public FilterExpression(LogicalOperator @operator)
        {
            Operator = @operator;
            Conditions = new List<BaseExpression>();
        }

        public void AddCondition(BaseExpression condition)
        {
            Conditions.Add(condition);
        }

        public void RemoveCondition(BaseExpression condition)
        {
            Conditions.Remove(condition);
        }

        public Func<T, bool> GenerateFilterExpression<T>()
        {
            ParameterExpression param = Expression.Parameter(typeof(T), "item");

            Expression body = BuildExpression<T>(param, Operator, Conditions);

            return Expression.Lambda<Func<T, bool>>(body, param).Compile();
        }

        private Expression BuildExpression<T>(ParameterExpression param, LogicalOperator logicalOperator, List<BaseExpression> conditions)
        {
            Expression result = null;// = Expression.Constant(true);

            foreach (var condition in conditions) {
                Expression childExpr = null;

                if (condition is FilterExpression logicExp) {
                    childExpr = BuildExpression<T>(param, logicExp.Operator, logicExp.Conditions);
                } else if (condition is ConditionExpression condExp) {
                    var propInfo = typeof(T).GetProperty(condExp.FieldName);
                    if (propInfo != null) {
                        Expression propertyExpr = Expression.Property(param, propInfo);
                        Expression valueExpr = Expression.Constant(condExp.Value);
                        childExpr = BuildComparison(propertyExpr, valueExpr, condExp.Operator);
                    }
                }

                if (childExpr != null) {
                    if (result == null) {
                        result = childExpr;
                    } else {
                        if (logicalOperator == LogicalOperator.And)
                            result = Expression.AndAlso(result, childExpr);
                        else
                            result = Expression.OrElse(result, childExpr);
                    }
                }
            }

            return result;
        }

        private static Expression BuildComparison(Expression left, Expression right, ConditionOperator op)
        {
            switch (op) {
                case ConditionOperator.NotEqual:
                    return Expression.NotEqual(left, right);

                case ConditionOperator.LessThan:
                    return Expression.LessThan(left, right);

                case ConditionOperator.LessThanOrEqual:
                    return Expression.LessThanOrEqual(left, right);

                case ConditionOperator.Equal:
                    return Expression.Equal(left, right);

                case ConditionOperator.GreaterThanOrEqual:
                    return Expression.GreaterThanOrEqual(left, right);

                case ConditionOperator.GreaterThan:
                    return Expression.GreaterThan(left, right);

                case ConditionOperator.Contains:
                    return Expression.Call(left, "Contains", Type.EmptyTypes, right);

                case ConditionOperator.NotContains:
                    return Expression.Not(Expression.Call(left, "Contains", Type.EmptyTypes, right));

                case ConditionOperator.ContainsMask:
                    MethodInfo meth1 = typeof(GKUtils).GetMethod("MatchesMask", BindingFlags.Public | BindingFlags.Static);
                    return Expression.Call(meth1, left, right);

                case ConditionOperator.NotContainsMask:
                    MethodInfo meth2 = typeof(GKUtils).GetMethod("MatchesMask", BindingFlags.Public | BindingFlags.Static);
                    return Expression.Not(Expression.Call(meth2, left, right));

                /*case ConditionOperator.StartsWith:
                    return Expression.Call(left, "StartsWith", Type.EmptyTypes, right);
                case ConditionOperator.EndsWith:
                    return Expression.Call(left, "EndsWith", Type.EmptyTypes, right);*/

                default:
                    throw new NotSupportedException($"Operator {op} is not supported");
            }
        }
    }
}
