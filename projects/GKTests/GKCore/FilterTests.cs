// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System.Collections.Generic;
using System.Linq;
using GDModel;
using NUnit.Framework;

namespace GKCore.Filters
{
    [TestFixture]
    public class FilterTests
    {
        private class LItem
        {
            public int id { get; set; }
            public string name { get; set; }
            public string value { get; set; }
        }

        private bool TestExternalFilterHandler(GDMRecord record)
        {
            return false;
        }

        [Test]
        public void Test_FiltersIntf()
        {
            ColumnConditionExpression cond = new ColumnConditionExpression(0, ConditionOperator.Contains, null);
            Assert.IsNotNull(cond);

            ExternalFilterHandler handler = TestExternalFilterHandler;
            Assert.IsFalse(handler.Invoke(null));
        }

        [Test]
        public void Test_ListFilter()
        {
            var listFilter = new ListFilter();
            Assert.IsNotNull(listFilter);
            Assert.AreEqual(0, listFilter.Conditions.Count);
            listFilter.Clear();
            Assert.AreEqual(0, listFilter.Conditions.Count);
        }

        [Test]
        public void Test_FilterExpr()
        {
            var list = new List<LItem>();
            list.Add(new LItem() { id = 1, name = "Adam", value = "Paradise" });
            list.Add(new LItem() { id = 2, name = "Eve", value = "Earth" });

            var conditions = new FilterExpression(null, LogicalOperator.And);
            conditions.AddCondition(new FieldConditionExpression() { FieldName = "name", Operator = ConditionOperator.Contains, Value = "Ada" });
            conditions.AddCondition(new FieldConditionExpression() { FieldName = "value", Operator = ConditionOperator.Contains, Value = "rad" });

            var filterExpr = conditions.GenerateFilterExpression<LItem>();

            var result = list.Where(filterExpr).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("Adam", result[0].name);
        }

        [Test]
        public void Test_FilterExpr_Mask()
        {
            var list = new List<LItem>();
            list.Add(new LItem() { id = 1, name = "Adam", value = "Paradise" });
            list.Add(new LItem() { id = 2, name = "Eve", value = "Earth" });

            var conditions = new FilterExpression(null, LogicalOperator.And);
            conditions.AddCondition(new FieldConditionExpression() { FieldName = "name", Operator = ConditionOperator.ContainsMask, Value = "*Ada*" });
            conditions.AddCondition(new FieldConditionExpression() { FieldName = "value", Operator = ConditionOperator.ContainsMask, Value = "*rad*" });

            var filterExpr = conditions.GenerateFilterExpression<LItem>();

            var result = list.Where(filterExpr).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("Adam", result[0].name);
        }
    }
}
