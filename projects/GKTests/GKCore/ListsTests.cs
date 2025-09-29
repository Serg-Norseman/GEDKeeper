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
using GDModel;
using GKCore.Design.Controls;
using GKCore.Filters;
using GKCore.Names;
using GKCore.Options;
using GKTests;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Lists
{
    [TestFixture]
    public class ListsTests
    {
        private readonly BaseContext fContext;

        public ListsTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_GDMDateItemAsIConvertible()
        {
            var dtx1 = new GDMDateValue();
            dtx1.ParseString("05 JAN 2013");
            IConvertible dtItem1 = new GDMDateItem(dtx1);

            Assert.AreEqual(TypeCode.Object, dtItem1.GetTypeCode());
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToBoolean(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToChar(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToSByte(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToByte(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToInt16(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToUInt16(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToInt32(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToUInt32(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToInt64(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToUInt64(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToSingle(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToDouble(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToDecimal(null); });
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToDateTime(null); });
            Assert.AreEqual("05.01.2013", dtItem1.ToString(null));
            Assert.Throws(typeof(NotImplementedException), () => { dtItem1.ToType(typeof(Int32), null); });
        }

        [Test]
        public void Test_GDMDateItem_Common()
        {
            var dtx1 = new GDMDateValue();
            dtx1.ParseString("05 JAN 2013");
            var dtItem1 = new GDMDateItem(dtx1);
            Assert.AreEqual("05.01.2013", dtItem1.ToString());

            var dtx2 = new GDMDateValue();
            dtx2.ParseString("17 FEB 2013");
            var dtItem2 = new GDMDateItem(dtx2);
            Assert.AreEqual("17.02.2013", dtItem2.ToString());

            Assert.AreEqual(0, dtItem1.CompareTo(dtItem1));
            Assert.AreEqual(-1, dtItem1.CompareTo(dtItem2));
            Assert.AreEqual(-1, dtItem1.CompareTo(null));
            Assert.AreEqual(+1, dtItem2.CompareTo(dtItem1));

            dtItem1 = new GDMDateItem(dtx1);
            dtItem2 = new GDMDateItem(null);
            Assert.AreEqual(-1, dtItem1.CompareTo(dtItem2));

            dtItem1 = new GDMDateItem(null);
            dtItem2 = new GDMDateItem(dtx2);
            Assert.AreEqual(+1, dtItem1.CompareTo(dtItem2));

            dtItem1 = new GDMDateItem(null);
            dtItem2 = new GDMDateItem(null);
            Assert.AreEqual(0, dtItem1.CompareTo(dtItem2));
        }

        [Test]
        public void Test_ListColumn()
        {
            var colStatic = new ListColumn(0, "", DataType.dtString, 0, true);
            Assert.IsNotNull(colStatic);
            Assert.AreEqual(0, colStatic.Order);
            Assert.AreEqual(false, colStatic.CurActive);
            Assert.AreEqual(0, colStatic.CurWidth);
        }

        [Test]
        public void Test_LMGroup()
        {
            var listManager = new GroupListModel(fContext);
            Assert.IsNotNull(listManager);

            var grpRec = fContext.Tree.FindXRef<GDMGroupRecord>("G1");
            listManager.Fetch(grpRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*roup*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(grpRec);

            //
            var colVal = listManager.GetColumnValue(0, false);
            Assert.IsNotNull(colVal);

            //
            var filter = listManager.Filter;
            var listColumns = listManager.ListColumns;

            var copyColumns = GroupListModel.CreateListColumns();
            listColumns.CopyTo(copyColumns);

            Assert.Throws(typeof(ArgumentNullException), () => { listColumns.CopyTo(null); });

            listManager.QuickFilter.Value = "*";
            listManager.AddCondition((byte)GroupListModel.ColumnType.ctName, ConditionOperator.ContainsMask, "*roup*");
            Assert.IsTrue(listManager.CheckFilter());
        }

        [Test]
        public void Test_LMCommunication()
        {
            var listManager = new CommunicationListModel(fContext);
            Assert.IsNotNull(listManager);

            Assert.IsNotNull(listManager.ContentList);

            listManager.ExternalFilter = null;
            Assert.IsNull(listManager.ExternalFilter);

            var communicationRec = fContext.Tree.FindXRef<GDMCommunicationRecord>("CM1");
            listManager.Fetch(communicationRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*commun*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(communicationRec);
        }

        [Test]
        public void Test_LMFamily()
        {
            var listManager = new FamilyListModel(fContext);
            Assert.IsNotNull(listManager);

            var familyRec = fContext.Tree.FindXRef<GDMFamilyRecord>("F1");
            listManager.Fetch(familyRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "* - *";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(familyRec);
        }

        private bool ExtFilterHandler(GDMRecord record)
        {
            return true;
        }

        [Test]
        public void Test_LMIndividual()
        {
            var listManager = new IndividualListModel(fContext);
            Assert.IsNotNull(listManager);

            var individualRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I4");
            listManager.Fetch(individualRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*Petr*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            GlobalOptions.Instance.ListHighlightUnparentedPersons = true;
            GlobalOptions.Instance.ListHighlightUnmarriedPersons = true;
            listManager.PrepareFilter();
            listManager.ExternalFilter = ExtFilterHandler;

            var listView = Substitute.For<IListView>();

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfFNP;
            listManager.UpdateColumns(listView);
            listManager.GetItemData(individualRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_NP;
            listManager.UpdateColumns(listView);
            listManager.GetItemData(individualRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_N_P;
            listManager.UpdateColumns(listView);
            listManager.GetItemData(individualRec);
        }

        [Test]
        public void Test_LMLocation()
        {
            var listManager = new LocationListModel(fContext);
            Assert.IsNotNull(listManager);

            var locationRec = fContext.Tree.FindXRef<GDMLocationRecord>("L1");
            listManager.Fetch(locationRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*Locat*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(locationRec);
        }

        [Test]
        public void Test_LMMultimedia()
        {
            var listManager = new MultimediaListModel(fContext);
            Assert.IsNotNull(listManager);

            var mediaRec = fContext.Tree.FindXRef<GDMMultimediaRecord>("O1");
            listManager.Fetch(mediaRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*media*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(mediaRec);
        }

        [Test]
        public void Test_LMNote()
        {
            var listManager = new NoteListModel(fContext);
            Assert.IsNotNull(listManager);

            var noteRec = new GDMNoteRecord(null);
            noteRec.AddNoteText("Test text");
            listManager.Fetch(noteRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*text*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(noteRec);
            noteRec.Clear();
            listManager.GetItemData(noteRec);
        }

        [Test]
        public void Test_LMRepository()
        {
            var listManager = new RepositoryListModel(fContext);
            Assert.IsNotNull(listManager);

            var repositoryRec = fContext.Tree.FindXRef<GDMRepositoryRecord>("R1");
            listManager.Fetch(repositoryRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*repos*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(repositoryRec);
        }

        [Test]
        public void Test_LMResearch()
        {
            var listManager = new ResearchListModel(fContext);
            Assert.IsNotNull(listManager);

            var researchRec = fContext.Tree.FindXRef<GDMResearchRecord>("RS1");
            listManager.Fetch(researchRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*resear*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(researchRec);
        }

        [Test]
        public void Test_LMSource()
        {
            var listManager = new SourceListModel(fContext);
            Assert.IsNotNull(listManager);

            var sourceRec = fContext.Tree.FindXRef<GDMSourceRecord>("S1");
            listManager.Fetch(sourceRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*sourc*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(sourceRec);
        }

        [Test]
        public void Test_LMTask()
        {
            var listManager = new TaskListModel(fContext);
            Assert.IsNotNull(listManager);

            var taskRec = fContext.Tree.FindXRef<GDMTaskRecord>("TK1");
            listManager.Fetch(taskRec);

            listManager.QuickFilter.Value = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*task*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter.Value = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListView>();
            listManager.UpdateColumns(listView);
            listManager.GetItemData(taskRec);
        }
    }
}
