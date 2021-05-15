﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKUI.Components;
using GKUI.Platform;
using NSubstitute;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class ListsTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);
            LangMan.DefInit();

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
        public void Test_Lists()
        {
            var colStatic = new ListColumn(0, 0, DataType.dtString, 0, true);
            Assert.IsNotNull(colStatic);
            Assert.AreEqual(0, colStatic.Order);
            Assert.AreEqual(false, colStatic.CurActive);
            Assert.AreEqual(0, colStatic.CurWidth);

            //
            var listFilter = new ListFilter();
            Assert.IsNotNull(listFilter);
            Assert.AreEqual(0, listFilter.Conditions.Count);
            listFilter.Clear();
            Assert.AreEqual(0, listFilter.Conditions.Count);
        }

        [Test]
        public void Test_LMGroup()
        {
            var listManager = new GroupListMan(fContext);
            Assert.IsNotNull(listManager);

            var grpRec = fContext.Tree.XRefIndex_Find("G1") as GDMGroupRecord;
            listManager.Fetch(grpRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*roup*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, grpRec);

            //
            var colVal = listManager.GetColumnInternalValue(0);
            Assert.IsNotNull(colVal);

            var data = listManager.GetItemData(grpRec);
            Assert.IsNotNull(data);
            Assert.IsTrue(data.Length > 0);

            //
            IListFilter filter = listManager.Filter;
            IListColumns listColumns = listManager.ListColumns;

            ListColumns copyColumns = GroupListMan.CreateGroupListColumns();
            listColumns.CopyTo(copyColumns);

            Assert.Throws(typeof(ArgumentNullException), () => { listColumns.CopyTo(null); });

            listManager.QuickFilter = "*";
            listManager.AddCondition((byte)GroupColumnType.ctName, ConditionKind.ck_Contains, "*roup*");
            Assert.IsTrue(listManager.CheckFilter());
        }

        [Test]
        public void Test_LMCommunication()
        {
            var listManager = new CommunicationListMan(fContext);
            Assert.IsNotNull(listManager);

            Assert.IsNotNull(listManager.ContentList);

            listManager.ExternalFilter = null;
            Assert.IsNull(listManager.ExternalFilter);

            var communicationRec = fContext.Tree.XRefIndex_Find("CM1") as GDMCommunicationRecord;
            listManager.Fetch(communicationRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*commun*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, communicationRec);
        }

        [Test]
        public void Test_LMFamily()
        {
            var listManager = new FamilyListMan(fContext);
            Assert.IsNotNull(listManager);

            var familyRec = fContext.Tree.XRefIndex_Find("F1") as GDMFamilyRecord;
            listManager.Fetch(familyRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "* - *";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, familyRec);
        }

        private bool ExtFilterHandler(GDMRecord record)
        {
            return true;
        }

        [Test]
        public void Test_LMIndividual()
        {
            var listManager = new IndividualListMan(fContext);
            Assert.IsNotNull(listManager);

            var individualRec = fContext.Tree.XRefIndex_Find("I4") as GDMIndividualRecord;
            listManager.Fetch(individualRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*Petr*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            GlobalOptions.Instance.ListHighlightUnparentedPersons = true;
            GlobalOptions.Instance.ListHighlightUnmarriedPersons = true;
            listManager.PrepareFilter();
            listManager.ExternalFilter = ExtFilterHandler;

            var listView = Substitute.For<IListViewEx>();

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfFNP;
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, individualRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_NP;
            listManager.UpdateColumns(listView);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, individualRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_N_P;
            listManager.UpdateColumns(listView);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, individualRec);
        }

        [Test]
        public void Test_LMLocation()
        {
            var listManager = new LocationListMan(fContext);
            Assert.IsNotNull(listManager);

            var locationRec = fContext.Tree.XRefIndex_Find("L1") as GDMLocationRecord;
            listManager.Fetch(locationRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*locat*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, locationRec);
        }

        [Test]
        public void Test_LMMultimedia()
        {
            var listManager = new MultimediaListMan(fContext);
            Assert.IsNotNull(listManager);

            var mediaRec = fContext.Tree.XRefIndex_Find("O1") as GDMMultimediaRecord;
            listManager.Fetch(mediaRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*media*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, mediaRec);
        }

        [Test]
        public void Test_LMNote()
        {
            var listManager = new NoteListMan(fContext);
            Assert.IsNotNull(listManager);

            var noteRec = new GDMNoteRecord(null);
            noteRec.AddNoteText("Test text");
            listManager.Fetch(noteRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*text*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, noteRec);
            noteRec.Clear();
            listManager.UpdateItem(0, listItem, noteRec);
        }

        [Test]
        public void Test_LMRepository()
        {
            var listManager = new RepositoryListMan(fContext);
            Assert.IsNotNull(listManager);

            var repositoryRec = fContext.Tree.XRefIndex_Find("R1") as GDMRepositoryRecord;
            listManager.Fetch(repositoryRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*repos*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, repositoryRec);
        }

        [Test]
        public void Test_LMResearch()
        {
            var listManager = new ResearchListMan(fContext);
            Assert.IsNotNull(listManager);

            var researchRec = fContext.Tree.XRefIndex_Find("RS1") as GDMResearchRecord;
            listManager.Fetch(researchRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*resear*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, researchRec);
        }

        [Test]
        public void Test_LMSource()
        {
            var listManager = new SourceListMan(fContext);
            Assert.IsNotNull(listManager);

            var sourceRec = fContext.Tree.XRefIndex_Find("S1") as GDMSourceRecord;
            listManager.Fetch(sourceRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*sourc*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, sourceRec);
        }

        [Test]
        public void Test_LMTask()
        {
            var listManager = new TaskListMan(fContext);
            Assert.IsNotNull(listManager);

            var taskRec = fContext.Tree.XRefIndex_Find("TK1") as GDMTaskRecord;
            listManager.Fetch(taskRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*task*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var listView = Substitute.For<IListViewEx>();
            listManager.UpdateColumns(listView);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(0, listItem, taskRec);
        }
    }
}
