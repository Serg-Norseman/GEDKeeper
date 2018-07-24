/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKUI;
using GKUI.Components;
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
            WinFormsAppHost.ConfigureBootstrap(false);

            LangMan.DefInit();

            fContext = TestStubs.CreateContext();
            TestStubs.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        [Test]
        public void Test_DateItems_IConvertible()
        {
            var dtx1 = new GEDCOMDateValue(null, null, "DATE", "05 JAN 2013");
            var dtItem1 = new GEDCOMDateItem(dtx1);

            Assert.AreEqual(TypeCode.Object, ((IConvertible)dtItem1).GetTypeCode());
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToBoolean(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToChar(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToSByte(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToByte(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToInt16(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToUInt16(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToInt32(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToUInt32(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToInt64(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToUInt64(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToSingle(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToDouble(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToDecimal(null); });
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToDateTime(null); });
            Assert.AreEqual("05.01.2013", ((IConvertible)dtItem1).ToString(null));
            Assert.Throws(typeof(NotImplementedException), () => { ((IConvertible)dtItem1).ToType(typeof(Int32), null); });
        }

        [Test]
        public void Test_DateItems()
        {
            var dtx1 = new GEDCOMDateValue(null, null, "DATE", "05 JAN 2013");
            var dtItem1 = new GEDCOMDateItem(dtx1);
            Assert.AreEqual("05.01.2013", dtItem1.ToString());

            var dtx2 = new GEDCOMDateValue(null, null, "DATE", "17 FEB 2013");
            var dtItem2 = new GEDCOMDateItem(dtx2);
            Assert.AreEqual("17.02.2013", dtItem2.ToString());

            Assert.AreEqual(0, dtItem1.CompareTo(dtItem1));
            Assert.AreEqual(-1, dtItem1.CompareTo(dtItem2));
            Assert.AreEqual(-1, dtItem1.CompareTo(null));
            Assert.AreEqual(+1, dtItem2.CompareTo(dtItem1));

            dtItem1 = new GEDCOMDateItem(dtx1);
            dtItem2 = new GEDCOMDateItem(null);
            Assert.AreEqual(-1, dtItem1.CompareTo(dtItem2));

            dtItem1 = new GEDCOMDateItem(null);
            dtItem2 = new GEDCOMDateItem(dtx2);
            Assert.AreEqual(+1, dtItem1.CompareTo(dtItem2));

            dtItem1 = new GEDCOMDateItem(null);
            dtItem2 = new GEDCOMDateItem(null);
            Assert.AreEqual(0, dtItem1.CompareTo(dtItem2));
        }

        private class ListViewMock : IListView
        {
            public void AddColumn(string caption, int width, bool autoSize) {}
        }

        private bool ExtFilterHandler(GEDCOMRecord record)
        {
            return true;
        }

        [Test]
        public void Test_Lists()
        {
            ListColumn colStatic = new ListColumn(0, 0, DataType.dtString, 0, true);
            Assert.IsNotNull(colStatic);
            Assert.AreEqual(0, colStatic.Order);
            Assert.AreEqual(false, colStatic.CurActive);
            Assert.AreEqual(0, colStatic.CurWidth);

            //
            ListFilter listFilter = new ListFilter();
            Assert.IsNotNull(listFilter);
            Assert.AreEqual(0, listFilter.Conditions.Count);
            listFilter.Clear();
            Assert.AreEqual(0, listFilter.Conditions.Count);

            ListManager listManager;
            ListViewMock lvMock = new ListViewMock();
            GKListItem listItem;

            //
            listManager = new GroupListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMGroupRecord grpRec = fContext.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            listManager.Fetch(grpRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*roup*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            listManager.UpdateColumns(lvMock);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, grpRec);

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
        public void Test_LM()
        {
            
        }

        [Test]
        public void Test_LMCommunication()
        {
            var listManager = new CommunicationListMan(fContext);
            Assert.IsNotNull(listManager);

            Assert.IsNotNull(listManager.ContentList);

            listManager.ExternalFilter = null;
            Assert.IsNull(listManager.ExternalFilter);

            GEDCOMCommunicationRecord commRec = fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord;
            listManager.Fetch(commRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*commun*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, commRec);
        }

        [Test]
        public void Test_LMFamily()
        {
            var listManager = new FamilyListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            listManager.Fetch(famRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "* - *";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, famRec);
        }

        [Test]
        public void Test_LMIndividual()
        {
            var listManager = new IndividualListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;
            listManager.Fetch(indRec);

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

            var lvMock = new ListViewMock();

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfFNP;
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, indRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_NP;
            listManager.UpdateColumns(lvMock);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, indRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_N_P;
            listManager.UpdateColumns(lvMock);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, indRec);
        }

        [Test]
        public void Test_LMLocation()
        {
            var listManager = new LocationListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMLocationRecord locRec = fContext.Tree.XRefIndex_Find("L1") as GEDCOMLocationRecord;
            listManager.Fetch(locRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*locat*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, locRec);
        }

        [Test]
        public void Test_LMMultimedia()
        {
            var listManager = new MultimediaListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMMultimediaRecord mediaRec = fContext.Tree.XRefIndex_Find("O1") as GEDCOMMultimediaRecord;
            listManager.Fetch(mediaRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*media*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, mediaRec);
        }

        [Test]
        public void Test_LMNote()
        {
            var listManager = new NoteListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMNoteRecord noteRec = new GEDCOMNoteRecord(null, null, "", "");
            noteRec.AddNoteText("Test text");
            listManager.Fetch(noteRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*text*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, noteRec);
            noteRec.Clear();
            listManager.UpdateItem(listItem, noteRec);
        }

        [Test]
        public void Test_LMRepository()
        {
            var listManager = new RepositoryListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMRepositoryRecord repoRec = fContext.Tree.XRefIndex_Find("R1") as GEDCOMRepositoryRecord;
            listManager.Fetch(repoRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*repos*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, repoRec);
        }

        [Test]
        public void Test_LMResearch()
        {
            var listManager = new ResearchListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMResearchRecord resRec = fContext.Tree.XRefIndex_Find("RS1") as GEDCOMResearchRecord;
            listManager.Fetch(resRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*resear*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, resRec);
        }

        [Test]
        public void Test_LMSource()
        {
            var listManager = new SourceListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            listManager.Fetch(srcRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*sourc*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, srcRec);
        }

        [Test]
        public void Test_LMTask()
        {
            var listManager = new TaskListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMTaskRecord tskRec = fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            listManager.Fetch(tskRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*task*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, tskRec);
        }
    }
}
