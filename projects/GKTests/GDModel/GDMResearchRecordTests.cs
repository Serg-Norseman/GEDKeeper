/*
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
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMResearchRecordTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_GEDCOMResearchRecord()
        {
            GDMCommunicationRecord commRec = fContext.Tree.CreateCommunication();
            Assert.IsNotNull(commRec);

            GDMTaskRecord taskRec = fContext.Tree.CreateTask();
            Assert.IsNotNull(taskRec);

            GDMGroupRecord groupRec = fContext.Tree.CreateGroup();
            Assert.IsNotNull(groupRec);

            using (GDMResearchRecord resRec = fContext.Tree.CreateResearch()) {
                resRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, resRec.Owner);

                Assert.IsNotNull(resRec.Communications);
                Assert.IsNotNull(resRec.Groups);
                Assert.IsNotNull(resRec.Tasks);

                resRec.ResearchName = "Test Research";
                Assert.AreEqual("Test Research", resRec.ResearchName);

                resRec.Priority = GDMResearchPriority.rpNormal;
                Assert.AreEqual(GDMResearchPriority.rpNormal, resRec.Priority);

                resRec.Status = GDMResearchStatus.rsOnHold;
                Assert.AreEqual(GDMResearchStatus.rsOnHold, resRec.Status);

                resRec.StartDate.Date = TestUtils.ParseDT("20.01.2013");
                Assert.AreEqual(TestUtils.ParseDT("20.01.2013"), resRec.StartDate.Date);

                resRec.StopDate.Date = TestUtils.ParseDT("21.01.2013");
                Assert.AreEqual(TestUtils.ParseDT("21.01.2013"), resRec.StopDate.Date);

                resRec.Percent = 33;
                Assert.AreEqual(33, resRec.Percent);

                Assert.Throws(typeof(ArgumentException), () => {
                    resRec.Assign(null);
                });

                string buf = TestUtils.GetTagStreamText(resRec, 0);
                Assert.AreEqual("0 @RS2@ _RESEARCH\r\n" +
                                "1 _STARTDATE 20 JAN 2013\r\n" +
                                "1 _STOPDATE 21 JAN 2013\r\n" +
                                "1 NAME Test Research\r\n" +
                                "1 _PRIORITY normal\r\n" +
                                "1 _STATUS onhold\r\n" +
                                "1 _PERCENT 33\r\n", buf);

                Assert.AreEqual(-1, resRec.IndexOfCommunication(null));
                resRec.AddCommunication(commRec);
                resRec.RemoveCommunication(commRec);
                resRec.RemoveCommunication(null);

                Assert.AreEqual(-1, resRec.IndexOfTask(null));
                resRec.AddTask(taskRec);
                resRec.RemoveTask(taskRec);
                resRec.RemoveTask(null);

                Assert.AreEqual(-1, resRec.IndexOfGroup(null));
                resRec.AddGroup(groupRec);
                resRec.RemoveGroup(groupRec);
                resRec.RemoveGroup(null);

                resRec.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(resRec.IsEmpty());
                resRec.Clear();
                Assert.IsTrue(resRec.IsEmpty());
            }
        }
    }
}
