/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMResearchRecordTests
    {
        private readonly BaseContext fContext;

        public GDMResearchRecordTests()
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
                resRec.ResetTree(fContext.Tree);

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

                string buf = GEDCOMProvider.GetTagStreamText(resRec, 0);
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
