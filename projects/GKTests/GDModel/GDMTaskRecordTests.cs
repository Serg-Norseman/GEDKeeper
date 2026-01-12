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
    public class GDMTaskRecordTests
    {
        private readonly BaseContext fContext;

        public GDMTaskRecordTests()
        {
            TestUtils.InitGEDCOMProviderTest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
            Assert.IsNotNull(iRec);

            GDMFamilyRecord famRec = fContext.Tree.FindXRef<GDMFamilyRecord>("F1");
            Assert.IsNotNull(famRec);

            GDMSourceRecord srcRec = fContext.Tree.FindXRef<GDMSourceRecord>("S1");
            Assert.IsNotNull(srcRec);

            using (GDMTaskRecord taskRec = new GDMTaskRecord(fContext.Tree)) {
                Assert.IsNotNull(taskRec);

                taskRec.Priority = GDMResearchPriority.rpNormal;
                Assert.AreEqual(GDMResearchPriority.rpNormal, taskRec.Priority);

                taskRec.StartDate.Date = TestUtils.ParseDT("20.01.2013");
                Assert.AreEqual(TestUtils.ParseDT("20.01.2013"), taskRec.StartDate.Date);

                taskRec.StopDate.Date = TestUtils.ParseDT("21.01.2013");
                Assert.AreEqual(TestUtils.ParseDT("21.01.2013"), taskRec.StopDate.Date);

                taskRec.Goal = "Test Goal";
                Assert.AreEqual("Test Goal", taskRec.Goal);
                var goal = GKUtils.GetTaskGoal(fContext.Tree, taskRec);
                Assert.AreEqual(GDMGoalType.gtOther, goal.GoalType);
                Assert.AreEqual(null, goal.GoalRec);

                taskRec.Goal = iRec.XRef;
                goal = GKUtils.GetTaskGoal(fContext.Tree, taskRec);
                Assert.AreEqual(GDMGoalType.gtIndividual, goal.GoalType);
                Assert.AreEqual(iRec, goal.GoalRec);

                taskRec.Goal = famRec.XRef;
                goal = GKUtils.GetTaskGoal(fContext.Tree, taskRec);
                Assert.AreEqual(GDMGoalType.gtFamily, goal.GoalType);
                Assert.AreEqual(famRec, goal.GoalRec);

                taskRec.Goal = srcRec.XRef;
                goal = GKUtils.GetTaskGoal(fContext.Tree, taskRec);
                Assert.AreEqual(GDMGoalType.gtSource, goal.GoalType);
                Assert.AreEqual(srcRec, goal.GoalRec);

                using (GDMTaskRecord task2 = fContext.Tree.CreateTask()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        task2.Assign(null);
                    });

                    task2.Assign(taskRec);

                    // FIXME: goal format invalid!
                    string buf = GEDCOMProvider.GetTagStreamText(task2, 0);
                    Assert.AreEqual("0 @TK2@ _TASK\r\n" +
                                    "1 _STARTDATE 20 JAN 2013\r\n" +
                                    "1 _STOPDATE 21 JAN 2013\r\n" +
                                    "1 _PRIORITY normal\r\n" +
                                    "1 _GOAL S1\r\n", buf);
                }

                taskRec.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(taskRec.IsEmpty());
                taskRec.Clear();
                Assert.IsTrue(taskRec.IsEmpty());
            }
        }
    }
}
