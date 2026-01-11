// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System.Collections.Generic;
using GDModel;
using GKTests;
using NUnit.Framework;

namespace GKCore.Stats
{
    [TestFixture]
    public class StatsTests
    {
        private readonly BaseContext fContext;

        public StatsTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_CompositeItem()
        {
            CompositeItem compositeItem = new CompositeItem();
            Assert.IsNotNull(compositeItem);
            compositeItem.TakeVal(0.0f, GDMSex.svMale, true);
            Assert.AreEqual(0, compositeItem.CommonVal);
            Assert.AreEqual(0, compositeItem.MaleVal);
            Assert.AreEqual(0, compositeItem.FemaleVal);
            compositeItem.TakeVal(1f, GDMSex.svFemale, true);
            compositeItem.TakeVal(1f, GDMSex.svMale, true);
            Assert.AreEqual(1, compositeItem.CommonVal);
            Assert.AreEqual(1, compositeItem.MaleVal);
            Assert.AreEqual(1, compositeItem.FemaleVal);
            compositeItem.TakeVal("1", GDMSex.svFemale, true);
            compositeItem.TakeVal("1", GDMSex.svMale, true);
            Assert.AreEqual(1, compositeItem.CommonVal);
            Assert.AreEqual(1, compositeItem.MaleVal);
            Assert.AreEqual(1, compositeItem.FemaleVal);
        }

        [Test]
        public void Test_StatsItem()
        {
            StatsItem statsItem = new StatsItem("test", false);
            Assert.IsNotNull(statsItem);
            Assert.AreEqual("test", statsItem.ToString());
            Assert.AreEqual("0", statsItem.GetDisplayString());

            statsItem = new StatsItem("test2", 0);
            Assert.IsNotNull(statsItem);
            Assert.AreEqual("test2", statsItem.ToString());

            statsItem = new StatsItem("test2", true);
            Assert.IsNotNull(statsItem);
            statsItem.ValF = 10;
            statsItem.ValM = 11;
            Assert.AreEqual("10 | 11", statsItem.GetDisplayString());
        }

        [Test]
        public void Test_Stats()
        {
            List<GDMRecord> selectedRecords = new List<GDMRecord>();
            var iEnum = fContext.Tree.GetEnumerator(GDMRecordType.rtIndividual);
            GDMRecord current;
            while (iEnum.MoveNext(out current)) {
                selectedRecords.Add(current);
            }

            TreeStats treeStats = new TreeStats(fContext, selectedRecords);
            Assert.IsNotNull(treeStats);

            CommonStats commonStats = treeStats.GetCommonStats();
            Assert.IsNotNull(commonStats);
            Assert.AreEqual(6, commonStats.persons, "Stats.TotalPersons");
            Assert.AreEqual(2, commonStats.persons_m, "Stats.SumM");
            Assert.AreEqual(4, commonStats.persons_f, "Stats.SumF");

            List<StatsItem> values = new List<StatsItem>();

            treeStats.GetSpecStats(StatsMode.smAncestors, values);
            treeStats.GetSpecStats(StatsMode.smDescendants, values);
            treeStats.GetSpecStats(StatsMode.smDescGenerations, values);
            treeStats.GetSpecStats(StatsMode.smSurnames, values);
            treeStats.GetSpecStats(StatsMode.smNames, values);
            treeStats.GetSpecStats(StatsMode.smPatronymics, values);
            treeStats.GetSpecStats(StatsMode.smAge, values);
            treeStats.GetSpecStats(StatsMode.smLifeExpectancy, values);
            treeStats.GetSpecStats(StatsMode.smBirthYears, values);
            treeStats.GetSpecStats(StatsMode.smBirthTenYears, values);
            treeStats.GetSpecStats(StatsMode.smDeathYears, values);
            treeStats.GetSpecStats(StatsMode.smDeathTenYears, values);
            treeStats.GetSpecStats(StatsMode.smChildsCount, values);
            treeStats.GetSpecStats(StatsMode.smChildsDistribution, values);
            treeStats.GetSpecStats(StatsMode.smBirthPlaces, values);
            treeStats.GetSpecStats(StatsMode.smDeathPlaces, values);
            treeStats.GetSpecStats(StatsMode.smResidences, values);
            treeStats.GetSpecStats(StatsMode.smOccupation, values);
            treeStats.GetSpecStats(StatsMode.smReligious, values);
            treeStats.GetSpecStats(StatsMode.smNational, values);
            treeStats.GetSpecStats(StatsMode.smEducation, values);
            treeStats.GetSpecStats(StatsMode.smCaste, values);
            treeStats.GetSpecStats(StatsMode.smFirstbornAge, values);
            treeStats.GetSpecStats(StatsMode.smMarriages, values);
            treeStats.GetSpecStats(StatsMode.smMarriageAge, values);
            treeStats.GetSpecStats(StatsMode.smSpousesDiff, values);
            treeStats.GetSpecStats(StatsMode.smHobby, values);
            treeStats.GetSpecStats(StatsMode.smAward, values);
            treeStats.GetSpecStats(StatsMode.smMili, values);
            treeStats.GetSpecStats(StatsMode.smMiliInd, values);
            treeStats.GetSpecStats(StatsMode.smMiliDis, values);
            treeStats.GetSpecStats(StatsMode.smMiliRank, values);
            treeStats.GetSpecStats(StatsMode.smAAF_1, values);
            treeStats.GetSpecStats(StatsMode.smAAF_2, values);
            treeStats.GetSpecStats(StatsMode.smCertaintyIndex, values);
            treeStats.GetSpecStats(StatsMode.smBirthByMonth, values);
            treeStats.GetSpecStats(StatsMode.smDemography, values);
        }
    }
}
