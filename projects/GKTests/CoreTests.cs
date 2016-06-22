/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.IO;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Lists;
using GKCore.Maps;
using GKCore.Options;
using GKCore.Stats;
using GKCore.Tools;
using GKCore.Types;
using NUnit.Framework;

namespace GKTests
{
    [TestFixture]
    public class CoreTests
    {
        BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            Console.WriteLine(@">>> START CoreTests");

            fContext = TestStubs.CreateContext();
            TestStubs.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
            Console.WriteLine(@">>> END CoreTests");
        }

        [Test]
        public void Context_Tests()
        {
            GEDCOMSourceRecord srcRec = fContext.FindSource("test source");
            Assert.IsNull(srcRec);

            StringList sources = new StringList();
            fContext.GetSourcesList(sources);
            Assert.AreEqual(0, sources.Count);

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.AreEqual(false, fContext.IsChildless(iRec));

            Assert.AreEqual(1990, fContext.GetRelativeYear(iRec, "BIRT"));

            Assert.AreEqual(1990, fContext.FindBirthYear(iRec));
            Assert.AreEqual(2010, fContext.FindDeathYear(iRec));

            //fContext.Clear();
            //Assert.AreEqual(0, fContext.Tree.RecordsCount);
        }
        
        [Test]
        public void Utils_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            // individual record tests
            Assert.IsNotNull(iRec);

            string age = GKUtils.GetAgeStr(null, 0);
            Assert.AreEqual("", age);

            age = GKUtils.GetAgeStr(iRec, -1);
            Assert.AreEqual("20", age);
            
            age = GKUtils.GetAgeStr(iRec, 2005);
            Assert.AreEqual("15", age);
            
            //
            
            //
            
            string st3;
            st3 = GKUtils.GetBirthDate(null, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("28.12.1990", st3);

            st3 = GKUtils.GetDeathDate(null, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("", st3);
            
            st3 = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("28.12.2010", st3);
            
            st3 = GKUtils.GetLifeStr(iRec);
            Assert.AreEqual(" (28.12.1990 - 28.12.2010)", st3);
            
            //
            
            st3 = GKUtils.GetBirthPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
            
            st3 = GKUtils.GetDeathPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetDeathPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
            
            st3 = GKUtils.GetResidencePlace(null, false);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetResidencePlace(iRec, true);
            Assert.AreEqual("", st3);
            
            //
            
            GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
            Assert.IsNotNull(evt);
            
            string st2 = GKUtils.GEDCOMEventToDateStr(null, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("", st2);
            
            st2 = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("1990.12.28", st2);
            
            //st2 = GKUtils.GetEventName(evt);
            //Assert.AreEqual("BIRT", st2);
            
            //
            
            string st1 = GKUtils.GetAttributeValue(null, "BIRT");
            Assert.AreEqual("", st1);
            
            st1 = GKUtils.GetAttributeValue(iRec, "BIRT");
            Assert.AreEqual("", st1);
            
            Assert.AreEqual(1, GKUtils.GetPersonEventIndex("BIRT"));
            Assert.AreEqual(2, GKUtils.GetFamilyEventIndex("MARR"));
            Assert.AreEqual(1, GKUtils.GetMarriageStatusIndex("MARRIED"));
            
            //
            
            //Assert.AreEqual(1990, context.FindBirthYear(iRec));
            //Assert.AreEqual(-1, context.FindDeathYear(iRec));
            
            string ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
            Assert.AreEqual("28.12.1990", ds);
            
            ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("1990.12.28", ds);
            
            ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY, false);
            Assert.AreEqual("1990", ds);
            
            ds = GKUtils.GEDCOMEventToDateStr(null, DateFormat.dfYYYY, false);
            Assert.AreEqual("", ds);
            
            evt = fContext.CreateEventEx(iRec, "FACT", "17 JAN 2013", "Ivanovo");
            Assert.IsNotNull(evt);
            GedcomTests.GEDCOMCustomEventTest(evt, "17.01.2013");
            
            string dst = GKUtils.CompactDate("__.__.2013");
            Assert.AreEqual("2013", dst);
            
            GEDCOMFamilyRecord fRec = fContext.Tree.CreateFamily();
            Assert.IsNotNull(fRec);
            
            evt = fContext.CreateEventEx(fRec, "MARR", "28 DEC 2013", "Ivanovo");
            Assert.IsNotNull(evt);
            GedcomTests.GEDCOMCustomEventTest(evt, "28.12.2013");
            
            // sex tests
            GEDCOMSex sex;
            sex = GKUtils.GetSexBySign('F');
            Assert.AreEqual(GEDCOMSex.svFemale, sex);
            sex = GKUtils.GetSexBySign('M');
            Assert.AreEqual(GEDCOMSex.svMale, sex);
            sex = GKUtils.GetSexBySign('U');
            Assert.AreEqual(GEDCOMSex.svUndetermined, sex);
            
            // path tests
            string st = GKUtils.GetTempDir();
            Assert.IsTrue(Directory.Exists(st));
            
            st = GKUtils.GetAppPath();
            Assert.IsTrue(Directory.Exists(st));
            
            // matches tests
            bool res = GKUtils.MatchesMask("abrakadabra", "*kad*");
            Assert.IsTrue(res);
            
            res = GKUtils.MatchesMask("abrakadabra", "*test*");
            Assert.IsFalse(res);

            GEDCOMListTests(iRec);

            //

            long val = GKUtils.Trunc(495.575);
            Assert.AreEqual(val, 495);

            Assert.AreEqual(3.0f, GKUtils.SafeDiv(9.0f, 3.0f));
            Assert.AreEqual(0.0f, GKUtils.SafeDiv(9.0f, 0.0f));
            
            // access tests
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.None));
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.None));
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.None));

            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.Middle));
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.Middle));
            Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.Middle));

            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.Maximum));
            Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.Maximum));
            Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.Maximum));
            
            st1 = GKUtils.HyperLink("@X001@", "test", 0);
            Assert.AreEqual("~^" + "@X001@" + ":" + "test" + "~", st1);
        }
        
        private const int REP_COUNT = 1000; // 1000000; // for profile tests

        private static void GEDCOMListTests(GEDCOMIndividualRecord iRec)
        {
            //GEDCOMListTest_Hot(iRec);
            
            for (int k = 0; k < REP_COUNT; k++) {
                GEDCOMListTest11(iRec);
                GEDCOMListTest12(iRec);
                GEDCOMListTest21(iRec);
                GEDCOMListTest22(iRec);
                GEDCOMListTest23(iRec);
                GEDCOMListTest3(iRec);
            }
        }

        /*private void GEDCOMListTest_Hot(GEDCOMIndividualRecord iRec)
		{
			for (int k = 0; k < 100000; k++) {
				GEDCOMListTest11(iRec);
				GEDCOMListTest12(iRec);
				GEDCOMListTest21(iRec);
				GEDCOMListTest22(iRec);
				GEDCOMListTest23(iRec);
				GEDCOMListTest3(iRec);
			}
		}*/

        private static void GEDCOMListTest11(GEDCOMIndividualRecord iRec)
        {
            foreach (GEDCOMCustomEvent evt1 in iRec.Events) {
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest12(GEDCOMIndividualRecord iRec)
        {
            IGEDCOMListEnumerator enumer = iRec.Events.GetEnumerator();
            while (enumer.MoveNext()) {
                GEDCOMCustomEvent evt1 = (GEDCOMCustomEvent)enumer.Current;
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest21(GEDCOMIndividualRecord iRec)
        {
            for (int i = 0; i < iRec.Events.Count; i++) {
                GEDCOMCustomEvent evt1 = iRec.Events[i];
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest22(GEDCOMIndividualRecord iRec)
        {
            for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                GEDCOMCustomEvent evt1 = iRec.Events[i];
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest23(GEDCOMIndividualRecord iRec)
        {
            GEDCOMList<GEDCOMCustomEvent> events = iRec.Events;
            for (int i = 0, num = events.Count; i < num; i++) {
                GEDCOMCustomEvent evt1 = events[i];
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest3(GEDCOMIndividualRecord iRec)
        {
            iRec.Events.ForEach(x => { x.GetHashCode(); });
        }

        [Test]
        public void RussianCulture_Tests()
        {
            Assert.AreEqual("", GKUtils.ClearSurname(null));
            Assert.AreEqual("", GKUtils.ClearSurname(""));
            Assert.AreEqual("Иванова", GKUtils.ClearSurname("Иванова (Петрова)"));
            Assert.AreEqual("Иванов", RussianCulture.PrepareRusSurname("Иванова", true));
            Assert.AreEqual("Бельский", RussianCulture.PrepareRusSurname("Бельская", true));
            Assert.AreEqual("Грозный", RussianCulture.PrepareRusSurname("Грозная", true));
            Assert.AreEqual("Иванов", RussianCulture.PrepareRusSurname("Иванов", false));
            Assert.AreEqual("Бельский", RussianCulture.PrepareRusSurname("Бельский", false));
            Assert.AreEqual("Грозный", RussianCulture.PrepareRusSurname("Грозный", false));

            Assert.AreEqual("?", RussianCulture.PrepareRusSurname(null, false));
            Assert.AreEqual("?", RussianCulture.PrepareRusSurname("", false));
            Assert.AreEqual("?", RussianCulture.PrepareRusSurname("(Иванова)", false));

            Assert.AreEqual("Иванова", RussianCulture.GetRusWifeSurname("Иванов"));
            Assert.AreEqual("Бельская", RussianCulture.GetRusWifeSurname("Бельский"));
            Assert.AreEqual("Грозная", RussianCulture.GetRusWifeSurname("Грозный"));

            Assert.AreEqual("?", RussianCulture.GetRusWifeSurname(""));
            Assert.AreEqual("?", RussianCulture.GetRusWifeSurname(null));

            string[] snms = GKUtils.GetSurnames("Бельская (Иванова)", true);
            Assert.AreEqual(2, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);
            Assert.AreEqual("Иванов", snms[1]);

            snms = GKUtils.GetSurnames("Бельская", true);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            snms = GKUtils.GetSurnames("Бельский", false);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            //

            GEDCOMSex sx = RussianCulture.GetSex("Мария", "Петровна", false);
            Assert.AreEqual(GEDCOMSex.svFemale, sx);

            sx = RussianCulture.GetSex("Иван", "Петрович", false);
            Assert.AreEqual(GEDCOMSex.svMale, sx);

            Assert.AreEqual(GEDCOMSex.svNone, RussianCulture.GetSex("", "", false));
        }

        [Test]
        public void Kinships_Tests()
        {
            RelationKind rel;
            int great, level;

            rel = KinshipsMan.FindKinship(RelationKind.rkFather, RelationKind.rkSon, out great, out level);
            Assert.AreEqual(RelationKind.rkBrother, rel);

            rel = KinshipsMan.FindKinship(RelationKind.rkNone, RelationKind.rkSon, out great, out level);
            Assert.AreEqual(RelationKind.rkSon, rel);
        }

        [Test]
        public void Lists_Tests()
        {
            ColumnProps colProps = new ColumnProps();
            Assert.IsNotNull(colProps);

            colProps = new ColumnProps(0, false, 10);
            Assert.IsNotNull(colProps);

            Assert.AreEqual(0, colProps.ColType);
            Assert.AreEqual(false, colProps.ColActive);
            Assert.AreEqual(10, colProps.ColWidth);

            colProps.Assign(null);

            ColumnStatic colStatic = new ColumnStatic();
            Assert.IsNotNull(colStatic);

            //ListColumns listColumns = new ListColumns();
            //Assert.IsNotNull(listColumns);

            ListFilter listFilter = new ListFilter();
            Assert.IsNotNull(listFilter);
            Assert.AreEqual(0, listFilter.Conditions.Count);
            listFilter.Clear();
            Assert.AreEqual(0, listFilter.Conditions.Count);

            ListManager listManager;

            listManager = new CommunicationListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(CommunicationColumnType), listManager.GetColumnsEnum());

            listManager = new FamilyListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(FamilyColumnType), listManager.GetColumnsEnum());

            listManager = new GroupListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(GroupColumnType), listManager.GetColumnsEnum());

            //

            listManager = new IndividualListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(PersonColumnType), listManager.GetColumnsEnum());
            
            IListFilter filter = listManager.Filter;
            IListColumns listColumns = listManager.ListColumns;

            //

            listManager = new LocationListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(LocationColumnType), listManager.GetColumnsEnum());

            listManager = new MultimediaListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(MultimediaColumnType), listManager.GetColumnsEnum());

            listManager = new NoteListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(NoteColumnType), listManager.GetColumnsEnum());

            listManager = new RepositoryListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(RepositoryColumnType), listManager.GetColumnsEnum());

            listManager = new ResearchListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(ResearchColumnType), listManager.GetColumnsEnum());

            listManager = new SourceListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(SourceColumnType), listManager.GetColumnsEnum());

            listManager = new TaskListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(TaskColumnType), listManager.GetColumnsEnum());
        }

        [Test]
        public void Maps_Tests()
        {
            GMapPoint mapPoint = new GMapPoint(0.5f, 0.5f, "test");
            Assert.IsNotNull(mapPoint);
            Assert.AreEqual(0.5f, mapPoint.Latitude);
            Assert.AreEqual(0.5f, mapPoint.Longitude);
            Assert.AreEqual("test", mapPoint.Hint);

        }

        [Test]
        public void Locales_Tests()
        {
            LangRecord langRecord = new LangRecord(1049, "rus", "Russian", "filename.lng");
            Assert.IsNotNull(langRecord);
            Assert.AreEqual(1049, langRecord.Code);
            Assert.AreEqual("rus", langRecord.Sign);
            Assert.AreEqual("Russian", langRecord.Name);
            Assert.AreEqual("filename.lng", langRecord.FileName);
        }

        [Test]
        public void Options_Tests()
        {
            //GlobalOptions globalOptions = new GlobalOptions();
            //Assert.IsNotNull(globalOptions);

            MRUFile mruFile = new MRUFile();
            Assert.IsNotNull(mruFile);

            PedigreeOptions pedigreeOptions = new PedigreeOptions();
            Assert.IsNotNull(pedigreeOptions);

            ProxyOptions proxyOptions = new ProxyOptions();
            Assert.IsNotNull(proxyOptions);

            TreeChartOptions treeChartOptions = new TreeChartOptions();
            Assert.IsNotNull(treeChartOptions);
        }

        [Test]
        public void Stats_Tests()
        {
            CompositeItem compositeItem = new CompositeItem();
            Assert.IsNotNull(compositeItem);
            compositeItem.TakeVal(0.0f, GEDCOMSex.svMale, true);
            Assert.AreEqual(0, compositeItem.CommonVal);
            Assert.AreEqual(0, compositeItem.MaleVal);
            Assert.AreEqual(0, compositeItem.FemaleVal);
            compositeItem.TakeVal(1f, GEDCOMSex.svFemale, true);
            compositeItem.TakeVal(1f, GEDCOMSex.svMale, true);
            Assert.AreEqual(1, compositeItem.CommonVal);
            Assert.AreEqual(1, compositeItem.MaleVal);
            Assert.AreEqual(1, compositeItem.FemaleVal);
            compositeItem.TakeVal("1", GEDCOMSex.svFemale, true);
            compositeItem.TakeVal("1", GEDCOMSex.svMale, true);
            Assert.AreEqual(1, compositeItem.CommonVal);
            Assert.AreEqual(1, compositeItem.MaleVal);
            Assert.AreEqual(1, compositeItem.FemaleVal);

            StatsItem statsItem = new StatsItem("test", false);
            Assert.IsNotNull(statsItem);
            Assert.AreEqual("test", statsItem.ToString());

            statsItem = new StatsItem("test2", 0);
            Assert.IsNotNull(statsItem);
            Assert.AreEqual("test2", statsItem.ToString());

            List<GEDCOMRecord> selectedRecords = new List<GEDCOMRecord>();
            IGEDCOMTreeEnumerator iEnum = fContext.Tree.GetEnumerator(GEDCOMRecordType.rtIndividual);
            GEDCOMRecord current;
            while (iEnum.MoveNext(out current)) {
                selectedRecords.Add(current);
            }

            TreeStats treeStats = new TreeStats(fContext.Tree, selectedRecords);
            Assert.IsNotNull(treeStats);

            CommonStats commonStats = treeStats.GetCommonStats();
            Assert.IsNotNull(commonStats);
            Assert.AreEqual(4, commonStats.persons, "Stats.TotalPersons");
            Assert.AreEqual(2, commonStats.persons_m, "Stats.SumM");
            Assert.AreEqual(2, commonStats.persons_f, "Stats.SumF");

            List<StatsItem> values = new List<StatsItem>();

            treeStats.GetSpecStats(StatsMode.smAncestors, values);
            treeStats.GetSpecStats(StatsMode.smDescendants, values);
            treeStats.GetSpecStats(StatsMode.smDescGenerations, values);
            treeStats.GetSpecStats(StatsMode.smFamilies, values);
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

        [Test]
        public void Tools_Tests()
        {
            PlaceObj placeObj = new PlaceObj();
            Assert.IsNotNull(placeObj);
            Assert.AreEqual(null, placeObj.Name);
            Assert.IsNotNull(placeObj.Facts);

            placeObj.Dispose();
            Assert.IsNotNull(placeObj);

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            List<GEDCOMRecord> walkList = new List<GEDCOMRecord>();
            TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, walkList);
            Assert.AreEqual(3, walkList.Count, "TreeTools.TreeWalk(twmAll)"); // 3 linked from 4 total

            //ValuesCollection valuesCollection = new ValuesCollection();
            //ProgressMock progress = new ProgressMock();
            //TreeTools.CheckGEDCOMFormat(fContext.Tree, valuesCollection, progress);
        }

        private class ProgressMock : IProgressController
        {
            public void ProgressInit(string title, int max) {}
            public void ProgressDone() {}
            public void ProgressStep() {}
            public void ProgressStep(int value) {}
        }
        
        [Test]
        public void Search_Tests()
        {
            SearchResult searchResult = new SearchResult(null);
            Assert.IsNotNull(searchResult);

            //BaseSearchStrategy searchStrategy = new BaseSearchStrategy(null, "");
            //Assert.IsNotNull(searchStrategy);
        }

        [Test]
        public void NavStack_Tests()
        {
            using (NavigationStack navStack = new NavigationStack())
            {
                Assert.IsNotNull(navStack);
                Assert.AreEqual(false, navStack.Busy);
                Assert.AreEqual(null, navStack.Current);
                navStack.Clear();
                Assert.AreEqual(null, navStack.Current);

                navStack.BeginNav();
                Assert.AreEqual(true, navStack.Busy);
                navStack.EndNav();
                Assert.AreEqual(false, navStack.Busy);

                Assert.AreEqual(false, navStack.CanBackward());
                Assert.AreEqual(false, navStack.CanForward());

                object test = new object();
                object test2 = new object();

                navStack.Current = test;
                navStack.Current = test2;
                
                Assert.AreEqual(test, navStack.Back());
                Assert.AreEqual(test2, navStack.Next());
            }
        }

        [Test]
        public void NamesTable_Tests()
        {
            using (NamesTable namesTable = new NamesTable())
            {
                Assert.IsNotNull(namesTable);
                
                NameEntry nameEntry = namesTable.AddName("Ivan");
                Assert.IsNotNull(nameEntry);
                Assert.AreEqual("Ivan", nameEntry.Name);

                nameEntry = namesTable.FindName("Ivan");
                Assert.IsNotNull(nameEntry);

                string pat = namesTable.GetPatronymicByName("Ivan", GEDCOMSex.svMale);
                Assert.IsNull(pat);

                string name = namesTable.GetNameByPatronymic("Ivanovich");
                Assert.AreEqual("", name);

                GEDCOMSex sex = namesTable.GetSexByName("Ivan");
                Assert.AreEqual(GEDCOMSex.svNone, sex);
                
                namesTable.SetName("Ivan", "Ivanovich", GEDCOMSex.svMale);
                namesTable.SetName("Ivan", "Ivanovna", GEDCOMSex.svFemale);

                pat = namesTable.GetPatronymicByName("Ivan", GEDCOMSex.svMale);
                Assert.AreEqual("Ivanovich", pat);

                pat = namesTable.GetPatronymicByName("Ivan", GEDCOMSex.svFemale);
                Assert.AreEqual("Ivanovna", pat);

                name = namesTable.GetNameByPatronymic("Ivanovich");
                Assert.AreEqual("Ivan", name);

                name = namesTable.GetNameByPatronymic("Ivanovna");
                Assert.AreEqual("Ivan", name);

                namesTable.SetNameSex("Maria", GEDCOMSex.svFemale);
                sex = namesTable.GetSexByName("Maria");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);

                GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
                Assert.IsNotNull(iRec);
                namesTable.ImportNames(iRec);

                sex = namesTable.GetSexByName("Anna");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);
            }
        }
    }
}
