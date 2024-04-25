/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using System.Text;
using BSLib;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Names;
using GKCore.Search;
using GKCore.Stats;
using GKCore.Types;
using GKTests;
using NSubstitute;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class CoreTests
    {
        private BaseContext fContext;

        public CoreTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_PG()
        {
            PatriarchObj pObj = new PatriarchObj();
            Assert.IsNotNull(pObj);
            Assert.IsNotNull(pObj.Links);

            PGNode pgNode = new PGNode("label", PGNodeType.Default);
            Assert.IsNotNull(pgNode);

            pgNode = new PGNode("label", PGNodeType.Default, 5);
            Assert.IsNotNull(pgNode);
        }

        private bool TestExternalFilterHandler(GDMRecord record)
        {
            return false;
        }

        [Test]
        public void Test_FiltersIntf()
        {
            FilterCondition cond = new FilterCondition(0, ConditionKind.ck_Contains, null);
            Assert.IsNotNull(cond);

            ExternalFilterHandler handler = TestExternalFilterHandler;
            Assert.IsFalse(handler.Invoke(null));
        }

        private void TweenHandler(int newX, int newY)
        {
        }

        [Test]
        public void Test_Tween()
        {
            #if !MONO
            var tween = new GKCore.Charts.TweenLibrary();
            tween.StartTween(TweenHandler, 0, 0, 10, 10, GKCore.Charts.TweenAnimation.EaseInOutQuad, 20);
            #endif
        }

        [Test]
        public void Test_SysUtils()
        {
            #if MONO
            Assert.IsTrue(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Unix, SysUtils.GetPlatformID());
            Assert.IsFalse(string.IsNullOrEmpty(SysUtils.GetMonoVersion()));
            Assert.AreNotEqual(DesktopType.Windows, SysUtils.GetDesktopType());
            #else
            Assert.IsFalse(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Win32NT, SysUtils.GetPlatformID());
            Assert.IsTrue(string.IsNullOrEmpty(SysUtils.GetMonoVersion()));
            Assert.AreEqual(DesktopType.Windows, SysUtils.GetDesktopType());
            #endif
        }

        [Test]
        public void Test_SysUtils_IsUnicodeEncoding()
        {
            Assert.IsTrue(SysUtils.IsUnicodeEncoding(Encoding.UTF8));
            Assert.IsFalse(SysUtils.IsUnicodeEncoding(Encoding.ASCII));
        }

        [Test]
        public void Test_SysUtils_GetAssemblyAttribute()
        {
            Assembly asm = this.GetType().Assembly;
            var attr1 = SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
            Assert.IsNotNull(attr1);
            Assert.AreEqual("GKTests", attr1.Title);

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(null); });
        }

        [Test]
        public void Test_SysUtils_ImplementsInterface()
        {
            Assert.IsTrue(SysUtils.ImplementsInterface(typeof(LangManager), typeof(ILangMan)));

            Assert.IsFalse(SysUtils.ImplementsInterface(typeof(LangManager), typeof(INamesTable)));
        }

        [Test]
        public void Test_Search()
        {
            SearchResult searchResult = new SearchResult(null);
            Assert.IsNotNull(searchResult);

            Assert.Throws(typeof(ArgumentNullException), () => { new SearchStrategy(null, null); });

            var workWindow = Substitute.For<IWorkWindow>();
            SearchStrategy strat = new SearchStrategy(workWindow, "");
            Assert.IsNotNull(strat);

            IList<ISearchResult> res = strat.FindAll();
            Assert.IsNotNull(res);

            Assert.IsFalse(strat.HasResults());
            Assert.IsNull(strat.FindNext());
            Assert.IsNull(strat.FindPrev());
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
            IGDMTreeEnumerator iEnum = fContext.Tree.GetEnumerator(GDMRecordType.rtIndividual);
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

        [Test]
        public void Test_NavStack()
        {
            var navStack = new NavigationStack<object>();
            {
                Assert.IsNotNull(navStack);
                Assert.AreEqual(null, navStack.Current);
                navStack.Clear();
                Assert.AreEqual(null, navStack.Current);

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
        public void Test_NamesTable()
        {
            var namesTable = new NamesTable();
            Assert.IsNotNull(namesTable);

            NameEntry nameEntry = namesTable.AddName("Ivan");
            Assert.IsNotNull(nameEntry);
            Assert.AreEqual("Ivan", nameEntry.Name);

            nameEntry = namesTable.FindName("Ivan");
            Assert.IsNotNull(nameEntry);

            string pat = namesTable.GetPatronymicByName("Ivan", GDMSex.svMale);
            Assert.IsNull(pat);

            string name = namesTable.GetNameByPatronymic("Ivanovich");
            Assert.AreEqual("", name);

            GDMSex sex = namesTable.GetSexByName("Ivan");
            Assert.AreEqual(GDMSex.svUnknown, sex);

            namesTable.SetName("Ivan", "Ivanovich", GDMSex.svMale);
            namesTable.SetName("Ivan", "Ivanovna", GDMSex.svFemale);

            pat = namesTable.GetPatronymicByName("Ivan", GDMSex.svMale);
            Assert.AreEqual("Ivanovich", pat);

            pat = namesTable.GetPatronymicByName("Ivan", GDMSex.svFemale);
            Assert.AreEqual("Ivanovna", pat);

            name = namesTable.GetNameByPatronymic("Ivanovich");
            Assert.AreEqual("Ivan", name);

            name = namesTable.GetNameByPatronymic("Ivanovna");
            Assert.AreEqual("Ivan", name);

            namesTable.SetNameSex("Maria", GDMSex.svFemale);
            sex = namesTable.GetSexByName("Maria");
            Assert.AreEqual(GDMSex.svFemale, sex);

            namesTable.SetName("", "", GDMSex.svUnknown);
            namesTable.SetNameSex("", GDMSex.svUnknown);

            namesTable.SetName("Anna", "Ivanovna", GDMSex.svFemale);
            sex = namesTable.GetSexByName("Anna");
            Assert.AreEqual(GDMSex.svFemale, sex);

            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);
            namesTable.ImportNames(fContext, iRec);

            namesTable.ImportNames(null, null);

            sex = namesTable.GetSexByName("Anna");
            Assert.AreEqual(GDMSex.svFemale, sex);

            string namesFile = TestUtils.GetTempFilePath("names.txt");
            try {
                namesTable.SaveToFile(namesFile);
                namesTable.LoadFromFile(namesFile);
            } finally {
                TestUtils.RemoveTestFile(namesFile);
            }
        }

        [Test]
        public void Test_Other()
        {
            ComboItem<object> comboItem = new ComboItem<object>("Test", null);
            Assert.IsNotNull(comboItem);
            Assert.AreEqual("Test", comboItem.ToString());

            ModifyEventArgs args = new ModifyEventArgs(RecordAction.raAdd, null);
            Assert.IsNotNull(args);
        }

        [Test]
        public void Test_LuaScripts()
        {
            var script = new ScriptEngine(null);
            script.lua_run("gk_print(\"Hello\")", null);
        }

        [Test]
        public void Test_LangMan()
        {
            LangManager langMan = new LangManager();
            Assert.IsNotNull(langMan);

            Assert.AreEqual("?", langMan.LS(LSID.First));
        }



        [Test]
        [TestCase(true, "Silkin*", "Silkinova")]
        [TestCase(true, "Si*kin", "Silkin")]
        [TestCase(true, "Si?kin", "Silkin")]
        [TestCase(false, "Si*kin", "Sunkin")]
        [TestCase(true, "Iv*nov", "Ivanov")]
        [TestCase(true, "Iv*nov|Sid*v", "Sidorov")]
        [TestCase(true, "Sid*v|Pe*ov|Iv*nov", "Ivanov")]
        [TestCase(true, "Sid*v|Iv*nov", "Sidorov")]
        [Parallelizable(ParallelScope.All)]
        public void Test_PatternMatcher(bool value, string expression, string name)
        {
            Assert.AreEqual(value, SysUtils.MatchPattern(expression, name));
        }
    }
}
