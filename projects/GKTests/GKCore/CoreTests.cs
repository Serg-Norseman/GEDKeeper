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
using System.Collections.Generic;
using System.Reflection;
using System.Text;

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Cultures;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Stats;
using GKCore.Tools;
using GKCore.Types;
using GKTests;
using GKTests.Mocks;
using GKUI;
using GKUI.Components;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class CoreTests
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

        private bool TestExternalFilterHandler(GEDCOMRecord record)
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
            #if !__MonoCS__
            TweenLibrary tween = new TweenLibrary();
            tween.StartTween(TweenHandler, 0, 0, 10, 10, TweenAnimation.EaseInOutQuad, 20);
            #endif
        }

        [Test]
        public void Test_SysUtils()
        {
            #if __MonoCS__
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

            //

            Assert.IsTrue(SysUtils.IsUnicodeEncoding(Encoding.UTF8));
            Assert.IsFalse(SysUtils.IsUnicodeEncoding(Encoding.ASCII));

            //

            Assembly asm = this.GetType().Assembly;
            var attr1 = SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
            Assert.IsNotNull(attr1);
            Assert.AreEqual("GKTests", attr1.Title);

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(null); });
        }

        private void TransactionEventHandler(object sender, TransactionType type)
        {
        }

        private class InvalidOperation : CustomOperation
        {
            public InvalidOperation(UndoManager manager) : base(manager) { }
            public override bool Redo() { return false; }
            public override void Undo() {}
        }

        [Test]
        public void Test_Undoman()
        {
            using (UndoManager undoman = new UndoManager()) {
                Assert.IsNotNull(undoman);

                Assert.IsFalse(undoman.CanUndo());
                Assert.IsFalse(undoman.CanRedo());

                undoman.Clear();

                Assert.IsFalse(undoman.DoOperation(null));

                undoman.Undo();
                undoman.Redo();
                undoman.Commit();
                undoman.Rollback();

                Assert.IsFalse(undoman.DoOperation(new InvalidOperation(undoman)));
            }

            Assert.AreEqual(fContext.Tree, fContext.Undoman.Tree);

            fContext.Undoman.OnTransaction += TransactionEventHandler;

            fContext.Undoman.Clear();

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.Throws(typeof(ArgumentNullException), () => {
                              fContext.Undoman.DoOrdinaryOperation(
                                  OperationType.otIndividualBookmarkChange, null, true); });

            Assert.Throws(typeof(ArgumentNullException), () => {
                              fContext.Undoman.DoOrdinaryOperation(
                                  OperationType.otIndividualBookmarkChange, iRec, null); });

            iRec.Bookmark = false;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            Assert.IsTrue(iRec.Bookmark);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            iRec.Patriarch = false;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            Assert.IsTrue(iRec.Patriarch);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Patriarch);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            iRec.Sex = GEDCOMSex.svUndetermined;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GEDCOMSex.svMale);
            Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            Assert.IsTrue(fContext.Undoman.CanRedo());
            fContext.Undoman.Redo();
            Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());

            fContext.Undoman.Clear();

            iRec.Bookmark = false;
            iRec.Patriarch = false;
            iRec.Sex = GEDCOMSex.svUndetermined;

            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GEDCOMSex.svMale);
            fContext.Undoman.Commit();
            Assert.IsTrue(iRec.Bookmark);
            Assert.IsTrue(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());


            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GEDCOMSex.svMale);
            fContext.Undoman.Rollback();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            fContext.Undoman.OnTransaction -= TransactionEventHandler;


            Assert.Throws(typeof(ArgumentNullException), () => {
                              fContext.Undoman.DoIndividualNameChange(null, "", "", ""); });
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetNameString(iRec, true, false));
            fContext.Undoman.DoIndividualNameChange(iRec, "Petrov", "Alex", "Ivanovich");
            Assert.AreEqual("Petrov Alex Ivanovich", GKUtils.GetNameString(iRec, true, false));
            fContext.Undoman.Rollback();
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetNameString(iRec, true, false));
        }

        [Test]
        public void Test_Search()
        {
            SearchResult searchResult = new SearchResult(null);
            Assert.IsNotNull(searchResult);

            Assert.Throws(typeof(ArgumentNullException), () => { new SearchStrategy(null, null); });

            SearchStrategy strat = new SearchStrategy(new WorkWindowMock(), "");
            Assert.IsNotNull(strat);

            IList<ISearchResult> res = strat.FindAll();
            Assert.IsNotNull(res);

            Assert.IsFalse(strat.HasResults());
            Assert.IsNull(strat.FindNext());
            Assert.IsNull(strat.FindPrev());
        }

        [Test]
        public void Test_Cultures()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            ICulture culture = new RussianCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            //
            string[] surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });


            culture = new AncientCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));


            culture = new IcelandCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));


            culture = new BritishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));


            culture = new SwedishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));

            //

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.German;
            Assert.IsInstanceOf(typeof(GermanCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Polish;
            Assert.IsInstanceOf(typeof(PolishCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Swedish;
            Assert.IsInstanceOf(typeof(SwedishCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Icelandic;
            Assert.IsInstanceOf(typeof(IcelandCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Russian;
            Assert.IsInstanceOf(typeof(RussianCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Ukrainian;
            Assert.IsInstanceOf(typeof(RussianCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Armenian;
            Assert.IsInstanceOf(typeof(ArmenianCulture), fContext.Culture);
            Assert.IsTrue(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Turkish;
            Assert.IsInstanceOf(typeof(TurkishCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.French;
            Assert.IsInstanceOf(typeof(FrenchCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Italian;
            Assert.IsInstanceOf(typeof(ItalianCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Cantonese;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Mandrin;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());
        }

        [Test]
        public void Test_RussianCulture()
        {
            RussianCulture rusCulture = new RussianCulture();

            //Assert.AreEqual("", GKUtils.GetMaidenSurname(null));
            //Assert.AreEqual("", GKUtils.GetMaidenSurname(""));
            //Assert.AreEqual("Иванова", GKUtils.GetMaidenSurname("Иванова (Петрова)"));

            Assert.AreEqual("Иванов", rusCulture.NormalizeSurname("Иванова", true));
            Assert.AreEqual("Бельский", rusCulture.NormalizeSurname("Бельская", true));
            Assert.AreEqual("Грозный", rusCulture.NormalizeSurname("Грозная", true));
            Assert.AreEqual("Иванов", rusCulture.NormalizeSurname("Иванов", false));
            Assert.AreEqual("Бельский", rusCulture.NormalizeSurname("Бельский", false));
            Assert.AreEqual("Грозный", rusCulture.NormalizeSurname("Грозный", false));

            Assert.AreEqual("?", rusCulture.NormalizeSurname(null, false));
            Assert.AreEqual("?", rusCulture.NormalizeSurname("", false));
            Assert.AreEqual("?", rusCulture.NormalizeSurname("(Иванова)", false));

            Assert.AreEqual("Иванова", rusCulture.GetMarriedSurname("Иванов"));
            Assert.AreEqual("Бельская", rusCulture.GetMarriedSurname("Бельский"));
            Assert.AreEqual("Грозная", rusCulture.GetMarriedSurname("Грозный"));

            Assert.AreEqual("?", rusCulture.GetMarriedSurname(""));
            Assert.AreEqual("?", rusCulture.GetMarriedSurname(null));

            string[] snms = rusCulture.GetSurnames("Бельская (Иванова)", true);
            Assert.AreEqual(2, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);
            Assert.AreEqual("Иванов", snms[1]);

            snms = rusCulture.GetSurnames("Бельская", true);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            snms = rusCulture.GetSurnames("Бельский", false);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            //

            GEDCOMSex sx = rusCulture.GetSex("Мария", "Петровна", false);
            Assert.AreEqual(GEDCOMSex.svFemale, sx);

            sx = rusCulture.GetSex("Иван", "Петрович", false);
            Assert.AreEqual(GEDCOMSex.svMale, sx);

            Assert.AreEqual(GEDCOMSex.svNone, rusCulture.GetSex("", "", false));

            Assert.AreEqual("Иванова Ивана Ивановича", rusCulture.GetPossessiveName("Иванов Иван Иванович"));
        }

        [Test]
        public void Test_Kinships()
        {
            int g, lev;
            var finRel = KinshipsMan.FindKinship(RelationKind.rkFather, RelationKind.rkSon, out g, out lev);
            Assert.AreEqual(RelationKind.rkBrother, finRel);

            finRel = KinshipsMan.FindKinship(RelationKind.rkNone, RelationKind.rkSon, out g, out lev);
            Assert.AreEqual(RelationKind.rkSon, finRel);

            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord chldRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord otherRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.SearchKinshipsGraph(fContext, null); });

            using (KinshipsGraph kinsGraph = TreeTools.SearchKinshipsGraph(fContext, indRec)) {
                Assert.IsNull(kinsGraph.AddIndividual(null));

                Assert.IsNotNull(kinsGraph.FindVertex(chldRec.XRef));

                // check invalid args
                kinsGraph.SetTreeRoot(null);
                kinsGraph.SetTreeRoot(otherRec);

                // valid individual
                kinsGraph.SetTreeRoot(indRec);

                Assert.AreEqual("???", kinsGraph.GetRelationship(null));
                Assert.AreEqual("???", kinsGraph.GetRelationship(otherRec));

                string result = kinsGraph.GetRelationship(chldRec);
                Assert.AreEqual("daughter", result);

                Assert.IsFalse(kinsGraph.IsEmpty());
                kinsGraph.Clear();
                Assert.IsTrue(kinsGraph.IsEmpty());
            }
        }

        [Test]
        public void Test_DateItems()
        {
            var dtx1 = new GEDCOMDateValue(null, null, "DATE", "05 JAN 2013");
            var dtItem1 = new GEDCOMDateItem(dtx1);

            var dtx2 = new GEDCOMDateValue(null, null, "DATE", "17 FEB 2013");
            var dtItem2 = new GEDCOMDateItem(dtx2);

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

        [Test]
        public void Test_Locales()
        {
            LangRecord langRecord = new LangRecord(1049, "rus", "Russian", "filename.lng");
            Assert.IsNotNull(langRecord);
            Assert.AreEqual(1049, langRecord.Code);
            Assert.AreEqual("rus", langRecord.Sign);
            Assert.AreEqual("Russian", langRecord.Name);
            Assert.AreEqual("filename.lng", langRecord.FileName);
        }

        [Test]
        public void Test_Options()
        {
            using (IniFile iniFile = new IniFile()) {
                GlobalOptions globalOptions = GlobalOptions.Instance;
                Assert.IsNotNull(globalOptions);

                Assert.IsNotNull(globalOptions.ChartOptions);
                Assert.IsNotNull(globalOptions.AncestorsCircleOptions);

                /*globalOptions.DefCharacterSet = GEDCOMCharacterSet.csUNICODE;
                Assert.AreEqual(GEDCOMCharacterSet.csUNICODE, globalOptions.DefCharacterSet);*/
                Assert.AreEqual(GEDCOMCharacterSet.csUTF8, globalOptions.DefCharacterSet);

                globalOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
                Assert.AreEqual(DateFormat.dfDD_MM_YYYY, globalOptions.DefDateFormat);

                globalOptions.ShowDatesSign = true;
                Assert.AreEqual(true, globalOptions.ShowDatesSign);

                globalOptions.DefNameFormat = NameFormat.nfF_N_P;
                Assert.AreEqual(NameFormat.nfF_N_P, globalOptions.DefNameFormat);

                Assert.IsNotNull(globalOptions.EventFilters);

                globalOptions.InterfaceLang = 1000;
                Assert.AreEqual(1000, globalOptions.InterfaceLang);

                globalOptions.LastDir = "c:\\";
                Assert.AreEqual("c:\\", globalOptions.LastDir);

                Assert.IsNotNull(globalOptions.MRUFiles);

                globalOptions.MWinRect = ExtRect.CreateEmpty();
                Assert.IsTrue(globalOptions.MWinRect.IsEmpty());

                globalOptions.MWinState = WindowState.Maximized;
                Assert.AreEqual(WindowState.Maximized, globalOptions.MWinState);

                Assert.IsNotNull(globalOptions.NameFilters);

                Assert.IsNotNull(globalOptions.PedigreeOptions);

                globalOptions.PlacesWithAddress = true;
                Assert.AreEqual(true, globalOptions.PlacesWithAddress);

                Assert.IsNotNull(globalOptions.Proxy);

                Assert.IsNotNull(globalOptions.Relations);

                Assert.IsNotNull(globalOptions.ResidenceFilters);

                globalOptions.FileBackup = FileBackup.fbOnlyPrev;
                Assert.AreEqual(FileBackup.fbOnlyPrev, globalOptions.FileBackup);

                globalOptions.ShowTips = true;
                Assert.AreEqual(true, globalOptions.ShowTips);

                globalOptions.ListHighlightUnmarriedPersons = true;
                Assert.AreEqual(true, globalOptions.ListHighlightUnmarriedPersons);

                globalOptions.ListHighlightUnparentedPersons = true;
                Assert.AreEqual(true, globalOptions.ListHighlightUnparentedPersons);

                Assert.IsNotNull(globalOptions.IndividualListColumns);

                globalOptions.ShowDatesCalendar = true;
                Assert.AreEqual(true, globalOptions.ShowDatesCalendar);

                globalOptions.Autosave = true;
                Assert.AreEqual(true, globalOptions.Autosave);

                globalOptions.AutosaveInterval = 10;
                Assert.AreEqual(10, globalOptions.AutosaveInterval);

                globalOptions.ExtendedNames = true;
                Assert.AreEqual(true, globalOptions.ExtendedNames);

                globalOptions.UseExtendedNotes = true;
                Assert.AreEqual(true, globalOptions.UseExtendedNotes);

                globalOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
                Assert.AreEqual(WomanSurnameFormat.wsfMaiden, globalOptions.WomanSurnameFormat);

                globalOptions.AddLastBase("sample.ged");
                Assert.AreEqual(1, globalOptions.GetLastBasesCount());
                Assert.AreEqual("sample.ged", globalOptions.GetLastBase(0));
                globalOptions.ClearLastBases();

                Assert.IsNotNull(globalOptions.Languages);


                globalOptions.SaveToFile(iniFile);
                globalOptions.LoadFromFile(iniFile);
                //Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.SaveToFile(null); });
                //Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.LoadFromFile(null); });


                MRUFile mruFile = new MRUFile();
                Assert.IsNotNull(mruFile);

                mruFile = new MRUFile("test.ged");
                Assert.IsNotNull(mruFile);
                Assert.AreEqual(-1, globalOptions.MRUFiles_IndexOf("test.ged"));
                globalOptions.MRUFiles.Add(mruFile);
                Assert.AreEqual(0, globalOptions.MRUFiles_IndexOf("test.ged"));


                mruFile.SaveToFile(iniFile, "xxx");
                mruFile.LoadFromFile(iniFile, "xxx");
                MRUFile.DeleteKeys(iniFile, "xxx");
                Assert.Throws(typeof(ArgumentNullException), () => { mruFile.SaveToFile(null, "xxx"); });
                Assert.Throws(typeof(ArgumentNullException), () => { mruFile.LoadFromFile(null, "xxx"); });
                Assert.Throws(typeof(ArgumentNullException), () => { MRUFile.DeleteKeys(null, "xxx"); });

                AncestorsCircleOptions circleOptions = new AncestorsCircleOptions();
                Assert.IsNotNull(circleOptions);
                circleOptions.Assign(null);
                circleOptions.Assign(new AncestorsCircleOptions());
                circleOptions.SaveToFile(iniFile);
                circleOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { circleOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { circleOptions.LoadFromFile(null); });

                PedigreeOptions pedigreeOptions = new PedigreeOptions();
                Assert.IsNotNull(pedigreeOptions);
                pedigreeOptions.Assign(null);
                pedigreeOptions.Assign(new PedigreeOptions());
                pedigreeOptions.SaveToFile(iniFile);
                pedigreeOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { pedigreeOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { pedigreeOptions.LoadFromFile(null); });

                ProxyOptions proxyOptions = new ProxyOptions();
                Assert.IsNotNull(proxyOptions);
                proxyOptions.Assign(null);
                proxyOptions.Assign(new ProxyOptions());
                proxyOptions.SaveToFile(iniFile);
                proxyOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { proxyOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { proxyOptions.LoadFromFile(null); });

                TreeChartOptions treeChartOptions = new TreeChartOptions();
                Assert.IsNotNull(treeChartOptions);
                treeChartOptions.Assign(null);
                treeChartOptions.Assign(new TreeChartOptions());
                treeChartOptions.SaveToFile(iniFile);
                treeChartOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { treeChartOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { treeChartOptions.LoadFromFile(null); });
            }
        }

        [Test]
        public void Test_Stats()
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

            TreeStats treeStats = new TreeStats(fContext, selectedRecords);
            Assert.IsNotNull(treeStats);

            CommonStats commonStats = treeStats.GetCommonStats();
            Assert.IsNotNull(commonStats);
            Assert.AreEqual(5, commonStats.persons, "Stats.TotalPersons");
            Assert.AreEqual(2, commonStats.persons_m, "Stats.SumM");
            Assert.AreEqual(3, commonStats.persons_f, "Stats.SumF");

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
            using (var navStack = new NavigationStack<object>())
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
        public void Test_NamesTable()
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

                namesTable.SetName("", "", GEDCOMSex.svNone);
                namesTable.SetNameSex("", GEDCOMSex.svNone);

                namesTable.SetName("Anna", "Ivanovna", GEDCOMSex.svFemale);
                sex = namesTable.GetSexByName("Anna");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);

                GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
                Assert.IsNotNull(iRec);
                namesTable.ImportNames(iRec);

                namesTable.ImportNames(null);

                sex = namesTable.GetSexByName("Anna");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);
            }
        }

        [Test]
        public void Test_UIControls()
        {
            GKComboItem comboItem = new GKComboItem("Test", null);
            Assert.IsNotNull(comboItem);
            Assert.AreEqual("Test", comboItem.ToString());

            GKListItem listItem = new GKListItem("Test", null);
            Assert.IsNotNull(listItem);
            Assert.AreEqual("Test", listItem.ToString());

            GKListSubItem listSubItem = new GKListSubItem("Test");
            Assert.IsNotNull(listSubItem);

            GKToolStripMenuItem tsMenuItem = new GKToolStripMenuItem("Test", null);
            Assert.IsNotNull(tsMenuItem);

            GKTreeNode treeNode = new GKTreeNode("Test", null);
            Assert.IsNotNull(treeNode);

            ModifyEventArgs args = new ModifyEventArgs(RecordAction.raAdd, null);
            Assert.IsNotNull(args);
        }

        [Test]
        public void Test_PedigreeExporter()
        {
            BaseWindowMock baseWin = new BaseWindowMock();
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new PedigreeExporter(null, null); });

            using (PedigreeExporter exporter = new PedigreeExporter(baseWin, iRec)) {
                exporter.Options = GlobalOptions.Instance;
                Assert.IsNotNull(exporter.Options);

                Assert.AreEqual(iRec, exporter.Root);
                Assert.AreEqual(ShieldState.None, exporter.ShieldState);

                exporter.Options.PedigreeOptions.IncludeAttributes = true;
                exporter.Options.PedigreeOptions.IncludeNotes = true;
                exporter.Options.PedigreeOptions.IncludeSources = true;
                exporter.Options.PedigreeOptions.IncludeGenerations = true;

                exporter.Kind = PedigreeExporter.PedigreeKind.pkDescend_Konovalov;
                Assert.AreEqual(PedigreeExporter.PedigreeKind.pkDescend_Konovalov, exporter.Kind);

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new MockWriter()));

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Compact;
                Assert.IsTrue(exporter.Generate(new MockWriter()));


                exporter.Kind = PedigreeExporter.PedigreeKind.pkDescend_dAboville;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new MockWriter()));


                exporter.Kind = PedigreeExporter.PedigreeKind.pkAscend;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new MockWriter()));
            }
        }

        [Test]
        public void Test_ExcelExporter()
        {
            BaseWindowMock baseWin = new BaseWindowMock();
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new ExcelExporter(null); });

            using (ExcelExporter exporter = new ExcelExporter(baseWin)) {

            }
        }

        [Test]
        public void Test_FamilyBookExporter()
        {
            BaseWindowMock baseWin = new BaseWindowMock();
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new FamilyBookExporter(null); });

            using (FamilyBookExporter exporter = new FamilyBookExporter(baseWin)) {

            }
        }

        private void EWriter_Test(CustomWriter writer)
        {
        }

        [Test]
        public void Test_Writers()
        {
            using (HTMLWriter writer = new HTMLWriter()) {
                EWriter_Test(writer);
            }

            using (RTFWriter writer = new RTFWriter()) {
                EWriter_Test(writer);
            }

            /*using (PDFWriter writer = new PDFWriter()) {
                // TravisCI crash
            }*/
        }

        [Test]
        public void Test_LuaScripts()
        {
            using (ScriptEngine script = new ScriptEngine()) {
                script.lua_run("gk_print(\"Hello\")", null, null);
            }
        }

        [Test]
        public void Test_LangMan()
        {
            LangManager langMan = new LangManager();
            Assert.IsNotNull(langMan);

            Assert.AreEqual("?", langMan.LS(LSID.LSID_First));
        }
    }
}
