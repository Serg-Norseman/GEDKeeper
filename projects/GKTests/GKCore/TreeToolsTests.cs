/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Reflection;

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.Tools;
using GKTests;
using GKTests.Stubs;
using GKUI;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class TreeToolsTests
    {
        private IBaseWindow fBaseWin;
        private ProgressStub fProgress;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);

            LangMan.DefInit();

            fBaseWin = new BaseWindowStub();

            AppHost.Container.Register<IProgressController, ProgressStub>(LifeCycle.Singleton, true);
            fProgress = new ProgressStub();
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }


        [Test]
        public void Test_MergeTree()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.MergeTree(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.MergeTree(fBaseWin.Context.Tree, null, null); });

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.MergeTreeFile(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.MergeTreeFile(fBaseWin.Context.Tree, null, null); });
        }

        [Test]
        public void Test_SearchTreeFragments_MergeTree()
        {
            List<List<GEDCOMRecord>> treeFragments;
            Assembly assembly = typeof(CoreTests).Assembly;

            using (var ctx1 = new BaseContext(null)) {
                IBaseWindow baseWin = new BaseWindowStub(ctx1);

                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test1.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx1.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);
                }

                treeFragments = TreeTools.SearchTreeFragments(ctx1.Tree, null);
                Assert.AreEqual(2, treeFragments.Count);
                Assert.AreEqual(13, treeFragments[0].Count);
                Assert.AreEqual(1, treeFragments[1].Count);

                using (var ctx2 = new BaseContext(null)) {
                    using (Stream stmGed2 = assembly.GetManifestResourceStream("GKTests.Resources.test2.ged")) {
                        var gedcomProvider = new GEDCOMProvider(ctx2.Tree);
                        gedcomProvider.LoadFromStreamExt(stmGed2, stmGed2);
                    }

                    treeFragments = TreeTools.SearchTreeFragments(ctx2.Tree, null);
                    Assert.AreEqual(2, treeFragments.Count);
                    Assert.AreEqual(15, treeFragments[0].Count);
                    Assert.AreEqual(1, treeFragments[1].Count);

                    TreeTools.MergeTree(ctx1.Tree, ctx2.Tree, null);

                    treeFragments = TreeTools.SearchTreeFragments(ctx1.Tree, null);
                    Assert.AreEqual(4, treeFragments.Count);
                    Assert.AreEqual(13, treeFragments[0].Count);
                    Assert.AreEqual(1, treeFragments[1].Count);
                    Assert.AreEqual(15, treeFragments[2].Count);
                    Assert.AreEqual(1, treeFragments[3].Count);

                    GEDCOMIndividualRecord iRec1 = ctx1.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    GEDCOMIndividualRecord iRec2 = ctx1.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec2);

                    TreeTools.MergeRecord(baseWin, iRec1, iRec2, true);

                    treeFragments = TreeTools.SearchTreeFragments(ctx1.Tree, null);
                    Assert.AreEqual(3, treeFragments.Count);
                    Assert.AreEqual(13, treeFragments[0].Count);
                    Assert.AreEqual(15, treeFragments[1].Count);
                    Assert.AreEqual(1, treeFragments[2].Count);
                }
            }
        }

        [Test]
        public void Test_PlaceObj()
        {
            PlaceObj placeObj = new PlaceObj(null);
            Assert.IsNotNull(placeObj);
            Assert.AreEqual(null, placeObj.Name);
            Assert.IsNotNull(placeObj.Facts);

            placeObj.Dispose();
            Assert.IsNotNull(placeObj);
        }

        [Test]
        public void Test_WalkTree()
        {
            GEDCOMIndividualRecord iRec = fBaseWin.Context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            List<GEDCOMRecord> walkList = new List<GEDCOMRecord>();
            TreeTools.WalkTree(iRec, TreeTools.TreeWalkMode.twmAll, walkList);
            Assert.AreEqual(5, walkList.Count, "TreeTools.TreeWalk(twmAll)"); // 3 linked from 4 total

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.WalkTree(null, TreeTools.TreeWalkMode.twmAll, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.WalkTree(iRec, TreeTools.TreeWalkMode.twmAll, null); });
        }

        [Test]
        public void Test_GenPatriarchsGraphviz()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GenPatriarchsGraphviz(null, "", 0, false); });

            string filename = TestUtils.GetTempFilePath("test.gvf");
            TreeTools.GenPatriarchsGraphviz(fBaseWin, filename, 0, false);
        }

        [Test]
        public void Test_CheckGEDCOMFormat()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(fBaseWin.Context.Tree, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(fBaseWin.Context.Tree, fBaseWin.Context, null); });
            TreeTools.CheckGEDCOMFormat(fBaseWin.Context.Tree, fBaseWin.Context, fProgress);
        }

        [Test]
        public void Test_CheckBaseAndRepairProblem()
        {
            List<TreeTools.CheckObj> checksList = new List<TreeTools.CheckObj>();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckBase(null, checksList); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckBase(fBaseWin, null); });

            TreeTools.CheckBase(fBaseWin, checksList);
            Assert.AreEqual(3, checksList.Count);

            Assert.AreEqual(TreeTools.CheckDiag.cdStrangeSpouse, checksList[0].Diag);
            Assert.AreEqual(TreeTools.CheckDiag.cdPersonLonglived, checksList[2].Diag);

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.RepairProblem(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.RepairProblem(fBaseWin, null); });

            TreeTools.RepairProblem(fBaseWin, checksList[2]);
        }

        [Test]
        public void Test_CheckRelations()
        {
            GEDCOMIndividualRecord iRec = fBaseWin.Context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckRelations(null); });

            List<GEDCOMRecord> splitList = new List<GEDCOMRecord>();
            splitList.Add(iRec);
            TreeTools.CheckRelations(splitList);
        }

        [Test]
        public void Test_GetUnlinkedNamesakes()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GetUnlinkedNamesakes(null); });
            List<TreeTools.ULIndividual> uln = TreeTools.GetUnlinkedNamesakes(fBaseWin);
        }

        [Test]
        public void Test_FindDuplicates()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(null, null, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fBaseWin.Context.Tree, null, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(null, fBaseWin.Context.Tree, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fBaseWin.Context.Tree, fBaseWin.Context.Tree, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fBaseWin.Context.Tree, fBaseWin.Context.Tree, 0.0f, null, fProgress); });
        }

        [Test]
        public void Test_CompareTree()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CompareTree(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CompareTree(fBaseWin.Context, null, null); });
        }

        [Test]
        public void Test_SearchPlaces()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.SearchPlaces_Clear(null); });

            StringList placesList = new StringList();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.SearchPlaces(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.SearchPlaces(fBaseWin.Context.Tree, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.SearchPlaces(fBaseWin.Context.Tree, placesList, null); });

            TreeTools.SearchPlaces(fBaseWin.Context.Tree, placesList, AppHost.Progress);
            Assert.IsTrue(placesList.IndexOf("Ivanovo") >= 0); // <- TestStubs
            Assert.IsTrue(placesList.IndexOf("unknown") >= 0); // <- TestStubs
            Assert.IsTrue(placesList.IndexOf("Far Forest") >= 0); // <- TestStubs
        }

        [Test]
        public void Test_X()
        {
            ValuesCollection valuesCollection = new ValuesCollection();
        }
    }
}
