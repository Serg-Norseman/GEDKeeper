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
using GKCore.Kinships;
using GKCore.Types;
using GKTests;
using GKUI;
using GKUI.Providers;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class KinshipsTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);

            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        [Test]
        public void Test_KinshipsMan()
        {
            int g, lev;
            var finRel = KinshipsMan.FindKinship(RelationKind.rkFather, RelationKind.rkSon, out g, out lev);
            Assert.AreEqual(RelationKind.rkBrother, finRel);

            finRel = KinshipsMan.FindKinship(RelationKind.rkNone, RelationKind.rkSon, out g, out lev);
            Assert.AreEqual(RelationKind.rkSon, finRel);
        }

        [Test]
        public void Test_KinshipsGraph()
        {
            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord chldRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord otherRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord wifeRec = fContext.Tree.XRefIndex_Find("I2") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord rec5 = fContext.Tree.XRefIndex_Find("I5") as GEDCOMIndividualRecord;

            Assert.Throws(typeof(ArgumentNullException), () => { KinshipsGraph.SearchGraph(fContext, null); });

            using (KinshipsGraph kinsGraph = KinshipsGraph.SearchGraph(fContext, indRec)) {
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

                result = kinsGraph.GetRelationship(wifeRec);
                Assert.AreEqual("wife", result);

                result = kinsGraph.GetRelationship(rec5);
                Assert.AreEqual("granddaughter", result);

                Assert.IsFalse(kinsGraph.IsEmpty());
                kinsGraph.Clear();
                Assert.IsTrue(kinsGraph.IsEmpty());
            }
        }
    }
}
