﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Kinships;
using GKTests;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class KinshipsTests
    {
        private BaseContext fContext;

        public KinshipsTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_KinshipsMan()
        {
            int g, deg;
            var finRel = KinshipSolver.FindKinship((int)KinshipType.ktNone, (int)KinshipType.ktFather, (int)KinshipType.ktSon, out g, out deg);
            Assert.AreEqual(KinshipType.ktBrother, finRel);

            finRel = KinshipSolver.FindKinship((int)KinshipType.ktNone, (int)KinshipType.ktNone, (int)KinshipType.ktSon, out g, out deg);
            Assert.AreEqual(KinshipType.ktSon, finRel);
        }

        [Test]
        public void Test_KinshipsGraph()
        {
            GDMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            GDMIndividualRecord chldRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            GDMIndividualRecord otherRec = fContext.Tree.XRefIndex_Find("I4") as GDMIndividualRecord;
            GDMIndividualRecord wifeRec = fContext.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            GDMIndividualRecord rec5 = fContext.Tree.XRefIndex_Find("I5") as GDMIndividualRecord;

            Assert.Throws(typeof(ArgumentNullException), () => { KinshipSolver.SearchGraph(fContext, null); });

            using (KinshipSolver kinsGraph = KinshipSolver.SearchGraph(fContext, indRec)) {
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
