/*
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
using GKTests;
using NUnit.Framework;

namespace GKCore.Kinships
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

            KinshipsGraph.InitDefaults();
        }

        [Test]
        public void Test_KinshipsMan()
        {
            int g = 0, deg = 0;
            var finRel = KinshipsGraph.FindKinship((int)KinshipType.ktNone, (int)KinshipType.ktFather, (int)KinshipType.ktSon, ref g, ref deg);
            Assert.AreEqual((int)KinshipType.ktBrother, finRel);

            finRel = KinshipsGraph.FindKinship((int)KinshipType.ktNone, (int)KinshipType.ktNone, (int)KinshipType.ktSon, ref g, ref deg);
            Assert.AreEqual((int)KinshipType.ktSon, finRel);
        }

        [Test]
        public void Test_KinshipsGraph()
        {
            GDMIndividualRecord indRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
            GDMIndividualRecord chldRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            GDMIndividualRecord otherRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I4");
            GDMIndividualRecord wifeRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I2");
            GDMIndividualRecord rec5 = fContext.Tree.FindXRef<GDMIndividualRecord>("I5");

            Assert.Throws(typeof(ArgumentNullException), () => { KinshipsGraph.SearchGraph(fContext, null); });

            using (KinshipsGraph kinsGraph = KinshipsGraph.SearchGraph(fContext, indRec)) {
                Assert.IsNull(kinsGraph.AddIndividual(null));

                Assert.IsNotNull(kinsGraph.FindIndividual(chldRec.XRef));

                // check invalid args
                kinsGraph.SetTreeRoot(null);
                kinsGraph.SetTreeRoot(otherRec);

                // valid individual
                kinsGraph.SetTreeRoot(indRec);

                Assert.AreEqual("???", kinsGraph.DetermineKinship(null));
                Assert.AreEqual("???", kinsGraph.DetermineKinship(otherRec));

                string result = kinsGraph.DetermineKinship(chldRec);
                Assert.AreEqual("daughter", result);

                result = kinsGraph.DetermineKinship(wifeRec);
                Assert.AreEqual("wife", result);

                result = kinsGraph.DetermineKinship(rec5);
                Assert.AreEqual("granddaughter", result);

                Assert.IsFalse(kinsGraph.IsEmpty());
                kinsGraph.Clear();
                Assert.IsTrue(kinsGraph.IsEmpty());
            }
        }
    }
}
