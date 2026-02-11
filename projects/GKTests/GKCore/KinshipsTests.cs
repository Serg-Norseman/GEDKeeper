/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        private readonly BaseContext fContext;

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
