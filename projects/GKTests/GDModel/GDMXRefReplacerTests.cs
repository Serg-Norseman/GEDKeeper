// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMXRefReplacerTests
    {
        private readonly BaseContext fContext;

        public GDMXRefReplacerTests()
        {
            TestUtils.InitGEDCOMProviderTest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_XRefReplacer()
        {
            using (GDMXRefReplacer replacer = new GDMXRefReplacer()) {
                Assert.IsNotNull(replacer);

                GDMIndividualRecord iRec = fContext.CreatePersonEx("ivan", "ivanovich", "ivanov", GDMSex.svMale, false);
                replacer.AddXRef(iRec, "I210", iRec.XRef);

                string newXRef = replacer.FindNewXRef("I210");
                Assert.AreEqual(iRec.XRef, newXRef);

                newXRef = replacer.FindNewXRef("I310");
                Assert.AreEqual("I310", newXRef);

                for (int i = 0; i < replacer.Count; i++) {
                    GDMXRefReplacer.XRefEntry xre = replacer[i];
                    Assert.AreEqual(iRec, xre.Rec);
                }
            }
        }
    }
}
