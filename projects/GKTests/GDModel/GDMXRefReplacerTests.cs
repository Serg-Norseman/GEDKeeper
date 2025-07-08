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
