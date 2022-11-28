/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMMultimediaLinkTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_GEDCOMMultimediaLink()
        {
            using (GDMMultimediaLink mmLink = new GDMMultimediaLink()) {
                Assert.IsNotNull(mmLink);
                Assert.IsTrue(mmLink.IsEmpty());

                // extensions
                Assert.IsFalse(mmLink.IsPrimaryCutout);
                mmLink.IsPrimaryCutout = true;
                Assert.IsTrue(mmLink.IsPrimaryCutout);

                mmLink.CutoutPosition.Value = ExtRect.Create(10, 15, 500, 600);
                ExtRect rt = mmLink.CutoutPosition.Value;
                Assert.AreEqual(10, rt.Left);
                Assert.AreEqual(15, rt.Top);
                Assert.AreEqual(500, rt.Right);
                Assert.AreEqual(600, rt.Bottom);

                Assert.AreEqual(10, mmLink.CutoutPosition.X1);
                Assert.AreEqual(15, mmLink.CutoutPosition.Y1);
                Assert.AreEqual(500, mmLink.CutoutPosition.X2);
                Assert.AreEqual(600, mmLink.CutoutPosition.Y2);

                mmLink.CutoutPosition.X1 = 10;
                mmLink.CutoutPosition.Y1 = 10;
                mmLink.CutoutPosition.X2 = 300;
                mmLink.CutoutPosition.Y2 = 400;
                Assert.AreEqual(10, mmLink.CutoutPosition.X1);
                Assert.AreEqual(10, mmLink.CutoutPosition.Y1);
                Assert.AreEqual(300, mmLink.CutoutPosition.X2);
                Assert.AreEqual(400, mmLink.CutoutPosition.Y2);

                mmLink.CutoutPosition.ParseString("11 15 576 611");
                Assert.IsFalse(mmLink.CutoutPosition.IsEmpty());
                Assert.AreEqual("11 15 576 611", mmLink.CutoutPosition.StringValue);

                Assert.Throws(typeof(ArgumentException), () => {
                    mmLink.Assign(null);
                });

                using (var mmRec = new GDMMultimediaRecord(fContext.Tree)) {
                    fContext.Tree.NewXRef(mmRec);
                    Assert.IsNull(mmLink.GetUID(fContext.Tree));

                    mmLink.XRef = mmRec.XRef;

                    Assert.IsNotNull(mmLink.GetUID(fContext.Tree));
                }

                mmLink.CutoutPosition.Clear();
                Assert.IsTrue(mmLink.CutoutPosition.IsEmpty());
                Assert.AreEqual("", mmLink.CutoutPosition.StringValue);
            }
        }
    }
}
