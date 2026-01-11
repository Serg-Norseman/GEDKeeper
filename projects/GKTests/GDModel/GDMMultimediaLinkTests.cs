// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

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
        private readonly BaseContext fContext;

        public GDMMultimediaLinkTests()
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
