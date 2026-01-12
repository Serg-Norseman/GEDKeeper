/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMTimeTests
    {
        [Test]
        public void Test_Common()
        {
            var tms = new TimeSpan(20, 20, 20);

            using (GDMTime time = new GDMTime()) {
                Assert.IsNotNull(time, "time != null");
                Assert.AreEqual(GEDCOMTagType.TIME, time.GetTagType());

                time.Value = tms;
                Assert.AreEqual(tms, time.Value);

                time.ParseString("20:20:20.100");
                Assert.AreEqual(20, time.Hour);
                Assert.AreEqual(20, time.Minutes);
                Assert.AreEqual(20, time.Seconds);
                Assert.AreEqual(100, time.Fraction);

                time.Fraction = 200;
                Assert.AreEqual(200, time.Fraction);

                Assert.AreEqual("20:20:20.200", time.StringValue);
                Assert.IsFalse(time.IsEmpty());
                time.Clear();
                Assert.IsTrue(time.IsEmpty());
                Assert.AreEqual("", time.StringValue);
            }
        }
    }
}
