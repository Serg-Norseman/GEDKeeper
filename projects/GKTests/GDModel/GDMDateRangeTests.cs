// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMDateRangeTests
    {
        public GDMDateRangeTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_Common()
        {
            using (var dtx1 = new GDMDateRange()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());
                Assert.AreEqual("", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true)); // date is empty
                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] - 2013.01.25 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("BEF 20 JAN 2013");
                Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(TestUtils.ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("< 2013.01.20 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("AFT 20 JAN 2013");
                Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(TestUtils.ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("2013.01.20 [G] >", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                Assert.Throws(typeof(NotSupportedException), () => {
                    dtx1.SetDateTime(DateTime.Now);
                });

                Assert.Throws(typeof(GEDCOMRangeDateException), () => {
                    dtx1.ParseString("BET 04 JAN 2013 X 25 JAN 2013");
                });

                Assert.AreEqual(string.Empty, dtx1.ParseString((string)null));
            }
        }
    }
}
