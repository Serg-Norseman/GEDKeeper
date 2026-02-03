/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Calendar;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMDatePeriodTests
    {
        public GDMDatePeriodTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_Common()
        {
            using (GDMDatePeriod dtx1 = new GDMDatePeriod()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());

                Assert.AreEqual("", dtx1.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, true)); // date is empty
                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");

                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] - 2013.01.23 [G]", dtx1.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 04 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013 TO 04 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(TestUtils.ParseDT("04.01.2013"), dtx1.Date);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("FROM 04 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(TestUtils.ParseDT("04.01.2013"), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] >", dtx1.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("TO 23 JAN 2013");
                Assert.AreEqual("TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(TestUtils.ParseDT("23.01.2013"), dtx1.Date);
                Assert.AreEqual("< 2013.01.23 [G]", dtx1.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                Assert.Throws(typeof(NotSupportedException), () => {
                    dtx1.SetDateTime(DateTime.Now);
                });

                Assert.AreEqual(string.Empty, dtx1.ParseString((string)null));
            }
        }
    }
}
