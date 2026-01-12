/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMDateValueTests
    {
        public GDMDateValueTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_Common()
        {
            // check empty dateval match
            using (GDMDateValue dtx1 = new GDMDateValue()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                using (GDMDateValue dtx2 = new GDMDateValue()) {
                    Assert.IsNotNull(dtx2, "dtx1 != null");

                    Assert.AreEqual(0.0f, dtx1.IsMatch(dtx2, new MatchParams()));
                }
            }

            using (GDMDateValue dtx1 = new GDMDateValue()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true)); // value is empty

                dtx1.ParseString("20 JAN 2013");
                Assert.AreEqual("2013.01.20 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));

                Assert.AreEqual(string.Empty, dtx1.ParseString((string)null));
            }

            using (GDMDateValue dtx1 = new GDMDateValue()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                dtx1.ParseString("20 JAN 2013");

                DateTime dt = TestUtils.ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");

                dtx1.ParseString("1716/"); // potentially incorrect value
                Assert.AreEqual("1716", dtx1.StringValue);

                dtx1.ParseString("1716/17");
                Assert.AreEqual("1716/17", dtx1.StringValue);

                dtx1.ParseString("1716/20");
                Assert.AreEqual("1716/20", dtx1.StringValue);

                dtx1.ParseString("3 MAY 1835/36");
                Assert.AreEqual("03 MAY 1835/36", dtx1.StringValue);

                dtx1.ParseString("ABT 1844/45");
                Assert.AreEqual("ABT 1844/45", dtx1.StringValue);

                dtx1.ParseString("FEB 1746/47");
                Assert.AreEqual("FEB 1746/47", dtx1.StringValue);

                dtx1.ParseString("INT 20 JAN 2013 (today)");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
                Assert.AreEqual("today", (dtx1.Value as GDMDateInterpreted).DatePhrase);

                (dtx1.Value as GDMDateInterpreted).DatePhrase = "now";
                Assert.AreEqual("INT 20 JAN 2013 (now)", dtx1.StringValue);

                (dtx1.Value as GDMDateInterpreted).DatePhrase = "(yesterday)";
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                dtx1.ParseString("INT 20 JAN 2013 (yesterday)");
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                string st;

                st = "ABT 20 JAN 2013";
                dtx1.ParseString(st);
                Assert.IsTrue(dtx1.Date.Equals(dt));
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(GDMApproximated.daAbout, ((GDMDate)dtx1.Value).Approximated);
                
                st = "CAL 20 JAN 2013";
                dtx1.ParseString(st);
                Assert.AreEqual(dtx1.Date, dt);
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(GDMApproximated.daCalculated, ((GDMDate)dtx1.Value).Approximated);
                
                st = "EST 20 DEC 2013";
                dtx1.ParseString(st);
                Assert.AreEqual(dtx1.Date, TestUtils.ParseDT("20.12.2013"));
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(GDMApproximated.daEstimated, ((GDMDate)dtx1.Value).Approximated);

                ((GDMDate)dtx1.Value).Approximated = GDMApproximated.daCalculated;
                Assert.AreEqual("CAL 20 DEC 2013", dtx1.StringValue);

                ((GDMDate)dtx1.Value).Approximated = GDMApproximated.daExact;
                Assert.AreEqual("20 DEC 2013", dtx1.StringValue);

                using (GDMDateValue dtx2 = new GDMDateValue()) {
                    dtx2.ParseString("19 JAN 2013");
                    int res = dtx1.CompareTo(dtx2);
                    Assert.AreEqual(1, res);
                }
                
                int res1 = dtx1.CompareTo(null);
                Assert.AreEqual(-1, res1);

                //
                
                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual("04 JAN 2013", (dtx1.Value as GDMDatePeriod).DateFrom.StringValue);
                Assert.AreEqual("23 JAN 2013", (dtx1.Value as GDMDatePeriod).DateTo.StringValue);
                dtx1.Clear();
                Assert.IsTrue(dtx1.IsEmpty());

                dtx1.ParseString("BEF 20 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual(TestUtils.ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);

                dtx1.ParseString("AFT 20 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual(TestUtils.ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);

                dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
                Assert.AreEqual("04 JAN 2013", (dtx1.Value as GDMDateRange).After.StringValue);
                Assert.AreEqual("25 JAN 2013", (dtx1.Value as GDMDateRange).Before.StringValue);
                dtx1.Clear();
                Assert.IsTrue(dtx1.IsEmpty());
            }
        }

        [Test]
        public void Test_NonStdMonthes()
        {
            using (GDMDateValue dtx1 = new GDMDateValue()) {
                // std
                dtx1.ParseString("20 FEB 1746");
                Assert.AreEqual("20 FEB 1746", dtx1.StringValue);

                // Russian short
                dtx1.ParseString("20 ФЕВ 1746");
                Assert.AreEqual("20 FEB 1746", dtx1.StringValue);

                dtx1.ParseString("20 фев 1746");
                Assert.AreEqual("20 FEB 1746", dtx1.StringValue);

                // Dutch short
                dtx1.ParseString("20 MEI 1746");
                Assert.AreEqual("20 MAY 1746", dtx1.StringValue);

                // French short
                dtx1.ParseString("20 AOÛ 1746");
                Assert.AreEqual("20 AUG 1746", dtx1.StringValue);

                // German short
                dtx1.ParseString("20 MÄR 1746");
                Assert.AreEqual("20 MAR 1746", dtx1.StringValue);

                // Spanish short
                dtx1.ParseString("20 ENE 1746");
                Assert.AreEqual("20 JAN 1746", dtx1.StringValue);
            }
        }
    }
}
