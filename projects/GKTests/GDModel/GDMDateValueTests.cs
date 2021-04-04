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
using GDModel;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMDateValueTests
    {
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

                Assert.AreEqual(string.Empty, dtx1.ParseString(null));
            }

            using (GDMDateValue dtx1 = new GDMDateValue()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                dtx1.ParseString("20 JAN 2013");

                DateTime dt = TestUtils.ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");

                dtx1.ParseString("1716/"); // potentially incorrect value
                Assert.AreEqual("1716", dtx1.StringValue);

                dtx1.ParseString("1716/1717");
                Assert.AreEqual("1716/1717", dtx1.StringValue);

                dtx1.ParseString("1716/20");
                Assert.AreEqual("1716/20", dtx1.StringValue);

                dtx1.ParseString("3 MAY 1835/1838");
                Assert.AreEqual("03 MAY 1835/1838", dtx1.StringValue);

                dtx1.ParseString("ABT 1844/1845");
                Assert.AreEqual("ABT 1844/1845", dtx1.StringValue);

                dtx1.ParseString("FEB 1746/1747");
                Assert.AreEqual("FEB 1746/1747", dtx1.StringValue);

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
    }
}
