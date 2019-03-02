/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKTests;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GEDCOMDateTests
    {
        [Test]
        public void TestMethod()
        {
        }

        [Test]
        public void Test_DateInterpreted()
        {
            DateTime dt = TestUtils.ParseDT("20.01.2013");

            using (var dtx1 = new GEDCOMDateInterpreted(null, null, "", "")) {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                dtx1.ParseString("INT 20 JAN 2013 (today)");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
                Assert.AreEqual("today", dtx1.DatePhrase);

                dtx1.DatePhrase = "now";
                Assert.AreEqual("INT 20 JAN 2013 (now)", dtx1.StringValue);

                dtx1.DatePhrase = "(yesterday)";
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                dtx1.ParseString("INT 20 JAN 2013 (yesterday)");
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.ParseString("10 JAN 2013 (today)"); });
            }
        }

        [Test]
        public void Test_CreateByFormattedStr()
        {
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr(null, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMDate.CreateByFormattedStr("20/12/1980", false).StringValue);
            Assert.AreEqual("DEC 1980", GEDCOMDate.CreateByFormattedStr("__/12/1980", false).StringValue);
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr("1980", false));
            
            Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.CreateByFormattedStr("1980", true); });
        }
    }
}
