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
using BSLib.Calendar;
using GDModel;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMDateRangeTests
    {
        [Test]
        public void Test_Common()
        {
            using (var dtx1 = new GDMDateRange(null)) {
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

                Assert.Throws(typeof(GDMDateException), () => {
                    dtx1.ParseString("BET 04 JAN 2013 X 25 JAN 2013");
                });
            }
        }
    }
}
