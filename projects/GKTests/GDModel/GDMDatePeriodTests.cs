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
    public class GDMDatePeriodTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMDatePeriod dtx1 = new GDMDatePeriod(null)) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());

                Assert.AreEqual("", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true)); // date is empty
                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");

                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] - 2013.01.23 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
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
                Assert.AreEqual("2013.01.04 [G] >", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("TO 23 JAN 2013");
                Assert.AreEqual("TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(TestUtils.ParseDT("23.01.2013"), dtx1.Date);
                Assert.AreEqual("< 2013.01.23 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                Assert.Throws(typeof(NotSupportedException), () => {
                    dtx1.SetDateTime(DateTime.Now);
                });
            }
        }
    }
}
