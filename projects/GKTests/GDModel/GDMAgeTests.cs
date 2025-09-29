/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMAgeTests
    {
        public GDMAgeTests()
        {
            // for tests on net6.0 needs instance of AppHost
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_Common()
        {
            using (var age = new GDMAge()) {
                Assert.IsNotNull(age, "age1 != null");

                Assert.AreEqual(string.Empty, age.ParseString("INFANT"));
                Assert.AreEqual(-1, age.Relative);
                Assert.AreEqual(1, age.Years);
                Assert.AreEqual(-1, age.Months);
                Assert.AreEqual(-1, age.Days);
                Assert.AreEqual("< 001y", age.StringValue);

                Assert.AreEqual(string.Empty, age.ParseString("CHILD"));
                Assert.AreEqual(-1, age.Relative);
                Assert.AreEqual(8, age.Years);
                Assert.AreEqual(-1, age.Months);
                Assert.AreEqual(-1, age.Days);
                Assert.AreEqual("< 008y", age.StringValue);

                Assert.AreEqual(string.Empty, age.ParseString("STILLBORN"));
                Assert.AreEqual(0, age.Relative);
                Assert.AreEqual(0, age.Years);
                Assert.AreEqual(0, age.Months);
                Assert.AreEqual(0, age.Days);
                Assert.AreEqual("STILLBORN", age.StringValue);

                Assert.AreEqual(string.Empty, age.ParseString("> 18y 2m 13d"));
                Assert.AreEqual(+1, age.Relative);
                Assert.AreEqual(18, age.Years);
                Assert.AreEqual(2, age.Months);
                Assert.AreEqual(13, age.Days);
                Assert.AreEqual("> 018y 02m 013d", age.StringValue);

                Assert.AreEqual(string.Empty, age.ParseString("> 18y2M 13D"));
                Assert.AreEqual(+1, age.Relative);
                Assert.AreEqual(18, age.Years);
                Assert.AreEqual(2, age.Months);
                Assert.AreEqual(13, age.Days);
                Assert.AreEqual("> 018y 02m 013d", age.StringValue);

                using (var age2 = new GDMAge()) {
                    Assert.IsNotNull(age2, "age2 != null");

                    Assert.Throws(typeof(ArgumentException), () => {
                        age2.Assign(null);
                    });

                    Assert.AreEqual("", age2.StringValue);
                    age2.Assign(age);
                    Assert.AreEqual("> 018y 02m 013d", age2.StringValue);
                }
            }
        }

        [Test]
        public void Test_Reads()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_evtages.ged")) {
                GDMIndividualEvent evt1, evt2;
                GDMFamilyEvent evtF;

                var iRec1 = ctx.Tree.FindXRef<GDMIndividualRecord>("I2");
                Assert.IsNotNull(iRec1);

                evt1 = iRec1.FindEvent("DEAT") as GDMIndividualEvent;
                Assert.IsNotNull(evt1);
                Assert.AreEqual("> 020y", evt1.Age.StringValue);

                var iRec2 = ctx.Tree.FindXRef<GDMIndividualRecord>("I3");
                Assert.IsNotNull(iRec1);

                evt2 = iRec2.FindEvent("DEAT") as GDMIndividualEvent;
                Assert.IsNotNull(evt2);
                Assert.AreEqual("> 028y", evt2.Age.StringValue);

                var famRec = ctx.Tree.FindXRef<GDMFamilyRecord>("F1");
                Assert.IsNotNull(famRec);

                evtF = famRec.FindEvent("MARR") as GDMFamilyEvent;
                Assert.IsNotNull(evtF);
                Assert.AreEqual("> 018y", evtF.HusbandAge.StringValue);
                Assert.AreEqual("> 016y", evtF.WifeAge.StringValue);
            }
        }
    }
}
