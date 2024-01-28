/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMLocationRecordTests
    {
        private BaseContext fContext;

        public GDMLocationRecordTests()
        {
            fContext = TestUtils.CreateContext();
        }

        [Test]
        public void Test_Common()
        {
            using (GDMLocationRecord locRec = fContext.Tree.CreateLocation()) {
                locRec.LocationName = "Test Location";
                Assert.AreEqual("Test Location", locRec.LocationName);

                Assert.IsNotNull(locRec.Map);
                locRec.Map.Lati = 5.111111;
                locRec.Map.Long = 7.999999;

                using (GDMLocationRecord loc3 = fContext.Tree.CreateLocation()) {
                    var matchParams = new MatchParams();
                    matchParams.NamesIndistinctThreshold = 100.0f;

                    Assert.AreEqual(0.0f, locRec.IsMatch(null, matchParams));

                    loc3.LocationName = "Test Location";
                    Assert.AreEqual(100.0f, locRec.IsMatch(loc3, matchParams));

                    loc3.LocationName = "test";
                    Assert.AreEqual(0.0f, locRec.IsMatch(loc3, matchParams));
                }

                using (GDMLocationRecord loc2 = fContext.Tree.CreateLocation()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        loc2.Assign(null);
                    });

                    loc2.Assign(locRec);

                    string buf = GEDCOMProvider.GetTagStreamText(loc2, 0);
                    Assert.AreEqual("0 @L3@ _LOC\r\n" +
                                    "1 MAP\r\n" +
                                    "2 LATI 5.111111\r\n" +
                                    "2 LONG 7.999999\r\n" +
                                    "1 NAME Test Location\r\n", buf);
                }

                locRec.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(locRec.IsEmpty());
                locRec.Clear();
                Assert.IsTrue(locRec.IsEmpty());

                string buf1 = GEDCOMProvider.GetTagStreamText(locRec, 0);
                Assert.AreEqual("0 @L1@ _LOC\r\n", buf1);
            }
        }

        [Test]
        public void Test_Names()
        {
            using (GDMLocationRecord locRec = fContext.Tree.CreateLocation()) {
                locRec.LocationName = "РФ, Кировская обл., Верхнекамский р-н, д. Прислонская";

                var locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Российское царство, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье";
                locName.Date.ParseString("FROM 1678 TO 1708");

                locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Сибирская губерния, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье";
                locName.Date.ParseString("FROM 1709 TO 1719");

                locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Сибирская губ., Вятская пров., Слободской дистр., Верховская вол., д. Прислонская";
                locName.Date.ParseString("FROM 1719 TO 1726");

                locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Казанская губ., Вятская пров., Слободской уезд, Верховская вол., д. Мокрая Слободка";
                locName.Date.ParseString("FROM 1727 TO 1764");

                // Сибирская губерния, 18 DEC 1708 - 18 JAN 1782
                // Казанская губерния, 1796 - 1920
                // Вятская провинция, to Sib 29 MAY 1719 — 28 APR 1727, to Kazan 29 APR 1727 — 10 SEP 1780

                // Вятское наместничество, 11 SEP 1780 - 30 DEC 1796
                // Вятская губерния, 31 DEC 1796 - 13 DEC 1929

                string buf = GEDCOMProvider.GetTagStreamText(locRec, 0);
                Assert.AreEqual("0 @L4@ _LOC\r\n" +
                                "1 NAME РФ, Кировская обл., Верхнекамский р-н, д. Прислонская\r\n" +
                                "1 NAME Российское царство, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье\r\n" +
                                "2 DATE FROM 1678 TO 1708\r\n" +
                                "1 NAME Сибирская губерния, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье\r\n" +
                                "2 DATE FROM 1709 TO 1719\r\n" +
                                "1 NAME Сибирская губ., Вятская пров., Слободской дистр., Верховская вол., д. Прислонская\r\n" +
                                "2 DATE FROM 1719 TO 1726\r\n" +
                                "1 NAME Казанская губ., Вятская пров., Слободской уезд, Верховская вол., д. Мокрая Слободка\r\n" +
                                "2 DATE FROM 1727 TO 1764\r\n", buf);

                Assert.IsFalse(locRec.IsEmpty());
                locRec.Clear();
                Assert.IsTrue(locRec.IsEmpty());
            }
        }

        [Test]
        public void Test_NamesByDate()
        {
            using (GDMLocationRecord locRec = new GDMLocationRecord(null)) {
                locRec.LocationName = "РФ, Кировская обл., Верхнекамский р-н, д. Прислонская";

                var locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Российское царство, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье";
                locName.Date.ParseString("FROM 1678 TO 1708");

                locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Сибирская губерния, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье";
                locName.Date.ParseString("FROM 1709 TO 1719");

                locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Сибирская губ., Вятская пров., Слободской дистр., Верховская вол., д. Прислонская";
                locName.Date.ParseString("FROM 1719 TO 1726");

                locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = "Казанская губ., Вятская пров., Слободской уезд, Верховская вол., д. Мокрая Слободка";
                locName.Date.ParseString("FROM 1727 TO 1764");

                // null date -> default name
                Assert.AreEqual("РФ, Кировская обл., Верхнекамский р-н, д. Прислонская", locRec.GetNameByDate(null));

                // empty date -> default name
                var dateVal = new GDMDate();
                Assert.AreEqual("РФ, Кировская обл., Верхнекамский р-н, д. Прислонская", locRec.GetNameByDate(dateVal));

                //
                dateVal.ParseString("10 JAN 1701");
                Assert.AreEqual("Российское царство, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье", locRec.GetNameByDate(dateVal));

                //
                dateVal.ParseString("1711");
                Assert.AreEqual("Сибирская губерния, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье", locRec.GetNameByDate(dateVal));

                //
                dateVal.ParseString("1726");
                Assert.AreEqual("Сибирская губ., Вятская пров., Слободской дистр., Верховская вол., д. Прислонская", locRec.GetNameByDate(dateVal));

                //
                dateVal.ParseString("1747");
                Assert.AreEqual("Казанская губ., Вятская пров., Слободской уезд, Верховская вол., д. Мокрая Слободка", locRec.GetNameByDate(dateVal));

                Assert.IsFalse(locRec.IsEmpty());
                locRec.Clear();
                Assert.IsTrue(locRec.IsEmpty());
            }
        }

        [Test]
        public void Test_DateIntersections()
        {
            var r1 = GetRange("FROM 1708 TO 1929");

            var r2 = GetRange("FROM 1719 TO 1727");
            var r3 = GetRange("FROM 1701 TO 1780");
            var r4 = GetRange("FROM 1780 TO 1936");

            Assert.AreEqual("FROM 1719 TO 1727", GDMCustomDate.GetIntersection(r1, r2).StringValue);
            Assert.AreEqual("FROM 1708 TO 1780", GDMCustomDate.GetIntersection(r1, r3).StringValue);
            Assert.AreEqual("FROM 1780 TO 1929", GDMCustomDate.GetIntersection(r1, r4).StringValue);
        }

        private static GDMDatePeriod GetRange(string strDateRange)
        {
            var result = new GDMDatePeriod();
            result.ParseString(strDateRange);
            return result;
        }
    }
}
