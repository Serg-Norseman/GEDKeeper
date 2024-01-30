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
using System.Collections.Generic;
using System.Linq;
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
            TestUtils.InitUITest();

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
        public void Test_Hierarchy()
        {
            var tree = new GDMTree();

            var locRus = tree.CreateLocation();
            locRus.AddLocName("Россия", "FROM 862 TO 9999");

            IList<GDMLocationName> fullNames = locRus.GetFullNames(tree);
            string result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'__.__._862 [G] - __.__.9999 [G]': 'Россия'",
                result);

            var locSibGub = tree.CreateLocation();
            locSibGub.AddLocName("Сибирская губерния", "FROM 18 DEC 1708 TO 18 JAN 1782");
            locSibGub.AddLocLink(locRus, "FROM 18 DEC 1708 TO 18 JAN 1782");

            fullNames = locSibGub.GetFullNames(tree);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'18.12.1708 [G] - 18.01.1782 [G]': 'Сибирская губерния, Россия'",
                result);

            var locKazGub = tree.CreateLocation();
            locKazGub.AddLocName("Казанская губерния", "FROM 18 DEC 1708 TO 27 SEP 1781");
            locKazGub.AddLocName("Казанское наместничество", "FROM 28 SEP 1781 TO 11 DEC 1796");
            locKazGub.AddLocName("Казанская губерния", "FROM 12 DEC 1796 TO 27 MAY 1920");
            locKazGub.AddLocLink(locRus, "FROM 18 DEC 1708 TO 27 MAY 1920");

            fullNames = locKazGub.GetFullNames(tree);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'18.12.1708 [G] - 27.09.1781 [G]': 'Казанская губерния, Россия'\n"+
                            "'28.09.1781 [G] - 11.12.1796 [G]': 'Казанское наместничество, Россия'\n" +
                            "'12.12.1796 [G] - 27.05.1920 [G]': 'Казанская губерния, Россия'",
                result);

            var locVyatGub = tree.CreateLocation();
            locVyatGub.AddLocName("Вятская провинция", "FROM 29 MAY 1719 TO 10 SEP 1780");
            locVyatGub.AddLocName("Вятское наместничество", "FROM 11 SEP 1780 TO 30 DEC 1796");
            locVyatGub.AddLocName("Вятская губерния", "FROM 31 DEC 1796 TO 13 DEC 1929");
            locVyatGub.AddLocLink(locSibGub, "FROM 29 MAY 1719 TO 28 APR 1727");
            locVyatGub.AddLocLink(locKazGub, "FROM 29 APR 1727 TO 10 SEP 1780");
            locVyatGub.AddLocLink(locRus, "FROM 11 SEP 1780 TO 13 DEC 1929");

            fullNames = locVyatGub.GetFullNames(tree);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'29.05.1719 [G] - 28.04.1727 [G]': 'Вятская провинция, Сибирская губерния, Россия'\n" +
                            "'29.04.1727 [G] - 10.09.1780 [G]': 'Вятская провинция, Казанская губерния, Россия'\n"+
                            "'11.09.1780 [G] - 30.12.1796 [G]': 'Вятское наместничество, Россия'\n"+
                            "'31.12.1796 [G] - 13.12.1929 [G]': 'Вятская губерния, Россия'",
                result);

            var locSlobUezd = tree.CreateLocation();
            locSlobUezd.AddLocName("Слободской уезд", "FROM 1646 TO 1719");
            locSlobUezd.AddLocName("Слободской дистр.", "FROM 1719 TO 1726");
            locSlobUezd.AddLocName("Слободской уезд", "FROM 1727 TO 1929");
            locSlobUezd.AddLocLink(locRus, "TO 1718");
            locSlobUezd.AddLocLink(locVyatGub, "FROM 1719 TO 1726");
            locSlobUezd.AddLocLink(locKazGub, "FROM 1727 TO 10 SEP 1780");
            locSlobUezd.AddLocLink(locVyatGub, "FROM 11 SEP 1780 TO 10 JUN 1929");

            fullNames = locSlobUezd.GetFullNames(tree);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'__.__.1646 [G] - __.__.1718 [G]': 'Слободской уезд, Россия'\n" +
                            "'29.05.1719 [G] - __.__.1726 [G]': 'Слободской дистр., Вятская провинция, Сибирская губерния, Россия'\n" +
                            "'__.__.1727 [G] - 10.09.1780 [G]': 'Слободской уезд, Казанская губерния, Россия'\n" +
                            "'11.09.1780 [G] - 30.12.1796 [G]': 'Слободской уезд, Вятское наместничество, Россия'\n" +
                            "'31.12.1796 [G] - __.__.1929 [G]': 'Слободской уезд, Вятская губерния, Россия'",
                result);

            var locVerhVol = tree.CreateLocation();
            locVerhVol.AddLocName("Верховская вол.", "FROM 1720 TO 1764");
            locVerhVol.AddLocLink(locSlobUezd, "FROM 1720 TO 1764");

            var locLoinVol = tree.CreateLocation();
            locLoinVol.AddLocName("Лоинская вол.", "FROM 1802 TO 1884");
            locLoinVol.AddLocLink(locSlobUezd, "FROM 1802 TO 1884");

            var locKirsVol = tree.CreateLocation();
            locKirsVol.AddLocName("Кирсинская вол.", "FROM 1884 TO 1926");
            locKirsVol.AddLocLink(locSlobUezd, "FROM 1884 TO 1926");

            var locPrislon = tree.CreateLocation();
            locPrislon.AddLocName("поч. Старое раменье", "FROM 1678 TO 1719");
            locPrislon.AddLocName("д. Прислонская", "FROM 1720 TO 1726");
            locPrislon.AddLocName("д. Мокрая Слободка", "FROM 1727 TO 1926");
            locPrislon.AddLocLink(locSlobUezd, "FROM 1678 TO 1719");
            locPrislon.AddLocLink(locVerhVol, "FROM 1720 TO 1801");
            locPrislon.AddLocLink(locLoinVol, "FROM 1802 TO 1883");
            locPrislon.AddLocLink(locKirsVol, "FROM 1884 TO 1926");

            fullNames = locPrislon.GetFullNames(tree);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'__.__.1678 [G] - __.__.1718 [G]': 'поч. Старое раменье, Слободской уезд, Россия'\n" +
                            "'__.__.1720 [G] - __.__.1726 [G]': 'д. Прислонская, Верховская вол., Слободской дистр., Вятская провинция, Сибирская губерния, Россия'\n" +
                            "'__.__.1727 [G] - __.__.1764 [G]': 'д. Мокрая Слободка, Верховская вол., Слободской уезд, Казанская губерния, Россия'\n" +
                            "'__.__.1802 [G] - __.__.1883 [G]': 'д. Мокрая Слободка, Лоинская вол., Слободской уезд, Вятская губерния, Россия'\n" +
                            "'__.__.1884 [G] - __.__.1926 [G]': 'д. Мокрая Слободка, Кирсинская вол., Слободской уезд, Вятская губерния, Россия'",
                result);

            /*var dateVal = new GDMDate();
            dateVal.ParseString("10 JAN 1701");
            Assert.AreEqual("Российское царство, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье", locPrislon.GetNameByDate(dateVal));

            //
            dateVal.ParseString("1711");
            Assert.AreEqual("Сибирская губерния, Вятка, Слободской уезд, Екатерининского мон. вотч., поч. Старое раменье", locPrislon.GetNameByDate(dateVal));

            //
            dateVal.ParseString("1726");
            Assert.AreEqual("Сибирская губ., Вятская пров., Слободской дистр., Верховская вол., д. Прислонская", locPrislon.GetNameByDate(dateVal));

            //
            dateVal.ParseString("1747");
            Assert.AreEqual("Казанская губ., Вятская пров., Слободской уезд, Верховская вол., д. Мокрая Слободка", locPrislon.GetNameByDate(dateVal));*/
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

    public static class LocExts
    {
        public static void AddLocName(this GDMLocationRecord locRec, string name, string date)
        {
            var locName = locRec.Names.Add(new GDMLocationName());
            locName.StringValue = name;
            locName.Date.ParseString(date);
        }

        public static void AddLocLink(this GDMLocationRecord locRec, GDMLocationRecord topLevel, string date)
        {
            var locLink = locRec.TopLevels.Add(new GDMLocationLink());
            locLink.XRef = topLevel.XRef;
            locLink.Date.ParseString(date);
        }

    }
}
