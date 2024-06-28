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
                var locName = locRec.Names.Add(new GDMLocationName());
                locName.StringValue = locRec.LocationName = "РФ, Кировская обл., Верхнекамский р-н, д. Прислонская";
                locName.Date.ParseString("FROM 1765");

                locName = locRec.Names.Add(new GDMLocationName());
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

                locRec.SortNames();

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

            var locRus = CreateRussia(tree);

            var fullNames = locRus.GetFullNames(tree, ATDEnumeration.fStL);
            string result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'__.__._862 [G] - __.__.1546 [G]': 'Россия'\n" +
                            "'__.__.1547 [G] - 21.10.1721 [G]': 'Российское царство'\n" +
                            "'22.10.1721 [G] - 31.08.1917 [G]': 'Российская империя'\n" +
                            "'01.09.1917 [G] - 18.07.1918 [G]': 'Российская республика'\n" +
                            "'19.07.1918 [G] - 29.12.1922 [G]': 'РСФСР'\n" +
                            "'30.12.1922 [G] - 26.12.1991 [G]': 'СССР'\n" +
                            "'27.12.1991 [G] >': 'РФ'",
                result);

            var locSibGub = tree.CreateLocation();
            locSibGub.AddLocName("Сибирская губерния", "FROM 18 DEC 1708 TO 19 JAN 1782");
            locSibGub.AddLocLink(locRus, "FROM 18 DEC 1708 TO 19 JAN 1782");
            Assert.True(locSibGub.ValidateLinks());
            Assert.True(locSibGub.ValidateNames());

            fullNames = locSibGub.GetFullNames(tree, ATDEnumeration.fStL);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'18.12.1708 [G] - 21.10.1721 [G]': 'Сибирская губерния, Российское царство'\n" +
                            "'22.10.1721 [G] - 19.01.1782 [G]': 'Сибирская губерния, Российская империя'",
                result);

            var locKazGub = tree.CreateLocation();
            locKazGub.AddLocName("Казанская губерния", "FROM 18 DEC 1708 TO 27 SEP 1781");
            locKazGub.AddLocName("Казанское наместничество", "FROM 28 SEP 1781 TO 11 DEC 1796");
            locKazGub.AddLocName("Казанская губерния", "FROM 12 DEC 1796 TO 27 MAY 1920");
            locKazGub.AddLocLink(locRus, "FROM 18 DEC 1708 TO 27 MAY 1920");
            Assert.True(locKazGub.ValidateLinks());
            Assert.True(locKazGub.ValidateNames());

            fullNames = locKazGub.GetFullNames(tree, ATDEnumeration.fStL);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'18.12.1708 [G] - 21.10.1721 [G]': 'Казанская губерния, Российское царство'\n" +
                            "'22.10.1721 [G] - 27.09.1781 [G]': 'Казанская губерния, Российская империя'\n" +
                            "'28.09.1781 [G] - 11.12.1796 [G]': 'Казанское наместничество, Российская империя'\n" +
                            "'12.12.1796 [G] - 31.08.1917 [G]': 'Казанская губерния, Российская империя'\n" +
                            "'01.09.1917 [G] - 18.07.1918 [G]': 'Казанская губерния, Российская республика'\n" +
                            "'19.07.1918 [G] - 27.05.1920 [G]': 'Казанская губерния, РСФСР'",
                result);

            var locNizhNovgGub = tree.CreateLocation();
            locNizhNovgGub.AddLocName("Нижегородская губерния", "FROM JAN 1714 TO 1717");
            locNizhNovgGub.AddLocName("Нижегородская губерния", "FROM 29 MAY 1719 TO 04 SEP 1779");
            locNizhNovgGub.AddLocName("Нижегородское наместничество", "FROM 05 SEP 1779 TO 11 DEC 1796");
            locNizhNovgGub.AddLocName("Нижегородская губерния", "FROM 12 DEC 1796 TO 13 JAN 1929");
            locNizhNovgGub.AddLocName("Нижегородская область", "FROM 14 JAN 1929 TO 14 JUL 1929");
            locNizhNovgGub.AddLocName("Нижегородский край", "FROM 15 JUL 1929 TO 06 OCT 1932");
            locNizhNovgGub.AddLocName("Горьковский край", "FROM 07 OCT 1932 TO 04 DEC 1936");
            locNizhNovgGub.AddLocName("Горьковская область", "FROM 05 DEC 1936 TO 21 OCT 1990");
            locNizhNovgGub.AddLocName("Нижегородская область", "FROM 22 OCT 1990 TO 9999");
            locNizhNovgGub.AddLocLink(locRus, "FROM JAN 1714 TO 9999");
            Assert.True(locNizhNovgGub.ValidateLinks());
            Assert.True(locNizhNovgGub.ValidateNames());

            var locVyatGub = tree.CreateLocation();
            locVyatGub.AddLocName("Вятская провинция", "FROM 29 MAY 1719 TO 10 SEP 1780");
            locVyatGub.AddLocName("Вятское наместничество", "FROM 11 SEP 1780 TO 30 DEC 1796");
            locVyatGub.AddLocName("Вятская губерния", "FROM 31 DEC 1796 TO 14 JUL 1929");
            locVyatGub.AddLocName("Вятский округ", "FROM 15 JUL 1929 TO 29 JUL 1930");
            locVyatGub.AddLocName("Кировский край", "FROM 07 DEC 1934 TO 04 DEC 1936");
            locVyatGub.AddLocName("Кировская область", "FROM 05 DEC 1936");
            locVyatGub.AddLocLink(locSibGub, "FROM 29 MAY 1719 TO 28 APR 1727");
            locVyatGub.AddLocLink(locKazGub, "FROM 29 APR 1727 TO 10 SEP 1780");
            locVyatGub.AddLocLink(locRus, "FROM 11 SEP 1780 TO 14 JUL 1929");
            locVyatGub.AddLocLink(locNizhNovgGub, "FROM 15 JUL 1929 TO 29 JUL 1930");
            locVyatGub.AddLocLink(locRus, "FROM 30 JUL 1930 TO 9999");
            Assert.True(locVyatGub.ValidateLinks());
            Assert.True(locVyatGub.ValidateNames());

            fullNames = locVyatGub.GetFullNames(tree, ATDEnumeration.fStL);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'29.05.1719 [G] - 21.10.1721 [G]': 'Вятская провинция, Сибирская губерния, Российское царство'\n" +
                            "'22.10.1721 [G] - 28.04.1727 [G]': 'Вятская провинция, Сибирская губерния, Российская империя'\n" +
                            "'29.04.1727 [G] - 10.09.1780 [G]': 'Вятская провинция, Казанская губерния, Российская империя'\n" +
                            "'11.09.1780 [G] - 30.12.1796 [G]': 'Вятское наместничество, Российская империя'\n" +
                            "'31.12.1796 [G] - 31.08.1917 [G]': 'Вятская губерния, Российская империя'\n" +
                            "'01.09.1917 [G] - 18.07.1918 [G]': 'Вятская губерния, Российская республика'\n" +
                            "'19.07.1918 [G] - 29.12.1922 [G]': 'Вятская губерния, РСФСР'\n" +
                            "'30.12.1922 [G] - 14.07.1929 [G]': 'Вятская губерния, СССР'\n" +
                            "'15.07.1929 [G] - 29.07.1930 [G]': 'Вятский округ, Нижегородский край, СССР'\n" +
                            "'07.12.1934 [G] - 04.12.1936 [G]': 'Кировский край, СССР'\n" +
                            "'05.12.1936 [G] - 26.12.1991 [G]': 'Кировская область, СССР'\n" +
                            "'27.12.1991 [G] - __.__.9999 [G]': 'Кировская область, РФ'\n" +
                            "'__.__.10000 [G] >': 'Кировская область'",
                result);

            var locSlobUezd = tree.CreateLocation();
            locSlobUezd.AddLocName("Слободской уезд", "FROM 1646 TO 1718");
            locSlobUezd.AddLocName("Слободской дистр.", "FROM 1719 TO 1726");
            locSlobUezd.AddLocName("Слободской уезд", "FROM 1727 TO 09 JUN 1929");
            locSlobUezd.AddLocName("Слободской район", "FROM 10 JUN 1929");
            locSlobUezd.AddLocLink(locRus, "TO 1718");
            locSlobUezd.AddLocLink(locVyatGub, "FROM 1719 TO 1726");
            locSlobUezd.AddLocLink(locVyatGub, "FROM 1727 TO 10 SEP 1780");
            locSlobUezd.AddLocLink(locVyatGub, "FROM 11 SEP 1780 TO 10 JUN 1929");
            Assert.True(locSlobUezd.ValidateLinks());
            Assert.True(locSlobUezd.ValidateNames());

            var locOmutUezd = tree.CreateLocation();
            locOmutUezd.AddLocName("Омутнинский уезд", "FROM 05 JAN 1921 TO 09 JUN 1929");
            locOmutUezd.AddLocName("Омутнинский район", "FROM 10 JUN 1929");
            locOmutUezd.AddLocLink(locVyatGub, "FROM 05 JAN 1921 TO 9999");
            Assert.True(locOmutUezd.ValidateLinks());
            Assert.True(locOmutUezd.ValidateNames());

            var locVerhkamRn = tree.CreateLocation();
            locVerhkamRn.AddLocName("Верхнекамский район", "FROM 12 JAN 1965");
            locVerhkamRn.AddLocLink(locVyatGub, "FROM 12 JAN 1965 TO 9999");
            Assert.True(locVerhkamRn.ValidateLinks());
            Assert.True(locVerhkamRn.ValidateNames());

            fullNames = locSlobUezd.GetFullNames(tree, ATDEnumeration.fStL);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'__.__.1646 [G] - __.__.1718 [G]': 'Слободской уезд, Российское царство'\n" +
                            "'__.__.1719 [G] - 28.05.1719 [G]': 'Слободской дистр.'\n" +
                            "'29.05.1719 [G] - 21.10.1721 [G]': 'Слободской дистр., Вятская провинция, Сибирская губерния, Российское царство'\n" +
                            "'22.10.1721 [G] - __.__.1726 [G]': 'Слободской дистр., Вятская провинция, Сибирская губерния, Российская империя'\n" +
                            "'__.__.1727 [G] - 28.04.1727 [G]': 'Слободской уезд, Вятская провинция, Сибирская губерния, Российская империя'\n" +
                            "'29.04.1727 [G] - 10.09.1780 [G]': 'Слободской уезд, Вятская провинция, Казанская губерния, Российская империя'\n" +
                            "'11.09.1780 [G] - 30.12.1796 [G]': 'Слободской уезд, Вятское наместничество, Российская империя'\n" +
                            "'31.12.1796 [G] - 31.08.1917 [G]': 'Слободской уезд, Вятская губерния, Российская империя'\n" +
                            "'01.09.1917 [G] - 18.07.1918 [G]': 'Слободской уезд, Вятская губерния, Российская республика'\n" +
                            "'19.07.1918 [G] - 29.12.1922 [G]': 'Слободской уезд, Вятская губерния, РСФСР'\n" +
                            "'30.12.1922 [G] - 09.06.1929 [G]': 'Слободской уезд, Вятская губерния, СССР'\n" +
                            "'10.06.1929 [G] - 10.06.1929 [G]': 'Слободской район, Вятская губерния, СССР'\n" +
                            "'11.06.1929 [G] >': 'Слободской район'",
                result);

            var locVerhVol = tree.CreateLocation();
            locVerhVol.AddLocName("Верховская вол.", "FROM 1720 TO 1764");
            locVerhVol.AddLocLink(locSlobUezd, "FROM 1720 TO 1764");
            Assert.True(locVerhVol.ValidateLinks());
            Assert.True(locVerhVol.ValidateNames());

            var locLoinVol = tree.CreateLocation();
            locLoinVol.AddLocName("Лоинская вол.", "FROM 1802 TO 1884");
            locLoinVol.AddLocLink(locSlobUezd, "FROM 1802 TO 1884");
            Assert.True(locLoinVol.ValidateLinks());
            Assert.True(locLoinVol.ValidateNames());

            var locKirsVol = tree.CreateLocation();
            locKirsVol.AddLocName("Кирсинская вол.", "FROM 1884 TO 1926");
            locKirsVol.AddLocLink(locSlobUezd, "FROM 1884 TO 1925");
            locKirsVol.AddLocLink(locOmutUezd, "FROM 1926 TO 1926");
            Assert.True(locKirsVol.ValidateLinks());
            Assert.True(locKirsVol.ValidateNames());

            var locPrislon = tree.CreateLocation();
            locPrislon.AddLocName("поч. Старое раменье", "FROM 1678 TO 1719");
            locPrislon.AddLocName("д. Прислонская", "FROM 1720 TO 1726");
            locPrislon.AddLocName("д. Мокрая Слободка", "FROM 1727 TO 1926");
            locPrislon.AddLocName("д. Колегово", "FROM 1927 TO 1978");
            locPrislon.AddLocLink(locSlobUezd, "FROM 1678 TO 1719");
            locPrislon.AddLocLink(locVerhVol, "FROM 1720 TO 1801");
            locPrislon.AddLocLink(locLoinVol, "FROM 1802 TO 1883");
            locPrislon.AddLocLink(locKirsVol, "FROM 1884 TO 1926");
            locPrislon.AddLocLink(locOmutUezd, "FROM 1927 TO 1964");
            locPrislon.AddLocLink(locVerhkamRn, "FROM 1965 TO 9999");
            Assert.True(locPrislon.ValidateLinks());
            Assert.True(locPrislon.ValidateNames());

            fullNames = locPrislon.GetFullNames(tree, ATDEnumeration.fStL);
            result = string.Join("\n", fullNames.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));
            Assert.AreEqual("'__.__.1678 [G] - __.__.1718 [G]': 'поч. Старое раменье, Слободской уезд, Российское царство'\n" +
                            "'__.__.1719 [G] - __.__.1719 [G]': 'поч. Старое раменье, Слободской дистр.'\n" +
                            "'__.__.1720 [G] - 21.10.1721 [G]': 'д. Прислонская, Верховская вол., Слободской дистр., Вятская провинция, Сибирская губерния, Российское царство'\n" +
                            "'22.10.1721 [G] - __.__.1726 [G]': 'д. Прислонская, Верховская вол., Слободской дистр., Вятская провинция, Сибирская губерния, Российская империя'\n" +
                            "'__.__.1727 [G] - 28.04.1727 [G]': 'д. Мокрая Слободка, Верховская вол., Слободской уезд, Вятская провинция, Сибирская губерния, Российская империя'\n" +
                            "'29.04.1727 [G] - __.__.1764 [G]': 'д. Мокрая Слободка, Верховская вол., Слободской уезд, Вятская провинция, Казанская губерния, Российская империя'\n" +
                            "'__.__.1765 [G] - __.__.1801 [G]': 'д. Мокрая Слободка'\n" +
                            "'__.__.1802 [G] - __.__.1883 [G]': 'д. Мокрая Слободка, Лоинская вол., Слободской уезд, Вятская губерния, Российская империя'\n" +
                            "'__.__.1884 [G] - 31.08.1917 [G]': 'д. Мокрая Слободка, Кирсинская вол., Слободской уезд, Вятская губерния, Российская империя'\n" +
                            "'01.09.1917 [G] - 18.07.1918 [G]': 'д. Мокрая Слободка, Кирсинская вол., Слободской уезд, Вятская губерния, Российская республика'\n" +
                            "'19.07.1918 [G] - 29.12.1922 [G]': 'д. Мокрая Слободка, Кирсинская вол., Слободской уезд, Вятская губерния, РСФСР'\n" +
                            "'30.12.1922 [G] - __.__.1925 [G]': 'д. Мокрая Слободка, Кирсинская вол., Слободской уезд, Вятская губерния, СССР'\n" +
                            "'__.__.1926 [G] - __.__.1926 [G]': 'д. Мокрая Слободка, Кирсинская вол., Омутнинский уезд, Вятская губерния, СССР'\n" +
                            "'__.__.1927 [G] - 09.06.1929 [G]': 'д. Колегово, Омутнинский уезд, Вятская губерния, СССР'\n" +
                            "'10.06.1929 [G] - 14.07.1929 [G]': 'д. Колегово, Омутнинский район, Вятская губерния, СССР'\n" +
                            "'15.07.1929 [G] - 29.07.1930 [G]': 'д. Колегово, Омутнинский район, Вятский округ, Нижегородский край, СССР'\n" +
                            "'30.07.1930 [G] - 06.12.1934 [G]': 'д. Колегово, Омутнинский район'\n" +
                            "'07.12.1934 [G] - 04.12.1936 [G]': 'д. Колегово, Омутнинский район, Кировский край, СССР'\n" +
                            "'05.12.1936 [G] - __.__.1964 [G]': 'д. Колегово, Омутнинский район, Кировская область, СССР'\n" +
                            "'__.__.1965 [G] - 11.01.1965 [G]': 'д. Колегово'\n" +
                            "'12.01.1965 [G] - __.__.1978 [G]': 'д. Колегово, Верхнекамский район, Кировская область, СССР'",
                result);

            var dateVal = new GDMDate();

            // null or empty date
            Assert.AreEqual("д. Колегово", locPrislon.GetNameByDate(null));
            Assert.AreEqual("д. Колегово", locPrislon.GetNameByDate(dateVal, false));
            Assert.AreEqual("д. Колегово, Верхнекамский район, Кировская область, СССР", locPrislon.GetNameByDate(dateVal, ATDEnumeration.fStL, true));

            dateVal.ParseString("10 JAN 1701");
            Assert.AreEqual("поч. Старое раменье, Слободской уезд, Российское царство", locPrislon.GetNameByDate(dateVal, ATDEnumeration.fStL, true));

            dateVal.ParseString("1724");
            Assert.AreEqual("д. Прислонская, Верховская вол., Слободской дистр., Вятская провинция, Сибирская губерния, Российская империя", locPrislon.GetNameByDate(dateVal, ATDEnumeration.fStL, true));

            dateVal.ParseString("1834");
            Assert.AreEqual("д. Мокрая Слободка, Лоинская вол., Слободской уезд, Вятская губерния, Российская империя", locPrislon.GetNameByDate(dateVal, ATDEnumeration.fStL, true));

            dateVal.ParseString("1930");
            Assert.AreEqual("д. Колегово, Омутнинский район, Вятский округ, Нижегородский край, СССР", locPrislon.GetNameByDate(dateVal, ATDEnumeration.fStL, true));

            //var gedcomProvider = new GEDCOMProvider(tree);
            //gedcomProvider.SaveToStreamExt(new FileStream("d:\\Russia.ged", FileMode.CreateNew), GEDCOMCharacterSet.csUTF8);
        }

        [Test]
        public void Test_Chisinau_SeparateTopLevels()
        {
            var tree = new GDMTree();
            var ussr = tree.CreateLocation();
            ussr.AddLocName("СССР", "FROM 30 DEC 1922 TO 26 DEC 1991");

            var moldova = tree.CreateLocation();
            moldova.AddLocName("Молдова", "FROM 28 AUG 1991");

            var moldavianSsr = tree.CreateLocation();
            moldavianSsr.AddLocName("Молдавская ССР", "FROM 2 AUG 1940 TO 27 AUG 1991");
            moldavianSsr.AddLocLink(ussr, "FROM 2 AUG 1940 TO 27 AUG 1991");

            var carbuna = tree.CreateLocation();
            carbuna.AddLocName("Кишинёв", "FROM 14 OCT 1436");
            carbuna.AddLocLink(moldavianSsr, "FROM 2 AUG 1940 TO 27 AUG 1991");
            carbuna.AddLocLink(moldova, "FROM 28 AUG 1991");

            var names = carbuna.GetFullNames(tree, ATDEnumeration.fLtS);
            var result = string.Join("\n", names.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));

            Assert.AreEqual(
                "'14.10.1436 [G] - 01.08.1940 [G]': 'Кишинёв'\n" +
                "'02.08.1940 [G] - 27.08.1991 [G]': 'СССР, Молдавская ССР, Кишинёв'\n" +
                "'28.08.1991 [G] >': 'Молдова, Кишинёв'"
                , result);
        }

        [Test]
        public void Test_Chisinau_CombinedTopLevel()
        {
            var tree = new GDMTree();
            var ussr = CreateRussia(tree);

            var moldova = tree.CreateLocation();
            moldova.AddLocName("Молдавская ССР", "FROM 2 AUG 1940 TO 27 AUG 1991");
            moldova.AddLocLink(ussr, "FROM 2 AUG 1940 TO 27 AUG 1991");

            var carbuna = tree.CreateLocation();
            carbuna.AddLocName("Кишинёв", "FROM 14 OCT 1436");
            carbuna.AddLocLink(moldova, "FROM 1812");

            var names = carbuna.GetFullNames(tree, ATDEnumeration.fLtS);
            var result = string.Join("\n", names.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));

            Assert.AreEqual(
                "'14.10.1436 [G] - 01.08.1940 [G]': 'Кишинёв'\n" +
                "'02.08.1940 [G] - 27.08.1991 [G]': 'СССР, Молдавская ССР, Кишинёв'\n" +
                "'28.08.1991 [G] >': 'Кишинёв'"
                , result);

            moldova.AddLocName("Молдова", "FROM 28 AUG 1991");

            names = carbuna.GetFullNames(tree, ATDEnumeration.fLtS);
            result = string.Join("\n", names.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));

            Assert.AreEqual(
                "'14.10.1436 [G] - 01.08.1940 [G]': 'Кишинёв'\n" +
                "'02.08.1940 [G] - 27.08.1991 [G]': 'СССР, Молдавская ССР, Кишинёв'\n" +
                "'28.08.1991 [G] >': 'Молдова, Кишинёв'"
                , result);
        }

        [Test]
        public void Test_VyatkaProvince_MissingLocationName()
        {
            var tree = new GDMTree();

            var locRus = CreateRussia(tree);

            var locSibGub = tree.CreateLocation();
            var locKazGub = tree.CreateLocation();
            locKazGub.AddLocName("Казанская губерния", "FROM 12 DEC 1796 TO 27 MAY 1920");
            locKazGub.AddLocLink(locRus, "FROM 12 DEC 1796 TO 27 MAY 1920");
            Assert.True(locKazGub.ValidateLinks());
            Assert.True(locKazGub.ValidateNames());

            locSibGub.AddLocName("Сибирская губерния", "FROM 18 DEC 1708 TO 19 JAN 1782");
            locSibGub.AddLocLink(locRus, "FROM 18 DEC 1708 TO 19 JAN 1782");
            Assert.True(locSibGub.ValidateLinks());
            Assert.True(locSibGub.ValidateNames());

            var locVyatGub = tree.CreateLocation();
            locVyatGub.AddLocName("Вятская провинция", "FROM 29 MAY 1719 TO 10 SEP 1780");
            locVyatGub.AddLocLink(locSibGub, "FROM 29 MAY 1719 TO 28 APR 1727");
            locVyatGub.AddLocLink(locKazGub, "FROM 29 APR 1727 TO 10 SEP 1780");
            Assert.True(locVyatGub.ValidateLinks());
            Assert.True(locVyatGub.ValidateNames());

            var names = locVyatGub.GetFullNames(tree, ATDEnumeration.fLtS);
            var result = string.Join("\n", names.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));

            Assert.AreEqual(
                "'29.05.1719 [G] - 21.10.1721 [G]': 'Российское царство, Сибирская губерния, Вятская провинция'\n" +
                "'22.10.1721 [G] - 28.04.1727 [G]': 'Российская империя, Сибирская губерния, Вятская провинция'\n" +
                "'29.04.1727 [G] - 10.09.1780 [G]': 'Вятская провинция'"
                , result);
        }

        [Test]
        public void Test_LocationsWithoutDates()
        {
            var tree = new GDMTree();
            var locRus = tree.CreateLocation();
            locRus.LocationName = "Россия";

            var locMoscow = tree.CreateLocation();
            locMoscow.LocationName = "Москва";
            locMoscow.TopLevels.Add(new GDMLocationLink { XRef = locRus.XRef });

            var names = locMoscow.GetFullNames(tree, ATDEnumeration.fLtS);
            var result = string.Join("\n", names.Select(x => string.Format("'{0}': '{1}'", x.Date.ToString(), x.StringValue)));

            Assert.AreEqual(
                "'': 'Москва'"
                , result);
        }

        [Test]
        public void Test_DateIntersections()
        {
            var r1 = GetRange("FROM 1708 TO 1929");

            var r2 = GetRange("FROM 1719 TO 1727");
            var r3 = GetRange("FROM 1701 TO 1780");
            var r4 = GetRange("FROM 1780 TO 1936");

            Assert.AreEqual("FROM 1719 TO 1727", GDMCustomDate.GetIntersection(r1, r2).StringValue); // r2 inside r1
            Assert.AreEqual("FROM 1708 TO 1780", GDMCustomDate.GetIntersection(r1, r3).StringValue); // r3 to left of r1
            Assert.AreEqual("FROM 1780 TO 1929", GDMCustomDate.GetIntersection(r1, r4).StringValue); // r4 to right of r1

            Assert.AreEqual("FROM 1747 TO 1764", GDMCustomDate.GetIntersection(GetRange("TO 1764"), GetRange("FROM 1747 TO 1834")).StringValue);
            Assert.AreEqual("FROM 1747 TO 1764", GDMCustomDate.GetIntersection(GetRange("FROM 1747 TO 1834"), GetRange("TO 1764")).StringValue);

            Assert.AreEqual("FROM 1782 TO 1834", GDMCustomDate.GetIntersection(GetRange("FROM 1782"), GetRange("FROM 1747 TO 1834")).StringValue);
            Assert.AreEqual("FROM 1782 TO 1834", GDMCustomDate.GetIntersection(GetRange("FROM 1747 TO 1834"), GetRange("FROM 1782")).StringValue);

            Assert.AreEqual("FROM 1782 TO 1834", GDMCustomDate.GetIntersection(GetRange("FROM 1782"), GetRange("TO 1834")).StringValue);
            Assert.AreEqual("FROM 1782 TO 1834", GDMCustomDate.GetIntersection(GetRange("TO 1834"), GetRange("FROM 1782")).StringValue);
            Assert.AreEqual("FROM 1782", GDMCustomDate.GetIntersection(GetRange("FROM 1782"), GetRange("FROM 1782")).StringValue);
            Assert.AreEqual("FROM 1834", GDMCustomDate.GetIntersection(GetRange("FROM 1782"), GetRange("FROM 1834")).StringValue);
            Assert.AreEqual("TO 1782", GDMCustomDate.GetIntersection(GetRange("TO 1782"), GetRange("TO 1782")).StringValue);
            Assert.AreEqual("TO 1782", GDMCustomDate.GetIntersection(GetRange("TO 1782"), GetRange("TO 1834")).StringValue);

            Assert.AreEqual("", GDMCustomDate.GetIntersection(GetRange("FROM 1858"), GetRange("TO 1834")).StringValue); // no intersects
            Assert.AreEqual("", GDMCustomDate.GetIntersection(GetRange("TO 1834"), GetRange("FROM 1858")).StringValue); // no intersects

            Assert.AreEqual("", GDMCustomDate.GetIntersection(GetRange(""), GetRange("FROM 1747 TO 1834")).StringValue); // no intersects

            Assert.AreEqual("", GDMCustomDate.GetIntersection(GetRange("FROM 1719 TO 1727"), GetRange("FROM 1747 TO 1834")).StringValue); // no intersects
        }

        [TestCase("FROM 1775", "FROM 1940 TO 1991", "FROM 1775 TO 1939", "FROM 1992")]
        [TestCase("FROM 1775", "FROM 1940", "FROM 1775 TO 1939", "")]
        [TestCase("TO 2001", "FROM 1940 TO 1991", "TO 1939", "FROM 1992 TO 2001")]
        [TestCase("TO 2001", "TO 1991", "", "FROM 1992 TO 2001")]
        [TestCase("FROM 1937", "TO 1953", "TO 1936", "FROM 1954")]
        [TestCase("FROM 1678 TO 1718", "FROM 1678 TO 1719", "", "FROM 1719 TO 1719")]
        public void Test_GetDifference(string range1, string range2, string expected1, string expected2)
        {
            var difference = GDMCustomDate.GetDifference(GetRange(range1), GetRange(range2));
            Assert.AreEqual(2, difference.Count);
            Assert.AreEqual(expected1, difference[0].StringValue);
            Assert.AreEqual(expected2, difference[1].StringValue);

            difference = GDMCustomDate.GetDifference(GetRange(range2), GetRange(range1));
            Assert.AreEqual(2, difference.Count);
            Assert.AreEqual(expected1, difference[0].StringValue);
            Assert.AreEqual(expected2, difference[1].StringValue);
        }

        private static GDMDatePeriod GetRange(string strDateRange)
        {
            var result = new GDMDatePeriod();
            result.ParseString(strDateRange);
            return result;
        }

        private static GDMLocationRecord CreateRussia(GDMTree tree)
        {
            // J2G Rus transfer: 26 JAN 1918 (Julian), 1 FEB -> 14 FEB 1918
            var locRus = tree.CreateLocation();
            locRus.AddLocName("Россия", "FROM 862 TO 1546");
            locRus.AddLocName("Российское царство", "FROM 1547 TO 21 OCT 1721"); //++
            locRus.AddLocName("Российская империя", "FROM 22 OCT 1721 TO 31 AUG 1917"); //++
            locRus.AddLocName("Российская республика", "FROM 01 SEP 1917 TO 18 JUL 1918"); //++
            locRus.AddLocName("РСФСР", "FROM 19 JUL 1918 TO 29 DEC 1922"); //++
            locRus.AddLocName("СССР", "FROM 30 DEC 1922 TO 26 DEC 1991"); //++
            locRus.AddLocName("РФ", "FROM 27 DEC 1991"); //++
            Assert.True(locRus.ValidateLinks());
            Assert.True(locRus.ValidateNames());
            return locRus;
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
