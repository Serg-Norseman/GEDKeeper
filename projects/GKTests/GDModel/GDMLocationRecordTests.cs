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
using GDModel;
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

        [TestFixtureSetUp]
        public void SetUp()
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

                    string buf = TestUtils.GetTagStreamText(loc2, 0);
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

                string buf1 = TestUtils.GetTagStreamText(locRec, 0);
                Assert.AreEqual("0 @L1@ _LOC\r\n", buf1);
            }
        }

        [Test]
        public void Test_GDMPlace()
        {
            using (GDMPlace place = GDMPlace.Create(null, "", "") as GDMPlace) {
                place.Form = "abrakadabra";
                Assert.AreEqual("abrakadabra", place.Form);

                Assert.IsNotNull(place.Map);
                Assert.IsNotNull(place.Location);

                using (GDMPlace place2 = GDMPlace.Create(null, "", "") as GDMPlace) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        place2.Assign(null);
                    });

                    place2.Assign(place);

                    string buf = TestUtils.GetTagStreamText(place2, 1);
                    Assert.AreEqual("1 PLAC\r\n" +
                                    "2 FORM abrakadabra\r\n", buf);
                }

                place.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(place.IsEmpty());
                place.Clear();
                Assert.IsTrue(place.IsEmpty());
            }
        }

        [Test]
        public void Test_GDMMap()
        {
            using (GDMMap map = GDMMap.Create(null, "", "") as GDMMap) {
                map.Lati = 5.111111;
                Assert.AreEqual(5.111111, map.Lati);

                map.Long = 7.999999;
                Assert.AreEqual(7.999999, map.Long);

                using (GDMMap map2 = GDMMap.Create(null, "", "") as GDMMap) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        map2.Assign(null);
                    });

                    map2.Assign(map);

                    string buf = TestUtils.GetTagStreamText(map2, 1);
                    Assert.AreEqual("1 MAP\r\n" +
                                    "2 LATI 5.111111\r\n" +
                                    "2 LONG 7.999999\r\n", buf);
                }

                Assert.IsFalse(map.IsEmpty());
                map.Clear();
                Assert.IsTrue(map.IsEmpty());
            }
        }
    }
}
