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

                string buf = TestUtils.GetTagStreamText(locRec, 0);
                Assert.AreEqual("0 @L1@ _LOC\r\n" +
                                "1 MAP\r\n" +
                                "2 LATI 5.111111\r\n" +
                                "2 LONG 7.999999\r\n" +
                                "1 NAME Test Location\r\n", buf);

                Assert.IsFalse(locRec.IsEmpty());
                locRec.Clear();
                Assert.IsTrue(locRec.IsEmpty());

                buf = TestUtils.GetTagStreamText(locRec, 0);
                Assert.AreEqual("0 @L1@ _LOC\r\n", buf);
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
            }
        }

        [Test]
        public void Test_GDMMap()
        {
            using (GDMMap map = GDMMap.Create(null, "", "") as GDMMap) {
                map.Lati = 5.11111;
                Assert.AreEqual(5.11111, map.Lati);

                map.Long = 7.99999;
                Assert.AreEqual(7.99999, map.Long);
            }
        }
    }
}
