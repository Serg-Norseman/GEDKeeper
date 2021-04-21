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
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMMapTests
    {
        [Test]
        public void Test_Common()
        {
            using (var map = new GDMMap()) {
                map.Lati = 5.111111;
                Assert.AreEqual(5.111111, map.Lati);

                map.Long = 7.999999;
                Assert.AreEqual(7.999999, map.Long);

                using (GDMMap map2 = new GDMMap()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        map2.Assign(null);
                    });

                    //map2.Assign(map);

                    var iRec = new GDMIndividualRecord(null);
                    var evt = new GDMIndividualEvent();
                    evt.SetName("BIRT");
                    iRec.Events.Add(evt);
                    evt.Place.Map.Assign(map);
                    string buf = TestUtils.GetTagStreamText(iRec, 1);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 SEX U\r\n" +
                                    "1 BIRT\r\n" +
                                    "2 PLAC\r\n" +
                                    "3 MAP\r\n" +
                                    "4 LATI 5.111111\r\n" +
                                    "4 LONG 7.999999\r\n", buf);
                }

                Assert.IsFalse(map.IsEmpty());
                map.Clear();
                Assert.IsTrue(map.IsEmpty());
            }
        }
    }
}
