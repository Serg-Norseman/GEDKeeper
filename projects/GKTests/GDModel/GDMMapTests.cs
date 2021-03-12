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
using GDModel;
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
            using (var map = new GDMMap(null)) {
                map.Lati = 5.111111;
                Assert.AreEqual(5.111111, map.Lati);

                map.Long = 7.999999;
                Assert.AreEqual(7.999999, map.Long);

                using (GDMMap map2 = new GDMMap(null)) {
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
