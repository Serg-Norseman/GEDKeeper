/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
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
                    string buf = GEDCOMProvider.GetTagStreamText(iRec, 1);
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
