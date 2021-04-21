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
    public class GDMChangeDateTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMChangeDate cd = new GDMChangeDate()) {
                Assert.IsNotNull(cd);

                DateTime dtNow = DateTime.Now;
                dtNow = dtNow.AddTicks(-dtNow.Ticks % 10000000);
                cd.ChangeDateTime = dtNow;

                DateTime dtx = cd.ChangeDateTime;
                Assert.AreEqual(dtNow, dtx);

                Assert.AreEqual(dtNow.ToString("yyyy.MM.dd HH:mm:ss"), cd.ToString());

                cd.ChangeDateTime = TestUtils.ParseDTX("04.01.2013 11:12");
                using (GDMChangeDate chd2 = new GDMChangeDate()) {
                    Assert.IsNotNull(chd2);

                    Assert.Throws(typeof(ArgumentException), () => {
                        chd2.Assign(null);
                    });

                    chd2.Assign(cd);

                    var iRec = new GDMIndividualRecord(null);
                    iRec.UID = "12345";
                    iRec.ChangeDate.Assign(chd2);

                    string buf = TestUtils.GetTagStreamText(iRec, 0, false);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 _UID 12345\r\n" +
                                    "1 CHAN\r\n" +
                                    "2 DATE 04 JAN 2013\r\n" +
                                    "3 TIME 11:12:00\r\n" +
                                    "1 SEX U\r\n", buf);
                }

                Assert.IsFalse(cd.IsEmpty());
                cd.Clear();
                Assert.IsTrue(cd.IsEmpty());
            }
        }
    }
}
