/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
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

                    string buf = GEDCOMProvider.GetTagStreamText(iRec, 0, false);
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
