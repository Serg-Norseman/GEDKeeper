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
    public class GDMUserReferenceTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMUserReference userRef = new GDMUserReference()) {
                Assert.IsNotNull(userRef);

                userRef.StringValue = "ref";

                userRef.ReferenceType = "test";
                Assert.AreEqual("test", userRef.ReferenceType);

                using (GDMUserReference uref2 = new GDMUserReference()) {
                    Assert.IsNotNull(uref2);

                    Assert.Throws(typeof(ArgumentException), () => {
                        uref2.Assign(null);
                    });

                    uref2.Assign(userRef);

                    // test of output format
                    var iRec = new GDMIndividualRecord(null);
                    iRec.UserReferences.Add(uref2);
                    string buf = GEDCOMProvider.GetTagStreamText(iRec, 0);
                    Assert.AreEqual("0 INDI\r\n"+
                                    "1 REFN ref\r\n"+
                                    "2 TYPE test\r\n"+
                                    "1 SEX U\r\n", buf);
                }

                Assert.IsFalse(userRef.IsEmpty());
                userRef.Clear();
                Assert.IsTrue(userRef.IsEmpty());
            }
        }
    }
}
