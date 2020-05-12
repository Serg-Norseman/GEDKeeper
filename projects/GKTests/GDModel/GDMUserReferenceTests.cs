﻿/*
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
using GDModel.Providers.GEDCOM;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMUserReferenceTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMUserReference userRef = new GDMUserReference(null)) {
                Assert.IsNotNull(userRef);

                userRef.StringValue = "ref";

                userRef.ReferenceType = "test";
                Assert.AreEqual("test", userRef.ReferenceType);

                using (GDMUserReference uref2 = new GDMUserReference(null)) {
                    Assert.IsNotNull(uref2);

                    Assert.Throws(typeof(ArgumentException), () => {
                        uref2.Assign(null);
                    });

                    uref2.Assign(userRef);

                    string buf = TestUtils.GetTagStreamText(uref2, 1);
                    Assert.AreEqual("1 REFN ref\r\n"+
                                    "2 TYPE test\r\n", buf);
                }

                Assert.IsFalse(userRef.IsEmpty());
                userRef.Clear();
                Assert.IsTrue(userRef.IsEmpty());
            }
        }
    }
}
