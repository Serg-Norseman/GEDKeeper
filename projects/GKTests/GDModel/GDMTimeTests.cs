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
using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMTimeTests
    {
        [Test]
        public void Test_Common()
        {
            var tms = new TimeSpan(20, 20, 20);

            using (GDMTime time = new GDMTime(null)) {
                Assert.IsNotNull(time, "time != null");
                Assert.AreEqual(GEDCOMTagType.TIME, time.GetTagType());

                time.Value = tms;
                Assert.AreEqual(tms, time.Value);

                time.ParseString("20:20:20.100");
                Assert.AreEqual(20, time.Hour);
                Assert.AreEqual(20, time.Minutes);
                Assert.AreEqual(20, time.Seconds);
                Assert.AreEqual(100, time.Fraction);

                time.Fraction = 200;
                Assert.AreEqual(200, time.Fraction);

                Assert.AreEqual("20:20:20.200", time.StringValue);
                Assert.IsFalse(time.IsEmpty());
                time.Clear();
                Assert.IsTrue(time.IsEmpty());
                Assert.AreEqual("", time.StringValue);
            }
        }
    }
}
