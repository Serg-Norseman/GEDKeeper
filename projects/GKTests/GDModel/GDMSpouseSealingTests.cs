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
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSpouseSealingTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMSpouseSealing spouseSealing = GDMSpouseSealing.Create(null, "", "") as GDMSpouseSealing) {
                Assert.IsNotNull(spouseSealing);

                Assert.IsNotNull(spouseSealing.Date);

                spouseSealing.TempleCode = "temple code";
                Assert.AreEqual("temple code", spouseSealing.TempleCode);

                spouseSealing.Place.StringValue = "test place";
                Assert.AreEqual("test place", spouseSealing.Place.StringValue);

                spouseSealing.SpouseSealingDateStatus = GDMSpouseSealingDateStatus.sdsCanceled;
                Assert.AreEqual(GDMSpouseSealingDateStatus.sdsCanceled, spouseSealing.SpouseSealingDateStatus);

                Assert.IsNotNull(spouseSealing.DateStatus);
            }
        }
    }
}
