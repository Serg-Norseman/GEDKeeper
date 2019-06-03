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
    public class GDMIndividualOrdinanceTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMIndividualOrdinance iOrd = GDMIndividualOrdinance.Create(null, "", "") as GDMIndividualOrdinance) {
                Assert.IsNotNull(iOrd);

                Assert.IsNotNull(iOrd.Date);

                iOrd.TempleCode = "temple code";
                Assert.AreEqual("temple code", iOrd.TempleCode);

                iOrd.Place.StringValue = "test place";
                Assert.AreEqual("test place", iOrd.Place.StringValue);
                
                iOrd.BaptismDateStatus = GDMBaptismDateStatus.bdsCompleted;
                Assert.AreEqual(GDMBaptismDateStatus.bdsCompleted, iOrd.BaptismDateStatus);

                iOrd.EndowmentDateStatus = GDMEndowmentDateStatus.edsExcluded;
                Assert.AreEqual(GDMEndowmentDateStatus.edsExcluded, iOrd.EndowmentDateStatus);
                
                Assert.IsNotNull(iOrd.Family);
                
                iOrd.ChildSealingDateStatus = GDMChildSealingDateStatus.cdsPre1970;
                Assert.AreEqual(GDMChildSealingDateStatus.cdsPre1970, iOrd.ChildSealingDateStatus);
                
                Assert.IsNotNull(iOrd.DateStatus);
            }
        }
    }
}
