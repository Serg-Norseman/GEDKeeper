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
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMChildToFamilyLinkTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMChildToFamilyLink childLink = new GDMChildToFamilyLink(null)) {
                Assert.IsNotNull(childLink);

                childLink.ChildLinkageStatus = GDMChildLinkageStatus.clChallenged;
                Assert.AreEqual(GDMChildLinkageStatus.clChallenged, childLink.ChildLinkageStatus);

                childLink.PedigreeLinkageType = GDMPedigreeLinkageType.plFoster;
                Assert.AreEqual(GDMPedigreeLinkageType.plFoster, childLink.PedigreeLinkageType);

                using (GDMChildToFamilyLink childLink2 = new GDMChildToFamilyLink(null)) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        childLink2.Assign(null);
                    });

                    childLink2.Assign(childLink);

                    string buf = TestUtils.GetTagStreamText(childLink2, 1);
                    Assert.AreEqual("1 FAMC\r\n" +
                                    "2 STAT challenged\r\n" +
                                    "2 PEDI foster\r\n", buf);
                }
            }
        }
    }
}
