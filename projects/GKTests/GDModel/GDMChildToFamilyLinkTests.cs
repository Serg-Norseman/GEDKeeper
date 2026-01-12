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
    public class GDMChildToFamilyLinkTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMChildToFamilyLink childLink = new GDMChildToFamilyLink()) {
                Assert.IsNotNull(childLink);

                childLink.ChildLinkageStatus = GDMChildLinkageStatus.clChallenged;
                Assert.AreEqual(GDMChildLinkageStatus.clChallenged, childLink.ChildLinkageStatus);

                childLink.PedigreeLinkageType = GDMPedigreeLinkageType.plFoster;
                Assert.AreEqual(GDMPedigreeLinkageType.plFoster, childLink.PedigreeLinkageType);

                using (GDMChildToFamilyLink childLink2 = new GDMChildToFamilyLink()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        childLink2.Assign(null);
                    });

                    var iRec = new GDMIndividualRecord(null);
                    childLink2.Assign(childLink);
                    iRec.ChildToFamilyLinks.Add(childLink2);

                    string buf = GEDCOMProvider.GetTagStreamText(iRec, 0);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 SEX U\r\n" +
                                    "1 FAMC\r\n" +
                                    "2 STAT challenged\r\n" +
                                    "2 PEDI foster\r\n", buf);
                }
            }
        }
    }
}
