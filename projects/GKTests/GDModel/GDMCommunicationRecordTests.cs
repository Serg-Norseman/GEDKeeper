/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMCommunicationRecordTests
    {
        private readonly BaseContext fContext;

        public GDMCommunicationRecordTests()
        {
            fContext = TestUtils.CreateContext();
        }

        [Test]
        public void Test_Common()
        {
            GDMIndividualRecord iRec = fContext.Tree.CreateIndividual();
            Assert.IsNotNull(iRec);

            using (GDMCommunicationRecord comRec = fContext.Tree.CreateCommunication()) {
                comRec.CommName = "Test Communication";
                Assert.AreEqual("Test Communication", comRec.CommName);

                comRec.CommunicationType = GDMCommunicationType.ctFax;
                Assert.AreEqual(GDMCommunicationType.ctFax, comRec.CommunicationType);

                comRec.Date.Date = TestUtils.ParseDT("23.01.2013");
                Assert.AreEqual(TestUtils.ParseDT("23.01.2013"), comRec.Date.Date);

                comRec.SetCorresponder(GDMCommunicationDir.cdFrom, iRec);
                Assert.AreEqual(GDMCommunicationDir.cdFrom, comRec.CommDirection);
                Assert.AreEqual(iRec, fContext.Tree.GetPtrValue<GDMRecord>(comRec.Corresponder));

                comRec.SetCorresponder(GDMCommunicationDir.cdTo, iRec);
                Assert.AreEqual(GDMCommunicationDir.cdTo, comRec.CommDirection);
                Assert.AreEqual(iRec, fContext.Tree.GetPtrValue<GDMRecord>(comRec.Corresponder));

                using (GDMCommunicationRecord comm2 = fContext.Tree.CreateCommunication()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        comm2.Assign(null);
                    });

                    comm2.Assign(comRec);

                    string buf = GEDCOMProvider.GetTagStreamText(comm2, 0);
                    Assert.AreEqual("0 @CM2@ _COMM\r\n" +
                                    "1 DATE 23 JAN 2013\r\n" +
                                    "1 NAME Test Communication\r\n" +
                                    "1 TYPE fax\r\n" +
                                    "1 TO @I1@\r\n", buf);
                }

                comRec.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(comRec.IsEmpty());
                comRec.Clear();
                Assert.IsTrue(comRec.IsEmpty());
            }
        }
    }
}
