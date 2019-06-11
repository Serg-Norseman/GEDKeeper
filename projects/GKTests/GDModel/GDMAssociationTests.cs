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
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMAssociationTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_GEDCOMAssociation()
        {
            using (GDMAssociation association = new GDMAssociation(fContext.Tree)) {
                Assert.IsNotNull(association);

                Assert.IsNotNull(association.SourceCitations);

                Assert.IsNotNull(association.Notes); // for GEDCOMPointerWithNotes

                association.Relation = "This is test relation";
                Assert.AreEqual("This is test relation", association.Relation);

                association.Individual = null;
                Assert.IsNull(association.Individual);

                GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec);

                association.Individual = iRec;
                Assert.IsNotNull(association.Individual);

                using (GDMAssociation asso2 = new GDMAssociation(null)) {
                    Assert.IsNotNull(asso2);

                    Assert.Throws(typeof(ArgumentException), () => {
                        asso2.Assign(null);
                    });

                    asso2.Assign(association);

                    string buf = TestUtils.GetTagStreamText(asso2, 1);
                    Assert.AreEqual("1 ASSO @I1@\r\n" +
                                    "2 RELA This is test relation\r\n", buf);
                }

                association.ReplaceXRefs(new GDMXRefReplacer());

                GDMTag tag = association.SourceCitations.Add(new GDMSourceCitation(association, GEDCOMTagType.SOUR, "xxx"));
                Assert.IsNotNull(tag);
                Assert.IsTrue(tag is GDMSourceCitation);

                Assert.IsFalse(association.IsEmpty());
                association.Clear();
                Assert.IsTrue(association.IsEmpty());
            }
        }
    }
}
