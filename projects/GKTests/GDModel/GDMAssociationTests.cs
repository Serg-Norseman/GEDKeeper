/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMAssociationTests
    {
        private readonly BaseContext fContext;

        public GDMAssociationTests()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_GEDCOMAssociation()
        {
            using (GDMAssociation association = new GDMAssociation()) {
                Assert.IsNotNull(association);

                Assert.IsNotNull(association.SourceCitations);

                Assert.IsNotNull(association.Notes); // for GEDCOMPointerWithNotes

                association.Relation = "This is test relation";
                Assert.AreEqual("This is test relation", association.Relation);

                association.XRef = string.Empty;
                Assert.IsNull(fContext.Tree.GetPtrValue(association));

                GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
                Assert.IsNotNull(iRec);

                association.XRef = iRec.XRef;
                Assert.AreEqual(iRec, fContext.Tree.GetPtrValue(association));

                using (GDMAssociation asso2 = new GDMAssociation()) {
                    Assert.IsNotNull(asso2);

                    Assert.Throws(typeof(ArgumentException), () => {
                        asso2.Assign(null);
                    });

                    var iRec2 = new GDMIndividualRecord(null);
                    asso2.Assign(association);
                    iRec2.Associations.Add(asso2);

                    string buf = GEDCOMProvider.GetTagStreamText(iRec2, 0);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 SEX U\r\n" +
                                    "1 ASSO @I1@\r\n" +
                                    "2 RELA This is test relation\r\n", buf);
                }

                association.ReplaceXRefs(new GDMXRefReplacer());

                GDMTag tag = association.SourceCitations.Add(new GDMSourceCitation());
                Assert.IsNotNull(tag);
                Assert.IsTrue(tag is GDMSourceCitation);

                Assert.IsFalse(association.IsEmpty());
                association.Clear();
                Assert.IsTrue(association.IsEmpty());
            }
        }
    }
}
