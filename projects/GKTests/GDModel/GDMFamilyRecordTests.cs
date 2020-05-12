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
using GKCore;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMFamilyRecordTests
    {
        [Test]
        public void Test_Common()
        {
            GDMTree tree = new GDMTree();
            Assert.IsNotNull(tree);

            GDMIndividualRecord indiv = tree.CreateIndividual();
            Assert.IsNotNull(indiv);

            GDMFamilyRecord famRec = tree.CreateFamily();
            Assert.IsNotNull(famRec);

            famRec.Restriction = GDMRestriction.rnLocked;
            Assert.AreEqual(GDMRestriction.rnLocked, famRec.Restriction);

            famRec.AddChild(indiv);
            Assert.AreEqual(0, famRec.IndexOfChild(indiv));

            famRec.Husband.Value = tree.CreateIndividual();
            famRec.Wife.Value = tree.CreateIndividual();

            using (GDMFamilyRecord fam2 = tree.CreateFamily()) {
                Assert.Throws(typeof(ArgumentException), () => {
                    fam2.Assign(null);
                });

                fam2.Assign(famRec);

                string buf = TestUtils.GetTagStreamText(fam2, 0);
                Assert.AreEqual("0 @F2@ FAM\r\n" +
                                "1 RESN locked\r\n" +
                                "1 HUSB @I2@\r\n" +
                                "1 WIFE @I3@\r\n" +
                                "1 CHIL @I1@\r\n", buf);
            }

            // Integrity test
            GDMChildToFamilyLink childLink = indiv.ChildToFamilyLinks[0];
            Assert.IsNotNull(childLink.Family);

            famRec.RemoveChild(indiv);
            Assert.AreEqual(-1, famRec.IndexOfChild(indiv));

            //

            Assert.Throws(typeof(ArgumentException), () => {
                famRec.AddEvent(new GDMIndividualEvent(null));
            });

            famRec.ReplaceXRefs(new GDMXRefReplacer());

            //

            famRec.Husband.Value = indiv;
            Assert.AreEqual(indiv, famRec.Husband.Individual);
            famRec.Husband.Value = null;

            //

            famRec.Wife.Value = indiv;
            Assert.AreEqual(indiv, famRec.Wife.Individual);
            famRec.Wife.Value = null;

            //

            indiv.Sex = GDMSex.svMale;
            famRec.AddSpouse(indiv);
            Assert.AreEqual(0, indiv.IndexOfSpouse(famRec));
            Test_GDMSpouseToFamilyLink(indiv.SpouseToFamilyLinks[0]);
            Assert.IsNull(famRec.GetSpouseBy(indiv));
            famRec.RemoveSpouse(indiv);

            indiv.Sex = GDMSex.svFemale;
            famRec.AddSpouse(indiv);
            Assert.AreEqual(0, indiv.IndexOfSpouse(famRec));
            Test_GDMSpouseToFamilyLink(indiv.SpouseToFamilyLinks[0]);
            Assert.IsNull(famRec.GetSpouseBy(indiv));
            famRec.RemoveSpouse(indiv);

            //

            famRec.SortChilds();

            //

            famRec.AddChild(null);
            famRec.RemoveChild(null);
            famRec.AddSpouse(null);
            famRec.RemoveSpouse(null);

            //
            famRec.AddSpouse(indiv);

            Assert.IsFalse(famRec.IsEmpty());
            famRec.Clear();
            Assert.IsTrue(famRec.IsEmpty());
        }

        public static void Test_GDMSpouseToFamilyLink(GDMSpouseToFamilyLink spouseLink)
        {
            Assert.IsNotNull(spouseLink.Family);

            using (spouseLink = new GDMSpouseToFamilyLink(null)) {
                Assert.IsNotNull(spouseLink);
            }
        }

        [Test]
        public void Test_Common2()
        {
            GDMTree tree = new GDMTree();

            using (GDMFamilyRecord famRec = new GDMFamilyRecord(tree)) {
                Assert.IsNotNull(famRec);

                GDMIndividualRecord unkInd = new GDMIndividualRecord(null);
                unkInd.Sex = GDMSex.svUnknown;
                Assert.IsFalse(famRec.AddSpouse(unkInd));

                GDMIndividualRecord child1 = tree.CreateIndividual(); // for pointer need a proper object
                Assert.IsTrue(famRec.AddChild(child1));

                GDMIndividualRecord child2 = tree.CreateIndividual(); // for pointer need a proper object
                Assert.IsTrue(famRec.AddChild(child2));
                Assert.AreEqual(1, famRec.IndexOfChild(child2));

                famRec.DeleteChild(child1);
                Assert.AreEqual(-1, famRec.IndexOfChild(child1));

                string str = GKUtils.GetFamilyString(famRec, null, null);
                Assert.AreEqual("? - ?", str);

                str = GKUtils.GetFamilyString(famRec, "x", "x");
                Assert.AreEqual("x - x", str);

                Assert.AreEqual(0.0f, famRec.IsMatch(null, new MatchParams()));
                Assert.AreEqual(100.0f, famRec.IsMatch(famRec, new MatchParams()));

                // MoveTo test
                Assert.Throws(typeof(ArgumentException), () => {
                    famRec.MoveTo(null, false);
                });

                GDMCustomEvent evt = famRec.AddEvent(new GDMFamilyEvent(famRec, (int)GEDCOMTagType.MARR, "01 SEP 1981"));
                Assert.AreEqual(1, famRec.Events.Count);
                Assert.AreEqual(evt, famRec.FindEvent(GEDCOMTagType.MARR));

                using (GDMFamilyRecord famRec2 = new GDMFamilyRecord(tree)) {
                    Assert.AreEqual(0, famRec2.Events.Count);
                    Assert.AreEqual(null, famRec2.FindEvent(GEDCOMTagType.MARR));

                    famRec.MoveTo(famRec2, false);

                    Assert.AreEqual(1, famRec2.Events.Count);
                    Assert.AreEqual(evt, famRec2.FindEvent(GEDCOMTagType.MARR));
                }

                famRec.ResetOwner(tree);
                Assert.AreEqual(tree, famRec.GetTree());
            }
        }
    }
}
