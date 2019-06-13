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
using GKCore.Types;
using GKTests;
using GKUI.Providers;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMIndividualRecordTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            // TempDirtyHack: some functions are references to GlobalOptions (and GfxInit)
            // TODO: replace to mocks
            WFAppHost.ConfigureBootstrap(false);

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            GDMCustomEvent evt, evtd;

            GEDCOMRecordTest(iRec);

            evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);

            evtd = iRec.FindEvent(GEDCOMTagType.DEAT);
            Assert.IsNotNull(evtd);

            Assert.IsFalse(iRec.IsLive());

            GDMIndividualRecord ind3 = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(ind3.GetParentsFamily());

            GDMIndividualRecord ind2 = fContext.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            Assert.IsNotNull(ind2.GetMarriageFamily());

            //
            GDMIndividualRecord indiRec = fContext.Tree.XRefIndex_Find("I4") as GDMIndividualRecord;
            Assert.IsNull(indiRec.GetMarriageFamily());
            Assert.IsNotNull(indiRec.GetMarriageFamily(true));

            //
            Assert.Throws(typeof(ArgumentException), () => { indiRec.Assign(null); });

            indiRec.AutomatedRecordID = "test11";
            Assert.AreEqual("test11", indiRec.AutomatedRecordID);

            Assert.AreEqual(GDMRecordType.rtIndividual, indiRec.RecordType);

            Assert.AreEqual(4, indiRec.GetId());
            Assert.AreEqual("4", indiRec.GetXRefNum());

            Assert.AreEqual(-1, indiRec.IndexOfSource(null));

            indiRec.AddUserRef("test userref");
            Assert.AreEqual("test userref", indiRec.UserReferences[0].StringValue);

            //
            Assert.IsNotNull(indiRec.Aliases);
            Assert.IsNotNull(indiRec.Associations);
            Assert.IsNotNull(indiRec.Submittors);
            Assert.IsNotNull(indiRec.UserReferences); // for GEDCOMRecord

            Assert.Throws(typeof(ArgumentException), () => {
                indiRec.AddEvent(new GDMFamilyEvent(null));
            });

            GDMIndividualRecord father, mother;
            GDMFamilyRecord fam = indiRec.GetParentsFamily();
            if (fam == null) {
                father = null;
                mother = null;
            } else {
                father = fam.GetHusband();
                mother = fam.GetWife();
            }

            Assert.IsNull(father);
            Assert.IsNull(mother);

            indiRec.Sex = GDMSex.svMale;
            Assert.AreEqual(GDMSex.svMale, indiRec.Sex);

            indiRec.Restriction = GDMRestriction.rnLocked;
            Assert.AreEqual(GDMRestriction.rnLocked, indiRec.Restriction);

            indiRec.Patriarch = true;
            Assert.AreEqual(true, indiRec.Patriarch);
            indiRec.Patriarch = false;
            Assert.AreEqual(false, indiRec.Patriarch);

            indiRec.Bookmark = true;
            Assert.AreEqual(true, indiRec.Bookmark);
            indiRec.Bookmark = false;
            Assert.AreEqual(false, indiRec.Bookmark);

            indiRec.AncestralFileNumber = "test11";
            Assert.AreEqual("test11", indiRec.AncestralFileNumber);

            indiRec.PermanentRecordFileNumber = "test22";
            Assert.AreEqual("test22", indiRec.PermanentRecordFileNumber);

            Assert.Throws(typeof(ArgumentException), () => { indiRec.MoveTo(null, false); });

            using (GDMIndividualRecord copyIndi = new GDMIndividualRecord(null)) {
                Assert.IsNotNull(copyIndi);

                Assert.Throws(typeof(ArgumentException), () => { copyIndi.Assign(null); });

                copyIndi.Assign(indiRec);
                Assert.AreEqual(100.0f, indiRec.IsMatch(copyIndi, new MatchParams()));
            }


            Assert.IsFalse(indiRec.IsEmpty());
            indiRec.Clear();
            Assert.IsTrue(indiRec.IsEmpty());

            float ca = indiRec.GetCertaintyAssessment();
            Assert.AreEqual(0.0f, ca);


            Assert.IsNull(indiRec.GetPrimaryMultimediaLink());
            GDMMultimediaLink mmLink = indiRec.SetPrimaryMultimediaLink(null);
            Assert.IsNull(mmLink);
            GDMMultimediaRecord mmRec = fContext.Tree.CreateMultimedia();
            mmLink = indiRec.SetPrimaryMultimediaLink(mmRec);
            Assert.IsNotNull(mmLink);
            mmLink = indiRec.GetPrimaryMultimediaLink();
            Assert.AreEqual(mmRec, mmLink.Value);


            Assert.AreEqual(-1, indiRec.IndexOfGroup(null));
            Assert.AreEqual(-1, indiRec.IndexOfSpouse(null));


            GDMIndividualRecord indi2 = fContext.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            GDMAssociation asso = indiRec.AddAssociation("test", indi2);
            Assert.IsNotNull(asso);

            using (GDMIndividualRecord indi = new GDMIndividualRecord(fContext.Tree)) {
                Assert.IsNotNull(indi);

                var parts = GKUtils.GetNameParts(indi); // test with empty PersonalNames
                Assert.AreEqual("", parts.Surname);
                Assert.AreEqual("", parts.Name);
                Assert.AreEqual("", parts.Patronymic);

                indi.AddPersonalName(new GDMPersonalName(indi)); // test with empty Name
                parts = GKUtils.GetNameParts(indi);
                Assert.AreEqual("", parts.Surname);
                Assert.AreEqual("", parts.Name);
                Assert.AreEqual("", parts.Patronymic);
                indi.PersonalNames.Clear();

                string st;
                Assert.AreEqual("", GKUtils.GetNameString(indi, true, false));
                Assert.AreEqual("", GKUtils.GetNickString(indi));

                GDMPersonalName pName = new GDMPersonalName(indi);
                indi.AddPersonalName(pName);
                pName.Pieces.Nickname = "BigHead";
                pName.SetNameParts("Ivan", "Petrov", "");

                st = GKUtils.GetNameString(indi, true, true);
                Assert.AreEqual("Petrov Ivan [BigHead]", st);
                st = GKUtils.GetNameString(indi, false, true);
                Assert.AreEqual("Ivan Petrov [BigHead]", st);
                Assert.AreEqual("BigHead", GKUtils.GetNickString(indi));

                Assert.IsNull(indi.GetParentsFamily());
                Assert.IsNotNull(indi.GetParentsFamily(true));

                // MoveTo test
                GDMIndividualRecord ind = fContext.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;

                indi.AddAssociation("test", ind);
                indi.Aliases.Add(new GDMAlias(indi));
                indi.Submittors.Add(new GDMPointer(indi, "", ""));

                using (GDMIndividualRecord indi3 = new GDMIndividualRecord(fContext.Tree)) {
                    indi.MoveTo(indi3, false);

                    st = GKUtils.GetNameString(indi3, true, true);
                    Assert.AreEqual("Petrov Ivan [BigHead]", st);
                }

                indi.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, indi.GetTree());
            }

            indiRec.ReplaceXRefs(new GDMXRefReplacer());
        }

        private static void GEDCOMRecordTest(GDMRecord rec)
        {
            Assert.Throws(typeof(ArgumentException), () => { rec.Assign(null); });

            rec.AutomatedRecordID = "test11";
            Assert.AreEqual("test11", rec.AutomatedRecordID);

            Assert.AreEqual(GDMRecordType.rtIndividual, rec.RecordType);

            Assert.AreEqual(1, rec.GetId());
            Assert.AreEqual("1", rec.GetXRefNum());

            Assert.AreEqual(-1, rec.IndexOfSource(null));

            rec.AddUserRef("test userref");
            Assert.AreEqual("test userref", rec.UserReferences[0].StringValue);
        }

        [Test]
        public void Test_IndiMathes()
        {
            GDMTree tree = new GDMTree();
            Assert.IsNotNull(tree);

            GDMIndividualRecord ind1, ind2;
            GDMCustomEvent ev1, ev2;
            GDMDateValue dtVal1, dtVal2;

            ind1 = tree.CreateIndividual();
            ind1.Sex = GDMSex.svMale;
            GDMPersonalName pn = ind1.AddPersonalName(new GDMPersonalName(ind1));
            pn.SetNameParts("Ivan Ivanov", "Fedoroff", "");

            ind2 = tree.CreateIndividual();
            ind2.Sex = GDMSex.svMale;
            pn = ind2.AddPersonalName(new GDMPersonalName(ind2));
            pn.SetNameParts("Ivan Ivanovich", "Fedoroff", "");

            ev1 = new GDMIndividualEvent(ind1, GEDCOMTagType.BIRT, "");
            dtVal1 = ev1.Date;
            ind1.AddEvent(ev1);

            ev2 = new GDMIndividualEvent(ind2, GEDCOMTagType.BIRT, "");
            dtVal2 = ev2.Date;
            ind2.AddEvent(ev2);

            float res;
            MatchParams mParams;
            mParams.NamesIndistinctThreshold = 1.0f;
            mParams.DatesCheck = true;
            mParams.YearsInaccuracy = 0;
            mParams.CheckEventPlaces = false;

            // null
            res = dtVal1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            // null
            res = ev1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            // dtVal1 -> dtVal2, delta = 0
            dtVal1.SetDateTime(DateTime.Parse("10.10.2013"));
            dtVal2.SetDateTime(DateTime.Parse("10.10.2013"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(100.0f, res);

            // ev1 -> ev2, delta = 0
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(100.0f, res);

            // dtVal1 -> dtVal2, delta = 3
            mParams.YearsInaccuracy = 3;

            dtVal2.SetDateTime(DateTime.Parse("10.10.2015"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(100.0f, res);

            // ev1 -> ev2, delta = 3
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(100.0f, res);

            dtVal2.SetDateTime(DateTime.Parse("10.10.2009"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(0.0f, res);

            // ev1 -> ev2, delta = 3
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(0.0f, res);

            // //

            res = ind1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            res = ind1.IsMatch(ind2, mParams);
            Assert.AreEqual(0.0f, res);

            // Ivanov - Ivanov(ich) : 3 chars of difference -> 0.88
            mParams.NamesIndistinctThreshold = 0.85f;
            mParams.YearsInaccuracy = 4;

            res = ind1.IsMatch(ind2, mParams);
            Assert.AreEqual(100.0f, res);
        }

        [Test]
        public void Test_GDMAlias()
        {
            using (GDMAlias alias = new GDMAlias(null)) {
                Assert.IsNotNull(alias, "alias != null");
            }
        }
    }
}
