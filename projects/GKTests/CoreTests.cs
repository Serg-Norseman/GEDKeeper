/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.IO;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Cultures;
using GKCore.Kinships;
using GKCore.Types;
using NUnit.Framework;

namespace GKTests
{
    [TestFixture]
    public class CoreTests
    {
        BaseContext fContext;
        
        [TestFixtureSetUp]
        public void SetUp()
        {
            Console.WriteLine(@">>> START CoreTests");

            fContext = TestStubs.CreateContext();
            GEDCOMTree tree = fContext.Tree;
            
            TestStubs.FillContext(fContext);
        }
        
        [TestFixtureTearDown]
        public void TearDown()
        {
            Console.WriteLine(@">>> END CoreTests");
        }
        
        [Test]
        public void Context_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.AreEqual(false, fContext.IsChildless(iRec));

            /*GEDCOMSourceRecord srcRec = _context.FindSource("test source");
			Assert.IsNull(srcRec);

			StringList sources = new StringList();
			_context.GetSourcesList(sources);
			Assert.AreEqual(0, sources.Count);*/
        }
        
        [Test]
        public void Utils_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            // individual record tests
            Assert.IsNotNull(iRec);

            string age = GKUtils.GetAgeStr(null, 0);
            Assert.AreEqual("", age);
            
            age = GKUtils.GetAgeStr(iRec, -1);
            Assert.AreEqual("20", age);
            
            age = GKUtils.GetAgeStr(iRec, 2005);
            Assert.AreEqual("15", age);
            
            //
            
            //
            
            string st3;
            st3 = GKUtils.GetBirthDate(null, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("28.12.1990", st3);

            st3 = GKUtils.GetDeathDate(null, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("", st3);
            
            st3 = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("28.12.2010", st3);
            
            st3 = GKUtils.GetLifeStr(iRec);
            Assert.AreEqual(" (28.12.1990 - 28.12.2010)", st3);
            
            //
            
            st3 = GKUtils.GetBirthPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
            
            st3 = GKUtils.GetDeathPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetDeathPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
            
            st3 = GKUtils.GetResidencePlace(null, false);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetResidencePlace(iRec, true);
            Assert.AreEqual("", st3);
            
            //
            
            GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
            Assert.IsNotNull(evt);
            
            string st2 = GKUtils.GEDCOMEventToDateStr(null, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("", st2);
            
            st2 = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("1990.12.28", st2);
            
            //st2 = GKUtils.GetEventName(evt);
            //Assert.AreEqual("BIRT", st2);
            
            //
            
            string st1 = GKUtils.GetAttributeValue(null, "BIRT");
            Assert.AreEqual("", st1);
            
            st1 = GKUtils.GetAttributeValue(iRec, "BIRT");
            Assert.AreEqual("", st1);
            
            Assert.AreEqual(1, GKUtils.GetPersonEventIndex("BIRT"));
            Assert.AreEqual(2, GKUtils.GetFamilyEventIndex("MARR"));
            Assert.AreEqual(1, GKUtils.GetMarriageStatusIndex("MARRIED"));
            
            //
            
            //Assert.AreEqual(1990, context.FindBirthYear(iRec));
            //Assert.AreEqual(-1, context.FindDeathYear(iRec));
            
            string ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
            Assert.AreEqual("28.12.1990", ds);
            
            ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("1990.12.28", ds);
            
            ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY, false);
            Assert.AreEqual("1990", ds);
            
            ds = GKUtils.GEDCOMEventToDateStr(null, DateFormat.dfYYYY, false);
            Assert.AreEqual("", ds);
            
            evt = fContext.CreateEventEx(iRec, "FACT", "17 JAN 2013", "Ivanovo");
            Assert.IsNotNull(evt);
            GedcomTests.GEDCOMCustomEventTest(evt, "17.01.2013");
            
            string dst = GKUtils.CompactDate("__.__.2013");
            Assert.AreEqual("2013", dst);
            
            GEDCOMFamilyRecord fRec = fContext.Tree.CreateFamily();
            Assert.IsNotNull(fRec);
            
            evt = fContext.CreateEventEx(fRec, "MARR", "28 DEC 2013", "Ivanovo");
            Assert.IsNotNull(evt);
            GedcomTests.GEDCOMCustomEventTest(evt, "28.12.2013");
            
            // sex tests
            GEDCOMSex sex;
            sex = GKUtils.GetSexBySign('F');
            Assert.AreEqual(GEDCOMSex.svFemale, sex);
            sex = GKUtils.GetSexBySign('M');
            Assert.AreEqual(GEDCOMSex.svMale, sex);
            sex = GKUtils.GetSexBySign('U');
            Assert.AreEqual(GEDCOMSex.svUndetermined, sex);
            
            // path tests
            string st = GKUtils.GetTempDir();
            Assert.IsTrue(Directory.Exists(st));
            
            st = GKUtils.GetAppPath();
            Assert.IsTrue(Directory.Exists(st));
            
            // matches tests
            bool res = GKUtils.MatchesMask("abrakadabra", "*kad*");
            Assert.IsTrue(res);
            
            res = GKUtils.MatchesMask("abrakadabra", "*test*");
            Assert.IsFalse(res);

            GEDCOMListTests(iRec);

            //

            long val = GKUtils.Trunc(495.575);
            Assert.AreEqual(val, 495);

            Assert.AreEqual(3.0f, GKUtils.SafeDiv(9.0f, 3.0f));
            Assert.AreEqual(0.0f, GKUtils.SafeDiv(9.0f, 0.0f));
            
            // access tests
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.None));
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.None));
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.None));

            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.Middle));
            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.Middle));
            Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.Middle));

            Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.Maximum));
            Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.Maximum));
            Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.Maximum));
            
            st1 = GKUtils.HyperLink("@X001@", "test", 0);
            Assert.AreEqual("~^" + "@X001@" + ":" + "test" + "~", st1);
        }
        
        private const int REP_COUNT = 1000; // 1000000; // for profile tests

        private static void GEDCOMListTests(GEDCOMIndividualRecord iRec)
        {
            //GEDCOMListTest_Hot(iRec);
            
            for (int k = 0; k < REP_COUNT; k++) {
                GEDCOMListTest11(iRec);
                GEDCOMListTest12(iRec);
                GEDCOMListTest21(iRec);
                GEDCOMListTest22(iRec);
                GEDCOMListTest23(iRec);
                GEDCOMListTest3(iRec);
            }
        }

        /*private void GEDCOMListTest_Hot(GEDCOMIndividualRecord iRec)
		{
			for (int k = 0; k < 100000; k++) {
				GEDCOMListTest11(iRec);
				GEDCOMListTest12(iRec);
				GEDCOMListTest21(iRec);
				GEDCOMListTest22(iRec);
				GEDCOMListTest23(iRec);
				GEDCOMListTest3(iRec);
			}
		}*/

        private static void GEDCOMListTest11(GEDCOMIndividualRecord iRec)
        {
            foreach (GEDCOMCustomEvent evt1 in iRec.Events) {
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest12(GEDCOMIndividualRecord iRec)
        {
            IGEDCOMListEnumerator enumer = iRec.Events.GetEnumerator();
            while (enumer.MoveNext()) {
                GEDCOMCustomEvent evt1 = (GEDCOMCustomEvent)enumer.Current;
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest21(GEDCOMIndividualRecord iRec)
        {
            for (int i = 0; i < iRec.Events.Count; i++) {
                GEDCOMCustomEvent evt1 = iRec.Events[i];
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest22(GEDCOMIndividualRecord iRec)
        {
            for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                GEDCOMCustomEvent evt1 = iRec.Events[i];
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest23(GEDCOMIndividualRecord iRec)
        {
            GEDCOMList<GEDCOMCustomEvent> events = iRec.Events;
            for (int i = 0, num = events.Count; i < num; i++) {
                GEDCOMCustomEvent evt1 = events[i];
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest3(GEDCOMIndividualRecord iRec)
        {
            iRec.Events.ForEach(x => { x.GetHashCode(); });
        }

        [Test]
        public void NamesTable_Tests()
        {
            Assert.AreEqual("", GKUtils.ClearSurname(null));
            Assert.AreEqual("", GKUtils.ClearSurname(""));
            Assert.AreEqual("Иванова", GKUtils.ClearSurname("Иванова (Петрова)"));
            Assert.AreEqual("Иванов", RussianCulture.PrepareRusSurname("Иванова", true));
            Assert.AreEqual("Бельский", RussianCulture.PrepareRusSurname("Бельская", true));
            Assert.AreEqual("Грозный", RussianCulture.PrepareRusSurname("Грозная", true));
            Assert.AreEqual("Иванов", RussianCulture.PrepareRusSurname("Иванов", false));
            Assert.AreEqual("Бельский", RussianCulture.PrepareRusSurname("Бельский", false));
            Assert.AreEqual("Грозный", RussianCulture.PrepareRusSurname("Грозный", false));

            Assert.AreEqual("?", RussianCulture.PrepareRusSurname(null, false));
            Assert.AreEqual("?", RussianCulture.PrepareRusSurname("", false));
            Assert.AreEqual("?", RussianCulture.PrepareRusSurname("(Иванова)", false));

            Assert.AreEqual("Иванова", RussianCulture.GetRusWifeSurname("Иванов"));
            Assert.AreEqual("Бельская", RussianCulture.GetRusWifeSurname("Бельский"));
            Assert.AreEqual("Грозная", RussianCulture.GetRusWifeSurname("Грозный"));

            Assert.AreEqual("?", RussianCulture.GetRusWifeSurname(""));
            Assert.AreEqual("?", RussianCulture.GetRusWifeSurname(null));
            
            string[] snms = GKUtils.GetSurnames("Бельская (Иванова)", true);
            Assert.AreEqual(2, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);
            Assert.AreEqual("Иванов", snms[1]);

            snms = GKUtils.GetSurnames("Бельская", true);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            snms = GKUtils.GetSurnames("Бельский", false);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            //

            GEDCOMSex sx = RussianCulture.GetSex("Мария", "Петровна", false);
            Assert.AreEqual(GEDCOMSex.svFemale, sx);

            sx = RussianCulture.GetSex("Иван", "Петрович", false);
            Assert.AreEqual(GEDCOMSex.svMale, sx);
        }

        [Test]
        public void Kinships_Tests()
        {
            RelationKind rel;
            int great, level;
            
            rel = KinshipsMan.FindKinship(RelationKind.rkFather, RelationKind.rkSon, out great, out level);
            Assert.AreEqual(RelationKind.rkBrother, rel);
            
            rel = KinshipsMan.FindKinship(RelationKind.rkNone, RelationKind.rkSon, out great, out level);
            Assert.AreEqual(RelationKind.rkSon, rel);
        }
    }
}
