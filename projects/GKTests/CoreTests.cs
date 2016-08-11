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
using System.Collections.Generic;
using System.IO;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Cultures;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Lists;
using GKCore.Maps;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Stats;
using GKCore.Tools;
using GKCore.Types;
using GKTests.Mocks;
using GKUI.Charts;
using GKUI.Controls;
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
            LangMan.DefInit();

            fContext = TestStubs.CreateContext();
            TestStubs.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        [Test]
        public void Context_Tests()
        {
            GEDCOMSourceRecord srcRec = fContext.FindSource("test source");
            Assert.IsNull(srcRec);

            StringList sources = new StringList();
            fContext.GetSourcesList(sources);
            Assert.AreEqual(1, sources.Count);

            Assert.IsNotNull(fContext.ValuesCollection);

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.AreEqual(false, fContext.IsChildless(iRec));

            Assert.AreEqual(1990, fContext.GetRelativeYear(iRec, "BIRT"));

            Assert.AreEqual(1990, fContext.FindBirthYear(iRec));
            Assert.AreEqual(2010, fContext.FindDeathYear(iRec));

            Assert.IsFalse(fContext.DeleteRecord(null));

            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateIndividual()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateFamily()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateNote()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateMultimedia()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateSource()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateRepository()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateGroup()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateResearch()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateTask()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateCommunication()));
            Assert.IsTrue(fContext.DeleteRecord(fContext.Tree.CreateLocation()));

            //fContext.Clear();
            //Assert.AreEqual(0, fContext.Tree.RecordsCount);

            fContext.BeginUpdate();
            Assert.IsTrue(fContext.IsUpdated());
            fContext.EndUpdate();
            Assert.IsFalse(fContext.IsUpdated());
        }

        private void TransactionEventHandler(object sender, TransactionType type)
        {
        }

        private class InvalidOperation : CustomOperation
        {
            public InvalidOperation(UndoManager manager) : base(manager) { }
            public override bool Redo() { return false; }
            public override void Undo() {}
        }

        [Test]
        public void Undoman_Tests()
        {
            using (UndoManager undoman = new UndoManager(fContext.Tree)) {
                Assert.IsNotNull(undoman);

                Assert.AreEqual(fContext.Tree, undoman.Tree);

                Assert.IsFalse(undoman.CanUndo());
                Assert.IsFalse(undoman.CanRedo());

                undoman.Clear();

                Assert.IsFalse(undoman.DoOperation(null));

                undoman.Undo();
                undoman.Redo();
                undoman.Commit();
                undoman.Rollback();

                Assert.IsFalse(undoman.DoOperation(new InvalidOperation(undoman)));
            }

            fContext.Undoman.OnTransaction += TransactionEventHandler;

            fContext.Undoman.Clear();

            Assert.Throws(typeof(ArgumentNullException), () => { fContext.ChangePersonBookmark(null, true); });
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.ChangePersonPatriarch(null, true); });
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.ChangePersonSex(null, GEDCOMSex.svUndetermined); });

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            iRec.Bookmark = false;
            fContext.ChangePersonBookmark(iRec, true);
            Assert.IsTrue(iRec.Bookmark);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            iRec.Patriarch = false;
            fContext.ChangePersonPatriarch(iRec, true);
            Assert.IsTrue(iRec.Patriarch);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Patriarch);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            iRec.Sex = GEDCOMSex.svUndetermined;
            fContext.ChangePersonSex(iRec, GEDCOMSex.svMale);
            Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            Assert.IsTrue(fContext.Undoman.CanRedo());
            fContext.Undoman.Redo();
            Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());

            fContext.Undoman.Clear();

            iRec.Bookmark = false;
            iRec.Patriarch = false;
            iRec.Sex = GEDCOMSex.svUndetermined;

            fContext.ChangePersonBookmark(iRec, true);
            fContext.ChangePersonPatriarch(iRec, true);
            fContext.ChangePersonSex(iRec, GEDCOMSex.svMale);
            fContext.Undoman.Commit();
            Assert.IsTrue(iRec.Bookmark);
            Assert.IsTrue(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());


            fContext.ChangePersonBookmark(iRec, true);
            fContext.ChangePersonPatriarch(iRec, true);
            fContext.ChangePersonSex(iRec, GEDCOMSex.svMale);
            fContext.Undoman.Rollback();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            fContext.Undoman.OnTransaction -= TransactionEventHandler;
        }

        [Test]
        public void SCCrypt_Tests()
        {
            const string pw = "test password";
            string crypt = SCCrypt.scEncrypt(pw, unchecked((ushort)CRC32.CrcStr("test")));
            string pw1 = SCCrypt.scDecrypt(crypt, unchecked((ushort)CRC32.CrcStr("test")));

            Assert.AreEqual(pw, pw1, "SCCrypt_Test");
        }

        [Test]
        public void Bits_Tests()
        {
            Assert.AreEqual(true, GKUtils.IsSetBit(3, 0));
            Assert.AreEqual(true, GKUtils.IsSetBit(3, 1));
            Assert.AreEqual(false, GKUtils.IsSetBit(3, 4));
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

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.FirstOrDefault<int>(null); });
            int N = GKUtils.FirstOrDefault<int>(new int[] { 5, 7, 10 });
            Assert.AreEqual(5, N);

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.LastOrDefault<int>(null); });
            N = GKUtils.LastOrDefault<int>(new int[] { 5, 7, 10 });
            Assert.AreEqual(10, N);

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SingleOrDefault<int>(null); });
            N = GKUtils.SingleOrDefault<int>(new int[] { 11 });
            Assert.AreEqual(11, N);
            N = GKUtils.SingleOrDefault<int>(new int[] { });
            Assert.AreEqual(0, N);
            Assert.Throws(typeof(Exception), () => { GKUtils.SingleOrDefault<int>(new int[] { 5, 7, 10 }); });

            //

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetPedigreeLifeStr(null, PedigreeFormat.Compact); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetDateFmtString(null, DateFormat.dfDD_MM_YYYY, false, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventName(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventCause(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventDesc(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetAttributeStr(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.InitExtData(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.InitExtCounts(null, 0); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(null, fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(fContext.Tree, null, false); });

            Assert.AreEqual("", GKUtils.GetDaysForBirth(null));
            Assert.AreEqual("", GKUtils.GetTaskGoalStr(null));
            Assert.AreEqual("", GKUtils.GetGoalStr(GKGoalType.gtIndividual, null));

            //

            GEDCOMRecord rec = fContext.Tree.XRefIndex_Find("I1");
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GenRecordName(rec, false));

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

            evt.Detail.Cause = "test cause";
            st2 = GKUtils.GetEventCause(evt);
            Assert.AreEqual("test cause", st2);

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
            
            string dst = GKUtils.CompactDate("__.__.2013");
            Assert.AreEqual("2013", dst);
            
            GEDCOMFamilyRecord fRec = fContext.Tree.CreateFamily();
            Assert.IsNotNull(fRec);
            
            evt = fContext.CreateEventEx(fRec, "MARR", "28 DEC 2013", "Ivanovo");
            Assert.IsNotNull(evt);

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

            //

            int days = GKUtils.DaysBetween(new DateTime(1990, 10, 10), new DateTime(1990, 10, 13));
            Assert.AreEqual(3, days);

            Assert.AreEqual(31, GKUtils.DaysInAMonth(1990, 5));

            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfNone));
            Assert.AreEqual(MultimediaKind.mkImage, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfBMP));
            Assert.AreEqual(MultimediaKind.mkText, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfTXT));
            Assert.AreEqual(MultimediaKind.mkAudio, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfWAV));
            Assert.AreEqual(MultimediaKind.mkVideo, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfAVI));
            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfOLE));

            //

            iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
            string[] surnames = GKUtils.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetSurnames(null); });

            //

            #if !__MonoCS__
            Assert.AreEqual("test.zip", GKUtils.GetContainerName("c:\\temp\\test.ged", true));
            Assert.AreEqual("test\\", GKUtils.GetContainerName("c:\\temp\\test.ged", false));
            #endif

            //

            GEDCOMCustomDate dtx = GKUtils.GetMarriageDate(null);
            Assert.IsNull(dtx);

            //

        }

        [Test]
        public void Search_Tests()
        {
            SearchResult searchResult = new SearchResult(null);
            Assert.IsNotNull(searchResult);

            Assert.Throws(typeof(ArgumentNullException), () => { new BaseSearchStrategy(null, null); });

            BaseSearchStrategy strat = new BaseSearchStrategy(new WorkWindowMock(), "");
            Assert.IsNotNull(strat);

            IList<ISearchResult> res = strat.FindAll();
            Assert.IsNotNull(res);

            Assert.IsFalse(strat.HasResults());
            Assert.IsNull(strat.FindNext());
            Assert.IsNull(strat.FindPrev());
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
        public void Cultures_Tests()
        {
            ICulture culture = new RussianCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());

            culture = new AncientCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());

            culture = new IcelandCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
        }

        [Test]
        public void RussianCulture_Tests()
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

            Assert.AreEqual(GEDCOMSex.svNone, RussianCulture.GetSex("", "", false));
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

        private class ListViewMock : IListView
        {
            public void AddListColumn(string caption, int width, bool autoSize) {}
        }

        private bool ExtFilterHandler(GEDCOMRecord record)
        {
            return true;
        }

        [Test]
        public void Lists_Tests()
        {
            //
            ColumnProps colProps = new ColumnProps();
            Assert.IsNotNull(colProps);

            colProps = new ColumnProps(0, false, 10);
            Assert.IsNotNull(colProps);
            Assert.AreEqual(0, colProps.ColType);
            Assert.AreEqual(false, colProps.ColActive);
            Assert.AreEqual(10, colProps.ColWidth);

            ColumnProps colProps2 = new ColumnProps();
            colProps2.Assign(null);
            colProps2.Assign(colProps);
            Assert.AreEqual(0, colProps.ColType);
            Assert.AreEqual(false, colProps.ColActive);
            Assert.AreEqual(10, colProps.ColWidth);

            //
            ColumnStatic colStatic = new ColumnStatic();
            Assert.IsNotNull(colStatic);

            //
            ListFilter listFilter = new ListFilter();
            Assert.IsNotNull(listFilter);
            Assert.AreEqual(0, listFilter.Conditions.Count);
            listFilter.Clear();
            Assert.AreEqual(0, listFilter.Conditions.Count);

            ListManager listManager;
            ListViewMock lvMock = new ListViewMock();
            GKListItem listItem;

            //
            listManager = new GroupListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(GroupColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMGroupRecord grpRec = fContext.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            listManager.Fetch(grpRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*roup*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            IListFilter filter = listManager.Filter;
            IListColumns listColumns = listManager.ListColumns;

            GroupListColumns copyColumns = new GroupListColumns();
            listColumns.CopyTo(copyColumns);

            Assert.Throws(typeof(ArgumentNullException), () => { listColumns.CopyTo(null); });

            listManager.QuickFilter = "*";
            listManager.AddCondition(GroupColumnType.gctName, ConditionKind.ck_Contains, "*roup*");
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));

            //
            listManager = new CommunicationListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(CommunicationColumnType), listManager.ListColumns.GetColumnsEnum());

            listManager.ExternalFilter = null;
            Assert.IsNull(listManager.ExternalFilter);

            GEDCOMCommunicationRecord commRec = fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord;
            listManager.Fetch(commRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*commun*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new FamilyListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(FamilyColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            listManager.Fetch(famRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "* - *";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new IndividualListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(PersonColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;
            listManager.Fetch(indRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*Petr*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            GlobalOptions.Instance.ListHighlightUnparentedPersons = true;
            GlobalOptions.Instance.ListHighlightUnmarriedPersons = true;
            listManager.InitFilter();
            listManager.ExternalFilter = ExtFilterHandler;

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfFNP;
            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_NP;
            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_N_P;
            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new LocationListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(LocationColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMLocationRecord locRec = fContext.Tree.XRefIndex_Find("L1") as GEDCOMLocationRecord;
            listManager.Fetch(locRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*locat*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new MultimediaListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(MultimediaColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMMultimediaRecord mediaRec = fContext.Tree.XRefIndex_Find("O1") as GEDCOMMultimediaRecord;
            listManager.Fetch(mediaRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*media*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new NoteListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(NoteColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMNoteRecord noteRec = new GEDCOMNoteRecord(null, null, "", "");
            noteRec.AddNoteText("Test text");
            listManager.Fetch(noteRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*text*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);
            noteRec.Clear();
            listManager.UpdateItem(listItem, true);

            //
            listManager = new RepositoryListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(RepositoryColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMRepositoryRecord repoRec = fContext.Tree.XRefIndex_Find("R1") as GEDCOMRepositoryRecord;
            listManager.Fetch(repoRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*repos*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new ResearchListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(ResearchColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMResearchRecord resRec = fContext.Tree.XRefIndex_Find("RS1") as GEDCOMResearchRecord;
            listManager.Fetch(resRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*resear*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new SourceListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(SourceColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            listManager.Fetch(srcRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*sourc*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);

            //
            listManager = new TaskListMan(this.fContext.Tree);
            Assert.IsNotNull(listManager);
            Assert.AreEqual(typeof(TaskColumnType), listManager.ListColumns.GetColumnsEnum());

            GEDCOMTaskRecord tskRec = fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            listManager.Fetch(tskRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*task*";
            Assert.IsTrue(listManager.CheckFilter(ShieldState.None));
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter(ShieldState.None));

            listManager.UpdateColumns(lvMock, true);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, true);
        }

        [Test]
        public void Maps_Tests()
        {
            GMapPoint mapPoint = new GMapPoint(0.5f, 0.5f, "test");
            Assert.IsNotNull(mapPoint);
            Assert.AreEqual(0.5f, mapPoint.Latitude);
            Assert.AreEqual(0.5f, mapPoint.Longitude);
            Assert.AreEqual("test", mapPoint.Hint);

        }

        [Test]
        public void Locales_Tests()
        {
            LangRecord langRecord = new LangRecord(1049, "rus", "Russian", "filename.lng");
            Assert.IsNotNull(langRecord);
            Assert.AreEqual(1049, langRecord.Code);
            Assert.AreEqual("rus", langRecord.Sign);
            Assert.AreEqual("Russian", langRecord.Name);
            Assert.AreEqual("filename.lng", langRecord.FileName);
        }

        [Test]
        public void Options_Tests()
        {
            //GlobalOptions globalOptions = new GlobalOptions();
            //Assert.IsNotNull(globalOptions);

            MRUFile mruFile = new MRUFile();
            Assert.IsNotNull(mruFile);

            mruFile = new MRUFile("test.ged");
            Assert.IsNotNull(mruFile);

            PedigreeOptions pedigreeOptions = new PedigreeOptions();
            Assert.IsNotNull(pedigreeOptions);

            ProxyOptions proxyOptions = new ProxyOptions();
            Assert.IsNotNull(proxyOptions);

            TreeChartOptions treeChartOptions = new TreeChartOptions();
            Assert.IsNotNull(treeChartOptions);
        }

        [Test]
        public void Stats_Tests()
        {
            CompositeItem compositeItem = new CompositeItem();
            Assert.IsNotNull(compositeItem);
            compositeItem.TakeVal(0.0f, GEDCOMSex.svMale, true);
            Assert.AreEqual(0, compositeItem.CommonVal);
            Assert.AreEqual(0, compositeItem.MaleVal);
            Assert.AreEqual(0, compositeItem.FemaleVal);
            compositeItem.TakeVal(1f, GEDCOMSex.svFemale, true);
            compositeItem.TakeVal(1f, GEDCOMSex.svMale, true);
            Assert.AreEqual(1, compositeItem.CommonVal);
            Assert.AreEqual(1, compositeItem.MaleVal);
            Assert.AreEqual(1, compositeItem.FemaleVal);
            compositeItem.TakeVal("1", GEDCOMSex.svFemale, true);
            compositeItem.TakeVal("1", GEDCOMSex.svMale, true);
            Assert.AreEqual(1, compositeItem.CommonVal);
            Assert.AreEqual(1, compositeItem.MaleVal);
            Assert.AreEqual(1, compositeItem.FemaleVal);

            StatsItem statsItem = new StatsItem("test", false);
            Assert.IsNotNull(statsItem);
            Assert.AreEqual("test", statsItem.ToString());

            statsItem = new StatsItem("test2", 0);
            Assert.IsNotNull(statsItem);
            Assert.AreEqual("test2", statsItem.ToString());

            List<GEDCOMRecord> selectedRecords = new List<GEDCOMRecord>();
            IGEDCOMTreeEnumerator iEnum = fContext.Tree.GetEnumerator(GEDCOMRecordType.rtIndividual);
            GEDCOMRecord current;
            while (iEnum.MoveNext(out current)) {
                selectedRecords.Add(current);
            }

            TreeStats treeStats = new TreeStats(fContext.Tree, selectedRecords);
            Assert.IsNotNull(treeStats);

            CommonStats commonStats = treeStats.GetCommonStats();
            Assert.IsNotNull(commonStats);
            Assert.AreEqual(4, commonStats.persons, "Stats.TotalPersons");
            Assert.AreEqual(2, commonStats.persons_m, "Stats.SumM");
            Assert.AreEqual(2, commonStats.persons_f, "Stats.SumF");

            List<StatsItem> values = new List<StatsItem>();

            treeStats.GetSpecStats(StatsMode.smAncestors, values);
            treeStats.GetSpecStats(StatsMode.smDescendants, values);
            treeStats.GetSpecStats(StatsMode.smDescGenerations, values);
            treeStats.GetSpecStats(StatsMode.smFamilies, values);
            treeStats.GetSpecStats(StatsMode.smNames, values);
            treeStats.GetSpecStats(StatsMode.smPatronymics, values);
            treeStats.GetSpecStats(StatsMode.smAge, values);
            treeStats.GetSpecStats(StatsMode.smLifeExpectancy, values);
            treeStats.GetSpecStats(StatsMode.smBirthYears, values);
            treeStats.GetSpecStats(StatsMode.smBirthTenYears, values);
            treeStats.GetSpecStats(StatsMode.smDeathYears, values);
            treeStats.GetSpecStats(StatsMode.smDeathTenYears, values);
            treeStats.GetSpecStats(StatsMode.smChildsCount, values);
            treeStats.GetSpecStats(StatsMode.smChildsDistribution, values);
            treeStats.GetSpecStats(StatsMode.smBirthPlaces, values);
            treeStats.GetSpecStats(StatsMode.smDeathPlaces, values);
            treeStats.GetSpecStats(StatsMode.smResidences, values);
            treeStats.GetSpecStats(StatsMode.smOccupation, values);
            treeStats.GetSpecStats(StatsMode.smReligious, values);
            treeStats.GetSpecStats(StatsMode.smNational, values);
            treeStats.GetSpecStats(StatsMode.smEducation, values);
            treeStats.GetSpecStats(StatsMode.smCaste, values);
            treeStats.GetSpecStats(StatsMode.smFirstbornAge, values);
            treeStats.GetSpecStats(StatsMode.smMarriages, values);
            treeStats.GetSpecStats(StatsMode.smMarriageAge, values);
            treeStats.GetSpecStats(StatsMode.smSpousesDiff, values);
            treeStats.GetSpecStats(StatsMode.smHobby, values);
            treeStats.GetSpecStats(StatsMode.smAward, values);
            treeStats.GetSpecStats(StatsMode.smMili, values);
            treeStats.GetSpecStats(StatsMode.smMiliInd, values);
            treeStats.GetSpecStats(StatsMode.smMiliDis, values);
            treeStats.GetSpecStats(StatsMode.smMiliRank, values);
            treeStats.GetSpecStats(StatsMode.smAAF_1, values);
            treeStats.GetSpecStats(StatsMode.smAAF_2, values);
            treeStats.GetSpecStats(StatsMode.smCertaintyIndex, values);
            treeStats.GetSpecStats(StatsMode.smBirthByMonth, values);
            treeStats.GetSpecStats(StatsMode.smDemography, values);
        }

        [Test]
        public void Tools_Tests()
        {
            PlaceObj placeObj = new PlaceObj();
            Assert.IsNotNull(placeObj);
            Assert.AreEqual(null, placeObj.Name);
            Assert.IsNotNull(placeObj.Facts);

            placeObj.Dispose();
            Assert.IsNotNull(placeObj);


            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);


            List<GEDCOMRecord> walkList = new List<GEDCOMRecord>();
            TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, walkList);
            Assert.AreEqual(3, walkList.Count, "TreeTools.TreeWalk(twmAll)"); // 3 linked from 4 total


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeWalk(null, TreeTools.TreeWalkMode.twmAll, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, null); });

            List<TreeTools.CheckObj> checksList = new List<TreeTools.CheckObj>();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckBase(null, checksList); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckBase(new BaseWindowMock(null), null); });

            TreeTools.CheckBase(new BaseWindowMock(fContext.Tree), checksList);


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GenPatriarchsGraphviz(null, "", 0, false); });


            ValuesCollection valuesCollection = new ValuesCollection();
            ProgressMock progress = new ProgressMock();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(fContext.Tree, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(fContext.Tree, valuesCollection, null); });
            //TreeTools.CheckGEDCOMFormat(fContext.Tree, valuesCollection, progress);


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeMerge(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeMerge(fContext.Tree, null, null); });


            BaseWindowMock basewin = new BaseWindowMock(null);
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.RepairProblem(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.RepairProblem(basewin, null); });


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckRelations(null); });


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GetUnlinkedNamesakes(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GetUnlinkedNamesakes(fContext.Tree, null); });


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(null, null, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fContext.Tree, null, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(null, fContext.Tree, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fContext.Tree, fContext.Tree, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fContext.Tree, fContext.Tree, 0.0f, null, progress); });


            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeCompare(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeCompare(fContext.Tree, null, null); });


            StringList placesList = new StringList();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch(fContext.Tree, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch(fContext.Tree, placesList, null); });
        }

        [Test]
        public void NavStack_Tests()
        {
            using (NavigationStack navStack = new NavigationStack())
            {
                Assert.IsNotNull(navStack);
                Assert.AreEqual(false, navStack.Busy);
                Assert.AreEqual(null, navStack.Current);
                navStack.Clear();
                Assert.AreEqual(null, navStack.Current);

                navStack.BeginNav();
                Assert.AreEqual(true, navStack.Busy);
                navStack.EndNav();
                Assert.AreEqual(false, navStack.Busy);

                Assert.AreEqual(false, navStack.CanBackward());
                Assert.AreEqual(false, navStack.CanForward());

                object test = new object();
                object test2 = new object();

                navStack.Current = test;
                navStack.Current = test2;
                
                Assert.AreEqual(test, navStack.Back());
                Assert.AreEqual(test2, navStack.Next());
            }
        }

        [Test]
        public void NamesTable_Tests()
        {
            using (NamesTable namesTable = new NamesTable())
            {
                Assert.IsNotNull(namesTable);
                
                NameEntry nameEntry = namesTable.AddName("Ivan");
                Assert.IsNotNull(nameEntry);
                Assert.AreEqual("Ivan", nameEntry.Name);

                nameEntry = namesTable.FindName("Ivan");
                Assert.IsNotNull(nameEntry);

                string pat = namesTable.GetPatronymicByName("Ivan", GEDCOMSex.svMale);
                Assert.IsNull(pat);

                string name = namesTable.GetNameByPatronymic("Ivanovich");
                Assert.AreEqual("", name);

                GEDCOMSex sex = namesTable.GetSexByName("Ivan");
                Assert.AreEqual(GEDCOMSex.svNone, sex);
                
                namesTable.SetName("Ivan", "Ivanovich", GEDCOMSex.svMale);
                namesTable.SetName("Ivan", "Ivanovna", GEDCOMSex.svFemale);

                pat = namesTable.GetPatronymicByName("Ivan", GEDCOMSex.svMale);
                Assert.AreEqual("Ivanovich", pat);

                pat = namesTable.GetPatronymicByName("Ivan", GEDCOMSex.svFemale);
                Assert.AreEqual("Ivanovna", pat);

                name = namesTable.GetNameByPatronymic("Ivanovich");
                Assert.AreEqual("Ivan", name);

                name = namesTable.GetNameByPatronymic("Ivanovna");
                Assert.AreEqual("Ivan", name);

                namesTable.SetNameSex("Maria", GEDCOMSex.svFemale);
                sex = namesTable.GetSexByName("Maria");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);

                namesTable.SetName("", "", GEDCOMSex.svNone);
                namesTable.SetNameSex("", GEDCOMSex.svNone);

                namesTable.SetName("Anna", "Ivanovna", GEDCOMSex.svFemale);
                sex = namesTable.GetSexByName("Anna");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);

                GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
                Assert.IsNotNull(iRec);
                namesTable.ImportNames(iRec);

                namesTable.ImportNames(null);

                sex = namesTable.GetSexByName("Anna");
                Assert.AreEqual(GEDCOMSex.svFemale, sex);
            }
        }

        [Test]
        public void UI_Charts_Tests()
        {
            using (ChartFilter cf = new ChartFilter()) {
                cf.Backup();
                cf.Restore();
            }

            PersonModifyEventArgs args = new PersonModifyEventArgs(null);
            Assert.IsNotNull(args);

            PersonList personList = new PersonList(true);
            Assert.IsNotNull(personList);

            using (TreeChartPerson tcPerson = new TreeChartPerson(null)) {
                Assert.IsNotNull(tcPerson);

                bool hasFail = false;
                tcPerson.BuildBy(null, ref hasFail);

                Assert.AreEqual(null, tcPerson.Rec);

                Assert.AreEqual(null, tcPerson.Portrait);
                Assert.AreEqual(0, tcPerson.PortraitWidth);

                tcPerson.Divorced = false;
                Assert.AreEqual(false, tcPerson.Divorced);
                tcPerson.Divorced = true;
                Assert.AreEqual(true, tcPerson.Divorced);

                tcPerson.IsDup = false;
                Assert.AreEqual(false, tcPerson.IsDup);
                tcPerson.IsDup = true;
                Assert.AreEqual(true, tcPerson.IsDup);

                Assert.AreEqual(0, tcPerson.Height);
                Assert.AreEqual(0, tcPerson.Width);

                tcPerson.IsDead = false;
                Assert.AreEqual(false, tcPerson.IsDead);
                tcPerson.IsDead = true;
                Assert.AreEqual(true, tcPerson.IsDead);

                Assert.AreEqual(0, tcPerson.PtX);
                tcPerson.PtX = 11;
                Assert.AreEqual(11, tcPerson.PtX);

                Assert.AreEqual(0, tcPerson.PtY);
                tcPerson.PtY = 22;
                Assert.AreEqual(22, tcPerson.PtY);

                tcPerson.Selected = false;
                Assert.AreEqual(false, tcPerson.Selected);
                tcPerson.Selected = true;
                Assert.AreEqual(true, tcPerson.Selected);

                Assert.AreEqual(GEDCOMSex.svNone, tcPerson.Sex);
                tcPerson.Sex = GEDCOMSex.svMale;
                Assert.AreEqual(GEDCOMSex.svMale, tcPerson.Sex);

                EnumSet<SpecialUserRef> enums = tcPerson.Signs;
                Assert.IsTrue(enums.IsEmpty());

                Assert.AreEqual(0, tcPerson.GetChildsCount());
                Assert.AreEqual(0, tcPerson.GetSpousesCount());

                TreeChartPerson child = new TreeChartPerson(null);
                tcPerson.AddChild(null);
                tcPerson.AddChild(child);
                Assert.AreEqual(1, tcPerson.GetChildsCount());
                Assert.AreEqual(child, tcPerson.GetChild(0));

                TreeChartPerson spouse = new TreeChartPerson(null);
                tcPerson.AddSpouse(null);
                tcPerson.AddSpouse(spouse);
                Assert.AreEqual(1, tcPerson.GetSpousesCount());
                Assert.AreEqual(spouse, tcPerson.GetSpouse(0));

                Assert.IsFalse(tcPerson.HasFlag(PersonFlag.pfDescWalk));
                tcPerson.SetFlag(PersonFlag.pfDescWalk);
                Assert.IsTrue(tcPerson.HasFlag(PersonFlag.pfDescWalk));

                bool hasMediaFail = false;
                tcPerson.BuildBy(null, ref hasMediaFail);

                ExtRect psnRt = tcPerson.Rect;
                Assert.IsTrue(psnRt.IsEmpty());

                //Assert.AreEqual(null, tcPerson.Portrait);
                //Assert.AreEqual(null, tcPerson.Portrait);
                //Assert.AreEqual(null, tcPerson.Portrait);
                //Assert.AreEqual(null, tcPerson.Portrait);
                //Assert.AreEqual(null, tcPerson.Portrait);
            }
        }

        [Test]
        public void UI_Controls_Tests()
        {
            GKComboItem comboItem = new GKComboItem("Test", null);
            Assert.IsNotNull(comboItem);
            Assert.AreEqual("Test", comboItem.ToString());

            GKListItem listItem = new GKListItem("Test", null);
            Assert.IsNotNull(listItem);
            Assert.AreEqual("Test", listItem.ToString());

            GKListSubItem listSubItem = new GKListSubItem("Test");
            Assert.IsNotNull(listSubItem);

            GKToolStripMenuItem tsMenuItem = new GKToolStripMenuItem("Test", null);
            Assert.IsNotNull(tsMenuItem);

            GKTreeNode treeNode = new GKTreeNode("Test", null);
            Assert.IsNotNull(treeNode);

            ModifyEventArgs args = new ModifyEventArgs(RecordAction.raAdd, null);
            Assert.IsNotNull(args);
        }

        [Test]
        public void UI_Utils_Tests()
        {
            StringList summary = new StringList();

            summary.Clear();
            GKUtils.ShowFamilyInfo(null, null, ShieldState.None);
            GEDCOMFamilyRecord famRec = this.fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            GKUtils.ShowFamilyInfo(famRec, summary, ShieldState.None);

            summary.Clear();
            GKUtils.ShowGroupInfo(null, null);
            GEDCOMGroupRecord grpRec = this.fContext.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            GKUtils.ShowGroupInfo(grpRec, summary);

            summary.Clear();
            GKUtils.ShowMultimediaInfo(null, null);
            GEDCOMMultimediaRecord mmRec = this.fContext.Tree.XRefIndex_Find("O1") as GEDCOMMultimediaRecord;
            GKUtils.ShowMultimediaInfo(mmRec, summary);

            summary.Clear();
            GKUtils.ShowNoteInfo(null, null);
            GEDCOMNoteRecord noteRec = this.fContext.Tree.XRefIndex_Find("N1") as GEDCOMNoteRecord;
            GKUtils.ShowNoteInfo(noteRec, summary);

            summary.Clear();
            GKUtils.ShowPersonInfo(null, null, ShieldState.None);
            GEDCOMIndividualRecord indRec = this.fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GKUtils.ShowPersonInfo(indRec, summary, ShieldState.None);

            summary.Clear();
            GKUtils.ShowSourceInfo(null, null);
            GEDCOMSourceRecord srcRec = this.fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            GKUtils.ShowSourceInfo(srcRec, summary);

            summary.Clear();
            GKUtils.ShowRepositoryInfo(null, null);
            GEDCOMRepositoryRecord repRec = this.fContext.Tree.XRefIndex_Find("R1") as GEDCOMRepositoryRecord;
            GKUtils.ShowRepositoryInfo(repRec, summary);

            summary.Clear();
            GKUtils.ShowResearchInfo(null, null);
            GEDCOMResearchRecord resRec = this.fContext.Tree.XRefIndex_Find("RS1") as GEDCOMResearchRecord;
            GKUtils.ShowResearchInfo(resRec, summary);

            summary.Clear();
            GKUtils.ShowTaskInfo(null, null);
            GEDCOMTaskRecord taskRec = this.fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            GKUtils.ShowTaskInfo(taskRec, summary);

            summary.Clear();
            GKUtils.ShowCommunicationInfo(null, null);
            GEDCOMCommunicationRecord commRec = this.fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord;
            GKUtils.ShowCommunicationInfo(commRec, summary);

            summary.Clear();
            GKUtils.ShowLocationInfo(null, null);
            GEDCOMLocationRecord locRec = this.fContext.Tree.XRefIndex_Find("L1") as GEDCOMLocationRecord;
            GKUtils.ShowLocationInfo(locRec, summary);
        }

        [Test]
        public void Export_Tests()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { new PedigreeExporter(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { new ExcelExporter(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { new FamilyBookExporter(null); });

            BaseWindowMock baseWin = new BaseWindowMock(this.fContext.Tree);

            using (PedigreeExporter exporter = new PedigreeExporter(baseWin)) {

            }

            using (ExcelExporter exporter = new ExcelExporter(baseWin)) {

            }

            using (FamilyBookExporter exporter = new FamilyBookExporter(baseWin)) {

            }
        }

        [Test]
        public void LuaScripts_Tests()
        {
            using (ScriptEngine script = new ScriptEngine()) {
                script.lua_run("gk_print(\"Hello\")", null, null);
            }
        }
    }
}
