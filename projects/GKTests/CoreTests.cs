/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon.IoC;
using GKCore;
using GKCore.Cultures;
using GKCore.Export;
using GKCore.Geocoding;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Stats;
using GKCore.Tools;
using GKCore.Types;
using GKTests.Mocks;
using GKUI;
using GKUI.Components;
using NUnit.Framework;

namespace GKTests.GKCore
{
    [TestFixture]
    public class CoreTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WinFormsAppHost.ConfigureBootstrap(false);

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
            Assert.IsNotNull(fContext.Culture);

            Assert.IsNull(fContext.Viewer);

            fContext.SetFileName("testfile.ged");
            Assert.AreEqual("testfile.ged", fContext.FileName);


            Assert.AreEqual(ShieldState.Maximum, fContext.ShieldState, "BaseContext.ShieldState.1");
            fContext.SwitchShieldState();
            Assert.AreEqual(ShieldState.Middle, fContext.ShieldState, "BaseContext.ShieldState.2");
            fContext.SwitchShieldState();
            Assert.AreEqual(ShieldState.None, fContext.ShieldState, "BaseContext.ShieldState.3");
            fContext.SwitchShieldState();
            Assert.AreEqual(ShieldState.Maximum, fContext.ShieldState, "BaseContext.ShieldState.4");


            GEDCOMSourceRecord srcRec = fContext.FindSource("test source");
            Assert.IsNull(srcRec);

            StringList sources = new StringList();
            fContext.GetSourcesList(sources);
            Assert.AreEqual(1, sources.Count);

            Assert.IsNotNull(fContext.ValuesCollection);

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.AreEqual(false, fContext.IsChildless(iRec));

            // FIXME: move to other tests
            Assert.AreEqual(1990, iRec.GetChronologicalYear("BIRT"));

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

            string fn = "";
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.GetStoreType(null, ref fn); });
            Assert.AreEqual(MediaStoreType.mstReference, fContext.GetStoreType(new GEDCOMFileReference(fContext.Tree, null, "", "file.txt"), ref fn));
            Assert.AreEqual(MediaStoreType.mstStorage, fContext.GetStoreType(new GEDCOMFileReference(fContext.Tree, null, "", "stg:file.txt"), ref fn));
            Assert.AreEqual(MediaStoreType.mstArchive, fContext.GetStoreType(new GEDCOMFileReference(fContext.Tree, null, "", "arc:file.txt"), ref fn));

            fContext.CollectEventValues(null);

            fContext.BeginUpdate();
            Assert.IsTrue(fContext.IsUpdated());
            fContext.EndUpdate();
            Assert.IsFalse(fContext.IsUpdated());

            //Graph patrGraph = fContext.GetPatriarchsGraph(1, true, false);
            //Assert.IsNotNull(patrGraph);

            fContext.DoUndo();
            fContext.DoRedo();
            fContext.DoCommit();
            fContext.DoRollback();
        }

        [Test]
        public void Context2_Tests()
        {
            BaseContext context = TestStubs.CreateContext();
            TestStubs.FillContext(context);
            Assert.AreEqual(15, context.Tree.RecordsCount);

            context.Clear();
            Assert.AreEqual(0, context.Tree.RecordsCount);
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
            using (UndoManager undoman = new UndoManager()) {
                Assert.IsNotNull(undoman);

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

            Assert.AreEqual(fContext.Tree, fContext.Undoman.Tree);

            fContext.Undoman.OnTransaction += TransactionEventHandler;

            fContext.Undoman.Clear();

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.Throws(typeof(ArgumentNullException), () => {
                              fContext.Undoman.DoOrdinaryOperation(
                                  OperationType.otIndividualBookmarkChange, null, true); });

            Assert.Throws(typeof(ArgumentNullException), () => {
                              fContext.Undoman.DoOrdinaryOperation(
                                  OperationType.otIndividualBookmarkChange, iRec, null); });

            iRec.Bookmark = false;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            Assert.IsTrue(iRec.Bookmark);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            iRec.Patriarch = false;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            Assert.IsTrue(iRec.Patriarch);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Patriarch);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            iRec.Sex = GEDCOMSex.svUndetermined;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GEDCOMSex.svMale);
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

            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GEDCOMSex.svMale);
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


            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GEDCOMSex.svMale);
            fContext.Undoman.Rollback();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GEDCOMSex.svUndetermined, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            fContext.Undoman.OnTransaction -= TransactionEventHandler;


            Assert.Throws(typeof(ArgumentNullException), () => {
                              fContext.Undoman.DoIndividualNameChange(null, "", "", ""); });
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetNameString(iRec, true, false));
            fContext.Undoman.DoIndividualNameChange(iRec, "Petrov", "Alex", "Ivanovich");
            Assert.AreEqual("Petrov Alex Ivanovich", GKUtils.GetNameString(iRec, true, false));
            fContext.Undoman.Rollback();
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetNameString(iRec, true, false));
        }

        [Test]
        public void Utils_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            GKUtils.PrepareHeader(fContext.Tree, "c:\\test.ged", GEDCOMCharacterSet.csUTF8, true);
            Assert.AreEqual(0, fContext.Tree.Header.FileRevision);
            GKUtils.PrepareHeader(fContext.Tree, "c:\\test.ged", GEDCOMCharacterSet.csUTF8, false);
            Assert.AreEqual(1, fContext.Tree.Header.FileRevision);

            // individual record tests
            Assert.IsNotNull(iRec);

            GEDCOMCustomDate date = GKUtils.GetBirthDate(iRec);
            Assert.IsNotNull(date);
            Assert.AreEqual("28 DEC 1990", date.StringValue);

            string age = GKUtils.GetAgeStr(null, 0);
            Assert.AreEqual("", age);

            age = GKUtils.GetAgeStr(iRec, -1);
            Assert.AreEqual("20", age);
            
            age = GKUtils.GetAgeStr(iRec, 2005);
            Assert.AreEqual("15", age);

            //

            GKUtils.CollectEventValues(null, null);

            //

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetPedigreeLifeStr(null, PedigreeFormat.Compact); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventName(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventCause(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventDesc(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetAttributeStr(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.InitExtData(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.InitExtCounts(null, 0); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null, "", ""); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNickString(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNameString(null, false, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SetMarriedSurname(null, ""); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(null, fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(fContext.Tree, null, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetStoreType(null); });

            Assert.AreEqual(null, GKUtils.GetDaysForBirth(null));
            Assert.AreEqual("", GKUtils.GetTaskGoalStr(null));
            Assert.AreEqual("", GKUtils.GetGoalStr(GKGoalType.gtIndividual, null));

            //

            GEDCOMRecord rec = fContext.Tree.XRefIndex_Find("I1");
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(rec, false));

            //

            string surname = "", name = "", patronymic = "";
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetRusNameParts(null, out surname, out name, out patronymic); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SetRusNameParts(null, surname, name, patronymic); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNameParts(null, out surname, out name, out patronymic); });

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

            evt.Cause = "test cause";
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

            GEDCOMListTests(iRec);

            // access tests
            fContext.ShieldState = ShieldState.None;
            Assert.IsTrue(fContext.IsRecordAccess(GEDCOMRestriction.rnNone));
            Assert.IsTrue(fContext.IsRecordAccess(GEDCOMRestriction.rnConfidential));
            Assert.IsTrue(fContext.IsRecordAccess(GEDCOMRestriction.rnPrivacy));

            fContext.ShieldState = ShieldState.Middle;
            Assert.IsTrue(fContext.IsRecordAccess(GEDCOMRestriction.rnNone));
            Assert.IsTrue(fContext.IsRecordAccess(GEDCOMRestriction.rnConfidential));
            Assert.IsFalse(fContext.IsRecordAccess(GEDCOMRestriction.rnPrivacy));

            fContext.ShieldState = ShieldState.Maximum;
            Assert.IsTrue(fContext.IsRecordAccess(GEDCOMRestriction.rnNone));
            Assert.IsFalse(fContext.IsRecordAccess(GEDCOMRestriction.rnConfidential));
            Assert.IsFalse(fContext.IsRecordAccess(GEDCOMRestriction.rnPrivacy));

            st1 = GKUtils.HyperLink("@X001@", "test", 0);
            Assert.AreEqual("[url=" + "@X001@" + "]" + "test" + "[/url]", st1);

            st1 = GKUtils.HyperLink("@X001@", "", 0);
            Assert.AreEqual("[url=" + "@X001@" + "]" + "???" + "[/url]", st1);

            //

            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfNone));
            Assert.AreEqual(MultimediaKind.mkImage, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfBMP));
            Assert.AreEqual(MultimediaKind.mkText, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfTXT));
            Assert.AreEqual(MultimediaKind.mkAudio, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfWAV));
            Assert.AreEqual(MultimediaKind.mkVideo, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfAVI));
            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfOLE));

            //

            #if !__MonoCS__
            Assert.AreEqual("test.zip", GKUtils.GetContainerName("c:\\temp\\test.ged", true)); // archive
            Assert.AreEqual("test\\", GKUtils.GetContainerName("c:\\temp\\test.ged", false)); // storage
            #endif

            //

            GEDCOMCustomDate dtx = GKUtils.GetMarriageDate(null);
            Assert.IsNull(dtx);

            //

            string test = GKUtils.TruncateStrings(new StringList("sample text for truncate"), 10);
            Assert.AreEqual("sample tex...", test);
        }

        [Test]
        public void Search_Tests()
        {
            SearchResult searchResult = new SearchResult(null);
            Assert.IsNotNull(searchResult);

            Assert.Throws(typeof(ArgumentNullException), () => { new SearchStrategy(null, null); });

            SearchStrategy strat = new SearchStrategy(new WorkWindowMock(), "");
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
            for (int k = 0; k < REP_COUNT; k++) {
                GEDCOMListTest11(iRec);
                GEDCOMListTest12(iRec);
                GEDCOMListTest13(iRec);
                GEDCOMListTest21(iRec);
                GEDCOMListTest22(iRec);
                GEDCOMListTest23(iRec);
            }
        }

        private static void GEDCOMListTest11(GEDCOMIndividualRecord iRec)
        {
            foreach (GEDCOMCustomEvent evt1 in iRec.Events) {
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest12(GEDCOMIndividualRecord iRec)
        {
            IGEDCOMListEnumerator<GEDCOMCustomEvent> enumer = iRec.Events.GetEnumerator();
            while (enumer.MoveNext()) {
                GEDCOMCustomEvent evt1 = enumer.Current;
                evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest13(GEDCOMIndividualRecord iRec)
        {
            foreach (GEDCOMCustomEvent evt1 in iRec.Events) {
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

        [Test(Description = "Cultures test")]
        public void Cultures_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;

            ICulture culture = new RussianCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            //
            string[] surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });


            culture = new AncientCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));


            culture = new IcelandCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));


            culture = new BritishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));


            culture = new SwedishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GEDCOMSex.svUndetermined, culture.GetSex("Alef", "", false));
            //
            surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));

            //

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.German;
            Assert.IsInstanceOf(typeof(GermanCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Polish;
            Assert.IsInstanceOf(typeof(PolishCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Swedish;
            Assert.IsInstanceOf(typeof(SwedishCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Icelandic;
            Assert.IsInstanceOf(typeof(IcelandCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Russian;
            Assert.IsInstanceOf(typeof(RussianCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Ukrainian;
            Assert.IsInstanceOf(typeof(RussianCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Armenian;
            Assert.IsInstanceOf(typeof(ArmenianCulture), fContext.Culture);
            Assert.IsTrue(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Turkish;
            Assert.IsInstanceOf(typeof(TurkishCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.French;
            Assert.IsInstanceOf(typeof(FrenchCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Italian;
            Assert.IsInstanceOf(typeof(ItalianCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Cantonese;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);

            fContext.Tree.Header.Language.Value = GEDCOMLanguageID.Mandrin;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());
        }

        [Test]
        public void RussianCulture_Tests()
        {
            RussianCulture rusCulture = new RussianCulture();

            //Assert.AreEqual("", GKUtils.GetMaidenSurname(null));
            //Assert.AreEqual("", GKUtils.GetMaidenSurname(""));
            //Assert.AreEqual("Иванова", GKUtils.GetMaidenSurname("Иванова (Петрова)"));

            Assert.AreEqual("Иванов", rusCulture.NormalizeSurname("Иванова", true));
            Assert.AreEqual("Бельский", rusCulture.NormalizeSurname("Бельская", true));
            Assert.AreEqual("Грозный", rusCulture.NormalizeSurname("Грозная", true));
            Assert.AreEqual("Иванов", rusCulture.NormalizeSurname("Иванов", false));
            Assert.AreEqual("Бельский", rusCulture.NormalizeSurname("Бельский", false));
            Assert.AreEqual("Грозный", rusCulture.NormalizeSurname("Грозный", false));

            Assert.AreEqual("?", rusCulture.NormalizeSurname(null, false));
            Assert.AreEqual("?", rusCulture.NormalizeSurname("", false));
            Assert.AreEqual("?", rusCulture.NormalizeSurname("(Иванова)", false));

            Assert.AreEqual("Иванова", rusCulture.GetMarriedSurname("Иванов"));
            Assert.AreEqual("Бельская", rusCulture.GetMarriedSurname("Бельский"));
            Assert.AreEqual("Грозная", rusCulture.GetMarriedSurname("Грозный"));

            Assert.AreEqual("?", rusCulture.GetMarriedSurname(""));
            Assert.AreEqual("?", rusCulture.GetMarriedSurname(null));

            string[] snms = rusCulture.GetSurnames("Бельская (Иванова)", true);
            Assert.AreEqual(2, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);
            Assert.AreEqual("Иванов", snms[1]);

            snms = rusCulture.GetSurnames("Бельская", true);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            snms = rusCulture.GetSurnames("Бельский", false);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            //

            GEDCOMSex sx = rusCulture.GetSex("Мария", "Петровна", false);
            Assert.AreEqual(GEDCOMSex.svFemale, sx);

            sx = rusCulture.GetSex("Иван", "Петрович", false);
            Assert.AreEqual(GEDCOMSex.svMale, sx);

            Assert.AreEqual(GEDCOMSex.svNone, rusCulture.GetSex("", "", false));

            Assert.AreEqual("Иванова Ивана Ивановича", rusCulture.GetPossessiveName("Иванов Иван Иванович"));
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

            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord chldRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord otherRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.SearchKinshipsGraph(fContext, null); });

            using (KinshipsGraph kinsGraph = TreeTools.SearchKinshipsGraph(fContext, indRec)) {
                Assert.IsNull(kinsGraph.AddIndividual(null));

                Assert.IsNotNull(kinsGraph.FindVertex(chldRec.XRef));

                // check invalid args
                kinsGraph.SetTreeRoot(null);
                kinsGraph.SetTreeRoot(otherRec);

                // valid individual
                kinsGraph.SetTreeRoot(indRec);

                Assert.AreEqual("???", kinsGraph.GetRelationship(null));
                Assert.AreEqual("???", kinsGraph.GetRelationship(otherRec));

                string result = kinsGraph.GetRelationship(chldRec);
                Assert.AreEqual("daughter", result);

                Assert.IsFalse(kinsGraph.IsEmpty());
                kinsGraph.Clear();
                Assert.IsTrue(kinsGraph.IsEmpty());
            }
        }

        [Test]
        public void DateItems_Tests()
        {
            var dtx1 = new GEDCOMDateValue(null, null, "DATE", "05 JAN 2013");
            var dtItem1 = new GEDCOMDateItem(dtx1);

            var dtx2 = new GEDCOMDateValue(null, null, "DATE", "17 FEB 2013");
            var dtItem2 = new GEDCOMDateItem(dtx2);

            Assert.AreEqual(0, dtItem1.CompareTo(dtItem1));
            Assert.AreEqual(-1, dtItem1.CompareTo(dtItem2));
            Assert.AreEqual(-1, dtItem1.CompareTo(null));
            Assert.AreEqual(+1, dtItem2.CompareTo(dtItem1));

            dtItem1 = new GEDCOMDateItem(dtx1);
            dtItem2 = new GEDCOMDateItem(null);
            Assert.AreEqual(-1, dtItem1.CompareTo(dtItem2));

            dtItem1 = new GEDCOMDateItem(null);
            dtItem2 = new GEDCOMDateItem(dtx2);
            Assert.AreEqual(+1, dtItem1.CompareTo(dtItem2));

            dtItem1 = new GEDCOMDateItem(null);
            dtItem2 = new GEDCOMDateItem(null);
            Assert.AreEqual(0, dtItem1.CompareTo(dtItem2));
        }

        private class ListViewMock : IListView
        {
            public void AddColumn(string caption, int width, bool autoSize) {}
        }

        private bool ExtFilterHandler(GEDCOMRecord record)
        {
            return true;
        }

        [Test]
        public void Lists_Tests()
        {
            ListColumn colStatic = new ListColumn(0, 0, DataType.dtString, 0, true);
            Assert.IsNotNull(colStatic);
            Assert.AreEqual(0, colStatic.Order);
            Assert.AreEqual(false, colStatic.CurActive);
            Assert.AreEqual(0, colStatic.CurWidth);

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
            listManager = new GroupListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMGroupRecord grpRec = fContext.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            listManager.Fetch(grpRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*roup*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            listManager.UpdateColumns(lvMock);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, grpRec);

            //
            IListFilter filter = listManager.Filter;
            IListColumns listColumns = listManager.ListColumns;

            ListColumns copyColumns = GroupListMan.CreateGroupListColumns();
            listColumns.CopyTo(copyColumns);

            Assert.Throws(typeof(ArgumentNullException), () => { listColumns.CopyTo(null); });

            listManager.QuickFilter = "*";
            listManager.AddCondition((byte)GroupColumnType.ctName, ConditionKind.ck_Contains, "*roup*");
            Assert.IsTrue(listManager.CheckFilter());
        }

        [Test]
        public void LM_Tests()
        {
            
        }

        [Test]
        public void LMCommunication_Tests()
        {
            var listManager = new CommunicationListMan(fContext);
            Assert.IsNotNull(listManager);

            Assert.IsNotNull(listManager.ContentList);

            listManager.ExternalFilter = null;
            Assert.IsNull(listManager.ExternalFilter);

            GEDCOMCommunicationRecord commRec = fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord;
            listManager.Fetch(commRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*commun*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, commRec);
        }

        [Test]
        public void LMFamily_Tests()
        {
            var listManager = new FamilyListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            listManager.Fetch(famRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "* - *";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, famRec);
        }

        [Test]
        public void LMIndividual_Tests()
        {
            var listManager = new IndividualListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;
            listManager.Fetch(indRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*Petr*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*alpha*";
            Assert.IsFalse(listManager.CheckFilter());

            GlobalOptions.Instance.ListHighlightUnparentedPersons = true;
            GlobalOptions.Instance.ListHighlightUnmarriedPersons = true;
            listManager.PrepareFilter();
            listManager.ExternalFilter = ExtFilterHandler;

            var lvMock = new ListViewMock();

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfFNP;
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, indRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_NP;
            listManager.UpdateColumns(lvMock);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, indRec);

            GlobalOptions.Instance.DefNameFormat = NameFormat.nfF_N_P;
            listManager.UpdateColumns(lvMock);
            listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, indRec);
        }

        [Test]
        public void LMLocation_Tests()
        {
            var listManager = new LocationListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMLocationRecord locRec = fContext.Tree.XRefIndex_Find("L1") as GEDCOMLocationRecord;
            listManager.Fetch(locRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*locat*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, locRec);
        }

        [Test]
        public void LMMultimedia_Tests()
        {
            var listManager = new MultimediaListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMMultimediaRecord mediaRec = fContext.Tree.XRefIndex_Find("O1") as GEDCOMMultimediaRecord;
            listManager.Fetch(mediaRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*media*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, mediaRec);
        }

        [Test]
        public void LMNote_Tests()
        {
            var listManager = new NoteListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMNoteRecord noteRec = new GEDCOMNoteRecord(null, null, "", "");
            noteRec.AddNoteText("Test text");
            listManager.Fetch(noteRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*text*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, noteRec);
            noteRec.Clear();
            listManager.UpdateItem(listItem, noteRec);
        }

        [Test]
        public void LMRepository_Tests()
        {
            var listManager = new RepositoryListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMRepositoryRecord repoRec = fContext.Tree.XRefIndex_Find("R1") as GEDCOMRepositoryRecord;
            listManager.Fetch(repoRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*repos*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, repoRec);
        }

        [Test]
        public void LMResearch_Tests()
        {
            var listManager = new ResearchListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMResearchRecord resRec = fContext.Tree.XRefIndex_Find("RS1") as GEDCOMResearchRecord;
            listManager.Fetch(resRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*resear*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, resRec);
        }

        [Test]
        public void LMSource_Tests()
        {
            var listManager = new SourceListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            listManager.Fetch(srcRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*sourc*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, srcRec);
        }

        [Test]
        public void LMTask_Tests()
        {
            var listManager = new TaskListMan(fContext);
            Assert.IsNotNull(listManager);

            GEDCOMTaskRecord tskRec = fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            listManager.Fetch(tskRec);

            listManager.QuickFilter = "*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*task*";
            Assert.IsTrue(listManager.CheckFilter());
            listManager.QuickFilter = "*xxxx*";
            Assert.IsFalse(listManager.CheckFilter());

            var lvMock = new ListViewMock();
            listManager.UpdateColumns(lvMock);
            var listItem = new GKListItem("", null);
            listManager.UpdateItem(listItem, tskRec);
        }

        [Test]
        public void Maps_Tests()
        {
            GeoPoint mapPoint = new GeoPoint(0.5f, 0.5f, "test");
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
            using (IniFile iniFile = new IniFile()) {
                GlobalOptions globalOptions = GlobalOptions.Instance;
                Assert.IsNotNull(globalOptions);

                Assert.IsNotNull(globalOptions.ChartOptions);
                Assert.IsNotNull(globalOptions.AncestorsCircleOptions);

                /*globalOptions.DefCharacterSet = GEDCOMCharacterSet.csUNICODE;
                Assert.AreEqual(GEDCOMCharacterSet.csUNICODE, globalOptions.DefCharacterSet);*/
                Assert.AreEqual(GEDCOMCharacterSet.csUTF8, globalOptions.DefCharacterSet);

                globalOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
                Assert.AreEqual(DateFormat.dfDD_MM_YYYY, globalOptions.DefDateFormat);

                globalOptions.ShowDatesSign = true;
                Assert.AreEqual(true, globalOptions.ShowDatesSign);

                globalOptions.DefNameFormat = NameFormat.nfF_N_P;
                Assert.AreEqual(NameFormat.nfF_N_P, globalOptions.DefNameFormat);

                Assert.IsNotNull(globalOptions.EventFilters);

                globalOptions.InterfaceLang = 1000;
                Assert.AreEqual(1000, globalOptions.InterfaceLang);

                globalOptions.LastDir = "c:\\";
                Assert.AreEqual("c:\\", globalOptions.LastDir);

                Assert.IsNotNull(globalOptions.MRUFiles);

                globalOptions.MWinRect = ExtRect.CreateEmpty();
                Assert.IsTrue(globalOptions.MWinRect.IsEmpty());

                globalOptions.MWinState = WindowState.Maximized;
                Assert.AreEqual(WindowState.Maximized, globalOptions.MWinState);

                Assert.IsNotNull(globalOptions.NameFilters);

                Assert.IsNotNull(globalOptions.PedigreeOptions);

                globalOptions.PlacesWithAddress = true;
                Assert.AreEqual(true, globalOptions.PlacesWithAddress);

                Assert.IsNotNull(globalOptions.Proxy);

                Assert.IsNotNull(globalOptions.Relations);

                Assert.IsNotNull(globalOptions.ResidenceFilters);

                globalOptions.FileBackup = FileBackup.fbOnlyPrev;
                Assert.AreEqual(FileBackup.fbOnlyPrev, globalOptions.FileBackup);

                globalOptions.ShowTips = true;
                Assert.AreEqual(true, globalOptions.ShowTips);

                globalOptions.ListHighlightUnmarriedPersons = true;
                Assert.AreEqual(true, globalOptions.ListHighlightUnmarriedPersons);

                globalOptions.ListHighlightUnparentedPersons = true;
                Assert.AreEqual(true, globalOptions.ListHighlightUnparentedPersons);

                Assert.IsNotNull(globalOptions.IndividualListColumns);

                globalOptions.ShowDatesCalendar = true;
                Assert.AreEqual(true, globalOptions.ShowDatesCalendar);

                globalOptions.Autosave = true;
                Assert.AreEqual(true, globalOptions.Autosave);

                globalOptions.AutosaveInterval = 10;
                Assert.AreEqual(10, globalOptions.AutosaveInterval);

                globalOptions.ExtendedNames = true;
                Assert.AreEqual(true, globalOptions.ExtendedNames);

                globalOptions.UseExtendedNotes = true;
                Assert.AreEqual(true, globalOptions.UseExtendedNotes);

                globalOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
                Assert.AreEqual(WomanSurnameFormat.wsfMaiden, globalOptions.WomanSurnameFormat);

                globalOptions.AddLastBase("sample.ged");
                Assert.AreEqual(1, globalOptions.GetLastBasesCount());
                Assert.AreEqual("sample.ged", globalOptions.GetLastBase(0));
                globalOptions.ClearLastBases();

                Assert.IsNotNull(globalOptions.Languages);


                globalOptions.SaveToFile(iniFile);
                globalOptions.LoadFromFile(iniFile);
                //Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.SaveToFile(null); });
                //Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.LoadFromFile(null); });


                MRUFile mruFile = new MRUFile();
                Assert.IsNotNull(mruFile);

                mruFile = new MRUFile("test.ged");
                Assert.IsNotNull(mruFile);
                Assert.AreEqual(-1, globalOptions.MRUFiles_IndexOf("test.ged"));
                globalOptions.MRUFiles.Add(mruFile);
                Assert.AreEqual(0, globalOptions.MRUFiles_IndexOf("test.ged"));


                mruFile.SaveToFile(iniFile, "xxx");
                mruFile.LoadFromFile(iniFile, "xxx");
                MRUFile.DeleteKeys(iniFile, "xxx");
                Assert.Throws(typeof(ArgumentNullException), () => { mruFile.SaveToFile(null, "xxx"); });
                Assert.Throws(typeof(ArgumentNullException), () => { mruFile.LoadFromFile(null, "xxx"); });
                Assert.Throws(typeof(ArgumentNullException), () => { MRUFile.DeleteKeys(null, "xxx"); });

                AncestorsCircleOptions circleOptions = new AncestorsCircleOptions();
                Assert.IsNotNull(circleOptions);
                circleOptions.Assign(null);
                circleOptions.Assign(new AncestorsCircleOptions());
                circleOptions.SaveToFile(iniFile);
                circleOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { circleOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { circleOptions.LoadFromFile(null); });

                PedigreeOptions pedigreeOptions = new PedigreeOptions();
                Assert.IsNotNull(pedigreeOptions);
                pedigreeOptions.Assign(null);
                pedigreeOptions.Assign(new PedigreeOptions());
                pedigreeOptions.SaveToFile(iniFile);
                pedigreeOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { pedigreeOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { pedigreeOptions.LoadFromFile(null); });

                ProxyOptions proxyOptions = new ProxyOptions();
                Assert.IsNotNull(proxyOptions);
                proxyOptions.Assign(null);
                proxyOptions.Assign(new ProxyOptions());
                proxyOptions.SaveToFile(iniFile);
                proxyOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { proxyOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { proxyOptions.LoadFromFile(null); });

                TreeChartOptions treeChartOptions = new TreeChartOptions();
                Assert.IsNotNull(treeChartOptions);
                treeChartOptions.Assign(null);
                treeChartOptions.Assign(new TreeChartOptions());
                treeChartOptions.SaveToFile(iniFile);
                treeChartOptions.LoadFromFile(iniFile);
                Assert.Throws(typeof(ArgumentNullException), () => { treeChartOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { treeChartOptions.LoadFromFile(null); });
            }
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

            TreeStats treeStats = new TreeStats(fContext, selectedRecords);
            Assert.IsNotNull(treeStats);

            CommonStats commonStats = treeStats.GetCommonStats();
            Assert.IsNotNull(commonStats);
            Assert.AreEqual(5, commonStats.persons, "Stats.TotalPersons");
            Assert.AreEqual(2, commonStats.persons_m, "Stats.SumM");
            Assert.AreEqual(3, commonStats.persons_f, "Stats.SumF");

            List<StatsItem> values = new List<StatsItem>();

            treeStats.GetSpecStats(StatsMode.smAncestors, values);
            treeStats.GetSpecStats(StatsMode.smDescendants, values);
            treeStats.GetSpecStats(StatsMode.smDescGenerations, values);
            treeStats.GetSpecStats(StatsMode.smSurnames, values);
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
            IBaseWindow baseWin = new BaseWindowMock();
            AppHost.Container.Register<IProgressController, ProgressMock>(LifeCycle.Singleton, true);

            ValuesCollection valuesCollection = new ValuesCollection();
            ProgressMock progress = new ProgressMock();

            //

            PlaceObj placeObj = new PlaceObj(null);
            Assert.IsNotNull(placeObj);
            Assert.AreEqual(null, placeObj.Name);
            Assert.IsNotNull(placeObj.Facts);

            placeObj.Dispose();
            Assert.IsNotNull(placeObj);

            //

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            //

            List<GEDCOMRecord> walkList = new List<GEDCOMRecord>();
            TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, walkList);
            Assert.AreEqual(3, walkList.Count, "TreeTools.TreeWalk(twmAll)"); // 3 linked from 4 total

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeWalk(null, TreeTools.TreeWalkMode.twmAll, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, null); });

            //

            List<TreeTools.CheckObj> checksList = new List<TreeTools.CheckObj>();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckBase(null, checksList); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckBase(baseWin, null); });

            TreeTools.CheckBase(baseWin, checksList);

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GenPatriarchsGraphviz(null, "", 0, false); });
            //string filename = GKUtils.GetTempDir() + "test.gvf";
            //if (File.Exists(filename)) File.Delete(filename); // for local tests!
            //TreeTools.GenPatriarchsGraphviz(baseWin, filename, 0, false);

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(fContext.Tree, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckGEDCOMFormat(fContext.Tree, valuesCollection, null); });
            TreeTools.CheckGEDCOMFormat(fContext.Tree, valuesCollection, progress);

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeMerge(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeMerge(fContext.Tree, null, null); });

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.RepairProblem(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.RepairProblem(baseWin, null); });

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.CheckRelations(null); });
            List<GEDCOMRecord> splitList = new List<GEDCOMRecord>();
            splitList.Add(iRec);
            TreeTools.CheckRelations(splitList);

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.GetUnlinkedNamesakes(null); });
            List<TreeTools.ULIndividual> uln = TreeTools.GetUnlinkedNamesakes(baseWin);

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(null, null, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fContext.Tree, null, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(null, fContext.Tree, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fContext.Tree, fContext.Tree, 0.0f, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.FindDuplicates(fContext.Tree, fContext.Tree, 0.0f, null, progress); });

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeCompare(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.TreeCompare(fContext, null, null); });

            //

            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch_Clear(null); });

            StringList placesList = new StringList();
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch(null, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch(fContext.Tree, null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { TreeTools.PlacesSearch(fContext.Tree, placesList, null); });

            TreeTools.PlacesSearch(fContext.Tree, placesList, AppHost.Progress);
            Assert.IsTrue(placesList.IndexOf("Ivanovo") >= 0); // <- TestStubs
            Assert.IsTrue(placesList.IndexOf("unknown") >= 0); // <- TestStubs
            Assert.IsTrue(placesList.IndexOf("Far Forest") >= 0); // <- TestStubs
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
            GKUtils.ShowFamilyInfo(fContext, null, null);
            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            GKUtils.ShowFamilyInfo(fContext, famRec, summary);

            summary.Clear();
            GKUtils.ShowGroupInfo(null, null);
            GEDCOMGroupRecord grpRec = fContext.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            GKUtils.ShowGroupInfo(grpRec, summary);

            summary.Clear();
            GKUtils.ShowMultimediaInfo(null, null);
            GEDCOMMultimediaRecord mmRec = fContext.Tree.XRefIndex_Find("O1") as GEDCOMMultimediaRecord;
            GKUtils.ShowMultimediaInfo(mmRec, summary);

            summary.Clear();
            GKUtils.ShowNoteInfo(null, null);
            GEDCOMNoteRecord noteRec = fContext.Tree.XRefIndex_Find("N1") as GEDCOMNoteRecord;
            GKUtils.ShowNoteInfo(noteRec, summary);

            summary.Clear();
            GKUtils.ShowPersonInfo(fContext, null, null);
            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GKUtils.ShowPersonInfo(fContext, indRec, summary);

            summary.Clear();
            GKUtils.ShowSourceInfo(null, null);
            GEDCOMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            GKUtils.ShowSourceInfo(srcRec, summary);

            summary.Clear();
            GKUtils.ShowRepositoryInfo(null, null);
            GEDCOMRepositoryRecord repRec = fContext.Tree.XRefIndex_Find("R1") as GEDCOMRepositoryRecord;
            GKUtils.ShowRepositoryInfo(repRec, summary);

            summary.Clear();
            GKUtils.ShowResearchInfo(null, null);
            GEDCOMResearchRecord resRec = fContext.Tree.XRefIndex_Find("RS1") as GEDCOMResearchRecord;
            GKUtils.ShowResearchInfo(resRec, summary);

            summary.Clear();
            GKUtils.ShowTaskInfo(null, null);
            GEDCOMTaskRecord taskRec = fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            GKUtils.ShowTaskInfo(taskRec, summary);

            summary.Clear();
            GKUtils.ShowCommunicationInfo(null, null);
            GEDCOMCommunicationRecord commRec = fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord;
            GKUtils.ShowCommunicationInfo(commRec, summary);

            summary.Clear();
            GKUtils.ShowLocationInfo(null, null);
            GEDCOMLocationRecord locRec = fContext.Tree.XRefIndex_Find("L1") as GEDCOMLocationRecord;
            GKUtils.ShowLocationInfo(locRec, summary);
        }

        private void EWriter_Test(CustomWriter writer)
        {
        }

        [Test]
        public void Export_Tests()
        {
            BaseWindowMock baseWin = new BaseWindowMock();

            using (HTMLWriter writer = new HTMLWriter()) {
                EWriter_Test(writer);
            }

            using (RTFWriter writer = new RTFWriter()) {
                EWriter_Test(writer);
            }

            /*using (PDFWriter writer = new PDFWriter()) {
                // TravisCI crash
            }*/

            Assert.Throws(typeof(ArgumentNullException), () => { new PedigreeExporter(null); });

            using (PedigreeExporter exporter = new PedigreeExporter(baseWin)) {
                exporter.Options = GlobalOptions.Instance;
                Assert.IsNotNull(exporter.Options);

                GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

                exporter.Root = iRec;
                Assert.AreEqual(iRec, exporter.Root);

                exporter.ShieldState = ShieldState.None;
                Assert.AreEqual(ShieldState.None, exporter.ShieldState);

                exporter.Options.PedigreeOptions.IncludeAttributes = true;
                exporter.Options.PedigreeOptions.IncludeNotes = true;
                exporter.Options.PedigreeOptions.IncludeSources = true;
                exporter.Options.PedigreeOptions.IncludeGenerations = true;

                exporter.Kind = PedigreeExporter.PedigreeKind.pkDescend_Konovalov;
                Assert.AreEqual(PedigreeExporter.PedigreeKind.pkDescend_Konovalov, exporter.Kind);

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new MockWriter()));

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Compact;
                Assert.IsTrue(exporter.Generate(new MockWriter()));


                exporter.Kind = PedigreeExporter.PedigreeKind.pkDescend_dAboville;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new MockWriter()));


                exporter.Kind = PedigreeExporter.PedigreeKind.pkAscend;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new MockWriter()));
            }

            Assert.Throws(typeof(ArgumentNullException), () => { new ExcelExporter(null); });

            using (ExcelExporter exporter = new ExcelExporter(baseWin)) {

            }

            Assert.Throws(typeof(ArgumentNullException), () => { new FamilyBookExporter(null); });

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

        [Test]
        public void LangMan_Tests()
        {
            LangManager langMan = new LangManager();
            Assert.IsNotNull(langMan);

            Assert.AreEqual("?", langMan.LS(LSID.LSID_First));
        }

        [Test]
        public void ExtendedWomanSurnames_Tests()
        {
            // Anna Jones
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I5") as GEDCOMIndividualRecord;

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
            Assert.AreEqual("Jones Anna", GKUtils.GetNameString(iRec, true, false));

            GKUtils.SetMarriedSurname(iRec, "Smith");

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
            Assert.AreEqual("Jones Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMarried;
            Assert.AreEqual("Smith Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden_Married;
            Assert.AreEqual("Jones (Smith) Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMarried_Maiden;
            Assert.AreEqual("Smith (Jones) Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
        }

        [Test]
        public void Geocoding_Tests()
        {
            IGeocoder geocoder = IGeocoder.Create("");
            IList<GeoPoint> geoPoints;

            geocoder.SetKey("");
            geocoder.SetProxy(null);
            geocoder.SetLang("");

            try {
                geocoder = IGeocoder.Create("Google");
                geocoder.SetKey(GKData.GAPI_KEY);
                geoPoints = geocoder.Geocode("New York", 1);
                //Assert.IsTrue(geoPoints.Count > 0);

                geocoder = IGeocoder.Create("Yandex");
                geoPoints = geocoder.Geocode("New York", 1);
                //Assert.IsTrue(geoPoints.Count > 0);
            } catch (Exception) {
                Assert.Fail();
            }
        }

        [Test]
        public void PortraitsCache_Tests()
        {
            PortraitsCache cache = PortraitsCache.Instance;
            Assert.IsNull(cache.GetImage(null, null));
            cache.RemoveObsolete(null);
        }
    }
}
