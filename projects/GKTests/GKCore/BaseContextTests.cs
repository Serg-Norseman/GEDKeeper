/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.Operations;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using GKUI;
using GKUI.Providers;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class BaseContextTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);
            AppHost.Container.Register<IProgressController, ProgressStub>(LifeCycle.Singleton, true);

            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }


        [Test]
        public void Test_Common()
        {
            Assert.IsNotNull(fContext.Culture);

            Assert.IsNull(fContext.Viewer);

            fContext.SetFileName("testfile.ged");
            Assert.AreEqual("testfile.ged", fContext.FileName);

            //

            fContext.CollectEventValues(null);


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

            fContext.LockRecord(iRec);
            fContext.UnlockRecord(iRec);

            Assert.AreEqual(false, fContext.IsChildless(iRec));

            Assert.IsNotNull(fContext.LangsList);
            Assert.AreEqual(0, fContext.LangsList.Count);
            fContext.CollectNameLangs(null);
            iRec.PersonalNames[0].Language.Value = GEDCOMLanguageID.AncientGreek;
            fContext.CollectNameLangs(iRec.PersonalNames[0]);
            Assert.AreEqual(1, fContext.LangsList.Count);
            
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

            Assert.Throws(typeof(ArgumentNullException), () => { fContext.GetStoreType(null); });
            var mediaStore = fContext.GetStoreType(new GEDCOMFileReference(fContext.Tree, null, "", "file.txt"));
            Assert.AreEqual(MediaStoreType.mstReference, mediaStore.StoreType);
            mediaStore = fContext.GetStoreType(new GEDCOMFileReference(fContext.Tree, null, "", "stg:file.txt"));
            Assert.AreEqual(MediaStoreType.mstStorage, mediaStore.StoreType);
            mediaStore = fContext.GetStoreType(new GEDCOMFileReference(fContext.Tree, null, "", "arc:file.txt"));
            Assert.AreEqual(MediaStoreType.mstArchive, mediaStore.StoreType);

            fContext.CollectEventValues(null);

            fContext.BeginUpdate();
            Assert.IsTrue(fContext.IsUpdated());
            fContext.EndUpdate();
            Assert.IsFalse(fContext.IsUpdated());

            fContext.DoUndo();
            fContext.DoRedo();
            fContext.DoCommit();
            fContext.DoRollback();

            // FIXME: error during execution of tests under TravisCI (a problem with UI)
            //var patrGraph = PatriarchsMan.GetPatriarchsGraph(fContext, 1, true, false);
            //Assert.IsNotNull(patrGraph);

            //

            var evt = fContext.CreateEventEx(iRec, "FACT", "17 JAN 2013", "Ivanovo");
            Assert.IsNotNull(evt);

            GEDCOMFamilyRecord fRec = fContext.Tree.CreateFamily();
            Assert.IsNotNull(fRec);

            evt = fContext.CreateEventEx(fRec, "MARR", "28 DEC 2013", "Ivanovo");
            Assert.IsNotNull(evt);
        }

        [Test]
        public void Test_Clear()
        {
            BaseContext context = TestUtils.CreateContext();
            TestUtils.FillContext(context);
            Assert.AreEqual(17, context.Tree.RecordsCount);

            context.Clear();
            Assert.AreEqual(0, context.Tree.RecordsCount);
        }

        [Test]
        public void Test_LoadAndSave()
        {
            string sourFile = TestUtils.PrepareTestFile("test1.ged");
            string destFile = TestUtils.GetTempFilePath("test11.ged");

            using (BaseContext ctx = new BaseContext(null)) {
                ctx.FileLoad(sourFile);
                ctx.FileSave(destFile);
                ctx.CriticalSave();
            }
        }

        [Test]
        public void Test_MediaLoadSave()
        {
            string sourFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");

            string gedFile = TestUtils.GetTempFilePath("test_mm.ged");

            using (BaseContext ctx = new BaseContext(null)) {
                Assert.IsTrue(ctx.IsUnknown());

                Assert.AreEqual(false, ctx.MediaSave(null, "", MediaStoreType.mstReference));
                Assert.AreEqual(null, ctx.LoadMediaImage(null, false));
                Assert.AreEqual(null, ctx.LoadMediaImage(null, 0, 0, ExtRect.Empty, false));
                Assert.AreEqual(null, ctx.GetPrimaryBitmap(null, 0, 0, false));
                Assert.AreEqual(null, ctx.GetPrimaryBitmapUID(null));

                ctx.FileSave(gedFile);
                Assert.AreEqual(true, ctx.CheckBasePath()); // need path for archive and storage

                var mmRecR = new GEDCOMMultimediaRecord(ctx.Tree, ctx.Tree, "", "");
                mmRecR.FileReferences.Add(new GEDCOMFileReferenceWithTitle(ctx.Tree, mmRecR, "", ""));
                Assert.AreEqual(true, ctx.MediaSave(mmRecR.FileReferences[0], sourFile, MediaStoreType.mstReference));
                Assert.IsNotNull(ctx.LoadMediaImage(mmRecR.FileReferences[0], false));
                Assert.IsNotNull(ctx.MediaLoad(mmRecR.FileReferences[0]));

                var mmRecA = new GEDCOMMultimediaRecord(ctx.Tree, ctx.Tree, "", "");
                mmRecA.FileReferences.Add(new GEDCOMFileReferenceWithTitle(ctx.Tree, mmRecA, "", ""));
                Assert.AreEqual(true, ctx.MediaSave(mmRecA.FileReferences[0], sourFile, MediaStoreType.mstArchive));
                Assert.IsNotNull(ctx.LoadMediaImage(mmRecA.FileReferences[0], false));
                Assert.IsNotNull(ctx.MediaLoad(mmRecA.FileReferences[0]));

                var mmRecS = new GEDCOMMultimediaRecord(ctx.Tree, ctx.Tree, "", "");
                mmRecS.FileReferences.Add(new GEDCOMFileReferenceWithTitle(ctx.Tree, mmRecS, "", ""));
                Assert.AreEqual(true, ctx.MediaSave(mmRecS.FileReferences[0], sourFile, MediaStoreType.mstStorage));
                Assert.IsNotNull(ctx.LoadMediaImage(mmRecS.FileReferences[0], false));
                Assert.IsNotNull(ctx.MediaLoad(mmRecS.FileReferences[0]));
            }
        }

        [Test]
        public void Test_CryptoLoadAndSave()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test1.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    string tempFileName = TestUtils.GetTempFilePath("test.geds");
                    ctx.SaveToSecFile(tempFileName, GEDCOMCharacterSet.csASCII, "test");

                    using (var ctx2 = new BaseContext(null)) {
                        ctx2.LoadFromSecFile(tempFileName, "test");
                    }
                }
            }
        }

        [Test]
        public void Test_IsRecordAccess()
        {
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
        }

        [Test]
        public void Test_Culture()
        {
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
        public void Test_X1()
        {
            
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
        public void Test_UndoManager()
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
        }

        [Test]
        public void Test_UndoRedo()
        {
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
    }
}
