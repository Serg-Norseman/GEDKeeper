/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Cultures;
using GKCore.Operations;
using GKCore.Types;
using GKTests;
using GKUI.Platform;
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
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);
            TestUtils.InitProgressStub();
            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            Assert.IsNull(fContext.Viewer);

            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.AreEqual(1990, iRec.GetChronologicalYear(GEDCOMTagName.BIRT));

            Assert.AreEqual(1990, fContext.FindBirthYear(iRec));
            Assert.AreEqual(2010, fContext.FindDeathYear(iRec));

            // FIXME: error during execution of tests under TravisCI (a problem with UI)
            //var patrGraph = PatriarchsMan.GetPatriarchsGraph(fContext, 1, true, false);
            //Assert.IsNotNull(patrGraph);
        }

        [Test]
        public void Test_IsChildless()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            Assert.AreEqual(false, fContext.IsChildless(iRec));
        }

        [Test]
        public void Test_LockRecord()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            fContext.LockRecord(iRec);
            fContext.UnlockRecord(iRec);
        }

        [Test]
        public void Test_DeleteRecord()
        {
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
        }

        [Test]
        public void Test_CreateEventEx()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            var evt = fContext.CreateEventEx(iRec, GEDCOMTagName.FACT, "17 JAN 2013", "Ivanovo");
            Assert.IsNotNull(evt);

            GDMFamilyRecord fRec = fContext.Tree.CreateFamily();
            Assert.IsNotNull(fRec);

            evt = fContext.CreateEventEx(fRec, GEDCOMTagName.MARR, "28 DEC 2013", "Ivanovo");
            Assert.IsNotNull(evt);
        }

        [Test]
        public void Test_Updating()
        {
            fContext.BeginUpdate();
            Assert.IsTrue(fContext.IsUpdated());
            fContext.EndUpdate();
            Assert.IsFalse(fContext.IsUpdated());
        }

        [Test]
        public void Test_CollectNameLangs()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.IsNotNull(fContext.LangsList);
            Assert.AreEqual(0, fContext.LangsList.Count);
            fContext.CollectNameLangs(null);
            iRec.PersonalNames[0].Language = GDMLanguageID.AncientGreek;
            fContext.CollectNameLangs(iRec.PersonalNames[0]);
            Assert.AreEqual(1, fContext.LangsList.Count);
        }

        [Test]
        public void Test_SetFileName()
        {
            fContext.SetFileName("testfile.ged");
            Assert.AreEqual("testfile.ged", fContext.FileName);
        }

        [Test]
        public void Test_FindSource()
        {
            GDMSourceRecord srcRec = fContext.FindSource("test source");
            Assert.IsNull(srcRec);
        }

        [Test]
        public void Test_GetSourcesList()
        {
            var sources = new StringList();
            fContext.GetSourcesList(sources);
            Assert.AreEqual(1, sources.Count);
        }

        [Test]
        public void Test_SwitchShieldState()
        {
            Assert.AreEqual(ShieldState.Maximum, fContext.ShieldState, "BaseContext.ShieldState.1");

            fContext.SwitchShieldState();
            Assert.AreEqual(ShieldState.Middle, fContext.ShieldState, "BaseContext.ShieldState.2");

            fContext.SwitchShieldState();
            Assert.AreEqual(ShieldState.None, fContext.ShieldState, "BaseContext.ShieldState.3");

            fContext.SwitchShieldState();
            Assert.AreEqual(ShieldState.Maximum, fContext.ShieldState, "BaseContext.ShieldState.4");
        }

        [Test]
        public void Test_GetStoreType()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.GetStoreType(null); });

            var fileRef = new GDMFileReference();
            fileRef.ParseString("file.txt");
            var mediaStore = fContext.GetStoreType(fileRef);
            Assert.AreEqual(MediaStoreType.mstReference, mediaStore.StoreType);

            fileRef.ParseString("stg:file.txt");
            mediaStore = fContext.GetStoreType(fileRef);
            Assert.AreEqual(MediaStoreType.mstStorage, mediaStore.StoreType);

            fileRef.ParseString("arc:file.txt");
            mediaStore = fContext.GetStoreType(fileRef);
            Assert.AreEqual(MediaStoreType.mstArchive, mediaStore.StoreType);

            fileRef.ParseString("rel:file.txt");
            mediaStore = fContext.GetStoreType(fileRef);
            Assert.AreEqual(MediaStoreType.mstRelativeReference, mediaStore.StoreType);
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
        public void Test_CollectEventValues()
        {
            Assert.IsNotNull(fContext.ValuesCollection);

            fContext.CollectEventValues(null);
            Assert.AreEqual(0, fContext.ValuesCollection.Count);

            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            var evt = new GDMIndividualAttribute((int)GEDCOMTagType._AWARD, "Congressional Gold Medal");

            fContext.CollectEventValues(evt);
            Assert.AreEqual(1, fContext.ValuesCollection.Count);
        }

        [Test]
        public void Test_LoadAndSave()
        {
            string sourFile = TestUtils.PrepareTestFile("test1.ged");
            string destFile = TestUtils.GetTempFilePath("test11.ged");
            string restoreFile = Path.ChangeExtension(destFile, ".restore");

            try {
                using (BaseContext ctx = new BaseContext(null)) {
                    ctx.FileLoad(sourFile);
                    ctx.FileSave(destFile);
                    ctx.CriticalSave();
                }
            } finally {
                TestUtils.RemoveTestFile(sourFile);
                TestUtils.RemoveTestFile(destFile);
                TestUtils.RemoveTestFile(restoreFile);
            }
        }

        [Test]
        public void Test_MediaLoadSave()
        {
            string sourFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");
            string gedFile = TestUtils.GetTempFilePath("test_mm.ged");
            string stgDirectory = string.Empty, arcFileName = string.Empty;

            try {
                using (BaseContext ctx = new BaseContext(null)) {
                    Assert.IsTrue(ctx.IsUnknown());

                    Assert.AreEqual(false, ctx.MediaSave(null, "", MediaStoreType.mstReference));
                    Assert.AreEqual(null, ctx.LoadMediaImage(null, false));
                    Assert.AreEqual(null, ctx.LoadMediaImage(null, 0, 0, ExtRect.Empty, false));
                    Assert.AreEqual(null, ctx.GetPrimaryBitmap(null, 0, 0, false));
                    Assert.AreEqual(null, ctx.GetPrimaryBitmapUID(null));

                    ctx.FileSave(gedFile);
                    Assert.AreEqual(true, ctx.CheckBasePath()); // need path for archive and storage

                    var mmRecR = new GDMMultimediaRecord(ctx.Tree);
                    mmRecR.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecR.FileReferences[0], sourFile, MediaStoreType.mstReference));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecR.FileReferences[0], false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecR.FileReferences[0]));

                    var mmRecA = new GDMMultimediaRecord(ctx.Tree);
                    mmRecA.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecA.FileReferences[0], sourFile, MediaStoreType.mstArchive));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecA.FileReferences[0], false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecA.FileReferences[0]));
                    arcFileName = ctx.GetArcFileName();

                    var mmRecS = new GDMMultimediaRecord(ctx.Tree);
                    mmRecS.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecS.FileReferences[0], sourFile, MediaStoreType.mstStorage));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecS.FileReferences[0], false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecS.FileReferences[0]));
                    stgDirectory = ctx.GetStgFolder(false);

                    var mmRecRl = new GDMMultimediaRecord(ctx.Tree);
                    mmRecRl.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecRl.FileReferences[0], sourFile, MediaStoreType.mstRelativeReference));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecRl.FileReferences[0], false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecRl.FileReferences[0]));
                }
            } finally {
                TestUtils.RemoveTestFile(sourFile);
                TestUtils.RemoveTestFile(gedFile);
                TestUtils.RemoveTestFile(arcFileName);
                TestUtils.RemoveTestDirectory(stgDirectory);
            }
        }

        [Test]
        public void Test_CryptoLoadAndSave()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test1.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    string tempFileName = TestUtils.GetTempFilePath("test.geds");

                    try {
                        ctx.SaveToSecFile(tempFileName, GEDCOMCharacterSet.csASCII, "test");

                        using (var ctx2 = new BaseContext(null)) {
                            ctx2.LoadFromSecFile(tempFileName, "test");
                        }
                    } finally {
                        TestUtils.RemoveTestFile(tempFileName);
                    }
                }
            }
        }

        [Test]
        public void Test_IsRecordAccess()
        {
            fContext.ShieldState = ShieldState.None;
            Assert.IsTrue(fContext.IsRecordAccess(GDMRestriction.rnNone));
            Assert.IsTrue(fContext.IsRecordAccess(GDMRestriction.rnConfidential));
            Assert.IsTrue(fContext.IsRecordAccess(GDMRestriction.rnPrivacy));

            fContext.ShieldState = ShieldState.Middle;
            Assert.IsTrue(fContext.IsRecordAccess(GDMRestriction.rnNone));
            Assert.IsTrue(fContext.IsRecordAccess(GDMRestriction.rnConfidential));
            Assert.IsFalse(fContext.IsRecordAccess(GDMRestriction.rnPrivacy));

            fContext.ShieldState = ShieldState.Maximum;
            Assert.IsTrue(fContext.IsRecordAccess(GDMRestriction.rnNone));
            Assert.IsFalse(fContext.IsRecordAccess(GDMRestriction.rnConfidential));
            Assert.IsFalse(fContext.IsRecordAccess(GDMRestriction.rnPrivacy));
        }

        [Test]
        public void Test_Culture()
        {
            Assert.IsNotNull(fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.German;
            Assert.IsInstanceOf(typeof(GermanCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Polish;
            Assert.IsInstanceOf(typeof(PolishCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Swedish;
            Assert.IsInstanceOf(typeof(SwedishCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Icelandic;
            Assert.IsInstanceOf(typeof(IcelandCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Russian;
            Assert.IsInstanceOf(typeof(RussianCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Ukrainian;
            Assert.IsInstanceOf(typeof(RussianCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Armenian;
            Assert.IsInstanceOf(typeof(ArmenianCulture), fContext.Culture);
            Assert.IsTrue(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());

            fContext.Tree.Header.Language = GDMLanguageID.Turkish;
            Assert.IsInstanceOf(typeof(TurkishCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());

            fContext.Tree.Header.Language = GDMLanguageID.French;
            Assert.IsInstanceOf(typeof(FrenchCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Italian;
            Assert.IsInstanceOf(typeof(ItalianCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Cantonese;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Mandrin;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic());
            Assert.IsTrue(fContext.Culture.HasSurname());
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
            var undoman = new UndoManager();
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

        [Test]
        public void Test_UndoRedo()
        {
            fContext.DoUndo();
            fContext.DoRedo();
            fContext.DoCommit();
            fContext.DoRollback();

            Assert.AreEqual(fContext, fContext.Undoman.Context);

            fContext.Undoman.OnTransaction += TransactionEventHandler;

            fContext.Undoman.Clear();

            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
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

            iRec.Sex = GDMSex.svUnknown;
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GDMSex.svMale);
            Assert.AreEqual(GDMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.AreEqual(GDMSex.svUnknown, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());

            Assert.IsTrue(fContext.Undoman.CanRedo());
            fContext.Undoman.Redo();
            Assert.AreEqual(GDMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());

            fContext.Undoman.Clear();

            iRec.Bookmark = false;
            iRec.Patriarch = false;
            iRec.Sex = GDMSex.svUnknown;

            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GDMSex.svMale);
            fContext.Undoman.Commit();
            Assert.IsTrue(iRec.Bookmark);
            Assert.IsTrue(iRec.Patriarch);
            Assert.AreEqual(GDMSex.svMale, iRec.Sex);
            Assert.IsTrue(fContext.Undoman.CanUndo());
            fContext.Undoman.Undo();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GDMSex.svUnknown, iRec.Sex);
            Assert.IsFalse(fContext.Undoman.CanUndo());


            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualBookmarkChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualPatriarchChange, iRec, true);
            fContext.Undoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, iRec, GDMSex.svMale);
            fContext.Undoman.Rollback();
            Assert.IsFalse(iRec.Bookmark);
            Assert.IsFalse(iRec.Patriarch);
            Assert.AreEqual(GDMSex.svUnknown, iRec.Sex);
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
        public void Test_Modified()
        {
            Assert.IsTrue(fContext.Modified);
        }

        [Test]
        public void Test_DeleteMediaRecord()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.DeleteMediaRecord(null); });
        }

        [Test]
        public void Test_FindAll()
        {
            var result = fContext.FindAll(GDMRecordType.rtIndividual, "ZZZ");
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void Test_CollectTips()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.CollectTips(null); });
        }

        [Test]
        public void Test_CheckPersonSex()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.CheckPersonSex(null); });
        }

        [Test]
        public void Test_MediaDelete()
        {
            var result = fContext.MediaDelete(null);
            Assert.IsFalse(result);
        }

        [Test]
        public void Test_VerifyMediaFile()
        {
            string fileName;
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.VerifyMediaFile(null, out fileName); });
        }

        [Test]
        public void Test_MediaExists()
        {
            var result = fContext.MediaExists(null);
            Assert.IsFalse(result);
        }

        [Test]
        public void Test_IsAvailableRecord()
        {
            var result = fContext.IsAvailableRecord(null);
            Assert.IsTrue(result);
        }

        [Test]
        public void Test_GetChildFamily()
        {
            var result = fContext.GetChildFamily(null, false, null);
            Assert.IsNull(result);
        }

        [Test]
        public void Test_AddFamilyForSpouse()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.AddFamilyForSpouse(null); });
        }

        [Test]
        public void Test_AddChildForParent()
        {
            var result = fContext.AddChildForParent(null, GDMSex.svMale);
            Assert.IsNull(result);
        }

        [Test]
        public void Test_SelectSpouseFor()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.SelectSpouseFor(null); });
        }
    }
}
