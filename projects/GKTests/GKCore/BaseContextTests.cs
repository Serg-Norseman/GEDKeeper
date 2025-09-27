﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Cultures;
using GKCore.Media;
using GKCore.Operations;
using GKCore.Options;
using GKTests;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class BaseContextTests
    {
        private readonly BaseContext fContext;

        public BaseContextTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Exceptions()
        {
            var instance = new MediaFileNotFoundException("test.ogg");
            Assert.AreEqual("Media file 'test.ogg' not found", instance.Message);
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
        public async Task Test_DeleteRecord()
        {
            Assert.IsFalse(await fContext.DeleteRecord(null));

            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateIndividual()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateFamily()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateNote()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateMultimedia()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateSource()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateRepository()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateGroup()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateResearch()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateTask()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateCommunication()));
            Assert.IsTrue(await fContext.DeleteRecord(fContext.Tree.CreateLocation()));
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

            Assert.IsNotNull(fContext.LangStats);
            Assert.AreEqual(0, fContext.LangStats.Count);
            fContext.CollectNameLangs(null);
            iRec.PersonalNames[0].Language = GDMLanguageID.AncientGreek;
            fContext.CollectNameLangs(iRec.PersonalNames[0]);
            Assert.AreEqual(1, fContext.LangStats.Count);
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

            var evt = new GDMIndividualAttribute((int)GEDCOMTagType._AWARD, "Congressional Gold Medal");

            fContext.CollectEventValues(evt);
            Assert.AreEqual(1, fContext.ValuesCollection.Count);
        }

        [Test]
        public void Test_LoadAndSave()
        {
            string sourFile = TestUtils.PrepareTestFile("test1.ged");
            string destFile = TestUtils.GetTempFilePath("test11.ged", out _);
            string restoreFile = Path.ChangeExtension(destFile, ".restore");

            try {
                using (BaseContext ctx = new BaseContext(null)) {
                    ctx.FileLoad(sourFile, false);
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
            string gedFile = TestUtils.GetTempFilePath("test_mm.ged", out _);
            string stgDirectory = string.Empty, arcFileName = string.Empty;

            try {
                using (BaseContext ctx = new BaseContext(null)) {
                    Assert.IsTrue(ctx.IsUnknown());

                    Assert.AreEqual(false, ctx.MediaSave(null, "", MediaStoreType.mstReference));
                    Assert.AreEqual(null, ctx.LoadMediaImage(null, -1, -1, ExtRect.Empty, false, false));
                    Assert.AreEqual(null, ctx.LoadMediaImage(null, 0, 0, ExtRect.Empty, false, false));
                    Assert.AreEqual(null, ctx.GetPrimaryBitmap(null, 0, 0, false));
                    Assert.AreEqual(null, ctx.GetPrimaryBitmapUID(null));

                    ctx.FileSave(gedFile);
                    Assert.AreEqual(true, ctx.CheckBasePath()); // need path for archive and storage

                    var mmRecR = new GDMMultimediaRecord(ctx.Tree);
                    mmRecR.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecR.FileReferences[0], sourFile, MediaStoreType.mstReference));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecR, -1, -1, ExtRect.Empty, false, false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecR.FileReferences[0]));

                    var mmRecA = new GDMMultimediaRecord(ctx.Tree);
                    mmRecA.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecA.FileReferences[0], sourFile, MediaStoreType.mstArchive));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecA, -1, -1, ExtRect.Empty, false, false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecA.FileReferences[0]));
                    arcFileName = ctx.GetArcFileName();

                    /*var mmRecS = new GDMMultimediaRecord(ctx.Tree);
                    mmRecS.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecS.FileReferences[0], sourFile, MediaStoreType.mstStorage));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecS, -1, -1, ExtRect.Empty, false, false));
                    Assert.IsNotNull(ctx.MediaLoad(mmRecS.FileReferences[0]));
                    stgDirectory = ctx.GetStgFolder();*/

                    var mmRecRl = new GDMMultimediaRecord(ctx.Tree);
                    mmRecRl.FileReferences.Add(new GDMFileReferenceWithTitle());
                    Assert.AreEqual(true, ctx.MediaSave(mmRecRl.FileReferences[0], sourFile, MediaStoreType.mstRelativeReference));
                    Assert.IsNotNull(ctx.LoadMediaImage(mmRecRl, -1, -1, ExtRect.Empty, false, false));
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
                    gedcomProvider.LoadFromStream(stmGed1);

                    string tempFileName = TestUtils.GetTempFilePath("test.geds", out _);

                    try {
                        const string password = "test";
                        var provider1 = new SecGEDCOMProvider(ctx.Tree, password, GlobalOptions.Instance.KeepRichNames, false);
                        GKUtils.PrepareHeader(ctx.Tree, tempFileName, GEDCOMCharacterSet.csASCII, false);
                        provider1.SaveToFile(tempFileName, GEDCOMCharacterSet.csASCII);

                        using (var ctx2 = new BaseContext(null)) {
                            var provider2 = new SecGEDCOMProvider(ctx2.Tree, password);
                            provider2.LoadFromFile(tempFileName);
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

            fContext.Tree.Header.Language = GDMLanguageID.Czech;
            Assert.IsInstanceOf(typeof(CzechCulture), fContext.Culture);

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
            Assert.IsTrue(fContext.Culture.HasPatronymic);
            Assert.IsTrue(fContext.Culture.HasSurname);

            fContext.Tree.Header.Language = GDMLanguageID.Turkish;
            Assert.IsInstanceOf(typeof(TurkishCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic);
            Assert.IsTrue(fContext.Culture.HasSurname);

            fContext.Tree.Header.Language = GDMLanguageID.French;
            Assert.IsInstanceOf(typeof(FrenchCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Italian;
            Assert.IsInstanceOf(typeof(ItalianCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Cantonese;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);

            fContext.Tree.Header.Language = GDMLanguageID.Mandrin;
            Assert.IsInstanceOf(typeof(ChineseCulture), fContext.Culture);
            Assert.IsFalse(fContext.Culture.HasPatronymic);
            Assert.IsTrue(fContext.Culture.HasSurname);
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
                              fContext.Undoman.DoIndividualNameChange(null, "", "", "", "", ""); });
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetNameString(iRec, true, false));
            fContext.Undoman.DoIndividualNameChange(iRec, "Petrov", "Alex", "Ivanovich", "", "");
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
            Assert.ThrowsAsync(typeof(ArgumentNullException), async () => { await fContext.DeleteMediaRecord(null); });
        }

        [Test]
        public void Test_FindAll()
        {
            var result = fContext.FindAll(GDMRecordType.rtIndividual, "ZZZ");
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void Test_CheckPersonSex()
        {
            Assert.ThrowsAsync(typeof(ArgumentNullException), async () => { await fContext.CheckPersonSex(null, null); });
        }

        [Test]
        public async Task Test_MediaDelete()
        {
            var result = await fContext.MediaDelete(null);
            Assert.IsFalse(result);
        }

        [Test]
        public void Test_VerifyMediaFile()
        {
            Assert.Throws(typeof(ArgumentNullException), () => {
                string strFileRef = null;
                fContext.VerifyMediaFile(strFileRef, out _);
            });

            Assert.AreEqual(MediaStoreStatus.mssFileNotFound, fContext.VerifyMediaFile("file:///randomfile.jpg", out _));
            Assert.AreEqual(MediaStoreStatus.mssFileNotFound, fContext.VerifyMediaFile("file:./randomfile.jpg", out _));
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
        public async Task Test_GetChildFamily()
        {
            var result = await fContext.GetChildFamily(null, false, null);
            Assert.IsNull(result);
        }

        [Test]
        public void Test_AddFamilyForSpouse()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fContext.AddFamilyForSpouse(null); });
        }

        [Test]
        public async Task Test_AddChildForParent()
        {
            //var result = await fContext.AddChildForParent(null, null, GDMSex.svMale);
            //Assert.IsNull(result);
        }

        [Test]
        public void Test_SelectSpouseFor()
        {
            //Assert.ThrowsAsync(typeof(ArgumentNullException), async () => { await fContext.SelectSpouseFor(null, null); });
        }
    }
}
