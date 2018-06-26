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
using GKCore.Types;
using GKTests;
using GKUI;
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
            BaseContext context = TestStubs.CreateContext();
            TestStubs.FillContext(context);
            Assert.AreEqual(15, context.Tree.RecordsCount);

            context.Clear();
            Assert.AreEqual(0, context.Tree.RecordsCount);
        }

        [Test]
        public void Test_CryptoLoadAndSave()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test1.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    string tempFileName = TestStubs.GetTempFilePath("test.geds");
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
        public void Test_X1()
        {
            
        }
    }
}
