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
using GDModel;
using GKCore;
using GKCore.Export;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using GKUI.Providers;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class ExportersTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);

            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        [Test]
        public void Test_PedigreeExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new PedigreeExporter(null, null); });

            using (PedigreeExporter exporter = new PedigreeExporter(baseWin, iRec)) {
                exporter.Options = GlobalOptions.Instance;
                Assert.IsNotNull(exporter.Options);

                Assert.AreEqual(iRec, exporter.Root);
                Assert.AreEqual(ShieldState.None, exporter.ShieldState);

                exporter.Options.PedigreeOptions.IncludeAttributes = true;
                exporter.Options.PedigreeOptions.IncludeNotes = true;
                exporter.Options.PedigreeOptions.IncludeSources = true;
                exporter.Options.PedigreeOptions.IncludeGenerations = true;

                exporter.Kind = PedigreeExporter.PedigreeKind.pkDescend_Konovalov;
                Assert.AreEqual(PedigreeExporter.PedigreeKind.pkDescend_Konovalov, exporter.Kind);

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new WriterStub()));

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Compact;
                Assert.IsTrue(exporter.Generate(new WriterStub()));


                exporter.Kind = PedigreeExporter.PedigreeKind.pkDescend_dAboville;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new WriterStub()));


                exporter.Kind = PedigreeExporter.PedigreeKind.pkAscend;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(new WriterStub()));
            }
        }

        [Test]
        public void Test_ExcelExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new ExcelExporter(null); });

            using (ExcelExporter exporter = new ExcelExporter(baseWin)) {

            }
        }

        [Test]
        public void Test_FamilyBookExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new FamilyBookExporter(null); });

            using (FamilyBookExporter exporter = new FamilyBookExporter(baseWin)) {

            }
        }

        [Test]
        public void Test_TreesAlbumExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new TreesAlbumExporter(null); });

            using (TreesAlbumExporter exporter = new TreesAlbumExporter(baseWin)) {

            }
        }

        private void EWriter_Test(CustomWriter writer)
        {
        }

        [Test]
        public void Test_Writers()
        {
            using (HTMLWriter writer = new HTMLWriter()) {
                EWriter_Test(writer);
            }

            using (RTFWriter writer = new RTFWriter()) {
                EWriter_Test(writer);
            }

            /*using (PDFWriter writer = new PDFWriter()) {
                // TravisCI crash
            }*/
        }
    }
}
