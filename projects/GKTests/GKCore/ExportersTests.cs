// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using GDModel;
using GKCore.Export.Formats;
using GKCore.Options;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Export
{
    [TestFixture]
    public class ExportersTests
    {
        private BaseContext fContext;

        public ExportersTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_PedigreeExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
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

                exporter.Type = PedigreeType.Descend;
                Assert.AreEqual(PedigreeType.Descend, exporter.Type);

                var writer = Substitute.For<CustomWriter>();

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(writer));

                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Compact;
                Assert.IsTrue(exporter.Generate(writer));


                exporter.Type = PedigreeType.Descend;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(writer));


                exporter.Type = PedigreeType.Ascend;
                exporter.Options.PedigreeOptions.Format = PedigreeFormat.Excess;
                Assert.IsTrue(exporter.Generate(writer));
            }
        }

        [Test]
        public void Test_ExcelExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new TableExporter(null); });

            using (TableExporter exporter = new TableExporter(baseWin)) {

            }
        }

        [Test]
        public void Test_FamilyBookExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
            baseWin.Context.ShieldState = ShieldState.None;

            Assert.Throws(typeof(ArgumentNullException), () => { new FamilyBookExporter(null); });

            using (FamilyBookExporter exporter = new FamilyBookExporter(baseWin)) {

            }
        }

        [Test]
        public void Test_TreesAlbumExporter()
        {
            BaseWindowStub baseWin = new BaseWindowStub();
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
