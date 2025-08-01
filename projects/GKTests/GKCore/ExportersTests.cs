/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Export;
using GKCore.Options;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore
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
