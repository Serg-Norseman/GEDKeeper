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
using GKCore;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSourceRecordTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            fContext = TestUtils.CreateContext();
        }

        [Test]
        public void Test_Common()
        {
            // check match
            using (GDMSourceRecord src1 = new GDMSourceRecord(fContext.Tree)) {
                Assert.IsNotNull(src1, "src1 != null");

                Assert.Throws(typeof(ArgumentNullException), () => {
                    src1.RemoveRepository(null);
                });

                using (GDMSourceRecord src2 = new GDMSourceRecord(fContext.Tree)) {
                    Assert.IsNotNull(src2, "src2 != null");

                    Assert.AreEqual(0.0f, src1.IsMatch(null, new MatchParams()));

                    // empty records
                    Assert.AreEqual(100.0f, src1.IsMatch(src2, new MatchParams()));

                    // filled records
                    src1.ShortTitle = "test source";
                    src2.ShortTitle = "test source";
                    Assert.AreEqual(100.0f, src1.IsMatch(src2, new MatchParams()));
                }

                src1.ResetTree(fContext.Tree);
            }

            // check move
            using (GDMSourceRecord src1 = new GDMSourceRecord(fContext.Tree)) {
                Assert.Throws(typeof(ArgumentException), () => {
                    src1.MoveTo(null);
                });

                // fill the record
                src1.ShortTitle = "test source";
                src1.Title.Lines.Text = ("test title");
                src1.Originator.Lines.Text = ("test author");
                src1.Publication.Lines.Text = ("test publ");
                src1.Text.Lines.Text = ("test text");

                Assert.AreEqual("test source", src1.ShortTitle);
                Assert.AreEqual("test title", src1.Title.Lines.Text);
                Assert.AreEqual("test author", src1.Originator.Lines.Text);
                Assert.AreEqual("test publ", src1.Publication.Lines.Text);
                Assert.AreEqual("test text", src1.Text.Lines.Text);

                src1.ReplaceXRefs(new GDMXRefReplacer());

                GDMRepositoryRecord repRec = fContext.Tree.CreateRepository();
                repRec.RepositoryName = "test repository";
                src1.AddRepository(repRec);
                Assert.AreEqual(1, src1.RepositoryCitations.Count);

                using (GDMSourceRecord src2 = new GDMSourceRecord(fContext.Tree)) {
                    src2.ShortTitle = "test source 2"; // title isn't replaced

                    Assert.AreEqual(0, src2.RepositoryCitations.Count);

                    src1.MoveTo(src2);

                    Assert.AreEqual("test source 2", src2.ShortTitle);

                    Assert.AreEqual("test title", src2.Title.Lines.Text);
                    Assert.AreEqual("test author", src2.Originator.Lines.Text);
                    Assert.AreEqual("test publ", src2.Publication.Lines.Text);
                    Assert.AreEqual("test text", src2.Text.Lines.Text);

                    Assert.AreEqual(1, src2.RepositoryCitations.Count);
                }
            }
        }

        [Test]
        public void Test_GDMSourceCitation()
        {
            GDMIndividualRecord indiv = new GDMIndividualRecord(fContext.Tree);

            GDMSourceRecord sourRec = new GDMSourceRecord(fContext.Tree);
            fContext.Tree.NewXRef(sourRec);

            using (GDMSourceCitation srcCit = indiv.AddSource(sourRec, "p2", 3)) {
                Assert.IsNotNull(srcCit);

                int idx = indiv.IndexOfSource(sourRec);
                Assert.AreEqual(0, idx);

                var foundSrcCit = indiv.FindSourceCitation(sourRec);
                Assert.AreEqual(srcCit, foundSrcCit);

                Assert.AreEqual("p2", srcCit.Page);
                Assert.AreEqual(3, srcCit.CertaintyAssessment);
                Assert.AreEqual(3, srcCit.GetValidCertaintyAssessment());

                Assert.IsTrue(srcCit.IsPointer, "srcCit.IsPointer");

                Assert.IsFalse(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer

                Assert.Throws(typeof(ArgumentException), () => {
                    srcCit.Assign(null);
                });

                srcCit.Clear();
                srcCit.XRef = string.Empty;

                Assert.IsTrue(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer

                srcCit.Description.Text = "test";
                Assert.AreEqual("test", srcCit.Description.Text);
            }
        }

        [Test]
        public void Test_GDMSourceCitation2()
        {
            string text = "0 @I1@ INDI\n1 SOUR Textual source citation\n2 CONT continue tag...";
            GDMIndividualRecord iRec = TestUtils.ParseIndiRec(text);
            Assert.AreEqual(1, iRec.SourceCitations.Count);
            var sourCit = iRec.SourceCitations[0];
            Assert.AreEqual("Textual source citation\r\ncontinue tag...", sourCit.Description.Text);
        }

        [Test]
        public void Test_GDMSourceCitation3()
        {
            string text = "0 @I1@ INDI\n1 SOUR @S1@";
            GDMIndividualRecord iRec = TestUtils.ParseIndiRec(text);
            Assert.AreEqual(1, iRec.SourceCitations.Count);
            var sourCit = iRec.SourceCitations[0];
            Assert.AreEqual("S1", sourCit.XRef);
        }

        [Test]
        public void Test_Common2()
        {
            GDMSourceRecord sourRec = fContext.Tree.CreateSource();
            GDMIndividualRecord indiv = fContext.Tree.CreateIndividual();
            GDMRepositoryRecord repRec = fContext.Tree.CreateRepository();

            Assert.IsNotNull(sourRec.Data);

            sourRec.ShortTitle = "This is test source";
            Assert.AreEqual("This is test source", sourRec.ShortTitle);

            //
            sourRec.Originator.Lines.Text = ("author");
            Assert.AreEqual("author", sourRec.Originator.Lines.Text);

            sourRec.Title.Lines.Text = ("title");
            Assert.AreEqual("title", sourRec.Title.Lines.Text);

            sourRec.Publication.Lines.Text = ("publication");
            Assert.AreEqual("publication", sourRec.Publication.Lines.Text);

            sourRec.Text.Lines.Text = ("sample");
            Assert.AreEqual("sample", sourRec.Text.Lines.Text);

            //
            sourRec.SetOriginatorArray(new string[] {"author1", "author2", "author3", "author4"});
            Assert.AreEqual("author1\r\nauthor2\r\nauthor3\r\nauthor4", sourRec.Originator.Lines.Text);

            sourRec.SetTitleArray(new string[] {"title"});
            Assert.AreEqual("title", sourRec.Title.Lines.Text);

            sourRec.SetPublicationArray(new string[] {"publication"});
            Assert.AreEqual("publication", sourRec.Publication.Lines.Text);

            sourRec.SetTextArray(new string[] {"sample"});
            Assert.AreEqual("sample", sourRec.Text.Lines.Text);

            //
            GEDCOMRepositoryCitationTest(sourRec, repRec);

            using (GDMSourceRecord sour2 = fContext.Tree.CreateSource()) {
                Assert.Throws(typeof(ArgumentException), () => {
                    sour2.Assign(null);
                });

                sour2.Assign(sourRec);

                string buf = TestUtils.GetTagStreamText(sour2, 0);
                Assert.AreEqual("0 @S2@ SOUR\r\n" +
                                "1 TITL title\r\n" +
                                "1 PUBL publication\r\n" +
                                "1 ABBR This is test source\r\n" +
                                "1 REPO @R2@\r\n" +
                                "1 AUTH author1\r\n" +
                                "2 CONT author2\r\n" +
                                "2 CONT author3\r\n" +
                                "2 CONT author4\r\n" +
                                "1 TEXT sample\r\n", buf);
            }

            sourRec.ReplaceXRefs(new GDMXRefReplacer());

            Assert.IsFalse(sourRec.IsEmpty());
            sourRec.Clear();
            Assert.IsTrue(sourRec.IsEmpty());
        }

        private static void GEDCOMRepositoryCitationTest(GDMSourceRecord sourRec, GDMRepositoryRecord repRec)
        {
            GDMRepositoryCitation repCit = sourRec.AddRepository(repRec);

            Assert.IsFalse(repCit.IsEmpty(), "repCit.IsEmpty()"); // its pointer
        }
    }
}
