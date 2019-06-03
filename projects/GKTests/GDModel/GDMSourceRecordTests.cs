/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
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

                src1.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, src1.GetTree());
            }

            // check move
            using (GDMSourceRecord src1 = new GDMSourceRecord(fContext.Tree)) {
                Assert.Throws(typeof(ArgumentException), () => {
                    src1.MoveTo(null, false);
                });

                // fill the record
                src1.ShortTitle = "test source";
                src1.Title = new StringList("test title");
                src1.Originator = new StringList("test author");
                src1.Publication = new StringList("test publ");
                src1.Text = new StringList("test text");

                Assert.AreEqual("test source", src1.ShortTitle);
                Assert.AreEqual("test title", src1.Title.Text);
                Assert.AreEqual("test author", src1.Originator.Text);
                Assert.AreEqual("test publ", src1.Publication.Text);
                Assert.AreEqual("test text", src1.Text.Text);

                GDMRepositoryRecord repRec = fContext.Tree.CreateRepository();
                repRec.RepositoryName = "test repository";
                src1.AddRepository(repRec);
                Assert.AreEqual(1, src1.RepositoryCitations.Count);

                using (GDMSourceRecord src2 = new GDMSourceRecord(fContext.Tree)) {
                    src2.ShortTitle = "test source 2"; // title isn't replaced

                    Assert.AreEqual(0, src2.RepositoryCitations.Count);

                    src1.MoveTo(src2, false);

                    Assert.AreEqual("test source 2", src2.ShortTitle);

                    Assert.AreEqual("test title", src2.Title.Text);
                    Assert.AreEqual("test author", src2.Originator.Text);
                    Assert.AreEqual("test publ", src2.Publication.Text);
                    Assert.AreEqual("test text", src2.Text.Text);

                    Assert.AreEqual(1, src2.RepositoryCitations.Count);
                }
            }
        }

        [Test]
        public void Test_GDMSourceCitation()
        {
            GDMIndividualRecord indiv = new GDMIndividualRecord(fContext.Tree);
            GDMSourceRecord sourRec = new GDMSourceRecord(fContext.Tree);

            using (GDMSourceCitation srcCit = indiv.AddSource(sourRec, "p2", 3)) {
                Assert.IsNotNull(srcCit);

                int idx = indiv.IndexOfSource(sourRec);
                Assert.AreEqual(0, idx);

                Assert.AreEqual("p2", srcCit.Page);
                Assert.AreEqual(3, srcCit.CertaintyAssessment);

                Assert.IsTrue(srcCit.IsPointer, "srcCit.IsPointer");

                Assert.IsFalse(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer

                srcCit.Clear();
                srcCit.Value = null;

                Assert.IsTrue(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer

                StringList strs = new StringList("test");
                srcCit.Description = strs;

                strs = srcCit.Description;
                Assert.AreEqual("test", strs.Text);
            }
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
            sourRec.Originator = new StringList("author");
            Assert.AreEqual("author", sourRec.Originator.Text.Trim());

            sourRec.Title = new StringList("title");
            Assert.AreEqual("title", sourRec.Title.Text.Trim());

            sourRec.Publication = new StringList("publication");
            Assert.AreEqual("publication", sourRec.Publication.Text.Trim());

            sourRec.Text = new StringList("sample");
            Assert.AreEqual("sample", sourRec.Text.Text.Trim());

            //
            sourRec.SetOriginatorArray(new string[] {"author"});
            Assert.AreEqual("author", sourRec.Originator.Text.Trim());

            sourRec.SetTitleArray(new string[] {"title"});
            Assert.AreEqual("title", sourRec.Title.Text.Trim());

            sourRec.SetPublicationArray(new string[] {"publication"});
            Assert.AreEqual("publication", sourRec.Publication.Text.Trim());

            sourRec.SetTextArray(new string[] {"sample"});
            Assert.AreEqual("sample", sourRec.Text.Text.Trim());

            //
            GEDCOMRepositoryCitationTest(sourRec, repRec);

            string buf = TestUtils.GetTagStreamText(sourRec, 0);
            Assert.AreEqual("0 @S1@ SOUR\r\n"+
                            "1 DATA\r\n"+
                            "1 ABBR This is test source\r\n"+
                            "1 AUTH author\r\n"+
                            "1 TITL title\r\n"+
                            "1 PUBL publication\r\n"+
                            "1 TEXT sample\r\n"+
                            "1 REPO @R2@\r\n", buf);

            //
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
