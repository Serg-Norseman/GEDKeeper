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
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMMultimediaRecordTests
    {
        private BaseContext fContext;

        public GDMMultimediaRecordTests()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            GDMIndividualRecord indiv = fContext.Tree.CreateIndividual();

            using (GDMMultimediaRecord mediaRec = fContext.Tree.CreateMultimedia()) {
                Assert.IsNotNull(mediaRec);

                mediaRec.ResetTree(fContext.Tree);

                Assert.AreEqual("", mediaRec.GetFileTitle());

                mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
                GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
                Assert.IsNotNull(fileRef);

                fileRef.Title = "File Title 2";
                Assert.AreEqual("File Title 2", fileRef.Title);

                fileRef.LinkFile("sample.png");
                fileRef.MediaType = GDMMediaType.mtManuscript;
                Assert.AreEqual("sample.png", fileRef.StringValue);
                Assert.AreEqual("png", fileRef.MultimediaFormat);
                Assert.AreEqual(GDMMediaType.mtManuscript, fileRef.MediaType);

                string title = mediaRec.GetFileTitle();
                Assert.AreEqual("File Title 2", title);

                using (GDMMultimediaRecord media2 = fContext.Tree.CreateMultimedia()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        media2.Assign(null);
                    });

                    media2.Assign(mediaRec);

                    string buf = GEDCOMProvider.GetTagStreamText(media2, 0);
                    Assert.AreEqual("0 @O3@ OBJE\r\n" +
                                    "1 FILE sample.png\r\n" +
                                    "2 FORM png\r\n" +
                                    "3 TYPE manuscript\r\n" +
                                    "2 TITL File Title 2\r\n", buf);
                }

                mediaRec.ReplaceXRefs(new GDMXRefReplacer());

                GEDCOMMultimediaLinkTest(mediaRec, indiv);

                Assert.IsFalse(mediaRec.IsEmpty());
                mediaRec.Clear();
                Assert.IsTrue(mediaRec.IsEmpty());
            }
        }

        private static void GEDCOMMultimediaLinkTest(GDMMultimediaRecord mediaRec, GDMIndividualRecord indiv)
        {
            GDMMultimediaLink mmLink = indiv.AddMultimedia(mediaRec);

            Assert.IsNotNull(mmLink.FileReferences);

            mmLink.Title = "Title1";
            Assert.AreEqual("Title1", mmLink.Title);

            var foundLink = indiv.FindMultimediaLink(mediaRec);
            Assert.AreEqual(mmLink, foundLink);

            string buf = GEDCOMProvider.GetTagStreamText(mmLink, 1);
            Assert.AreEqual("1 OBJE @O2@\r\n"+
                            "2 TITL Title1\r\n", buf);

            mmLink.ReplaceXRefs(new GDMXRefReplacer());

            Assert.IsTrue(mmLink.IsPointer, "mmLink.IsPointer");

            mmLink.IsPrimary = true;
            Assert.IsTrue(mmLink.IsPrimary, "mmLink.IsPrimary");

            Assert.IsFalse(mmLink.IsEmpty(), "mmLink.IsEmpty()"); // its pointer

            mmLink.Clear();
        }
    }
}
