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
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMHeaderTests
    {
        [Test]
        public void Test_Common()
        {
            GDMTree tree = new GDMTree();
            GDMHeader headRec = tree.Header;

            headRec.Notes = new StringList("This notes test");
            Assert.AreEqual("This notes test", headRec.Notes[0]);

            headRec.CharacterSet = GEDCOMCharacterSet.csASCII;
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, headRec.CharacterSet);

            headRec.CharacterSetVersion = "1x";
            Assert.AreEqual("1x", headRec.CharacterSetVersion);

            headRec.Copyright = "copyright";
            Assert.AreEqual("copyright", headRec.Copyright);

            headRec.Source = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.Source);

            headRec.ReceivingSystemName = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.ReceivingSystemName);

            headRec.Language.Value = GDMLanguageID.Russian;
            Assert.AreEqual("Russian", headRec.Language.StringValue);

            headRec.GEDCOMVersion = "5.5";
            Assert.AreEqual("5.5", headRec.GEDCOMVersion);

            headRec.GEDCOMForm = "LINEAGE-LINKED";
            Assert.AreEqual("LINEAGE-LINKED", headRec.GEDCOMForm);

            headRec.FileName = "testfile.ged";
            Assert.AreEqual("testfile.ged", headRec.FileName);

            DateTime dtx = DateTime.Now;
            dtx = dtx.AddTicks(-dtx.Ticks % 10000000);
            headRec.TransmissionDateTime = dtx;
            Assert.AreEqual(dtx, headRec.TransmissionDateTime);

            headRec.FileRevision = 113;
            Assert.AreEqual(113, headRec.FileRevision);

            headRec.PlaceHierarchy = "test11";
            Assert.AreEqual("test11", headRec.PlaceHierarchy);

            Assert.IsNotNull(headRec.SourceBusinessAddress);

            headRec.SourceBusinessName = "test23";
            Assert.AreEqual("test23", headRec.SourceBusinessName);

            headRec.SourceProductName = "test33";
            Assert.AreEqual("test33", headRec.SourceProductName);

            headRec.SourceVersion = "test44";
            Assert.AreEqual("test44", headRec.SourceVersion);

            Assert.IsNotNull(headRec.Submission);

            Assert.IsFalse(headRec.IsEmpty());
            headRec.Clear();
            Assert.IsTrue(headRec.IsEmpty());
        }

        [Test]
        public void Test_GDMLanguage()
        {
            using (GDMLanguage langTag = GDMLanguage.Create(null, "", "") as GDMLanguage) {
                Assert.IsTrue(langTag.IsEmpty());

                langTag.Value = GDMLanguageID.AngloSaxon;
                Assert.AreEqual(GDMLanguageID.AngloSaxon, langTag.Value);

                langTag.ParseString("Spanish");
                Assert.AreEqual("Spanish", langTag.StringValue);

                using (GDMLanguage langTag2 = GDMLanguage.Create(null, "", "") as GDMLanguage) {
                    Assert.IsTrue(langTag2.IsEmpty());

                    Assert.Throws(typeof(ArgumentException), () => { langTag2.Assign(null); });

                    langTag2.Assign(langTag);
                    Assert.AreEqual("Spanish", langTag2.StringValue);
                }

                langTag.Clear();
                Assert.IsTrue(langTag.IsEmpty());
            }
        }
    }
}
