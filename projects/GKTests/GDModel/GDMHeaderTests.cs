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

            headRec.Note.Lines.Text = "This notes test";
            Assert.AreEqual("This notes test", headRec.Note.Lines.Text);

            headRec.CharacterSet.Value = GEDCOMCharacterSet.csASCII;
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, headRec.CharacterSet.Value);

            headRec.CharacterSet.Version = "1x";
            Assert.AreEqual("1x", headRec.CharacterSet.Version);

            headRec.Copyright = "copyright";
            Assert.AreEqual("copyright", headRec.Copyright);

            headRec.Source.StringValue = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.Source.StringValue);

            headRec.ReceivingSystemName = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.ReceivingSystemName);

            headRec.Language = GDMLanguageID.Russian;
            Assert.AreEqual(GDMLanguageID.Russian, headRec.Language);

            headRec.GEDCOM.Version = "5.5";
            Assert.AreEqual("5.5", headRec.GEDCOM.Version);

            headRec.GEDCOM.Form = "LINEAGE-LINKED";
            Assert.AreEqual("LINEAGE-LINKED", headRec.GEDCOM.Form);

            headRec.File.StringValue = "testfile.ged";
            Assert.AreEqual("testfile.ged", headRec.File.StringValue);

            DateTime dtx = DateTime.Now;
            dtx = dtx.AddTicks(-dtx.Ticks % 10000000);
            headRec.TransmissionDateTime = dtx;
            Assert.AreEqual(dtx, headRec.TransmissionDateTime);

            headRec.File.Revision = 113;
            Assert.AreEqual(113, headRec.File.Revision);

            headRec.Place.Form = "test11";
            Assert.AreEqual("test11", headRec.Place.Form);

            /*Assert.IsNotNull(headRec.Source.BusinessAddress);

            headRec.Source.BusinessName = "test23";
            Assert.AreEqual("test23", headRec.Source.BusinessName);*/

            headRec.Source.ProductName = "test33";
            Assert.AreEqual("test33", headRec.Source.ProductName);

            headRec.Source.Version = "test44";
            Assert.AreEqual("test44", headRec.Source.Version);

            Assert.IsNotNull(headRec.Submission);

            Assert.IsFalse(headRec.IsEmpty());
            headRec.Clear();
            Assert.IsTrue(headRec.IsEmpty());
        }

        [Test]
        public void Test_GDMLanguage()
        {
            using (GDMLanguage langTag = new GDMLanguage(null)) {
                Assert.IsTrue(langTag.IsEmpty());

                langTag.Value = GDMLanguageID.AngloSaxon;
                Assert.AreEqual(GDMLanguageID.AngloSaxon, langTag.Value);

                langTag.ParseString("Spanish");
                Assert.AreEqual("Spanish", langTag.StringValue);

                using (GDMLanguage langTag2 = new GDMLanguage(null)) {
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
