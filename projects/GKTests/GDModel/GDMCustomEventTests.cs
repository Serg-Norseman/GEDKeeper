/*
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
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Calendar;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMCustomEventTests
    {
        private BaseContext fContext;

        public GDMCustomEventTests()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            using (GDMIndividualAttribute customEvent = new GDMIndividualAttribute()) {
                Assert.IsNotNull(customEvent);

                customEvent.Date.ParseString("28 DEC 1990");
                string dateTest = "28.12.1990";
                Assert.AreEqual(TestUtils.ParseDT(dateTest), customEvent.Date.GetDateTime());
                Assert.AreEqual(1990, customEvent.GetChronologicalYear());

                Assert.AreEqual(TestUtils.ParseDT(dateTest), customEvent.Date.Date);

                Assert.IsFalse(customEvent.HasPlace);
                customEvent.Place.ParseString("Ivanovo");
                Assert.AreEqual("Ivanovo", customEvent.Place.StringValue);

                Assert.IsTrue(customEvent.HasPlace);
                Assert.IsNotNull(customEvent.Place);

                customEvent.Agency = "test agency";
                Assert.AreEqual("test agency", customEvent.Agency);

                customEvent.Classification = "test type";
                Assert.AreEqual("test type", customEvent.Classification);

                customEvent.Cause = "test cause";
                Assert.AreEqual("test cause", customEvent.Cause);

                customEvent.ReligiousAffilation = "test aff";
                Assert.AreEqual("test aff", customEvent.ReligiousAffilation);

                customEvent.Restriction = GDMRestriction.rnLocked;
                Assert.AreEqual(GDMRestriction.rnLocked, customEvent.Restriction);

                GDMLines strs = new GDMLines("test");
                customEvent.PhysicalDescription = strs;
                Assert.AreEqual(strs.Text, customEvent.PhysicalDescription.Text);

                Assert.IsFalse(customEvent.HasAddress);
                var addr = customEvent.Address;
                Assert.IsNotNull(addr);
                addr.AddEmailAddress("email");
                Assert.AreEqual("email", addr.EmailAddresses[0].StringValue);
                Assert.IsTrue(customEvent.HasAddress);
            }

            using (GDMIndividualEvent customEvent = new GDMIndividualEvent()) {
                Assert.IsNotNull(customEvent);

                // stream test
                customEvent.SetName(GEDCOMTagName.BIRT);
                customEvent.Date.ParseString("20 SEP 1970");
                customEvent.Place.StringValue = "test place";
                customEvent.Agency = "Agency";
                customEvent.Classification = "custom";
                customEvent.ReligiousAffilation = "rel_aff";
                customEvent.Cause = "Cause";
                customEvent.Address.AddressLine1 = "adr1";
                customEvent.Restriction = GDMRestriction.rnConfidential;

                var note = new GDMNotes();
                note.Lines.Text = "event notes";
                customEvent.Notes.Add(note);

                var sourCit = new GDMSourceCitation();
                sourCit.Description.Text = "event sour desc";
                customEvent.SourceCitations.Add(sourCit);

                var mmLink = new GDMMultimediaLink();
                mmLink.Title = "event media title";
                customEvent.MultimediaLinks.Add(mmLink);

                using (GDMIndividualEvent copyEvent = new GDMIndividualEvent()) {
                    Assert.IsNotNull(copyEvent);
                    copyEvent.Assign(customEvent);

                    var iRec = new GDMIndividualRecord(null);
                    iRec.Events.Add(copyEvent);
                    string buf1 = GEDCOMProvider.GetTagStreamText(iRec, 0);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 SEX U\r\n" +
                                    "1 BIRT\r\n" +
                                    "2 TYPE custom\r\n" +
                                    "2 DATE 20 SEP 1970\r\n" +
                                    "2 PLAC test place\r\n" +
                                    "2 ADDR\r\n" +
                                    "3 ADR1 adr1\r\n" +
                                    "2 CAUS Cause\r\n" +
                                    "2 AGNC Agency\r\n" +
                                    "2 RELI rel_aff\r\n" +
                                    "2 RESN confidential\r\n" +
                                    "2 NOTE event notes\r\n"+
                                    "2 SOUR event sour desc\r\n"+
                                    "2 OBJE\r\n"+
                                    "3 TITL event media title\r\n", buf1);
                }

                var addr = customEvent.Address;
                addr.AddEmailAddress("email");
                Assert.AreEqual("email", addr.EmailAddresses[0].StringValue);
            }

            using (GDMFamilyEvent customEvent = new GDMFamilyEvent()) {
                Assert.IsNotNull(customEvent);

                var addr = customEvent.Address;
                addr.AddEmailAddress("email");
                Assert.AreEqual("email", addr.EmailAddresses[0].StringValue);
            }
        }

        [Test]
        public void Test_Assign()
        {
            var instance = new GDMIndividualEvent();
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(null);
            });
        }

        [Test]
        public void Test_Clear()
        {
            var instance = new GDMIndividualEvent();
            instance.Clear();
        }

        [Test]
        public void Test_IsEmpty()
        {
            var instance = new GDMIndividualEvent();
            Assert.IsTrue(instance.IsEmpty());
        }

        [Test]
        public void Test_ReplaceXRefs()
        {
            var instance = new GDMIndividualEvent();
            instance.ReplaceXRefs(null);
        }

        [Test]
        public void Test_GDMIndividualEvent()
        {
            using (GDMIndividualEvent iEvent = new GDMIndividualEvent()) {
                Assert.IsNotNull(iEvent);
            }
        }

        [Test]
        public void Test_UDN()
        {
            UDN emptyUDN = UDN.Empty;
            Assert.IsTrue(emptyUDN.IsEmpty());

            // BIRT: "28 DEC 1990"
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            UDN testUDN = iRec.GetUDN(GEDCOMTagName.BIRT);
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            testUDN = iRec.GetUDN(GEDCOMTagType.BIRT);
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            testUDN = GDMDate.GetUDNByFormattedStr("28/12/1990", GDMCalendar.dcGregorian);
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            using (GDMDateValue dateVal = new GDMDateValue()) {
                dateVal.ParseString("28 DEC 1990");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("1990/12/28", testUDN.ToString());

                dateVal.ParseString("ABT 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/01/20", testUDN.ToString());

                dateVal.ParseString("CAL 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/01/20", testUDN.ToString());

                dateVal.ParseString("EST 20 DEC 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/12/20", testUDN.ToString());

                dateVal.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("2013/01/14", testUDN.ToString());

                dateVal.ParseString("BEF 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("<2013/01/20", testUDN.ToString());

                dateVal.ParseString("AFT 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual(">2013/01/20", testUDN.ToString());
            }
        }
    }
}
