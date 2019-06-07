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
using BSLib.Calendar;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMCustomEventTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            using (GDMIndividualAttribute customEvent = GDMIndividualAttribute.Create(null, "", "") as GDMIndividualAttribute) {
                Assert.IsNotNull(customEvent);

                Assert.IsNotNull(customEvent.Address);

                customEvent.Date.ParseString("28 DEC 1990");
                string dateTest = "28.12.1990";
                Assert.AreEqual(TestUtils.ParseDT(dateTest), customEvent.Date.GetDateTime());
                Assert.AreEqual(1990, customEvent.GetChronologicalYear());

                Assert.AreEqual(TestUtils.ParseDT(dateTest), customEvent.Date.Date);
                customEvent.Place.ParseString("Ivanovo");
                Assert.AreEqual("Ivanovo", customEvent.Place.StringValue);

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


                StringList strs = new StringList("test");
                customEvent.PhysicalDescription = strs;
                Assert.AreEqual(strs.Text, customEvent.PhysicalDescription.Text);

                customEvent.Address.AddEmailAddress("email");
                Assert.AreEqual("email", customEvent.Address.EmailAddresses[0].StringValue);
            }

            using (GDMIndividualEvent customEvent = GDMIndividualEvent.Create(null, "", "") as GDMIndividualEvent) {
                Assert.IsNotNull(customEvent);

                // stream test
                customEvent.SetName(GEDCOMTagType.BIRT);
                customEvent.Date.ParseString("20 SEP 1970");
                customEvent.Place.StringValue = "test place";
                customEvent.Agency = "Agency";
                customEvent.Classification = "custom";
                customEvent.ReligiousAffilation = "rel_aff";
                customEvent.Cause = "Cause";
                customEvent.Address.AddressLine1 = "adr1";
                customEvent.Restriction = GDMRestriction.rnConfidential;

                using (GDMIndividualEvent copyEvent = GDMIndividualEvent.Create(null, "", "") as GDMIndividualEvent) {
                    Assert.IsNotNull(copyEvent);
                    copyEvent.Assign(customEvent);

                    string buf1 = TestUtils.GetTagStreamText(copyEvent, 0);
                    Assert.AreEqual("0 BIRT\r\n" +
                                    "1 TYPE custom\r\n" +
                                    "1 DATE 20 SEP 1970\r\n" +
                                    "1 PLAC test place\r\n" +
                                    "1 ADDR\r\n" +
                                    "2 ADR1 adr1\r\n" +
                                    "1 CAUS Cause\r\n" +
                                    "1 AGNC Agency\r\n" +
                                    "1 RELI rel_aff\r\n" +
                                    "1 RESN confidential\r\n", buf1);
                }

                customEvent.Address.AddEmailAddress("email");
                Assert.AreEqual("email", customEvent.Address.EmailAddresses[0].StringValue);
            }

            using (GDMFamilyEvent customEvent = GDMFamilyEvent.Create(null, "", "") as GDMFamilyEvent) {
                Assert.IsNotNull(customEvent);

                customEvent.Address.AddEmailAddress("email");
                Assert.AreEqual("email", customEvent.Address.EmailAddresses[0].StringValue);
            }
        }

        [Test]
        public void Test_GEDCOMIndividualEvent()
        {
            using (GDMIndividualEvent iEvent = GDMIndividualEvent.Create(null, "", "") as GDMIndividualEvent) {
                Assert.IsNotNull(iEvent);
                Assert.IsNotNull(iEvent.Family);
            }
        }

        [Test]
        public void Test_UDN()
        {
            UDN emptyUDN = UDN.CreateEmpty();
            Assert.IsTrue(emptyUDN.IsEmpty());

            // BIRT: "28 DEC 1990"
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            UDN testUDN = iRec.GetUDN(GEDCOMTagType.BIRT);
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            testUDN = GDMDate.GetUDNByFormattedStr("28/12/1990", GDMCalendar.dcGregorian);
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            using (GDMDateValue dateVal = new GDMDateValue(null, "", "")) {
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
