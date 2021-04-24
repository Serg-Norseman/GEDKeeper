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
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMAddressTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMAddress addr = new GDMAddress()) {
                Assert.IsNotNull(addr, "addr != null");

                addr.SetAddressText("test");
                Assert.AreEqual("test", addr.Lines.Text.Trim());

                addr.Lines.Text = "This\r\naddress\r\ntest";
                Assert.AreEqual("This\r\naddress\r\ntest", addr.Lines.Text.Trim());
                Assert.AreEqual("This", addr.Lines[0]);
                Assert.AreEqual("address", addr.Lines[1]);
                Assert.AreEqual("test", addr.Lines[2]);

                addr.AddPhoneNumber("8 911 101 99 99");
                Assert.AreEqual("8 911 101 99 99", addr.PhoneNumbers[0].StringValue);

                addr.AddEmailAddress("test@mail.com");
                Assert.AreEqual("test@mail.com", addr.EmailAddresses[0].StringValue);

                addr.AddFaxNumber("abrakadabra");
                Assert.AreEqual("abrakadabra", addr.FaxNumbers[0].StringValue);

                addr.AddWebPage("http://test.com");
                Assert.AreEqual("http://test.com", addr.WebPages[0].StringValue);

                addr.AddPhoneNumber("8 911 101 33 33");
                Assert.AreEqual("8 911 101 33 33", addr.PhoneNumbers[1].StringValue);

                addr.AddEmailAddress("test@mail.ru");
                Assert.AreEqual("test@mail.ru", addr.EmailAddresses[1].StringValue);

                addr.AddFaxNumber("abrakadabra");
                Assert.AreEqual("abrakadabra", addr.FaxNumbers[1].StringValue);

                addr.AddWebPage("http://test.ru");
                Assert.AreEqual("http://test.ru", addr.WebPages[1].StringValue);

                //

                addr.AddressLine1 = "test1";
                Assert.AreEqual("test1", addr.AddressLine1);

                addr.AddressLine2 = "test2";
                Assert.AreEqual("test2", addr.AddressLine2);

                addr.AddressLine3 = "test3";
                Assert.AreEqual("test3", addr.AddressLine3);

                addr.AddressCity = "test4";
                Assert.AreEqual("test4", addr.AddressCity);

                addr.AddressState = "test5";
                Assert.AreEqual("test5", addr.AddressState);

                addr.AddressCountry = "test6";
                Assert.AreEqual("test6", addr.AddressCountry);

                addr.AddressPostalCode = "test7";
                Assert.AreEqual("test7", addr.AddressPostalCode);

                using (GDMAddress addr2 = new GDMAddress()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        addr2.Assign(null);
                    });

                    addr2.Assign(addr);

                    var iRec = new GDMIndividualRecord(null);
                    var evt = new GDMIndividualEvent();
                    evt.SetName("BIRT");
                    iRec.Events.Add(evt);
                    evt.Address.Assign(addr);

                    string buf = TestUtils.GetTagStreamText(iRec, 0);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 SEX U\r\n" +
                                    "1 BIRT\r\n" +
                                    "2 ADDR This\r\n" +
                                    "3 CONT address\r\n" +
                                    "3 CONT test\r\n" +
                                    "3 ADR1 test1\r\n" +
                                    "3 ADR2 test2\r\n" +
                                    "3 ADR3 test3\r\n" +
                                    "3 CITY test4\r\n" +
                                    "3 STAE test5\r\n" +
                                    "3 CTRY test6\r\n" +
                                    "3 POST test7\r\n" +
                                    "2 PHON 8 911 101 99 99\r\n" +
                                    "2 PHON 8 911 101 33 33\r\n" +
                                    "2 EMAIL test@mail.com\r\n" +
                                    "2 EMAIL test@mail.ru\r\n" +
                                    "2 FAX abrakadabra\r\n" +
                                    "2 FAX abrakadabra\r\n" +
                                    "2 WWW http://test.com\r\n" +
                                    "2 WWW http://test.ru\r\n", buf);

                    Assert.AreEqual("This\r\naddress\r\ntest", addr2.Lines.Text.Trim());
                    Assert.AreEqual("8 911 101 99 99", addr2.PhoneNumbers[0].StringValue);
                    Assert.AreEqual("test@mail.com", addr2.EmailAddresses[0].StringValue);
                    Assert.AreEqual("abrakadabra", addr2.FaxNumbers[0].StringValue);
                    Assert.AreEqual("http://test.com", addr2.WebPages[0].StringValue);
                    Assert.AreEqual("8 911 101 33 33", addr2.PhoneNumbers[1].StringValue);
                    Assert.AreEqual("test@mail.ru", addr2.EmailAddresses[1].StringValue);
                    Assert.AreEqual("abrakadabra", addr2.FaxNumbers[1].StringValue);
                    Assert.AreEqual("http://test.ru", addr2.WebPages[1].StringValue);
                    Assert.AreEqual("test1", addr2.AddressLine1);
                    Assert.AreEqual("test2", addr2.AddressLine2);
                    Assert.AreEqual("test3", addr2.AddressLine3);
                    Assert.AreEqual("test4", addr2.AddressCity);
                    Assert.AreEqual("test5", addr2.AddressState);
                    Assert.AreEqual("test6", addr2.AddressCountry);
                    Assert.AreEqual("test7", addr2.AddressPostalCode);
                }

                addr.SetAddressArray(new string[] { "test11", "test21", "test31" });
                Assert.AreEqual("test11", addr.Lines[0]);
                Assert.AreEqual("test21", addr.Lines[1]);
                Assert.AreEqual("test31", addr.Lines[2]);

                Assert.IsFalse(addr.IsEmpty());
                addr.Clear();
                Assert.IsTrue(addr.IsEmpty());
            }
        }

        [Test]
        public void Test_Create()
        {
            GDMAddress instance = new GDMAddress();
            Assert.IsNotNull(instance);
        }

        [Test]
        public void Test_SetAddress()
        {
            var vals = new string[] {
                "Address Line 1",
                "Address Line 2"
            };
            GDMLines value = new GDMLines(vals);
            GDMAddress instance = new GDMAddress();
            instance.Lines.AddRange(value);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_SetAddressLine1()
        {
            string value = "1234 Main St";
            GDMAddress instance = new GDMAddress();
            instance.AddressLine1 = value;
            Assert.AreEqual(value, instance.AddressLine1);
        }

        [Test]
        public void Test_SetAddressLine2()
        {
            string value = "Novozavodskaya ul., 10";
            GDMAddress instance = new GDMAddress();
            instance.AddressLine2 = value;
            Assert.AreEqual(value, instance.AddressLine2);
        }

        [Test]
        public void Test_SetAddressLine3()
        {
            string value = "ALEKSCEVSKTY r-n";
            GDMAddress instance = new GDMAddress();
            instance.AddressLine3 = value;
            Assert.AreEqual(value, instance.AddressLine3);
        }

        [Test]
        public void Test_SetAddressCity()
        {
            string value = "Moskva";
            GDMAddress instance = new GDMAddress();
            instance.AddressCity = value;
            Assert.AreEqual(value, instance.AddressCity);
        }

        [Test]
        public void Test_SetAddressState()
        {
            string value = "VORONEJSKAYA obl";
            GDMAddress instance = new GDMAddress();
            instance.AddressState = value;
            Assert.AreEqual(value, instance.AddressState);
        }

        [Test]
        public void Test_SetAddressPostalCode()
        {
            string value = "1234A567";
            GDMAddress instance = new GDMAddress();
            instance.AddressPostalCode = value;
            Assert.AreEqual(value, instance.AddressPostalCode);
        }

        [Test]
        public void Test_SetAddressCountry()
        {
            string value = "Moosylvania";
            GDMAddress instance = new GDMAddress();
            instance.AddressCountry = value;
            Assert.AreEqual(value, instance.AddressCountry);
        }

        [Test]
        public void Test_AddEmailAddress()
        {
            string value = "serg.zhdanovskih@yandex.ru";
            GDMAddress instance = new GDMAddress();
            instance.AddEmailAddress(value);
            GDMList<GDMTag> pl = instance.EmailAddresses;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl.Extract(0).StringValue);
        }

        [Test]
        public void Test_AddFaxNumber()
        {
            string value = "(214) 748-3647";
            GDMAddress instance = new GDMAddress();
            instance.AddFaxNumber(value);
            GDMList<GDMTag> pl = instance.FaxNumbers;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl.Extract(0).StringValue);
        }

        [Test]
        public void Test_AddPhoneNumber()
        {
            string value = "(214) 748-3647";
            GDMAddress instance = new GDMAddress();
            instance.AddPhoneNumber(value);
            GDMList<GDMTag> pl = instance.PhoneNumbers;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl[0].StringValue);
        }

        [Test]
        public void Test_AddPhoneNumber2()
        {
            string value1 = "(214) 748-3647";
            string value2 = "(999) 748-3647";
            GDMAddress instance = new GDMAddress();
            instance.AddPhoneNumber(value1);
            instance.AddPhoneNumber(value2);
            GDMList<GDMTag> pl = instance.PhoneNumbers;
            Assert.AreEqual(2, pl.Count);
            string res = pl[0].StringValue;
            Assert.AreEqual(res, value1);
            res = pl[1].StringValue;
            Assert.AreEqual(res, value2);
        }
        
        [Test]
        public void Test_AddWebPage1()
        {
            string value = "http://www.bitboost.com/ref/international-address-formats/russia/";
            GDMAddress instance = new GDMAddress();
            instance.AddWebPage(value);
            GDMList<GDMTag> wp = instance.WebPages;
            Assert.AreEqual(1, wp.Count);
            string res = wp.Extract(0).StringValue;
            Assert.AreEqual(res, value);
        }

        [Test]
        public void Test_AddWebPage2()
        {
            string value1 = "http://www.bitboost.com/ref/international-address-formats/russia/";
            string value2 = "http://google.com/search";
            GDMAddress instance = new GDMAddress();
            instance.AddWebPage(value1);
            instance.AddWebPage(value2);
            GDMList<GDMTag> wp = instance.WebPages;
            Assert.AreEqual(2, wp.Count);
            string res1 = wp[0].StringValue;
            Assert.AreEqual(res1, value1);
            string res2 = wp[1].StringValue;
            Assert.AreEqual(res2, value2);
        }

        [Test]
        public void Test_Assign()
        {
            GDMTag source = null;
            GDMAddress instance = new GDMAddress();

            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });

            source = new GDMAddress();
            instance.Assign(source);
        }

        [Test]
        public void Test_Clear()
        {
            GDMAddress instance = new GDMAddress();
            instance.SetAddressText("blah");
            instance.Clear();
            Assert.IsTrue(instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyT()
        {
            GDMAddress instance = new GDMAddress();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GDMAddress instance = new GDMAddress();
            instance.SetAddressText("blah");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_SetAddressText()
        {
            string value = "this is a test";
            GDMAddress instance = new GDMAddress();
            instance.SetAddressText(value);
            StringList val2 = new StringList(value);
            Assert.AreEqual(val2.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_SetAddressArray()
        {
            // http://www.bitboost.com/ref/international-address-formats/russia/
            string[] value = new string[] {
                "ul. Lesnaya d. 5",
                "pos. Lesnoe",
                "ALEKSCEVSKTY r-n",
                "VORONEJSKAYA obl",
                "247112",
                "RUSSIAN FEDERATION"
            };
            GDMAddress instance = new GDMAddress();
            instance.SetAddressArray(value);
            Assert.AreEqual(new StringList(value).Text, instance.Lines.Text);
        }

        /// <summary>
        /// Support function. Parse GEDCOM string, returns ADDR object found.
        /// </summary>
        private static GDMAddress AddrParse(string text)
        {
            GDMIndividualRecord rec2 = TestUtils.ParseIndiRec(text);

            GDMList<GDMCustomEvent> events = rec2.Events;
            Assert.AreEqual(1, events.Count);
            GDMCustomEvent evt = events[0];
            return evt.Address;
        }

        [Test]
        public void Test_Address1Fail()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n3 ADR1 Novozavodskaya ul., 10\n3 CITY Moskva\n3 CTRY Russia\n3 POST 121309";
            GDMAddress res = AddrParse(text);
            Assert.AreEqual("Novozavodskaya ul., 10", res.AddressLine1);
        }

        [Test]
        public void Test_AddressParse2()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n3 CONT Novozavodskaya ul., 10\n3 CITY Moskva\n3 CTRY Russia\n3 POST 121309";
            GDMAddress res = AddrParse(text);
            Assert.AreEqual("Institute for Higher\r\nLearning\r\nNovozavodskaya ul., 10", res.Lines.Text);
        }

        [Test]
        public void Test_AddressParse()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n3 ADR1 Novozavodskaya ul., 10\n3 CITY Moskva\n3 CTRY Russia\n3 POST 121309";
            GDMAddress res = AddrParse(text);
            Assert.AreEqual("Moskva", res.AddressCity);
            Assert.AreEqual("Russia", res.AddressCountry);
            Assert.AreEqual("121309", res.AddressPostalCode);
            Assert.AreEqual("", res.AddressState);
        }

        [Test]
        public void Test_PhoneParse()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 PHON +7 499 277-71-00\n2 ADDR Institute for Higher\n3 CONT Learning\n2 PHON +7 495 967-77-76\n";
            GDMAddress res = AddrParse(text);
            Assert.AreEqual(2, res.PhoneNumbers.Count);
            Assert.AreEqual("Institute for Higher\r\nLearning", res.Lines.Text);
        }
    }
}
