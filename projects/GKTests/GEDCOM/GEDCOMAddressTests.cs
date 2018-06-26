/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    /**
     *
     * @author Kevin Routley
     */
    [TestFixture]
    public class GEDCOMAddressTests
    {
        [Test]
        public void Test_SetAddress()
        {
            var vals = new string[] {
                "Address Line 1",
                "Address Line 2"
            };

            StringList value = new StringList(vals);
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.Address = value;
            
            StringList result = instance.Address;
            Assert.AreEqual(value.Text, result.Text);
        }

        [Test]
        public void Test_SetAddressLine1()
        {
            string value = "1234 Main St";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressLine1 = value;
            string result = instance.AddressLine1;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_SetAddressLine2()
        {
            string value = "Novozavodskaya ul., 10";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressLine2 = value;
            string result = instance.AddressLine2;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_SetAddressLine3()
        {
            string value = "ALEKSCEVSKTY r-n";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressLine3 = value;
            string result = instance.AddressLine3;
            Assert.AreEqual(result, value);
        }

        [Test]
        public void Test_SetAddressCity()
        {
            string value = "Moskva";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressCity = value;
            string result = instance.AddressCity;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_SetAddressState()
        {
            string value = "VORONEJSKAYA obl";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressState = value;
            string result = instance.AddressState;
            Assert.AreEqual(result, value);
        }

        [Test]
        public void Test_SetAddressPostalCode()
        {
            string value = "1234A567";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressPostalCode = value;
            string result = instance.AddressPostalCode;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_SetAddressCountry()
        {
            string value = "Moosylvania";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddressCountry = value;
            string result = instance.AddressCountry;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_AddEmailAddress()
        {
            string value = "serg.zhdanovskih@yandex.ru";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddEmailAddress(value);
            GEDCOMList<GEDCOMTag> pl = instance.EmailAddresses;
            Assert.AreEqual(1, pl.Count);
            string res = pl.Extract(0).StringValue;
            Assert.AreEqual(res, value);
        }

        [Test]
        public void Test_AddFaxNumber()
        {
            string value = "(214) 748-3647";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddFaxNumber(value);
            GEDCOMList<GEDCOMTag> pl = instance.FaxNumbers;
            Assert.AreEqual(1, pl.Count);
            string res = pl.Extract(0).StringValue;
            Assert.AreEqual(res, value);
        }

        [Test]
        public void Test_AddPhoneNumber()
        {
            string value = "(214) 748-3647";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddPhoneNumber(value);
            GEDCOMList<GEDCOMTag> pl = instance.PhoneNumbers;
            Assert.AreEqual(1, pl.Count);
            string res = pl[0].StringValue;
            Assert.AreEqual(res, value);
        }

        [Test]
        public void Test_AddPhoneNumber2()
        {
            string value1 = "(214) 748-3647";
            string value2 = "(999) 748-3647";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddPhoneNumber(value1);
            instance.AddPhoneNumber(value2);
            GEDCOMList<GEDCOMTag> pl = instance.PhoneNumbers;
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
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddWebPage(value);
            GEDCOMList<GEDCOMTag> wp = instance.WebPages;
            Assert.AreEqual(1, wp.Count);
            string res = wp.Extract(0).StringValue;
            Assert.AreEqual(res, value);
        }

        [Test]
        public void Test_AddWebPage2()
        {
            string value1 = "http://www.bitboost.com/ref/international-address-formats/russia/";
            string value2 = "http://google.com/search";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.AddWebPage(value1);
            instance.AddWebPage(value2);
            GEDCOMList<GEDCOMTag> wp = instance.WebPages;
            Assert.AreEqual(2, wp.Count);
            string res1 = wp[0].StringValue;
            Assert.AreEqual(res1, value1);
            string res2 = wp[1].StringValue;
            Assert.AreEqual(res2, value2);
        }

        // Agreed as not needing explicit testing
        //    [Test]
        //    public void testCreateObj() {
        //        System.out.println("createObj");
        //        GEDCOMTree owner = null;
        //        GEDCOMObject parent = null;
        //        GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
        //        instance.createObj(owner, parent);
        //        fail("The test case is a prototype.");
        //    }

        [Test]
        public void Test_SaveTagsToStream()
        {
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");

            StreamWriter stream = null;
            Assert.Throws(typeof(NullReferenceException), () => {
                instance.SaveToStream(stream);
            });
        }

        // Agreed as not needing explicit testing
        //    [Test]
        //    public void testDispose() {
        //        System.out.println("dispose");
        //        bool disposing = false;
        //        GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
        //        instance.dispose(disposing);
        //        fail("The test case is a prototype.");
        //    }

        [Test]
        public void Test_Assign()
        {
            GEDCOMTag source = null;
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");

            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });

            source = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.Assign(source);
        }

        [Test]
        public void Test_AddTag()
        {
            const string tagName = "BABA";
            const string tagValue = "YAGA";
            TagConstructor tagConstructor = null;
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            GEDCOMTag result = instance.AddTag(tagName, tagValue, tagConstructor);
            Assert.IsNotNull(instance.FindTag(tagName, 0));
        }

        [Test]
        public void Test_Clear()
        {
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.SetAddressText("blah");
            instance.Clear();
            Assert.IsTrue(instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyT()
        {
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            bool expResult = true;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.SetAddressText("blah");
            bool expResult = false;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }
        
        [Test]
        public void Test_ResetOwner()
        {
            GEDCOMTree newOwner = new GEDCOMTree();
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");

            instance.ResetOwner(newOwner);
            Assert.AreEqual(newOwner, instance.Owner);
        }

        // Agreed as not needing explicit testing
        //    [Test]
        //    public void testCreate() {
        //        System.out.println("create");
        //        GEDCOMTree owner = null;
        //        GEDCOMObject parent = null;
        //        string tagName = "";
        //        string tagValue = "";
        //        GEDCOMTag expResult = null;
        //        GEDCOMTag result = GEDCOMAddress.Create(owner, parent, tagName, tagValue);
        //        Assert.AreEqual(expResult, result);
        //        fail("The test case is a prototype.");
        //    }
        
        [Test]
        public void Test_SetAddressText()
        {
            string value = "this is a test";
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.SetAddressText(value);
            string res = instance.Address.Text;
            StringList val2 = new StringList(value);
            Assert.AreEqual(val2.Text, res);
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
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
            instance.SetAddressArray(value);
            string val = instance.Address.Text;
            Assert.AreEqual(new StringList(value).Text, val);
        }

        // Support function. Parse GEDCOM string, returns ADDR object found
        private GEDCOMAddress AddrParse(string text)
        {
            // TODO should go into general utility class
            GEDCOMTree tee = new GEDCOMTree();
            GEDCOMProvider gp = new GEDCOMProvider(tee);
            try {
                gp.LoadFromString(text);
            } catch (Exception) {
            }
            Assert.AreEqual(1, tee.RecordsCount);
            GEDCOMRecord rec = tee[0];
            Assert.IsTrue(rec is GEDCOMIndividualRecord);
            GEDCOMIndividualRecord rec2 = (GEDCOMIndividualRecord)rec;
            // end for utility class
            
            GEDCOMList<GEDCOMCustomEvent> events = rec2.Events;
            Assert.AreEqual(1, events.Count);
            GEDCOMCustomEvent evt = events[0];
            return evt.Address;
        }

        [Test]
        public void Test_Address1Fail()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n3 ADR1 Novozavodskaya ul., 10\n3 CITY Moskva\n3 CTRY Russia\n3 POST 121309";
            GEDCOMAddress res = AddrParse(text);

            Assert.AreEqual("Novozavodskaya ul., 10", res.AddressLine1);
        }

        [Test]
        public void Test_AddressParse2()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n3 CONT Novozavodskaya ul., 10\n3 CITY Moskva\n3 CTRY Russia\n3 POST 121309";
            GEDCOMAddress res = AddrParse(text);

            string val = res.Address.Text;
            Assert.AreEqual("Institute for Higher\r\nLearning\r\nNovozavodskaya ul., 10", val);
        }

        [Test]
        public void Test_AddressParse()
        {
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n3 ADR1 Novozavodskaya ul., 10\n3 CITY Moskva\n3 CTRY Russia\n3 POST 121309";
            GEDCOMAddress res = AddrParse(text);
            Assert.AreEqual("Moskva", res.AddressCity);
            Assert.AreEqual("Russia", res.AddressCountry);
            Assert.AreEqual("121309", res.AddressPostalCode);
            Assert.AreEqual("", res.AddressState);
        }

        [Test]
        public void Test_PhoneParse()
        {
            // TODO The standard should be the second version of the input line, but in the future we can work out the first
            //string text = "0 @I1@ INDI\n1 FACT\n2 PHON +7 499 277-71-00\n2 ADDR Institute for Higher\n3 CONT Learning\n2 PHON +7 495 967-77-76\n";
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n2 PHON +7 499 277-71-00\n2 PHON +7 495 967-77-76\n";
            GEDCOMAddress res = AddrParse(text);
            Assert.AreEqual(2, res.PhoneNumbers.Count);

            Assert.AreEqual("Institute for Higher\r\nLearning", res.Address.Text);
        }
    }
}
