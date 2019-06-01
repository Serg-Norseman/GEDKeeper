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
using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    /**
     *
     * @author Kevin Routley
     */
    [TestFixture]
    public class GDMAddressTests
    {
        [Test]
        public void Test_Create()
        {
            GDMAddress instance = (GDMAddress)GDMAddress.Create(null, "", "");
            Assert.IsNotNull(instance);
        }

        [Test]
        public void Test_SetAddress()
        {
            var vals = new string[] {
                "Address Line 1",
                "Address Line 2"
            };
            StringList value = new StringList(vals);
            GDMAddress instance = new GDMAddress(null);
            instance.Address = value;
            Assert.AreEqual(value.Text, instance.Address.Text);
        }

        [Test]
        public void Test_SetAddressLine1()
        {
            string value = "1234 Main St";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressLine1 = value;
            Assert.AreEqual(value, instance.AddressLine1);
        }

        [Test]
        public void Test_SetAddressLine2()
        {
            string value = "Novozavodskaya ul., 10";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressLine2 = value;
            Assert.AreEqual(value, instance.AddressLine2);
        }

        [Test]
        public void Test_SetAddressLine3()
        {
            string value = "ALEKSCEVSKTY r-n";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressLine3 = value;
            Assert.AreEqual(value, instance.AddressLine3);
        }

        [Test]
        public void Test_SetAddressCity()
        {
            string value = "Moskva";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressCity = value;
            Assert.AreEqual(value, instance.AddressCity);
        }

        [Test]
        public void Test_SetAddressState()
        {
            string value = "VORONEJSKAYA obl";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressState = value;
            Assert.AreEqual(value, instance.AddressState);
        }

        [Test]
        public void Test_SetAddressPostalCode()
        {
            string value = "1234A567";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressPostalCode = value;
            Assert.AreEqual(value, instance.AddressPostalCode);
        }

        [Test]
        public void Test_SetAddressCountry()
        {
            string value = "Moosylvania";
            GDMAddress instance = new GDMAddress(null);
            instance.AddressCountry = value;
            Assert.AreEqual(value, instance.AddressCountry);
        }

        [Test]
        public void Test_AddEmailAddress()
        {
            string value = "serg.zhdanovskih@yandex.ru";
            GDMAddress instance = new GDMAddress(null);
            instance.AddEmailAddress(value);
            GDMList<GDMTag> pl = instance.EmailAddresses;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl.Extract(0).StringValue);
        }

        [Test]
        public void Test_AddFaxNumber()
        {
            string value = "(214) 748-3647";
            GDMAddress instance = new GDMAddress(null);
            instance.AddFaxNumber(value);
            GDMList<GDMTag> pl = instance.FaxNumbers;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl.Extract(0).StringValue);
        }

        [Test]
        public void Test_AddPhoneNumber()
        {
            string value = "(214) 748-3647";
            GDMAddress instance = new GDMAddress(null);
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
            GDMAddress instance = new GDMAddress(null);
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
            GDMAddress instance = new GDMAddress(null);
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
            GDMAddress instance = new GDMAddress(null);
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
            GDMAddress instance = new GDMAddress(null);

            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });

            source = new GDMAddress(null);
            instance.Assign(source);
        }

        [Test]
        public void Test_Clear()
        {
            GDMAddress instance = new GDMAddress(null);
            instance.SetAddressText("blah");
            instance.Clear();
            Assert.IsTrue(instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyT()
        {
            GDMAddress instance = new GDMAddress(null);
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GDMAddress instance = new GDMAddress(null);
            instance.SetAddressText("blah");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_SetAddressText()
        {
            string value = "this is a test";
            GDMAddress instance = new GDMAddress(null);
            instance.SetAddressText(value);
            StringList val2 = new StringList(value);
            Assert.AreEqual(val2.Text, instance.Address.Text);
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
            GDMAddress instance = new GDMAddress(null);
            instance.SetAddressArray(value);
            Assert.AreEqual(new StringList(value).Text, instance.Address.Text);
        }

        // Support function. Parse GEDCOM string, returns ADDR object found
        private GDMAddress AddrParse(string text)
        {
            // TODO should go into general utility class
            GDMTree tree = new GDMTree();
            GEDCOMProvider gp = new GEDCOMProvider(tree);
            try {
                gp.LoadFromString(text);
            } catch (Exception) {
            }
            Assert.AreEqual(1, tree.RecordsCount);
            GDMRecord rec = tree[0];
            Assert.IsTrue(rec is GDMIndividualRecord);
            GDMIndividualRecord rec2 = (GDMIndividualRecord)rec;
            // end for utility class

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
            Assert.AreEqual("Institute for Higher\r\nLearning\r\nNovozavodskaya ul., 10", res.Address.Text);
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
            // TODO The standard should be the second version of the input line, but in the future we can work out the first
            //string text = "0 @I1@ INDI\n1 FACT\n2 PHON +7 499 277-71-00\n2 ADDR Institute for Higher\n3 CONT Learning\n2 PHON +7 495 967-77-76\n";
            string text = "0 @I1@ INDI\n1 FACT\n2 ADDR Institute for Higher\n3 CONT Learning\n2 PHON +7 499 277-71-00\n2 PHON +7 495 967-77-76\n";
            GDMAddress res = AddrParse(text);
            Assert.AreEqual(2, res.PhoneNumbers.Count);
            Assert.AreEqual("Institute for Higher\r\nLearning", res.Address.Text);
        }
    }
}
