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
        public void Test_Create()
        {
            GEDCOMAddress instance = (GEDCOMAddress)GEDCOMAddress.Create(null, null, "", "");
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
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.Address = value;
            Assert.AreEqual(value.Text, instance.Address.Text);
        }

        [Test]
        public void Test_SetAddressLine1()
        {
            string value = "1234 Main St";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressLine1 = value;
            Assert.AreEqual(value, instance.AddressLine1);
        }

        [Test]
        public void Test_SetAddressLine2()
        {
            string value = "Novozavodskaya ul., 10";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressLine2 = value;
            Assert.AreEqual(value, instance.AddressLine2);
        }

        [Test]
        public void Test_SetAddressLine3()
        {
            string value = "ALEKSCEVSKTY r-n";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressLine3 = value;
            Assert.AreEqual(value, instance.AddressLine3);
        }

        [Test]
        public void Test_SetAddressCity()
        {
            string value = "Moskva";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressCity = value;
            Assert.AreEqual(value, instance.AddressCity);
        }

        [Test]
        public void Test_SetAddressState()
        {
            string value = "VORONEJSKAYA obl";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressState = value;
            Assert.AreEqual(value, instance.AddressState);
        }

        [Test]
        public void Test_SetAddressPostalCode()
        {
            string value = "1234A567";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressPostalCode = value;
            Assert.AreEqual(value, instance.AddressPostalCode);
        }

        [Test]
        public void Test_SetAddressCountry()
        {
            string value = "Moosylvania";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddressCountry = value;
            Assert.AreEqual(value, instance.AddressCountry);
        }

        [Test]
        public void Test_AddEmailAddress()
        {
            string value = "serg.zhdanovskih@yandex.ru";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddEmailAddress(value);
            GEDCOMList<GEDCOMTag> pl = instance.EmailAddresses;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl.Extract(0).StringValue);
        }

        [Test]
        public void Test_AddFaxNumber()
        {
            string value = "(214) 748-3647";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddFaxNumber(value);
            GEDCOMList<GEDCOMTag> pl = instance.FaxNumbers;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl.Extract(0).StringValue);
        }

        [Test]
        public void Test_AddPhoneNumber()
        {
            string value = "(214) 748-3647";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddPhoneNumber(value);
            GEDCOMList<GEDCOMTag> pl = instance.PhoneNumbers;
            Assert.AreEqual(1, pl.Count);
            Assert.AreEqual(value, pl[0].StringValue);
        }

        [Test]
        public void Test_AddPhoneNumber2()
        {
            string value1 = "(214) 748-3647";
            string value2 = "(999) 748-3647";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
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
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
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
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.AddWebPage(value1);
            instance.AddWebPage(value2);
            GEDCOMList<GEDCOMTag> wp = instance.WebPages;
            Assert.AreEqual(2, wp.Count);
            string res1 = wp[0].StringValue;
            Assert.AreEqual(res1, value1);
            string res2 = wp[1].StringValue;
            Assert.AreEqual(res2, value2);
        }

        [Test]
        public void Test_SaveTagsToStream()
        {
            GEDCOMAddress instance = new GEDCOMAddress(null, null);

            StreamWriter stream = null;
            Assert.Throws(typeof(NullReferenceException), () => {
                instance.SaveToStream(stream);
            });
        }

        [Test]
        public void Test_Assign()
        {
            GEDCOMTag source = null;
            GEDCOMAddress instance = new GEDCOMAddress(null, null);

            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });

            source = new GEDCOMAddress(null, null);
            instance.Assign(source);
        }

        [Test]
        public void Test_AddTag()
        {
            const string tagName = "BABA";
            const string tagValue = "YAGA";
            TagConstructor tagConstructor = null;
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            GEDCOMTag result = instance.AddTag(tagName, tagValue, tagConstructor);
            Assert.IsNotNull(instance.FindTag(tagName, 0));
        }

        [Test]
        public void Test_Clear()
        {
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.SetAddressText("blah");
            instance.Clear();
            Assert.IsTrue(instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyT()
        {
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.SetAddressText("blah");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_ResetOwner()
        {
            GEDCOMTree newOwner = new GEDCOMTree();
            GEDCOMAddress instance = new GEDCOMAddress(null, null);

            instance.ResetOwner(newOwner);
            Assert.AreEqual(newOwner, instance.Owner);
        }
        
        [Test]
        public void Test_SetAddressText()
        {
            string value = "this is a test";
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
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
            GEDCOMAddress instance = new GEDCOMAddress(null, null);
            instance.SetAddressArray(value);
            Assert.AreEqual(new StringList(value).Text, instance.Address.Text);
        }

        // Support function. Parse GEDCOM string, returns ADDR object found
        private GEDCOMAddress AddrParse(string text)
        {
            // TODO should go into general utility class
            GEDCOMTree tree = new GEDCOMTree();
            GEDCOMProvider gp = new GEDCOMProvider(tree);
            try {
                gp.LoadFromString(text);
            } catch (Exception) {
            }
            Assert.AreEqual(1, tree.RecordsCount);
            GEDCOMRecord rec = tree[0];
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
            Assert.AreEqual("Institute for Higher\r\nLearning\r\nNovozavodskaya ul., 10", res.Address.Text);
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
