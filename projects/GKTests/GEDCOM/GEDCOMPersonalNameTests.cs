/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    /**
     *
     * @author Kevin Routley (KBR) aka fire-eggs
     */
    [TestFixture]
    public class GEDCOMPersonalNameTest
    {
        public GEDCOMPersonalNameTest()
        {
        }
        
        [Test]
        public void testGetFullName()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.SetNameParts("Ivan Ivanov", "Fedoroff", "");
            string expResult = "Ivan Ivanov Fedoroff";
            string result = instance.FullName;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testGetFirstPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string expResult = "Ivan";
            string result = instance.FirstPart;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetFirstPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string value = "Baba Yaga";
            instance.FirstPart = value;
            string result = instance.FullName;
            string expResult = "Baba Yaga Fedoroff";
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testGetSurname()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string expResult = "Fedoroff";
            string result = instance.Surname;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetSurname()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string value = "Yaga";
            instance.Surname = value;
            string result = instance.FullName;
            string expResult = "Ivan Yaga";
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testGetLastPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string expResult = "Esquire";
            string result = instance.LastPart;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetLastPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string value = "the III";
            instance.LastPart = value;
            string result = instance.LastPart;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void testGetPieces()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            Assert.IsNotNull(instance.Pieces);
        }

        [Test]
        public void testGetNameType()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            GEDCOMNameType expResult = GEDCOMNameType.ntNone;
            GEDCOMNameType result = instance.NameType;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetNameType()
        {
            GEDCOMNameType value = GEDCOMNameType.ntBirth;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.NameType = value;
            GEDCOMNameType result = instance.NameType;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void testGetStringValue()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");

            string expResult = "Ivan Ivanoff /Fedoroff/ Esquire";
            Assert.AreEqual(expResult, instance.StringValue);
        }

        [Test]
        public void testParseString()
        {
            // TODO BUG return value from parsestring has no meaning (all codepaths return same)
            string strValue = "";
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
        }

        /**
         * Code coverage: unterminated surname
         */
        [Test]
        public void testParseString2()
        {
            string strValue = "Pytor /the great";
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            string expResult = "Pytor";
            instance.ParseString(strValue);
            string result = instance.FullName;
            Assert.AreEqual(expResult, result);
        }
        
        [Test]
        public void testSetNameParts()
        {
            string firstPart = "Ivan Ivanoff";
            string surname = "Fedoroff";
            string lastPart = "Esquire";
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.SetNameParts(firstPart, surname, lastPart);
            string result = instance.FullName;
            string expResult = "Ivan Ivanoff Fedoroff Esquire";
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetNamePartsNull()
        {
            string firstPart = "Ivan Ivanoff";
            string surname = "Fedoroff";
            string lastPart = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.SetNameParts(firstPart, surname, lastPart);

            Assert.AreEqual("Ivan Ivanoff Fedoroff", instance.FullName);
        }

        [Test]
        public void testAddTag()
        {
            string tagName = "TYPE";
            string tagValue = "gibber";
            TagConstructor tagConstructor = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.AddTag(tagName, tagValue, tagConstructor);
            Assert.IsNotNull(instance.FindTag(tagName, 0));
        }

        [Test]
        public void testAssign()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");

            GEDCOMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });
        }

        [Test]
        public void testClear()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            instance.Clear();
            bool expResult = true;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testIsEmpty()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            bool expResult = true;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testIsEmptyF()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            bool expResult = false;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }
        
        [Test]
        public void testPack()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.AddTag("BLECH", null, null);
            instance.Pack();
            Assert.IsNull(instance.FindTag("BLECH", 0));
        }

        [Test]
        public void testReplaceXRefs()
        {
            XRefReplacer map = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ReplaceXRefs(map);
        }

        [Test]
        public void testResetOwner()
        {
            GEDCOMTree newOwner = new GEDCOMTree();
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");
            instance.ResetOwner(newOwner);

            Assert.AreEqual(newOwner, instance.Owner);
        }

        [Test]
        public void testSaveToStream()
        {
            StreamWriter stream = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, null, "", "");

            Assert.Throws(typeof(NullReferenceException), () => {
                instance.SaveToStream(stream);
            });
        }

        /**
         * First part only, will match.
         */
        [Test]
        public void testIsMatch1()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.ParseString("Ivan Ivanoff");

            bool onlyFirstPart = true;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(100.0f, result, 0.0);
        }

        /**
         * First part only, will not match.
         */
        [Test]
        public void testIsMatch2()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.ParseString("Pyotr Ivanoff");
            bool onlyFirstPart = true;
            float expResult = 0.0F;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(expResult, result, 0.0);
        }

        /**
         * Not first part only, will match.
         */
        [Test]
        public void testIsMatch3()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.SetNameParts("Ivan Ivanoff", "Fedoroff", "");
            bool onlyFirstPart = false;
            float expResult = 100.0F;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(expResult, result, 0.0);
        }

        /**
         * Not first part only, will not match.
         */
        [Test]
        public void testIsMatch4()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.SetNameParts("Pyotr", "Fedoroff", "Esquire");
            bool onlyFirstPart = false;
            float expResult = 100.0F;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreNotEqual(expResult, result);
        }

        /**
         * Code coverage: "Other" name null
         */
        [Test]
        public void testIsMatch5()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            bool onlyFirstPart = false;
            float expResult = 0.0F;
            float result = instance1.IsMatch(null, onlyFirstPart);
            Assert.AreEqual(expResult, result, 0.0);
        }
        
        /**
         * Code coverage: surnames of "?"
         */
        [Test]
        public void testIsMatch6()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /?/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.SetNameParts("Ivan Ivanoff", "?", "");
            bool onlyFirstPart = false;
            float expResult = 100.0F;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(expResult, result, 0.0);
        }

        /**
         * Code coverage: empty first parts
         */
        [Test]
        public void testIsMatch7()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("/Federoff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.SetNameParts("", "Federoff", "");
            bool onlyFirstPart = false;
            float expResult = 100.0F;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(expResult, result, 0.0);
        }

        /**
         * Code coverage: surnames of "?"/"Unknown"
         */
        [Test]
        public void testIsMatch8()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Ivan Ivanoff /?/");

            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.SetNameParts("Ivan Ivanoff", "Unknown", "");

            bool onlyFirstPart = false;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(100.0f, result, 0.0);
        }

        /**
         * Code coverage: empty surnames
         */
        [Test]
        public void testIsMatch9()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, null, "", "");
            instance1.ParseString("Vasiliy Pupkin");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, null, "", "");
            instance2.SetNameParts("Vasiliy Pupkin", "", "");
            bool onlyFirstPart = false;
            float expResult = 100.0F;
            float result = instance1.IsMatch(instance2, onlyFirstPart);
            Assert.AreEqual(expResult, result, 0.0);
        }

        [Test]
        public void testCreate()
        {
            GEDCOMTree owner = null;
            GEDCOMObject parent = null;
            string tagName = "";
            string tagValue = "";
            GEDCOMTag expResult = null;
            GEDCOMTag result = GEDCOMPersonalName.Create(owner, parent, tagName, tagValue);
            Assert.IsNotNull(result);
        }
    }
}