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
using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    /**
     *
     * @author Kevin Routley (KBR) aka fire-eggs
     */
    [TestFixture]
    public class GEDCOMPersonalNameTests
    {
        [Test]
        public void Test_GetFullName()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.SetNameParts("Ivan Ivanov", "Fedoroff", "");
            string expResult = "Ivan Ivanov Fedoroff";
            string result = instance.FullName;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_GetFirstPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string expResult = "Ivan";
            string result = instance.FirstPart;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetFirstPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string value = "Baba Yaga";
            instance.FirstPart = value;
            string result = instance.FullName;
            string expResult = "Baba Yaga Fedoroff";
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_GetSurname()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string expResult = "Fedoroff";
            string result = instance.Surname;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetSurname()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string value = "Yaga";
            instance.Surname = value;
            string result = instance.FullName;
            string expResult = "Ivan Yaga";
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_GetLastPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string expResult = "Esquire";
            string result = instance.LastPart;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetLastPart()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string value = "the III";
            instance.LastPart = value;
            string result = instance.LastPart;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_GetPieces()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            Assert.IsNotNull(instance.Pieces);
        }

        [Test]
        public void Test_GetNameType()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            GEDCOMNameType expResult = GEDCOMNameType.ntNone;
            GEDCOMNameType result = instance.NameType;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetNameType()
        {
            GEDCOMNameType value = GEDCOMNameType.ntBirth;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.NameType = value;
            GEDCOMNameType result = instance.NameType;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_GetStringValue()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");

            string expResult = "Ivan Ivanoff /Fedoroff/ Esquire";
            Assert.AreEqual(expResult, instance.StringValue);
        }

        [Test]
        public void Test_ParseString()
        {
            // TODO BUG return value from parsestring has no meaning (all codepaths return same)
            string strValue = "";
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            Assert.AreEqual("", instance.ParseString(strValue));
        }

        /**
         * Code coverage: unterminated surname
         */
        [Test]
        public void Test_ParseString2()
        {
            string strValue = "Pytor /the great";
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString(strValue);
            Assert.AreEqual("Pytor", instance.FullName);
        }
        
        [Test]
        public void Test_SetNameParts()
        {
            string firstPart = "Ivan Ivanoff";
            string surname = "Fedoroff";
            string lastPart = "Esquire";
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.SetNameParts(firstPart, surname, lastPart);
            Assert.AreEqual("Ivan Ivanoff Fedoroff Esquire", instance.FullName);
        }

        [Test]
        public void Test_SetNamePartsNull()
        {
            string firstPart = "Ivan Ivanoff";
            string surname = "Fedoroff";
            string lastPart = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.SetNameParts(firstPart, surname, lastPart);
            Assert.AreEqual("Ivan Ivanoff Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_AddTag()
        {
            string tagName = GEDCOMTagType.TYPE;
            string tagValue = "gibber";
            TagConstructor tagConstructor = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.AddTag(tagName, tagValue, tagConstructor);
            Assert.IsNotNull(instance.FindTag(tagName, 0));
        }

        [Test]
        public void Test_Assign()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");

            GEDCOMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });
        }

        [Test]
        public void Test_Clear()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            instance.Clear();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmpty()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_Pack()
        {
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.AddTag("BLECH", null, null);
            instance.Pack();
            Assert.IsNull(instance.FindTag("BLECH", 0));
        }

        [Test]
        public void Test_ReplaceXRefs()
        {
            XRefReplacer map = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");
            instance.ReplaceXRefs(map);
        }

        [Test]
        public void Test_SaveToStream()
        {
            StreamWriter stream = null;
            GEDCOMPersonalName instance = new GEDCOMPersonalName(null, "", "");

            Assert.Throws(typeof(NullReferenceException), () => {
                instance.SaveToStream(stream, 0);
            });
        }

        /**
         * First part only, will match.
         */
        [Test]
        public void Test_IsMatch1()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.ParseString("Ivan Ivanoff");

            float result = instance1.IsMatch(instance2, true);
            Assert.AreEqual(100.0f, result, 0.0);
        }

        /**
         * First part only, will not match.
         */
        [Test]
        public void Test_IsMatch2()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.ParseString("Pyotr Ivanoff");
            float result = instance1.IsMatch(instance2, true);
            Assert.AreEqual(0.0F, result, 0.0);
        }

        /**
         * Not first part only, will match.
         */
        [Test]
        public void Test_IsMatch3()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.SetNameParts("Ivan Ivanoff", "Fedoroff", "");
            float result = instance1.IsMatch(instance2, false);
            Assert.AreEqual(100.0F, result, 0.0);
        }

        /**
         * Not first part only, will not match.
         */
        [Test]
        public void Test_IsMatch4()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.SetNameParts("Pyotr", "Fedoroff", "Esquire");
            float result = instance1.IsMatch(instance2, false);
            Assert.AreNotEqual(100.0F, result);
        }

        /**
         * Code coverage: "Other" name null
         */
        [Test]
        public void Test_IsMatch5()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            float result = instance1.IsMatch(null, false);
            Assert.AreEqual(0.0F, result, 0.0);
        }
        
        /**
         * Code coverage: surnames of "?"
         */
        [Test]
        public void Test_IsMatch6()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /?/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.SetNameParts("Ivan Ivanoff", "?", "");
            float result = instance1.IsMatch(instance2, false);
            Assert.AreEqual(100.0F, result, 0.0);
        }

        /**
         * Code coverage: empty first parts
         */
        [Test]
        public void Test_IsMatch7()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("/Federoff/");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.SetNameParts("", "Federoff", "");
            float result = instance1.IsMatch(instance2, false);
            Assert.AreEqual(100.0F, result, 0.0);
        }

        /**
         * Code coverage: surnames of "?"/"Unknown"
         */
        [Test]
        public void Test_IsMatch8()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /?/");

            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.SetNameParts("Ivan Ivanoff", "Unknown", "");

            float result = instance1.IsMatch(instance2, false);
            Assert.AreEqual(100.0f, result, 0.0);
        }

        /**
         * Code coverage: empty surnames
         */
        [Test]
        public void Test_IsMatch9()
        {
            GEDCOMPersonalName instance1 = new GEDCOMPersonalName(null, "", "");
            instance1.ParseString("Vasiliy Pupkin");
            GEDCOMPersonalName instance2 = new GEDCOMPersonalName(null, "", "");
            instance2.SetNameParts("Vasiliy Pupkin", "", "");
            float result = instance1.IsMatch(instance2, false);
            Assert.AreEqual(100.0F, result, 0.0);
        }

        [Test]
        public void Test_Create()
        {
            GEDCOMTag result = GEDCOMPersonalName.Create(null, "", "");
            Assert.IsNotNull(result);
        }
    }
}