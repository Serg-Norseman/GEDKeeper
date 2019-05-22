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
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.SetNameParts("Ivan Ivanov", "Fedoroff", "");
            Assert.AreEqual("Ivan Ivanov Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_GetFirstPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            Assert.AreEqual("Ivan", instance.FirstPart);
        }

        [Test]
        public void Test_SetFirstPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            instance.FirstPart = "Baba Yaga";
            Assert.AreEqual("Baba Yaga Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_GetSurname()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/");
            string expResult = "Fedoroff";
            string result = instance.Surname;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetSurname()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string expResult = "Esquire";
            string result = instance.LastPart;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetLastPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string value = "the III";
            instance.LastPart = value;
            string result = instance.LastPart;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_GetPieces()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            Assert.IsNotNull(instance.Pieces);
        }

        [Test]
        public void Test_GetNameType()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            GEDCOMNameType expResult = GEDCOMNameType.ntNone;
            GEDCOMNameType result = instance.NameType;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetNameType()
        {
            GEDCOMNameType value = GEDCOMNameType.ntBirth;
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.NameType = value;
            GEDCOMNameType result = instance.NameType;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_GetStringValue()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");

            string expResult = "Ivan Ivanoff /Fedoroff/ Esquire";
            Assert.AreEqual(expResult, instance.StringValue);
        }

        [Test]
        public void Test_ParseString()
        {
            // TODO BUG return value from parsestring has no meaning (all codepaths return same)
            string strValue = "";
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            Assert.AreEqual("", instance.ParseString(strValue));
        }

        /**
         * Code coverage: unterminated surname
         */
        [Test]
        public void Test_ParseString2()
        {
            string strValue = "Pytor /the great";
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString(strValue);
            Assert.AreEqual("Pytor", instance.FullName);
        }
        
        [Test]
        public void Test_SetNameParts()
        {
            string firstPart = "Ivan Ivanoff";
            string surname = "Fedoroff";
            string lastPart = "Esquire";
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.SetNameParts(firstPart, surname, lastPart);
            Assert.AreEqual("Ivan Ivanoff Fedoroff Esquire", instance.FullName);
        }

        [Test]
        public void Test_SetNamePartsNull()
        {
            string firstPart = "Ivan Ivanoff";
            string surname = "Fedoroff";
            string lastPart = null;
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.SetNameParts(firstPart, surname, lastPart);
            Assert.AreEqual("Ivan Ivanoff Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_NameType()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.NameType = GEDCOMNameType.ntImmigrant;
            Assert.AreEqual(GEDCOMNameType.ntImmigrant, instance.NameType);
        }

        [Test]
        public void Test_Assign()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");

            GDMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });
        }

        [Test]
        public void Test_Clear()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            instance.Clear();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmpty()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_Pack()
        {
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.AddTag(new GDMTag(instance, "BLECH", null));
            instance.Pack();
            Assert.IsNotNull(instance.FindTag("BLECH", 0));
        }

        [Test]
        public void Test_ReplaceXRefs()
        {
            XRefReplacer map = null;
            GDMPersonalName instance = new GDMPersonalName(null, "", "");
            instance.ReplaceXRefs(map);
        }

        [Test]
        public void Test_SaveToStream()
        {
            StreamWriter stream = null;
            GDMPersonalName instance = new GDMPersonalName(null, "", "");

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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");
            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /?/");
            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("/Federoff/");
            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Ivan Ivanoff /?/");

            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
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
            GDMPersonalName instance1 = new GDMPersonalName(null, "", "");
            instance1.ParseString("Vasiliy Pupkin");
            GDMPersonalName instance2 = new GDMPersonalName(null, "", "");
            instance2.SetNameParts("Vasiliy Pupkin", "", "");
            float result = instance1.IsMatch(instance2, false);
            Assert.AreEqual(100.0F, result, 0.0);
        }

        [Test]
        public void Test_Create()
        {
            GDMTag result = GDMPersonalName.Create(null, "", "");
            Assert.IsNotNull(result);
        }
    }
}