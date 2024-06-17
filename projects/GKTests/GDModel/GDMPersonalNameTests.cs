/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    /**
     *
     * @author Kevin Routley (KBR) aka fire-eggs
     */
    [TestFixture]
    public class GDMPersonalNameTests
    {
        private BaseContext fContext;

        public GDMPersonalNameTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_GEDCOMStandard()
        {
            // checking for correction of incorrect parts of the NAME tag when reading subordinate tags
            string text = 
                "0 @I1@ INDI\r\n" +
                "1 NAME Lt0 Petr1 Ivanovich2 /de3 Fedoroff4/ jr.5\r\n" + // wrong parts of NAME
                "2 SURN Fedoroff\r\n" +
                "2 GIVN Petr\r\n" +
                "2 _PATN Ivanovich\r\n" +
                "2 NPFX Lt.\r\n" +
                "2 NICK Nickname\r\n" +
                "2 SPFX de\r\n" +
                "2 NSFX jr.\r\n";

            GDMIndividualRecord iRec = TestUtils.ParseIndiRec(text);

            Assert.AreEqual(1, iRec.PersonalNames.Count);
            var persName = iRec.PersonalNames[0];

            Assert.AreEqual("Lt.", persName.NamePrefix);
            Assert.AreEqual("Petr", persName.Given);
            Assert.AreEqual("Ivanovich", persName.PatronymicName);
            Assert.AreEqual("de", persName.SurnamePrefix);
            Assert.AreEqual("Fedoroff", persName.Surname);
            Assert.AreEqual("jr.", persName.NameSuffix);

            Assert.AreEqual("Lt. Petr Ivanovich /de Fedoroff/ jr.", persName.StringValue);
        }

        [Test]
        public void Test_Common()
        {
            fContext.Tree.Header.Language = GDMLanguageID.Russian;

            var iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            GDMPersonalName pName = iRec.PersonalNames[0];
            Assert.AreEqual("Ivanov", pName.Surname);
            Assert.AreEqual("Ivan", pName.Given);
            Assert.AreEqual("Ivanovich", pName.PatronymicName);

            pName.ParseString("Ivan Ivanovich /Ivanov/ jr.");
            Assert.AreEqual("Ivanov", pName.Surname);
            Assert.AreEqual("Ivan Ivanovich", pName.Given);
            Assert.AreEqual("jr.", pName.NameSuffix);

            pName.PatronymicName = string.Empty;
            var parts = GKUtils.GetNameParts(fContext.Tree, iRec);
            Assert.AreEqual("Ivanov", parts.Surname);
            Assert.AreEqual("Ivan", parts.Name);
            Assert.AreEqual("Ivanovich", parts.Patronymic);


            GDMPersonalName persName = new GDMPersonalName();
            iRec.AddPersonalName(persName);

            persName = iRec.PersonalNames[0];
            persName.NameType = GDMNameType.ntBirth;
            Assert.AreEqual(GDMNameType.ntBirth, persName.NameType);

            //

            persName.ParseString("Petr /Ivanov/ esq");

            Assert.AreEqual("Petr", persName.Given);
            Assert.AreEqual("Ivanov", persName.Surname);
            Assert.AreEqual("esq", persName.NameSuffix);

            Assert.AreEqual("Petr Ivanov esq", persName.FullName);

            persName.Given = "Petr";
            Assert.AreEqual("Petr", persName.Given);

            persName.NamePrefix = "Lt.";
            Assert.AreEqual("Lt.", persName.NamePrefix);

            persName.Nickname = "Nickname";
            Assert.AreEqual("Nickname", persName.Nickname);

            persName.SurnamePrefix = "de";
            Assert.AreEqual("de", persName.SurnamePrefix);

            persName.Surname = "Fedoroff";
            Assert.AreEqual("Fedoroff", persName.Surname);

            persName.NameSuffix = "Suffix";
            Assert.AreEqual("Suffix", persName.NameSuffix);

            persName.PatronymicName = "Ivanovich";
            Assert.AreEqual("Ivanovich", persName.PatronymicName);

            persName.MarriedName = "MarriedName";
            Assert.AreEqual("MarriedName", persName.MarriedName);

            persName.ReligiousName = "ReligiousName";
            Assert.AreEqual("ReligiousName", persName.ReligiousName);

            persName.CensusName = "CensusName";
            Assert.AreEqual("CensusName", persName.CensusName);

            //

            Assert.AreEqual(GDMLanguageID.Unknown, persName.Language);
            persName.Language = GDMLanguageID.English;
            Assert.AreEqual(GDMLanguageID.English, persName.Language);
            persName.Language = GDMLanguageID.Unknown;
            Assert.AreEqual(GDMLanguageID.Unknown, persName.Language);
            persName.Language = GDMLanguageID.Polish;
            Assert.AreEqual(GDMLanguageID.Polish, persName.Language);

            //

            var note = new GDMNotes();
            note.Lines.Text = "persname notes";
            persName.Notes.Add(note);

            var sourCit = new GDMSourceCitation();
            sourCit.Description.Text = "persname sour desc";
            persName.SourceCitations.Add(sourCit);

            //

            string buf = GEDCOMProvider.GetTagStreamText(persName, 1);
            Assert.AreEqual("1 NAME Lt. Petr Ivanovich /de Fedoroff/ Suffix\r\n"+
                            "2 LANG Polish\r\n"+ // extension
                            "2 TYPE birth\r\n"+
                            "2 NOTE persname notes\r\n"+
                            "2 SOUR persname sour desc\r\n"+
                            "2 SURN Fedoroff\r\n"+
                            "2 GIVN Petr\r\n"+
                            "2 _PATN Ivanovich\r\n"+
                            "2 NPFX Lt.\r\n"+
                            "2 NICK Nickname\r\n"+
                            "2 SPFX de\r\n"+
                            "2 NSFX Suffix\r\n"+
                            "2 _MARN MarriedName\r\n"+
                            "2 _RELN ReligiousName\r\n"+
                            "2 _CENN CensusName\r\n", buf);

            persName.Language = GDMLanguageID.Unknown;

            using (GDMPersonalName nameCopy = new GDMPersonalName()) {
                Assert.Throws(typeof(ArgumentException), () => { nameCopy.Assign(null); });

                iRec.AddPersonalName(nameCopy);
                nameCopy.Assign(persName);

                string buf2 = GEDCOMProvider.GetTagStreamText(nameCopy, 1);
                Assert.AreEqual("1 NAME Lt. Petr Ivanovich /de Fedoroff/ Suffix\r\n"+
                                "2 TYPE birth\r\n"+
                                "2 NOTE persname notes\r\n"+
                                "2 SOUR persname sour desc\r\n"+
                                "2 SURN Fedoroff\r\n"+
                                "2 GIVN Petr\r\n"+
                                "2 _PATN Ivanovich\r\n"+
                                "2 NPFX Lt.\r\n"+
                                "2 NICK Nickname\r\n"+
                                "2 SPFX de\r\n"+
                                "2 NSFX Suffix\r\n"+
                                "2 _MARN MarriedName\r\n"+
                                "2 _RELN ReligiousName\r\n"+
                                "2 _CENN CensusName\r\n", buf2);

                iRec.PersonalNames.Remove(nameCopy);
            }

            using (GDMPersonalName name1 = new GDMPersonalName()) {
                Assert.AreEqual("", name1.Given);
                Assert.AreEqual("", name1.Surname);

                Assert.AreEqual(0.0f, name1.IsMatch(null, false));

                using (GDMPersonalName name2 = new GDMPersonalName()) {
                    Assert.AreEqual(0.0f, name1.IsMatch(name2, false));

                    name1.ParseString("Ivan /Dub/");
                    name2.ParseString("Ivan /Dub/");
                    Assert.AreEqual(100.0f, name1.IsMatch(name2, false));

                    name1.ParseString("Ivan /Dub/");
                    name2.ParseString("Ivan /Dub2/");
                    Assert.AreEqual(12.5f, name1.IsMatch(name2, false));

                    name1.ParseString("Ivan /Dub/");
                    name2.ParseString("Ivan2 /Dub/");
                    Assert.AreEqual(50.0f, name1.IsMatch(name2, false));
                }
            }

            persName.Clear();
            Assert.IsTrue(persName.IsEmpty());
        }


        [Test]
        public void Test_GetFullName()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan Ivanov /Fedoroff/");
            Assert.AreEqual("Ivan Ivanov Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_GetFirstPart()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan/Fedoroff/");
            Assert.AreEqual("Ivan", instance.Given);
        }

        [Test]
        public void Test_GetSurname()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan/Fedoroff/");
            Assert.AreEqual("Fedoroff", instance.Surname);
        }

        [Test]
        public void Test_SetSurname()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan/Fedoroff/");
            instance.Surname = "Yaga";
            Assert.AreEqual("Ivan Yaga", instance.FullName);
        }

        [Test]
        public void Test_GetLastPart()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            Assert.AreEqual("Esquire", instance.NameSuffix);
        }

        [Test]
        public void Test_SetLastPart()
        {
            var instance = new GDMPersonalName();

            instance.ParseString("Ivan/Fedoroff/ Esquire");
            Assert.AreEqual("Esquire", instance.NameSuffix);

            string value = "the III";
            instance.NameSuffix = value;
            Assert.AreEqual(value, instance.NameSuffix);
        }

        [Test]
        public void Test_GetNameType()
        {
            GDMPersonalName instance = new GDMPersonalName();
            Assert.AreEqual(GDMNameType.ntNone, instance.NameType);
        }

        [Test]
        public void Test_SetNameType()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.NameType = GDMNameType.ntBirth;
            Assert.AreEqual(GDMNameType.ntBirth, instance.NameType);
        }

        [Test]
        public void Test_GetStringValue()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            Assert.AreEqual("Ivan Ivanoff /Fedoroff/ Esquire", instance.StringValue);
        }

        [Test]
        public void Test_ParseString()
        {
            // TODO BUG return value from parsestring has no meaning (all codepaths return same)
            GDMPersonalName instance = new GDMPersonalName();
            Assert.AreEqual("", instance.ParseString(""));
        }

        /**
         * Code coverage: unterminated surname
         */
        [Test]
        public void Test_ParseString2()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Pytor /the great");
            Assert.AreEqual("Pytor", instance.FullName);
        }
        
        [Test]
        public void Test_SetNameParts()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan Ivanoff /Fedoroff/ Esquire");
            Assert.AreEqual("Ivan Ivanoff Fedoroff Esquire", instance.FullName);
        }

        [Test]
        public void Test_SetNamePartsNull()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString("Ivan Ivanoff /Fedoroff/");
            Assert.AreEqual("Ivan Ivanoff Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_NameType()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.NameType = GDMNameType.ntImmigrant;
            Assert.AreEqual(GDMNameType.ntImmigrant, instance.NameType);
        }

        [Test]
        public void Test_Assign()
        {
            GDMPersonalName instance = new GDMPersonalName();

            GDMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });
        }

        [Test]
        public void Test_Clear()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            instance.Clear();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmpty()
        {
            GDMPersonalName instance = new GDMPersonalName();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_Pack()
        {
            GDMPersonalName instance = new GDMPersonalName();
            instance.AddTag(new GDMTag(GEDCOMTagsTable.Lookup("BLECH"), null));
            Assert.IsNotNull(instance.FindTag("BLECH", 0));
        }

        [Test]
        public void Test_ReplaceXRefs()
        {
            GDMXRefReplacer map = null;
            GDMPersonalName instance = new GDMPersonalName();
            instance.ReplaceXRefs(map);
        }

        /**
         * First part only, will match.
         */
        [Test]
        public void Test_IsMatch1()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Ivan Ivanoff");

            Assert.AreEqual(100.0f, instance1.IsMatch(instance2, true), 0.0);
        }

        /**
         * First part only, will not match.
         */
        [Test]
        public void Test_IsMatch2()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Pyotr Ivanoff");

            Assert.AreEqual(0.0F, instance1.IsMatch(instance2, true), 0.0);
        }

        /**
         * Not first part only, will match.
         */
        [Test]
        public void Test_IsMatch3()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Ivan Ivanoff /Fedoroff/");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Not first part only, will not match.
         */
        [Test]
        public void Test_IsMatch4()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Pyotr /Fedoroff/ Esquire");

            Assert.AreNotEqual(100.0F, instance1.IsMatch(instance2, false));
        }

        /**
         * Code coverage: "Other" name null
         */
        [Test]
        public void Test_IsMatch5()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            Assert.AreEqual(0.0F, instance1.IsMatch(null, false), 0.0);
        }
        
        /**
         * Code coverage: surnames of "?"
         */
        [Test]
        public void Test_IsMatch6()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /?/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Ivan Ivanoff /?/");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Code coverage: empty first parts
         */
        [Test]
        public void Test_IsMatch7()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("/Federoff/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("/Federoff/");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Code coverage: surnames of "?"/"Unknown"
         */
        [Test]
        public void Test_IsMatch8()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Ivan Ivanoff /?/");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Ivan Ivanoff /Unknown/");

            Assert.AreEqual(100.0f, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Code coverage: empty surnames
         */
        [Test]
        public void Test_IsMatch9()
        {
            GDMPersonalName instance1 = new GDMPersonalName();
            instance1.ParseString("Vasiliy Pupkin");

            GDMPersonalName instance2 = new GDMPersonalName();
            instance2.ParseString("Vasiliy Pupkin");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        [Test]
        public void Test_Create()
        {
            GDMTag result = new GDMPersonalName();
            Assert.IsNotNull(result);
        }
    }
}
