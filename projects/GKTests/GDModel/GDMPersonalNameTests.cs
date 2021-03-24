/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using GKUI;
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

        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();
            // TempDirtyHack: some functions are references to GlobalOptions (and GfxInit)
            // TODO: replace to mocks
            WFAppHost.ConfigureBootstrap(false);

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            GDMPersonalName pName = iRec.PersonalNames[0];
            Assert.AreEqual("Ivanov", pName.Surname);
            Assert.AreEqual("Ivan Ivanovich", pName.FirstPart);

            pName.SetNameParts("Ivan Ivanovich", "Ivanov", "testLastPart");
            Assert.AreEqual("Ivanov", pName.Surname);
            Assert.AreEqual("Ivan Ivanovich", pName.FirstPart);
            Assert.AreEqual("testLastPart", pName.LastPart);

//            GEDCOMPersonalNamePieces pieces = pName.Pieces;
//            Assert.AreEqual(pieces.Surname, "surname");
//            Assert.AreEqual(pieces.Name, "name");
//            Assert.AreEqual(pieces.PatronymicName, "patr");

            var parts = GKUtils.GetNameParts(fContext.Tree, iRec);
            Assert.AreEqual("Ivanov", parts.Surname);
            Assert.AreEqual("Ivan", parts.Name);
            Assert.AreEqual("Ivanovich", parts.Patronymic);


            GDMPersonalName persName = new GDMPersonalName(iRec);
            iRec.AddPersonalName(persName);

            persName = iRec.PersonalNames[0];
            persName.NameType = GDMNameType.ntBirth;
            Assert.AreEqual(GDMNameType.ntBirth, persName.NameType);

            //

            persName.SetNameParts("Petr", "Ivanov", "Fedoroff");

            //persName.Surname = "Ivanov";
            Assert.AreEqual("Petr", persName.FirstPart);
            Assert.AreEqual("Ivanov", persName.Surname);
            Assert.AreEqual("Fedoroff", persName.LastPart);

            Assert.AreEqual("Petr Ivanov Fedoroff", persName.FullName);

            persName.FirstPart = "Petr";
            Assert.AreEqual("Petr", persName.FirstPart);

            persName.Surname = "Test";
            Assert.AreEqual("Test", persName.Surname);

            persName.LastPart = "Fedoroff";
            Assert.AreEqual("Fedoroff", persName.LastPart);

            //

            GDMPersonalNamePieces pnPieces = persName.Pieces;
            
            pnPieces.Prefix = "Prefix";
            Assert.AreEqual("Prefix", pnPieces.Prefix);

            pnPieces.Given = "Given";
            Assert.AreEqual("Given", pnPieces.Given);

            pnPieces.Nickname = "Nickname";
            Assert.AreEqual("Nickname", pnPieces.Nickname);

            pnPieces.SurnamePrefix = "SurnamePrefix";
            Assert.AreEqual("SurnamePrefix", pnPieces.SurnamePrefix);

            pnPieces.Surname = "Surname";
            Assert.AreEqual("Surname", pnPieces.Surname);

            pnPieces.Suffix = "Suffix";
            Assert.AreEqual("Suffix", pnPieces.Suffix);

            pnPieces.PatronymicName = "PatronymicName";
            Assert.AreEqual("PatronymicName", pnPieces.PatronymicName);

            pnPieces.MarriedName = "MarriedName";
            Assert.AreEqual("MarriedName", pnPieces.MarriedName);

            pnPieces.ReligiousName = "ReligiousName";
            Assert.AreEqual("ReligiousName", pnPieces.ReligiousName);

            pnPieces.CensusName = "CensusName";
            Assert.AreEqual("CensusName", pnPieces.CensusName);

            Assert.Throws(typeof(ArgumentException), () => {
                pnPieces.Assign(null);
            });

            //

            Assert.AreEqual(GDMLanguageID.Unknown, persName.Language);
            persName.Language = GDMLanguageID.English;
            Assert.AreEqual(GDMLanguageID.English, persName.Language);
            persName.Language = GDMLanguageID.Unknown;
            Assert.AreEqual(GDMLanguageID.Unknown, persName.Language);
            persName.Language = GDMLanguageID.Polish;
            Assert.AreEqual(GDMLanguageID.Polish, persName.Language);

            //

            var note = new GDMNotes(persName);
            note.Lines.Text = "persname notes";
            persName.Notes.Add(note);

            var sourCit = new GDMSourceCitation(persName);
            sourCit.Description.Text = "persname sour desc";
            persName.SourceCitations.Add(sourCit);

            //

            string buf = TestUtils.GetTagStreamText(persName, 1);
            Assert.AreEqual("1 NAME Petr /Test/ Fedoroff\r\n"+
                            "2 LANG Polish\r\n"+ // extension
                            "2 TYPE birth\r\n"+
                            "2 NOTE persname notes\r\n"+
                            "2 SOUR persname sour desc\r\n"+
                            "2 SURN Surname\r\n"+
                            "2 GIVN Given\r\n"+
                            "2 _PATN PatronymicName\r\n"+
                            "2 NPFX Prefix\r\n"+
                            "2 NICK Nickname\r\n"+
                            "2 SPFX SurnamePrefix\r\n"+
                            "2 NSFX Suffix\r\n"+
                            "2 _MARN MarriedName\r\n"+
                            "2 _RELN ReligiousName\r\n"+
                            "2 _CENN CensusName\r\n", buf);

            persName.Language = GDMLanguageID.Unknown;

            using (GDMPersonalName nameCopy = new GDMPersonalName(iRec)) {
                Assert.Throws(typeof(ArgumentException), () => { nameCopy.Assign(null); });

                iRec.AddPersonalName(nameCopy);
                nameCopy.Assign(persName);

                string buf2 = TestUtils.GetTagStreamText(nameCopy, 1);
                Assert.AreEqual("1 NAME Petr /Test/ Fedoroff\r\n"+
                                "2 TYPE birth\r\n"+
                                "2 NOTE persname notes\r\n"+
                                "2 SOUR persname sour desc\r\n"+
                                "2 SURN Surname\r\n"+
                                "2 GIVN Given\r\n"+
                                "2 _PATN PatronymicName\r\n"+
                                "2 NPFX Prefix\r\n"+
                                "2 NICK Nickname\r\n"+
                                "2 SPFX SurnamePrefix\r\n"+
                                "2 NSFX Suffix\r\n"+
                                "2 _MARN MarriedName\r\n"+
                                "2 _RELN ReligiousName\r\n"+
                                "2 _CENN CensusName\r\n", buf2);

                iRec.PersonalNames.Delete(nameCopy);
            }

            using (GDMPersonalName name1 = new GDMPersonalName(null)) {
                Assert.AreEqual("", name1.FirstPart);
                Assert.AreEqual("", name1.Surname);

                Assert.AreEqual(0.0f, name1.IsMatch(null, false));

                using (GDMPersonalName name2 = new GDMPersonalName(null)) {
                    Assert.AreEqual(0.0f, name1.IsMatch(name2, false));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan", "Dub", "");
                    Assert.AreEqual(100.0f, name1.IsMatch(name2, false));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan", "Dub2", "");
                    Assert.AreEqual(12.5f, name1.IsMatch(name2, false));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan2", "Dub", "");
                    Assert.AreEqual(50.0f, name1.IsMatch(name2, false));
                }
            }

            persName.ResetOwner(fContext.Tree);

            persName.Clear();
            Assert.IsTrue(persName.IsEmpty());
        }


        [Test]
        public void Test_GetFullName()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.SetNameParts("Ivan Ivanov", "Fedoroff", "");
            Assert.AreEqual("Ivan Ivanov Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_GetFirstPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Ivan/Fedoroff/");
            Assert.AreEqual("Ivan", instance.FirstPart);
        }

        [Test]
        public void Test_SetFirstPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Ivan/Fedoroff/");
            instance.FirstPart = "Baba Yaga";
            Assert.AreEqual("Baba Yaga Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_GetSurname()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Ivan/Fedoroff/");
            Assert.AreEqual("Fedoroff", instance.Surname);
        }

        [Test]
        public void Test_SetSurname()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Ivan/Fedoroff/");
            instance.Surname = "Yaga";
            Assert.AreEqual("Ivan Yaga", instance.FullName);
        }

        [Test]
        public void Test_GetLastPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            Assert.AreEqual("Esquire", instance.LastPart);
        }

        [Test]
        public void Test_SetLastPart()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Ivan/Fedoroff/ Esquire");
            string value = "the III";
            instance.LastPart = value;
            Assert.AreEqual(value, instance.LastPart);
        }

        [Test]
        public void Test_GetPieces()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            Assert.IsNotNull(instance.Pieces);
        }

        [Test]
        public void Test_GetNameType()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            Assert.AreEqual(GDMNameType.ntNone, instance.NameType);
        }

        [Test]
        public void Test_SetNameType()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.NameType = GDMNameType.ntBirth;
            Assert.AreEqual(GDMNameType.ntBirth, instance.NameType);
        }

        [Test]
        public void Test_GetStringValue()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            Assert.AreEqual("Ivan Ivanoff /Fedoroff/ Esquire", instance.StringValue);
        }

        [Test]
        public void Test_ParseString()
        {
            // TODO BUG return value from parsestring has no meaning (all codepaths return same)
            GDMPersonalName instance = new GDMPersonalName(null);
            Assert.AreEqual("", instance.ParseString(""));
        }

        /**
         * Code coverage: unterminated surname
         */
        [Test]
        public void Test_ParseString2()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString("Pytor /the great");
            Assert.AreEqual("Pytor", instance.FullName);
        }
        
        [Test]
        public void Test_SetNameParts()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.SetNameParts("Ivan Ivanoff", "Fedoroff", "Esquire");
            Assert.AreEqual("Ivan Ivanoff Fedoroff Esquire", instance.FullName);
        }

        [Test]
        public void Test_SetNamePartsNull()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.SetNameParts("Ivan Ivanoff", "Fedoroff", null);
            Assert.AreEqual("Ivan Ivanoff Fedoroff", instance.FullName);
        }

        [Test]
        public void Test_NameType()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.NameType = GDMNameType.ntImmigrant;
            Assert.AreEqual(GDMNameType.ntImmigrant, instance.NameType);
        }

        [Test]
        public void Test_Assign()
        {
            GDMPersonalName instance = new GDMPersonalName(null);

            GDMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(source);
            });
        }

        [Test]
        public void Test_Clear()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            instance.Clear();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmpty()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_IsEmptyF()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ParseString(" Ivan Ivanoff / Fedoroff / Esquire ");
            Assert.AreEqual(false, instance.IsEmpty());
        }
        
        [Test]
        public void Test_Pack()
        {
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.AddTag(new GDMTag(instance, GEDCOMTagsTable.Lookup("BLECH"), null));
            Assert.IsNotNull(instance.FindTag("BLECH", 0));
        }

        [Test]
        public void Test_ReplaceXRefs()
        {
            GDMXRefReplacer map = null;
            GDMPersonalName instance = new GDMPersonalName(null);
            instance.ReplaceXRefs(map);
        }

        /**
         * First part only, will match.
         */
        [Test]
        public void Test_IsMatch1()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.ParseString("Ivan Ivanoff");

            Assert.AreEqual(100.0f, instance1.IsMatch(instance2, true), 0.0);
        }

        /**
         * First part only, will not match.
         */
        [Test]
        public void Test_IsMatch2()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.ParseString("Pyotr Ivanoff");

            Assert.AreEqual(0.0F, instance1.IsMatch(instance2, true), 0.0);
        }

        /**
         * Not first part only, will match.
         */
        [Test]
        public void Test_IsMatch3()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.SetNameParts("Ivan Ivanoff", "Fedoroff", "");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Not first part only, will not match.
         */
        [Test]
        public void Test_IsMatch4()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.SetNameParts("Pyotr", "Fedoroff", "Esquire");

            Assert.AreNotEqual(100.0F, instance1.IsMatch(instance2, false));
        }

        /**
         * Code coverage: "Other" name null
         */
        [Test]
        public void Test_IsMatch5()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /Fedoroff/");

            Assert.AreEqual(0.0F, instance1.IsMatch(null, false), 0.0);
        }
        
        /**
         * Code coverage: surnames of "?"
         */
        [Test]
        public void Test_IsMatch6()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /?/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.SetNameParts("Ivan Ivanoff", "?", "");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Code coverage: empty first parts
         */
        [Test]
        public void Test_IsMatch7()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("/Federoff/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.SetNameParts("", "Federoff", "");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Code coverage: surnames of "?"/"Unknown"
         */
        [Test]
        public void Test_IsMatch8()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Ivan Ivanoff /?/");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.SetNameParts("Ivan Ivanoff", "Unknown", "");

            Assert.AreEqual(100.0f, instance1.IsMatch(instance2, false), 0.0);
        }

        /**
         * Code coverage: empty surnames
         */
        [Test]
        public void Test_IsMatch9()
        {
            GDMPersonalName instance1 = new GDMPersonalName(null);
            instance1.ParseString("Vasiliy Pupkin");

            GDMPersonalName instance2 = new GDMPersonalName(null);
            instance2.SetNameParts("Vasiliy Pupkin", "", "");

            Assert.AreEqual(100.0F, instance1.IsMatch(instance2, false), 0.0);
        }

        [Test]
        public void Test_Create()
        {
            GDMTag result = new GDMPersonalName(null);
            Assert.IsNotNull(result);
        }
    }
}