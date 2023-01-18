/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2015-2023 by Serg V. Zhdanovskih.
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

using NUnit.Framework;

namespace GKCore.Linguistics
{
    [TestFixture]
    public class MorpherTests
    {
        [Test]
        public void Test_BaseMorpher_Transliterate()
        {
            Assert.AreEqual("Zhdanovskikh", Morpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, "Ждановских"));
            Assert.AreEqual("ZHDANOVSKIKH", Morpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, "ЖДАНОВСКИХ"));

            Assert.AreEqual("Ждановских", Morpher.Transliterate(TranslitScheme.ts_GOST, TranslitScheme.ts_Russian, "Zhdanovskikh"));
            Assert.AreEqual("ЖДАНОВСКИХ", Morpher.Transliterate(TranslitScheme.ts_GOST, TranslitScheme.ts_Russian, "ZHDANOVSKIKH"));

            Assert.AreEqual("ЖдАноВскИх", Morpher.Transliterate(TranslitScheme.ts_GOST, TranslitScheme.ts_Russian, "ZhdAnoVskIkh"));
            Assert.AreEqual("ZHDANOVSKIKH", Morpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, "ZHDANOVSKIKH"));
        }

        [Test]
        public void Test_BaseMorpher_SpellNumber()
        {
            Assert.AreEqual("сто двадцать три", Morpher.SpellNumber(123));
        }

        [Test]
        public void Test_Morpher_GetDeclension()
        {
            Assert.AreEqual("Иванова Ивана Ивановича", Morpher.GetDeclension("Иванов Иван Иванович", DeclensionCase.Genitive));

            Assert.AreEqual("Иванова-Петрова Ивана Ивановича", Morpher.GetDeclension("Иванов-Петров Иван Иванович", DeclensionCase.Genitive));

            //Assert.AreEqual("атому", RusDeclension.GetDeclension("атом", DeclensionCase.Dative));
            //Assert.AreEqual("лугу", RusDeclension.GetDeclension("луг", DeclensionCase.Dative));
        }

        [Test]
        public void Test_Morpher_MorphAdjective()
        {
            Assert.AreEqual("хороший", Morpher.MorphAdjective("хороший", DeclensionCase.Nominative, Number.nSingle, DeclensionGender.Masculine));
            Assert.AreEqual("хорошего", Morpher.MorphAdjective("хороший", DeclensionCase.Genitive, Number.nSingle, DeclensionGender.Masculine));
            Assert.AreEqual("хорошему", Morpher.MorphAdjective("хороший", DeclensionCase.Dative, Number.nSingle, DeclensionGender.Masculine));
            //Assert.AreEqual("хороший", Morpher.MorphAdjective("хороший", DeclensionCase.Accusative, Number.nSingle, DeclensionGender.Masculine));
            Assert.AreEqual("хорошим", Morpher.MorphAdjective("хороший", DeclensionCase.Instrumental, Number.nSingle, DeclensionGender.Masculine));
            Assert.AreEqual("хорошем", Morpher.MorphAdjective("хороший", DeclensionCase.Prepositional, Number.nSingle, DeclensionGender.Masculine));
        }

        [Test]
        public void Test_Morpher_MorphNoun()
        {
            Assert.AreEqual("предмет", Morpher.MorphNoun("предмет", DeclensionCase.Nominative, Number.nSingle, DeclensionGender.Masculine, false, true));
            Assert.AreEqual("предмета", Morpher.MorphNoun("предмет", DeclensionCase.Genitive, Number.nSingle, DeclensionGender.Masculine, false, true));
            Assert.AreEqual("предмету", Morpher.MorphNoun("предмет", DeclensionCase.Dative, Number.nSingle, DeclensionGender.Masculine, false, true));
            //Assert.AreEqual("предмет", Morpher.MorphNoun("предмет", DeclensionCase.Accusative, Number.nSingle, DeclensionGender.Masculine, false, true));
            Assert.AreEqual("предметом", Morpher.MorphNoun("предмет", DeclensionCase.Instrumental, Number.nSingle, DeclensionGender.Masculine, false, true));
            Assert.AreEqual("предмете", Morpher.MorphNoun("предмет", DeclensionCase.Prepositional, Number.nSingle, DeclensionGender.Masculine, false, true));
        }

        [Test]
        public void Test_Morpher_Pluralize()
        {
            string[] nounForms = new string[] { "монета", "монеты", "монет" };
            Assert.AreEqual("монета", Morpher.Pluralize(1, nounForms));
            Assert.AreEqual("монеты", Morpher.Pluralize(2, nounForms));
            Assert.AreEqual("монеты", Morpher.Pluralize(22, nounForms));
            Assert.AreEqual("монет", Morpher.Pluralize(5, nounForms));
            Assert.AreEqual("монет", Morpher.Pluralize(11, nounForms));
        }
    }
}
