using System;
using BSLib.Linguistics.Grammar;
using NUnit.Framework;

namespace BSLib.Linguistics
{
    [TestFixture]
    public class BaseMorpherTests
    {
        [Test]
        public void Test_BaseMorpher_Transliterate()
        {
            Assert.AreEqual("Zhdanovskikh", BaseMorpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, "Ждановских"));
            Assert.AreEqual("ZHDANOVSKIKH", BaseMorpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, "ЖДАНОВСКИХ"));

            Assert.AreEqual("Ждановских", BaseMorpher.Transliterate(TranslitScheme.ts_GOST, TranslitScheme.ts_Russian, "Zhdanovskikh"));
            Assert.AreEqual("ЖДАНОВСКИХ", BaseMorpher.Transliterate(TranslitScheme.ts_GOST, TranslitScheme.ts_Russian, "ZHDANOVSKIKH"));

            Assert.AreEqual("ЖдАноВскИх", BaseMorpher.Transliterate(TranslitScheme.ts_GOST, TranslitScheme.ts_Russian, "ZhdAnoVskIkh"));
            Assert.AreEqual("ZHDANOVSKIKH", BaseMorpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, "ZHDANOVSKIKH"));
        }

        [Test]
        public void Test_BaseMorpher_SpellNumber()
        {
            Assert.AreEqual("сто двадцать три", BaseMorpher.SpellNumber(123));
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
