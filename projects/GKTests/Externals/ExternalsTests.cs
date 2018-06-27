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

using BSLib;
using BSLib.Linguistics.Grammar;
using NUnit.Framework;

namespace Externals
{
    [TestFixture]
    public class ExternalsTests
    {
        /*public void MyTestFunc1(
            [Values(1, 2, 5)]int x,
            [Values("hello", "buy")]string s)
        {
            Assert.IsTrue(x < 10);
        }

        [Test(Description = "X test")]
        public void MyTestFunc2(
            [Range(1, 100, 2)]int x,
            [Values("hello", "buy")]string s)
        {
            Assert.IsTrue(x < 50);
        }

        public void MyTestFunc3(
            [Random(100)]int x,
            [Values("hello", "buy")]string s)
        {
        }*/

        /*[TestCase("", Description = "Empty XRef Test", ExpectedException = typeof(EGEDCOMException))]
        [TestCase("@sample", Description = "Bad XRef Test", ExpectedException = typeof(EGEDCOMException))]
        public void GEDCOMUtils_ExtractXRef_Tests(string arg)
        {
        }*/

        [Test]
        public void Test_ConvertHelper_UniformName()
        {
            string st = "ivan";
            st = ConvertHelper.UniformName(st);
            Assert.AreEqual("Ivan", st);

            st = ConvertHelper.UniformName(null);
            Assert.AreEqual(null, st);
        }

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
    }
}
