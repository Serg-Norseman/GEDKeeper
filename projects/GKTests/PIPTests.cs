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

using GKCommon;
using NUnit.Framework;

namespace GKTests
{
    [TestFixture]
    public class PIPTests
    {
        [Test]
        public void ImpUtils_Valid_Tests()
        {
            bool res;
            string pid;

            res = ImportUtils.IsPersonLine_Konovalov("1. Ivan", out pid);
            Assert.AreEqual("1.", pid, "[v1]");
            Assert.AreEqual(true, res, "[v1]");

            res = ImportUtils.IsPersonLine_Konovalov("2-1. Ivan", out pid);
            Assert.AreEqual("2-1.", pid);
            Assert.AreEqual(true, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-21. Ivan", out pid);
            Assert.AreEqual("11-21.", pid);
            Assert.AreEqual(true, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-21/1. Ivan", out pid);
            Assert.AreEqual("11-21/1.", pid);
            Assert.AreEqual(true, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-21/?. Ivan", out pid);
            Assert.AreEqual("11-21/?.", pid, "[v2]");
            Assert.AreEqual(true, res, "[v2]");

            res = ImportUtils.IsPersonLine_Konovalov("11-21/1 (test+2, test). Ivan", out pid);
            Assert.AreEqual("11-21/1 (test+2, test).", pid);
            Assert.AreEqual(true, res);



            string persId, parentId, marNum, extData;
            int pos;
            res = ImportUtils.ParsePersonLine_Konovalov("11-21/1 (test+2, test). Ivan", out persId, out parentId, out marNum, out extData, out pos);
            Assert.AreEqual("11", persId);
            Assert.AreEqual("21", parentId);
            Assert.AreEqual("1", marNum);
            Assert.AreEqual("(test+2, test)", extData);
            Assert.AreEqual(true, res);

            res = ImportUtils.ParsePersonLine_Konovalov("11-21/?. Ivan", out persId, out parentId, out marNum, out extData, out pos);
            Assert.AreEqual("11", persId);
            Assert.AreEqual("21", parentId);
            Assert.AreEqual("?", marNum);
            Assert.AreEqual("", extData);
            Assert.AreEqual(true, res);



            res = ImportUtils.IsPersonLine_DAboville("1. Ivan", out pid);
            Assert.AreEqual("1.", pid, "[v2-1]");
            Assert.AreEqual(true, res, "[v2-1]");

            res = ImportUtils.IsPersonLine_DAboville("1.1. Ivan", out pid);
            Assert.AreEqual("1.1.", pid, "[v2-2]");
            Assert.AreEqual(true, res, "[v2-2]");

            res = ImportUtils.IsPersonLine_DAboville("11.21.31.11. Ivan", out pid);
            Assert.AreEqual("11.21.31.11.", pid, "[v2-3]");
            Assert.AreEqual(true, res, "[v2-3]");



            res = ImportUtils.ParsePersonLine_DAboville("1. Ivan", out persId, out parentId, out marNum, out extData, out pos);
            Assert.AreEqual("1.", persId);
            Assert.AreEqual("", parentId);
            Assert.AreEqual("", marNum);
            Assert.AreEqual("", extData);
            Assert.AreEqual(true, res);

            res = ImportUtils.ParsePersonLine_DAboville("11.21.31.11. Ivan", out persId, out parentId, out marNum, out extData, out pos);
            Assert.AreEqual("11.21.31.11.", persId);
            Assert.AreEqual("11.21.31.", parentId);
            Assert.AreEqual("", marNum);
            Assert.AreEqual("", extData);
            Assert.AreEqual(true, res);



            int marrNum;
            string spouse;
            res = ImportUtils.ParseSpouseLine("Ж: Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual("Ж", spouse, "[v3-1]");
            Assert.AreEqual(1, marrNum, "[v3-1]");
            Assert.AreEqual("", extData);
            Assert.AreEqual(true, res, "[v3-1]");

            res = ImportUtils.ParseSpouseLine("Ж2 (test): Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual("Ж", spouse, "[v3-2]");
            Assert.AreEqual(2, marrNum, "[v3-2]");
            Assert.AreEqual("(test)", extData, "[v3-2]");
            Assert.AreEqual(true, res, "[v3-2]");

            res = ImportUtils.ParseSpouseLine("Ж - Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual("Ж", spouse, "[v3-3]");
            Assert.AreEqual(1, marrNum, "[v3-3]");
            Assert.AreEqual("", extData);
            Assert.AreEqual(true, res, "[v3-3]");

            res = ImportUtils.ParseSpouseLine("Ж3 (test2) - Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual("Ж", spouse, "[v3-4]");
            Assert.AreEqual(3, marrNum, "[v3-4]");
            Assert.AreEqual("(test2)", extData, "[v3-4]");
            Assert.AreEqual(true, res, "[v3-4]");
        }

        [Test]
        public void ImpUtils_Invalid_Tests()
        {
            bool res;
            string pid;

            res = ImportUtils.IsPersonLine_Konovalov("-1. Ivan", out pid);
            Assert.AreEqual(false, res);

            res = ImportUtils.IsPersonLine_Konovalov("1 Ivan", out pid);
            Assert.AreEqual(false, res);

            res = ImportUtils.IsPersonLine_Konovalov("1-. Ivan", out pid);
            Assert.AreEqual(false, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-11 Ivan", out pid);
            Assert.AreEqual(false, res);

            //res = ImpUtils.IsPersonLine_Konovalov("1.2. Ivan", out pid); // now false-positive
            //Assert.AreEqual(false, res, "[i1]");

            //res = ImpUtils.IsPersonLine_Konovalov("1.1.1. Ivan", out pid); // now false-positive
            //Assert.AreEqual(false, res, "[i2]");

            res = ImportUtils.IsPersonLine_Konovalov("11-21/. Ivan", out pid);
            Assert.AreEqual(false, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-21-31. Ivan", out pid);
            Assert.AreEqual(false, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-21-31 (. Ivan", out pid);
            Assert.AreEqual(false, res);

            res = ImportUtils.IsPersonLine_Konovalov("11-21-31 (test) Ivan", out pid);
            Assert.AreEqual(false, res);



            res = ImportUtils.IsPersonLine_DAboville("-1. Ivan", out pid);
            Assert.AreEqual(false, res, "[i2-1]");

            res = ImportUtils.IsPersonLine_DAboville("1-1. Ivan", out pid);
            Assert.AreEqual(false, res, "[i2-2]");

            res = ImportUtils.IsPersonLine_DAboville(".1. Ivan", out pid);
            Assert.AreEqual(false, res, "[i2-3]");

            res = ImportUtils.IsPersonLine_DAboville("1710 (80), 1727 (80).", out pid);
            Assert.AreEqual(false, res, "[i2-4]");



            string spouse, extData;
            int marrNum;
            int pos;
            res = ImportUtils.ParseSpouseLine("Жена Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual(false, res, "[i3-1]");

            res = ImportUtils.ParseSpouseLine("Ж2 Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual(false, res, "[i3-2]");

            res = ImportUtils.ParseSpouseLine("Ж Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual(false, res, "[i3-3]");

            res = ImportUtils.ParseSpouseLine("Ж3 (test2 - Ivanova", out spouse, out marrNum, out extData, out pos);
            Assert.AreEqual(false, res, "[i3-4]");
        }

        [Test]
        public void Other_Tests()
        {
            Assert.IsTrue(ImportUtils.IsRomeLine("XIV"));
            Assert.IsFalse(ImportUtils.IsRomeLine("XVA"));
            Assert.IsFalse(ImportUtils.IsRomeLine(""));
        }
    }
}
