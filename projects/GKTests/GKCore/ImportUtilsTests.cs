/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using NUnit.Framework;

namespace GKCore.Import
{
    [TestFixture]
    public class ImportUtilsTests
    {
        [Test]
        public void Test_ValidCases()
        {
            string pid;

            pid = ImportUtils.IsPersonLine_Konovalov("1. Ivan");
            Assert.AreEqual("1.", pid, "[v1]");

            pid = ImportUtils.IsPersonLine_Konovalov("2-1. Ivan");
            Assert.AreEqual("2-1.", pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-21. Ivan");
            Assert.AreEqual("11-21.", pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-21/1. Ivan");
            Assert.AreEqual("11-21/1.", pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-21/?. Ivan");
            Assert.AreEqual("11-21/?.", pid, "[v2]");

            pid = ImportUtils.IsPersonLine_Konovalov("11-21/1 (test+2, test). Ivan");
            Assert.AreEqual("11-21/1 (test+2, test).", pid);



            var plRet = ImportUtils.ParsePersonLine_Konovalov("11-21/1 (test+2, test). Ivan");
            Assert.AreEqual("11", plRet.PersId);
            Assert.AreEqual("21", plRet.ParentId);
            Assert.AreEqual("1", plRet.MarNum);
            Assert.AreEqual("(test+2, test)", plRet.ExtData);
            Assert.IsNotNull(plRet);

            plRet = ImportUtils.ParsePersonLine_Konovalov("11-21/?. Ivan");
            Assert.AreEqual("11", plRet.PersId);
            Assert.AreEqual("21", plRet.ParentId);
            Assert.AreEqual("?", plRet.MarNum);
            Assert.AreEqual("", plRet.ExtData);
            Assert.IsNotNull(plRet);



            pid = ImportUtils.IsPersonLine_DAboville("1. Ivan");
            Assert.AreEqual("1.", pid, "[v2-1]");

            pid = ImportUtils.IsPersonLine_DAboville("1.1. Ivan");
            Assert.AreEqual("1.1.", pid, "[v2-2]");

            pid = ImportUtils.IsPersonLine_DAboville("11.21.31.11. Ivan");
            Assert.AreEqual("11.21.31.11.", pid, "[v2-3]");



            plRet = ImportUtils.ParsePersonLine_DAboville("1. Ivan");
            Assert.AreEqual("1.", plRet.PersId);
            Assert.AreEqual("", plRet.ParentId);
            Assert.AreEqual("", plRet.MarNum);
            Assert.AreEqual("", plRet.ExtData);
            Assert.IsNotNull(plRet);

            plRet = ImportUtils.ParsePersonLine_DAboville("11.21.31.11. Ivan");
            Assert.AreEqual("11.21.31.11.", plRet.PersId);
            Assert.AreEqual("11.21.31.", plRet.ParentId);
            Assert.AreEqual("", plRet.MarNum);
            Assert.AreEqual("", plRet.ExtData);
            Assert.IsNotNull(plRet);



            var slRet = ImportUtils.ParseSpouseLine("Ж: Ivanova");
            Assert.AreEqual("Ж", slRet.Spouse, "[v3-1]");
            Assert.AreEqual(1, slRet.MarrNum, "[v3-1]");
            Assert.AreEqual("", slRet.ExtData);
            Assert.IsNotNull(slRet, "[v3-1]");

            slRet = ImportUtils.ParseSpouseLine("Ж2 (test): Ivanova");
            Assert.AreEqual("Ж", slRet.Spouse, "[v3-2]");
            Assert.AreEqual(2, slRet.MarrNum, "[v3-2]");
            Assert.AreEqual("(test)", slRet.ExtData, "[v3-2]");
            Assert.IsNotNull(slRet, "[v3-2]");

            slRet = ImportUtils.ParseSpouseLine("Ж - Ivanova");
            Assert.AreEqual("Ж", slRet.Spouse, "[v3-3]");
            Assert.AreEqual(1, slRet.MarrNum, "[v3-3]");
            Assert.AreEqual("", slRet.ExtData);
            Assert.IsNotNull(slRet, "[v3-3]");

            slRet = ImportUtils.ParseSpouseLine("Ж3 (test2) - Ivanova");
            Assert.AreEqual("Ж", slRet.Spouse, "[v3-4]");
            Assert.AreEqual(3, slRet.MarrNum, "[v3-4]");
            Assert.AreEqual("(test2)", slRet.ExtData, "[v3-4]");
            Assert.IsNotNull(slRet, "[v3-4]");
        }

        [Test]
        public void Test_InvalidCases()
        {
            string pid;

            pid = ImportUtils.IsPersonLine_Konovalov("-1. Ivan");
            Assert.AreEqual(null, pid);

            pid = ImportUtils.IsPersonLine_Konovalov("1 Ivan");
            Assert.AreEqual(null, pid);

            pid = ImportUtils.IsPersonLine_Konovalov("1-. Ivan");
            Assert.AreEqual(null, pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-11 Ivan");
            Assert.AreEqual(null, pid);

            //res = ImpUtils.IsPersonLine_Konovalov("1.2. Ivan", out pid); // now false-positive
            //Assert.AreEqual(null, pid, "[i1]");

            //res = ImpUtils.IsPersonLine_Konovalov("1.1.1. Ivan", out pid); // now false-positive
            //Assert.AreEqual(null, pid, "[i2]");

            pid = ImportUtils.IsPersonLine_Konovalov("11-21/. Ivan");
            Assert.AreEqual(null, pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-21-31. Ivan");
            Assert.AreEqual(null, pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-21-31 (. Ivan");
            Assert.AreEqual(null, pid);

            pid = ImportUtils.IsPersonLine_Konovalov("11-21-31 (test) Ivan");
            Assert.AreEqual(null, pid);



            pid = ImportUtils.IsPersonLine_DAboville("-1. Ivan");
            Assert.AreEqual(null, pid, "[i2-1]");

            pid = ImportUtils.IsPersonLine_DAboville("1-1. Ivan");
            Assert.AreEqual(null, pid, "[i2-2]");

            pid = ImportUtils.IsPersonLine_DAboville(".1. Ivan");
            Assert.AreEqual(null, pid, "[i2-3]");

            pid = ImportUtils.IsPersonLine_DAboville("1710 (80), 1727 (80).");
            Assert.AreEqual(null, pid, "[i2-4]");



            var slRet = ImportUtils.ParseSpouseLine("Жена Ivanova");
            Assert.IsNull(slRet, "[i3-1]");

            slRet = ImportUtils.ParseSpouseLine("Ж2 Ivanova");
            Assert.IsNull(slRet, "[i3-2]");

            slRet = ImportUtils.ParseSpouseLine("Ж Ivanova");
            Assert.IsNull(slRet, "[i3-3]");

            slRet = ImportUtils.ParseSpouseLine("Ж3 (test2 - Ivanova");
            Assert.IsNull(slRet, "[i3-4]");
        }

        [Test]
        public void Test_Other()
        {
            Assert.IsTrue(ImportUtils.IsRomeLine("XIV"));
            Assert.IsFalse(ImportUtils.IsRomeLine("XVA"));
            Assert.IsFalse(ImportUtils.IsRomeLine(""));
        }
    }
}
