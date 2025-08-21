/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2016 by Ruslan Garipov.
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
using BSLib;
using NUnit.Framework;

namespace GKCore.Utilities
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class SCCTests
    {
        [Test]
        public void Test_Complex()
        {
            const string pw = "test password";
            string crypt = SCCrypt.scEncrypt(pw, unchecked((ushort)CRC32.CrcStr("test")));
            string pw1 = SCCrypt.scDecrypt(crypt, unchecked((ushort)CRC32.CrcStr("test")));

            Assert.AreEqual(pw, pw1, "SCCrypt_Test");

            byte[] salt = SCCrypt.CreateRandomSalt(24);
            Assert.IsNotNull(salt);
            Assert.AreEqual(24, salt.Length);

            SCCrypt.ClearBytes(salt);
            Assert.Throws(typeof(ArgumentNullException), () => { SCCrypt.ClearBytes(null); });
        }

        [Test]
        public void Test_Common()
        {
            uint source = 0xabbccdd;
            byte[] arrayNew = SCCrypt.MoveL2S(source, 4);
            Assert.AreEqual(new byte[] { 221, 204, 187, 10 }, arrayNew, "test #1");

            source = 0xbadf00d;
            arrayNew = SCCrypt.MoveL2S(source, 4);
            Assert.AreEqual(new byte[] { 13, 240, 173, 11 }, arrayNew, "test #2");

            source = 0x1020304;
            arrayNew = SCCrypt.MoveL2S(source, 2);
            Assert.AreEqual(new byte[] { 4, 3 }, arrayNew, "test #3");

            source = 0xff00ff00;
            arrayNew = SCCrypt.MoveL2S(source, 1);
            Assert.AreEqual(new byte[] { 0 }, arrayNew, "test #4");

            byte[] array = new byte[] {0x0d, 0xf0, 0xad, 0xb};
            uint sourceNew = SCCrypt.MoveS2L(array, 4);
            Assert.AreEqual(195948557, sourceNew, "test #5");

            array = new byte[] { 0x1, 0x2, 0x3, 0x4 };
            sourceNew = SCCrypt.MoveS2L(array, 3);
            Assert.AreEqual(197121, sourceNew, "test #6");
        }
    }
}
