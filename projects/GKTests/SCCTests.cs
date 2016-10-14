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
using GKCore;
using NUnit.Framework;

namespace GKTests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class SCCTests
    {
        [Test]
        public void SCCrypt_Tests()
        {
            uint source = 0xabbccdd;
            byte[] array = MoveL2S_old(source, 4);
            byte[] arrayNew = SCCrypt.MoveL2S(source, 4);
            Assert.AreEqual(array, arrayNew, "test #1");

            source = 0xbadf00d;
            array = MoveL2S_old(source, 4);
            arrayNew = SCCrypt.MoveL2S(source, 4);
            Assert.AreEqual(array, arrayNew, "test #2");

            source = 0x1020304;
            array = MoveL2S_old(source, 2);
            arrayNew = SCCrypt.MoveL2S(source, 2);
            Assert.AreEqual(array, arrayNew, "test #3");

            source = 0xff00ff00;
            array = MoveL2S_old(source, 1);
            arrayNew = SCCrypt.MoveL2S(source, 1);
            Assert.AreEqual(array, arrayNew, "test #4");

            array = new byte[] {0x0d, 0xf0, 0xad, 0xb};
            source = MoveS2L_old(array, 4);
            uint sourceNew = SCCrypt.MoveS2L(array, 4);
            Assert.AreEqual(source, sourceNew, "test #5");

            array = new byte[] { 0x1, 0x2, 0x3, 0x4 };
            source = MoveS2L_old(array, 3);
            sourceNew = SCCrypt.MoveS2L(array, 3);
            Assert.AreEqual(source, sourceNew, "test #6");
        }

        // The original `GKCore::SCCrypt::MoveL2S` method
        private static byte[] MoveL2S_old(uint source, int count)
        {
            byte[] dest = new byte[count];
            unchecked
            {
                ushort wl = (ushort)(source);
                ushort wh = (ushort)(source >> 16);

                if (count >= 1) dest[0] = (byte)wl;
                if (count >= 2) dest[1] = (byte)(wl >> 8);
                if (count >= 3) dest[2] = (byte)wh;
                if (count >= 4) dest[3] = (byte)(wh >> 8);
            }

            return dest;
        }

        // The original `GKCore::SCCrypt::MoveS2L` method
        private static uint MoveS2L_old(byte[] source, int count)
        {
            byte[] bytes = new byte[4];
            bytes[0] = (byte)((count >= 1) ? source[0] : 0);
            bytes[1] = (byte)((count >= 2) ? source[1] : 0);
            bytes[2] = (byte)((count >= 3) ? source[2] : 0);
            bytes[3] = (byte)((count >= 4) ? source[3] : 0);

            uint dest;
            dest = (uint)((bytes[0] | bytes[1] << 8) | (bytes[2] | bytes[3] << 8) << 16);
            return dest;
        }
    }
}
