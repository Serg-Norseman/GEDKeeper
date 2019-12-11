/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using System.Security.Cryptography;
using System.Text;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public static class SCCrypt
    {
        public static byte[] CreateRandomSalt(int length)
        {
            // Create a buffer
            byte[] randBytes = (length >= 1) ? new byte[length] : new byte[1];

            // Create a new RNGCryptoServiceProvider
            RNGCryptoServiceProvider rand = new RNGCryptoServiceProvider();

            // Fill the buffer with random bytes
            rand.GetBytes(randBytes);

            // return the bytes
            return randBytes;
        }

        public static void ClearBytes(byte[] buffer)
        {
            // Check arguments
            if (buffer == null)
                throw new ArgumentNullException("buffer");

            Array.Clear(buffer, 0, buffer.Length);
        }

        private static byte[] ArrConcat(byte[] L, byte[] R)
        {
            int num = ((L != null) ? L.Length : 0);
            int num2 = ((R != null) ? R.Length : 0);

            byte[] result = new byte[num + num2];
            if (num > 0) Array.Copy(L, 0, result, 0, num);
            if (num2 > 0) Array.Copy(R, 0, result, num, num2);
            return result;
        }

        private static byte[] ArrCopy(byte[] src, int index, int count)
        {
            byte[] result = null;
            if (count > 0)
            {
                int srcLen = ((src != null) ? src.Length : 0);
                index = (index < 0) ? 0 : index;

                if (srcLen > 0 && index < srcLen)
                {
                    if (count > srcLen - index) {
                        count = srcLen - index;
                    }

                    if (count > 0) {
                        result = new byte[count];
                        Array.Copy(src, index, result, 0, count);
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Converts the specified 32-bits integer value to array of bytes. Only
        /// the specified number of bytes is copied to the result array.
        /// Despite the fact that I stuck with the original name of the method
        /// (`MoveL2S`), I really don't know what it means.
        /// </summary>
        /// <param name="source">Source 32-bits value to be converted to array of
        /// bytes.</param>
        /// <param name="count">Number of bytes to convert (starting from the
        /// least signficant byte).</param>
        /// <returns>Array of byte that represents the <parameref name="source">.
        /// Size of the array equals to <paramref name="count" />.</returns>
        public static byte[] MoveL2S(uint source, int count)
        {
            byte[] result = new byte[count];
            for (int it = 0; count > it; ++it)
            {
                result[it] = (byte) (0xFF & (source >> (it << 3)));
            }
            return result;
        }

        private static byte[] Decode(byte[] data)
        {
            byte[] result = null;

            if (data != null)
            {
                uint I;
                switch (data.Length)
                {
                    case 2:
                        I = (uint)(U1_MAP[data[0]] + (U1_MAP[data[1]] << 6));
                        result = MoveL2S(I, 1);
                        break;

                    case 3:
                        I = (uint)(U1_MAP[data[0]] + (U1_MAP[data[1]] << 6) + (U1_MAP[data[2]] << 12));
                        result = MoveL2S(I, 2);
                        break;

                    case 4:
                        I = (uint)(U1_MAP[data[0]] + (U1_MAP[data[1]] << 6) + (U1_MAP[data[2]] << 12) + (U1_MAP[data[3]] << 18));
                        result = MoveL2S(I, 3);
                        break;
                }
            }
            
            return result;
        }

        // <summary>
        // Converts the specified array of bytes to 32-bits signed integer
        // value.
        // Despite the fact that I stuck with the original name of the method
        // (`MoveS2L`), I really don't know what it means.
        // </summary>
        // <param name="source">Source array of bytes to be converted.</param>
        // <returns>32-bits integer signed avlue that represents the
        // <parameref name="source">.</returns>
        public static uint MoveS2L(byte[] source, int count)
        {
            uint result = 0;
            for (int it = 0; count > it; ++it)
            {
                result |= (uint) (source[it] << (it << 3));
            }
            return result;
        }

        private static byte[] Encode(byte[] data)
        {
            int num = (data != null) ? data.Length : 0;
            byte[] res = new byte[num + 1];

            if (num != 0) {
                uint I = MoveS2L(data, num);
                switch (num) {
                    case 1:
                        res[0] = (byte)U2_MAP[I % 64];
                        res[1] = (byte)U2_MAP[(I >> 6) % 64];
                        break;
                    case 2:
                        res[0] = (byte)U2_MAP[I % 64];
                        res[1] = (byte)U2_MAP[(I >> 6) % 64];
                        res[2] = (byte)U2_MAP[(I >> 12) % 64];
                        break;
                    case 3:
                        res[0] = (byte)U2_MAP[I % 64];
                        res[1] = (byte)U2_MAP[(I >> 6) % 64];
                        res[2] = (byte)U2_MAP[(I >> 12) % 64];
                        res[3] = (byte)U2_MAP[(I >> 18) % 64];
                        break;
                }
            }
            
            return res;
        }

        public static string scDecrypt(string str, ushort key)
        {
            string res = "";

            if (!string.IsNullOrEmpty(str))
            {
                byte[] ssd = Encoding.ASCII.GetBytes(str);
                byte[] ppd = null;
                
                int idx = 0;
                while (idx < ssd.Length)
                {
                    byte[] sd = ArrCopy(ssd, idx, 4);
                    ppd = ArrConcat(ppd, Decode(sd));
                    idx += sd.Length;
                }

                if (ppd != null) {
                    byte[] tmp = (byte[])ppd.Clone();

                    uint seed = key;
                    for (int i = 0; i < ppd.Length; i++)
                    {
                        tmp[i] = (byte)(tmp[i] ^ seed >> 8);
                        seed = unchecked((ushort)((ppd[i] + seed) * 28732u + 28446u));
                    }
                    res = Encoding.ASCII.GetString(tmp);
                }
            }

            return res;
        }

        public static string scEncrypt(string str, ushort key)
        {
            string res = "";

            if (!string.IsNullOrEmpty(str))
            {
                uint seed = key;
                byte[] idata = Encoding.ASCII.GetBytes(str);
                for (int i = 0; i < idata.Length; i++)
                {
                    idata[i] = (byte)(idata[i] ^ seed >> 8);
                    seed = unchecked((ushort)((idata[i] + seed) * 28732u + 28446u));
                }

                byte[] resData = null;

                int idx = 0;
                while (idx < idata.Length)
                {
                    byte[] sd = ArrCopy(idata, idx, 3);
                    resData = ArrConcat(resData, Encode(sd));
                    idx += sd.Length;
                }

                res = (resData == null) ? "" : Encoding.ASCII.GetString(resData);
            }

            return res;
        }

        private static readonly byte[] U1_MAP;
        private static readonly char[] U2_MAP;

        static SCCrypt()
        {
            U1_MAP = new byte[256];
            U1_MAP[43] = 62;
            U1_MAP[47] = 63;
            U1_MAP[48] = 52;
            U1_MAP[49] = 53;
            U1_MAP[50] = 54;
            U1_MAP[51] = 55;
            U1_MAP[52] = 56;
            U1_MAP[53] = 57;
            U1_MAP[54] = 58;
            U1_MAP[55] = 59;
            U1_MAP[56] = 60;
            U1_MAP[57] = 61;

            U1_MAP[66] = 1;
            U1_MAP[67] = 2;
            U1_MAP[68] = 3;
            U1_MAP[69] = 4;
            U1_MAP[70] = 5;
            U1_MAP[71] = 6;
            U1_MAP[72] = 7;
            U1_MAP[73] = 8;
            U1_MAP[74] = 9;
            U1_MAP[75] = 10;
            U1_MAP[76] = 11;
            U1_MAP[77] = 12;
            U1_MAP[78] = 13;
            U1_MAP[79] = 14;
            U1_MAP[80] = 15;
            U1_MAP[81] = 16;
            U1_MAP[82] = 17;
            U1_MAP[83] = 18;
            U1_MAP[84] = 19;
            U1_MAP[85] = 20;
            U1_MAP[86] = 21;
            U1_MAP[87] = 22;
            U1_MAP[88] = 23;
            U1_MAP[89] = 24;
            U1_MAP[90] = 25;

            U1_MAP[97] = 26;
            U1_MAP[98] = 27;
            U1_MAP[99] = 28;
            U1_MAP[100] = 29;
            U1_MAP[101] = 30;
            U1_MAP[102] = 31;
            U1_MAP[103] = 32;
            U1_MAP[104] = 33;
            U1_MAP[105] = 34;
            U1_MAP[106] = 35;
            U1_MAP[107] = 36;
            U1_MAP[108] = 37;
            U1_MAP[109] = 38;
            U1_MAP[110] = 39;
            U1_MAP[111] = 40;
            U1_MAP[112] = 41;
            U1_MAP[113] = 42;
            U1_MAP[114] = 43;
            U1_MAP[115] = 44;
            U1_MAP[116] = 45;
            U1_MAP[117] = 46;
            U1_MAP[118] = 47;
            U1_MAP[119] = 48;
            U1_MAP[120] = 49;
            U1_MAP[121] = 50;
            U1_MAP[122] = 51;

            U2_MAP = new char[64];
            for (int i = 0; i < 26; i++)
                U2_MAP[i] = (char)('A' + 0);
            for (int i = 0; i < 26; i++)
                U2_MAP[i + 26] = (char)('a' + 0);
            for (int i = 0; i < 10; i++)
                U2_MAP[i + 52] = (char)('0' + i);
            U2_MAP[62] = '+';
            U2_MAP[63] = '/';            
        }
    }
}
