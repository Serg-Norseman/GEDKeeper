/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.Text;

namespace GKCommon
{
    public static class CRC32
    {
        private const uint DefaultPolynomial = 0xedb88320u;
        private static readonly uint[] CCITT32_TABLE;

        static CRC32()
        {
            CCITT32_TABLE = new uint[256];

            for (uint i = 0; i < 256; i++)
            {
                uint val = i;

                for (uint j = 0; j < 8; j++)
                    if ((val & 1) == 1)
                        val = (val >> 1) ^ DefaultPolynomial;
                    else
                        val = val >> 1;

                CCITT32_TABLE[i] = val;
            }
        }

        public static uint CrcBytes(byte[] data)
        {
            uint crc = 0u;
            if (data != null && data.Length != 0)
            {
                for (int i = 0; i < data.Length; i++)
                {
                    byte c = data[i];
                    crc = ((crc >> 8 & 16777215u) ^ CCITT32_TABLE[(int)((crc ^ (uint)c) & 255u)]);
                }
            }
            return crc;
        }

        public static uint CrcStr(string str)
        {
            uint crc = 0u;
            if (!string.IsNullOrEmpty(str))
            {
                byte[] data = Encoding.Unicode.GetBytes(str);
                crc = CrcBytes(data);
            }
            return crc;
        }
    }
}
