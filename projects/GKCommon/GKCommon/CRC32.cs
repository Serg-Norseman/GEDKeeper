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

namespace GKCommon
{
    public static class CRC32
    {
        private static readonly uint[] CCITT32_TABLE;

        static CRC32()
        {
            CCITT32_TABLE = new uint[256];
            BuildCRCTable();
        }

        private static void BuildCRCTable()
        {
            unchecked
            {
                uint i = 0u;
                do
                {
                    uint val = i;
                    uint j = 4294967288u;
                    do
                    {
                        if ((val & 1u) != 0u)
                        {
                            val = (val >> 1 ^ 3988292384u);
                        }
                        else
                        {
                            val >>= 1;
                        }
                        j += 1u;
                    }
                    while (j != 0u);
                    CCITT32_TABLE[(int)i] = val;
                    i += 1u;
                }
                while (i != 256u);
            }
        }

        public static uint CrcStr(string str)
        {
            uint crc = 0u;
            if (!string.IsNullOrEmpty(str))
            {
                for (int i = 0; i < str.Length; i++)
                {
                    byte c = (byte)str[i];
                    crc = ((crc >> 8 & 16777215u) ^ CCITT32_TABLE[(int)((crc ^ (uint)c) & 255u)]);
                }
            }
            return crc;
        }
    }
}
