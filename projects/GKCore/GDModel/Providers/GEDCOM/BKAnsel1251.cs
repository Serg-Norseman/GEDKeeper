/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections;
using System.Collections.Generic;
using GKCore;

namespace GDModel.Providers.GEDCOM
{
    /// <summary>
    /// A special decoder for old Cyrillic files from Brother's Keeper,
    /// where the ANSEL and Win1251 encodings are combined into one.
    /// </summary>
    public static class BKAnsel1251
    {
        private class ByteArrayComparer : IEqualityComparer<byte[]>
        {
            public bool Equals(byte[] x, byte[] y)
            {
                return StructuralComparisons.StructuralEqualityComparer.Equals(x, y);
            }

            public int GetHashCode(byte[] obj)
            {
                return StructuralComparisons.StructuralEqualityComparer.GetHashCode(obj);
            }
        }

        private static readonly Dictionary<byte[], byte> Mapping = new Dictionary<byte[], byte>(new ByteArrayComparer())
        {
            { new byte[] { 0xC0 }, 0xC0 },          /* А+ */
            { new byte[] { 0xC1 }, 0xC1 },          /* Б+ */
            { new byte[] { 0xC2 }, 0xC2 },          /* В+ */
            { new byte[] { 0xC3 }, 0xC3 },          /* Г+ */
            { new byte[] { 0xE8, 0x41 }, 0xC4 },    /* Д+ */
            { new byte[] { 0xEA, 0x41 }, 0xC5 },    /* Е+ */
            { new byte[] { 0xA8 }, 0xA8 },          /* Ё+ */
            { new byte[] { 0xA5 }, 0xC6 },          /* Ж+ */
            { new byte[] { 0xF0, 0x43 }, 0xC7 },    /* З+ */
            { new byte[] { 0xC8 }, 0xC8 },          /* И+ */
            { new byte[] { 0xE2, 0x45 }, 0xC9 },    /* Й+ */
            { new byte[] { 0xCA }, 0xCA },          /* К+ */
            { new byte[] { 0xCB }, 0xCB },          /* Л+ */
            { new byte[] { 0xCC }, 0xCC },          /* М+ */
            { new byte[] { 0xCD }, 0xCD },          /* Н+ */
            { new byte[] { 0xCE }, 0xCE },          /* О+ */
            { new byte[] { 0xCF }, 0xCF },          /* П+ */
            { new byte[] { 0xA3 }, 0xD0 },          /* Р+ */
            { new byte[] { 0xE4, 0x4E }, 0xD1 },    /* С+ */
            { new byte[] { 0xD2 }, 0xD2 },          /* Т+ */
            { new byte[] { 0xD3 }, 0xD3 },          /* У+ */
            { new byte[] { 0xD4 }, 0xD4 },          /* Ф+ */
            { new byte[] { 0xD5 }, 0xD5 },          /* Х+ */
            { new byte[] { 0xE8, 0x4F }, 0xD6 },    /* Ц+ */
            { new byte[] { 0xD7 }, 0xD7 },          /* Ч+ */
            { new byte[] { 0xA2 }, 0xD8 },          /* Ш+ */
            { new byte[] { 0xD9 }, 0xD9 },          /* Щ+ */
            { new byte[] { 0xDA }, 0xDA },          /* Ь+ */
            { new byte[] { 0xDB }, 0xDB },          /* Ы+ */
            { new byte[] { 0xE8, 0x55 }, 0xDC },    /* Ъ+ */
            { new byte[] { 0xDD }, 0xDD },          /* Э+ */
            { new byte[] { 0xB4 }, 0xDE },          /* Ю+ */
            //{ new byte[] { 0xCF }, 0xDF },        /* П/Я~ */

            { new byte[] { 0xE1, 0x61 }, 0xE0 },    /* а+ */
            { new byte[] { 0xE2, 0x61 }, 0xE1 },    /* б+ */
            { new byte[] { 0xE3, 0x61 }, 0xE2 },    /* в+ */
            { new byte[] { 0xE4, 0x61 }, 0xE3 },    /* г+ */
            { new byte[] { 0xE8, 0x61 }, 0xE4 },    /* д+ */
            { new byte[] { 0xEA, 0x61 }, 0xE5 },    /* е+ */
            { new byte[] { 0xB8 }, 0xB8 },          /* ё+ */
            { new byte[] { 0xB5 }, 0xE6 },          /* ж+ */
            { new byte[] { 0xF0, 0x63 }, 0xE7 },    /* з+ */
            { new byte[] { 0xE1, 0x65 }, 0xE8 },    /* и+ */
            { new byte[] { 0xE2, 0x65 }, 0xE9 },    /* й+ */
            { new byte[] { 0xE3, 0x65 }, 0xEA },    /* к+ */
            { new byte[] { 0xE8, 0x65 }, 0xEB },    /* л+ */
            { new byte[] { 0xE1, 0x69 }, 0xEC },    /* м+ */
            { new byte[] { 0xE2, 0x69 }, 0xED },    /* н+ */
            { new byte[] { 0xE3, 0x69 }, 0xEE },    /* о+ */
            { new byte[] { 0xE8, 0x69 }, 0xEF },    /* п+ */
            { new byte[] { 0xBA }, 0xF0 },          /* р+ */
            { new byte[] { 0xE4, 0x6E }, 0xF1 },    /* с+ */
            { new byte[] { 0xE1, 0x6F }, 0xF2 },    /* т+ */
            { new byte[] { 0xE2, 0x6F }, 0xF3 },    /* у+ */
            { new byte[] { 0xE3, 0x6F }, 0xF4 },    /* ф+ */
            { new byte[] { 0xE4, 0x6F }, 0xF5 },    /* х+ */
            { new byte[] { 0xE8, 0x6F }, 0xF6 },    /* ц+ */
            { new byte[] { 0xF7, 0xB2 }, 0xF7 },    /* ч+ */

            //{ new byte[] { 0xE1, 0x75 }, 0xF8 },    /* ш+ */
            { new byte[] { 0xB2 }, 0xF8 },    /* ш+ */

            //{ new byte[] { 0xE8, 0x75 }, 0xF9 },    /* щ */
            //{ new byte[] { 0xE2, 0x75 }, 0xFA },  /* ъ [-] */ 

            { new byte[] { 0xE3, 0x75 }, 0xFB },    /* ы+ */
            { new byte[] { 0xE8, 0x75 }, 0xFC },    /* ь+ */
            { new byte[] { 0xFD }, 0xFD },          /* э+ */
            { new byte[] { 0xA4 }, 0xFE },          /* ю+ */
            { new byte[] { 0xFF }, 0xFF }           /* я+ */
        };

        public static byte[] Decode(byte[] input, int lineLen)
        {
            try {
                var result = new List<byte>();

                for (int i = 0; i < lineLen; i++) {
                    // Check for a two-byte sequence
                    if (i + 1 < lineLen) {
                        byte[] duo = new byte[] { input[i], input[i + 1] };
                        if (Mapping.TryGetValue(duo, out byte dVal)) {
                            result.Add(dVal);
                            i++;
                            continue;
                        }
                    }

                    // Check for a single-byte sequence
                    byte[] solo = new byte[] { input[i] };
                    if (Mapping.TryGetValue(solo, out byte sVal))
                        result.Add(sVal);
                    else
                        result.Add(input[i]); // Leave it as is, unless it's in the mapping
                }

                return result.ToArray();
            } catch (Exception ex) {
                Logger.WriteError("BKAnsel1251.Decode()", ex);
                throw ex;
            }
        }
    }
}
