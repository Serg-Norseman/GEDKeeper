/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009 by Sergey V. Zhdanovskih.
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

namespace Externals.Linguistics
{
    public enum TranslitScheme
    {
        // Russian
        ts_Russian,
        // Library of Congress
        ts_LibCongress,
        // Pokrovsky EuroTex-92
        ts_Pokrovsky,
        // Volapuyk
        ts_Volapuyk,
        // GOST 16876-71
        ts_GOST,
        // Simplified
        ts_Simplified
    }

    /// <summary>
    /// 
    /// </summary>
    public static class Translit
    {
        public static string[][] TranslitTable;

        static Translit()
        {
            string[][] res = {
                new string[] { "А",    "A",  "A",  "A",  "A",   "A" },
                new string[] { "Б",    "B",  "B",  "B",  "B",   "B" },
                new string[] { "В",    "V",  "V",  "V",  "V",   "V" },
                new string[] { "Г",    "G",  "G",  "G",  "G",   "G" },
                new string[] { "Д",    "D",  "D",  "D",  "D",   "D" },
                new string[] { "Е",    "E",  "E",  "E",  "E",   "E" },
                new string[] { "Ё",   "JO", "JO", "Y`", "JO",  "JO" },
                new string[] { "Ж",   "ZH", "ZH",  "J", "ZH",  "ZH" },
                new string[] { "З",    "Z",  "Z",  "Z",  "Z",   "Z" },
                new string[] { "И",    "I",  "I",  "I",  "I",   "I" },
                new string[] { "Й",    "J", "JI", "I`", "JJ",   "J" },
                new string[] { "К",    "K",  "K",  "K",  "K",   "K" },
                new string[] { "Л",    "L",  "L",  "L",  "L",   "L" },
                new string[] { "М",    "M",  "M",  "M",  "M",   "M" },
                new string[] { "Н",    "N",  "N",  "N",  "N",   "N" },
                new string[] { "О",    "O",  "O",  "O",  "O",   "O" },
                new string[] { "П",    "P",  "P",  "P",  "P",   "P" },
                new string[] { "Р",    "R",  "R",  "R",  "R",   "R" },
                new string[] { "С",    "S",  "S",  "S",  "S",   "S" },
                new string[] { "Т",    "T",  "T",  "T",  "T",   "T" },
                new string[] { "У",    "U",  "U",  "U",  "U",   "U" },
                new string[] { "Ф",    "F",  "F",  "F",  "F",   "F" },
                new string[] { "Х",   "KH", "KH",  "H", "KH",   "X" },
                new string[] { "Ц",    "C",  "C",  "C",  "C",   "C" },
                new string[] { "Ч",   "CH", "CH", "C`", "CH",  "CH" },
                new string[] { "Ш",   "SH", "SH", "S`", "SH",  "SH" },
                new string[] { "Щ", "SHCH",  "W", "H`", "HH", "SCH" },
                new string[] { "Ъ",    "\"",  "X", "X`", "``",   "`" },
                new string[] { "Ы",    "Y",  "Y",  "Y",  "Y",   "Y" },
                new string[] { "Ь",   "'",  "Q",  "X",  "`",  "'" },
                new string[] { "Э",   "EH", "EH", "E`", "EH",   "E" },
                new string[] { "Ю",   "JU", "JU", "U`", "JU",  "YU" },
                new string[] { "Я",   "JA", "JA", "A`", "JA",  "YA" }
            };

            TranslitTable = res;
        }

        private static void PrepareOrder(int[] order, int sch)
        {
            int tbLen = TranslitTable.Length;
            for (int i = 0; i < tbLen; i++) order[i] = i;

            for (int i = 0; i < tbLen; i++) {
                for (int k = i + 1; k < tbLen; k++) {
                    if (TranslitTable[k][sch].Length > TranslitTable[i][sch].Length) {
                        int t = order[i];
                        order[i] = order[k];
                        order[k] = t;
                    }
                }
            }
        }

        private static int FindSymbol(int[] order, int s, string check)
        {
            int idx = -1;
            for (int i = 0; i < TranslitTable.Length; i++) {
                int k = order[i];
                if (check.IndexOf(TranslitTable[k][s]) == 0) {
                    idx = k;
                    break;
                }
            }
            return idx;
        }

        public static string Transliterate(TranslitScheme s, TranslitScheme t, string str)
        {
            string result = "";

            int[] order = new int[TranslitTable.Length];
            PrepareOrder(order, (int)s);

            string tmp = str.ToUpper();
            while (tmp != "") {
                bool isUpper = Char.IsUpper(str[0]);

                int dCnt;
                int idx = FindSymbol(order, (int)s, tmp);
                if (idx > -1) {
                    string[] row = TranslitTable[idx];
                    dCnt = row[(int)s].Length;
                    string tgt = row[(int)t];

                    bool nextLower = (str.Length > 1) && Char.IsLower(str[1]);

                    if (!isUpper) {
                        tgt = tgt.ToLower();
                    } else {
                        if (tgt.Length > 1 && nextLower) {
                            tgt = Char.ToUpper(tgt[0]) + tgt.Substring(1).ToLower();
                        } else {
                            tgt = tgt.ToUpper();
                        }
                    }

                    result = result + tgt;
                } else {
                    dCnt = 1;
                    result = result + tmp[0];
                }

                tmp = tmp.Remove(0, dCnt);
                str = str.Remove(0, dCnt);
            }

            return result;
        }
    }
}
