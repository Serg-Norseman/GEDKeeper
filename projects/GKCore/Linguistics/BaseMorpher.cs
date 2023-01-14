/*
 *  "BSLib.Linguistics".
 *  Copyright (C) 2009 by Sergey V. Zhdanovskih.
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

namespace BSLib.Linguistics.Grammar
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
    public abstract class BaseMorpher
    {
        private static string[][] TranslitTable;

        static BaseMorpher()
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

        private static int FindSymbol(int[] order, int s, string check, int startIndex)
        {
            int idx = -1;
            for (int i = 0; i < TranslitTable.Length; i++) {
                int k = order[i];
                if (check.IndexOf(TranslitTable[k][s], startIndex, StringComparison.InvariantCultureIgnoreCase) == startIndex) {
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

            int i = 0;
            while (i < str.Length) {
                bool isUpper = Char.IsUpper(str[i]);

                int dCnt;
                int idx = FindSymbol(order, (int)s, str, i);
                if (idx > -1) {
                    string[] row = TranslitTable[idx];
                    dCnt = row[(int)s].Length;
                    string tgt = row[(int)t];

                    bool nextLower = (i + 1 < str.Length) && Char.IsLower(str[i + 1]);

                    if (!isUpper) {
                        tgt = tgt.ToLower();
                    } else {
                        if (tgt.Length > 1 && nextLower) {
                            tgt = /*Char.ToUpper*/(tgt[0]) + tgt.Substring(1).ToLower();
                        } else {
                            //tgt = tgt.ToUpper();
                        }
                    }

                    result = result + tgt;
                } else {
                    dCnt = 1;
                    result = result + str[i];
                }
                i += dCnt;
            }

            return result;
        }

        private static string[] hundrs = new string[] {
            "", "сто ", "двести ", "триста ", "четыреста ", "пятьсот ",
            "шестьсот ", "семьсот ", "восемьсот ", "девятьсот "
        };

        // 2..9
        private static string[] tens = new string[] {
            "двадцать ", "тридцать ", "сорок ", "пятьдесят ",
            "шестьдесят ", "семьдесят ", "восемьдесят ", "девяносто "
        };

        private static string[] ones = new string[] {
            "", "один ", "два ", "три ", "четыре ", "пять ", "шесть ",
            "семь ", "восемь ", "девять ", "десять ", "одиннадцать ",
            "двенадцать ", "тринадцать ", "четырнадцать ", "пятнадцать ",
            "шестнадцать ", "семнадцать ", "восемнадцать ", "девятнадцать "
        };

        // 0..1, 1..2
        private static string[][] onetwo = {
            new string[] {"один ", "два "},
            new string[] {"одна ", "две "}
        };

        // 0..3, 1..5
        private static string[][] abbrs = {
            new string[] {"миллиарда ", "миллиард ", "миллиардов ", "млрд. ", ""},
            new string[] {"миллиона ", "миллион ", "миллионов ", "млн. ", ""},
            new string[] {"тысячи ", "тысяча ", "тысяч ", "тыс. ", ""},
            new string[] {"", "", "", "", ""}
        };

        // Получить строчное написание числа
        public static string SpellNumber(int x)
        {
            string S, N;
            int i, j, z, x1, x2, x3;

            S = "";
            if (x < 0) {
                x = -x;
                S = "минус ";
            }

            N = x.ToString("000000000000");
            i = 1;

            /*while (N[i] == " ") {
                N[i] = '0';
                i++;
            }*/

            for (j = 0; j <= 3; j++) {
                ones[1] = onetwo[(j >= 2 ? 1 : 0)][0];
                ones[2] = onetwo[(j >= 2 ? 1 : 0)][1];

                z = j * 3; //  + 1
                x1 = (int)(N[z  ]) - 48;
                x2 = (int)(N[z+1]) - 48;
                x3 = (int)(N[z+2]) - 48;

                if (x1 + x2 + x3 == 0) {
                    if ((z == 10) && (N == "000000000000")) {
                        S = S + "ноль ";
                    }

                    i = 5;
                } else {
                    S = S + hundrs[x1];

                    if (x2 < 2) {
                        x3 += (10 * x2);
                    } else S = S + tens[x2 - 2];

                    S = S + ones[x3];

                    if ((x3 > 4) || (x3 == 0)) {
                        i = 3;
                    } else i = 1 + (x3 == 1 ? 1 : 0);
                }

                S = S + abbrs[j][i - 1];
            }

            S = S.Trim();

            return S;
        }
    }
}
