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

using BSLib;

namespace GKCore.Import
{
    /// <summary>
    /// 
    /// </summary>
    public static class ImportUtils
    {
        /// <summary>
        /// Text modules used to refer to events in the pedigree documents.
        /// </summary>
        public const string STD_BIRTH_SIGN = "*";
        public const string STD_ILLEGALLY_BIRTH_SIGN = "(*)";
        public const string STD_DEATH_SIGN = "+";
        public const string STD_BAPTISM_SIGN = "~";
        public const string STD_BURIED_SIGN = "[]";
        public const string STD_KILLED_IN_ACTION_SIGN = "x";
        public const string STD_ENGAGED_SIGN = "o";
        public const string STD_MARRIED_SIGN = "oo";
        public const string STD_UNMARRIED_SIGN = "o-o";
        public const string STD_DIVORCED_SIGN = "o|o";
        public const string STD_DIVORCED2_SIGN = "o/o";


        public const string STD_MALE_SIGN_U = "♂";
        public const string STD_FEMALE_SIGN_U = "♀";
        public const string STD_INTERSEX_SIGN_U = "⚥";
        public const string STD_NEUTER_SIGN_U = "⚲";
        public const string STD_BIRTH_SIGN_U = "*";
        public const string STD_ILLEGALLY_BIRTH_SIGN_U = "⊛";
        public const string STD_DEATH_SIGN_U = "✝︎";
        public const string STD_BAPTISM_SIGN_U = "~";
        public const string STD_BURIED_SIGN_U = "⚰︎";
        public const string STD_CREMATION_SIGN_U = "⚱︎";
        public const string STD_KILLED_IN_ACTION_SIGN_U = "⚔︎";
        public const string STD_ENGAGED_SIGN_U = "⚬";
        public const string STD_MARRIED_SIGN_U = "⚭";
        public const string STD_UNMARRIED_SIGN_U = "⚯";
        public const string STD_DIVORCED_SIGN_U = "⚮";

        private const string ROME_CHARS = "IVXLCDM";

        private static bool IsRomeChar(char c)
        {
            return (ROME_CHARS.IndexOf(c) >= 0);
        }

        public static bool IsRomeLine(string str)
        {
            if (string.IsNullOrEmpty(str)) {
                return false;
            }

            string rs = "";
            int i = 0;
            while (i < str.Length && IsRomeChar(str[i])) {
                rs += str[i];
                i++;
            }
            return (rs != "" && rs == str);
        }

        public static string IsPersonLine_DAboville(string str)
        {
            // "11.21.31.11."

            string pId = "";

            StringTokenizer strTok = new StringTokenizer(str);
            strTok.RecognizeDecimals = false;

            Token token, prev = null;
            do {
                token = strTok.Next();

                if (token.Kind == TokenKind.Symbol && token.Value == ".") {
                    if (prev != null && prev.Kind == TokenKind.Number) {
                        pId += token.Value;
                        prev = token;
                    } else {
                        break;
                    }
                } else if (token.Kind == TokenKind.Number) {
                    pId += token.Value;
                    prev = token;
                } else {
                    break;
                }
            } while (token.Kind != TokenKind.EOF);

            // TODO: testing and transfer to parse!
            if ((prev == null || prev.Kind != TokenKind.Symbol || prev.Value != ".")
                /* || (token.Kind != TokenKind.WhiteSpace || token.Value != " ")*/) {
                return null;
            }

            return pId;
        }

        public static PersonLineRet ParsePersonLine_DAboville(string str)
        {
            // "11.21.31.11."

            string persId = "";
            string parentId = "";
            string marNum = "";
            string extData = "";
            int pos = 0;

            StringTokenizer strTok = new StringTokenizer(str);
            strTok.RecognizeDecimals = false;

            Token token, prev = null;
            do {
                token = strTok.Next();

                if (token.Kind == TokenKind.Symbol && token.Value == ".") {
                    if (prev != null && prev.Kind == TokenKind.Number) {
                        persId += token.Value;
                        prev = token;
                    } else {
                        break;
                    }
                } else if (token.Kind == TokenKind.Number) {
                    persId += token.Value;
                    prev = token;
                } else {
                    break;
                }
            } while (token.Kind != TokenKind.EOF);

            if (token.Kind != TokenKind.WhiteSpace || token.Value != " ") {
                return null;
            }

            int idx = persId.LastIndexOf(".", persId.Length - 2);
            if (idx > 0) {
                parentId = persId.Substring(0, idx + 1);
            }

            pos = strTok.Position;

            return new PersonLineRet(persId, parentId, marNum, extData, pos);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="str"></param>
        /// <param name="pId"></param>
        /// <returns>Personal identifier or empty if line isn't valid Konovalov's line</returns>
        public static string IsPersonLine_Konovalov(string str)
        {
            // "11-21/1 (test+2, test)."

            string pId = "";

            StringTokenizer strTok = new StringTokenizer(str);
            strTok.RecognizeDecimals = false;
            Token token;

            token = strTok.Next();
            if (token.Kind == TokenKind.Number) {
                pId += token.Value;
            } else {
                return null;
            }

            token = strTok.Next();
            if (token.Kind == TokenKind.Symbol && token.Value == "-") {
                pId += token.Value;

                token = strTok.Next();
                if (token.Kind == TokenKind.Number) {
                    pId += token.Value;
                } else {
                    return null;
                }

                token = strTok.Next();
                if (token.Kind == TokenKind.Symbol && token.Value == "/") {
                    pId += token.Value;

                    token = strTok.Next();
                    if (token.Kind == TokenKind.Number || (token.Kind == TokenKind.Symbol && token.Value == "?")) {
                        pId += token.Value;
                    } else {
                        return null;
                    }

                    token = strTok.Next();
                }
            }

            if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
                pId += token.Value;
                token = strTok.Next();
            }

            if (token.Kind == TokenKind.Symbol && token.Value == "(") {
                pId += token.Value;

                do {
                    token = strTok.Next();
                    if (token.Kind == TokenKind.EOL || token.Kind == TokenKind.EOF) {
                        return null;
                    }
                    pId += token.Value;
                } while (token.Kind != TokenKind.Symbol || token.Value != ")");

                token = strTok.Next();
            }

            if (token.Kind == TokenKind.Symbol && token.Value == ".") {
                pId += token.Value;
            } else {
                return null;
            }

            /*token = strTok.Next();
            if (token.Kind != TokenKind.WhiteSpace || token.Value != " ") {
                return false;
            }*/

            return pId;
        }

        public static PersonLineRet ParsePersonLine_Konovalov(string str)
        {
            // "11-21/1 (test+2, test)."

            string persId = "";
            string parentId = "";
            string marNum = "";
            string extData = "";
            int pos = 0;

            StringTokenizer strTok = new StringTokenizer(str);
            strTok.RecognizeDecimals = false;
            Token token;

            token = strTok.Next();
            if (token.Kind == TokenKind.Number) {
                persId = token.Value;
            } else {
                return null;
            }

            token = strTok.Next();
            if (token.Kind == TokenKind.Symbol && token.Value == "-") {
                //p_id += token.Value;

                token = strTok.Next();
                if (token.Kind == TokenKind.Number) {
                    parentId = token.Value;
                } else {
                    return null;
                }

                token = strTok.Next();
                if (token.Kind == TokenKind.Symbol && token.Value == "/") {
                    //p_id += token.Value;

                    token = strTok.Next();
                    if (token.Kind == TokenKind.Number || (token.Kind == TokenKind.Symbol && token.Value == "?")) {
                        marNum = token.Value;
                    } else {
                        return null;
                    }

                    token = strTok.Next();
                }
            }

            if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
                //p_id += token.Value;
                token = strTok.Next();
            }

            if (token.Kind == TokenKind.Symbol && token.Value == "(") {
                extData += token.Value;

                do {
                    token = strTok.Next();
                    if (token.Kind == TokenKind.EOL || token.Kind == TokenKind.EOF) {
                        return null;
                    }
                    extData += token.Value;
                } while (token.Kind != TokenKind.Symbol || token.Value != ")");

                token = strTok.Next();
            }

            if (token.Kind == TokenKind.Symbol && token.Value == ".") {
                //p_id += token.Value;
            } else {
                return null;
            }

            /*token = strTok.Next();
            if (token.Kind != TokenKind.WhiteSpace || token.Value != " ") {
                return false;
            }*/

            pos = strTok.Position;

            return new PersonLineRet(persId, parentId, marNum, extData, pos);
        }

        public sealed class PersonLineRet
        {
            public string PersId;
            public string ParentId;
            public string MarNum;
            public string ExtData;
            public int Pos;

            internal PersonLineRet(string persId, string parentId, string marNum, string extData, int pos)
            {
                PersId = persId;
                ParentId = parentId;
                MarNum = marNum;
                ExtData = extData;
                Pos = pos;
            }
        }

        ////////////////////

        public sealed class SpouseLineRet
        {
            public string Spouse;
            public int MarrNum;
            public string ExtData;
            public int Pos;

            internal SpouseLineRet(string spouse, int marrNum, string extData, int pos)
            {
                Spouse = spouse;
                MarrNum = marrNum;
                ExtData = extData;
                Pos = pos;
            }
        }

        public static SpouseLineRet ParseSpouseLine(string str)
        {
            // "М/Ж[N]{": " | " - "}name"

            string spouse = "";
            int marrNum = 1;
            string extData = "";
            int pos = 0;

            StringTokenizer strTok = new StringTokenizer(str);
            strTok.IgnoreWhiteSpace = false;
            strTok.RecognizeDecimals = false;

            Token token;

            token = strTok.Next();
            if (token.Kind == TokenKind.Word && (token.Value == "М" || token.Value == "Ж")) {
                spouse = token.Value;
                token = strTok.Next();
            } else {
                return null;
            }

            if (token.Kind == TokenKind.Number) {
                marrNum = int.Parse(token.Value);
                token = strTok.Next();
            } else {
                marrNum = 1;
            }

            if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
                token = strTok.Next();
            }

            if (token.Kind == TokenKind.Symbol && token.Value == "(") {
                extData += token.Value;

                do {
                    token = strTok.Next();
                    if (token.Kind == TokenKind.EOL || token.Kind == TokenKind.EOF) {
                        return null;
                    }
                    extData += token.Value;
                } while (token.Kind != TokenKind.Symbol || token.Value != ")");

                token = strTok.Next();
            }

            if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
                token = strTok.Next();
            }

            if (token.Kind == TokenKind.Symbol && (token.Value == ":" || token.Value == "-")) {
                token = strTok.Next();
            } else {
                return null;
            }

            pos = strTok.Position;

            return new SpouseLineRet(spouse, marrNum, extData, pos);
        }
    }
}
