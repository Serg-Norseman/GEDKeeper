/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

namespace GDModel.Providers.GEDCOM
{
    public enum GEDCOMToken
    {
        Unknown,
        Whitespace,
        Symbol,
        Word,
        Number,
        XRef,
        EOL
    }

    /// <summary>
    /// GEDCOMParser tokenized string into tokens.
    /// </summary>
    public sealed class GEDCOMParser
    {
        private const char EOL = (char)0;

        private GEDCOMToken fCurrentToken;
        private char[] fData;
        private bool fIgnoreWhitespace;
        private int fLength;
        private int fPos;
        private int fSavePos;
        private int fTokenEnd;

        private int fIntValue;
        private string fStrValue;
        private bool fValueReset;


        public GEDCOMToken CurrentToken
        {
            get { return fCurrentToken; }
        }

        public char[] Data
        {
            get { return fData; }
        }

        public int Length
        {
            get { return fLength; }
        }

        public int Position
        {
            get { return fPos; }
        }


        public GEDCOMParser(bool ignoreWhitespace)
        {
            fIgnoreWhitespace = ignoreWhitespace;
        }

        public GEDCOMParser(string data, bool ignoreWhitespace)
        {
            if (data == null)
                throw new ArgumentNullException("data");

            fIgnoreWhitespace = ignoreWhitespace;

            Reset(data.ToCharArray(), 0, data.Length);
        }

        public GEDCOMParser(char[] data, int startIndex, int length, bool ignoreWhitespace)
        {
            if (data == null)
                throw new ArgumentNullException("data");

            fIgnoreWhitespace = ignoreWhitespace;

            Reset(data, startIndex, length);
        }

        public void Reset(char[] data, int startIndex, int length)
        {
            fData = data;
            fLength = length;

            fCurrentToken = GEDCOMToken.Unknown;
            fPos = startIndex;
            fValueReset = false;
        }

        public GEDCOMToken Next()
        {
            while (true) {
                char ch = (fPos >= fLength) ? EOL : fData[fPos];
                char ltr = (char)(ch | ' ');

                if ((ltr >= 'a' && ltr <= 'z') || ch == '_') {
                    fSavePos = fPos;
                    fPos++;
                    while (true) {
                        ch = (fPos >= fLength) ? EOL : fData[fPos];
                        ltr = (char)(ch | ' ');
                        if ((ltr >= 'a' && ltr <= 'z') || (ch >= '0' && ch <= '9') || ch == '_') {
                            fPos++;
                        } else
                            break;
                    }

                    fTokenEnd = fPos;
                    fValueReset = true;
                    fCurrentToken = GEDCOMToken.Word;
                    return fCurrentToken;
                }

                if (ch >= '0' && ch <= '9') {
                    fSavePos = fPos;
                    fPos++;
                    fIntValue = ((int)ch - 48);
                    while (true) {
                        ch = (fPos >= fLength) ? EOL : fData[fPos];
                        if (ch >= '0' && ch <= '9') {
                            fPos++;
                            fIntValue = (fIntValue * 10 + ((int)ch - 48));
                        } else
                            break;
                    }

                    fTokenEnd = fPos;
                    fValueReset = true;
                    fCurrentToken = GEDCOMToken.Number;
                    return fCurrentToken;
                }

                if (ch == ' ' || ch == '\t') {
                    if (fIgnoreWhitespace) {
                        fPos++;
                        continue;
                    }

                    fSavePos = fPos;
                    fPos++;
                    while (true) {
                        ch = (fPos >= fLength) ? EOL : fData[fPos];
                        if (ch == ' ' || ch == '\t')
                            fPos++;
                        else
                            break;
                    }

                    fTokenEnd = fPos;
                    fValueReset = true;
                    fCurrentToken = GEDCOMToken.Whitespace;
                    return fCurrentToken;
                }

                if (ch == '@') {
                    fSavePos = ++fPos;
                    while (true) {
                        ch = (fPos >= fLength) ? EOL : fData[fPos];
                        fPos++;
                        if (ch == '@') {
                            fTokenEnd = fPos - 1;
                            break;
                        }
                    }

                    fValueReset = true;
                    fCurrentToken = GEDCOMToken.XRef;
                    return fCurrentToken;
                }

                if (ch == EOL) {
                    fValueReset = true;
                    fCurrentToken = GEDCOMToken.EOL;
                    return fCurrentToken;
                } else {
                    fSavePos = fPos;
                    fPos++;

                    fTokenEnd = fPos;
                    fValueReset = true;
                    fCurrentToken = GEDCOMToken.Symbol;
                    return fCurrentToken;
                }
            }
        }

        public void SkipWhitespaces()
        {
            if (fCurrentToken == GEDCOMToken.Unknown) {
                Next();
            }

            while (fCurrentToken == GEDCOMToken.Whitespace) {
                Next();
            }
        }

        public string GetWord()
        {
            if (fValueReset) {
                fStrValue = new string(fData, fSavePos, fTokenEnd - fSavePos);
                fValueReset = false;
            }
            return fStrValue;
        }

        public int GetNumber()
        {
            return fIntValue;
        }

        public char GetSymbol()
        {
            return fData[fSavePos];
        }

        public string GetRest()
        {
            return (fPos >= fLength) ? string.Empty : new string(fData, fPos, fLength - fPos);
        }

        public string GetFullStr()
        {
            return new string(fData, 0, fLength);
        }

        public bool RequireToken(GEDCOMToken tokenKind)
        {
            return (fCurrentToken == tokenKind);
        }

        public bool RequireWord(string token)
        {
            return (fCurrentToken == GEDCOMToken.Word && GetWord() == token);
        }

        public bool RequireSymbol(char symbol)
        {
            return (fCurrentToken == GEDCOMToken.Symbol && GetSymbol() == symbol);
        }

        public void RequestSymbol(char symbol)
        {
            if (fCurrentToken != GEDCOMToken.Symbol || GetSymbol() != symbol) {
                throw new GEDCOMParserException("Required symbol not found");
            }
        }

        public void RequestNextSymbol(char symbol)
        {
            var token = Next();
            if (token != GEDCOMToken.Symbol || GetSymbol() != symbol) {
                throw new GEDCOMParserException("Required symbol not found");
            }
        }

        public int RequestInt()
        {
            if (fCurrentToken != GEDCOMToken.Number) {
                throw new GEDCOMParserException("Required integer not found");
            }
            return GetNumber();
        }

        public int RequestNextInt()
        {
            var token = Next();
            if (token != GEDCOMToken.Number) {
                throw new GEDCOMParserException("Required integer not found");
            }
            return GetNumber();
        }
    }
}
