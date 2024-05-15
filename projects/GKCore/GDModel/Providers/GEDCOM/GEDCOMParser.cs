/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
    /// <summary>
    /// Introduced as a stripped-down analogue of Span&lt;char&gt; to minimize memory allocations
    /// when passing tag values between parsing methods.
    /// </summary>
    public struct StringSpan
    {
        public static readonly StringSpan Empty = new StringSpan(null, 0, 0);

        public readonly char[] Data;
        public readonly int Length;
        public readonly int Pos;

        public bool IsEmpty
        {
            get { return (Data == null || Length == 0); }
        }

        // :this() for compatibility with the old assembly chain
        public StringSpan(char[] data, int length, int pos) : this()
        {
            Data = data;
            Length = length; // full length of data in line buffer
            Pos = pos; // current position between 0 and length
        }

        public static implicit operator string(StringSpan span)
        {
            return (span.Data == null || span.Length == 0) ? string.Empty : new string(span.Data, span.Pos, span.Length - span.Pos);
        }

        public static implicit operator StringSpan(string str)
        {
            return (string.IsNullOrEmpty(str)) ? StringSpan.Empty : new StringSpan(str.ToCharArray(), str.Length, 0);
        }
    }

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
    /// <remarks>
    /// This class has been heavily refactored under profiling. Any alterations must take into account the factor 
    /// of performance degradation when changing the approach, even in small things.
    /// </remarks>
    public sealed class GEDCOMParser
    {
        public static readonly GEDCOMParser Default = new GEDCOMParser(false);

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

        public GEDCOMParser(StringSpan strSpan, bool ignoreWhitespace)
        {
            fIgnoreWhitespace = ignoreWhitespace;

            Reset(strSpan);
        }

        public void Reset(char[] data, int startIndex, int length)
        {
            fData = data;
            fPos = startIndex;
            fLength = length;

            fCurrentToken = GEDCOMToken.Unknown;
            fValueReset = false;
        }

        public void Reset(StringSpan strSpan)
        {
            fData = strSpan.Data;
            fPos = strSpan.Pos;
            fLength = strSpan.Length;

            fCurrentToken = GEDCOMToken.Unknown;
            fValueReset = false;
        }

        public GEDCOMToken Next(bool skipOneSpace = false)
        {
            while (true) {
                char ch = (fPos >= fLength) ? EOL : fData[fPos];
                char ltr = (char)(ch | ' ');

                if ((ltr >= 'a' && ltr <= 'z') || ch == '_' || char.IsLetter(ch)) {
                    fSavePos = fPos;
                    fPos++;
                    while (true) {
                        ch = (fPos >= fLength) ? EOL : fData[fPos];
                        ltr = (char)(ch | ' ');
                        if ((ltr >= 'a' && ltr <= 'z') || ch == '_' || char.IsLetter(ch) || (ch >= '0' && ch <= '9')) {
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

                    if (!skipOneSpace) {
                        while (true) {
                            ch = (fPos >= fLength) ? EOL : fData[fPos];
                            if (ch == ' ' || ch == '\t')
                                fPos++;
                            else
                                break;
                        }
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
            while (fCurrentToken <= GEDCOMToken.Whitespace) {
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

        public StringSpan GetRestSpan()
        {
            return new StringSpan(fData, fLength, fPos);
        }

        public string GetFullStr()
        {
            return new string(fData, 0, fLength);
        }

        public StringSpan GetFullSpan()
        {
            return new StringSpan(fData, fLength, 0);
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

        public int RequestNextSignedInt()
        {
            var token = Next();

            bool neg = (token == GEDCOMToken.Symbol && GetSymbol() == '-');
            if (neg) {
                token = Next();
            }

            if (token != GEDCOMToken.Number) {
                throw new GEDCOMParserException("Required integer not found");
            }

            int number = GetNumber();
            if (neg) {
                number = -number;
            }
            return number;
        }

        public int TokenLength()
        {
            return fTokenEnd - fSavePos;
        }
    }
}
