/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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

        public bool IsEmptyOrEnd
        {
            get { return (Data == null || Length == 0 || Pos >= Length); }
        }

        // :this() for compatibility with the old assembly chain
        public StringSpan(char[] data, int pos, int length) : this()
        {
            Data = data;
            Pos = pos; // current position between 0 and length
            Length = length; // full length of data in line buffer
        }

        public static implicit operator string(StringSpan span)
        {
            return (span.Data == null || span.Length == 0) ? string.Empty : new string(span.Data, span.Pos, span.Length - span.Pos);
        }

        public static implicit operator StringSpan(string str)
        {
            return (string.IsNullOrEmpty(str)) ? StringSpan.Empty : new StringSpan(str.ToCharArray(), 0, str.Length);
        }

        public override string ToString()
        {
            return (Data == null || Length == 0) ? string.Empty : new string(Data, Pos, Length - Pos);
        }
    }

    public enum GEDCOMToken
    {
        Unknown,
        Whitespace, // space or tab
        Symbol,     // any symbol that is not a letter or a number
        Word,       // any sequence of letters and numbers beginning with Latin characters, not interrupted by punctuation marks
        Number,     // any number
        XRef,       // any sequence of letters and numbers between the @ symbols
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
        private bool fPureWords;
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


        public GEDCOMParser(bool ignoreWhitespace, bool pureWords = false)
        {
            fIgnoreWhitespace = ignoreWhitespace;
            fPureWords = pureWords;
        }

        public GEDCOMParser(string data, bool ignoreWhitespace)
        {
            if (data == null)
                throw new ArgumentNullException(nameof(data));

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
                        if ((ltr >= 'a' && ltr <= 'z') || ch == '_' || char.IsLetter(ch) || (!fPureWords && (ch >= '0' && ch <= '9'))) {
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
                    fIntValue = (ch - 48);
                    while (true) {
                        ch = (fPos >= fLength) ? EOL : fData[fPos];
                        if (ch >= '0' && ch <= '9') {
                            fPos++;
                            fIntValue = (fIntValue * 10 + (ch - 48));
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

        public string GetRest()
        {
            return (fPos >= fLength) ? string.Empty : new string(fData, fPos, fLength - fPos);
        }

        public StringSpan GetRestSpan()
        {
            return new StringSpan(fData, fPos, fLength);
        }

        public string GetFullStr()
        {
            return new string(fData, 0, fLength);
        }

        public StringSpan GetFullSpan()
        {
            return new StringSpan(fData, 0, fLength);
        }

        public int GetNumber()
        {
            return fIntValue;
        }

        public char GetSymbol()
        {
            return fData[fSavePos];
        }

        public string GetToken()
        {
            if (fValueReset) {
                fStrValue = new string(fData, fSavePos, fTokenEnd - fSavePos);
                fValueReset = false;
            }
            return fStrValue;
        }

        public bool HasSymbol(char symbol)
        {
            return (fCurrentToken == GEDCOMToken.Symbol && fData[fSavePos] == symbol);
        }

        public bool HasWhitespace(char symbol)
        {
            return (fCurrentToken == GEDCOMToken.Whitespace && fData[fSavePos] == symbol);
        }

        public bool HasWord(out string token)
        {
            bool result = (fCurrentToken == GEDCOMToken.Word);
            token = !result ? string.Empty : new string(fData, fSavePos, fTokenEnd - fSavePos);
            return result;
        }

        public bool HasXRef(out string token)
        {
            bool result = (fCurrentToken == GEDCOMToken.XRef);
            token = !result ? string.Empty : new string(fData, fSavePos, fTokenEnd - fSavePos);
            return result;
        }

        public void RequestSymbol(char symbol)
        {
            var token = Next();
            if (token != GEDCOMToken.Symbol || fData[fSavePos] != symbol) {
                throw new GEDCOMParserException("Required symbol not found");
            }
        }

        public int RequestInt()
        {
            var token = Next();
            if (token != GEDCOMToken.Number) {
                throw new GEDCOMParserException("Required integer not found");
            }
            return fIntValue;
        }

        public int RequestSignedInt()
        {
            var token = Next();

            bool neg = (fCurrentToken == GEDCOMToken.Symbol && fData[fSavePos] == '-');
            if (neg) {
                token = Next();
            }

            if (token != GEDCOMToken.Number) {
                throw new GEDCOMParserException("Required integer not found");
            }

            int number = fIntValue;
            if (neg) {
                number = -number;
            }
            return number;
        }

        public bool HasNumber(int digits, out int number)
        {
            bool result = (fCurrentToken == GEDCOMToken.Number && (fTokenEnd - fSavePos <= digits));
            number = (!result) ? 0 : fIntValue;
            return result;
        }
    }
}
