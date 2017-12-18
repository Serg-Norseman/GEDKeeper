/**
 *	Author: Andrew Deren
 *	Date: July, 2004
 *	http://www.adersoftware.com
 * 
 *	StringTokenizer class. You can use this class in any way you want
 *  as long as this header remains in this file.
 */

using System;
using System.Globalization;

namespace GKCommon
{
    public enum TokenKind
    {
        Unknown,
        Word,
        Ident,
        Number,
        HexNumber,
        BinNumber,
        QuotedString,
        WhiteSpace,
        Symbol,
        EOL,
        EOF
    }

    public class Token
    {
        public readonly TokenKind Kind;
        public readonly string Value;
        public readonly int Line;
        public readonly int Column;
        public readonly object ValObj;

        public Token(TokenKind kind, string value, int line, int column)
        {
            Kind = kind;
            Value = value;
            Line = line;
            Column = column;
        }

        public Token(TokenKind kind, string value, object valObj, int line, int column)
        {
            Kind = kind;
            Value = value;
            ValObj = valObj;
            Line = line;
            Column = column;
        }
    }

    /// <summary>
    /// StringTokenizer tokenized string into tokens.
    /// </summary>
    public class StringTokenizer
    {
        private static readonly NumberFormatInfo NumberFormat = SysUtils.CreateDefaultNumberFormat();

        private const char EOF = (char)0;

        private readonly char[] fData;

        private int fColumn;
        private int fLine;
        private int fPos; // position within data

        private Token fCurrentToken;
        private bool fIgnoreWhiteSpace;
        private bool fRecognizeDecimals;
        private bool fRecognizeHex;
        private bool fRecognizeBin;
        private bool fRecognizeIdents;
        private int fSaveCol;
        private int fSaveLine;
        private int fSavePos;
        private char[] fSymbolChars;

        #region Properties

        public Token CurrentToken
        {
            get { return fCurrentToken; }
        }

        /// <summary>
        /// If set to true, white space characters will be ignored,
        /// but EOL and whitespace inside of string will still be tokenized
        /// </summary>
        public bool IgnoreWhiteSpace
        {
            get { return fIgnoreWhiteSpace; }
            set { fIgnoreWhiteSpace = value; }
        }

        public bool RecognizeDecimals
        {
            get { return fRecognizeDecimals; }
            set { fRecognizeDecimals = value; }
        }

        public bool RecognizeHex
        {
            get { return fRecognizeHex; }
            set { fRecognizeHex = value; }
        }

        public bool RecognizeBin
        {
            get { return fRecognizeBin; }
            set { fRecognizeBin = value; }
        }

        public bool RecognizeIdents
        {
            get { return fRecognizeIdents; }
            set { fRecognizeIdents = value; }
        }

        /// <summary>
        /// Gets or sets which characters are part of TokenKind.Symbol
        /// </summary>
        public char[] SymbolChars
        {
            get { return fSymbolChars; }
            set { fSymbolChars = value; }
        }

        public int Position
        {
            get { return fPos; }
        }

        #endregion

        #region Private methods

        private void Reset()
        {
            fCurrentToken = null;
            fIgnoreWhiteSpace = false;
            fRecognizeDecimals = false;
            fSymbolChars = new char[] {'=', '+', '-', '/', ',', '.', '*', '~', '!', '@', '#', '$', '%', '^', '&', '(', ')', '{', '}', '[', ']', ':', ';', '<', '>', '?', '|', '\\'};

            fLine = 1;
            fColumn = 1;
            fPos = 0;
        }

        /// <summary>
        /// save read point positions so that CreateToken can use those
        /// </summary>
        private void StartRead()
        {
            fSaveCol = fColumn;
            fSaveLine = fLine;
            fSavePos = fPos;
        }

        protected char LookAhead(int count)
        {
            return (fPos + count >= fData.Length) ? EOF : fData[fPos+count];
        }

        protected char Consume()
        {
            char ret = fData[fPos];
            fPos++;
            fColumn++;

            return ret;
        }

        protected Token CreateToken(TokenKind kind, string value, object valObj)
        {
            fCurrentToken = new Token(kind, value, valObj, fLine, fColumn);
            return fCurrentToken;
        }

        protected Token CreateToken(TokenKind kind)
        {
            string tokenData = new string(fData, fSavePos, fPos - fSavePos);
            fCurrentToken = new Token(kind, tokenData, fSaveLine, fSaveCol);
            return fCurrentToken;
        }

        /// <summary>
        /// reads all whitespace characters (does not include newline)
        /// </summary>
        /// <returns></returns>
        protected Token ReadWhitespace()
        {
            StartRead();

            Consume(); // consume the looked-ahead whitespace char

            while (true)
            {
                char ch = LookAhead(0);
                if (ch == '\t' || ch == ' ')
                    Consume();
                else
                    break;
            }

            return CreateToken(TokenKind.WhiteSpace);
        }

        /// <summary>
        /// reads number. Number is: DIGIT+ ("." DIGIT*)?
        /// </summary>
        /// <returns></returns>
        protected Token ReadNumber()
        {
            StartRead();

            TokenKind kind = TokenKind.Number;
            bool hadDot = false;
            bool hadHex = false;
            bool hadBin = false;

            Consume(); // read first digit
            while (true)
            {
                char ch = LookAhead(0);

                if (char.IsDigit(ch)) {
                    Consume();
                } else if (((ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f')) && fRecognizeHex && hadHex) {
                    Consume();
                } else if (ch == '.' && fRecognizeDecimals && !hadDot) {
                    hadDot = true;
                    Consume();
                } else if ((ch == 'e' || ch == 'E' || ch == '+' || ch == '-') && fRecognizeDecimals && hadDot) {
                    Consume();
                } else if (ch == 'x' && fRecognizeHex && !hadHex) {
                    hadHex = true;
                    kind = TokenKind.HexNumber;
                    Consume();
                } else if (ch == 'b' && fRecognizeBin && !hadBin) {
                    hadBin = true;
                    kind = TokenKind.BinNumber;
                    Consume();
                }
                else break;
            }

            string tokVal = new string(fData, fSavePos, fPos - fSavePos);
            object val = null;

            switch (kind) {
                case TokenKind.Number:
                    if (hadDot) {
                        val = Convert.ToDouble(tokVal, NumberFormat);
                    } else {
                        val = ConvertIntNumber(fData, fSavePos, fPos, 10);
                    }
                    break;

                case TokenKind.HexNumber:
                    val = Convert.ToInt32(tokVal, 16);
                    break;

                case TokenKind.BinNumber:
                    val = Convert.ToInt32(tokVal.Substring(2), 2);
                    break;
            }

            return CreateToken(kind, tokVal, val);
        }

        /// <summary>
        /// reads word. Word contains any alpha character or _
        /// </summary>
        protected Token ReadWord()
        {
            StartRead();

            TokenKind kind = TokenKind.Word;

            Consume(); // consume first character of the word
            while (true)
            {
                char ch = LookAhead(0);
                if (char.IsLetter(ch) || ch == '_') {
                    Consume();
                } else if (char.IsDigit(ch) && fRecognizeIdents) {
                    kind = TokenKind.Ident;
                    Consume();
                } else
                    break;
            }

            return CreateToken(kind);
        }

        /// <summary>
        /// reads all characters until next " is found.
        /// If "" (2 quotes) are found, then they are consumed as
        /// part of the string
        /// </summary>
        /// <returns></returns>
        protected Token ReadString()
        {
            StartRead();

            Consume(); // read "

            while (true)
            {
                char ch = LookAhead(0);
                if (ch == EOF) break;
                
                if (ch == '\r')	// handle CR in strings
                {
                    Consume();
                    if (LookAhead(0) == '\n')	// for DOS & windows
                        Consume();

                    fLine++;
                    fColumn = 1;
                }
                else if (ch == '\n')	// new line in quoted string
                {
                    Consume();

                    fLine++;
                    fColumn = 1;
                }
                else if (ch == '"')
                {
                    Consume();
                    if (LookAhead(0) != '"') break; // done reading, and this quotes does not have escape character
                    
                    Consume(); // consume second ", because first was just an escape
                }
                else
                    Consume();
            }

            return CreateToken(TokenKind.QuotedString);
        }

        /// <summary>
        /// checks whether c is a symbol character.
        /// </summary>
        protected bool IsSymbol(char c)
        {
            for (int i = 0; i < fSymbolChars.Length; i++)
                if (fSymbolChars[i] == c)
                    return true;

            return false;
        }

        #endregion

        public StringTokenizer(string data)
        {
            if (data == null)
                throw new ArgumentNullException("data");

            // according to the profiler, "Next()" is faster (+7.1%)
            fData = new char[data.Length];
            data.CopyTo(0, fData, 0, data.Length);

            Reset();
        }

        public string GetRest()
        {
            int len = fData.Length;
            string result = (fPos >= len) ? string.Empty : new string(fData, fPos, len - fPos);
            return result;
        }

        public Token Next()
        {
            while (true) {
                char ch = LookAhead(0);
                switch (ch)
                {
                    case EOF:
                        return CreateToken(TokenKind.EOF, string.Empty, null);

                    case ' ':
                    case '\t':
                        {
                            if (!fIgnoreWhiteSpace)
                                return ReadWhitespace();

                            Consume();
                            break;
                        }

                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        return ReadNumber();

                    case '\r':
                        {
                            StartRead();
                            Consume();
                            if (LookAhead(0) == '\n')
                                Consume();	// on DOS/Windows we have \r\n for new line

                            fLine++;
                            fColumn = 1;

                            return CreateToken(TokenKind.EOL);
                        }

                    case '\n':
                        {
                            StartRead();
                            Consume();
                            fLine++;
                            fColumn = 1;

                            return CreateToken(TokenKind.EOL);
                        }

                    case '"':
                        {
                            return ReadString();
                        }

                    default:
                        {
                            if (char.IsLetter(ch) || ch == '_')
                                return ReadWord();

                            if (IsSymbol(ch))
                            {
                                StartRead();
                                Consume();
                                return CreateToken(TokenKind.Symbol);
                            }
                            else
                            {
                                StartRead();
                                Consume();
                                return CreateToken(TokenKind.Unknown);
                            }
                        }
                }
            }
        }

        public bool RequireToken(TokenKind tokenKind)
        {
            return (fCurrentToken != null && fCurrentToken.Kind == tokenKind);
        }

        public void RequestSymbol(char symbol)
        {
            if (fCurrentToken == null || fCurrentToken.Kind != TokenKind.Symbol || fCurrentToken.Value[0] != symbol) {
                throw new Exception("Required symbol not found");
            }
        }

        public void SkipWhiteSpaces()
        {
            while (fCurrentToken != null && fCurrentToken.Kind == TokenKind.WhiteSpace) {
                Next();
            }
        }

        public int RequestInt()
        {
            if (fCurrentToken == null || fCurrentToken.Kind != TokenKind.Number) {
                throw new Exception("Required integer not found");
            }

            int result = /*fCurrentToken.IntVal; //*/ConvertNumber(fCurrentToken.Value, 10);
            //int.Parse(fCurrentToken.Value, NumberStyles.Integer, NumberFormat);
            return result;
        }

        private static int ConvertIntNumber(char[] buf, int first, int last, byte numBase)
        {
            int fvalue = 0;
            try
            {
                while (first < last)
                {
                    char ch = buf[first];
                    byte c = (byte)((int)ch - 48);
                    if (c > 9)
                    {
                        c -= 7;

                        if (c > 15) {
                            c -= 32;
                        }
                    }

                    if (c >= numBase) {
                        break;
                    }

                    fvalue = (fvalue * numBase + c);
                    first++;
                }
            }
            catch (OverflowException)
            {
                // KBR Parser blows up when trying to parse a large number
            }

            return fvalue;
        }

        private static int ConvertNumber(string expr, byte numBase)
        {
            int fvalue = 0;

            for (int i = 0; i < expr.Length; i++)
            {
                char ch = expr[i];
                byte c = (byte)((int)ch - 48);
                if (c > 9)
                {
                    c -= 7;

                    if (c > 15) {
                        c -= 32;
                    }
                }

                if (c >= numBase) {
                    break;
                }

                fvalue = (fvalue * numBase + c);
            }

            return fvalue;
        }
    }
}
