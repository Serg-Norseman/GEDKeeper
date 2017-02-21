/**
 *	Author: Andrew Deren
 *	Date: July, 2004
 *	http://www.adersoftware.com
 * 
 *	StringTokenizer class. You can use this class in any way you want
 *  as long as this header remains in this file.
 */

using System;

namespace GKCommon
{
    public enum TokenKind
    {
        Unknown,
        Word,
        Number,
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

        public Token(TokenKind kind, string value, int line, int column)
        {
            Kind = kind;
            Value = value;
            Line = line;
            Column = column;
        }
    }

    /// <summary>
    /// StringTokenizer tokenized string (or stream) into tokens.
    /// </summary>
    public class StringTokenizer
    {
        private const char EOF = (char)0;

        private int fLine;
        private int fColumn;
        private int fPos;	// position within data

        private readonly string fData;

        private bool fIgnoreWhiteSpace;
        private bool fRecognizeDecimals;
        private char[] fSymbolChars;

        private int fSaveLine;
        private int fSaveCol;
        private int fSavePos;

        public StringTokenizer(string data)
        {
            if (data == null)
                throw new ArgumentNullException("data");

            fData = data;

            Reset();
        }

        /// <summary>
        /// gets or sets which characters are part of TokenKind.Symbol
        /// </summary>
        public char[] SymbolChars
        {
            get { return fSymbolChars; }
            set { fSymbolChars = value; }
        }

        /// <summary>
        /// if set to true, white space characters will be ignored,
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

        public int Position
        {
            get { return fPos; }
        }

        private void Reset()
        {
            fIgnoreWhiteSpace = false;
            fRecognizeDecimals = true;
            fSymbolChars = new char[]{'=', '+', '-', '/', ',', '.', '*', '~', '!', '@', '#', '$', '%', '^', '&', '(', ')', '{', '}', '[', ']', ':', ';', '<', '>', '?', '|', '\\'};

            fLine = 1;
            fColumn = 1;
            fPos = 0;
        }

        protected char LA(int count)
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

        protected Token CreateToken(TokenKind kind, string value)
        {
            return new Token(kind, value, fLine, fColumn);
        }

        protected Token CreateToken(TokenKind kind)
        {
            string tokenData = fData.Substring(fSavePos, fPos-fSavePos);
            return new Token(kind, tokenData, fSaveLine, fSaveCol);
        }

        public Token Next()
        {
        ReadToken:

            char ch = LA(0);
            switch (ch)
            {
                case EOF:
                    return CreateToken(TokenKind.EOF, string.Empty);

                case ' ':
                case '\t':
                    {
                        if (fIgnoreWhiteSpace)
                        {
                            Consume();
                            goto ReadToken;
                        }
                        return ReadWhitespace();
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
                        if (LA(0) == '\n')
                            Consume();	// on DOS/Windows we have \r\n for new line

                        fLine++;
                        fColumn=1;

                        return CreateToken(TokenKind.EOL);
                    }
                case '\n':
                    {
                        StartRead();
                        Consume();
                        fLine++;
                        fColumn=1;
                        
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

        /// <summary>
        /// save read point positions so that CreateToken can use those
        /// </summary>
        private void StartRead()
        {
            fSaveLine = fLine;
            fSaveCol = fColumn;
            fSavePos = fPos;
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
                char ch = LA(0);
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

            bool hadDot = false;

            Consume(); // read first digit

            while (true)
            {
                char ch = LA(0);
                if (char.IsDigit(ch))
                    Consume();
                else if (ch == '.' && fRecognizeDecimals && !hadDot)
                {
                    hadDot = true;
                    Consume();
                }
                else
                    break;
            }

            return CreateToken(TokenKind.Number);
        }

        /// <summary>
        /// reads word. Word contains any alpha character or _
        /// </summary>
        protected Token ReadWord()
        {
            StartRead();

            Consume(); // consume first character of the word

            while (true)
            {
                char ch = LA(0);
                if (char.IsLetter(ch) || ch == '_')
                    Consume();
                else
                    break;
            }

            return CreateToken(TokenKind.Word);
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
                char ch = LA(0);
                if (ch == EOF) break;
                
                if (ch == '\r')	// handle CR in strings
                {
                    Consume();
                    if (LA(0) == '\n')	// for DOS & windows
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
                    if (LA(0) != '"') break; // done reading, and this quotes does not have escape character
                    
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
    }
}
