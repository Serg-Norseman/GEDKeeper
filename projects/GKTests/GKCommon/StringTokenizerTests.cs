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

using System;
using GKCommon;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class StringTokenizerTests
    {
        [Test]
        public void BBCodes_Parse_Tests()
        {
            StringTokenizer strTok = new StringTokenizer("alpha beta 123  456.57, x [b]bold text[/b] qq \r\n"
                                                         +" [url=http://test.com/~user/index.html]url text[/url]");
            strTok.IgnoreWhiteSpace = false;
            strTok.RecognizeDecimals = false;

            string result = "";

            Token tok = strTok.Next();
            while (tok.Kind != TokenKind.EOF) {
                if (tok.Kind == TokenKind.Symbol && tok.Value == "[") {
                    tok = strTok.Next();

                    // closed tag
                    if (tok.Kind == TokenKind.Symbol && tok.Value == "/") {
                        tok = strTok.Next();
                    }

                    if (tok.Kind != TokenKind.Word) throw new Exception("not tag");
                    string tag = tok.Value;

                    tok = strTok.Next();

                    if (tag == "url") {
                        if (tok.Kind == TokenKind.Symbol && tok.Value == "=") {
                            tok = strTok.Next();
                            string url = "";
                            do {
                                url += tok.Value;
                                tok = strTok.Next();
                            } while (tok.Kind != TokenKind.Symbol || tok.Value != "]");

                            Assert.AreEqual("http://test.com/~user/index.html", url);
                        }
                    }
                    if (tok.Kind != TokenKind.Symbol || tok.Value != "]") throw new Exception("not bracket");
                } else {
                    result += tok.Value;
                }

                tok = strTok.Next();
            }

            Assert.AreEqual("alpha beta 123  456.57, x bold text qq \r\n url text", result);
        }

        [Test]
        public void Test_Common()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { new StringTokenizer(null); });

            StringTokenizer strTok = new StringTokenizer("alpha beta 123  456.57, x 0x123F");
            Assert.IsNotNull(strTok);

            strTok.RecognizeHex = true;
            strTok.IgnoreWhiteSpace = false;
            Assert.IsFalse(strTok.IgnoreWhiteSpace);

            char[] symChars = strTok.SymbolChars;
            strTok.SymbolChars = symChars;

            strTok.RecognizeDecimals = false;
            Assert.IsFalse(strTok.RecognizeDecimals);

            Token tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("alpha", tok.Value);
            Assert.AreEqual(5, strTok.Position);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("beta", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("123", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("456", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(".", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("57", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(",", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("x", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.HexNumber, tok.Kind);
            Assert.AreEqual(true, strTok.RequireToken(TokenKind.HexNumber));
            Assert.AreEqual("0x123F", tok.Value);

            //

            strTok = new StringTokenizer("alpha beta 123 456.57, x; \r\n \r \n \"test quote\"  \"test \r \n quote2\"");
            Assert.IsNotNull(strTok);

            strTok.IgnoreWhiteSpace = true;
            Assert.IsTrue(strTok.IgnoreWhiteSpace);

            strTok.RecognizeDecimals = true;
            Assert.IsTrue(strTok.RecognizeDecimals);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("alpha", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("beta", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("123", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("456.57", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(",", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("x", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(";", tok.Value);
            Assert.AreEqual(1, tok.Line);


            tok = strTok.Next();
            Assert.AreEqual(TokenKind.EOL, tok.Kind);
            Assert.AreEqual(1, tok.Line);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.EOL, tok.Kind);
            Assert.AreEqual(2, tok.Line);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.EOL, tok.Kind);
            Assert.AreEqual(3, tok.Line);


            tok = strTok.Next();
            Assert.AreEqual(TokenKind.QuotedString, tok.Kind);
            Assert.AreEqual("\"test quote\"", tok.Value);
            Assert.AreEqual(4, tok.Line);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.QuotedString, tok.Kind);
            Assert.AreEqual("\"test \r \n quote2\"", tok.Value);

            //

            strTok = new StringTokenizer("alpha 0x601 0b11000000001 alg_123");
            Assert.IsNotNull(strTok);
            strTok.IgnoreWhiteSpace = true;

            strTok.RecognizeHex = true;
            Assert.IsTrue(strTok.RecognizeHex);

            strTok.RecognizeBin = true;
            Assert.IsTrue(strTok.RecognizeBin);

            strTok.RecognizeIdents = true;
            Assert.IsTrue(strTok.RecognizeIdents);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("alpha", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.HexNumber, tok.Kind);
            Assert.AreEqual("0x601", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.BinNumber, tok.Kind);
            Assert.AreEqual("0b11000000001", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Ident, tok.Kind);
            Assert.AreEqual("alg_123", tok.Value);
        }
    }
}
