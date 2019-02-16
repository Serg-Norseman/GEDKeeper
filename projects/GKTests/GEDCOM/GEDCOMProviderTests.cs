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

using GKCommon.GEDCOM;
using NUnit.Framework;
using BSLib;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GEDCOMProviderTests
    {
        [Test]
        public void Test_GetGEDCOMFormat()
        {
            GEDCOMTree tree = new GEDCOMTree();

            // Tests of determine GEDCOM-format
            Assert.AreEqual(GEDCOMFormat.gf_Unknown, GEDCOMProvider.GetGEDCOMFormat(tree));
            tree.Header.Source = "GENBOX";
            Assert.AreEqual(GEDCOMFormat.gf_GENBOX, GEDCOMProvider.GetGEDCOMFormat(tree));
        }

        [Test]
        public void Test_GetTagProps()
        {
            TagProperties props = GEDCOMProvider.GetTagProps(GEDCOMTagType.ADDR);
            Assert.IsNotNull(props);
            Assert.IsTrue(props.SkipEmpty);

            props = GEDCOMProvider.GetTagProps("test");
            Assert.IsNull(props);
        }

        private static void ParseTag_oldST(string line, out int tagLevel, out string tagXRef, out string tagName, out string tagValue)
        {
            tagLevel = 0;
            tagXRef = "";
            tagName = "";
            tagValue = "";

            var strTok = new StringTokenizer(line);
            strTok.RecognizeDecimals = false;
            strTok.IgnoreWhiteSpace = false;
            strTok.RecognizeIdents = true;

            var token = strTok.Next(); // already trimmed
            if (token.Kind != TokenKind.Number) {
                // syntax error
                throw new EGEDCOMException(string.Format("The string {0} doesn't start with a valid number", line));
            }
            tagLevel = (int)token.ValObj;

            token = strTok.Next();
            if (token.Kind != TokenKind.WhiteSpace) {
                // syntax error
            }

            token = strTok.Next();
            if (token.Kind == TokenKind.Symbol && token.Value[0] == '@') {
                token = strTok.Next();
                while (token.Kind != TokenKind.Symbol && token.Value[0] != '@') {
                    tagXRef += token.Value;
                    token = strTok.Next();
                }
                // FIXME: check for errors
                //throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", str));
                //throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", str));

                token = strTok.Next();
                strTok.SkipWhiteSpaces();
            }

            token = strTok.CurrentToken;
            if (token.Kind != TokenKind.Word && token.Kind != TokenKind.Ident) {
                // syntax error
            }
            tagName = token.Value.ToUpperInvariant();

            token = strTok.Next();
            if (token.Kind == TokenKind.WhiteSpace) {
                tagValue = strTok.GetRest();
            }

            // convert codepages
            /*if (!string.IsNullOrEmpty(tagValue) && fEncodingState == EncodingState.esChanged) {
                tagValue = ConvertStr(fSourceEncoding, tagValue);
            }*/
        }

        [Test]
        public void Test_ParseTag()
        {
            string str;
            int tagLevel1, tagLevel2, res;
            string tagXRef1, tagName1, tagValue1, tagXRef2, tagName2, tagValue2;

            str = "0 HEAD";
            ParseTag_oldST(str, out tagLevel1, out tagXRef1, out tagName1, out tagValue1);
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(tagLevel1, tagLevel2);
            Assert.AreEqual(tagXRef1, tagXRef2);
            Assert.AreEqual(tagName1, tagName2);
            Assert.AreEqual(tagValue1, tagValue2);
            Assert.AreEqual(2, res);

            str = "0 @SUB1@ SUBM";
            ParseTag_oldST(str, out tagLevel1, out tagXRef1, out tagName1, out tagValue1);
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(tagLevel1, tagLevel2);
            Assert.AreEqual(tagXRef1, tagXRef2);
            Assert.AreEqual(tagName1, tagName2);
            Assert.AreEqual(tagValue1, tagValue2);
            Assert.AreEqual(3, res);

            str = "0 @SUB1@ SUBM testVal";
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("SUB1", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("testVal", tagValue2);
            Assert.AreEqual(4, res);

            str = "1 SUBM @SUB1@";
            ParseTag_oldST(str, out tagLevel1, out tagXRef1, out tagName1, out tagValue1);
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(tagLevel1, tagLevel2);
            Assert.AreEqual(tagXRef1, tagXRef2);
            Assert.AreEqual(tagName1, tagName2);
            Assert.AreEqual(tagValue1, tagValue2);
            Assert.AreEqual(3, res);

            str = "    1 SUBM @SUB1@";
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(1, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("@SUB1@", tagValue2);
            Assert.AreEqual(3, res);

            str = "2 DATE FROM 20 JAN 1979 TO 15 MAY 2012";
            ParseTag_oldST(str, out tagLevel1, out tagXRef1, out tagName1, out tagValue1);
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(tagLevel1, tagLevel2);
            Assert.AreEqual(tagXRef1, tagXRef2);
            Assert.AreEqual(tagName1, tagName2);
            Assert.AreEqual(tagValue1, tagValue2);
            Assert.AreEqual(3, res);


            str = "    test test test (FTB line with error)";
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual("test test test (FTB line with error)", tagValue2);
            Assert.AreEqual(-1, res);

            str = "        ";
            res = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(-2, res);
        }
    }
}
