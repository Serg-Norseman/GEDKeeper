/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKTests;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.BBText
{
    [TestFixture]
    public class BBTextParserTests
    {
        public BBTextParserTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_BBTextChunk()
        {
            var instance = new BBTextChunk(1, 10.0f, GKFontStyle.Bold, null);
            Assert.IsNotNull(instance);
            instance.Text = "Test chunk";

            var copy = instance.Clone();
            Assert.IsNotNull(copy);

            Assert.AreEqual("[BBTextChunk Line=0, Text=Test chunk, Size=10]", copy.ToString());
        }

        [Test]
        public void Test_Common()
        {
            string sample = "[size=+1][color=red][b]bold text[/b] [i][u]italic[/i] and underline[/u] qq[/color] "+
                "[size=-1][s]strikeout[/s][/size] \r\n [url=http://test.com/~user/index.html]url text[/url][/size]";

            var color = Substitute.For<IColor>();
            var parser = new BBTextParser(AppHost.GfxProvider, 12.0f, color, color);

            List<BBTextChunk> chunksList = new List<BBTextChunk>();

            parser.ParseText(chunksList, sample);

            Assert.AreEqual(13, chunksList.Count);
            Assert.AreEqual("bold text", chunksList[0].Text);
            Assert.AreEqual(" ", chunksList[1].Text);
            Assert.AreEqual("italic", chunksList[2].Text);
            Assert.AreEqual(" and underline", chunksList[3].Text);
            Assert.AreEqual(" qq", chunksList[4].Text);
            Assert.AreEqual(" ", chunksList[5].Text);
            Assert.AreEqual("strikeout", chunksList[6].Text);
            Assert.AreEqual(" ", chunksList[7].Text);
            Assert.AreEqual(" ", chunksList[8].Text); // ???
            Assert.AreEqual(" ", chunksList[9].Text);
            Assert.AreEqual("url text", chunksList[10].Text);
            Assert.AreEqual("", chunksList[11].Text); // ???
            Assert.AreEqual(" ", chunksList[12].Text); // ???

            Assert.IsFalse(chunksList[0].HasCoord(5, 5));
        }

        [Test]
        public void Test_DirtySample()
        {
            string sample = "[u][b][size=+1]Ingvar [the Mighty][/size][/u][/b]";

            var color = Substitute.For<IColor>();
            var parser = new BBTextParser(AppHost.GfxProvider, 12.0f, color, color);

            List<BBTextChunk> chunksList = new List<BBTextChunk>();

            parser.ParseText(chunksList, sample);

            Assert.AreEqual(3, chunksList.Count);
            Assert.AreEqual("Ingvar [the Mighty]", chunksList[0].Text);
            Assert.AreEqual("", chunksList[1].Text); // ???
            Assert.AreEqual(" ", chunksList[2].Text); // ???
        }
    }
}
