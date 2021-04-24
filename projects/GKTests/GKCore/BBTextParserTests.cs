/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2021 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Drawing;
using BSLib.Design.Handlers;
using GKCore.BBText;
using GKUI;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class BBTextParserTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);
        }

        [Test]
        public void Test_BBTextChunk()
        {
            var instance = new BBTextChunk(1, 10.0f, BSLib.Design.BSDTypes.FontStyle.Bold, null);
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

            var parser = new BBTextParser(AppHost.GfxProvider, 12.0f,
                                          new ColorHandler(Color.Blue),
                                          new ColorHandler(Color.Black));

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

            var parser = new BBTextParser(AppHost.GfxProvider, 12.0f,
                                          new ColorHandler(Color.Blue),
                                          new ColorHandler(Color.Black));

            List<BBTextChunk> chunksList = new List<BBTextChunk>();

            parser.ParseText(chunksList, sample);

            Assert.AreEqual(3, chunksList.Count);
            Assert.AreEqual("Ingvar [the Mighty]", chunksList[0].Text);
            Assert.AreEqual("", chunksList[1].Text); // ???
            Assert.AreEqual(" ", chunksList[2].Text); // ???
        }
    }
}
