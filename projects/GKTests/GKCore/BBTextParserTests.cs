/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using GKUI.Components;
using GKUI.Providers;
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
        public void Test_Common()
        {
            string sample = "[size=+1][color=red][b]bold text[/b] [i][u]italic[/i] and underline[/u] qq[/color] "+
                "[size=-1][s]strikeout[/s][/size] \r\n [url=http://test.com/~user/index.html]url text[/url][/size]";

            var parser = new BBTextParser(AppHost.GfxProvider, 12.0f,
                                          new ColorHandler(Color.Blue),
                                          new ColorHandler(Color.Black));

            List<BBTextChunk> chunksList = new List<BBTextChunk>();

            parser.ParseText(chunksList, sample);

            Assert.IsFalse(chunksList[0].HasCoord(5, 5, 0, 0));

            Assert.IsFalse(chunksList[0].HasCoord(5, 5));
        }
    }
}
