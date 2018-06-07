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

using BSLib;
using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GEDCOMTagTests
    {
        [Test]
        public void TestMethod()
        {
            // Reminder for yourself, about the work of string.Remove()

            string str = "test string";
            Assert.IsNotNull(str);
            Assert.AreEqual(11, str.Length);

            str = str.Remove(0, str.Length);
            Assert.IsNotNull(str);
            Assert.AreEqual(0, str.Length);
        }

        [Test]
        public void TestSetTagStringsA()
        {
            var tag = new GEDCOMTag(null, null, "TEST", "");
            Assert.IsNotNull(tag);

            // very long string, 248"A" and " BBB BBBB"
            var strings = new string[] { "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA BBB BBBB" };
            GEDCOMTag.SetTagStrings(tag, strings);

            Assert.AreEqual(248, tag.StringValue.Length);

            var strList = GEDCOMTag.GetTagStrings(tag);
            Assert.IsNotNull(strList);
            Assert.AreEqual(1, strList.Count);
            Assert.AreEqual(strings[0], strList.Text);
        }

        [Test]
        public void TestSetTagStringsL()
        {
            var tag = new GEDCOMTag(null, null, "TEST", "");
            Assert.IsNotNull(tag);

            // very long string, 248"A" and " BBB BBBB"
            var strings = new StringList( "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA BBB BBBB" );

            GEDCOMTag.SetTagStrings(null, strings);

            GEDCOMTag.SetTagStrings(tag, strings);

            Assert.AreEqual(248, tag.StringValue.Length);

            var strList = GEDCOMTag.GetTagStrings(tag);
            Assert.IsNotNull(strList);
            Assert.AreEqual(1, strList.Count);
            Assert.AreEqual(strings.Text, strList.Text);
        }
    }
}
