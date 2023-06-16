/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using GDModel.Providers.GEDCOM;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMTagTests
    {
        public GDMTagTests()
        {
            TestUtils.InitGEDCOMProviderTest();
        }

        [Test]
        public void Test_GEDCOMObject()
        {
            GDMObject obj = new GDMObject();
            obj.Dispose();
        }

        [Test]
        public void Test_AssignNull()
        {
            GDMTag tag = new GDMTag();
            tag.Assign(null); // nothing
            tag.Dispose();
        }

        [Test]
        public void Test_FindTags()
        {
            var tag = new GDMTag(GEDCOMTagsTable.Lookup("TEST"), "");
            Assert.IsNotNull(tag);

            tag.AddTag(new GDMValueTag((int)GEDCOMTagType._FOLDER, "Private"));
            tag.AddTag(new GDMValueTag((int)GEDCOMTagType._FOLDER, "Friends"));
            tag.AddTag(new GDMValueTag((int)GEDCOMTagType._FOLDER, "Research"));

            var subTags = tag.FindTags(GEDCOMTagName._FOLDER);
            Assert.AreEqual(3, subTags.Count);

            tag.DeleteTag(GEDCOMTagName._FOLDER);

            subTags = tag.FindTags(GEDCOMTagName._FOLDER);
            Assert.AreEqual(0, subTags.Count);
        }

        [Test]
        public void Test_IndexOf()
        {
            using (GDMTag tag = new GDMTag(GEDCOMTagsTable.Lookup(""), "")) {
                Assert.AreEqual(-1, tag.SubTags.IndexOf(null));
            }
        }
    }
}
