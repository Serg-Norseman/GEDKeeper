// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

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
