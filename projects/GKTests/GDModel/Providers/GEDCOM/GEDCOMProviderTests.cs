// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using NUnit.Framework;

namespace GDModel.Providers.GEDCOM
{
    [TestFixture]
    public class GEDCOMProviderTests
    {
        [Test]
        public void Test_GetGEDCOMFormat()
        {
            GDMTree tree = new GDMTree();

            bool badLines;
            // Tests of determine GEDCOM-format
            Assert.AreEqual(GEDCOMFormat.Unknown, GEDCOMProvider.GetGEDCOMFormat(tree, out badLines));
            tree.Header.Source.StringValue = "GENBOX";
            Assert.AreEqual(GEDCOMFormat.GENBOX, GEDCOMProvider.GetGEDCOMFormat(tree, out badLines));
        }

        [Test]
        public void Test_GetTagProps()
        {
            GEDCOMTagProps props = GEDCOMTagsTable.GetTagProps(GEDCOMTagName.ADDR);
            Assert.IsNotNull(props);
            Assert.IsTrue(props.SkipEmpty);

            props = GEDCOMTagsTable.GetTagProps("test");
            Assert.IsNull(props);
        }
    }
}
