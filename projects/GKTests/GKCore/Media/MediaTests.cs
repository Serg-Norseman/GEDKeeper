// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using BSLib;
using NUnit.Framework;

namespace GKCore.Media
{
    [TestFixture]
    public class MediaTests
    {
        [Test]
        public void TestNamedRegion()
        {
            var region = new NamedRegion("test", ExtRect.Empty);
            Assert.AreEqual("test", region.Name);
        }
    }
}
