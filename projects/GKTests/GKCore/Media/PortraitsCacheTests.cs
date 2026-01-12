/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using NUnit.Framework;

namespace GKCore.Media
{
    [TestFixture]
    public class PortraitsCacheTests
    {
        public PortraitsCacheTests()
        {
        }

        [Test]
        public void Test_Instance()
        {
            var inst = PortraitsCache.Instance;
            Assert.IsNotNull(inst);
        }

        [Test]
        public void Test_Common()
        {
            PortraitsCache cache = PortraitsCache.Instance;
            Assert.IsNull(cache.GetImage(null, null));
            cache.RemoveObsolete(null);
        }
    }
}
