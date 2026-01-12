/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKTests;
using NUnit.Framework;

namespace GKCore.ExtData
{
    [TestFixture]
    public class TipsCollectorTests
    {
        private readonly BaseContext fContext;

        public TipsCollectorTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Collect_Null()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { TipsCollector.Collect(fContext, null); });
        }
    }
}
