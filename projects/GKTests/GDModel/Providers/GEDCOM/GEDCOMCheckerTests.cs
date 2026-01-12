/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GKCore;
using GKCore.Design;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GDModel.Providers.GEDCOM
{
    [TestFixture]
    public class GEDCOMCheckerTests
    {
        private readonly IBaseWindow fBaseWin;

        public GEDCOMCheckerTests()
        {
            TestUtils.InitUITest();

            fBaseWin = new BaseWindowStub();
        }

        [Test]
        public void Test_CheckGEDCOMFormat()
        {
            var progress = Substitute.For<IProgressController>();

            Assert.Throws(typeof(ArgumentNullException), () => { GEDCOMChecker.CheckGEDCOMFormat(null, null); });
            GEDCOMChecker.CheckGEDCOMFormat(fBaseWin.Context, progress);
        }

        [Test]
        public async Task Test_WorkDataCollector()
        {
            Assert.ThrowsAsync(typeof(ArgumentNullException), async () => { await WorkDataCollector.Collect(null); });
            await WorkDataCollector.Collect(fBaseWin.Context);
        }
    }
}
