/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class TreeChartWinControllerTests : ControllerTest
    {
        [Test]
        public void Test_TreeChartWinController()
        {
            var view = Substitute.For<ITreeChartWin>();
            var controller = new TreeChartWinController(view);
        }
    }
}
