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
    public class CircleChartWinControllerTests : ControllerTest
    {
        [Test]
        public void Test_CircleChartWinController()
        {
            var view = Substitute.For<ICircleChartWin>();
            //var controller = new CircleChartWinController(view);
        }
    }
}
