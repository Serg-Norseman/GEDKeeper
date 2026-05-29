/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class SlideshowControllerTests : ControllerTest
    {
        [Test]
        public void Test_SlideshowController()
        {
            var view = Substitute.For<ISlideshowWin>();
            SubstituteControl<IToolItem>(view, "tbStart");
            SubstituteControl<IToolItem>(view, "tbPrev");
            SubstituteControl<IToolItem>(view, "tbNext");

            var controller = new SlideshowController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();
        }
    }
}
