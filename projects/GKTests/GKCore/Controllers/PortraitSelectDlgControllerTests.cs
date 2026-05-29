/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class PortraitSelectDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_PortraitSelectDlgController()
        {
            IPortraitSelectDlg view = CreateMockView();
            var controller = new PortraitSelectDlgController(view);
            controller.Init(fBaseWin);

            var multimediaLink = new GDMMultimediaLink();
            controller.MultimediaLink = multimediaLink;

            controller.UpdateView();
        }

        private static IPortraitSelectDlg CreateMockView()
        {
            var view = Substitute.For<IPortraitSelectDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            view.ImageCtl.Returns(Substitute.For<IImageView>());
            return view;
        }
    }
}
