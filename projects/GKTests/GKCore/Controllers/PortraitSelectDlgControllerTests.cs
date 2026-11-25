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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IImageView _imageView1;

        [Test]
        public void Test_PortraitSelectDlgController()
        {
            IPortraitSelectDlg view = CreateMockView();
            var controller = new PortraitSelectDlgController(view);
            controller.InitView(fBaseWin);

            var multimediaLink = new GDMMultimediaLink();
            controller.MultimediaLink = multimediaLink;

            controller.UpdateView();
        }

        private IPortraitSelectDlg CreateMockView()
        {
            var view = Substitute.For<IPortraitSelectDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _imageView1 = SubstCtl<IImageView>(view, "imageView1");
            return view;
        }
    }
}
