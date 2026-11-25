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
    public class AboutDlgControllerTests : ControllerTest
    {
        private ILabel _lblProduct;
        private ILabel _lblVersion;
        private ILabel _lblCopyright;
        private IButton _btnClose;
        private ILabel _lblForum;
        private ILabel _lblChannel;

        [Test]
        public void Test_AboutDlgController()
        {
            IAboutDlg view = CreateMockView();
            var controller = new AboutDlgController(view);
            controller.InitView(null);
            controller.SetLocale();
            controller.UpdateView();
        }

        private IAboutDlg CreateMockView()
        {
            var view = Substitute.For<IAboutDlg>();
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _lblProduct = SubstCtl<ILabel>(view, "lblProduct");
            _lblVersion = SubstCtl<ILabel>(view, "lblVersion");
            _lblCopyright = SubstCtl<ILabel>(view, "lblCopyright");
            _lblForum = SubstCtl<ILabel>(view, "lblForum");
            _lblChannel = SubstCtl<ILabel>(view, "lblChannel");
            return view;
        }
    }
}
