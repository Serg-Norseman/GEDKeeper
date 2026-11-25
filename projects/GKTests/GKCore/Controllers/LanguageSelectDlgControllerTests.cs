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
    public class LanguageSelectDlgControllerTests : ControllerTest
    {
        private IListView _lstLanguages;
        private IButton _btnAccept;
        private IButton _btnCancel;

        [Test]
        public void Test_LanguageSelectDlgController()
        {
            ILanguageSelectDlg view = CreateMockView();
            var controller = new LanguageSelectDlgController(view);
            controller.InitView(null);

            controller.Accept();
        }

        private ILanguageSelectDlg CreateMockView()
        {
            var view = Substitute.For<ILanguageSelectDlg>();
            _lstLanguages = SubstCtl<IListView>(view, "lstLanguages");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            return view;
        }
    }
}
