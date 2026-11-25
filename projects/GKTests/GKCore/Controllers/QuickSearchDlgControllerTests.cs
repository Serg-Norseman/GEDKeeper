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
    public class QuickSearchDlgControllerTests : ControllerTest
    {
        private ITextBox _txtSearchPattern;
        private IButton _btnPrev;
        private IButton _btnNext;

        [Test]
        public void Test_QuickSearchDlgController()
        {
            var view = CreateMockView();

            var baseWin = GetBaseSubst();
            var controller = new QuickSearchDlgController(view, baseWin);
            controller.InitView(null);

            controller.UpdateView();

            _txtSearchPattern.Text = "sample";
            controller.ChangeText();
            controller.FindNext();
            controller.FindPrev();
        }

        private IQuickSearchDlg CreateMockView()
        {
            var view = Substitute.For<IQuickSearchDlg>();
            _txtSearchPattern = SubstCtl<ITextBox>(view, "txtSearchPattern");
            _btnPrev = SubstCtl<IButton>(view, "btnPrev");
            _btnNext = SubstCtl<IButton>(view, "btnNext");
            return view;
        }
    }
}
