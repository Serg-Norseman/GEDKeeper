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
        [Test]
        public void Test_QuickSearchDlgController()
        {
            var view = Substitute.For<IQuickSearchDlg>();
            SubstituteControl<IButton>(view, "btnPrev");
            SubstituteControl<IButton>(view, "btnNext");

            view.SearchPattern.Returns(Substitute.For<ITextBox>());

            var baseWin = GetBaseSubst();
            var controller = new QuickSearchDlgController(view, baseWin);

            controller.UpdateView();

            controller.ChangeText();
            controller.FindNext();
            controller.FindPrev();
        }
    }
}
