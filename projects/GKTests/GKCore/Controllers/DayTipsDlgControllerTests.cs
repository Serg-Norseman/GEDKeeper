/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.ExtData;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class DayTipsDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_DayTipsDlgController()
        {
            IDayTipsDlg view = CreateMockView();

            var tips = new StringList();
            TipsCollector.Collect(fBaseWin.Context, tips);

            var controller = new DayTipsDlgController(view);
            controller.InitTips("birth days", true, tips);

            controller.GetNextTip();
            controller.UpdateView();
        }

        private static IDayTipsDlg CreateMockView()
        {
            var view = Substitute.For<IDayTipsDlg>();
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnNextTip");
            SubstituteControl<ICheckBox>(view, "chkShow");
            SubstituteControl<ILabel>(view, "lblTitle");

            view.TitleLabel.Returns(Substitute.For<ILabel>());
            view.TipText.Returns(Substitute.For<ITextContainer>());
            view.NextButton.Returns(Substitute.For<IButton>());
            return view;
        }
    }
}
