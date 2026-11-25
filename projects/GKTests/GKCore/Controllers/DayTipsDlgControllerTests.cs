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
        private ICheckBox _chkShow;
        private IButton _btnNextTip;
        private IButton _btnClose;
        private ILabel _lblTitle;
        private ITextContainer _txtTip;

        [Test]
        public void Test_DayTipsDlgController()
        {
            IDayTipsDlg view = CreateMockView();

            var tips = new StringList();
            TipsCollector.Collect(fBaseWin.Context, tips);

            var controller = new DayTipsDlgController(view);
            controller.InitView(null);
            controller.InitTips("birth days", true, tips);

            controller.GetNextTip();
            controller.UpdateView();
        }

        private IDayTipsDlg CreateMockView()
        {
            var view = Substitute.For<IDayTipsDlg>();
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _btnNextTip = SubstCtl<IButton>(view, "btnNextTip");
            _chkShow = SubstCtl<ICheckBox>(view, "chkShow");
            _lblTitle = SubstCtl<ILabel>(view, "lblTitle");
            _txtTip = SubstCtl<ITextContainer>(view, "txtTip");
            return view;
        }
    }
}
