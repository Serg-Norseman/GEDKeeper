/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class CommonFilterDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnReset;
        private ITabPage _pageFieldsFilter;
        private IFilterGridView _filterView;

        [Test]
        public void Test_CommonFilterDlgController()
        {
            ICommonFilterDlg view = CreateMockView();
            var listMan = new NoteListModel(fBaseWin.Context);
            var controller = new CommonFilterDlgController(view, listMan);
            controller.InitView(null);

            controller.Accept();
            controller.UpdateView();
            controller.Reset();
        }

        private ICommonFilterDlg CreateMockView()
        {
            var view = Substitute.For<ICommonFilterDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnReset = SubstCtl<IButton>(view, "btnReset");
            _pageFieldsFilter = SubstCtl<ITabPage>(view, "pageFieldsFilter");
            _filterView = SubstCtl<IFilterGridView>(view, "filterView");
            return view;
        }
    }
}
