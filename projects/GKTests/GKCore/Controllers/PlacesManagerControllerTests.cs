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
    public class PlacesManagerControllerTests : ControllerTest
    {
        private IButton _btnClose;
        private ITabPage _pagePlaceManage;
        private IButton _btnAnalysePlaces;
        private IButton _btnLocExpert;
        private IButton _btnIntoList;
        private IListView _ListPlaces;
        private ILabel _lblFilter;
        private ITextBox _txtFilter;

        [Test]
        public void Test_PlacesManagerController()
        {
            IPlacesManagerDlg view = CreateMockView();
            var controller = new PlacesManagerController(view);
            controller.InitView(fBaseWin);

            controller.CheckPlaces();
            controller.CreateLocationRecord(_ListPlaces.GetSelectedItems());
        }

        private IPlacesManagerDlg CreateMockView()
        {
            var view = Substitute.For<IPlacesManagerDlg>();
            _pagePlaceManage = SubstCtl<ITabPage>(view, "pagePlaceManage");
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _btnLocExpert = SubstCtl<IButton>(view, "btnLocExpert");
            _btnIntoList = SubstCtl<IButton>(view, "btnIntoList");
            _btnAnalysePlaces = SubstCtl<IButton>(view, "btnAnalysePlaces");
            _lblFilter = SubstCtl<ILabel>(view, "lblFilter");
            _txtFilter = SubstCtl<ITextBox>(view, "txtFilter");
            _ListPlaces = SubstCtl<IListView>(view, "ListPlaces");
            return view;
        }
    }
}
