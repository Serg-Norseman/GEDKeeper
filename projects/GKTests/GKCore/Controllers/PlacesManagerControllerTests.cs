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
        [Test]
        public void Test_PlacesManagerController()
        {
            IPlacesManagerDlg view = CreateMockView();
            var controller = new PlacesManagerController(view);
            controller.Init(fBaseWin);

            controller.CheckPlaces();
            controller.CreateLocationRecord(view.PlacesList.GetSelectedItems());
        }

        private static IPlacesManagerDlg CreateMockView()
        {
            var view = Substitute.For<IPlacesManagerDlg>();
            SubstituteControl<ITabPage>(view, "pagePlaceManage");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnLocExpert");
            SubstituteControl<IButton>(view, "btnIntoList");
            SubstituteControl<IButton>(view, "btnAnalysePlaces");
            SubstituteControl<ILabel>(view, "lblFilter");

            view.FilterBox.Returns(Substitute.For<ITextBox>());
            view.PlacesList.Returns(Substitute.For<IListView>());
            return view;
        }
    }
}
