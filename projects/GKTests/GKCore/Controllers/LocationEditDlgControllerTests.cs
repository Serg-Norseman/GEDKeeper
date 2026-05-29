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
    public class LocationEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_LocationEditDlgController()
        {
            ILocationEditDlg view = CreateMockView();
            var controller = new LocationEditDlgController(view);
            controller.Init(fBaseWin);

            var locRec = fBaseWin.Context.Tree.CreateLocation();

            controller.LocationRecord = locRec;
            Assert.AreEqual(locRec, controller.LocationRecord);

            view.Name.Text = "sample location";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample location", locRec.LocationName);

            var geoPoint = controller.GetSelectedGeoPoint();
            Assert.AreEqual(null, geoPoint);

            controller.Search();

            controller.SelectCoords();

            controller.SelectName();

            controller.SelectGeoPoint();

            controller.ShowOnMap();
        }

        private static ILocationEditDlg CreateMockView()
        {
            var view = Substitute.For<ILocationEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageHistory");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblLatitude");
            SubstituteControl<ILabel>(view, "lblLongitude");
            SubstituteControl<IButton>(view, "btnShowOnMap");
            SubstituteControl<IGroupBox>(view, "grpSearch");
            SubstituteControl<IButton>(view, "btnSearch");
            SubstituteControl<IButton>(view, "btnSelect");
            SubstituteControl<IButton>(view, "btnSelectName");
            SubstituteControl<IButton>(view, "btnSelectCursor");
            SubstituteControl<IGroupBox>(view, "pageHistNames");
            SubstituteControl<IGroupBox>(view, "pageHistLinks");

            view.MapBrowser.Returns(Substitute.For<IMapBrowser>());
            view.NamesList.Returns(Substitute.For<ISheetList>());
            view.LinksList.Returns(Substitute.For<ISheetList>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.GeoCoordsList.Returns(Substitute.For<IListView>());
            return view;
        }
    }
}
