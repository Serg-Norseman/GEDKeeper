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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageCommon;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblLatitude;
        private ITextBox _txtLatitude;
        private ILabel _lblLongitude;
        private ITextBox _txtLongitude;
        private IGroupBox _grpSearch;
        private IListView _ListGeoCoords;
        private IButton _btnSearch;
        private IButton _btnSelect;
        private IButton _btnSelectName;
        private IButton _btnSelectCursor;
        private IButton _btnShowOnMap;
        private IMapBrowser _fMapBrowser;
        private ISheetList _fMediaList;
        private ISheetList _fNamesList;
        private ISheetList _fLinksList;
        private ISheetList _fNotesList;
        private ITabPage _pageHistory;
        private IGroupBox _pageHistNames;
        private IGroupBox _pageHistLinks;

        [Test]
        public void Test_LocationEditDlgController()
        {
            ILocationEditDlg view = CreateMockView();
            var controller = new LocationEditDlgController(view);
            controller.InitView(fBaseWin);

            var locRec = fBaseWin.Context.Tree.CreateLocation();

            controller.LocationRecord = locRec;
            Assert.AreEqual(locRec, controller.LocationRecord);

            _txtName.Text = "sample location";

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

        private ILocationEditDlg CreateMockView()
        {
            var view = Substitute.For<ILocationEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _pageHistory = SubstCtl<ITabPage>(view, "pageHistory");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _lblLatitude = SubstCtl<ILabel>(view, "lblLatitude");
            _txtLatitude = SubstCtl<ITextBox>(view, "txtLatitude");
            _lblLongitude = SubstCtl<ILabel>(view, "lblLongitude");
            _txtLongitude = SubstCtl<ITextBox>(view, "txtLongitude");
            _btnShowOnMap = SubstCtl<IButton>(view, "btnShowOnMap");
            _grpSearch = SubstCtl<IGroupBox>(view, "grpSearch");
            _btnSearch = SubstCtl<IButton>(view, "btnSearch");
            _btnSelect = SubstCtl<IButton>(view, "btnSelect");
            _btnSelectName = SubstCtl<IButton>(view, "btnSelectName");
            _btnSelectCursor = SubstCtl<IButton>(view, "btnSelectCursor");
            _pageHistNames = SubstCtl<IGroupBox>(view, "pageHistNames");
            _pageHistLinks = SubstCtl<IGroupBox>(view, "pageHistLinks");

            _fMapBrowser = SubstCtl<IMapBrowser>(view, "fMapBrowser");
            _fNamesList = SubstCtl<ISheetList>(view, "fNamesList");
            _fLinksList = SubstCtl<ISheetList>(view, "fLinksList");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            _ListGeoCoords = SubstCtl<IListView>(view, "ListGeoCoords");
            return view;
        }
    }
}
