/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class LocationLinkEditDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnLocationAdd;
        private ILabel _lblLocation;
        private ITextBox _txtTopLevel;
        private ILabel _lblDate;
        private IDateControl _dateCtl;

        [Test]
        public void Test_LocationLinkEditDlgController()
        {
            ILocationLinkEditDlg view = CreateMockView();
            var controller = new LocationLinkEditDlgController(view);
            controller.InitView(fBaseWin);

            var locationLink = new GDMLocationLink();
            controller.LocationLink = locationLink;

            controller.UpdateLocationName();
            controller.UpdateView();
        }

        private ILocationLinkEditDlg CreateMockView()
        {
            var view = Substitute.For<ILocationLinkEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnLocationAdd = SubstCtl<IButton>(view, "btnLocationAdd");
            _lblLocation = SubstCtl<ILabel>(view, "lblLocation");
            _txtTopLevel = SubstCtl<ITextBox>(view, "txtTopLevel");
            _lblDate = SubstCtl<ILabel>(view, "lblDate");
            _dateCtl = SubstCtl<IDateControl>(view, "dateCtl");
            return view;
        }
    }
}
