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
    public class LocationNameEditDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblTitle;
        private ITextBox _txtTitle;
        private ILabel _lblShortTitle;
        private ITextBox _txtShortTitle;
        private ILabel _lblDate;
        private IDateControl _dateCtl;

        [Test]
        public void Test_LocationNameEditDlgController()
        {
            ILocationNameEditDlg view = CreateMockView();
            var controller = new LocationNameEditDlgController(view);
            controller.InitView(fBaseWin);

            var locationName = new GDMLocationName();
            controller.LocationName = locationName;

            _txtTitle.Text = "sample title";
            _txtShortTitle.Text = "sample short title";

            controller.Accept();
            Assert.AreEqual("sample title", locationName.StringValue);
            Assert.AreEqual("sample short title", locationName.Abbreviation);

            controller.UpdateView();
        }

        private ILocationNameEditDlg CreateMockView()
        {
            var view = Substitute.For<ILocationNameEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblTitle = SubstCtl<ILabel>(view, "lblTitle");
            _txtTitle = SubstCtl<ITextBox>(view, "txtTitle");
            _lblShortTitle = SubstCtl<ILabel>(view, "lblShortTitle");
            _txtShortTitle = SubstCtl<ITextBox>(view, "txtShortTitle");
            _lblDate = SubstCtl<ILabel>(view, "lblDate");
            _dateCtl = SubstCtl<IDateControl>(view, "dateCtl");
            return view;
        }
    }
}
