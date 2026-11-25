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
    public class OrganizerControllerTests : ControllerTest
    {
        private ITabPage _pageAddresses;
        private ITabPage _pageTelephones;
        private ITabPage _pageMails;
        private ITabPage _pageWebs;

        private ISheetList _fAdrList;
        private ISheetList _fPhonesList;
        private ISheetList _fMailsList;
        private ISheetList _fWebsList;

        [Test]
        public void Test_OrganizerController()
        {
            IOrganizerWin view = CreateMockView();
            var controller = new OrganizerController(view);
            controller.InitView(fBaseWin);

            controller.UpdateView();
        }

        private IOrganizerWin CreateMockView()
        {
            var view = Substitute.For<IOrganizerWin>();
            _pageAddresses = SubstCtl<ITabPage>(view, "pageAddresses");
            _pageTelephones = SubstCtl<ITabPage>(view, "pageTelephones");
            _pageMails = SubstCtl<ITabPage>(view, "pageMails");
            _pageWebs = SubstCtl<ITabPage>(view, "pageWebs");

            _fAdrList = SubstCtl<ISheetList>(view, "fAdrList");
            _fPhonesList = SubstCtl<ISheetList>(view, "fPhonesList");
            _fMailsList = SubstCtl<ISheetList>(view, "fMailsList");
            _fWebsList = SubstCtl<ISheetList>(view, "fWebsList");
            return view;
        }
    }
}
