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
        [Test]
        public void Test_OrganizerController()
        {
            IOrganizerWin view = CreateMockView();
            var controller = new OrganizerController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();
        }

        private static IOrganizerWin CreateMockView()
        {
            var view = Substitute.For<IOrganizerWin>();
            SubstituteControl<ITabPage>(view, "pageAddresses");
            SubstituteControl<ITabPage>(view, "pageTelephones");
            SubstituteControl<ITabPage>(view, "pageMails");
            SubstituteControl<ITabPage>(view, "pageWebs");

            view.AdrList.Returns(Substitute.For<ISheetList>());
            view.PhonesList.Returns(Substitute.For<ISheetList>());
            view.MailsList.Returns(Substitute.For<ISheetList>());
            view.WebsList.Returns(Substitute.For<ISheetList>());
            return view;
        }
    }
}
