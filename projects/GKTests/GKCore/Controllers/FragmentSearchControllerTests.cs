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
    public class FragmentSearchControllerTests : ControllerTest
    {
        [Test]
        public void Test_FragmentSearchController()
        {
            IFragmentSearchDlg view = CreateMockView();
            var controller = new FragmentSearchController(view);
            controller.Init(fBaseWin);

            controller.CheckGroups();
        }

        private static IFragmentSearchDlg CreateMockView()
        {
            var view = Substitute.For<IFragmentSearchDlg>();
            SubstituteControl<IMenuItem>(view, "miDetails");
            SubstituteControl<IMenuItem>(view, "miGoToRecord");
            SubstituteControl<IMenuItem>(view, "miCopyXRef");
            SubstituteControl<IMenuItem>(view, "miDQRefresh");
            SubstituteControl<IMenuItem>(view, "miDQResetFilter");
            SubstituteControl<ITabPage>(view, "pageFamilyGroups");
            SubstituteControl<IButton>(view, "btnAnalyseGroups");
            SubstituteControl<ITabPage>(view, "pageDataQuality");
            return view;
        }
    }
}
