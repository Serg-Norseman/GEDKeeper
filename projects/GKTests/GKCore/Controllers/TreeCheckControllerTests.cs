/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class TreeCheckControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_TreeCheckController()
        {
            ITreeCheckDlg view = CreateMockView();
            var controller = new TreeCheckController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();

            controller.CheckBase();
            await controller.Repair();
        }

        private static ITreeCheckDlg CreateMockView()
        {
            var view = Substitute.For<ITreeCheckDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeCheck");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnAnalyseBase");
            SubstituteControl<IButton>(view, "btnBaseRepair");
            SubstituteControl<IMenuItem>(view, "miDetails");
            SubstituteControl<IMenuItem>(view, "miGoToRecord");
            SubstituteControl<IMenuItem>(view, "miCopyXRef");

            SubstituteControl<ITabPage>(view, "pageOptions");
            SubstituteControl<ICheckBox>(view, "chkCheckPersonPlaces");
            SubstituteControl<ICheckBox>(view, "chkCheckCensuses");
            SubstituteControl<ICheckBox>(view, "chkCheckLinks");

            view.ChecksList.Returns(Substitute.For<IListView>());
            return view;
        }
    }
}
