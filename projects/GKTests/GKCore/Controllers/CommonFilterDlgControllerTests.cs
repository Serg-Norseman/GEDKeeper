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
    public class CommonFilterDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_CommonFilterDlgController()
        {
            ICommonFilterDlg view = CreateMockView();
            var listMan = new NoteListModel(fBaseWin.Context);
            var controller = new CommonFilterDlgController(view, listMan);

            controller.Accept();
            controller.UpdateView();
            controller.Reset();
        }

        private static ICommonFilterDlg CreateMockView()
        {
            var view = Substitute.For<ICommonFilterDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IButton>(view, "btnReset");
            SubstituteControl<ITabPage>(view, "pageFieldsFilter");

            view.FilterGrid.Returns(Substitute.For<IFilterGridView>());
            return view;
        }
    }
}
