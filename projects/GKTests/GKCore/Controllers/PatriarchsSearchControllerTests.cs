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
    public class PatriarchsSearchControllerTests : ControllerTest
    {
        [Test]
        public void Test_PatriarchsSearchController()
        {
            IPatriarchsSearchDlg view = CreateMockView();
            var controller = new PatriarchsSearchController(view);
            controller.Init(fBaseWin);

            view.MinGensNum.Value = 1;
            controller.Search();
            controller.SetPatriarch();

            //ClickButton("btnPatriarchsDiagram", form);
            //var pvWin = new FormTester("PatriarchsViewerWin");
            //pvWin.Close();
        }

        private static IPatriarchsSearchDlg CreateMockView()
        {
            var view = Substitute.For<IPatriarchsSearchDlg>();
            SubstituteControl<ITabPage>(view, "pagePatSearch");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<ILabel>(view, "lblMinGenerations");
            SubstituteControl<IButton>(view, "btnSetPatriarch");
            SubstituteControl<IButton>(view, "btnPatSearch");
            SubstituteControl<ICheckBox>(view, "chkWithoutDates");
            SubstituteControl<IButton>(view, "btnPatriarchsDiagram");

            view.MinGensNum.Returns(Substitute.For<INumericBox>());
            view.WithoutDatesCheck.Returns(Substitute.For<ICheckBox>());
            view.PatriarchsList.Returns(Substitute.For<IListView>());
            return view;
        }
    }
}
