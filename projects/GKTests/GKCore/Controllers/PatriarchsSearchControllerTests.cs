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
        private IButton _btnClose;
        private ITabPage _pagePatSearch;
        private IButton _btnPatSearch;
        private ILabel _lblMinGenerations;
        private INumericBox _edMinGens;
        private IButton _btnSetPatriarch;
        private IButton _btnPatriarchsDiagram;
        private ICheckBox _chkWithoutDates;
        private IListView _ListPatriarchs;

        [Test]
        public void Test_PatriarchsSearchController()
        {
            IPatriarchsSearchDlg view = CreateMockView();
            var controller = new PatriarchsSearchController(view);
            controller.InitView(fBaseWin);

            _edMinGens.Value = 1;
            controller.Search();
            controller.SetPatriarch();

            //ClickButton("btnPatriarchsDiagram", form);
            //var pvWin = new FormTester("PatriarchsViewerWin");
            //pvWin.Close();
        }

        private IPatriarchsSearchDlg CreateMockView()
        {
            var view = Substitute.For<IPatriarchsSearchDlg>();
            _pagePatSearch = SubstCtl<ITabPage>(view, "pagePatSearch");
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _btnPatSearch = SubstCtl<IButton>(view, "btnPatSearch");
            _lblMinGenerations = SubstCtl<ILabel>(view, "lblMinGenerations");
            _edMinGens = SubstCtl<INumericBox>(view, "edMinGens");
            _btnSetPatriarch = SubstCtl<IButton>(view, "btnSetPatriarch");
            _btnPatriarchsDiagram = SubstCtl<IButton>(view, "btnPatriarchsDiagram");
            _chkWithoutDates = SubstCtl<ICheckBox>(view, "chkWithoutDates");
            _ListPatriarchs = SubstCtl<IListView>(view, "ListPatriarchs");
            return view;
        }
    }
}
