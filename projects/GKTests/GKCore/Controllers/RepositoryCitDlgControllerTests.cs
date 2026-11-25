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
using GKCore.Lists;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class RepositoryCitDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnRepositoryAdd;
        private ILabel _lblRepository;
        private IComboBox _cmbRepository;
        private ITabPage _pageCallNumbers;
        private ISheetList _fCallNumbersList;

        [Test]
        public void Test_RepositoryCitDlgController()
        {
            IRepositoryCitEditDlg view = CreateMockView();
            var controller = new RepositoryCitDlgController(view);
            controller.InitView(fBaseWin);

            var repositoryCitation = new GDMRepositoryCitation();
            controller.RepositoryCitation = repositoryCitation;

            controller.Accept();

            controller.UpdateView();
        }

        private IRepositoryCitEditDlg CreateMockView()
        {
            var view = Substitute.For<IRepositoryCitEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnRepositoryAdd = SubstCtl<IButton>(view, "btnRepositoryAdd");
            _lblRepository = SubstCtl<ILabel>(view, "lblRepository");
            _cmbRepository = SubstCtl<IComboBox>(view, "cmbRepository");
            _pageCallNumbers = SubstCtl<ITabPage>(view, "pageCallNumbers");
            _fCallNumbersList = SubstCtl<ISheetList>(view, "fCallNumbersList");
            return view;
        }
    }
}
