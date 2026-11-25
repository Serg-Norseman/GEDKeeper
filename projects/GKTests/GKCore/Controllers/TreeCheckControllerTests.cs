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
        private IButton _btnClose;
        private ITabPage _pageTreeCheck;
        private IButton _btnAnalyseBase;
        private IButton _btnBaseRepair;
        private IMenuItem _miDetails;
        private IMenuItem _miGoToRecord;
        private IMenuItem _miCopyXRef;
        private IListView _ListChecks;
        private ITabPage _pageOptions;
        private ICheckBox _chkCheckPersonPlaces;
        private ICheckBox _chkCheckCensuses;
        private ICheckBox _chkCheckLinks;

        [Test]
        public async Task Test_TreeCheckController()
        {
            ITreeCheckDlg view = CreateMockView();
            var controller = new TreeCheckController(view);
            controller.InitView(fBaseWin);

            controller.UpdateView();

            controller.CheckBase();
            await controller.Repair();
        }

        private ITreeCheckDlg CreateMockView()
        {
            var view = Substitute.For<ITreeCheckDlg>();
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _pageTreeCheck = SubstCtl<ITabPage>(view, "pageTreeCheck");
            _btnAnalyseBase = SubstCtl<IButton>(view, "btnAnalyseBase");
            _btnBaseRepair = SubstCtl<IButton>(view, "btnBaseRepair");
            _miDetails = SubstCtl<IMenuItem>(view, "miDetails");
            _miGoToRecord = SubstCtl<IMenuItem>(view, "miGoToRecord");
            _miCopyXRef = SubstCtl<IMenuItem>(view, "miCopyXRef");
            _ListChecks = SubstCtl<IListView>(view, "ListChecks");

            _pageOptions = SubstCtl<ITabPage>(view, "pageOptions");
            _chkCheckPersonPlaces = SubstCtl<ICheckBox>(view, "chkCheckPersonPlaces");
            _chkCheckCensuses = SubstCtl<ICheckBox>(view, "chkCheckCensuses");
            _chkCheckLinks = SubstCtl<ICheckBox>(view, "chkCheckLinks");

            return view;
        }
    }
}
