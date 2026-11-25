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
        private IButton _btnClose;
        private ITabPage _pageFamilyGroups;
        private ITreeView _tvGroups;
        private ILogChart _gkLogChart1;
        private IButton _btnAnalyseGroups;
        private IMenuItem _miDetails;
        private IMenuItem _miGoToRecord;
        private IMenuItem _miCopyXRef;
        private ITabPage _pageDataQuality;
        private IMenuItem _miDQRefresh;
        private IMenuItem _miDQResetFilter;

        [Test]
        public void Test_FragmentSearchController()
        {
            IFragmentSearchDlg view = CreateMockView();
            var controller = new FragmentSearchController(view);
            controller.InitView(fBaseWin);

            controller.CheckGroups();
        }

        private IFragmentSearchDlg CreateMockView()
        {
            var view = Substitute.For<IFragmentSearchDlg>();
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _pageFamilyGroups = SubstCtl<ITabPage>(view, "pageFamilyGroups");
            _tvGroups = SubstCtl<ITreeView>(view, "tvGroups");
            _gkLogChart1 = SubstCtl<ILogChart>(view, "gkLogChart1");
            _btnAnalyseGroups = SubstCtl<IButton>(view, "btnAnalyseGroups");
            _miDetails = SubstCtl<IMenuItem>(view, "miDetails");
            _miGoToRecord = SubstCtl<IMenuItem>(view, "miGoToRecord");
            _miCopyXRef = SubstCtl<IMenuItem>(view, "miCopyXRef");
            _pageDataQuality = SubstCtl<ITabPage>(view, "pageDataQuality");
            _miDQRefresh = SubstCtl<IMenuItem>(view, "miDQRefresh");
            _miDQResetFilter = SubstCtl<IMenuItem>(view, "miDQResetFilter");
            return view;
        }
    }
}
