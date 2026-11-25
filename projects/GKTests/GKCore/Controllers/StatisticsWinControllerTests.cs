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
using GKCore.Stats;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class StatisticsWinControllerTests : ControllerTest
    {
        private IComboBox _cbType;
        private IListView _lvSummary;
#if NETCOREAPP
        private IButton _tbExcelExport;
#else
        private IToolItem _tbExcelExport;
#endif
        private IGraphControl _fGraph;
        private IListView _fListStats;

        [Test]
        public async Task Test_StatisticsWinController()
        {
            IStatisticsWin view = CreateMockView();
            var controller = new StatisticsWinController(view, null);
            controller.InitView(fBaseWin);
            controller.UpdateStatsTypes();

            for (StatsMode sm = StatsMode.smAncestors; sm <= StatsMode.smLast; sm++) {
                _cbType.SelectedIndex = (int)sm;
            }

            controller.UpdateView();

            _cbType.SelectedIndex = 0;
            await controller.ExportToExcel();
        }

        private IStatisticsWin CreateMockView()
        {
            var view = Substitute.For<IStatisticsWin>();
            SubstituteControl<IGroupBox>(view, "grpSummary");
            _cbType = SubstCtl<IComboBox>(view, "cbType");
            _lvSummary = SubstCtl<IListView>(view, "lvSummary");
#if NETCOREAPP
            _tbExcelExport = SubstCtl<IButton>(view, "tbExcelExport");
#else
            _tbExcelExport = SubstCtl<IToolItem>(view, "tbExcelExport");
#endif
            _fGraph = SubstCtl<IGraphControl>(view, "fGraph");
            _fListStats = SubstCtl<IListView>(view, "fListStats");
            return view;
        }
    }
}
