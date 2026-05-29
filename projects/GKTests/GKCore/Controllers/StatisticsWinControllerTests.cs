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
        [Test]
        public async Task Test_StatisticsWinController()
        {
            IStatisticsWin view = CreateMockView();
            var controller = new StatisticsWinController(view, null);
            controller.Init(fBaseWin);
            controller.UpdateStatsTypes();

            for (StatsMode sm = StatsMode.smAncestors; sm <= StatsMode.smLast; sm++) {
                view.StatsType.SelectedIndex = (int)sm;
            }

            controller.UpdateView();

            view.StatsType.SelectedIndex = 0;
            await controller.ExportToExcel();
        }

        private static IStatisticsWin CreateMockView()
        {
            var view = Substitute.For<IStatisticsWin>();
            SubstituteControl<IGroupBox>(view, "grpSummary");
            SubstituteControl<IButton>(view, "tbExcelExport");

            view.Graph.Returns(Substitute.For<IGraphControl>());
            view.ListStats.Returns(Substitute.For<IListView>());
            view.Summary.Returns(Substitute.For<IListView>());
            view.StatsType.Returns(Substitute.For<IComboBox>());
            return view;
        }
    }
}
