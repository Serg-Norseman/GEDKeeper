/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Charts;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class TreeFilterDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_TreeFilterDlgController()
        {
            ITreeFilterDlg view = CreateMockView();
            var controller = new TreeFilterDlgController(view);
            controller.Init(fBaseWin);

            var filter = new ChartFilter();
            controller.Filter = filter;

            controller.UpdateView();

            view.GetCoreControl<IRadioButton>("rbCutNone").Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.None, filter.BranchCut);

            view.GetCoreControl<IRadioButton>("rbCutYears").Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.Years, filter.BranchCut);

            view.GetCoreControl<IRadioButton>("rbCutPersons").Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.Persons, filter.BranchCut);
        }

        private static ITreeFilterDlg CreateMockView()
        {
            var view = Substitute.For<ITreeFilterDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IGroupBox>(view, "rgBranchCut");
            SubstituteControl<IRadioButton>(view, "rbCutNone");
            SubstituteControl<IRadioButton>(view, "rbCutYears");
            SubstituteControl<ILabel>(view, "lblYear");
            SubstituteControl<IRadioButton>(view, "rbCutPersons");
            SubstituteControl<ILabel>(view, "lblRPSources");

            view.PersonsList.Returns(Substitute.For<ISheetList>());
            view.YearNum.Returns(Substitute.For<INumericBox>());
            view.SourceCombo.Returns(Substitute.For<IComboBox>());
            return view;
        }
    }
}
