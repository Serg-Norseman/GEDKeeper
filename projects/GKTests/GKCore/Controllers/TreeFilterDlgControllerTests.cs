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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IGroupBox _rgBranchCut;
        private IRadioButton _rbCutNone;
        private IRadioButton _rbCutYears;
        private IRadioButton _rbCutPersons;
        private ILabel _lblYear;
        private INumericBox _edYear;
        private ISheetList _fPersonsList;
        private ILabel _lblRPSources;
        private IComboBox _cmbSource;
        private ICheckBox _chkHideDatesAfterBoundary;
        private ITextBox _txtDateBoundary;

        [Test]
        public void Test_TreeFilterDlgController()
        {
            ITreeFilterDlg view = CreateMockView();
            var controller = new TreeFilterDlgController(view);
            controller.InitView(fBaseWin);

            var filter = new ChartFilter();
            controller.Filter = filter;

            controller.UpdateView();

            _rbCutNone.Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.None, filter.BranchCut);

            _rbCutYears.Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.Years, filter.BranchCut);

            _rbCutPersons.Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.Persons, filter.BranchCut);
        }

        private ITreeFilterDlg CreateMockView()
        {
            var view = Substitute.For<ITreeFilterDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _rgBranchCut = SubstCtl<IGroupBox>(view, "rgBranchCut");
            _rbCutNone = SubstCtl<IRadioButton>(view, "rbCutNone");
            _rbCutYears = SubstCtl<IRadioButton>(view, "rbCutYears");
            _rbCutPersons = SubstCtl<IRadioButton>(view, "rbCutPersons");
            _lblYear = SubstCtl<ILabel>(view, "lblYear");
            _edYear = SubstCtl<INumericBox>(view, "edYear");
            _fPersonsList = SubstCtl<ISheetList>(view, "fPersonsList");
            _lblRPSources = SubstCtl<ILabel>(view, "lblRPSources");
            _cmbSource = SubstCtl<IComboBox>(view, "cmbSource");
            _chkHideDatesAfterBoundary = SubstCtl<ICheckBox>(view, "chkHideDatesAfterBoundary");
            _txtDateBoundary = SubstCtl<ITextBox>(view, "txtDateBoundary");
            return view;
        }
    }
}
