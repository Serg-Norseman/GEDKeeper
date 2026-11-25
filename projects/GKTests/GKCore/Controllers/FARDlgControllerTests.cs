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
    public class FARDlgControllerTests : ControllerTest
    {
        private ILabel _lblPattern;
        private IComboBox _cmbPattern;
        private ILabel _lblReplacement;
        private IComboBox _cmbReplacement;
        private ICheckBox _chkMatchCase;
        private ICheckBox _chkMatchWildcards;
        private ICheckBox _chkWholeWord;
        private IGroupBox _gbFilters;
        private ILabel _lblRecord;
        private IComboBox _cmbRecord;
        private ILabel _lblProperty;
        private IComboBox _cmbProperty;
        private IButton _btnPrev;
        private IButton _btnNext;
        private IButton _btnReplace;
        private IButton _btnReplaceAll;

        [Test]
        public void Test_FARDlgController()
        {
            IFARDlg view = CreateMockView();
            var controller = new FARDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateView();

            controller.Prev();
            controller.Next();
        }

        private IFARDlg CreateMockView()
        {
            var view = Substitute.For<IFARDlg>();
            _lblPattern = SubstCtl<ILabel>(view, "lblPattern");
            _cmbPattern = SubstCtl<IComboBox>(view, "cmbPattern");
            _lblReplacement = SubstCtl<ILabel>(view, "lblReplacement");
            _cmbReplacement = SubstCtl<IComboBox>(view, "cmbReplacement");
            _chkMatchCase = SubstCtl<ICheckBox>(view, "chkMatchCase");
            _chkMatchWildcards = SubstCtl<ICheckBox>(view, "chkMatchWildcards");
            _chkWholeWord = SubstCtl<ICheckBox>(view, "chkWholeWord");
            _gbFilters = SubstCtl<IGroupBox>(view, "gbFilters");
            _lblRecord = SubstCtl<ILabel>(view, "lblRecord");
            _cmbRecord = SubstCtl<IComboBox>(view, "cmbRecord");
            _lblProperty = SubstCtl<ILabel>(view, "lblProperty");
            _cmbProperty = SubstCtl<IComboBox>(view, "cmbProperty");
            _btnPrev = SubstCtl<IButton>(view, "btnPrev");
            _btnNext = SubstCtl<IButton>(view, "btnNext");
            _btnReplace = SubstCtl<IButton>(view, "btnReplace");
            _btnReplaceAll = SubstCtl<IButton>(view, "btnReplaceAll");
            return view;
        }
    }
}
