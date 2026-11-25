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
    public class TreeCompareControllerTests : ControllerTest
    {
        private ITabPage _pageTreeCompare;
        private ITextBox _ListCompare;
        private IButton _btnClose;
        private IGroupBox _grpMatchType;
        private IRadioButton _radMatchInternal;
        private IRadioButton _radMathExternal;
        private IRadioButton _radAnalysis;
        private ILabel _lblFile;
        private ITextBox _txtCompareFile;
        private IButton _btnFileChoose;
        private IButton _btnMatch;

        [Test]
        public void Test_TreeCompareController()
        {
            ITreeCompareDlg view = CreateMockView();
            var controller = new TreeCompareController(view);
            controller.InitView(fBaseWin);

            _radMatchInternal.Checked = true;
            controller.Match();

            _radAnalysis.Checked = true;
            controller.Match();

            _radMathExternal.Checked = true;

            //SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            controller.SelectExternalFile();
            //controller.Match();

            controller.UpdateView();
        }

        private ITreeCompareDlg CreateMockView()
        {
            var view = Substitute.For<ITreeCompareDlg>();
            _pageTreeCompare = SubstCtl<ITabPage>(view, "pageTreeCompare");
            _ListCompare = SubstCtl<ITextBox>(view, "ListCompare");
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _lblFile = SubstCtl<ILabel>(view, "lblFile");
            _txtCompareFile = SubstCtl<ITextBox>(view, "txtCompareFile");
            _btnFileChoose = SubstCtl<IButton>(view, "btnFileChoose");
            _grpMatchType = SubstCtl<IGroupBox>(view, "grpMatchType");
            _radMatchInternal = SubstCtl<IRadioButton>(view, "radMatchInternal");
            _radMathExternal = SubstCtl<IRadioButton>(view, "radMathExternal");
            _radAnalysis = SubstCtl<IRadioButton>(view, "radAnalysis");
            _btnMatch = SubstCtl<IButton>(view, "btnMatch");
            return view;
        }
    }
}
