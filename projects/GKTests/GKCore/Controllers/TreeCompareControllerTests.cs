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
        [Test]
        public void Test_TreeCompareController()
        {
            ITreeCompareDlg view = CreateMockView();
            var controller = new TreeCompareController(view);
            controller.Init(fBaseWin);

            view.GetCoreControl<IRadioButton>("radMatchInternal").Checked = true;
            controller.Match();

            view.GetCoreControl<IRadioButton>("radAnalysis").Checked = true;
            controller.Match();

            view.GetCoreControl<IRadioButton>("radMathExternal").Checked = true;

            //SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            controller.SelectExternalFile();
            //controller.Match();

            controller.UpdateView();
        }

        private static ITreeCompareDlg CreateMockView()
        {
            var view = Substitute.For<ITreeCompareDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeCompare");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<ILabel>(view, "lblFile");
            SubstituteControl<IButton>(view, "btnFileChoose");
            SubstituteControl<IGroupBox>(view, "grpMatchType");
            SubstituteControl<IRadioButton>(view, "radMatchInternal");
            SubstituteControl<IRadioButton>(view, "radMathExternal");
            SubstituteControl<IRadioButton>(view, "radAnalysis");
            SubstituteControl<IButton>(view, "btnMatch");

            view.ExternalBase.Returns(Substitute.For<ITextBox>());
            view.CompareOutput.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
