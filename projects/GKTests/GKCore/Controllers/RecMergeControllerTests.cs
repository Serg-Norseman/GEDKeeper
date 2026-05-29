/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class RecMergeControllerTests : ControllerTest
    {
        [Test]
        public void Test_RecMergeController()
        {
            IRecMergeDlg view = CreateMockView();
            var controller = new RecMergeController(view);
            controller.Init(fBaseWin);

            view.GetCoreControl<ICheckBox>("chkBookmarkMerged").Checked = true;
            view.GetCoreControl<ICheckBox>("chkBookmarkMerged").Checked = false;

            view.GetCoreControl<IRadioButton>("radPersons").Checked = true;

            //CustomWindowTest.SetSelectItemHandler(0);
            //ClickButton("btnRec1Select", form);

            //CustomWindowTest.SetSelectItemHandler(1);
            //ClickButton("btnRec2Select", form);

            controller.SearchDuplicates();

            controller.Skip();
        }

        private static IRecMergeDlg CreateMockView()
        {
            var view = Substitute.For<IRecMergeDlg>();

            SubstituteControl<IGroupBox>(view, "grpSearchPersons");
            SubstituteControl<IGroupBox>(view, "grpMergeOther");
            SubstituteControl<IGroupBox>(view, "rgMode");
            SubstituteControl<ITabPage>(view, "pageMerge");
            SubstituteControl<ITabPage>(view, "pageMergeOptions");
            SubstituteControl<IButton>(view, "btnAutoSearch");
            SubstituteControl<IButton>(view, "btnSkip");
            SubstituteControl<IRadioButton>(view, "radPersons");
            SubstituteControl<IRadioButton>(view, "radNotes");
            SubstituteControl<IRadioButton>(view, "radFamilies");
            SubstituteControl<IRadioButton>(view, "radSources");
            SubstituteControl<ICheckBox>(view, "chkIndistinctMatching");
            SubstituteControl<ICheckBox>(view, "chkBirthYear");
            SubstituteControl<ILabel>(view, "lblNameAccuracy");
            SubstituteControl<ILabel>(view, "lblYearInaccuracy");
            SubstituteControl<ICheckBox>(view, "chkBookmarkMerged");
            SubstituteControl<IButton>(view, "btnRec1Select");
            SubstituteControl<IButton>(view, "btnRec2Select");
            SubstituteControl<IButton>(view, "btnEditLeft");
            SubstituteControl<IButton>(view, "btnEditRight");

            view.View1.Returns(Substitute.For<IHyperView>());
            view.View2.Returns(Substitute.For<IHyperView>());
            view.SkipBtn.Returns(Substitute.For<IButton>());
            view.ProgressBar.Returns(Substitute.For<IProgressBar>());
            view.IndistinctMatchingChk.Returns(Substitute.For<ICheckBox>());
            view.NameAccuracyNum.Returns(Substitute.For<INumericBox>());
            view.BirthYearChk.Returns(Substitute.For<ICheckBox>());
            view.YearInaccuracyNum.Returns(Substitute.For<INumericBox>());

            view.View1.Lines.Returns(new StringList());
            view.View2.Lines.Returns(new StringList());
            return view;
        }
    }
}
