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
        private ITabPage _pageMerge;
        private IButton _btnAutoSearch;
        private IButton _btnSkip;
        private IProgressBar _ProgressBar1;
        private ITabPage _pageMergeOptions;
        private IGroupBox _rgMode;
        private IGroupBox _grpSearchPersons;
        private ILabel _lblNameAccuracy;
        private ILabel _lblYearInaccuracy;
        private INumericBox _edNameAccuracy;
        private INumericBox _edYearInaccuracy;
        private ICheckBox _chkBirthYear;
        private IRadioButton _radPersons;
        private IRadioButton _radNotes;
        private IRadioButton _radFamilies;
        private IRadioButton _radSources;
        private ICheckBox _chkBookmarkMerged;
        private IGroupBox _grpMergeOther;
        private ICheckBox _chkIndistinctMatching;
        private IButton _btnMergeToRight;
        private IButton _btnMergeToLeft;
        private IButton _btnRec2Select;
        private IButton _btnRec1Select;
        private IButton _btnEditLeft;
        private IButton _btnEditRight;
        private ITextBox _Edit2;
        private ITextBox _Edit1;
        private ILabel _Lab2;
        private ILabel _Lab1;
        private IHyperView _fView1;
        private IHyperView _fView2;

        [Test]
        public void Test_RecMergeController()
        {
            IRecMergeDlg view = CreateMockView();
            var controller = new RecMergeController(view);
            controller.InitView(fBaseWin);

            _chkBookmarkMerged.Checked = true;
            _chkBookmarkMerged.Checked = false;

            _radPersons.Checked = true;

            //CustomWindowTest.SetSelectItemHandler(0);
            //ClickButton("btnRec1Select", form);

            //CustomWindowTest.SetSelectItemHandler(1);
            //ClickButton("btnRec2Select", form);

            controller.SearchDuplicates();

            controller.Skip();
        }

        private IRecMergeDlg CreateMockView()
        {
            var view = Substitute.For<IRecMergeDlg>();

            _grpSearchPersons = SubstCtl<IGroupBox>(view, "grpSearchPersons");
            _grpMergeOther = SubstCtl<IGroupBox>(view, "grpMergeOther");
            _rgMode = SubstCtl<IGroupBox>(view, "rgMode");
            _pageMerge = SubstCtl<ITabPage>(view, "pageMerge");
            _pageMergeOptions = SubstCtl<ITabPage>(view, "pageMergeOptions");
            _btnAutoSearch = SubstCtl<IButton>(view, "btnAutoSearch");
            _btnSkip = SubstCtl<IButton>(view, "btnSkip");
            _radPersons = SubstCtl<IRadioButton>(view, "radPersons");
            _radNotes = SubstCtl<IRadioButton>(view, "radNotes");
            _radFamilies = SubstCtl<IRadioButton>(view, "radFamilies");
            _radSources = SubstCtl<IRadioButton>(view, "radSources");
            _chkIndistinctMatching = SubstCtl<ICheckBox>(view, "chkIndistinctMatching");
            _chkBirthYear = SubstCtl<ICheckBox>(view, "chkBirthYear");
            _lblNameAccuracy = SubstCtl<ILabel>(view, "lblNameAccuracy");
            _lblYearInaccuracy = SubstCtl<ILabel>(view, "lblYearInaccuracy");
            _edNameAccuracy = SubstCtl<INumericBox>(view, "edNameAccuracy");
            _edYearInaccuracy = SubstCtl<INumericBox>(view, "edYearInaccuracy");
            _chkBookmarkMerged = SubstCtl<ICheckBox>(view, "chkBookmarkMerged");
            _btnRec1Select = SubstCtl<IButton>(view, "btnRec1Select");
            _btnRec2Select = SubstCtl<IButton>(view, "btnRec2Select");
            _btnEditLeft = SubstCtl<IButton>(view, "btnEditLeft");
            _btnEditRight = SubstCtl<IButton>(view, "btnEditRight");
            _btnMergeToLeft = SubstCtl<IButton>(view, "btnMergeToLeft");
            _btnMergeToRight = SubstCtl<IButton>(view, "btnMergeToRight");
            _Lab1 = SubstCtl<ILabel>(view, "Lab1");
            _Lab2 = SubstCtl<ILabel>(view, "Lab2");
            _Edit1 = SubstCtl<ITextBox>(view, "Edit1");
            _Edit2 = SubstCtl<ITextBox>(view, "Edit2");
            _ProgressBar1 = SubstCtl<IProgressBar>(view, "ProgressBar1");
            _fView1 = SubstCtl<IHyperView>(view, "fView1");
            _fView2 = SubstCtl<IHyperView>(view, "fView2");

            _fView1.Lines.Returns(new StringList());
            _fView2.Lines.Returns(new StringList());
            return view;
        }
    }
}
