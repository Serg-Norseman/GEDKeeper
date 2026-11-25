/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class TaskEditDlgControllerTests : ControllerTest
    {
        private ITabPage _pageNotes;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblGoal;
        private IComboBox _cmbGoalType;
        private ITextBox _txtGoal;
        private IButton _btnGoalSelect;
        private ILabel _lblPriority;
        private IComboBox _txtPriority;
        private ILabel _lblStartDate;
        private IDateBox _txtStartDate;
        private IDateBox _txtStopDate;
        private ILabel _lblStopDate;
        private ISheetList _fNotesList;

        [Test]
        public void Test_TaskEditDlgController()
        {
            ITaskEditDlg view = CreateMockView();
            var controller = new TaskEditDlgController(view);
            controller.InitView(fBaseWin);

            var taskRecord = new GDMTaskRecord(fBaseWin.Context.Tree);
            controller.TaskRecord = taskRecord;


            _txtPriority.SelectedIndex = 1;
            for (GDMGoalType gt = GDMGoalType.gtIndividual; gt <= GDMGoalType.gtOther; gt++) {
                _cmbGoalType.SelectedIndex = (int)gt;
            }
            controller.Accept();
            Assert.AreEqual(GDMResearchPriority.rpLow, taskRecord.Priority);
            Assert.AreEqual("", taskRecord.StartDate.StringValue);
            Assert.AreEqual("", taskRecord.StopDate.StringValue);


            _txtPriority.SelectedIndex = 1;
            _txtStartDate.NormalizeDate = "01.01.2000";
            _txtStopDate.NormalizeDate = "02.02.2000";
            for (GDMGoalType gt = GDMGoalType.gtIndividual; gt <= GDMGoalType.gtOther; gt++) {
                _cmbGoalType.SelectedIndex = (int)gt;
            }
            controller.Accept();
            Assert.AreEqual(GDMResearchPriority.rpLow, taskRecord.Priority);
            Assert.AreEqual("01 JAN 2000", taskRecord.StartDate.StringValue);
            Assert.AreEqual("02 FEB 2000", taskRecord.StopDate.StringValue);


            _cmbGoalType.SelectedIndex = 3;
            controller.SelectGoal();

            _cmbGoalType.SelectedIndex = 2;
            var src = fBaseWin.Context.Tree.CreateSource();
            RecordSelectDialogStub.SetTestResult(src);
            controller.SelectGoal();

            _cmbGoalType.SelectedIndex = 1;
            var fam = fBaseWin.Context.Tree.CreateFamily();
            RecordSelectDialogStub.SetTestResult(fam);
            controller.SelectGoal();

            _cmbGoalType.SelectedIndex = 0;
            var indi = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(indi);
            controller.SelectGoal();

            controller.Accept();
            controller.UpdateView();
        }

        private ITaskEditDlg CreateMockView()
        {
            var view = Substitute.For<ITaskEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _lblGoal = SubstCtl<ILabel>(view, "lblGoal");
            _lblPriority = SubstCtl<ILabel>(view, "lblPriority");
            _lblStartDate = SubstCtl<ILabel>(view, "lblStartDate");
            _lblStopDate = SubstCtl<ILabel>(view, "lblStopDate");
            _btnGoalSelect = SubstCtl<IButton>(view, "btnGoalSelect");

            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _txtPriority = SubstCtl<IComboBox>(view, "txtPriority");
            _txtStartDate = SubstCtl<IDateBox>(view, "txtStartDate");
            _txtStopDate = SubstCtl<IDateBox>(view, "txtStopDate");
            _cmbGoalType = SubstCtl<IComboBox>(view, "cmbGoalType");
            _txtGoal = SubstCtl<ITextBox>(view, "txtGoal");
            return view;
        }
    }
}
