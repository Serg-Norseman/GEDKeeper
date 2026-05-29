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
        [Test]
        public void Test_TaskEditDlgController()
        {
            ITaskEditDlg view = CreateMockView();
            var controller = new TaskEditDlgController(view);
            controller.Init(fBaseWin);

            var taskRecord = new GDMTaskRecord(fBaseWin.Context.Tree);
            controller.TaskRecord = taskRecord;


            view.Priority.SelectedIndex = 1;
            for (GDMGoalType gt = GDMGoalType.gtIndividual; gt <= GDMGoalType.gtOther; gt++) {
                view.GoalType.SelectedIndex = (int)gt;
            }
            controller.Accept();
            Assert.AreEqual(GDMResearchPriority.rpLow, taskRecord.Priority);
            Assert.AreEqual("", taskRecord.StartDate.StringValue);
            Assert.AreEqual("", taskRecord.StopDate.StringValue);


            view.Priority.SelectedIndex = 1;
            view.StartDate.NormalizeDate = "01.01.2000";
            view.StopDate.NormalizeDate = "02.02.2000";
            for (GDMGoalType gt = GDMGoalType.gtIndividual; gt <= GDMGoalType.gtOther; gt++) {
                view.GoalType.SelectedIndex = (int)gt;
            }
            controller.Accept();
            Assert.AreEqual(GDMResearchPriority.rpLow, taskRecord.Priority);
            Assert.AreEqual("01 JAN 2000", taskRecord.StartDate.StringValue);
            Assert.AreEqual("02 FEB 2000", taskRecord.StopDate.StringValue);


            view.GoalType.SelectedIndex = 3;
            controller.SelectGoal();

            view.GoalType.SelectedIndex = 2;
            var src = fBaseWin.Context.Tree.CreateSource();
            RecordSelectDialogStub.SetTestResult(src);
            controller.SelectGoal();

            view.GoalType.SelectedIndex = 1;
            var fam = fBaseWin.Context.Tree.CreateFamily();
            RecordSelectDialogStub.SetTestResult(fam);
            controller.SelectGoal();

            view.GoalType.SelectedIndex = 0;
            var indi = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(indi);
            controller.SelectGoal();

            controller.Accept();
            controller.UpdateView();
        }

        private static ITaskEditDlg CreateMockView()
        {
            var view = Substitute.For<ITaskEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ILabel>(view, "lblGoal");
            SubstituteControl<ILabel>(view, "lblPriority");
            SubstituteControl<ILabel>(view, "lblStartDate");
            SubstituteControl<ILabel>(view, "lblStopDate");
            SubstituteControl<IButton>(view, "btnGoalSelect");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.Priority.Returns(Substitute.For<IComboBox>());
            view.StartDate.Returns(Substitute.For<IDateBox>());
            view.StopDate.Returns(Substitute.For<IDateBox>());
            view.GoalType.Returns(Substitute.For<IComboBox>());
            view.Goal.Returns(Substitute.For<ITextBox>());
            view.GoalSelect.Returns(Substitute.For<IButton>());
            return view;
        }
    }
}
