/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
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
    public class ResearchEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_ResearchEditDlgController()
        {
            IResearchEditDlg view = CreateMockView();
            var controller = new ResearchEditDlgController(view);
            controller.Init(fBaseWin);

            var resRecord = new GDMResearchRecord(fBaseWin.Context.Tree);
            controller.ResearchRecord = resRecord;

            view.Name.Text = "sample text";
            view.Priority.SelectedIndex = 1;
            view.Status.SelectedIndex = 1;
            view.Percent.Value = 11;
            view.StartDate.NormalizeDate = "01.01.2000";
            view.StopDate.NormalizeDate = "02.02.2000";
            controller.Accept();
            Assert.AreEqual("sample text", resRecord.ResearchName);
            Assert.AreEqual(GDMResearchPriority.rpLow, resRecord.Priority);
            Assert.AreEqual(GDMResearchStatus.rsInProgress, resRecord.Status);
            Assert.AreEqual(11, resRecord.Percent);
            Assert.AreEqual("01 JAN 2000", resRecord.StartDate.StringValue);
            Assert.AreEqual("02 FEB 2000", resRecord.StopDate.StringValue);

            // tasks
            var taskModel = ((ResTasksListModel)view.TasksList.ListModel);
            Assert.AreEqual(0, resRecord.Tasks.Count);
            var task = fBaseWin.Context.Tree.CreateTask();
            RecordSelectDialogStub.SetTestResult(task);
            await taskModel.Modify(view.TasksList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, resRecord.Tasks.Count);

            //await taskModel.Modify(view.TasksList, new ModifyEventArgs(RecordAction.raEdit, resRecord.Tasks[0]));
            //Assert.AreEqual(1, resRecord.Tasks.Count);

            StdDialogsStub.SetQuestionResult(true);
            await taskModel.Modify(view.TasksList, new ModifyEventArgs(RecordAction.raDelete, resRecord.Tasks[0]));
            Assert.AreEqual(0, resRecord.Tasks.Count);

            // communications
            var commModel = ((ResCommunicationsListModel)view.CommunicationsList.ListModel);
            Assert.AreEqual(0, resRecord.Communications.Count);
            var comm = fBaseWin.Context.Tree.CreateCommunication();
            RecordSelectDialogStub.SetTestResult(comm);
            await commModel.Modify(view.CommunicationsList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, resRecord.Communications.Count);

            //await commModel.Modify(view.CommunicationsList, new ModifyEventArgs(RecordAction.raEdit, resRecord.Communications[0]));
            //Assert.AreEqual(1, resRecord.Communications.Count);

            StdDialogsStub.SetQuestionResult(true);
            await commModel.Modify(view.CommunicationsList, new ModifyEventArgs(RecordAction.raDelete, resRecord.Communications[0]));
            Assert.AreEqual(0, resRecord.Communications.Count);

            // groups
            var grpModel = ((ResGroupsListModel)view.GroupsList.ListModel);
            Assert.AreEqual(0, resRecord.Groups.Count);
            var grp = fBaseWin.Context.Tree.CreateGroup();
            RecordSelectDialogStub.SetTestResult(grp);
            await grpModel.Modify(view.GroupsList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, resRecord.Groups.Count);

            //await grpModel.Modify(view.GroupsList, new ModifyEventArgs(RecordAction.raEdit, resRecord.Groups[0]));
            //Assert.AreEqual(1, resRecord.Groups.Count);

            StdDialogsStub.SetQuestionResult(true);
            await grpModel.Modify(view.GroupsList, new ModifyEventArgs(RecordAction.raDelete, resRecord.Groups[0]));
            Assert.AreEqual(0, resRecord.Groups.Count);

            controller.Accept();
            controller.UpdateView();
        }

        private static IResearchEditDlg CreateMockView()
        {
            var view = Substitute.For<IResearchEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageTasks");
            SubstituteControl<ITabPage>(view, "pageCommunications");
            SubstituteControl<ITabPage>(view, "pageGroups");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblPriority");
            SubstituteControl<ILabel>(view, "lblStatus");
            SubstituteControl<ILabel>(view, "lblPercent");
            SubstituteControl<ILabel>(view, "lblStartDate");
            SubstituteControl<ILabel>(view, "lblStopDate");

            view.TasksList.Returns(Substitute.For<ISheetList>());
            view.CommunicationsList.Returns(Substitute.For<ISheetList>());
            view.GroupsList.Returns(Substitute.For<ISheetList>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Priority.Returns(Substitute.For<IComboBox>());
            view.Status.Returns(Substitute.For<IComboBox>());
            view.StartDate.Returns(Substitute.For<IDateBox>());
            view.StopDate.Returns(Substitute.For<IDateBox>());
            view.Percent.Returns(Substitute.For<INumericBox>());
            return view;
        }
    }
}
