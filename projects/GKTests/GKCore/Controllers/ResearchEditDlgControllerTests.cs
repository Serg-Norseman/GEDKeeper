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
        private ITabPage _pageTasks;
        private ITabPage _pageCommunications;
        private ITabPage _pageGroups;
        private ITabPage _pageNotes;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITextBox _txtName;
        private ILabel _lblName;
        private ILabel _lblPriority;
        private IComboBox _cmbPriority;
        private ILabel _lblStatus;
        private IComboBox _cmbStatus;
        private ILabel _lblStartDate;
        private IDateBox _txtStartDate;
        private ILabel _lblStopDate;
        private IDateBox _txtStopDate;
        private ILabel _lblPercent;
        private INumericBox _nudPercent;
        private ISheetList _fTasksList;
        private ISheetList _fCommunicationsList;
        private ISheetList _fGroupsList;
        private ISheetList _fNotesList;

        [Test]
        public async Task Test_ResearchEditDlgController()
        {
            IResearchEditDlg view = CreateMockView();
            var controller = new ResearchEditDlgController(view);
            controller.InitView(fBaseWin);

            var resRecord = new GDMResearchRecord(fBaseWin.Context.Tree);
            controller.ResearchRecord = resRecord;

            _txtName.Text = "sample text";
            _cmbPriority.SelectedIndex = 1;
            _cmbStatus.SelectedIndex = 1;
            _nudPercent.Value = 11;
            _txtStartDate.NormalizeDate = "01.01.2000";
            _txtStopDate.NormalizeDate = "02.02.2000";
            controller.Accept();
            Assert.AreEqual("sample text", resRecord.ResearchName);
            Assert.AreEqual(GDMResearchPriority.rpLow, resRecord.Priority);
            Assert.AreEqual(GDMResearchStatus.rsInProgress, resRecord.Status);
            Assert.AreEqual(11, resRecord.Percent);
            Assert.AreEqual("01 JAN 2000", resRecord.StartDate.StringValue);
            Assert.AreEqual("02 FEB 2000", resRecord.StopDate.StringValue);

            // tasks
            var taskModel = ((ResTasksListModel)_fTasksList.ListModel);
            Assert.AreEqual(0, resRecord.Tasks.Count);
            var task = fBaseWin.Context.Tree.CreateTask();
            RecordSelectDialogStub.SetTestResult(task);
            await taskModel.Modify(_fTasksList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, resRecord.Tasks.Count);

            //await taskModel.Modify(_fTasksList, new ModifyEventArgs(RecordAction.raEdit, resRecord.Tasks[0]));
            //Assert.AreEqual(1, resRecord.Tasks.Count);

            StdDialogsStub.SetQuestionResult(true);
            await taskModel.Modify(_fTasksList, new ModifyEventArgs(RecordAction.raDelete, resRecord.Tasks[0]));
            Assert.AreEqual(0, resRecord.Tasks.Count);

            // communications
            var commModel = ((ResCommunicationsListModel)_fCommunicationsList.ListModel);
            Assert.AreEqual(0, resRecord.Communications.Count);
            var comm = fBaseWin.Context.Tree.CreateCommunication();
            RecordSelectDialogStub.SetTestResult(comm);
            await commModel.Modify(_fCommunicationsList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, resRecord.Communications.Count);

            //await commModel.Modify(_fCommunicationsList, new ModifyEventArgs(RecordAction.raEdit, resRecord.Communications[0]));
            //Assert.AreEqual(1, resRecord.Communications.Count);

            StdDialogsStub.SetQuestionResult(true);
            await commModel.Modify(_fCommunicationsList, new ModifyEventArgs(RecordAction.raDelete, resRecord.Communications[0]));
            Assert.AreEqual(0, resRecord.Communications.Count);

            // groups
            var grpModel = ((ResGroupsListModel)_fGroupsList.ListModel);
            Assert.AreEqual(0, resRecord.Groups.Count);
            var grp = fBaseWin.Context.Tree.CreateGroup();
            RecordSelectDialogStub.SetTestResult(grp);
            await grpModel.Modify(_fGroupsList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, resRecord.Groups.Count);

            //await grpModel.Modify(_fGroupsList, new ModifyEventArgs(RecordAction.raEdit, resRecord.Groups[0]));
            //Assert.AreEqual(1, resRecord.Groups.Count);

            StdDialogsStub.SetQuestionResult(true);
            await grpModel.Modify(_fGroupsList, new ModifyEventArgs(RecordAction.raDelete, resRecord.Groups[0]));
            Assert.AreEqual(0, resRecord.Groups.Count);

            controller.Accept();
            controller.UpdateView();
        }

        private IResearchEditDlg CreateMockView()
        {
            var view = Substitute.For<IResearchEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageTasks = SubstCtl<ITabPage>(view, "pageTasks");
            _pageCommunications = SubstCtl<ITabPage>(view, "pageCommunications");
            _pageGroups = SubstCtl<ITabPage>(view, "pageGroups");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _lblPriority = SubstCtl<ILabel>(view, "lblPriority");
            _lblStatus = SubstCtl<ILabel>(view, "lblStatus");
            _lblPercent = SubstCtl<ILabel>(view, "lblPercent");
            _lblStartDate = SubstCtl<ILabel>(view, "lblStartDate");
            _lblStopDate = SubstCtl<ILabel>(view, "lblStopDate");

            _fTasksList = SubstCtl<ISheetList>(view, "fTasksList");
            _fCommunicationsList = SubstCtl<ISheetList>(view, "fCommunicationsList");
            _fGroupsList = SubstCtl<ISheetList>(view, "fGroupsList");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _cmbPriority = SubstCtl<IComboBox>(view, "cmbPriority");
            _cmbStatus = SubstCtl<IComboBox>(view, "cmbStatus");
            _txtStartDate = SubstCtl<IDateBox>(view, "txtStartDate");
            _txtStopDate = SubstCtl<IDateBox>(view, "txtStopDate");
            _nudPercent = SubstCtl<INumericBox>(view, "nudPercent");
            return view;
        }
    }
}
