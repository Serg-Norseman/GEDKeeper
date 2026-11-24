/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class GroupEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_GroupEditDlgController()
        {
            IGroupEditDlg view = CreateMockView();
            var controller = new GroupEditDlgController(view);
            controller.InitView(fBaseWin);

            var group = fBaseWin.Context.Tree.CreateGroup();

            controller.GroupRecord = group;
            Assert.AreEqual(group, controller.GroupRecord);

            _edName.Text = "sample group";

            // add member
            var iRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(iRec1);
            await ((GroupMembersListModel)_fMembersList.ListModel).Modify(_fMembersList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, group.Members.Count);
            // delete member
            StdDialogsStub.SetQuestionResult(true);
            await ((GroupMembersListModel)_fMembersList.ListModel).Modify(_fMembersList, new ModifyEventArgs(RecordAction.raDelete, group.Members[0]));
            Assert.AreEqual(0, group.Members.Count);

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample group", group.GroupName);

            controller.JumpToRecord(null); // nothing
            controller.JumpToRecord(group); // simulate jump to self
        }

        private ITextBox _edName;
        private ILabel _lblName;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageMembers;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ISheetList _fMembersList;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;

        private IGroupEditDlg CreateMockView()
        {
            var view = Substitute.For<IGroupEditDlg>();
            _edName = SubstCtl<ITextBox>(view, "edName");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageMembers = SubstCtl<ITabPage>(view, "pageMembers");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _fMembersList = SubstCtl<ISheetList>(view, "fMembersList");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            return view;
        }
    }
}
