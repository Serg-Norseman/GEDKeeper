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
            controller.Init(fBaseWin);

            var group = fBaseWin.Context.Tree.CreateGroup();

            controller.GroupRecord = group;
            Assert.AreEqual(group, controller.GroupRecord);

            view.Name.Text = "sample group";

            // add member
            var iRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(iRec1);
            await ((GroupMembersListModel)view.MembersList.ListModel).Modify(view.MembersList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, group.Members.Count);
            // delete member
            StdDialogsStub.SetQuestionResult(true);
            await ((GroupMembersListModel)view.MembersList.ListModel).Modify(view.MembersList, new ModifyEventArgs(RecordAction.raDelete, group.Members[0]));
            Assert.AreEqual(0, group.Members.Count);

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample group", group.GroupName);

            controller.JumpToRecord(null); // nothing
            controller.JumpToRecord(group); // simulate jump to self
        }

        private static IGroupEditDlg CreateMockView()
        {
            var view = Substitute.For<IGroupEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ITabPage>(view, "pageMembers");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");

            view.MembersList.Returns(Substitute.For<ISheetList>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            return view;
        }
    }
}
