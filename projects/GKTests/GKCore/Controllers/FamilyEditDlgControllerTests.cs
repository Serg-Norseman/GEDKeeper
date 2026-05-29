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
    public class FamilyEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_FamilyEditDlgController()
        {
            IFamilyEditDlg view = CreateMockView();
            var controller = new FamilyEditDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            var famRec = baseWin.Context.Tree.CreateFamily();
            controller.FamilyRecord = famRec;
            Assert.AreEqual(famRec, controller.FamilyRecord);

            var relHusband = baseWin.Context.Tree.CreateIndividual();
            relHusband.Sex = GDMSex.svMale;
            RecordSelectDialogStub.SetTestResult(relHusband);
            controller.AddHusband();

            var relWife = baseWin.Context.Tree.CreateIndividual();
            relWife.Sex = GDMSex.svFemale;
            RecordSelectDialogStub.SetTestResult(relWife);
            controller.AddWife();

            controller.UpdateView();
            controller.Accept();

            Assert.AreEqual(relHusband, baseWin.Context.Tree.GetPtrValue(famRec.Husband));
            Assert.AreEqual(relWife, baseWin.Context.Tree.GetPtrValue(famRec.Wife));

            controller.JumpToHusband();
            controller.JumpToWife();

            // on all messages
            StdDialogsStub.SetQuestionResult(true);

            view.MarriageStatus.SelectedIndex = 0;

            // spouses
            controller.DeleteHusband();
            controller.DeleteWife();

            // children
            Assert.AreEqual(0, famRec.Children.Count);
            var child = baseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(child);
            await ModifySheetList<ChildrenListModel>(view.ChildrenList, RecordAction.raAdd, null);
            Assert.AreEqual(1, famRec.Children.Count);

            /*SelectSheetListItem("fChildsList", fDialog, 0);
            ModalFormHandler = CustomWindowTest.IndividualEdit_Mini_Handler;
            ClickToolStripButton("fChildsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(1, famRec.Children.Count);*/

            await ModifySheetList<ChildrenListModel>(view.ChildrenList, RecordAction.raDelete, famRec.Children[0]);
            Assert.AreEqual(0, famRec.Children.Count);

            // events
            /*Assert.AreEqual(0, familyRecord.Events.Count);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, familyRecord.Events.Count);

            SelectSheetListItem("fEventsList", fDialog, 0);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(1, familyRecord.Events.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fEventsList", fDialog, 0);
            ClickToolStripButton("fEventsList_ToolBar_btnDelete", fDialog);
            Assert.AreEqual(0, familyRecord.Events.Count);*/

            controller.Accept();

            // notes sheet
            Assert.AreEqual(0, famRec.Notes.Count);
            var notRec = baseWin.Context.Tree.CreateNote();
            RecordSelectDialogStub.SetTestResult(notRec);
            await ModifySheetList<NoteLinksListModel>(view.NotesList, RecordAction.raAdd, null);
            Assert.AreEqual(1, famRec.Notes.Count);

            /*SelectSheetListItem("fNotesList", dlg, 0);
            ModalFormHandler = CustomWindowTest.NoteAdd_Mini_Handler;
            ClickToolStripButton("fNotesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, famRec.Notes.Count);*/

            await ModifySheetList<NoteLinksListModel>(view.NotesList, RecordAction.raDelete, famRec.Notes[0]);
            Assert.AreEqual(0, famRec.Notes.Count);
            controller.Accept();

            // media sheet
            /*try {
                Assert.AreEqual(0, record.MultimediaLinks.Count);
                CustomWindowTest.SetCreateItemHandler(this, MediaEditDlgTests.MultimediaRecord_Add_Handler);
                ClickToolStripButton("fMediaList_ToolBar_btnAdd", dlg);
                Assert.AreEqual(1, record.MultimediaLinks.Count);

                SelectSheetListItem("fMediaList", dlg, 0);
                ModalFormHandler = MediaEditDlgTests.MultimediaRecord_Add_Handler;
                ClickToolStripButton("fMediaList_ToolBar_btnEdit", dlg);
                Assert.AreEqual(1, record.MultimediaLinks.Count);

                SelectSheetListItem("fMediaList", dlg, 0);
                ModalFormHandler = MessageBox_YesHandler;
                ClickToolStripButton("fMediaList_ToolBar_btnDelete", dlg);
                Assert.AreEqual(0, record.MultimediaLinks.Count);
            } finally {
                TestUtils.RemoveTestFile(MediaEditDlgTests.MediaSampleFile);
            }*/
            controller.Accept();

            // sources sheet
            /*Assert.AreEqual(0, famRec.SourceCitations.Count);
            ModalFormHandler = CustomWindowTest.SourceCitEditDlg_AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, famRec.SourceCitations.Count);

            SelectSheetListItem("fSourcesList", dlg, 0);
            ModalFormHandler = CustomWindowTest.SourceCitEditDlg_AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, famRec.SourceCitations.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fSourcesList", dlg, 0);
            ClickToolStripButton("fSourcesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, famRec.SourceCitations.Count);
            ClickButton("btnAccept", fDialog);*/
        }

        private static IFamilyEditDlg CreateMockView()
        {
            var view = Substitute.For<IFamilyEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageChilds");
            SubstituteControl<ITabPage>(view, "pageEvents");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ITabPage>(view, "pageSources");
            SubstituteControl<ITabPage>(view, "pageUserRefs");
            SubstituteControl<IGroupBox>(view, "GroupBox1");
            SubstituteControl<ILabel>(view, "lblHusband");
            SubstituteControl<ILabel>(view, "lblWife");
            SubstituteControl<ILabel>(view, "lblStatus");
            SubstituteControl<ILabel>(view, "lblRestriction");
            SubstituteControl<IButton>(view, "btnHusbandAdd");
            SubstituteControl<IButton>(view, "btnHusbandDelete");
            SubstituteControl<IButton>(view, "btnHusbandSel");
            SubstituteControl<IButton>(view, "btnWifeAdd");
            SubstituteControl<IButton>(view, "btnWifeDelete");
            SubstituteControl<IButton>(view, "btnWifeSel");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.SourcesList.Returns(Substitute.For<ISheetList>());
            view.ChildrenList.Returns(Substitute.For<ISheetList>());
            view.EventsList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.MarriageStatus.Returns(Substitute.For<IComboBox>());
            view.Restriction.Returns(Substitute.For<IComboBox>());
            view.Husband.Returns(Substitute.For<ITextBox>());
            view.Wife.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
