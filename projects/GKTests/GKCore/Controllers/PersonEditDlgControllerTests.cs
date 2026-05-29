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
    public class PersonEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_PersonEditDlgController()
        {
            IStdPersonEditDlg view = CreateMockView();
            var controller = new StdPersonEditDlgController(view);
            controller.Init(fBaseWin);

            var indiRec = fBaseWin.Context.CreatePersonEx("Ivan", "", "Smith", GDMSex.svMale, true);
            controller.IndividualRecord = indiRec;

            view.SexCombo.SelectedIndex = 1; // male

            var cmbRestriction = view.RestrictionCombo;
            cmbRestriction.SelectedIndex = (3);
            cmbRestriction.SelectedIndex = (2);
            cmbRestriction.SelectedIndex = (1);
            cmbRestriction.SelectedIndex = (0);

            // on all messages
            StdDialogsStub.SetQuestionResult(true);

            // parents
            var parfamRec = fBaseWin.Context.Tree.CreateFamily();
            RecordSelectDialogStub.SetTestResult(parfamRec);
            controller.AddParents();
            StdDialogsStub.SetQuestionResult(true);
            controller.DeleteParents();

            // father
            var fathRec = fBaseWin.Context.CreatePersonEx("Steph", "", "Smith", GDMSex.svMale, true);
            RecordSelectDialogStub.SetTestResult(fathRec);
            controller.AddFather();
            StdDialogsStub.SetQuestionResult(true);
            controller.DeleteFather();

            // mother
            var mothRec = fBaseWin.Context.CreatePersonEx("Anna", "", "Smith", GDMSex.svFemale, true);
            RecordSelectDialogStub.SetTestResult(mothRec);
            controller.AddMother();
            controller.DeleteMother();

            controller.CopyPersonName();

            // events
            /*tabs.SelectTab(0);
            Assert.AreEqual(1, indiRecord.Events.Count);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(2, indiRecord.Events.Count);

            SelectSheetListItem("fEventsList", fDialog, 1);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(2, indiRecord.Events.Count);

            TestDeleteSheetListItem("fEventsList", 1);
            Assert.AreEqual(1, indiRecord.Events.Count);*/

            // spouses
            /*tabs.SelectTab(1);
            Assert.AreEqual(0, indiRec.SpouseToFamilyLinks.Count);
            ModalFormHandler = FamilyEditDlgTests.SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRec.SpouseToFamilyLinks.Count);

            SelectSheetListItem("fSpousesList", fDialog, 0);
            ModalFormHandler = FamilyEditDlgTests.SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(1, indiRec.SpouseToFamilyLinks.Count);

            TestDeleteSheetListItem("fSpousesList", 0);
            Assert.AreEqual(0, indiRec.SpouseToFamilyLinks.Count);*/

            // names
            /*tabs.SelectTab(2);
            Assert.AreEqual(1, indiRec.PersonalNames.Count);
            ModalFormHandler = CustomWindowTest.PersonalNameEditAdd_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(2, indiRec.PersonalNames.Count);
            Assert.AreEqual("sample surname", indiRec.PersonalNames[1].Surname);

            SelectSheetListItem("fNamesList", fDialog, 1);
            ModalFormHandler = CustomWindowTest.PersonalNameEditEdit_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(2, indiRec.PersonalNames.Count);
            Assert.AreEqual("sample surname2", indiRec.PersonalNames[1].Surname);

            TestDeleteSheetListItem("fNamesList", 1);
            Assert.AreEqual(1, indiRec.PersonalNames.Count);*/

            // associations
            /*tabs.SelectTab(3);
            Assert.AreEqual(0, indiRecord.Associations.Count);
            ModalFormHandler = AssociationEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fAssociationsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRecord.Associations.Count);
            Assert.AreEqual("sample relation", indiRecord.Associations[0].Relation);

            TestDeleteSheetListItem("fAssociationsList", 0);
            Assert.AreEqual(0, indiRecord.Associations.Count);*/

            // groups
            Assert.AreEqual(0, indiRec.Groups.Count);
            var grpRec = fBaseWin.Context.Tree.CreateGroup();
            grpRec.GroupName = "sample group";
            RecordSelectDialogStub.SetTestResult(grpRec);
            await ModifySheetList<IndiGroupsListModel>(view.GroupsList, RecordAction.raAdd, null);
            Assert.AreEqual(1, indiRec.Groups.Count);
            Assert.AreEqual("sample group", fBaseWin.Context.Tree.GetPtrValue<GDMGroupRecord>(indiRec.Groups[0]).GroupName);
            await ModifySheetList<IndiGroupsListModel>(view.GroupsList, RecordAction.raDelete, indiRec.Groups[0]);
            Assert.AreEqual(0, indiRec.Groups.Count);

            // userrefs
            /*tabs.SelectTab(8);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);
            ModalFormHandler = UserRefEditDlgTests.AcceptHandler;
            ClickToolStripButton("fUserRefList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRecord.UserReferences.Count);
            Assert.AreEqual("sample reference", indiRecord.UserReferences[0].StringValue);

            TestDeleteSheetListItem("fUserRefList", 0);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);*/

            controller.Accept();

            controller.UpdateView();
        }

        private static IStdPersonEditDlg CreateMockView()
        {
            var view = Substitute.For<IStdPersonEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblSurname");
            SubstituteControl<ILabel>(view, "lblMarriedSurname");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblPatronymic");
            SubstituteControl<ILabel>(view, "lblSex");
            SubstituteControl<ILabel>(view, "lblNickname");
            SubstituteControl<ICheckBox>(view, "chkPatriarch");
            SubstituteControl<ICheckBox>(view, "chkBookmark");
            SubstituteControl<ILabel>(view, "lblParents");
            SubstituteControl<ILabel>(view, "lblMother");
            SubstituteControl<ILabel>(view, "lblFather");
            SubstituteControl<ITabPage>(view, "pageEvents");
            SubstituteControl<ITabPage>(view, "pageSpouses");
            SubstituteControl<ITabPage>(view, "pageAssociations");
            SubstituteControl<ITabPage>(view, "pageGroups");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ITabPage>(view, "pageSources");
            SubstituteControl<ITabPage>(view, "pageUserRefs");
            SubstituteControl<ILabel>(view, "lblRestriction");
            SubstituteControl<ITabPage>(view, "pageNames");
            SubstituteControl<ITabPage>(view, "pageParents");
            SubstituteControl<ITabPage>(view, "pageOther");
            SubstituteControl<ITabPage>(view, "pageFamily");
            SubstituteControl<IButton>(view, "btnPortraitAdd");
            SubstituteControl<IButton>(view, "btnPortraitDelete");
            SubstituteControl<IButton>(view, "btnParentsAdd");
            SubstituteControl<IButton>(view, "btnParentsEdit");
            SubstituteControl<IButton>(view, "btnParentsDelete");
            SubstituteControl<IButton>(view, "btnFatherAdd");
            SubstituteControl<IButton>(view, "btnFatherDelete");
            SubstituteControl<IButton>(view, "btnFatherSel");
            SubstituteControl<IButton>(view, "btnMotherAdd");
            SubstituteControl<IButton>(view, "btnMotherDelete");
            SubstituteControl<IButton>(view, "btnMotherSel");
            SubstituteControl<IButton>(view, "btnNameCopy");
            SubstituteControl<ITabPage>(view, "pageChilds");
            SubstituteControl<ITabPage>(view, "pageDNATests");

            view.EventsList.Returns(Substitute.For<ISheetList>());
            view.SpousesList.Returns(Substitute.For<ISheetList>());
            view.AssociationsList.Returns(Substitute.For<ISheetList>());
            view.GroupsList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.NamesList.Returns(Substitute.For<ISheetList>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.SourcesList.Returns(Substitute.For<ISheetList>());
            view.ParentsList.Returns(Substitute.For<ISheetList>());
            view.Portrait.Returns(Substitute.For<IPortraitControl>());
            view.Father.Returns(Substitute.For<ITextBox>());
            view.Mother.Returns(Substitute.For<ITextBox>());
            view.Surname.Returns(Substitute.For<ITextBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Patronymic.Returns(Substitute.For<IComboBox>());
            view.Nickname.Returns(Substitute.For<ITextBox>());
            view.MarriedSurname.Returns(Substitute.For<ITextBox>());
            view.SurnameLabel.Returns(Substitute.For<ILabel>());
            view.RestrictionCombo.Returns(Substitute.For<IComboBox>());
            view.SexCombo.Returns(Substitute.For<IComboBox>());
            view.Patriarch.Returns(Substitute.For<ICheckBox>());
            view.Bookmark.Returns(Substitute.For<ICheckBox>());
            view.ChildrenList.Returns(Substitute.For<ISheetList>());
            view.DNATestsList.Returns(Substitute.For<ISheetList>());
            return view;
        }
    }
}
