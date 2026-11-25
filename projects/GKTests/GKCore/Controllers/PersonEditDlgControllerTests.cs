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
        private ITabControl _tabsOther;
        private ITabPage _pageEvents;
        private ITabPage _pageSpouses;
        private ITabPage _pageAssociations;
        private ITabPage _pageGroups;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageSources;
        private ITabPage _pageUserRefs;
        private ITabPage _pageNames;
        private ITabPage _pageParents;
        private ITabPage _pageOther;
        private ITabPage _pageFamily;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnPortraitAdd;
        private IButton _btnPortraitDelete;
        private IButton _btnParentsAdd;
        private IButton _btnParentsEdit;
        private IButton _btnParentsDelete;
        private IButton _btnFatherAdd;
        private IButton _btnFatherDelete;
        private IButton _btnFatherSel;
        private IButton _btnMotherAdd;
        private IButton _btnMotherDelete;
        private IButton _btnMotherSel;
        private IButton _btnNameCopy;
        private ILabel _lblSurname;
        private ILabel _lblName;
        private ILabel _lblRestriction;
        private ITextBox _txtSurname;
        private ITextBox _txtName;
        private IComboBox _cmbPatronymic;
        private IComboBox _cmbSex;
        private ITextBox _txtNickname;
        private ITextBox _txtFather;
        private ITextBox _txtMother;
        private ITextBox _txtMarriedSurname;
        private IComboBox _cmbRestriction;
        private ICheckBox _chkPatriarch;
        private ICheckBox _chkBookmark;
        private ISheetList _fEventsList;
        private ISheetList _fSpousesList;
        private ISheetList _fAssociationsList;
        private ISheetList _fGroupsList;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;
        private ISheetList _fNamesList;
        private ISheetList _fParentsList;
        private ISheetList _fChildrenList;
        private ISheetList _fDNATestsList;

        [Test]
        public async Task Test_PersonEditDlgController()
        {
            IStdPersonEditDlg view = CreateMockView();
            var controller = new StdPersonEditDlgController(view);
            controller.InitView(fBaseWin);

            var indiRec = fBaseWin.Context.CreatePersonEx("Ivan", "", "Smith", GDMSex.svMale, true);
            controller.IndividualRecord = indiRec;

            _cmbSex.SelectedIndex = 1; // male

            _cmbRestriction.SelectedIndex = (3);
            _cmbRestriction.SelectedIndex = (2);
            _cmbRestriction.SelectedIndex = (1);
            _cmbRestriction.SelectedIndex = (0);

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
            await ModifySheetList<IndiGroupsListModel>(_fGroupsList, RecordAction.raAdd, null);
            Assert.AreEqual(1, indiRec.Groups.Count);
            Assert.AreEqual("sample group", fBaseWin.Context.Tree.GetPtrValue<GDMGroupRecord>(indiRec.Groups[0]).GroupName);
            await ModifySheetList<IndiGroupsListModel>(_fGroupsList, RecordAction.raDelete, indiRec.Groups[0]);
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

        private IStdPersonEditDlg CreateMockView()
        {
            var view = Substitute.For<IStdPersonEditDlg>();

            _tabsOther = SubstCtl<ITabControl>(view, "tabsOther");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblSurname = SubstCtl<ILabel>(view, "lblSurname");
            SubstCtl<ILabel>(view, "lblMarriedSurname");
            SubstCtl<ILabel>(view, "lblName");
            SubstCtl<ILabel>(view, "lblPatronymic");
            SubstCtl<ILabel>(view, "lblSex");
            SubstCtl<ILabel>(view, "lblNickname");
            _chkPatriarch = SubstCtl<ICheckBox>(view, "chkPatriarch");
            _chkBookmark = SubstCtl<ICheckBox>(view, "chkBookmark");
            SubstCtl<ILabel>(view, "lblParents");
            SubstCtl<ILabel>(view, "lblMother");
            SubstCtl<ILabel>(view, "lblFather");
            _pageEvents = SubstCtl<ITabPage>(view, "pageEvents");
            _pageSpouses = SubstCtl<ITabPage>(view, "pageSpouses");
            _pageAssociations = SubstCtl<ITabPage>(view, "pageAssociations");
            _pageGroups = SubstCtl<ITabPage>(view, "pageGroups");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageSources = SubstCtl<ITabPage>(view, "pageSources");
            _pageUserRefs = SubstCtl<ITabPage>(view, "pageUserRefs");
            _lblRestriction = SubstCtl<ILabel>(view, "lblRestriction");
            _pageNames = SubstCtl<ITabPage>(view, "pageNames");
            _pageParents = SubstCtl<ITabPage>(view, "pageParents");
            _pageOther = SubstCtl<ITabPage>(view, "pageOther");
            _pageFamily = SubstCtl<ITabPage>(view, "pageFamily");
            _btnPortraitAdd = SubstCtl<IButton>(view, "btnPortraitAdd");
            _btnPortraitDelete = SubstCtl<IButton>(view, "btnPortraitDelete");
            SubstCtl<IButton>(view, "btnParentsAdd");
            SubstCtl<IButton>(view, "btnParentsEdit");
            SubstCtl<IButton>(view, "btnParentsDelete");
            SubstCtl<IButton>(view, "btnFatherAdd");
            SubstCtl<IButton>(view, "btnFatherDelete");
            SubstCtl<IButton>(view, "btnFatherSel");
            SubstCtl<IButton>(view, "btnMotherAdd");
            SubstCtl<IButton>(view, "btnMotherDelete");
            SubstCtl<IButton>(view, "btnMotherSel");
            SubstCtl<IButton>(view, "btnNameCopy");
            SubstCtl<ITabPage>(view, "pageChilds");
            SubstCtl<ITabPage>(view, "pageDNATests");

            _fEventsList = SubstCtl<ISheetList>(view, "fEventsList");
            _fSpousesList = SubstCtl<ISheetList>(view, "fSpousesList");
            _fAssociationsList = SubstCtl<ISheetList>(view, "fAssociationsList");
            _fGroupsList = SubstCtl<ISheetList>(view, "fGroupsList");
            _fUserRefList = SubstCtl<ISheetList>(view, "fUserRefList");
            _fNamesList = SubstCtl<ISheetList>(view, "fNamesList");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            _fSourcesList = SubstCtl<ISheetList>(view, "fSourcesList");
            _fParentsList = SubstCtl<ISheetList>(view, "fParentsList");
            _txtFather = SubstCtl<ITextBox>(view, "txtFather");
            _txtMother = SubstCtl<ITextBox>(view, "txtMother");
            _txtSurname = SubstCtl<ITextBox>(view, "txtSurname");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _cmbPatronymic = SubstCtl<IComboBox>(view, "cmbPatronymic");
            _txtNickname = SubstCtl<ITextBox>(view, "txtNickname");
            _txtMarriedSurname = SubstCtl<ITextBox>(view, "txtMarriedSurname");
            _cmbRestriction = SubstCtl<IComboBox>(view, "cmbRestriction");
            _cmbSex = SubstCtl<IComboBox>(view, "cmbSex");
            _fChildrenList = SubstCtl<ISheetList>(view, "fChildrenList");
            _fDNATestsList = SubstCtl<ISheetList>(view, "fDNATestsList");
            return view;
        }
    }
}
