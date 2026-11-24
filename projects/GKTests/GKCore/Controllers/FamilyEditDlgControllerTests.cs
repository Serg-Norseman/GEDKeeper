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
            controller.InitView(baseWin);

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

            _cmbMarriageStatus.SelectedIndex = 0;

            // spouses
            controller.DeleteHusband();
            controller.DeleteWife();

            // children
            Assert.AreEqual(0, famRec.Children.Count);
            var child = baseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(child);
            await ModifySheetList<ChildrenListModel>(_fChildrenList, RecordAction.raAdd, null);
            Assert.AreEqual(1, famRec.Children.Count);

            /*SelectSheetListItem("fChildsList", fDialog, 0);
            ModalFormHandler = CustomWindowTest.IndividualEdit_Mini_Handler;
            ClickToolStripButton("fChildsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(1, famRec.Children.Count);*/

            await ModifySheetList<ChildrenListModel>(_fChildrenList, RecordAction.raDelete, famRec.Children[0]);
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
            await ModifySheetList<NoteLinksListModel>(_fNotesList, RecordAction.raAdd, null);
            Assert.AreEqual(1, famRec.Notes.Count);

            /*SelectSheetListItem("fNotesList", dlg, 0);
            ModalFormHandler = CustomWindowTest.NoteAdd_Mini_Handler;
            ClickToolStripButton("fNotesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, famRec.Notes.Count);*/

            await ModifySheetList<NoteLinksListModel>(_fNotesList, RecordAction.raDelete, famRec.Notes[0]);
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

        private ITabControl _tabsData;
        private ITabPage _pageEvents;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageSources;
        private ITabPage _pageChilds;
        private ITabPage _pageUserRefs;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IGroupBox _GroupBox1;
        private ILabel _lblHusband;
        private ITextBox _txtHusband;
        private IButton _btnHusbandAdd;
        private IButton _btnHusbandDelete;
        private IButton _btnHusbandSel;
        private IButton _btnWifeSel;
        private IButton _btnWifeDelete;
        private IButton _btnWifeAdd;
        private ITextBox _txtWife;
        private ILabel _lblWife;
        private ILabel _lblStatus;
        private IComboBox _cmbMarriageStatus;
        private ILabel _lblRestriction;
        private IComboBox _cmbRestriction;
        private ISheetList _fChildrenList;
        private ISheetList _fEventsList;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;

        private IFamilyEditDlg CreateMockView()
        {
            var view = Substitute.For<IFamilyEditDlg>();
            _tabsData = SubstCtl<ITabControl>(view, "tabsData");
            _pageEvents = SubstCtl<ITabPage>(view, "pageEvents");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageSources = SubstCtl<ITabPage>(view, "pageSources");
            _pageChilds = SubstCtl<ITabPage>(view, "pageChilds");
            _pageUserRefs = SubstCtl<ITabPage>(view, "pageUserRefs");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _GroupBox1 = SubstCtl<IGroupBox>(view, "GroupBox1");
            _lblHusband = SubstCtl<ILabel>(view, "lblHusband");
            _txtHusband = SubstCtl<ITextBox>(view, "txtHusband");
            _btnHusbandAdd = SubstCtl<IButton>(view, "btnHusbandAdd");
            _btnHusbandDelete = SubstCtl<IButton>(view, "btnHusbandDelete");
            _btnHusbandSel = SubstCtl<IButton>(view, "btnHusbandSel");
            _btnWifeSel = SubstCtl<IButton>(view, "btnWifeSel");
            _btnWifeDelete = SubstCtl<IButton>(view, "btnWifeDelete");
            _btnWifeAdd = SubstCtl<IButton>(view, "btnWifeAdd");
            _txtWife = SubstCtl<ITextBox>(view, "txtWife");
            _lblWife = SubstCtl<ILabel>(view, "lblWife");
            _lblStatus = SubstCtl<ILabel>(view, "lblStatus");
            _cmbMarriageStatus = SubstCtl<IComboBox>(view, "cmbMarriageStatus");
            _lblRestriction = SubstCtl<ILabel>(view, "lblRestriction");
            _cmbRestriction = SubstCtl<IComboBox>(view, "cmbRestriction");
            _fChildrenList = SubstCtl<ISheetList>(view, "fChildrenList");
            _fEventsList = SubstCtl<ISheetList>(view, "fEventsList");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            _fSourcesList = SubstCtl<ISheetList>(view, "fSourcesList");
            _fUserRefList = SubstCtl<ISheetList>(view, "fUserRefList");
            return view;
        }
    }
}
