// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.ExtData;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Names;
using GKCore.Options;
using GKCore.Stats;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class ControllerTests
    {
        private readonly IBaseWindow fBaseWin;

        public ControllerTests()
        {
            TestUtils.InitUITest();

            // prevent LanguageSelectDlg modal dialog from showing on first run
            AppHost.Options.InterfaceLang = LangMan.LS_DEF_CODE;

            // required for testing, otherwise the engine will require saving
            // the database (requires path of files for the archive and storage)
            GlobalOptions.Instance.AllowMediaStoreReferences = true;

            fBaseWin = new BaseWindowStub(true);
        }

        private static IBaseWindow GetBaseSubst()
        {
            var baseWin = Substitute.For<IBaseWindow>();
            var baseContext = new BaseContext(baseWin);
            baseWin.Context.Returns(baseContext);
            return baseWin;
        }

        private static void SubstituteControl<T>(IView dialog, string ctlName) where T : class, IControl
        {
            TestUtils.SubstituteControl<T>(dialog, ctlName);
        }

        [Test]
        public async Task Test_AboutDlgController()
        {
            var view = Substitute.For<IAboutDlg>();
            SubstituteControl<ILabel>(view, "btnClose");
            SubstituteControl<ILabel>(view, "lblProduct");
            SubstituteControl<ILabel>(view, "lblVersion");
            SubstituteControl<ILabel>(view, "lblCopyright");
            SubstituteControl<ILabel>(view, "lblForum");
            SubstituteControl<ILabel>(view, "lblChannel");

            var controller = new AboutDlgController(view);
            controller.SetLocale();
            controller.UpdateView();
        }

        [Test]
        public async Task Test_AddressEditDlgController()
        {
            var view = Substitute.For<IAddressEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ILabel>(view, "lblCountry");
            SubstituteControl<ILabel>(view, "lblState");
            SubstituteControl<ILabel>(view, "lblCity");
            SubstituteControl<ILabel>(view, "lblPostalCode");
            SubstituteControl<ILabel>(view, "lblAddress");
            SubstituteControl<ITabPage>(view, "pagePhones");
            SubstituteControl<ITabPage>(view, "pageEmails");
            SubstituteControl<ITabPage>(view, "pageWebPages");

            view.PhonesList.Returns(Substitute.For<ISheetList>());
            view.PhonesList.ListView.Returns(Substitute.For<IListView>());
            view.PhonesList.ListView.ListMan.Returns(new TagsListModel(fBaseWin.Context, "phones"));

            view.MailsList.Returns(Substitute.For<ISheetList>());
            view.MailsList.ListView.Returns(Substitute.For<IListView>());
            view.MailsList.ListView.ListMan.Returns(new TagsListModel(fBaseWin.Context, "mails"));

            view.WebsList.Returns(Substitute.For<ISheetList>());
            view.WebsList.ListView.Returns(Substitute.For<IListView>());
            view.WebsList.ListView.ListMan.Returns(new TagsListModel(fBaseWin.Context, "webs"));

            var controller = new AddressEditDlgController(view);
            controller.ApplyTheme();

            var addr = new GDMAddress();

            controller.Address = addr;
            Assert.AreEqual(addr, controller.Address);

            // fields
            addr.AddressCountry = "sample country";
            addr.AddressState = "state";
            addr.AddressCity = "city";
            addr.AddressPostalCode = "0123456";
            controller.UpdateView();
            Assert.AreEqual("sample country", view.Country.Text);
            Assert.AreEqual("state", view.State.Text);
            Assert.AreEqual("city", view.City.Text);
            Assert.AreEqual("0123456", view.PostalCode.Text);

            // phones
            StdDialogsStub.SetStrInputResult("test phone");
            await controller.DoPhonesAction(RecordAction.raAdd, null);
            Assert.AreEqual(1, addr.PhoneNumbers.Count);
            Assert.AreEqual("test phone", addr.PhoneNumbers[0].StringValue);

            StdDialogsStub.SetStrInputResult("test phone 2");
            await controller.DoPhonesAction(RecordAction.raEdit, addr.PhoneNumbers[0]);
            Assert.AreEqual(1, addr.PhoneNumbers.Count);
            Assert.AreEqual("test phone 2", addr.PhoneNumbers[0].StringValue);

            await controller.DoPhonesAction(RecordAction.raDelete, addr.PhoneNumbers[0]);
            Assert.AreEqual(0, addr.PhoneNumbers.Count);

            // mails
            StdDialogsStub.SetStrInputResult("test mail");
            await controller.DoMailsAction(RecordAction.raAdd, null);
            Assert.AreEqual(1, addr.EmailAddresses.Count);
            Assert.AreEqual("test mail", addr.EmailAddresses[0].StringValue);

            StdDialogsStub.SetStrInputResult("test mail 2");
            await controller.DoMailsAction(RecordAction.raEdit, addr.EmailAddresses[0]);
            Assert.AreEqual(1, addr.EmailAddresses.Count);
            Assert.AreEqual("test mail 2", addr.EmailAddresses[0].StringValue);

            await controller.DoMailsAction(RecordAction.raDelete, addr.EmailAddresses[0]);
            Assert.AreEqual(0, addr.EmailAddresses.Count);

            // web sites
            StdDialogsStub.SetStrInputResult("test site");
            await controller.DoWebsAction(RecordAction.raAdd, null);
            Assert.AreEqual(1, addr.WebPages.Count);
            Assert.AreEqual("test site", addr.WebPages[0].StringValue);

            StdDialogsStub.SetStrInputResult("test site 2");
            await controller.DoWebsAction(RecordAction.raEdit, addr.WebPages[0]);
            Assert.AreEqual(1, addr.WebPages.Count);
            Assert.AreEqual("test site 2", addr.WebPages[0].StringValue);

            await controller.DoWebsAction(RecordAction.raDelete, addr.WebPages[0]);
            Assert.AreEqual(0, addr.WebPages.Count);

            // accept?
            controller.Accept();
            Assert.AreEqual("sample country", addr.AddressCountry);
        }

        [Test]
        public void Test_AssociationEditDlgController()
        {
            var view = Substitute.For<IAssociationEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblRelation");
            SubstituteControl<IComboBox>(view, "cmbRelation");
            SubstituteControl<ILabel>(view, "lblPerson");
            SubstituteControl<ITextBox>(view, "txtPerson");
            SubstituteControl<IButton>(view, "btnPersonAdd");

            Assert.IsNull(view.Association);
            Assert.IsNotNull(view.Person);
            Assert.IsNotNull(view.Relation);

            var baseWin = GetBaseSubst();
            var association = new GDMAssociation(); // for xref pointers to work

            var controller = new AssociationEditDlgController(view);
            Assert.IsNotNull(controller);
            controller.ApplyTheme();

            controller.Init(baseWin);
            Assert.AreEqual(baseWin, controller.Base);

            controller.Association = association;
            Assert.AreEqual(association, controller.Association);

            // the association is empty
            controller.UpdateView();

            // the relation is empty, an exception will be thrown, accept will be false
            Assert.IsFalse(controller.Accept());

            // substitutes of values
            var relValue = "test relation";
            view.Relation.Text.Returns(relValue);
            var relPerson = baseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relPerson);

            controller.SetPerson();

            Assert.IsTrue(controller.Accept());
            controller.UpdateView();
            Assert.AreEqual(relValue, association.Relation);
            Assert.AreEqual(relPerson, baseWin.Context.Tree.GetPtrValue(association));
        }

        [Test]
        public void Test_BaseWinController()
        {
            var view = Substitute.For<IBaseWindowView>();
            //var controller = new BaseWinController(view);
        }

        [Test]
        public void Test_CircleChartWinController()
        {
            var view = Substitute.For<ICircleChartWin>();
            //var controller = new CircleChartWinController(view);
        }

        [Test]
        public void Test_CommonFilterDlgController()
        {
            var view = Substitute.For<ICommonFilterDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IButton>(view, "btnReset");
            SubstituteControl<ITabPage>(view, "pageFieldsFilter");

            view.FilterGrid.Returns(Substitute.For<IFilterGridView>());

            var listMan = new NoteListModel(fBaseWin.Context);
            var controller = new CommonFilterDlgController(view, listMan);

            controller.Accept();
            controller.UpdateView();
            controller.Reset();
        }

        [Test]
        public void Test_CommunicationEditDlgController()
        {
            var view = Substitute.For<ICommunicationEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ILabel>(view, "lblTheme");
            SubstituteControl<ILabel>(view, "lblCorresponder");
            SubstituteControl<ILabel>(view, "lblType");
            SubstituteControl<ILabel>(view, "lblDate");

            view.Corresponder.Returns(Substitute.For<ITextBox>());
            view.CorrType.Returns(Substitute.For<IComboBox>());
            view.Date.Returns(Substitute.For<IDateBox>());
            view.Dir.Returns(Substitute.For<IComboBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());

            var controller = new CommunicationEditDlgController(view);
            controller.Init(fBaseWin);

            var commRec = fBaseWin.Context.Tree.CreateCommunication();

            controller.CommunicationRecord = commRec;
            Assert.AreEqual(commRec, controller.CommunicationRecord);

            view.Name.Text = "sample theme";
            controller.Accept();
            Assert.AreEqual("sample theme", commRec.CommName);

            view.Name.Text = "sample text";
            view.CorrType.SelectedIndex = 1;
            controller.Accept();
            Assert.AreEqual("sample text", commRec.CommName);
            Assert.AreEqual(GDMCommunicationType.ctEMail, commRec.CommunicationType);
            Assert.AreEqual("", commRec.Date.StringValue);

            view.Name.Text = "sample text";
            view.Date.NormalizeDate = "02.02.2000";
            controller.Accept();
            Assert.AreEqual("sample text", commRec.CommName);
            Assert.AreEqual("02 FEB 2000", commRec.Date.StringValue);

            var iRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(iRec1);
            controller.SetPerson();
            controller.Accept();

            controller.UpdateView();
        }

        [Test]
        public void Test_DayTipsDlgController()
        {
            var view = Substitute.For<IDayTipsDlg>();
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnNextTip");
            SubstituteControl<ICheckBox>(view, "chkShow");
            SubstituteControl<ILabel>(view, "lblTitle");

            view.TitleLabel.Returns(Substitute.For<ILabel>());
            view.TipText.Returns(Substitute.For<ITextContainer>());
            view.NextButton.Returns(Substitute.For<IButton>());

            var tips = new StringList();
            TipsCollector.Collect(fBaseWin.Context, tips);

            var controller = new DayTipsDlgController(view);
            controller.InitTips("birth days", true, tips);

            controller.GetNextTip();
            controller.UpdateView();
        }

        [Test]
        public void Test_EventEditDlgController()
        {
            var view = Substitute.For<IEventEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IButton>(view, "btnAddress");
            SubstituteControl<IButton>(view, "btnPlaceAdd");
            SubstituteControl<IButton>(view, "btnPlaceDelete");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ITabPage>(view, "pageSources");
            SubstituteControl<ILabel>(view, "lblEvent");
            SubstituteControl<ILabel>(view, "lblAttrValue");
            SubstituteControl<ILabel>(view, "lblPlace");
            SubstituteControl<ILabel>(view, "lblDate");
            SubstituteControl<ILabel>(view, "lblCause");
            SubstituteControl<ILabel>(view, "lblOrg");

            SubstituteControl<ILabel>(view, "lblAge");
            SubstituteControl<ITextBox>(view, "txtAge");
            SubstituteControl<IButton>(view, "btnAge");

            view.EventType.Returns(Substitute.For<IComboBox>());
            view.Date.Returns(Substitute.For<IDateControl>());
            view.Attribute.Returns(Substitute.For<IComboBox>());
            view.Place.Returns(Substitute.For<ITextBox>());
            view.Cause.Returns(Substitute.For<IComboBox>());
            view.Agency.Returns(Substitute.For<IComboBox>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.SourcesList.Returns(Substitute.For<ISheetList>());

            var controller = new EventEditDlgController(view);
            controller.Init(fBaseWin);

            var evt = new GDMIndividualEvent();
            controller.Event = evt;

            view.EventType.SelectedIndex = 1; // Birth(indi) / ?(fam)
            view.Place.Text = "test place";

            // FIXME: create GKDateControl tests
            /*SelectCombo("cmbEventDateType", form, 3); // Between
            EnterMaskedText("txtEventDate1", form, "01.01.1900");
            EnterMaskedText("txtEventDate2", form, "10.01.1900");
            SelectCombo("cmbDate1Calendar", form, 1); // Julian
            SelectCombo("cmbDate2Calendar", form, 1); // Julian*/

            view.Cause.Text = "test cause";
            view.Agency.Text = "test agency";

            //SetModalFormHandler(fFormTest, AddressEditDlgTests.AddressEditDlg_btnAccept_Handler);
            //ClickButton("btnAddress", form);

            var loc = fBaseWin.Context.Tree.CreateLocation();
            RecordSelectDialogStub.SetTestResult(loc);
            controller.AddPlace();

            controller.RemovePlace();

            controller.Accept();
            controller.UpdateView();
        }

        [Test]
        public async Task Test_FamilyEditDlgController()
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

        [Test]
        public void Test_FilePropertiesDlgController()
        {
            var view = Substitute.For<IFilePropertiesDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageAuthor");
            SubstituteControl<ITabPage>(view, "pageOther");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblAddress");
            SubstituteControl<ILabel>(view, "lblTelephone");
            SubstituteControl<ILabel>(view, "lblLanguage");

            view.RecordStats.Returns(Substitute.For<IListView>());
            view.Language.Returns(Substitute.For<IComboBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Address.Returns(Substitute.For<ITextBox>());
            view.Tel.Returns(Substitute.For<ITextBox>());

            var controller = new FilePropertiesDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            controller.UpdateView();

            view.Name.Text = "sample text";

            var langValue = GDMLanguageID.AngloSaxon;
            view.Language.GetSelectedTag<GDMLanguageID>().Returns(langValue);

            controller.Accept();

            GDMSubmitterRecord submitter = baseWin.Context.Tree.GetPtrValue<GDMSubmitterRecord>(baseWin.Context.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        [Test]
        public void Test_FragmentSearchController()
        {
            var view = Substitute.For<IFragmentSearchDlg>();
            SubstituteControl<IMenuItem>(view, "miDetails");
            SubstituteControl<IMenuItem>(view, "miGoToRecord");
            SubstituteControl<IMenuItem>(view, "miCopyXRef");
            SubstituteControl<IMenuItem>(view, "miDQRefresh");
            SubstituteControl<IMenuItem>(view, "miDQResetFilter");
            SubstituteControl<ITabPage>(view, "pageFamilyGroups");
            SubstituteControl<IButton>(view, "btnAnalyseGroups");
            SubstituteControl<ITabPage>(view, "pageDataQuality");

            var controller = new FragmentSearchController(view);
            controller.Init(fBaseWin);

            controller.CheckGroups();
        }

        [Test]
        public async Task Test_GroupEditDlgController()
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

        [Test]
        public void Test_LanguageSelectDlgController()
        {
            var view = Substitute.For<ILanguageSelectDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            var controller = new LanguageSelectDlgController(view);
        }

        [Test]
        public void Test_LocationEditDlgController()
        {
            var view = Substitute.For<ILocationEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageHistory");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblLatitude");
            SubstituteControl<ILabel>(view, "lblLongitude");
            SubstituteControl<IButton>(view, "btnShowOnMap");
            SubstituteControl<IGroupBox>(view, "grpSearch");
            SubstituteControl<IButton>(view, "btnSearch");
            SubstituteControl<IButton>(view, "btnSelect");
            SubstituteControl<IButton>(view, "btnSelectName");
            SubstituteControl<IButton>(view, "btnSelectCursor");
            SubstituteControl<IGroupBox>(view, "pageHistNames");
            SubstituteControl<IGroupBox>(view, "pageHistLinks");

            view.MapBrowser.Returns(Substitute.For<IMapBrowser>());
            view.NamesList.Returns(Substitute.For<ISheetList>());
            view.LinksList.Returns(Substitute.For<ISheetList>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.GeoCoordsList.Returns(Substitute.For<IListView>());

            var controller = new LocationEditDlgController(view);
            controller.Init(fBaseWin);

            var locRec = fBaseWin.Context.Tree.CreateLocation();

            controller.LocationRecord = locRec;
            Assert.AreEqual(locRec, controller.LocationRecord);

            view.Name.Text = "sample location";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample location", locRec.LocationName);

            var geoPoint = controller.GetSelectedGeoPoint();
            Assert.AreEqual(null, geoPoint);

            controller.Search();

            controller.SelectCoords();

            controller.SelectName();

            controller.SelectGeoPoint();

            controller.ShowOnMap();
        }

        [Test]
        public void Test_MapsViewerWinController()
        {
            var view = Substitute.For<IMapsViewerWin>();
            SubstituteControl<ITabPage>(view, "pagePlaces");
            SubstituteControl<IGroupBox>(view, "grpSelection");
            SubstituteControl<IRadioButton>(view, "radTotal");
            SubstituteControl<ICheckBox>(view, "chkBirth");
            SubstituteControl<ICheckBox>(view, "chkDeath");
            SubstituteControl<ICheckBox>(view, "chkResidence");
            SubstituteControl<IRadioButton>(view, "radSelected");
            SubstituteControl<IButton>(view, "btnSelectPlaces");
            SubstituteControl<ICheckBox>(view, "chkLinesVisible");
            SubstituteControl<IButtonToolItem>(view, "tbLoadPlaces");
            SubstituteControl<IButtonToolItem>(view, "tbProviders");
            SubstituteControl<ITabPage>(view, "pageCoordinates");
            SubstituteControl<IButtonToolItem>(view, "tbClear");
            SubstituteControl<IButton>(view, "btnSearch");
            SubstituteControl<IToolItem>(view, "tbSaveSnapshot");

            view.MapBrowser.Returns(Substitute.For<IMapBrowser>());
            view.PersonsCombo.Returns(Substitute.For<IComboBox>());
            view.PlacesTree.Returns(Substitute.For<ITreeView>());
            view.SelectPlacesBtn.Returns(Substitute.For<IButton>());
            view.BirthCheck.Returns(Substitute.For<ICheckBox>());
            view.DeathCheck.Returns(Substitute.For<ICheckBox>());
            view.ResidenceCheck.Returns(Substitute.For<ICheckBox>());
            view.LinesVisibleCheck.Returns(Substitute.For<ICheckBox>());
            view.TotalRadio.Returns(Substitute.For<IRadioButton>());
            view.SelectedRadio.Returns(Substitute.For<IRadioButton>());

            var controller = new MapsViewerWinController(view, null);
            controller.Init(fBaseWin);
        }

        [Test]
        public async Task Test_MediaEditDlgController()
        {
            var view = Substitute.For<IMediaEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageSources");
            SubstituteControl<ITabPage>(view, "pageUserRefs");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblType");
            SubstituteControl<ILabel>(view, "lblStoreType");
            SubstituteControl<ILabel>(view, "lblFile");
            SubstituteControl<IButton>(view, "btnView");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.SourcesList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.MediaType.Returns(Substitute.For<IComboBox>());
            view.StoreType.Returns(Substitute.For<IComboBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.File.Returns(Substitute.For<ITextBox>());
            view.FileSelectButton.Returns(Substitute.For<IButton>());

            var controller = new MediaEditDlgController(view);
            controller.Init(fBaseWin);

            var multimediaRecord = new GDMMultimediaRecord(fBaseWin.Context.Tree);
            multimediaRecord.FileReferences.Add(new GDMFileReferenceWithTitle());
            controller.MultimediaRecord = multimediaRecord;

            var mediaSampleFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");
            try {
                view.Name.Text = "sample text";
                view.MediaType.SelectedIndex = 1;
                view.StoreType.SelectedIndex = 0; // Reference

                StdDialogsStub.SetOpenedFile(mediaSampleFile);
                await controller.SelectFile();

                controller.Accept();

                Assert.AreEqual("sample text", multimediaRecord.GetFileTitle());
            } finally {
                TestUtils.RemoveTestFile(mediaSampleFile);
            }
        }

        [Test]
        public void Test_NameEditDlgController()
        {
            var view = Substitute.For<INameEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblSex");
            SubstituteControl<IGroupBox>(view, "grpPatronymics");
            SubstituteControl<ILabel>(view, "lblFemale");
            SubstituteControl<ILabel>(view, "lblMale");

            view.Name.Returns(Substitute.For<ITextBox>());
            view.FPatr.Returns(Substitute.For<ITextBox>());
            view.MPatr.Returns(Substitute.For<ITextBox>());
            view.SexCombo.Returns(Substitute.For<IComboBox>());

            var controller = new NameEditDlgController(view);
            controller.Init(fBaseWin);

            var nameEntry = new NameEntry();
            controller.NameEntry = nameEntry;

            view.SexCombo.SelectedIndex = 1; // male
            view.Name.Text = "Ivan";
            view.FPatr.Text = "Ivanovna";
            view.MPatr.Text = "Ivanovich";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("Ivan", nameEntry.Name);
            Assert.AreEqual("Ivanovich", nameEntry.M_Patronymic);
            Assert.AreEqual("Ivanovna", nameEntry.F_Patronymic);
            Assert.AreEqual(GDMSex.svMale, nameEntry.Sex);
        }

        [Test]
        public void Test_NoteEditDlgController()
        {
            var view = Substitute.For<INoteEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            view.Note.Returns(Substitute.For<ITextBox>());

            var controller = new NoteEditDlgController(view);
            controller.Init(fBaseWin);

            var noteRecord = new GDMNoteRecord(fBaseWin.Context.Tree);
            controller.NoteRecord = noteRecord;

            view.Note.Text = "sample text";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample text", noteRecord.Lines.Text);
        }

        [Test]
        public void Test_OptionsDlgController()
        {
            var view = Substitute.For<IOptionsDlg>();
            //var controller = new OptionsDlgController(view);
        }

        [Test]
        public void Test_OrganizerController()
        {
            var view = Substitute.For<IOrganizerWin>();
            SubstituteControl<ITabPage>(view, "pageAddresses");
            SubstituteControl<ITabPage>(view, "pageTelephones");
            SubstituteControl<ITabPage>(view, "pageMails");
            SubstituteControl<ITabPage>(view, "pageWebs");

            view.AdrList.Returns(Substitute.For<ISheetList>());
            view.PhonesList.Returns(Substitute.For<ISheetList>());
            view.MailsList.Returns(Substitute.For<ISheetList>());
            view.WebsList.Returns(Substitute.For<ISheetList>());

            var controller = new OrganizerController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();
        }

        [Test]
        public void Test_ParentsEditDlgController()
        {
            var view = Substitute.For<IParentsEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblChildName");
            SubstituteControl<ILabel>(view, "lblParents");
            SubstituteControl<ILabel>(view, "lblLinkageType");
            SubstituteControl<IButton>(view, "btnParentsEdit");
            SubstituteControl<IButton>(view, "btnFatherAdd");
            SubstituteControl<IButton>(view, "btnFatherDelete");
            SubstituteControl<IButton>(view, "btnMotherAdd");
            SubstituteControl<IButton>(view, "btnMotherDelete");

            view.Father.Returns(Substitute.For<ITextBox>());
            view.Mother.Returns(Substitute.For<ITextBox>());
            view.ChildName.Returns(Substitute.For<ITextBox>());
            view.LinkageTypeCombo.Returns(Substitute.For<IComboBox>());

            var controller = new ParentsEditDlgController(view);
            controller.Init(fBaseWin);

            var individual = new GDMIndividualRecord(null);
            var childLink = new GDMChildToFamilyLink();

            controller.IndividualRecord = individual;
            controller.ChildLink = childLink;

            var relRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec1);
            controller.AddFather();

            var relRec2 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec2);
            controller.AddMother();

            controller.Accept();

            controller.UpdateView();
        }

        [Test]
        public void Test_PatriarchsSearchController()
        {
            var view = Substitute.For<IPatriarchsSearchDlg>();
            SubstituteControl<ITabPage>(view, "pagePatSearch");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<ILabel>(view, "lblMinGenerations");
            SubstituteControl<IButton>(view, "btnSetPatriarch");
            SubstituteControl<IButton>(view, "btnPatSearch");
            SubstituteControl<ICheckBox>(view, "chkWithoutDates");
            SubstituteControl<IButton>(view, "btnPatriarchsDiagram");

            view.MinGensNum.Returns(Substitute.For<INumericBox>());
            view.WithoutDatesCheck.Returns(Substitute.For<ICheckBox>());
            view.PatriarchsList.Returns(Substitute.For<IListView>());

            var controller = new PatriarchsSearchController(view);
            controller.Init(fBaseWin);

            view.MinGensNum.Value = 1;
            controller.Search();
            controller.SetPatriarch();

            //ClickButton("btnPatriarchsDiagram", form);
            //var pvWin = new FormTester("PatriarchsViewerWin");
            //pvWin.Close();
        }

        [Test]
        public void Test_PersonalNameEditDlgController()
        {
            var view = Substitute.For<IPersonalNameEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblSurname");
            SubstituteControl<ILabel>(view, "lblMarriedSurname");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblPatronymic");
            SubstituteControl<ILabel>(view, "lblNickname");
            SubstituteControl<ILabel>(view, "lblSurnamePrefix");
            SubstituteControl<ILabel>(view, "lblNamePrefix");
            SubstituteControl<ILabel>(view, "lblNameSuffix");
            SubstituteControl<ILabel>(view, "lblType");
            SubstituteControl<ILabel>(view, "lblLanguage");

            view.SurnameLabel.Returns(Substitute.For<ILabel>());
            view.Surname.Returns(Substitute.For<ITextBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Patronymic.Returns(Substitute.For<ITextBox>());
            view.NameType.Returns(Substitute.For<IComboBox>());
            view.NamePrefix.Returns(Substitute.For<ITextBox>());
            view.Nickname.Returns(Substitute.For<ITextBox>());
            view.SurnamePrefix.Returns(Substitute.For<ITextBox>());
            view.NameSuffix.Returns(Substitute.For<ITextBox>());
            view.MarriedSurname.Returns(Substitute.For<ITextBox>());
            view.Language.Returns(Substitute.For<IComboBox>());

            var controller = new PersonalNameEditDlgController(view);
            controller.Init(fBaseWin);

            var person = new GDMIndividualRecord(fBaseWin.Context.Tree);
            var personalName = new GDMPersonalName();

            controller.IndividualRecord = person;
            controller.PersonalName = personalName;

            view.Surname.Text = "sample text";

            controller.Accept();

            Assert.AreEqual("sample text", personalName.Surname);

            controller.UpdateView();
        }

        [Test]
        public async Task Test_PersonEditDlgController()
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

        private static async Task ModifySheetList<T>(ISheetList sheetList, RecordAction action, object itemData) where T : ISheetModel
        {
            await ((T)sheetList.ListModel).Modify(sheetList, new ModifyEventArgs(action, itemData));
        }

        [Test]
        public void Test_PersonsFilterDlgController()
        {
            var view = Substitute.For<IPersonsFilterDlg>();
            SubstituteControl<ITabPage>(view, "pageSpecificFilter");
            SubstituteControl<IRadioButton>(view, "rbAll");
            SubstituteControl<IRadioButton>(view, "rbOnlyLive");
            SubstituteControl<IRadioButton>(view, "rbOnlyDead");
            SubstituteControl<IRadioButton>(view, "rbAliveBefore");
            SubstituteControl<IRadioButton>(view, "rbSexAll");
            SubstituteControl<IRadioButton>(view, "rbSexMale");
            SubstituteControl<IRadioButton>(view, "rbSexFemale");
            SubstituteControl<ILabel>(view, "lblNameMask");
            SubstituteControl<ILabel>(view, "lblPlaceMask");
            SubstituteControl<ILabel>(view, "lblEventsMask");
            SubstituteControl<ILabel>(view, "lblGroups");
            SubstituteControl<ILabel>(view, "lblSources");
            SubstituteControl<ICheckBox>(view, "chkOnlyPatriarchs");

            var controller = new PersonsFilterDlgController(view, RecordsListModel<GDMRecord>.Create(fBaseWin.Context, GDMRecordType.rtIndividual, false));
            controller.Init(fBaseWin);
            controller.UpdateView();

            view.GetCoreControl<IRadioButton>("rbAliveBefore").Checked = true;
            view.GetCoreControl<IRadioButton>("rbAll").Checked = true;

            view.GetCoreControl<IRadioButton>("rbSexMale").Checked = true;
            view.GetCoreControl<IRadioButton>("rbOnlyLive").Checked = true;

            view.GetCoreControl<IComboBox>("txtName").Text = "*Ivan*";

            view.GetCoreControl<IComboBox>("cmbResidence").Text = "*test place*";

            view.GetCoreControl<IComboBox>("cmbEventVal").Text = "*test event*";

            view.GetCoreControl<IComboBox>("cmbGroup").Text = "- any -";

            view.GetCoreControl<IComboBox>("cmbSource").Text = "- any -";

            controller.Accept();
        }

        [Test]
        public void Test_PlacesManagerController()
        {
            var view = Substitute.For<IPlacesManagerDlg>();
            SubstituteControl<ITabPage>(view, "pagePlaceManage");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnLocExpert");
            SubstituteControl<IButton>(view, "btnIntoList");
            SubstituteControl<IButton>(view, "btnAnalysePlaces");
            SubstituteControl<ILabel>(view, "lblFilter");

            view.FilterBox.Returns(Substitute.For<ITextBox>());
            view.PlacesList.Returns(Substitute.For<IListView>());

            var controller = new PlacesManagerController(view);
            controller.Init(fBaseWin);

            controller.CheckPlaces();
            controller.CreateLocationRecord(view.PlacesList.GetSelectedItems());
        }

        [Test]
        public void Test_PortraitSelectDlgController()
        {
            var view = Substitute.For<IPortraitSelectDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            view.ImageCtl.Returns(Substitute.For<IImageView>());

            var controller = new PortraitSelectDlgController(view);
            controller.Init(fBaseWin);

            var multimediaLink = new GDMMultimediaLink();
            controller.MultimediaLink = multimediaLink;

            controller.UpdateView();
        }

        [Test]
        public void Test_QuickSearchDlgController()
        {
            var view = Substitute.For<IQuickSearchDlg>();
            SubstituteControl<IButton>(view, "btnPrev");
            SubstituteControl<IButton>(view, "btnNext");

            view.SearchPattern.Returns(Substitute.For<ITextBox>());

            var baseWin = GetBaseSubst();
            var controller = new QuickSearchDlgController(view, baseWin);

            controller.UpdateView();

            controller.ChangeText();
            controller.FindNext();
            controller.FindPrev();
        }

        [Test]
        public void Test_RecMergeController()
        {
            var view = Substitute.For<IRecMergeDlg>();

            SubstituteControl<IGroupBox>(view, "grpSearchPersons");
            SubstituteControl<IGroupBox>(view, "grpMergeOther");
            SubstituteControl<IGroupBox>(view, "rgMode");
            SubstituteControl<ITabPage>(view, "pageMerge");
            SubstituteControl<ITabPage>(view, "pageMergeOptions");
            SubstituteControl<IButton>(view, "btnAutoSearch");
            SubstituteControl<IButton>(view, "btnSkip");
            SubstituteControl<IRadioButton>(view, "radPersons");
            SubstituteControl<IRadioButton>(view, "radNotes");
            SubstituteControl<IRadioButton>(view, "radFamilies");
            SubstituteControl<IRadioButton>(view, "radSources");
            SubstituteControl<ICheckBox>(view, "chkIndistinctMatching");
            SubstituteControl<ICheckBox>(view, "chkBirthYear");
            SubstituteControl<ILabel>(view, "lblNameAccuracy");
            SubstituteControl<ILabel>(view, "lblYearInaccuracy");
            SubstituteControl<ICheckBox>(view, "chkBookmarkMerged");
            SubstituteControl<IButton>(view, "btnRec1Select");
            SubstituteControl<IButton>(view, "btnRec2Select");
            SubstituteControl<IButton>(view, "btnEditLeft");
            SubstituteControl<IButton>(view, "btnEditRight");

            view.View1.Returns(Substitute.For<IHyperView>());
            view.View2.Returns(Substitute.For<IHyperView>());
            view.SkipBtn.Returns(Substitute.For<IButton>());
            view.ProgressBar.Returns(Substitute.For<IProgressBar>());
            view.IndistinctMatchingChk.Returns(Substitute.For<ICheckBox>());
            view.NameAccuracyNum.Returns(Substitute.For<INumericBox>());
            view.BirthYearChk.Returns(Substitute.For<ICheckBox>());
            view.YearInaccuracyNum.Returns(Substitute.For<INumericBox>());

            view.View1.Lines.Returns(new StringList());
            view.View2.Lines.Returns(new StringList());

            var controller = new RecMergeController(view);
            controller.Init(fBaseWin);

            view.GetCoreControl<ICheckBox>("chkBookmarkMerged").Checked = true;
            view.GetCoreControl<ICheckBox>("chkBookmarkMerged").Checked = false;

            view.GetCoreControl<IRadioButton>("radPersons").Checked = true;

            //CustomWindowTest.SetSelectItemHandler(0);
            //ClickButton("btnRec1Select", form);

            //CustomWindowTest.SetSelectItemHandler(1);
            //ClickButton("btnRec2Select", form);

            controller.SearchDuplicates();

            controller.Skip();
        }

        [Test]
        public void Test_RecordInfoDlgController()
        {
            var view = Substitute.For<IRecordInfoDlg>();
            SubstituteControl<IHyperView>(view, "HyperView");

            view.HyperView.Lines.Returns(new StringList());

            var controller = new RecordInfoDlgController(view);
            controller.Init(fBaseWin);

            var iRec = fBaseWin.Context.Tree.CreateIndividual();
            controller.Record = iRec;

            controller.UpdateView();
        }

        [Test]
        public void Test_RecordSelectDlgController()
        {
            var view = Substitute.For<IRecordSelectDialog>();
            SubstituteControl<IButton>(view, "btnCreate");
            SubstituteControl<IButton>(view, "btnSelect");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITextBox>(view, "txtFastFilter");

            view.FilterCombo.Returns(Substitute.For<IComboBox>());
            view.FilterText.Returns(Substitute.For<ITextBox>());
            view.FilterCtl.Returns(Substitute.For<IFilterControl>());
            view.RecordsList.Returns(Substitute.For<IListView>());

            var controller = new RecordSelectDlgController(view);
            controller.Init(fBaseWin);
            controller.RecType = GDMRecordType.rtIndividual;
        }

        [Test]
        public void Test_RelationshipCalculatorDlgController()
        {
            var view = Substitute.For<IRelationshipCalculatorDlg>();

            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnRec1Select");
            SubstituteControl<IButton>(view, "btnRec2Select");
            SubstituteControl<ILabel>(view, "lblKinship");
            SubstituteControl<IButton>(view, "btnSwap");

            view.Label1.Returns(Substitute.For<ILabel>());
            view.Label2.Returns(Substitute.For<ILabel>());
            view.Person1.Returns(Substitute.For<ITextBox>());
            view.Person2.Returns(Substitute.For<ITextBox>());
            view.Result.Returns(Substitute.For<ITextBox>());

            var controller = new RelationshipCalculatorDlgController(view);
            controller.Init(fBaseWin);

            controller.SetRec1(null);
            controller.SetRec2(null);

            GDMIndividualRecord iRec1 = fBaseWin.Context.Tree.FindXRef<GDMIndividualRecord>("I1");
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(fBaseWin.Context.Tree, iRec1, false));
            GDMIndividualRecord iRec2 = fBaseWin.Context.Tree.FindXRef<GDMIndividualRecord>("I2");
            Assert.IsNotNull(iRec2);
            Assert.AreEqual("Ivanova Maria Petrovna", GKUtils.GetRecordName(fBaseWin.Context.Tree, iRec2, false));
            AppHost.TEST_MODE = true; // FIXME: dirty hack
            controller.SetRec1(iRec1);
            controller.SetRec2(iRec2);
            Assert.AreEqual("Ivanov Ivan Ivanovich is husband of Ivanova Maria Petrovna", view.Result.Text); // :D

            var relRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec1);
            controller.SelectRec1();

            var relRec2 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec2);
            controller.SelectRec2();

            controller.UpdateView();

            controller.Swap();
        }

        [Test]
        public void Test_RepositoryEditDlgController()
        {
            var view = Substitute.For<IRepositoryEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageUserRefs");
            SubstituteControl<IButton>(view, "btnAddress");
            SubstituteControl<ILabel>(view, "lblName");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.Name.Returns(Substitute.For<ITextBox>());

            var controller = new RepositoryEditDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            var rep = baseWin.Context.Tree.CreateRepository();

            controller.RepositoryRecord = rep;
            Assert.AreEqual(rep, controller.RepositoryRecord);

            view.Name.Text = "test repo";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("test repo", rep.RepositoryName);

            // TODO: btnAddress, NotesList, UserRefList
        }

        [Test]
        public async Task Test_ResearchEditDlgController()
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

        [Test]
        public void Test_ScriptEditWinController()
        {
            var view = Substitute.For<IScriptEditWin>();
            SubstituteControl<IToolItem>(view, "tbNewScript");
            SubstituteControl<IToolItem>(view, "tbLoadScript");
            SubstituteControl<IToolItem>(view, "tbSaveScript");
            SubstituteControl<IToolItem>(view, "tbRun");

            view.ScriptText.Returns(Substitute.For<ITextBox>());
            view.DebugOutput.Returns(Substitute.For<ITextBox>());

            var controller = new ScriptEditWinController(view);
            controller.Init(fBaseWin);

            controller.NewScript();

            view.ScriptText.Text = ("gk_print(\"Hello\")");
            controller.RunScript();

            view.ScriptText.Text = ("R = gt_get_records_count()");
            controller.RunScript();

            view.ScriptText.Text = ("R = gt_get_record(0); rt = gt_get_record_type(R); " +
                                "xref = gt_get_record_xref(R); uid = gt_get_record_uid(R);" +
                                "isf = gt_record_is_filtered(R); tn = gt_get_record_type_name(rt);" +
                                "num = gt_get_records_count();");
            controller.RunScript();

            view.ScriptText.Text = ("gk_progress_init(1, \"Hello\"); gk_progress_step(); gk_progress_done(); gk_update_view()");
            controller.RunScript();

            view.ScriptText.Text = ("x = gk_strpos(\"test\", \"alpha test\");");
            controller.RunScript();

            view.ScriptText.Text = ("indi = gt_create_person(\"Ivan\", \"Ivanovich\", \"Ivanov\", \"M\");" +
                                "gt_set_person_sex(indi, \"M\"); name = gt_get_person_name(indi);" +
                                "gt_add_person_association(indi, \"rel\", indi);" +
                                "assoNum = gt_get_person_associations_count(indi);" +
                                "asso = gt_get_person_association(indi, 0);" +
                                "gt_delete_person_association(indi, 0);" +
                                "evtNum = gt_get_person_events_count(indi);" +
                                "evt = gt_get_person_event(indi, 0);" +
                                "gt_delete_person_event(indi, 0);" +
                                "parentsFam = gt_get_person_parents_family(indi);" +
                                "sx = gt_get_person_sex(indi);" +
                                "evt2 = gt_get_person_event_ex(indi, \"BIRT\");");
            controller.RunScript();

            view.ScriptText.Text = ("indi = gt_create_person(\"John\", \"\", \"Smith\", \"M\");" +
                                "evt = gt_create_event(indi, \"FACT\");" +
                                "gt_set_event_date(evt, \"08 MAR 1990\");" +
                                "gt_set_event_place(evt, \"sample place\");" +
                                "gt_set_event_value(evt, \"sample value\");" +
                                "ed = gt_get_event_date(evt);" +
                                "en = gt_get_event_name(evt);" +
                                "ep = gt_get_event_place(evt);" +
                                "ev = gt_get_event_value(evt);" +
                                "ey = gt_get_event_year(evt);");
            controller.RunScript();

            view.ScriptText.Text = ("fam = gt_create_family(); evt = gt_create_event(fam, \"MARR\");" +
                                "R = gt_get_record(0); gt_bind_family_spouse(fam, R); " +
                                "R2 = gt_get_record(1); gt_bind_family_child(fam, R2); " +
                                "chNum = gt_get_family_childs_count(fam); chl = gt_get_family_child(fam, 0);" +
                                "h = gt_get_family_husband(fam); w = gt_get_family_wife(fam);" +
                                "spNum = gt_get_person_spouses_count(R);" +
                                "fam2 = gt_get_person_spouse_family(R, 0);");
            controller.RunScript();

            view.ScriptText.Text = ("note = gt_create_note(); gt_add_note_text(note, \"test\");" +
                                "R = gt_get_record(0); gt_bind_record_note(R, note); " +
                                "ntNum = gt_get_record_notes_count(R);");
            controller.RunScript();

            view.ScriptText.Text = ("src = gt_create_source(\"source\");" +
                                "R = gt_get_record(0); gt_bind_record_source(R, src, \"p1\", 1); " +
                                "src = gt_find_source(\"source\");");
            controller.RunScript();

            view.ScriptText.Text = ("grp = gt_create_group(\"group\");" +
                                "R = gt_get_record(0); gt_bind_group_member(grp, R); " +
                                "gname = gt_get_group_name(grp);" +
                                "gNum = gt_get_person_groups_count(R); grp1 = gt_get_person_group(R, 0);" +
                                "gt_delete_record(grp);");
            controller.RunScript();

            view.ScriptText.Text = ("x = gt_get_location_usages(loc);"); // -1
            controller.RunScript();

            view.ScriptText.Text = ("con = ado_open(\"test\"); qr = ado_query_open(con, \"select * from X\"); " +
                                "ado_query_first(con); ado_query_prev(con);" +
                                "ado_query_next(con); ado_query_last(con);" +
                                "x = ado_get_query_field(con, \"field\");" +
                                "ado_query_close(qr); ado_dump(con);  ado_close(con);");
            controller.RunScript();

            //StdDialogsStub.SetOpenedFile(...);
            view.ScriptText.Text = ("file = gk_select_file();");
            controller.RunScript();

            //StdDialogsStub.SetOpenedFile(...);
            view.ScriptText.Text = ("R = gt_select_record(rtIndividual);");
            controller.RunScript();

            /*SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            ClickToolStripButton("tbLoadScript", form);

            SetModalFormHandler(fFormTest, SaveFile_Cancel_Handler);
            ClickToolStripButton("tbSaveScript", form);*/
        }

        [Test]
        public void Test_SexCheckDlgController()
        {
            var view = Substitute.For<ISexCheckDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IGroupBox>(view, "grpSex");
            SubstituteControl<IRadioButton>(view, "rbNone");
            SubstituteControl<IRadioButton>(view, "rbMale");
            SubstituteControl<IRadioButton>(view, "rbFemale");
            SubstituteControl<ITextBox>(view, "txtName");

            var controller = new SexCheckDlgController(view);
            controller.Init(fBaseWin);

            //controller.indi = "test indi";
            controller.Sex = GDMSex.svMale;

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual(GDMSex.svMale, controller.Sex);
        }

        [Test]
        public void Test_SlideshowController()
        {
            var view = Substitute.For<ISlideshowWin>();
            SubstituteControl<IButtonToolItem>(view, "tbStart");
            SubstituteControl<IButtonToolItem>(view, "tbPrev");
            SubstituteControl<IButtonToolItem>(view, "tbNext");

            var controller = new SlideshowController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();
        }

        [Test]
        public void Test_SourceCitEditDlgController()
        {
            var view = Substitute.For<ISourceCitEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageOther");
            SubstituteControl<ILabel>(view, "lblSource");
            SubstituteControl<ILabel>(view, "lblPage");
            SubstituteControl<ILabel>(view, "lblCertainty");
            SubstituteControl<IButton>(view, "btnSourceAdd");

            view.Page.Returns(Substitute.For<ITextBox>());
            view.Certainty.Returns(Substitute.For<IComboBox>());
            view.Source.Returns(Substitute.For<IComboBox>());
            view.DataDate.Returns(Substitute.For<IDateControl>());
            view.DataText.Returns(Substitute.For<ITextBox>());

            var controller = new SourceCitEditDlgController(view);
            controller.Init(fBaseWin);

            var sourceCitation = new GDMSourceCitation();
            controller.SourceCitation = sourceCitation;

            controller.Accept();

            controller.UpdateView();
        }

        [Test]
        public async Task Test_SourceEditDlgController()
        {
            var view = Substitute.For<ISourceEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            SubstituteControl<ILabel>(view, "lblShortTitle");
            SubstituteControl<ILabel>(view, "lblAuthor");
            SubstituteControl<ILabel>(view, "lblTitle");
            SubstituteControl<ILabel>(view, "lblPublication");
            SubstituteControl<ILabel>(view, "lblDate");

            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageText");
            SubstituteControl<ITabPage>(view, "pageRepositories");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ITabPage>(view, "pageUserRefs");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.RepositoriesList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.ShortTitle.Returns(Substitute.For<ITextBox>());
            view.Author.Returns(Substitute.For<ITextBox>());
            view.DescTitle.Returns(Substitute.For<ITextBox>());
            view.Publication.Returns(Substitute.For<ITextBox>());
            view.Text.Returns(Substitute.For<ITextBox>());
            view.Date.Returns(Substitute.For<IDateControl>());

            var controller = new SourceEditDlgController(view);
            controller.Init(fBaseWin);

            var src = fBaseWin.Context.Tree.CreateSource();
            controller.SourceRecord = src;
            Assert.AreEqual(src, controller.SourceRecord);

            // add repository
            /*var repRec = fBaseWin.Context.Tree.CreateRepository();
            RecordSelectDialogStub.SetTestResult(repRec);
            await((RepositoryCitationsListModel)view.RepositoriesList.ListModel).Modify(view.RepositoriesList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, src.RepositoryCitations.Count);
            // delete repository
            StdDialogsStub.SetQuestionResult(true);
            await((RepositoryCitationsListModel)view.RepositoriesList.ListModel).Modify(view.RepositoriesList, new ModifyEventArgs(RecordAction.raDelete, src.RepositoryCitations[0]));
            Assert.AreEqual(0, src.RepositoryCitations.Count);*/

            view.ShortTitle.Text = "sample text";
            view.Author.Lines.Returns(new string[] { "sample text" });

            controller.Accept();

            Assert.AreEqual("sample text", src.ShortTitle);
            Assert.AreEqual("sample text", src.Originator.Lines.Text);

            controller.UpdateView();
        }

        [Test]
        public async Task Test_StatisticsWinController()
        {
            var view = Substitute.For<IStatisticsWin>();
            SubstituteControl<IGroupBox>(view, "grpSummary");
            SubstituteControl<IButton>(view, "tbExcelExport");

            view.Graph.Returns(Substitute.For<IGraphControl>());
            view.ListStats.Returns(Substitute.For<IListView>());
            view.Summary.Returns(Substitute.For<IListView>());
            view.StatsType.Returns(Substitute.For<IComboBox>());

            var controller = new StatisticsWinController(view, null);
            controller.Init(fBaseWin);
            controller.UpdateStatsTypes();

            for (StatsMode sm = StatsMode.smAncestors; sm <= StatsMode.smLast; sm++) {
                view.StatsType.SelectedIndex = (int)sm;
            }

            controller.UpdateView();

            view.StatsType.SelectedIndex = 0;
            await controller.ExportToExcel();
        }

        [Test]
        public void Test_TaskEditDlgController()
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

        [Test]
        public void Test_TreeChartWinController()
        {
            var view = Substitute.For<ITreeChartWin>();
            var controller = new TreeChartWinController(view);
        }

        [Test]
        public async Task Test_TreeCheckController()
        {
            var view = Substitute.For<ITreeCheckDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeCheck");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnAnalyseBase");
            SubstituteControl<IButton>(view, "btnBaseRepair");
            SubstituteControl<IMenuItem>(view, "miDetails");
            SubstituteControl<IMenuItem>(view, "miGoToRecord");
            SubstituteControl<IMenuItem>(view, "miCopyXRef");

            SubstituteControl<ITabPage>(view, "pageOptions");
            SubstituteControl<ICheckBox>(view, "chkCheckPersonPlaces");
            SubstituteControl<ICheckBox>(view, "chkCheckCensuses");
            SubstituteControl<ICheckBox>(view, "chkCheckLinks");

            view.ChecksList.Returns(Substitute.For<IListView>());

            var controller = new TreeCheckController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();

            controller.CheckBase();
            await controller.Repair();
        }

        [Test]
        public void Test_TreeCompareController()
        {
            var view = Substitute.For<ITreeCompareDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeCompare");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<ILabel>(view, "lblFile");
            SubstituteControl<IButton>(view, "btnFileChoose");
            SubstituteControl<IGroupBox>(view, "grpMatchType");
            SubstituteControl<IRadioButton>(view, "radMatchInternal");
            SubstituteControl<IRadioButton>(view, "radMathExternal");
            SubstituteControl<IRadioButton>(view, "radAnalysis");
            SubstituteControl<IButton>(view, "btnMatch");

            view.ExternalBase.Returns(Substitute.For<ITextBox>());
            view.CompareOutput.Returns(Substitute.For<ITextBox>());

            var controller = new TreeCompareController(view);
            controller.Init(fBaseWin);

            view.GetCoreControl<IRadioButton>("radMatchInternal").Checked = true;
            controller.Match();

            view.GetCoreControl<IRadioButton>("radAnalysis").Checked = true;
            controller.Match();

            view.GetCoreControl<IRadioButton>("radMathExternal").Checked = true;

            //SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            controller.SelectExternalFile();
            //controller.Match();

            controller.UpdateView();
        }

        [Test]
        public void Test_TreeFilterDlgController()
        {
            var view = Substitute.For<ITreeFilterDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IGroupBox>(view, "rgBranchCut");
            SubstituteControl<IRadioButton>(view, "rbCutNone");
            SubstituteControl<IRadioButton>(view, "rbCutYears");
            SubstituteControl<ILabel>(view, "lblYear");
            SubstituteControl<IRadioButton>(view, "rbCutPersons");
            SubstituteControl<ILabel>(view, "lblRPSources");

            view.PersonsList.Returns(Substitute.For<ISheetList>());
            view.YearNum.Returns(Substitute.For<INumericBox>());
            view.SourceCombo.Returns(Substitute.For<IComboBox>());

            var controller = new TreeFilterDlgController(view);
            controller.Init(fBaseWin);

            var filter = new ChartFilter();
            controller.Filter = filter;

            controller.UpdateView();

            view.GetCoreControl<IRadioButton>("rbCutNone").Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.None, filter.BranchCut);

            view.GetCoreControl<IRadioButton>("rbCutYears").Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.Years, filter.BranchCut);

            view.GetCoreControl<IRadioButton>("rbCutPersons").Checked = true;
            controller.Accept();
            Assert.AreEqual(ChartFilter.BranchCutType.Persons, filter.BranchCut);
        }

        [Test]
        public void Test_TreeMergeController()
        {
            var view = Substitute.For<ITreeMergeDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeMerge");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnTreeMerge");
            SubstituteControl<ILabel>(view, "lblMasterBase");
            SubstituteControl<ILabel>(view, "lblOtherBase");
            SubstituteControl<ITextBox>(view, "edMasterBase");

            view.UpdateBase.Returns(Substitute.For<ITextBox>());
            view.SyncLog.Returns(Substitute.For<ITextBox>());

            var controller = new TreeMergeController(view);
            controller.Init(fBaseWin);

            //SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            controller.Merge();

            controller.UpdateView();
        }

        [Test]
        public void Test_TreeSplitController()
        {
            var view = Substitute.For<ITreeSplitDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeSplit");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnSelectAll");
            SubstituteControl<IButton>(view, "btnSelectFamily");
            SubstituteControl<IButton>(view, "btnSelectAncestors");
            SubstituteControl<IButton>(view, "btnSelectDescendants");
            SubstituteControl<IButton>(view, "btnSelectList");
            SubstituteControl<IButton>(view, "btnDelete");
            SubstituteControl<IButton>(view, "btnSave");

            var controller = new TreeSplitController(view);
            controller.Init(fBaseWin);

            GDMIndividualRecord iRec = fBaseWin.Context.Tree.FindXRef<GDMIndividualRecord>("I1");
            Assert.IsNotNull(iRec);

            controller.UpdateView();

            controller.Select(iRec, Tools.TreeTools.TreeWalkMode.twmAll);

            // FIXME
            //ClickButton("btnSelectFamily", form);
            //ClickButton("btnSelectAncestors", form);
            //ClickButton("btnSelectDescendants", form);
            //ClickButton("btnSelectAll", form);

            /*SetModalFormHandler(fFormTest, SaveFile_Cancel_Handler);
            ClickButton("btnSave", form);

            try {
                SetModalFormHandler(fFormTest, SaveFileGED_Handler);
                ClickButton("btnSave", form);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.ged"));
            }*/

            // FIXME
            //ModalFormHandler = MessageBox_OkHandler;
            controller.Delete(); // <<- ui.ShowMessage()
        }

        [Test]
        public void Test_UserRefEditDlgController()
        {
            var view = Substitute.For<IUserRefEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblReference");
            SubstituteControl<IComboBox>(view, "cmbRef");
            SubstituteControl<ILabel>(view, "lblRefType");
            SubstituteControl<IComboBox>(view, "cmbRefType");

            var controller = new UserRefEditDlgController(view);
            controller.ApplyTheme();

            var userRef = new GDMUserReference();

            controller.UserReference = userRef;
            Assert.AreEqual(userRef, controller.UserReference);

            view.Ref.Text = "sample text2";
            view.RefType.Text = "sample text3";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample text2", userRef.StringValue);
            Assert.AreEqual("sample text3", userRef.ReferenceType);
        }
    }
}
