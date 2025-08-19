/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.ExtData;
using GKCore.Lists;
using GKCore.Names;
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

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());

            var controller = new CommunicationEditDlgController(view);
            controller.Init(fBaseWin);

            var comm = fBaseWin.Context.Tree.CreateCommunication();

            controller.CommunicationRecord = comm;
            Assert.AreEqual(comm, controller.CommunicationRecord);

            view.Name.Text = "sample theme";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample theme", comm.CommName);
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
        }

        [Test]
        public void Test_FamilyEditDlgController()
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

            var controller = new FamilyEditDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            var fam = baseWin.Context.Tree.CreateFamily();

            controller.FamilyRecord = fam;
            Assert.AreEqual(fam, controller.FamilyRecord);

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

            Assert.AreEqual(relHusband, baseWin.Context.Tree.GetPtrValue(fam.Husband));
            Assert.AreEqual(relWife, baseWin.Context.Tree.GetPtrValue(fam.Wife));

            controller.JumpToHusband();
            controller.JumpToWife();

            //controller.DeleteHusband();
            //controller.DeleteWife();
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
            view.Language.Returns(Substitute.For<ITextBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Address.Returns(Substitute.For<ITextBox>());
            view.Tel.Returns(Substitute.For<ITextBox>());

            var controller = new FilePropertiesDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            controller.UpdateView();

            view.Name.Text = "sample text";

            controller.Accept();

            GDMSubmitterRecord submitter = baseWin.Context.Tree.GetPtrValue<GDMSubmitterRecord>(baseWin.Context.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        [Test]
        public void Test_FragmentSearchController()
        {
            var view = Substitute.For<IFragmentSearchDlg>();
            //var controller = new FragmentSearchController(view);
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
        public void Test_LanguageEditDlgController()
        {
            var view = Substitute.For<ILanguageEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblLanguage");
            SubstituteControl<IComboBox>(view, "cmbLanguage");

            Assert.AreEqual(GDMLanguageID.Unknown, view.LanguageID);

            var baseWin = Substitute.For<IBaseWindow>();
            var tree = new GDMTree();

            var controller = new LanguageEditDlgController(view);
            Assert.IsNotNull(controller);

            controller.Init(baseWin);
            Assert.AreEqual(baseWin, controller.Base);

            controller.LanguageID = GDMLanguageID.Akkadian;
            Assert.AreEqual(GDMLanguageID.Akkadian, controller.LanguageID);

            var langValue = GDMLanguageID.AngloSaxon;

            // substitutes of values
            view.LanguageCombo.GetSelectedTag<GDMLanguageID>().Returns(langValue);

            controller.UpdateView();
            Assert.IsTrue(controller.Accept());
            Assert.AreEqual(langValue, controller.LanguageID);
        }

        [Test]
        public void Test_LanguageSelectDlgController()
        {
            var view = Substitute.For<ILanguageSelectDlg>();
            //var controller = new LanguageSelectDlgController(view);
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
            //var controller = new MapsViewerWinController(view);
        }

        [Test]
        public void Test_MediaEditDlgController()
        {
            var view = Substitute.For<IMediaEditDlg>();
            //var controller = new MediaEditDlgController(view);
        }

        [Test]
        public void Test_MediaViewerController()
        {
            var view = Substitute.For<IMediaViewerWin>();
            //var controller = new MediaViewerController(view);
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
            //var controller = new OrganizerController(view);
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
            //var controller = new PatriarchsSearchController(view);
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
        public void Test_PersonEditDlgController()
        {
            var view = Substitute.For<IPersonEditDlg>();
            //var controller = new PersonEditDlgController(view);
        }

        [Test]
        public void Test_PersonsFilterDlgController()
        {
            var view = Substitute.For<IPersonsFilterDlg>();
            //var controller = new PersonsFilterDlgController(view);
        }

        [Test]
        public void Test_PlacesManagerController()
        {
            var view = Substitute.For<IPlacesManagerDlg>();
            //var controller = new PlacesManagerController(view);
        }

        [Test]
        public void Test_PortraitSelectDlgController()
        {
            var view = Substitute.For<IPortraitSelectDlg>();
            //var controller = new PortraitSelectDlgController(view);
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
            //var controller = new RecMergeController(view);
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
            //var controller = new RecordSelectDlgController(view);
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

            var controller = new RelationshipCalculatorDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            controller.SetRec1(null);
            controller.SetRec2(null);

            var relRec1 = baseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec1);
            controller.SelectRec1();

            var relRec2 = baseWin.Context.Tree.CreateIndividual();
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
        public void Test_ResearchEditDlgController()
        {
            var view = Substitute.For<IResearchEditDlg>();
            //var controller = new ResearchEditDlgController(view);
        }

        [Test]
        public void Test_ScriptEditWinController()
        {
            var view = Substitute.For<IScriptEditWin>();
            var controller = new ScriptEditWinController(view);
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
            //var controller = new SlideshowController(view);
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
            view.Author.Lines = new string[] { "sample text" };

            controller.Accept();

            Assert.AreEqual("sample text", src.ShortTitle);
            Assert.AreEqual("sample text", src.Originator.Lines.Text);

            controller.UpdateView();
        }

        [Test]
        public void Test_StatisticsWinController()
        {
            var view = Substitute.For<IStatisticsWin>();
            //var controller = new StatisticsWinController(view);
        }

        [Test]
        public void Test_TaskEditDlgController()
        {
            var view = Substitute.For<ITaskEditDlg>();
            //var controller = new TaskEditDlgController(view);
        }

        [Test]
        public void Test_TreeChartWinController()
        {
            var view = Substitute.For<ITreeChartWin>();
            //var controller = new TreeChartWinController(view);
        }

        [Test]
        public void Test_TreeCheckController()
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

            var controller = new TreeCompareController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();
        }

        [Test]
        public void Test_TreeFilterDlgController()
        {
            var view = Substitute.For<ITreeFilterDlg>();
            //var controller = new TreeFilterDlgController(view);
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

            GDMIndividualRecord iRec = fBaseWin.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            controller.UpdateView();

            controller.Select(iRec, Tools.TreeTools.TreeWalkMode.twmAll);

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
