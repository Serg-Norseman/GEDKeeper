/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class ControllerTests
    {
        private IBaseWindow fBaseWin;

        public ControllerTests()
        {
            TestUtils.InitUITest();

            fBaseWin = new BaseWindowStub(true);
        }

        private static void SubstituteControl<T>(IView dialog, string ctlName) where T : class, IControl
        {
            var substControl = Substitute.For<T>();
            dialog.GetControl(ctlName).Returns(substControl);
        }

        [Test]
        public void Test_AddressEditDlgController()
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
            view.MailsList.Returns(Substitute.For<ISheetList>());
            view.WebsList.Returns(Substitute.For<ISheetList>());

            var controller = new AddressEditDlgController(view);

            var addr = new GDMAddress();

            controller.Address = addr;
            Assert.AreEqual(addr, controller.Address);

            view.Country.Text = "sample country";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample country", addr.AddressCountry);
        }

        private static IBaseWindow GetBaseSubst()
        {
            var baseContext = Substitute.For<IBaseContext>();
            baseContext.Tree.Returns(new GDMTree());
            var baseWin = Substitute.For<IBaseWindow>();
            baseWin.Context.Returns(baseContext);
            return baseWin;
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
            baseWin.Context.SelectPerson(view, null, TargetMode.tmNone, GDMSex.svUnknown).Returns(relPerson);

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

            var controller = new DayTipsDlgController(view);

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
            baseWin.Context.SelectPerson(view, null, TargetMode.tmSpouse, GDMSex.svMale).Returns(relHusband);

            controller.AddHusband();

            var relWife = baseWin.Context.Tree.CreateIndividual();
            relWife.Sex = GDMSex.svFemale;
            baseWin.Context.SelectPerson(view, relHusband, TargetMode.tmSpouse, GDMSex.svFemale).Returns(relWife);

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
            controller.Accept();
        }

        [Test]
        public void Test_FragmentSearchController()
        {
            var view = Substitute.For<IFragmentSearchDlg>();
            //var controller = new FragmentSearchController(view);
        }

        [Test]
        public void Test_GroupEditDlgController()
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
            //var controller = new NameEditDlgController(view);
        }

        [Test]
        public void Test_NoteEditDlgController()
        {
            var view = Substitute.For<INoteEditDlg>();
            //var controller = new NoteEditDlgController(view);
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
            //var controller = new ParentsEditDlgController(view);
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
            //var controller = new PersonalNameEditDlgController(view);
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
            //var controller = new RecordInfoDlgController(view);
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
            baseWin.Context.SelectRecord(view, GDMRecordType.rtIndividual, null).Returns(relRec1);
            controller.SelectRec1();

            var relRec2 = baseWin.Context.Tree.CreateIndividual();
            baseWin.Context.SelectRecord(view, GDMRecordType.rtIndividual, null).Returns(relRec2);
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

            var controller = new RepositoryEditDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            var rep = baseWin.Context.Tree.CreateRepository();

            controller.RepositoryRecord = rep;
            Assert.AreEqual(rep, controller.RepositoryRecord);

            controller.UpdateView();
            controller.Accept();
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
        public void Test_SlideshowController()
        {
            var view = Substitute.For<ISlideshowWin>();
            //var controller = new SlideshowController(view);
        }

        [Test]
        public void Test_SourceCitEditDlgController()
        {
            var view = Substitute.For<ISourceCitEditDlg>();
            //var controller = new SourceCitEditDlgController(view);
        }

        [Test]
        public void Test_SourceEditDlgController()
        {
            var view = Substitute.For<ISourceEditDlg>();
            //var controller = new SourceEditDlgController(view);
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
