/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

#if !MONO

using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class ControllerTests : CustomWindowTest
    {
        private IBaseWindow fBaseWin;

        [TestFixtureSetUp]
        public void SetUp()
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

            var baseContext = Substitute.For<IBaseContext>();
            baseContext.Tree.Returns(new GDMTree());
            var baseWin = Substitute.For<IBaseWindow>();
            baseWin.Context.Returns(baseContext);
            var tree = baseContext.Tree;
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

            var relValue = "test relation";
            var relPerson = tree.CreateIndividual();

            // substitutes of values
            view.Relation.Text.Returns(relValue);
            baseWin.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown).Returns(relPerson);

            controller.SetPerson();

            Assert.IsTrue(controller.Accept());
            Assert.AreEqual(relValue, association.Relation);
            Assert.AreEqual(relPerson, tree.GetPtrValue(association));
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
            //var controller = new CommonFilterDlgController(view);
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

            Assert.AreEqual("sample theme", comm.CommName);
        }

        [Test]
        public void Test_DayTipsDlgController()
        {
            var view = Substitute.For<IDayTipsDlg>();
            //var controller = new DayTipsDlgController(view);
        }

        [Test]
        public void Test_EventEditDlgController()
        {
            var view = Substitute.For<IEventEditDlg>();
            //var controller = new EventEditDlgController(view);
        }

        [Test]
        public void Test_FamilyEditDlgController()
        {
            var view = Substitute.For<IFamilyEditDlg>();
            //var controller = new FamilyEditDlgController(view);
        }

        [Test]
        public void Test_FilePropertiesDlgController()
        {
            var view = Substitute.For<IFilePropertiesDlg>();
            //var controller = new FilePropertiesDlgController(view);
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

            view.MapBrowser.Returns(Substitute.For<IMapBrowser>());
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
            //var controller = new QuickSearchDlgController(view);
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
            //var controller = new RelationshipCalculatorDlgController(view);
        }

        [Test]
        public void Test_RepositoryEditDlgController()
        {
            var view = Substitute.For<IRepositoryEditDlg>();
            //var controller = new RepositoryEditDlgController(view);
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

            view.ChecksList.Returns(Substitute.For<IListViewEx>());

            var controller = new TreeCheckController(view);
            controller.Init(fBaseWin);
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
            SubstituteControl<IButton>(view, "btnDelete");
            SubstituteControl<IButton>(view, "btnSave");

            var controller = new TreeSplitController(view);
            controller.Init(fBaseWin);

            GDMIndividualRecord iRec = fBaseWin.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            controller.Select(iRec, Tools.TreeTools.TreeWalkMode.twmAll);

            ModalFormHandler = MessageBox_OkHandler;
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

            Assert.AreEqual("sample text2", userRef.StringValue);
            Assert.AreEqual("sample text3", userRef.ReferenceType);
        }
    }
}

#endif
