/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

#if !__MonoCS__

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKTests.ControlTesters;
using GKUI.Components;
using GKUI.Forms;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// Tests for the main application window. Dependent calls of other windows
    /// and dialogs that are heavily dependent on the main window.
    /// </summary>
    [TestFixture]
    public class MainSDITests : CustomWindowTest
    {
        private Form fMainWin;
        private IBaseWindow fCurBase;

        public override void Setup()
        {
            base.Setup();

            WFAppHost.ConfigureBootstrap(false);

            var appHost = new WFAppHost();
            appHost.Init(null, false);

            var indiCols = GlobalOptions.Instance.IndividualListColumns;
            for (int i = 0; i < indiCols.Count; i++) {
                var colProps = indiCols[i];
                colProps.CurActive = true;
            }
        }

        public void AboutDlg_Handler()
        {
            ClickButton("btnClose", "AboutDlg");
        }

        [STAThread, Test]
        public void Test_Common()
        {
            // required for testing, otherwise the engine will require saving
            // the database (requires path of files for the archive and storage)
            GlobalOptions.Instance.AllowMediaStoreReferences = true;

            var appHost = (WFAppHost)AppHost.Instance;
            Assert.IsNotNull(appHost.AppContext);

            appHost.BaseClosed(null);
            appHost.CloseWindow(null);
            appHost.SaveWinMRU(null);

            //

            // at complex tests, first form hasn't focus
            ((Form)AppHost.Instance.RunningForms[0]).Show(); // FIXME

            fMainWin = (Form)AppHost.Instance.GetActiveWindow();

            // Stage 1: call to AboutDlg, closing in AboutDlg_Handler
            ExpectModal("AboutDlg", "AboutDlg_Handler");
            //ModalFormHandler = AboutDlgTests.AboutDlg_Handler;
            ClickToolStripMenuItem("miAbout", fMainWin);


            // Stage 2.1: GetCurrentFile()
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            Assert.IsNotNull(curBase, "Stage 2.1");
            Assert.AreEqual(fMainWin, curBase);

            // Stage 2.2: create an empty base
            //ClickToolStripButton("tbFileNew", fBaseSDI);

            // Stage 2.3: GetCurrentFile()
            fCurBase = AppHost.Instance.GetCurrentFile();
            Assert.IsNotNull(fCurBase, "Stage 2.3");

            // Stage 2.4: fill context for sample data
            TestUtils.FillContext(fCurBase.Context);
            fCurBase.UpdateSettings();

            // Stage 2.5: select first individual record in base
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);

            // Stage 3: call to FilePropertiesDlg
            ModalFormHandler = Dialog_Cancel_Handler;
            ClickToolStripMenuItem("miFileProperties", fMainWin);
            SetModalFormHandler(this, FilePropertiesDlgTests.FilePropertiesDlg_btnAccept_Handler);
            ClickToolStripMenuItem("miFileProperties", fMainWin);


            // Stage 4: call to OptionsDlg
            ModalFormHandler = Dialog_Cancel_Handler;
            ClickToolStripMenuItem("miOptions", fMainWin);
            ModalFormHandler = OptionsDlgTests.OptionsDlg_btnAccept_Handler;
            ClickToolStripMenuItem("miOptions", fMainWin);


            // Stage 5: internals of BaseWin
            BaseWin_Tests(fCurBase, "Stage 5");


            // Stage 6
            ModalFormHandler = DayTipsDlgTests.CloseModalHandler;
            AppHost.Instance.ShowTips(); // don't show dialog because BirthDays is empty

            AppHost.Instance.AddMRU("test.ged");

            fMainWin.Activate();
            Assert.AreEqual("Unknown", AppHost.Instance.GetCurrentFileName(), "check AppHost.Instance.GetCurrentFileName()");

            Assert.Throws(typeof(ArgumentNullException), () => { AppHost.Instance.RequestGeoCoords(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { AppHost.Instance.RequestGeoCoords("Moscow", null); });

            // IHost tests
            //IHost host = fMainWin;
            // FIXME: !!!
            IHost host = AppHost.Instance;

            GlobalOptions.Instance.LastDir = "";
            string ufPath = host.GetUserFilesPath("");
            Assert.AreEqual(GKUtils.GetHomePath(), ufPath);
            Assert.IsFalse(string.IsNullOrEmpty(ufPath));

            IBaseWindow baseWin = host.FindBase("Unknown");
            Assert.IsNotNull(baseWin);

            ModalFormHandler = MessageBox_OkHandler;
            AppHost.StdDialogs.ShowWarning("test warn");

            ILangMan langMan = host.CreateLangMan(null);
            Assert.IsNull(langMan);

            host.WidgetShow(null);
            host.WidgetClose(null);
            Assert.IsFalse(host.IsWidgetActive(null));
            host.EnableWindow(null, false);
            host.BaseRenamed(null, "", "");

            ClickToolStripButton("tbNext", fMainWin);
            ClickToolStripButton("tbPrev", fMainWin);


            // Stage 7: call to QuickFind
            ((BaseWinSDI)fCurBase).ShowRecordsTab(GDMRecordType.rtIndividual);
            QuickSearchDlgTests.QuickSearch_Test(fMainWin);


            // Stage 21: call to TreeToolsWin
            SetModalFormHandler(this, TreeToolsWinTests.TreeCompareDlg_Handler);
            ClickToolStripMenuItem("miTreeCompare", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.TreeMergeDlg_Handler);
            ClickToolStripMenuItem("miTreeMerge", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.TreeSplitDlg_Handler);
            ClickToolStripMenuItem("miTreeSplit", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.RecMergeDlg_Handler);
            ClickToolStripMenuItem("miRecMerge", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.FamilyGroupsDlg_Handler);
            ClickToolStripMenuItem("miFamilyGroups", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.TreeCheckDlg_Handler);
            ClickToolStripMenuItem("miTreeCheck", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.PatSearchDlg_Handler);
            ClickToolStripMenuItem("miPatSearch", fMainWin);

            SetModalFormHandler(this, TreeToolsWinTests.PlacesManagerDlg_Handler);
            ClickToolStripMenuItem("miPlacesManager", fMainWin);


            // Stage 22-24: call to exports
            Exporter.TEST_MODE = true;

            ModalFormHandler = SaveFilePDF_Handler;
            ClickToolStripMenuItem("miExportToFamilyBook", fMainWin);

            ModalFormHandler = SaveFileXLS_Handler;
            ClickToolStripMenuItem("miExportToExcelFile", fMainWin);

            GeneratePedigree_Tests("Stage 24");

            // FIXME: fatal loop
            //ModalFormHandler = SaveFilePDF_Handler;
            //ClickToolStripMenuItem("miExportToTreesAlbum", fMainWin);


            // Stage 25: call to CircleChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef, "Stage 25.0");
            ClickToolStripMenuItem("miAncestorsCircle", fMainWin);
            CircleChartWinTests.CircleChartWin_Tests(this, GetActiveForm("CircleChartWin"), "Stage 25");

            // Stage 26: call to CircleChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef, "Stage 26.0");
            ClickToolStripMenuItem("miDescendantsCircle", fMainWin);
            CircleChartWinTests.CircleChartWin_Tests(this, GetActiveForm("CircleChartWin"), "Stage 26");


            // Stage 27: call to TreeChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef, "Stage 27.0");
            ClickToolStripButton("tbTreeAncestors", fMainWin);
            TreeChartWinTests.TreeChartWin_Tests(this, fMainWin, GetActiveForm("TreeChartWin"), TreeChartKind.ckAncestors, "Stage 27", "I3");


            // Stage 28: call to TreeChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef, "Stage 28.0");
            ClickToolStripButton("tbTreeDescendants", fMainWin);
            TreeChartWinTests.TreeChartWin_Tests(this, fMainWin, GetActiveForm("TreeChartWin"), TreeChartKind.ckDescendants, "Stage 28", "I1");


            // Stage 29: call to TreeChartWin (required the base, selected person)
            ClickToolStripButton("tbTreeBoth", fMainWin);
            TreeChartWinTests.TreeChartWin_Tests(this, fMainWin, GetActiveForm("TreeChartWin"), TreeChartKind.ckBoth, "Stage 29", "I1");


            // Stage 30: call to StatsWin (required the base)
            ClickToolStripButton("tbStats", fMainWin);
            StatisticsWinTests.StatsWin_Handler(this, GetActiveForm("StatisticsWin"), "Stage 30");


            // Stage 31: call to SlideshowWin (required the base)
            ClickToolStripMenuItem("miSlideshow", fMainWin);
            SlideshowWinTests.SlideshowWin_Handler(this, GetActiveForm("SlideshowWin"), "Stage 31");


            // Stage 32: call to ScriptEditWin (required the base)
            SetModalFormHandler(this, ScriptEditWinTests.ScriptEditWin_Handler);
            ClickToolStripMenuItem("miScripts", fMainWin);
            //Assert.IsTrue((Form)AppHost.Instance.GetActiveWindow(), "Stage 32");


            // Stage 33: call to OrganizerWin
            ModalFormHandler = OrganizerWinTests.OrganizerWin_Handler;
            ClickToolStripMenuItem("miOrganizer", fMainWin);


            // Stage 34: call to RelationshipCalculatorDlg
            ModalFormHandler = RelationshipCalculatorDlgTests.RelationshipCalculatorDlg_Handler;
            ClickToolStripMenuItem("miRelationshipCalculator", fMainWin);


            // Stage 35: call to MapsViewerWin (required the base)
            ClickToolStripMenuItem("miMap", fMainWin);
            MapsViewerWinTests.MapsViewerWin_Handler(this, GetActiveForm("MapsViewerWin"), "Stage 35");


            // Stage 36
            ModalFormHandler = MessageBox_OkHandler;
            fCurBase.DuplicateRecord();


            // Stage 47: close Base
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileLoad", fMainWin);


            // Stage 48: close Base
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileSaveAs", fMainWin);


            // Stage 49: close Base
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileSave", fMainWin);


            // Stage 50: close Base
            Assert.IsTrue(fCurBase.Context.Modified);
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileClose", fMainWin);


            // Stage 51: call to LanguageSelectDlg
            ModalFormHandler = LanguageSelectDlgTests.LanguageSelectDlg_Accept_Handler;
            AppHost.Instance.LoadLanguage(0);


            // Stage 52: exit
            //ClickToolStripMenuItem("miExit", fBaseSDI);


            // Other
            ModalFormHandler = MessageBox_OkHandler;
            AppHost.StdDialogs.ShowMessage("test msg");

            ModalFormHandler = MessageBox_OkHandler;
            AppHost.StdDialogs.ShowError("test error msg");
        }

        private void BaseWin_Tests(IBaseWindow baseWin, string stage)
        {
            // Stage 5: calls to the different Editors
            for (GDMRecordType rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                Assert.IsNotNull(((BaseWinSDI)baseWin).GetHyperViewByType(rt), stage + ".1");

                baseWin.ShowRecordsTab(rt);

                ModalFormHandler = Dialog_Cancel_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                ModalFormHandler = EditorDlg_btnAccept_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                ModalFormHandler = Dialog_Cancel_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);

                ModalFormHandler = EditorDlg_btnAccept_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);

                IListManager listMan = baseWin.GetRecordsListManByType(rt);
                listMan.AddCondition((byte)PersonColumnType.ctPatriarch, ConditionKind.ck_Contains, "test"); // any first column

                ModalFormHandler = CommonFilterDlgTests.CommonFilterDlg_btnAccept_Handler;
                ClickToolStripButton("tbFilter", fMainWin);
                ModalFormHandler = CommonFilterDlgTests.CommonFilterDlg_btnReset_Handler;
                ClickToolStripButton("tbFilter", fMainWin);
            }

            Assert.IsTrue(baseWin.Context.IsUnknown(), stage + ".2");

            baseWin.ShowRecordsTab(GDMRecordType.rtIndividual);
            baseWin.SelectRecordByXRef("I1");

            GDMRecord record = ((BaseWinSDI)baseWin).GetSelectedRecordEx();
            Assert.IsNotNull(record, stage + ".4");

            StringList recordContent = baseWin.GetRecordContent(record);
            Assert.IsNotNull(recordContent, stage + ".4.1");

            Assert.IsTrue(baseWin.Context.IsAvailableRecord(record), stage + ".5");
            Assert.IsTrue(baseWin.RecordIsFiltered(record), stage + ".6");

            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.ShowMedia(null, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.Context.SelectSpouseFor(null); });
            baseWin.NotifyRecord(null, RecordAction.raAdd);

            IList<ISearchResult> search = baseWin.FindAll("Maria");
            Assert.AreEqual(1, search.Count);

            Assert.AreEqual(null, baseWin.Context.GetChildFamily(null, false, null));
            Assert.AreEqual(null, baseWin.Context.AddChildForParent(null, GDMSex.svUnknown));
            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.Context.AddFamilyForSpouse(null); });

            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.Context.CollectTips(null); });
            baseWin.Context.CollectTips(new StringList());

            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.Context.CheckPersonSex(null); });

            baseWin.NotifyRecord(null, RecordAction.raEdit);

            baseWin.ApplyFilter();

            // default lang for tests is English
            string patr = baseWin.Context.DefinePatronymic("Ivan", GDMSex.svMale, false);
            Assert.AreEqual("", patr);

            ModalFormHandler = SexCheckDlgTests.SexCheckDlgTests_AcceptM_Handler;
            GDMSex sex = baseWin.Context.DefineSex("Ivan", "Ivanovich");
            Assert.AreEqual(GDMSex.svMale, sex);
        }

        #region Exports tests

        private void GeneratePedigree_Tests(string stage)
        {
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef, stage + ".1");

            GeneratePedigree(stage, "miPedigreeAscend");

            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef, stage + ".2");

            GeneratePedigree(stage, "miPedigree_dAboville");
            GeneratePedigree(stage, "miPedigree_Konovalov");
        }

        private void GeneratePedigree(string stage, string menuItem)
        {
            ModalFormHandler = SaveFileHTML_Handler;
            ClickToolStripMenuItem(menuItem, fMainWin);

            ModalFormHandler = SaveFileRTF_Handler;
            ClickToolStripMenuItem(menuItem, fMainWin);

            #if !__MonoCS__
            ModalFormHandler = SaveFilePDF_Handler;
            ClickToolStripMenuItem(menuItem, fMainWin);
            #endif
        }

        #endregion

        #region EditorDlg handlers

        public void EditorDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            if (form is NoteEditDlg) {
                NoteEditDlgTests.NoteEditDlg_Handler((NoteEditDlg) form);
                return;
            }

            if (form is FamilyEditDlg) {
                FamilyEditDlg_Handler((FamilyEditDlg) form);
                return;
            }

            if (form is GroupEditDlg) {
                GroupEditDlgTests.GroupEditDlg_Handler((GroupEditDlg) form);
                return;
            }

            if (form is PersonEditDlg) {
                PersonEditDlg_Handler((PersonEditDlg) form);
                return;
            }

            if (form is ResearchEditDlg) {
                ResearchEditDlgTests.ResearchEditDlg_Handler((ResearchEditDlg) form);
                return;
            }

            if (form is LocationEditDlg) {
                LocationEditDlgTests.LocationEditDlg_Handler((LocationEditDlg) form);
                return;
            }

            if (form is SourceEditDlg) {
                SourceEditDlgTests.SourceEditDlg_Handler((SourceEditDlg) form);
                return;
            }

            if (form is CommunicationEditDlg) {
                CommunicationEditDlgTests.CommunicationEditDlg_Handler((CommunicationEditDlg) form);
                return;
            }

            if (form is TaskEditDlg) {
                TaskEditDlgTests.TaskEditDlg_Handler((TaskEditDlg) form);
                return;
            }

            if (form is MediaEditDlg) {
                ClickButton("btnAccept", form);
                return;
            }

            if (form is RepositoryEditDlg) {
                ClickButton("btnAccept", form);
                return;
            }
        }

        private void StructsDlg_Handler(GDMRecordWithEvents record, Form dlg, TabControlTester tabs, int[] tabIndexes)
        {
            GKSheetListTester sheetTester;

            // notes
            Assert.AreEqual(0, record.Notes.Count);
            tabs.SelectTab(tabIndexes[0]);
            RecordSelectDlgTests.SetSelectItemHandler(this, 0);
            ClickToolStripButton("fNotesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.Notes.Count);

            SelectSheetListItem("fNotesList", dlg, 0);
            ClickToolStripButton("fNotesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, record.Notes.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fNotesList", dlg, 0);
            ClickToolStripButton("fNotesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, record.Notes.Count);

            // media
            Assert.AreEqual(0, record.MultimediaLinks.Count);
            tabs.SelectTab(tabIndexes[1]);
            RecordSelectDlgTests.SetSelectItemHandler(this, 0);
            ClickToolStripButton("fMediaList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.MultimediaLinks.Count);

            SelectSheetListItem("fMediaList", dlg, 0);
            ClickToolStripButton("fMediaList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, record.MultimediaLinks.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fMediaList", dlg, 0);
            ClickToolStripButton("fMediaList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, record.MultimediaLinks.Count);

            // sources
            Assert.AreEqual(0, record.SourceCitations.Count);
            tabs.SelectTab(tabIndexes[2]);
            ModalFormHandler = SourceCitEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.SourceCitations.Count);

            SelectSheetListItem("fSourcesList", dlg, 0);
            ClickToolStripButton("fSourcesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, record.SourceCitations.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fSourcesList", dlg, 0);
            ClickToolStripButton("fSourcesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, record.SourceCitations.Count);
        }

        private void FamilyEditDlg_Handler(FamilyEditDlg dlg)
        {
            GDMFamilyRecord familyRecord = dlg.Family;
            var tabs = new TabControlTester("tabsFamilyData", dlg);
            GKSheetListTester sheetTester;

            // father
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svMale);
            ClickButton("btnHusbandAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnHusbandDelete", dlg);

            // mother
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svFemale);
            ClickButton("btnWifeAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnWifeDelete", dlg);

            // childs
            Assert.AreEqual(0, familyRecord.Children.Count);
            tabs.SelectTab(0);
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svFemale);
            ClickToolStripButton("fChildsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.Children.Count);

            //SelectSheetListItem("fEventsList", dlg, 0);
            //ModalFormHandler = EventEditDlg_Select_Handler;
            //ClickToolStripButton("fChildsList_ToolBar_btnEdit", dlg);
            //Assert.AreEqual(1, familyRecord.Childrens.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fChildsList", dlg, 0);
            ClickToolStripButton("fChildsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.Children.Count);

            // events
            Assert.AreEqual(0, familyRecord.Events.Count);
            tabs.SelectTab(1);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.Events.Count);

            SelectSheetListItem("fEventsList", dlg, 0);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, familyRecord.Events.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fEventsList", dlg, 0);
            ClickToolStripButton("fEventsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.Events.Count);

            StructsDlg_Handler(familyRecord, dlg, tabs, new int[] { 2, 3, 4 });

            ClickButton("btnAccept", dlg);
        }

        private void PersonEditDlg_Handler(PersonEditDlg dlg)
        {
            GDMIndividualRecord indiRecord = dlg.Person;

            SelectCombo("cmbSex", dlg, 1); // male

            var tabs = new TabControlTester("tabsPersonData", dlg);
            GKSheetListTester sheetTester;

            var cmbRestriction = new ComboBoxTester("cmbRestriction", dlg);
            cmbRestriction.Select(3);
            cmbRestriction.Select(2);
            cmbRestriction.Select(1);
            cmbRestriction.Select(0);

            var txtSurname = new TextBoxTester("txtSurname", dlg);
            txtSurname.FireEvent("KeyDown", new KeyEventArgs(Keys.Down | Keys.Control));

            // parents
            RecordSelectDlgTests.SetCreateItemHandler(this, FamilyEditDlgTests.FamilyAdd_Mini_Handler);
            ClickButton("btnParentsAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnParentsDelete", dlg);

            // father
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svMale);
            ClickButton("btnFatherAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnFatherDelete", dlg);

            // mother
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svFemale);
            ClickButton("btnMotherAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnMotherDelete", dlg);

            ClickButton("btnNameCopy", dlg);

            // events
            tabs.SelectTab(0);
            Assert.AreEqual(1, indiRecord.Events.Count);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(2, indiRecord.Events.Count);

            SelectSheetListItem("fEventsList", dlg, 1);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(2, indiRecord.Events.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fEventsList", dlg, 1);
            ClickToolStripButton("fEventsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(1, indiRecord.Events.Count);

            // spouses
            tabs.SelectTab(1);
            Assert.AreEqual(0, indiRecord.SpouseToFamilyLinks.Count);
            ModalFormHandler = FamilyEditDlgTests.SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.SpouseToFamilyLinks.Count);

            SelectSheetListItem("fSpousesList", dlg, 1);
            ModalFormHandler = FamilyEditDlgTests.SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, indiRecord.SpouseToFamilyLinks.Count);

            SelectSheetListItem("fSpousesList", dlg, 1);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fSpousesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.SpouseToFamilyLinks.Count);

            // names
            tabs.SelectTab(2);
            Assert.AreEqual(1, indiRecord.PersonalNames.Count);
            ModalFormHandler = PersonalNameEditDlgTests.NameEditAdd_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(2, indiRecord.PersonalNames.Count);
            Assert.AreEqual("sample surname", indiRecord.PersonalNames[1].Surname);

            SelectSheetListItem("fNamesList", dlg, 1);
            ModalFormHandler = PersonalNameEditDlgTests.NameEditEdit_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(2, indiRecord.PersonalNames.Count);
            Assert.AreEqual("sample surname2", indiRecord.PersonalNames[1].Surname);

            SelectSheetListItem("fNamesList", dlg, 1);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fNamesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(1, indiRecord.PersonalNames.Count);

            // associations
            tabs.SelectTab(3);
            Assert.AreEqual(0, indiRecord.Associations.Count);
            ModalFormHandler = AssociationEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fAssociationsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.Associations.Count);
            Assert.AreEqual("sample relation", indiRecord.Associations[0].Relation);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fAssociationsList", dlg, 0);
            ClickToolStripButton("fAssociationsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.Associations.Count);

            // groups
            tabs.SelectTab(4);
            Assert.AreEqual(0, indiRecord.Groups.Count);
            RecordSelectDlgTests.SetCreateItemHandler(this, GroupEditDlgTests.GroupAdd_Mini_Handler);
            ClickToolStripButton("fGroupsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.Groups.Count);
            Assert.AreEqual("sample group", ((GDMGroupRecord)indiRecord.Groups[0].Value).GroupName);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fGroupsList", dlg, 0);
            ClickToolStripButton("fGroupsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.Groups.Count);


            StructsDlg_Handler(indiRecord, dlg, tabs, new int[] { 5, 6, 7 });


            // userrefs
            tabs.SelectTab(8);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);
            ModalFormHandler = UserRefEditDlgTests.AcceptHandler;
            ClickToolStripButton("fUserRefList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.UserReferences.Count);
            Assert.AreEqual("sample reference", indiRecord.UserReferences[0].StringValue);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fUserRefList", dlg, 0);
            ClickToolStripButton("fUserRefList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);

            ClickButton("btnAccept", dlg);
        }

        #endregion
    }
}

#endif
