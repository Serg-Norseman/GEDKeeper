/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#define MAIN_TEST

#if !MONO && !DIS_NUF
#if MAIN_TEST

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKUI.Platform;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// Tests for the main application window. Dependent calls of other windows
    /// and dialogs that are heavily dependent on the main window.
    /// </summary>
    [TestFixture, RequiresThread(ApartmentState.STA)]
    public class MainSDITests : CustomWindowTest
    {
        public override bool UseHidden
        {
            get { return true; }
        }

        private Form fMainWin;
        private IBaseWindow fCurBase;

        public override async void Setup()
        {
            TestUtilsUI.InitUITest();

            var appHost = new WFAppHost();

            // prevent LanguageSelectDlg modal dialog from showing on first run
            AppHost.Options.InterfaceLang = LangMan.LS_DEF_CODE;

            await appHost.Init(null, false);

            var indiCols = GlobalOptions.Instance.ListOptions[GDMRecordType.rtIndividual].Columns;
            for (int i = 0; i < indiCols.Count; i++) {
                var colProps = indiCols[i];
                colProps.CurActive = true;
            }

            // required for testing, otherwise the engine will require saving
            // the database (requires path of files for the archive and storage)
            GlobalOptions.Instance.AllowMediaStoreReferences = true;

            // at complex tests, first form hasn't focus
            var form0 = AppHost.Instance.GetRunningForms<Form>().FirstOrDefault();
            if (form0 != null) form0.Show();
            fMainWin = (Form)AppHost.Instance.GetActiveWindow();

            fCurBase = AppHost.Instance.GetCurrentFile();
            TestUtils.FillContext(fCurBase.Context);
            fCurBase.RefreshLists(true);
        }

        [Test]
        public void Test_ShowAbout()
        {
            ExpectModal("AboutDlg", delegate {
                ClickButton("btnClose", "AboutDlg");
            });
            ClickToolStripMenuItem("miAbout", fMainWin);
        }

        [Test]
        public void Test_GetCurrentFile()
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            Assert.IsNotNull(curBase);
            Assert.AreEqual(fMainWin, curBase);
        }

        [Test]
        public void Test_SelectRecordByXRef()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
        }

        [Test]
        public void Test_UpdateSettings()
        {
            fCurBase.UpdateSettings();
        }

        [Test]
        public void Test_DuplicateRecord()
        {
            fCurBase.SelectRecordByXRef("I1");
            ModalFormHandler = MessageBox_OkHandler;
            fCurBase.DuplicateRecord();
        }

        [Test]
        public void Test_FileNew()
        {
            ClickToolStripButton("tbFileNew", fMainWin);
        }

        [Test]
        public void Test_FileLoad()
        {
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileLoad", fMainWin);
        }

        [Test]
        public void Test_FileSaveAs()
        {
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileSaveAs", fMainWin);
        }

        [Test]
        public void Test_FileClose()
        {
            fCurBase.Context.Modified = true;
            Assert.IsTrue(fCurBase.Context.Modified);
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileClose", fMainWin);
        }

        [Test]
        public void Test_FileSave()
        {
            ModalFormHandler = MessageBox_CancelHandler;
            ClickToolStripMenuItem("miFileSave", fMainWin);
        }

        [Test]
        public void Test_Exit()
        {
            ClickToolStripMenuItem("miExit", fMainWin);
        }

        [Test]
        public async Task Test_TabsAndLists()
        {
            // calls to the different Editors
            for (GDMRecordType rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                Assert.IsNotNull(((BaseWinSDI)fCurBase).GetHyperViewByType(rt));

                fCurBase.ShowRecordsTab(rt);

                ModalFormHandler = Dialog_Cancel_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                //ModalFormHandler = EditorDlg_btnAccept_Handler;
                //ClickToolStripButton("tbRecordAdd", fMainWin);

                ModalFormHandler = Dialog_Cancel_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);

                //ModalFormHandler = EditorDlg_btnAccept_Handler;
                //ClickToolStripButton("tbRecordEdit", fMainWin);

                IRecordsListModel listMan = fCurBase.GetRecordsListManByType(rt);
                listMan.AddCondition((byte)IndividualListModel.ColumnType.ctPatriarch, ConditionKind.ck_Contains, "test"); // any first column

                ModalFormHandler = CommonFilterDlgTests.CommonFilterDlg_btnAccept_Handler;
                ClickToolStripButton("tbFilter", fMainWin);
                ModalFormHandler = CommonFilterDlgTests.CommonFilterDlg_btnReset_Handler;
                ClickToolStripButton("tbFilter", fMainWin);
            }

            Assert.IsTrue(fCurBase.Context.IsUnknown());

            fCurBase.ShowRecordsTab(GDMRecordType.rtIndividual);
            fCurBase.SelectRecordByXRef("I1");

            GDMRecord record = ((BaseWinSDI)fCurBase).GetSelectedRecordEx();
            Assert.IsNotNull(record);

            StringList recordContent = fCurBase.GetRecordContent(record, RecordContentType.Quick);
            Assert.IsNotNull(recordContent);

            Assert.IsTrue(fCurBase.Context.IsAvailableRecord(record));
            Assert.IsTrue(fCurBase.RecordIsFiltered(record));

            Assert.Throws(typeof(ArgumentNullException), () => { fCurBase.ShowMedia(null, false); });
            fCurBase.NotifyRecord(null, RecordAction.raAdd);

            IList<ISearchResult> search = fCurBase.FindAll("Maria");
            Assert.AreEqual(1, search.Count);

            Assert.AreEqual(null, await fCurBase.Context.GetChildFamily(null, false, null));

            fCurBase.NotifyRecord(null, RecordAction.raEdit);

            fCurBase.ApplyFilter();

            // default lang for tests is English
            string patr = await fCurBase.Context.DefinePatronymic(null, "Ivan", GDMSex.svMale, false);
            Assert.AreEqual("", patr);
        }

        [Test]
        public void Test_ShowSexCheckDlg()
        {
            // FIXME: Expected Modal Form did not show
            //ModalFormHandler = SexCheckDlgTests.SexCheckDlgTests_AcceptM_Handler;
            //GDMSex sex = fCurBase.Context.DefineSex("Ivan", "Ivanovich");
            //Assert.AreEqual(GDMSex.svMale, sex);
        }

        [Test]
        public void Test_ShowFilePropertiesDlg()
        {
            ModalFormHandler = Dialog_Cancel_Handler;
            ClickToolStripMenuItem("miFileProperties", fMainWin);
            SetModalFormHandler(this, FilePropertiesDlgTests.FilePropertiesDlg_btnAccept_Handler);
            ClickToolStripMenuItem("miFileProperties", fMainWin);
        }

        [Test]
        public void Test_ShowOptionsDlg()
        {
            ModalFormHandler = Dialog_Cancel_Handler;
            ClickToolStripMenuItem("miOptions", fMainWin);
            ModalFormHandler = OptionsDlgTests.OptionsDlg_btnAccept_Handler;
            ClickToolStripMenuItem("miOptions", fMainWin);
        }

        [Test]
        public void Test_NavButtons()
        {
            ClickToolStripButton("tbNext", fMainWin);
            ClickToolStripButton("tbPrev", fMainWin);
        }

        [Test]
        public void Test_ShowTreeCompareDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.TreeCompareDlg_Handler);
            ClickToolStripMenuItem("miTreeCompare", fMainWin);
        }

        [Test]
        public void Test_ShowTreeSplitDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.TreeSplitDlg_Handler);
            ClickToolStripMenuItem("miTreeSplit", fMainWin);
        }

        [Test]
        public void Test_ShowRecMergeDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.RecMergeDlg_Handler);
            ClickToolStripMenuItem("miRecMerge", fMainWin);
        }

        [Test]
        public void Test_ShowFamilyGroupsDlg()
        {
            ClickToolStripMenuItem("miFamilyGroups", fMainWin);
            TreeToolsWinTests.FamilyGroupsDlg_Handler(this, GetActiveForm("TTFamilyGroupsDlg"));
        }

        [Test]
        public void Test_ShowTreeCheckDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.TreeCheckDlg_Handler);
            ClickToolStripMenuItem("miTreeCheck", fMainWin);
        }

        [Test]
        public void Test_ShowTreeMergeDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.TreeMergeDlg_Handler);
            ClickToolStripMenuItem("miTreeMerge", fMainWin);
        }

        [Test]
        public void Test_ShowPatSearchDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.PatSearchDlg_Handler);
            ClickToolStripMenuItem("miPatSearch", fMainWin);
        }

        [Test]
        public void Test_ShowPlacesManagerDlg()
        {
            SetModalFormHandler(this, TreeToolsWinTests.PlacesManagerDlg_Handler);
            ClickToolStripMenuItem("miPlacesManager", fMainWin);
        }

        [Test]
        public void Test_ShowQuickSearchDlg()
        {
            ((BaseWinSDI)fCurBase).ShowRecordsTab(GDMRecordType.rtIndividual);
            QuickSearchDlgTests.QuickSearch_Test(this, fMainWin);
        }

        [Test]
        public void Test_ExportToFamilyBook()
        {
            try {
                ModalFormHandler = SaveFilePDF_Handler;
                ClickToolStripMenuItem("miExportToFamilyBook", fMainWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.pdf"));
            }
        }

        [Test]
        public void Test_ExportTable()
        {
            try {
                ModalFormHandler = SaveFileXLS_Handler;
                ClickToolStripMenuItem("miExportTable", fMainWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.xls"));
            }
        }

        [Test]
        public void Test_ExportToTreesAlbum()
        {
            // FIXME
            //ModalFormHandler = SaveFilePDF_Handler;
            //ClickToolStripMenuItem("miExportToTreesAlbum", fMainWin);
        }

        [Test]
        public void Test_ShowAncestorsCircle()
        {
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripMenuItem("miAncestorsCircle", fMainWin);
            CircleChartWinTests.CircleChartWin_Tests(this, GetActiveForm("CircleChartWin"));
        }

        [Test]
        public void Test_ShowDescendantsCircle()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripMenuItem("miDescendantsCircle", fMainWin);
            CircleChartWinTests.CircleChartWin_Tests(this, GetActiveForm("CircleChartWin"));
        }

        [Test]
        public void Test_ShowTreeAncestors()
        {
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripButton("tbTreeAncestors", fMainWin);
            TreeChartWinTests.TreeChartWin_Tests(this, GetActiveForm("TreeChartWin"), TreeChartKind.ckAncestors, "I3");
        }

        [Test]
        public void Test_ShowTreeDescendants()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripButton("tbTreeDescendants", fMainWin);
            TreeChartWinTests.TreeChartWin_Tests(this, GetActiveForm("TreeChartWin"), TreeChartKind.ckDescendants, "I1");
        }

        [Test]
        public void Test_ShowTreeBoth()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripButton("tbTreeBoth", fMainWin);
            TreeChartWinTests.TreeChartWin_Tests(this, GetActiveForm("TreeChartWin"), TreeChartKind.ckBoth, "I1");
        }

        [Test]
        public void Test_ShowStatisticsWin()
        {
            ClickToolStripButton("tbStats", fMainWin);
            StatisticsWinTests.StatsWin_Handler(this, GetActiveForm("StatisticsWin"));
        }

        [Test]
        public void Test_ShowSlideshowWin()
        {
            // Form.OnLoad -> FileNotFound error msg
            SetModalFormHandler(this, MessageBox_OkHandler);

            ClickToolStripMenuItem("miSlideshow", fMainWin);
            SlideshowWinTests.SlideshowWin_Handler(this, GetActiveForm("SlideshowWin"));
        }

        [Test]
        public void Test_ShowScriptsEditWin()
        {
            SetModalFormHandler(this, ScriptEditWinTests.ScriptEditWin_Handler);
            ClickToolStripMenuItem("miScripts", fMainWin);
        }

        [Test]
        public void Test_ShowOrganizerWin()
        {
            ModalFormHandler = OrganizerWinTests.OrganizerWin_Handler;
            ClickToolStripMenuItem("miOrganizer", fMainWin);
        }

        [Test]
        public void Test_ShowRelationshipCalculatorDlg()
        {
            SetModalFormHandler(this, RelationshipCalculatorDlgTests.RelationshipCalculatorDlg_Handler);
            ClickToolStripMenuItem("miRelationshipCalculator", fMainWin);
        }

        [Test]
        public void Test_ShowMapsViewerWin()
        {
            ClickToolStripMenuItem("miMap", fMainWin);
            MapsViewerWinTests.MapsViewerWin_Handler(this, GetActiveForm("MapsViewerWin"));
        }

        [Test]
        public async Task Test_ShowLanguageSelectDlg()
        {
            ModalFormHandler = LanguageSelectDlgTests.LanguageSelectDlg_Accept_Handler;
            await AppHost.Instance.LoadLanguage(0, false);
        }

        [Test]
        public async Task Test_ShowDayTipsDlg()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fCurBase.Context.CollectTips(null); });
            fCurBase.Context.CollectTips(new StringList());

            // FIXME: don't show dialog because BirthDays is empty
            //ModalFormHandler = DayTipsDlgTests.CloseModalHandler;
            await AppHost.Instance.ShowTips();
        }

        [Test]
        public void Test_GenPedigreeAscend()
        {
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef);
            GeneratePedigree("miPedigreeAscend");
        }

        [Test]
        public void Test_GenPedigreeDescend()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            GeneratePedigree("miPedigreeDescend");
        }

        private void GeneratePedigree(string menuItem)
        {
            try {
                ModalFormHandler = SaveFileHTML_Handler;
                ClickToolStripMenuItem(menuItem, fMainWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.html"));
            }

            try {
                ModalFormHandler = SaveFileRTF_Handler;
                ClickToolStripMenuItem(menuItem, fMainWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.rtf"));
            }

#if !MONO
            try {
                ModalFormHandler = SaveFilePDF_Handler;
                ClickToolStripMenuItem(menuItem, fMainWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.pdf"));
            }
#endif
        }

        [Test]
        public void Test_Other()
        {
            var appHost = (WFAppHost)AppHost.Instance;
            Assert.IsNotNull(appHost);

            appHost.BaseClosed(null);
            appHost.CloseWindow(null);
            appHost.SaveWinMRU(null);

            Assert.Throws(typeof(ArgumentNullException), () => { AppHost.Instance.RequestGeoCoords(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { AppHost.Instance.RequestGeoCoords("Moscow", null); });

            ILangMan langMan = appHost.CreateLangMan(null);
            Assert.IsNull(langMan);

            appHost.WidgetShow(null);
            appHost.WidgetClose(null);
            Assert.IsFalse(appHost.IsWidgetActive(null));
            appHost.EnableWindow(null, false);
            appHost.BaseRenamed(null, "", "");

            ModalFormHandler = MessageBox_OkHandler;
            AppHost.StdDialogs.ShowWarning("test warn");

            ModalFormHandler = MessageBox_OkHandler;
            AppHost.StdDialogs.ShowMessage("test msg");

            ModalFormHandler = MessageBox_OkHandler;
            AppHost.StdDialogs.ShowError("test error msg");

            IBaseWindow baseWin = appHost.FindBase("Unknown");
            Assert.IsNotNull(baseWin);

            GlobalOptions.Instance.LastDir = "";
            string ufPath = appHost.GetUserFilesPath("");
            Assert.AreEqual(GKUtils.GetHomePath(), ufPath);
            Assert.IsFalse(string.IsNullOrEmpty(ufPath));

            AppHost.Instance.AddMRU("test.ged");

            fMainWin.Activate();
            Assert.AreEqual("Unknown", AppHost.Instance.GetCurrentFileName());
        }
    }
}

#endif
#endif
