/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class BaseWinControllerTests
    {
        /*
        public override async void Setup()
        {
            CustomWindowTest.InitUITest();

            var appHost = new WFAppHost();

            await appHost.Init(null, false);

            var indiCols = GlobalOptions.Instance.ListOptions[GDMRecordType.rtIndividual].Columns;
            for (int i = 0; i < indiCols.Count; i++) {
                var colProps = indiCols[i];
                colProps.CurActive = true;
            }

            // at complex tests, first form hasn't focus
            var form0 = AppHost.Instance.GetRunningForms<Form>().FirstOrDefault();
            if (form0 != null) form0.Show();
            fMainWin = (Form)AppHost.Instance.GetActiveWindow();

            fCurBase = AppHost.Instance.GetCurrentFile();
            TestUtils.FillContext(fCurBase.Context);
            fCurBase.RefreshLists(true);
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
                listMan.AddCondition((byte)IndividualListModel.ColumnType.ctPatriarch, ConditionOperator.Contains, "test"); // any first column

                //ModalFormHandler = CustomWindowTest.CommonFilterDlg_btnAccept_Handler;
                //ClickToolStripButton("tbFilter", fMainWin);
                //ModalFormHandler = CustomWindowTest.CommonFilterDlg_btnReset_Handler;
                //ClickToolStripButton("tbFilter", fMainWin);
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
            SetModalFormHandler(this, CustomWindowTest.FilePropertiesDlg_btnAccept_Handler);
            ClickToolStripMenuItem("miFileProperties", fMainWin);
        }

        [Test]
        public void Test_ShowOptionsDlg()
        {
            ModalFormHandler = Dialog_Cancel_Handler;
            ClickToolStripMenuItem("miOptions", fMainWin);
            ModalFormHandler = CustomWindowTest.OptionsDlg_btnAccept_Handler;
            ClickToolStripMenuItem("miOptions", fMainWin);
        }

        [Test]
        public void Test_NavButtons()
        {
            ClickToolStripButton("tbNext", fMainWin);
            ClickToolStripButton("tbPrev", fMainWin);
        }

        [Test]
        public void Test_ShowQuickSearchDlg()
        {
            ((BaseWinSDI)fCurBase).ShowRecordsTab(GDMRecordType.rtIndividual);
            CustomWindowTest.QuickSearch_Test(this, fMainWin);
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
            CustomWindowTest.CircleChartWin_Tests(this, GetActiveForm("CircleChartWin"));
        }

        [Test]
        public void Test_ShowDescendantsCircle()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripMenuItem("miDescendantsCircle", fMainWin);
            CustomWindowTest.CircleChartWin_Tests(this, GetActiveForm("CircleChartWin"));
        }

        [Test]
        public void Test_ShowTreeAncestors()
        {
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripButton("tbTreeAncestors", fMainWin);
            CustomWindowTest.TreeChartWin_Tests(this, GetActiveForm("TreeChartWin"), TreeChartKind.ckAncestors, "I3");
        }

        [Test]
        public void Test_ShowTreeDescendants()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripButton("tbTreeDescendants", fMainWin);
            CustomWindowTest.TreeChartWin_Tests(this, GetActiveForm("TreeChartWin"), TreeChartKind.ckDescendants, "I1");
        }

        [Test]
        public void Test_ShowTreeBoth()
        {
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", fCurBase.GetSelectedPerson().XRef);
            ClickToolStripButton("tbTreeBoth", fMainWin);
            CustomWindowTest.TreeChartWin_Tests(this, GetActiveForm("TreeChartWin"), TreeChartKind.ckBoth, "I1");
        }

        [Test]
        public void Test_ShowStatisticsWin()
        {
            //ClickToolStripButton("tbStats", fMainWin);
        }

        [Test]
        public void Test_ShowSlideshowWin()
        {
            // Form.OnLoad -> FileNotFound error msg
            SetModalFormHandler(this, MessageBox_OkHandler);

            ClickToolStripMenuItem("miSlideshow", fMainWin);
            CustomWindowTest.SlideshowWin_Handler(this, GetActiveForm("SlideshowWin"));
        }

        [Test]
        public void Test_ShowRelationshipCalculatorDlg()
        {
            //SetModalFormHandler(this, CustomWindowTest.RelationshipCalculatorDlg_Handler);
            //ClickToolStripMenuItem("miRelationshipCalculator", fMainWin);
        }

        [Test]
        public void Test_ShowMapsViewerWin()
        {
            ClickToolStripMenuItem("miMap", fMainWin);
            CustomWindowTest.MapsViewerWin_Handler(this, GetActiveForm("MapsViewerWin"));
        }

        [Test]
        public async Task Test_ShowLanguageSelectDlg()
        {
            ModalFormHandler = CustomWindowTest.LanguageSelectDlg_Accept_Handler;
            await AppHost.Instance.LoadLanguage(0, false);
        }

        [Test]
        public async Task Test_ShowDayTipsDlg()
        {
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

            try {
                ModalFormHandler = SaveFilePDF_Handler;
                ClickToolStripMenuItem(menuItem, fMainWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.pdf"));
            }
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

         
         
        public static void PrepareFileSave(string fileName, IntPtr hWnd)
        {
            fileName = TestUtils.GetTempFilePath(fileName);

            var saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.SaveFile(fileName);
            saveDlg.SaveFile();
        }

        public static void SaveFileGED_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.ged", hWnd);
        }

        public static void SaveFileJPG_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.jpg", hWnd);
        }

        public static void SaveFileSVG_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.svg", hWnd);
        }

        public static void SaveFileXLS_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.xls", hWnd);
        }

        public static void SaveFilePDF_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.pdf", hWnd);
        }

        public static void SaveFileHTML_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.html", hWnd);
        }

        public static void SaveFileRTF_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.rtf", hWnd);
        }

        public static void FilePropertiesDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            FilePropertiesDlg dlg = (FilePropertiesDlg)form;
            var baseContext = dlg.Base.Context;

            EnterText("txtName", form, "sample text");

            ClickButton("btnAccept", form);

            GDMSubmitterRecord submitter = baseContext.Tree.GetPtrValue<GDMSubmitterRecord>(baseContext.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        public static void QuickSearch_Test(NUnitFormTest formTest, Form mainWin)
        {
            ClickToolStripMenuItem("miSearch", mainWin);

            var searchPanel = new FormTester("QuickSearchDlg");
            var frm = (QuickSearchDlg)searchPanel.Properties;

            // handlers for empty text
            ClickButton("btnPrev", frm);
            ClickButton("btnNext", frm);

            EnterText("txtSearchPattern", frm, "John");
            // handlers for entered text? - msgbox processing

            // NoMatchesFound error msg
            SetModalFormHandler(formTest, MessageBox_OkHandler);
            KeyDownForm(frm.Name, Keys.Enter);

            SetModalFormHandler(formTest, MessageBox_OkHandler);
            KeyDownForm(frm.Name, Keys.Enter | Keys.Shift);

            KeyDownForm(frm.Name, Keys.Escape);
        }

        public static void TreeFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        public static void SlideshowWin_Handler(CustomWindowTest formTest, Form frm)
        {
            Assert.IsInstanceOf(typeof(SlideshowWin), frm);
            var slidesWin = (SlideshowWin)frm;

            //ClickToolStripButton("tbStart", frm); // start
            //ClickToolStripButton("tbStart", frm); // stop

            SetModalFormHandler(formTest, MessageBox_OkHandler);
            ClickToolStripButton("tbNext", frm);

            SetModalFormHandler(formTest, MessageBox_OkHandler);
            ClickToolStripButton("tbPrev", frm);

            KeyDownForm(frm.Name, Keys.Escape);
        }

        public static void OptionsDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            var optDlg = ((OptionsDlg)form);

            optDlg.SetPage(OptionsPage.opCommon);

            optDlg.SetPage(OptionsPage.opTreeChart);
            CheckBox("chkPortraitsVisible", form, false);
            CheckBox("chkPortraitsVisible", form, true);

            optDlg.SetPage(OptionsPage.opCircleChart);

            optDlg.SetPage(OptionsPage.opInterface);
            CheckBox("chkExtendWomanSurnames", form, true);
            CheckBox("chkExtendWomanSurnames", form, false);

            optDlg.SetPage(OptionsPage.opPedigree);

            ClickButton("btnColumnUp", form);
            ClickButton("btnColumnDown", form);
            ClickButton("btnResetDefaults", form);

            ClickButton("btnAccept", form);
        }

        public static void MapsViewerWin_Handler(CustomWindowTest formTest, Form form)
        {
            Assert.IsInstanceOf(typeof(MapsViewerWin), form);

            ClickRadioButton("radTotal", form);

            formTest.ModalFormHandler = SaveFile_Cancel_Handler;
            ClickToolStripButton("tbSaveSnapshot", form);

            KeyDownForm(form.Name, Keys.Escape);
            form.Dispose();
        }

        public static void CircleChartWin_Tests(CustomWindowTest formTest, Form frm)
        {
            CircleChartWin ccWin = frm as CircleChartWin;
            IBaseWindow curBase = ccWin.OwnerWindow as IBaseWindow;
            Assert.IsNotNull(curBase);

            ccWin.UpdateSettings();

            Assert.IsFalse(ccWin.AllowFilter());
            Assert.IsFalse(ccWin.AllowQuickSearch());
            Assert.IsTrue(ccWin.AllowPrint());

            // forced update
            ccWin.Refresh();

            Assert.IsFalse(ccWin.NavCanBackward());
            ccWin.NavPrev();
            Assert.IsFalse(ccWin.NavCanForward());
            ccWin.NavNext();

            // empty methods
            Assert.IsNotNull(ccWin.FindAll(""));
            ccWin.QuickSearch();
            ccWin.SelectByRec(null);
            ccWin.SetFilter();

            try {
                formTest.ModalFormHandler = SaveFileJPG_Handler;
                ClickToolStripButton("tbImageSave", ccWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.jpg"));
            }

            try {
                formTest.ModalFormHandler = SaveFileSVG_Handler;
                ClickToolStripButton("tbImageSave", ccWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.svg"));
            }

            KeyDownForm(frm.Name, Keys.Escape);
            frm.Dispose();
        }

        public static void TreeChartWin_Tests(CustomWindowTest formTest, Form frm, TreeChartKind kind, string checkXRef)
        {
            TreeChartWin tcWin = frm as TreeChartWin;
            IBaseWindow curBase = tcWin.OwnerWindow as IBaseWindow;
            Assert.IsNotNull(curBase);

            Assert.AreEqual(kind, ((ITreeChartWin)tcWin).TreeBox.Model.Kind);
            tcWin.UpdateSettings();

            Assert.IsTrue(tcWin.AllowFilter());
            Assert.IsTrue(tcWin.AllowQuickSearch());
            Assert.IsTrue(tcWin.AllowPrint());

            // forced update
            tcWin.Refresh();

            Assert.Throws(typeof(ArgumentNullException), () => { tcWin.SelectByRec(null); });

            GDMIndividualRecord iRec = curBase.GetSelectedPerson();
            Assert.AreEqual(checkXRef, iRec.XRef);
            tcWin.SelectByRec(iRec);

            tcWin.NavPrev();
            tcWin.NavNext();

            formTest.ModalFormHandler = CustomWindowTest.TreeFilterDlg_btnAccept_Handler;
            tcWin.SetFilter();

            IList<ISearchResult> search = tcWin.FindAll("Maria");
            Assert.AreEqual(1, search.Count);

            ClickToolStripMenuItem("miModeBoth", tcWin);
            ClickToolStripMenuItem("miModeAncestors", tcWin);
            ClickToolStripMenuItem("miModeDescendants", tcWin);

            ClickToolStripMenuItem("miCertaintyIndex", tcWin);
            ClickToolStripMenuItem("miTraceKinships", tcWin);
            ClickToolStripMenuItem("miTraceSelected", tcWin);

            // handlers tests
            //ClickToolStripMenuItem("miEdit", tcWin);
            //ClickToolStripMenuItem("miFatherAdd", tcWin);
            //ClickToolStripMenuItem("miMotherAdd", tcWin);
            //ClickToolStripMenuItem("miSpouseAdd", tcWin);
            //ClickToolStripMenuItem("miSonAdd", tcWin);
            //ClickToolStripMenuItem("miDaughterAdd", tcWin);
            //ClickToolStripMenuItem("miFamilyAdd", tcWin);
            //ClickToolStripMenuItem("miDelete", tcWin);
            //ClickToolStripMenuItem("miRebuildKinships", tcWin);
            //ClickToolStripMenuItem("miFillColor", tcWin);
            //ClickToolStripMenuItem("miFillImage", tcWin);
            //ClickToolStripMenuItem("miRebuildTree", tcWin);

            try {
                formTest.ModalFormHandler = SaveFileJPG_Handler;
                ClickToolStripButton("tbImageSave", tcWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.jpg"));
            }

            try {
                formTest.ModalFormHandler = SaveFileSVG_Handler;
                ClickToolStripButton("tbImageSave", tcWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.svg"));
            }

#if !CI_MODE
            //formTest.ModalFormHandler = PrintDialog_Handler;
            //ClickToolStripButton("tbDocPrint", tcWin);

            formTest.ModalFormHandler = PrintDialog_Handler;
            ClickToolStripButton("tbDocPreview", tcWin);
#endif

            KeyDownForm(frm.Name, Keys.Escape);
            frm.Dispose();
        }
         */
    }
}
