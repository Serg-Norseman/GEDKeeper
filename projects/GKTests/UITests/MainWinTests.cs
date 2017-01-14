/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKTests.Service;
using GKUI;
using GKUI.Charts;
using GKUI.Dialogs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// Tests for the main application window. Dependent calls of other windows
    /// and dialogs that are heavily dependent on the main window.
    /// </summary>
    [TestFixture]
    public class MainWinTests : CustomWindowTest
    {
        private MainWin fMainWin;
        private IBaseWindow fCurBase;
        private int fIndex;

        public override void Setup()
        {
            base.Setup();
            fMainWin = new MainWin();
            fMainWin.Show();
        }

        [STAThread]
        [Test]
        public void Test_Common()
        {
            // Stage 1: call to AboutDlg, closing in AboutDlg_Handler
            ExpectModal("AboutDlg", "AboutDlg_Handler");
            ClickToolStripMenuItem("miAbout", fMainWin);


            // Stage 2.1: GetCurrentFile()
            Assert.IsNull(fMainWin.GetCurrentFile(), "Stage 2.1");

            // Stage 2.2: create an empty base
            ClickToolStripButton("tbFileNew", fMainWin);

            // Stage 2.3: GetCurrentFile()
            fCurBase = fMainWin.GetCurrentFile();
            Assert.IsNotNull(fCurBase, "Stage 2.3");

            // Stage 2.4: fill context for sample data
            TestStubs.FillContext(fCurBase.Context);
            fCurBase.UpdateView();

            // Stage 2.5: select first individual record in base
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", ((BaseWin) fCurBase).GetSelectedPerson().XRef);

            // Stage 3: call to FilePropertiesDlg
            ModalFormHandler = FilePropertiesDlg_btnCancel_Handler; // FilePropertiesDlg.Cancel
            ClickToolStripMenuItem("miFileProperties", fMainWin);
            ModalFormHandler = FilePropertiesDlg_btnAccept_Handler; // FilePropertiesDlg.Accept
            ClickToolStripMenuItem("miFileProperties", fMainWin);


            // Stage 4: call to OptionsDlg
            ModalFormHandler = OptionsDlg_btnCancel_Handler; // OptionsDlg.Cancel
            ClickToolStripMenuItem("miOptions", fMainWin);
            ModalFormHandler = OptionsDlg_btnAccept_Handler; // OptionsDlg.Accept
            ClickToolStripMenuItem("miOptions", fMainWin);


            // Stage 5: internals of BaseWin
            BaseWin_Tests(fCurBase as BaseWin, "Stage 5");


            // Stage 6
            MainWin_Test();


            // Stage 7: call to QuickFind
            QuickSearch_Test();


            // Stage 20: call to PersonsFilterDlg
            ModalFormHandler = PersonsFilterDlg_btnCancel_Handler;
            ClickToolStripMenuItem("miFilter", fMainWin);
            ModalFormHandler = PersonsFilterDlg_btnAccept_Handler;
            ClickToolStripMenuItem("miFilter", fMainWin);


            // Stage 21: call to TreeToolsWin
            ModalFormHandler = TreeToolsWin_Handler;
            ClickToolStripMenuItem("miTreeTools", fMainWin);


            // Stage 22-24: call to exports
            Exporter.TEST_MODE = true;
            GenerateFamilyBook_Tests("Stage 22");
            GenerateExcel_Tests("Stage 23");
            GeneratePedigree_Tests("Stage 24");


            // Stage 25: call to CircleChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", ((BaseWin) fCurBase).GetSelectedPerson().XRef, "Stage 25.0");
            ClickToolStripMenuItem("miAncestorsCircle", fMainWin);
            CircleChartWin_Tests(fMainWin.ActiveMdiChild, "Stage 25");

            // Stage 26: call to CircleChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", ((BaseWin) fCurBase).GetSelectedPerson().XRef, "Stage 26.0");
            ClickToolStripMenuItem("miDescendantsCircle", fMainWin);
            CircleChartWin_Tests(fMainWin.ActiveMdiChild, "Stage 26");


            // Stage 27: call to TreeChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", ((BaseWin) fCurBase).GetSelectedPerson().XRef, "Stage 27.0");
            ClickToolStripMenuItem("miTreeAncestors", fMainWin);
            TreeChartWin_Tests(fMainWin.ActiveMdiChild, TreeChartBox.ChartKind.ckAncestors, "Stage 27", "I3");


            // Stage 28: call to TreeChartWin (required the base, selected person)
            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", ((BaseWin) fCurBase).GetSelectedPerson().XRef, "Stage 28.0");
            ClickToolStripMenuItem("miTreeDescendants", fMainWin);
            TreeChartWin_Tests(fMainWin.ActiveMdiChild, TreeChartBox.ChartKind.ckDescendants, "Stage 28", "I1");


            // Stage 29: call to TreeChartWin (required the base, selected person)
            ClickToolStripMenuItem("miTreeBoth", fMainWin);
            TreeChartWin_Tests(fMainWin.ActiveMdiChild, TreeChartBox.ChartKind.ckBoth, "Stage 29", "I1");


            // Stage 30: call to StatsWin (required the base)
            ClickToolStripButton("tbStats", fMainWin);
            StatsWin_Tests(fMainWin.ActiveMdiChild, "Stage 30");


            // Stage 31: call to SlideshowWin (required the base)
            ClickToolStripMenuItem("miSlideshow", fMainWin);
            SlideshowWin_Tests(fMainWin.ActiveMdiChild, "Stage 31");


            // Stage 32: call to ScriptEditWin (required the base)
            ModalFormHandler = ScriptEditWin_Handler;
            ClickToolStripMenuItem("miScripts", fMainWin);
            //Assert.IsTrue(fMainWin.ActiveMdiChild is SlideshowWin, "Stage 32");


            // Stage 33: call to OrganizerWin
            ModalFormHandler = OrganizerWin_Handler;
            ClickToolStripMenuItem("miOrganizer", fMainWin);


            // Stage 34: call to RelationshipCalculatorDlg
            ModalFormHandler = RelationshipCalculatorDlg_Handler;
            ClickToolStripMenuItem("miRelationshipCalculator", fMainWin);


            // Stage 35: call to MapsViewerWin (required the base)
            ClickToolStripMenuItem("miMap", fMainWin);
            MapsViewerWin_Tests(fMainWin.ActiveMdiChild, "Stage 35");


            // Stage 36
            ModalFormHandler = MessageBox_OkHandler;
            ((BaseWin)fCurBase).RecordDuplicate();

            // Stage 50: close Base
            ClickToolStripMenuItem("miFileClose", fMainWin);


            // Stage 51: call to LanguageSelectDlg
            ModalFormHandler = LanguageSelectDlg_Handler;
            fMainWin.LoadLanguage(0);


            // Stage 52: exit
            ClickToolStripMenuItem("miExit", fMainWin);
        }

        private void BaseWin_Tests(BaseWin baseWin, string stage)
        {
            // Stage 5: calls to the different Editors
            for (GEDCOMRecordType rt = GEDCOMRecordType.rtIndividual; rt <= GEDCOMRecordType.rtLocation; rt++) {
                Assert.IsNotNull(baseWin.GetHyperViewByType(rt), stage + ".1");

                baseWin.ShowRecordsTab(rt);

                ModalFormHandler = EditorDlg_btnCancel_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                ModalFormHandler = EditorDlg_btnAccept_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                ModalFormHandler = EditorDlg_btnCancel_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);

                ModalFormHandler = EditorDlg_btnAccept_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);

                IListManager listMan = baseWin.GetRecordsListManByType(rt);
                listMan.AddCondition(PersonColumnType.pctPatriarch, ConditionKind.ck_Contains, "test"); // any first column

                ModalFormHandler = CommonFilterDlg_btnAccept_Handler;
                ClickToolStripMenuItem("miFilter", fMainWin);
                ModalFormHandler = CommonFilterDlg_btnReset_Handler;
                ClickToolStripMenuItem("miFilter", fMainWin);
            }

            Assert.IsTrue(baseWin.IsUnknown(), stage + ".2");

            Assert.IsNotNull(baseWin.Navman, stage + ".3");

            baseWin.ShowRecordsTab(GEDCOMRecordType.rtIndividual);
            baseWin.SelectRecordByXRef("I1");

            GEDCOMRecord record = baseWin.GetSelectedRecordEx();
            Assert.IsNotNull(record, stage + ".4");

            StringList recordContent = baseWin.GetRecordContent(record);
            Assert.IsNotNull(recordContent, stage + ".4.1");

            string recordName = baseWin.GetRecordName(record, true);
            Assert.IsNotNull(recordName, stage + ".4.2");

            Assert.IsTrue(baseWin.IsAvailableRecord(record), stage + ".5");
            Assert.IsTrue(baseWin.RecordIsFiltered(record), stage + ".6");

            Assert.AreEqual(ShieldState.Maximum, baseWin.ShieldState, stage + ".7.1");
            baseWin.ShieldState = ShieldState.None;
            Assert.AreEqual(ShieldState.None, baseWin.ShieldState, stage + ".7.2");

            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.ShowMedia(null, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.SelectSpouseFor(null); });
            baseWin.RecordNotify(null, RecordAction.raAdd);

            IList<ISearchResult> search = ((IWorkWindow)baseWin).FindAll("Maria");
            Assert.AreEqual(1, search.Count);

            Assert.AreEqual(null, baseWin.GetChildFamily(null, false, null));
            Assert.AreEqual(null, baseWin.AddChildForParent(null, GEDCOMSex.svNone));
            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.AddFamilyForSpouse(null); });

            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.CollectTips(null); });
            baseWin.CollectTips(new StringList());

            Assert.Throws(typeof(ArgumentNullException), () => { baseWin.CheckPersonSex(null); });

            baseWin.ChangeRecord(null);

            baseWin.ApplyFilter();
        }

        private void MainWin_Test()
        {
            fMainWin.AddMRU("test.ged");

            Assert.AreEqual("Unknown", fMainWin.GetCurrentFileName(), "check MainWin.GetCurrentFileName()");

            ModalFormHandler = DayTipsDlgTests.CloseModalHandler;
            fMainWin.ShowTips(); // don't show dialog because BirthDays is empty
            DayTipsDlg.ShowTipsEx("", true, null, fMainWin.Handle);

            // TODO: implement this!
            //CreateLangMan()

            //ClickToolStripButton("tbNext", fMainWin);
            //ClickToolStripButton("tbPrev", fMainWin);

            ClickToolStripMenuItem("miWinArrange", fMainWin);
            ClickToolStripMenuItem("miWinCascade", fMainWin);
            ClickToolStripMenuItem("miWinHTile", fMainWin);
            ClickToolStripMenuItem("miWinMinimize", fMainWin);
            ClickToolStripMenuItem("miWinVTile", fMainWin);
            ClickToolStripMenuItem("miWinArrange", fMainWin);
        }

        private void QuickSearch_Test()
        {
            ClickToolStripMenuItem("miSearch", fMainWin);

            var searchPanel = new FormTester("SearchPanel");
            // handlers for empty text
            ClickButton("btnPrev", searchPanel.Properties);
            ClickButton("btnNext", searchPanel.Properties);
            // enter text
            var txtSearchPattern = new TextBoxTester("txtSearchPattern", searchPanel.Properties);
            txtSearchPattern.Enter("John");
            // handlers for entered text? - msgbox processing
        }

        #region Exports tests

        private string fFileExt;

        private void GeneratePedigree_Tests(string stage)
        {
            fCurBase.SelectRecordByXRef("I3");
            Assert.AreEqual("I3", ((BaseWin) fCurBase).GetSelectedPerson().XRef, stage + ".1");

            GeneratePedigree(stage, "miPedigreeAscend");

            fCurBase.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", ((BaseWin) fCurBase).GetSelectedPerson().XRef, stage + ".2");

            GeneratePedigree(stage, "miPedigree_dAboville");
            GeneratePedigree(stage, "miPedigree_Konovalov");
        }

        private void GeneratePedigree(string stage, string menuItem)
        {
            fFileExt = ".html";
            ModalFormHandler = GeneratePedigree_Handler;
            ClickToolStripMenuItem("miPedigreeAscend", fMainWin);

            fFileExt = ".rtf";
            ModalFormHandler = GeneratePedigree_Handler;
            ClickToolStripMenuItem("miPedigreeAscend", fMainWin);

            #if !__MonoCS__
            fFileExt = ".pdf";
            ModalFormHandler = GeneratePedigree_Handler;
            ClickToolStripMenuItem("miPedigreeAscend", fMainWin);
            #endif
        }

        private void GeneratePedigree_Handler(string name, IntPtr hWnd, Form form)
        {
            string filename = GKUtils.GetTempDir() + "test" + fFileExt;
            if (File.Exists(filename)) File.Delete(filename); // for local tests!

            SaveFileDialogTester saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.SaveFile(filename);
            saveDlg.SaveFile();
        }

        private void GenerateExcel_Tests(string stage)
        {
            ModalFormHandler = GenerateExcel_Handler;
            ClickToolStripMenuItem("miExportToExcelFile", fMainWin);
        }

        private void GenerateExcel_Handler(string name, IntPtr hWnd, Form form)
        {
            string filename = GKUtils.GetTempDir() + "test.xls";
            if (File.Exists(filename)) File.Delete(filename); // for local tests!

            SaveFileDialogTester saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.SaveFile(filename);
            saveDlg.SaveFile();
        }

        private void GenerateFamilyBook_Tests(string stage)
        {
            ModalFormHandler = GenerateFamilyBook_Handler;
            ClickToolStripMenuItem("miExportToFamilyBook", fMainWin);
        }

        private void GenerateFamilyBook_Handler(string name, IntPtr hWnd, Form form)
        {
            string filename = GKUtils.GetTempDir() + "test2.pdf";
            if (File.Exists(filename)) File.Delete(filename); // for local tests!

            SaveFileDialogTester saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.SaveFile(filename);
            saveDlg.SaveFile();
        }

        #endregion

        #region AboutDlg handler

        public void AboutDlg_Handler()
        {
            ClickButton("btnClose", "AboutDlg");
        }

        #endregion

        #region FilePropertiesDlg handlers

        private void FilePropertiesDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        private void FilePropertiesDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            Assert.AreEqual(fCurBase, ((IBaseEditor)form).Base);

            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");

            ModalFormHandler = LanguageEditDlg_Handler;
            ClickButton("btnLangEdit", form);

            ClickButton("btnAccept", form);

            GEDCOMSubmitterRecord submitter = fCurBase.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region OptionsDlg handlers

        private void OptionsDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        private void OptionsDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            var optDlg = ((OptionsDlg)form);
            Assert.AreEqual(GlobalOptions.Instance, optDlg.Options);

            optDlg.SetPage(OptionsPage.opCommon);
            optDlg.SetPage(OptionsPage.opTreeChart);
            optDlg.SetPage(OptionsPage.opAncestorsCircle);
            optDlg.SetPage(OptionsPage.opInterface);
            optDlg.SetPage(OptionsPage.opPedigree);

            // CheckBoxTester uncheckBoxTester = new CheckBoxTester("aPanelName.checkBoxName", "MyFormName");
            // RadioButtonTester radioTester = new RadioButtonTester("mainFormControlName.panelName.radioButtonName",  "MyFormName");

            /*var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);*/

            ClickButton("btnColumnUp", form);
            ClickButton("btnColumnDown", form);
            ClickButton("btnDefList", form);

            ClickButton("btnAccept", form);

            //GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            //Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region XFilterDlg handlers

        private void CommonFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            CommonFilterDlg cfDlg = ((CommonFilterDlg)form);
            Assert.AreEqual(fCurBase, cfDlg.Base);

            IListManager listMan = cfDlg.ListMan;

            var tabsFilters = new TabControlTester("tabsFilters", form);
            tabsFilters.SelectTab(0);

            //var maskedTextBox = new MaskedTextBoxTester("fMaskedTextBox", form);

            var dataGridView1 = new DataGridViewTester("dataGridView1", form);
            dataGridView1.SelectCell(0, 0);
            dataGridView1.Properties.BeginEdit(false);
            dataGridView1.Properties.EndEdit();
            dataGridView1.SelectCell(0, 1);
            dataGridView1.Properties.BeginEdit(false);
            dataGridView1.Properties.EndEdit();
            dataGridView1.SelectCell(0, 2);
            dataGridView1.Properties.BeginEdit(false);
            dataGridView1.Properties.EndEdit();

            /*var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);*/

            ClickButton("btnAccept", form);

            //GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            //Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        private void CommonFilterDlg_btnReset_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnReset", form);

            ClickButton("btnAccept", form);
        }

        private void PersonsFilterDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        private void PersonsFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            Assert.AreEqual(fCurBase, ((CommonFilterDlg)form).Base);

            /*var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);*/

            ClickButton("btnAccept", form);

            //GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            //Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region ScriptEditWin handlers

        private void ScriptEditWin_Handler(string name, IntPtr ptr, Form form)
        {
            ScriptEditWin scriptWin = form as ScriptEditWin;
            Assert.AreEqual("unknown.lua", scriptWin.FileName);

            var txtScriptText = new TextBoxTester("txtScriptText");

            txtScriptText.Enter("gk_print(\"Hello\")");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("R = gt_get_records_count()");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("R = gt_get_record(0); rt = gt_get_record_type(R); "+
                                "xref = gt_get_record_xref(R); uid = gt_get_record_uid(R);"+
                                "isf = gt_record_is_filtered(R); tn = gt_get_record_type_name(rt);");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("gk_progress_init(1, \"Hello\"); gk_progress_step(); gk_progress_done(); gk_update_view()");
            ClickToolStripButton("tbRun", form);

            form.Close();
        }

        #endregion

        #region TreeToolsWin handlers

        private void TreeToolsWin_Handler(string name, IntPtr ptr, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            for (int i = 0; i < tabs.Properties.TabCount; i++) {
                tabs.SelectTab(i);

                TreeToolsWin.ToolType tt = (TreeToolsWin.ToolType)i;
                if (tt == TreeToolsWin.ToolType.ttPatSearch) {
                    var edMinGens = new NumericUpDownTester("edMinGens", form);
                    edMinGens.EnterValue(1);

                    ClickButton("btnPatSearch", form);

                    ClickButton("btnPatriarchsDiagram", form);
                    var pvWin = new FormTester("PatriarchsViewerWin");
                    pvWin.Close();
                }
            }

            form.Close();
        }

        #endregion

        #region OrganizerWin handlers

        private void OrganizerWin_Handler(string name, IntPtr ptr, Form form)
        {
            form.Close();
        }

        #endregion

        #region RelationshipCalculatorDlg handlers

        private void RelationshipCalculatorDlg_Handler(string name, IntPtr ptr, Form form)
        {
            Assert.IsTrue(fCurBase.Context.Tree.RecordsCount > 1);
            GEDCOMIndividualRecord iRec1 = fCurBase.Context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec1);
            GEDCOMIndividualRecord iRec2 = fCurBase.Context.Tree.XRefIndex_Find("I2") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec2);

            fIndex = 0;
            ModalFormHandler = RCD_RecordSelectDlg_Select_Handler; // required
            ClickButton("btnRec1Select", form);
            fIndex = 1;
            ModalFormHandler = RCD_RecordSelectDlg_Select_Handler; // required
            ClickButton("btnRec2Select", form);

            var txtResult = new TextBoxTester("txtResult", form);
            // default is not Russian culture
            Assert.AreEqual("Ivanova Maria Petrovna is wife of Ivanov Ivan Ivanovich", txtResult.Text); // :D

            ClickButton("btnClose", form);
        }

        private void RCD_RecordSelectDlg_Select_Handler(string name, IntPtr ptr, Form form)
        {
            var listRecords = new GKRecordsViewTester("fListRecords", form);

            Assert.IsFalse(listRecords.Properties.IsMainList);
            Assert.IsNotNull(listRecords.Properties.Tree);
            Assert.AreNotEqual(GEDCOMRecordType.rtNone, listRecords.Properties.RecordType);

            listRecords.Properties.SelectItem(fIndex);
            ClickButton("btnSelect", form);
        }

        #endregion

        #region LanguageSelectDlg handlers

        private void LanguageSelectDlg_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        #endregion

        #region LanguageEditDlg handlers

        private void LanguageEditDlg_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        #endregion

        #region EditorDlg handlers

        private static bool FamilyEditDlg_FirstCall = true;
        private static bool GroupEditDlg_FirstCall = true;
        private static bool PersonEditDlg_FirstCall = true;
        private static bool ResearchEditDlg_FirstCall = true;
        private static bool LocationEditDlg_FirstCall = true;
        private static bool SourceEditDlg_FirstCall = true;
        private static bool CommunicationEditDlg_FirstCall = true;
        private static bool TaskEditDlg_FirstCall = true;

        public void EditorDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        public void EditorDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            //Assert.AreEqual(fCurWin, ((IBaseEditor)form).Base);

            if (FamilyEditDlg_FirstCall && form is FamilyEditDlg) {
                FamilyEditDlg_Handler(form as FamilyEditDlg);
                FamilyEditDlg_FirstCall = false;
            }

            if (GroupEditDlg_FirstCall && form is GroupEditDlg) {
                GroupEditDlg_Handler(form as GroupEditDlg);
                GroupEditDlg_FirstCall = false;
            }

            if (PersonEditDlg_FirstCall && form is PersonEditDlg) {
                PersonEditDlg_Handler(form as PersonEditDlg);
                PersonEditDlg_FirstCall = false;
            }

            if (ResearchEditDlg_FirstCall && form is ResearchEditDlg) {
                ResearchEditDlg_Handler(form as ResearchEditDlg);
                ResearchEditDlg_FirstCall = false;
            }

            if (LocationEditDlg_FirstCall && form is LocationEditDlg) {
                LocationEditDlg_Handler(form as LocationEditDlg);
                LocationEditDlg_FirstCall = false;
            }

            if (SourceEditDlg_FirstCall && form is SourceEditDlg) {
                SourceEditDlg_Handler(form as SourceEditDlg);
                SourceEditDlg_FirstCall = false;
            }

            if (CommunicationEditDlg_FirstCall && form is CommunicationEditDlg) {
                CommunicationEditDlg_Handler(form as CommunicationEditDlg);
                CommunicationEditDlg_FirstCall = false;
            }

            if (TaskEditDlg_FirstCall && form is TaskEditDlg) {
                TaskEditDlg_Handler(form as TaskEditDlg);
                TaskEditDlg_FirstCall = false;
            }

            ClickButton("btnAccept", form);
        }

        private void FamilyEditDlg_Handler(FamilyEditDlg dlg)
        {
            GEDCOMFamilyRecord familyRecord = dlg.Family;
            var tabs = new TabControlTester("tabsFamilyData", dlg);
            GKSheetListTester sheetTester;

            // father
            fNeedIndividualSex = GEDCOMSex.svMale;
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnHusbandAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnHusbandDelete", dlg);

            // mother
            fNeedIndividualSex = GEDCOMSex.svFemale;
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnWifeAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnWifeDelete", dlg);

            // childs
            Assert.AreEqual(0, familyRecord.Childrens.Count);
            tabs.SelectTab(0);
            fNeedIndividualSex = GEDCOMSex.svFemale;
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fChildsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.Childrens.Count);

            //sheetTester = new GKSheetListTester("fEventsList", dlg);
            //sheetTester.Properties.SelectItem(0);
            //ModalFormHandler = EventEditDlg_Select_Handler;
            //ClickToolStripButton("fChildsList_ToolBar_btnEdit", dlg);
            //Assert.AreEqual(1, familyRecord.Childrens.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester = new GKSheetListTester("fChildsList", dlg);
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fChildsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.Childrens.Count);

            // events
            Assert.AreEqual(0, familyRecord.Events.Count);
            tabs.SelectTab(1);
            ModalFormHandler = EventEditDlg_Select_Handler;
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.Events.Count);

            sheetTester = new GKSheetListTester("fEventsList", dlg);
            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = EventEditDlg_Select_Handler;
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, familyRecord.Events.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fEventsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.Events.Count);

            // notes
            Assert.AreEqual(0, familyRecord.Notes.Count);
            tabs.SelectTab(2);
            ModalFormHandler = RecordSelectDlg_Select_Handler;
            ClickToolStripButton("fNotesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.Notes.Count);

            sheetTester = new GKSheetListTester("fNotesList");
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fNotesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, familyRecord.Notes.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fNotesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.Notes.Count);

            // media
            Assert.AreEqual(0, familyRecord.MultimediaLinks.Count);
            tabs.SelectTab(3);
            ModalFormHandler = RecordSelectDlg_Select_Handler;
            ClickToolStripButton("fMediaList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.MultimediaLinks.Count);

            sheetTester = new GKSheetListTester("fMediaList");
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fMediaList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, familyRecord.MultimediaLinks.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fMediaList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.MultimediaLinks.Count);

            // sources
            Assert.AreEqual(0, familyRecord.SourceCitations.Count);
            tabs.SelectTab(4);
            ModalFormHandler = SourceCitEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, familyRecord.SourceCitations.Count);

            sheetTester = new GKSheetListTester("fSourcesList");
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fSourcesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, familyRecord.SourceCitations.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fSourcesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, familyRecord.SourceCitations.Count);
        }

        private void PersonEditDlg_Handler(PersonEditDlg dlg)
        {
            GEDCOMIndividualRecord indiRecord = dlg.Person;

            var cmbSex = new ComboBoxTester("cmbSex", dlg);
            cmbSex.Select(1); // male

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
            RSD_SubHandler = FamilyAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnParentsAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnParentsDelete", dlg);

            // father
            fNeedIndividualSex = GEDCOMSex.svMale;
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnFatherAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnFatherDelete", dlg);

            // mother
            fNeedIndividualSex = GEDCOMSex.svFemale;
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnMotherAdd", dlg);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnMotherDelete", dlg);

            ClickButton("btnNameCopy", dlg);

            // events
            tabs.SelectTab(0);
            Assert.AreEqual(1, indiRecord.Events.Count);
            ModalFormHandler = EventEditDlg_Select_Handler;
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(2, indiRecord.Events.Count);

            sheetTester = new GKSheetListTester("fEventsList", dlg);
            sheetTester.Properties.SelectItem(1);
            ModalFormHandler = EventEditDlg_Select_Handler;
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(2, indiRecord.Events.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(1);
            ClickToolStripButton("fEventsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(1, indiRecord.Events.Count);

            // spouses
            tabs.SelectTab(1);
            sheetTester = new GKSheetListTester("fSpousesList", dlg);
            Assert.AreEqual(0, indiRecord.SpouseToFamilyLinks.Count);
            ModalFormHandler = SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.SpouseToFamilyLinks.Count);

            sheetTester.Properties.SelectItem(1);
            ModalFormHandler = SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, indiRecord.SpouseToFamilyLinks.Count);

            sheetTester.Properties.SelectItem(1);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fSpousesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.SpouseToFamilyLinks.Count);

            // names
            tabs.SelectTab(2);
            sheetTester = new GKSheetListTester("fNamesList", dlg);
            Assert.AreEqual(1, indiRecord.PersonalNames.Count);
            ModalFormHandler = NameEditAdd_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(2, indiRecord.PersonalNames.Count);
            Assert.AreEqual("sample surname", indiRecord.PersonalNames[1].Surname);

            sheetTester.Properties.SelectItem(1);
            ModalFormHandler = NameEditEdit_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(2, indiRecord.PersonalNames.Count);
            Assert.AreEqual("sample surname2", indiRecord.PersonalNames[1].Surname);

            sheetTester.Properties.SelectItem(1);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fNamesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(1, indiRecord.PersonalNames.Count);

            // associations
            tabs.SelectTab(3);
            sheetTester = new GKSheetListTester("fAssociationsList", dlg);
            Assert.AreEqual(0, indiRecord.Associations.Count);
            ModalFormHandler = AssociationEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fAssociationsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.Associations.Count);
            Assert.AreEqual("sample relation", indiRecord.Associations[0].Relation);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fAssociationsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.Associations.Count);

            // groups
            tabs.SelectTab(4);
            sheetTester = new GKSheetListTester("fGroupsList", dlg);
            Assert.AreEqual(0, indiRecord.Groups.Count);
            RSD_SubHandler = GroupAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fGroupsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.Groups.Count);
            Assert.AreEqual("sample group", ((GEDCOMGroupRecord)indiRecord.Groups[0].Value).GroupName);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fGroupsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.Groups.Count);



            // notes
            tabs.SelectTab(5);
            Assert.AreEqual(0, indiRecord.Notes.Count);
            RSD_SubHandler = NoteAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fNotesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.Notes.Count);

            sheetTester = new GKSheetListTester("fNotesList");
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fNotesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, indiRecord.Notes.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fNotesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.Notes.Count);

            // media
            tabs.SelectTab(6);
            Assert.AreEqual(0, indiRecord.MultimediaLinks.Count);
            RSD_SubHandler = MediaAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fMediaList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.MultimediaLinks.Count);

            sheetTester = new GKSheetListTester("fMediaList");
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fMediaList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, indiRecord.MultimediaLinks.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fMediaList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.MultimediaLinks.Count);

            // sources
            /*tabs.SelectTab(7);
            Assert.AreEqual(0, indiRecord.SourceCitations.Count);
            RSD_SubHandler = SourceAdd_Mini_Handler;
            ModalFormHandler = SourceCitEditDlg_Handler;
            ClickToolStripButton("fSourcesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.SourceCitations.Count);

            sheetTester = new GKSheetListTester("fSourcesList");
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fSourcesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, indiRecord.SourceCitations.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fSourcesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.SourceCitations.Count);*/


            // userrefs
            tabs.SelectTab(8);
            sheetTester = new GKSheetListTester("fUserRefList", dlg);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);
            ModalFormHandler = UserRefEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fUserRefList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, indiRecord.UserReferences.Count);
            Assert.AreEqual("sample reference", indiRecord.UserReferences[0].StringValue);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fUserRefList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);
        }

        private void SourceEditDlg_Handler(SourceEditDlg dlg)
        {
            GEDCOMSourceRecord srcRecord = dlg.SourceRecord;
            GKSheetListTester sheetTester;
            var tabs = new TabControlTester("tabsData", dlg);

            // repositories
            tabs.SelectTab(2);
            Assert.AreEqual(0, srcRecord.RepositoryCitations.Count);
            RSD_SubHandler = TaskAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fRepositoriesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, srcRecord.RepositoryCitations.Count);

            sheetTester = new GKSheetListTester("fRepositoriesList", dlg);
            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fRepositoriesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, srcRecord.RepositoryCitations.Count);
        }

        private void ResearchEditDlg_Handler(ResearchEditDlg dlg)
        {
            GEDCOMResearchRecord resRecord = dlg.Research;
            GKSheetListTester sheetTester;
            var tabs = new TabControlTester("tabsData", dlg);

            // tasks
            tabs.SelectTab(0);
            Assert.AreEqual(0, resRecord.Tasks.Count);
            RSD_SubHandler = TaskAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fTasksList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, resRecord.Tasks.Count);

            sheetTester = new GKSheetListTester("fTasksList", dlg);
            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = TaskAdd_Mini_Handler;
            ClickToolStripButton("fTasksList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, resRecord.Tasks.Count);

            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fTasksList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, resRecord.Tasks.Count);

            // communications
            tabs.SelectTab(1);
            Assert.AreEqual(0, resRecord.Communications.Count);
            RSD_SubHandler = CommunicationAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fCommunicationsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, resRecord.Communications.Count);

            sheetTester = new GKSheetListTester("fCommunicationsList", dlg);
            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = CommunicationAdd_Mini_Handler;
            ClickToolStripButton("fCommunicationsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, resRecord.Communications.Count);

            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fCommunicationsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, resRecord.Communications.Count);

            // groups
            tabs.SelectTab(2);
            Assert.AreEqual(0, resRecord.Groups.Count);
            RSD_SubHandler = GroupAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickToolStripButton("fGroupsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, resRecord.Groups.Count);
            Assert.AreEqual("sample group", ((GEDCOMGroupRecord)resRecord.Groups[0].Value).GroupName);

            sheetTester = new GKSheetListTester("fGroupsList", dlg);
            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = GroupAdd_Mini_Handler;
            ClickToolStripButton("fGroupsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, resRecord.Groups.Count);

            sheetTester.Properties.SelectItem(0);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fGroupsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, resRecord.Groups.Count);
        }

        private void LocationEditDlg_Handler(LocationEditDlg dlg)
        {
            GEDCOMLocationRecord resRecord = dlg.LocationRecord;
        }

        private void CommunicationEditDlg_Handler(CommunicationEditDlg dlg)
        {
            fNeedIndividualSex = GEDCOMSex.svMale;
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnPersonAdd", dlg);
        }

        private void TaskEditDlg_Handler(TaskEditDlg dlg)
        {
            var cmbGoalType = new ComboBoxTester("cmbGoalType", dlg);
            cmbGoalType.Select(3);
            ClickButton("btnGoalSelect", dlg);

            cmbGoalType.Select(2);
            RSD_SubHandler = SourceAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnGoalSelect", dlg);

            cmbGoalType.Select(1);
            RSD_SubHandler = FamilyAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnGoalSelect", dlg);

            cmbGoalType.Select(0);
            RSD_SubHandler = IndividualAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnGoalSelect", dlg);
        }

        private void GroupEditDlg_Handler(GroupEditDlg dlg)
        {
            GEDCOMGroupRecord groupRecord = dlg.Group;
            GKSheetListTester sheetTester = new GKSheetListTester("fMembersList", dlg);

            // members
            Assert.AreEqual(0, groupRecord.Members.Count);
            ModalFormHandler = RecordSelectDlg_Select_Handler;
            ClickToolStripButton("fMembersList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, groupRecord.Members.Count);

            ModalFormHandler = MessageBox_YesHandler;
            sheetTester.Properties.SelectItem(0);
            ClickToolStripButton("fMembersList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, groupRecord.Members.Count);
        }

        private void MessageBox_YesHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.Yes);
        }

        private void MessageBox_OkHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.OK);
        }

        private void EventEditDlg_Select_Handler(string name, IntPtr ptr, Form form)
        {
            EventEditDlg eventDlg = (EventEditDlg) form;
            Assert.AreEqual(fCurBase, eventDlg.Base);
            Assert.IsNotNull(eventDlg.Event);

            var cmbEventType = new ComboBoxTester("cmbEventType", form);
            cmbEventType.Select(1); // Birth(indi) / ?(fam)

            var txtEventPlace = new TextBoxTester("txtEventPlace", form);
            txtEventPlace.Enter("test place");

            var cmbEventDateType = new ComboBoxTester("cmbEventDateType", form);
            cmbEventDateType.Select(3); // Between

            var txtEventDate1 = new MaskedTextBoxTester("txtEventDate1", form);
            txtEventDate1.Enter("01.01.1900");

            var txtEventDate2 = new MaskedTextBoxTester("txtEventDate2", form);
            txtEventDate2.Enter("10.01.1900");

            var cmbDate1Calendar = new ComboBoxTester("cmbDate1Calendar", form);
            cmbDate1Calendar.Select(1); // Julian

            var cmbDate2Calendar = new ComboBoxTester("cmbDate2Calendar", form);
            cmbDate2Calendar.Select(1); // Julian

            var txtEventCause = new TextBoxTester("txtEventCause", form);
            txtEventCause.Enter("test cause");

            var txtEventOrg = new TextBoxTester("txtEventOrg", form);
            txtEventOrg.Enter("test agency");

            ModalFormHandler = AddressEditDlg_btnCancel_Handler;
            ClickButton("btnAddress", form);


            RSD_SubHandler = LocationAdd_Mini_Handler;
            ModalFormHandler = RecordSelectDlg_Create_Handler;
            ClickButton("btnPlaceAdd", form);

            ClickButton("btnPlaceDelete", form);


            ClickButton("btnAccept", form);

            // this don't working here
            //Assert.AreEqual("BIRT", eventDlg.Event.Name);
            //Assert.AreEqual("test place", eventDlg.Event.Detail.Place.StringValue);
            //Assert.IsInstanceOf(typeof(GEDCOMDateExact), eventDlg.Event.Detail.Date.Value);
            //Assert.AreEqual("01 JAN 1900", eventDlg.Event.Detail.Date.Value.StringValue);
            //Assert.AreEqual("test cause", eventDlg.Event.Detail.Cause);
            //Assert.AreEqual("test agency", eventDlg.Event.Detail.Agency);
        }

        public void AddressEditDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        #region RecordSelectDlg handlers

        private GEDCOMSex fNeedIndividualSex;

        public void IndividualAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            var txtName = new TextBoxTester("txtName", form);
            txtName.Enter("test");

            var cmbSex = new ComboBoxTester("cmbSex", form);
            cmbSex.Select((int)fNeedIndividualSex);

            ClickButton("btnAccept", form);
        }

        public void FamilyAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //var edName = new TextBoxTester("edName", form);
            //edName.Enter("sample group");

            ClickButton("btnAccept", form);
        }

        public void NoteAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            var txtNote = new TextBoxTester("txtNote");
            txtNote.Enter("sample text");

            ClickButton("btnAccept", form);
        }

        public void SourceAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            var txtShortTitle = new TextBoxTester("txtShortTitle");
            txtShortTitle.Enter("sample text");

            ClickButton("btnAccept", form);
        }

        public void MediaAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //var txtName = new TextBoxTester("txtName");
            //txtName.Enter("sample text");

            ClickButton("btnAccept", form);
        }

        public void LocationAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            var txtName = new TextBoxTester("txtName", form);
            txtName.Enter("sample location");

            ClickButton("btnAccept", form);
        }

        public void GroupAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            var edName = new TextBoxTester("edName", form);
            edName.Enter("sample group");

            ClickButton("btnAccept", form);
        }

        public void TaskAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //var edName = new TextBoxTester("edName", form);
            //edName.Enter("sample group");

            ClickButton("btnAccept", form);
        }

        public void CommunicationAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //var edName = new TextBoxTester("edName", form);
            //edName.Enter("sample group");

            ClickButton("btnAccept", form);
        }

        public void SpouseEdit_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        public void NameEditAdd_Handler(string name, IntPtr ptr, Form form)
        {
            var txtSurname = new TextBoxTester("txtSurname", form);
            txtSurname.Enter("sample surname");

            ClickButton("btnAccept", form);
        }

        public void NameEditEdit_Handler(string name, IntPtr ptr, Form form)
        {
            var txtSurname = new TextBoxTester("txtSurname", form);
            txtSurname.Enter("sample surname2");

            ClickButton("btnAccept", form);
        }

        private ModalFormHandler RSD_SubHandler;

        public void RecordSelectDlg_Create_Handler(string name, IntPtr ptr, Form form)
        {
            ModalFormHandler = RSD_SubHandler;
            ClickButton("btnCreate", form);
        }

        public void RecordSelectDlg_Select_Handler(string name, IntPtr ptr, Form form)
        {
            var txtFastFilter = new TextBoxTester("txtFastFilter", form);
            txtFastFilter.Enter("*");

            var listRecords = new GKRecordsViewTester("fListRecords", form);
            listRecords.Properties.SelectItem(0);

            ClickButton("btnSelect", form);
        }

        #endregion

        #endregion

        private void CircleChartWin_Tests(Form frm, string stage)
        {
            Assert.IsInstanceOf(typeof(CircleChartWin), frm, stage);

            CircleChartWin ccWin = frm as CircleChartWin;
            Assert.AreEqual(fCurBase, ccWin.Base);
            ccWin.UpdateView();

            // forced update
            ccWin.Refresh();

            Assert.IsFalse(ccWin.NavCanBackward());
            ccWin.NavPrev();
            Assert.IsFalse(ccWin.NavCanForward());
            ccWin.NavNext();

            // empty methods
            Assert.IsNull(ccWin.FindAll(""));
            ccWin.QuickFind();
            ccWin.SelectByRec(null);
            ccWin.SetFilter();

            frm.Close();
        }

        private void TreeChartWin_Tests(Form frm, TreeChartBox.ChartKind kind, string stage, string checkXRef)
        {
            Assert.IsInstanceOf(typeof(TreeChartWin), frm, stage);

            TreeChartWin tcWin = frm as TreeChartWin;
            Assert.AreEqual(fCurBase, tcWin.Base);
            Assert.AreEqual(kind, tcWin.ChartKind);
            tcWin.UpdateView();

            // forced update
            tcWin.Refresh();

            ClickToolStripMenuItem("miCertaintyIndex", tcWin);
            ClickToolStripMenuItem("miTraceKinships", tcWin);
            ClickToolStripMenuItem("miTraceSelected", tcWin);

            Assert.Throws(typeof(ArgumentNullException), () => { tcWin.SelectByRec(null); });

            GEDCOMIndividualRecord iRec = ((BaseWin) fCurBase).GetSelectedPerson();
            Assert.AreEqual(checkXRef, iRec.XRef);
            tcWin.SelectByRec(iRec);

            var formTester = new FormTester(tcWin.Name);
            Assert.IsNotNull(formTester);
            formTester[0].FireEvent("KeyDown", new KeyEventArgs(Keys.F5));
            formTester[0].FireEvent("KeyDown", new KeyEventArgs(Keys.F6));
            formTester[0].FireEvent("KeyDown", new KeyEventArgs(Keys.F7));

            //formTester[0].FireEvent("KeyDown", new KeyEventArgs(Keys.F | Keys.Control));

            tcWin.NavPrev();
            tcWin.NavNext();

            ModalFormHandler = TreeFilterDlg_btnAccept_Handler;
            tcWin.SetFilter();

            IList<ISearchResult> search = tcWin.FindAll("Maria");
            Assert.AreEqual(1, search.Count);

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

            frm.Close();
        }

        private void TreeFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        private void TreeFilterDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        private void StatsWin_Tests(Form frm, string stage)
        {
            Assert.IsInstanceOf(typeof(StatisticsWin), frm, stage);
            frm.Close();
        }

        private void SlideshowWin_Tests(Form frm, string stage)
        {
            Assert.IsInstanceOf(typeof(SlideshowWin), frm, stage);

            SlideshowWin slidesWin = (SlideshowWin)frm;

            Assert.AreEqual(null, slidesWin.FindAll(""));

            Assert.AreEqual(false, slidesWin.AllowFilter());
            slidesWin.SetFilter();

            slidesWin.SelectByRec(null);
            slidesWin.UpdateView();

            Assert.AreEqual(false, slidesWin.AllowQuickFind());
            slidesWin.QuickFind();

            Assert.AreEqual(false, slidesWin.NavCanBackward());
            slidesWin.NavPrev();

            Assert.AreEqual(false, slidesWin.NavCanForward());
            slidesWin.NavNext();

            frm.Close();
        }

        private void MapsViewerWin_Tests(Form frm, string stage)
        {
            Assert.IsInstanceOf(typeof(MapsViewerWin), frm, stage);
            frm.Close();
        }
    }
}

#endif
