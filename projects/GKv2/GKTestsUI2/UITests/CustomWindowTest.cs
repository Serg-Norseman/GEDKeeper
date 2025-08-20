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

#if !DIS_NUF

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Search;
using GKTests.ControlTesters;
using GKUI.Forms;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class CustomWindowTest : NUnitFormTest
    {
        public override bool UseHidden
        {
            get { return true; }
        }

        public static Form GetActiveForm(string formName)
        {
            var tester = new FormTester(formName);
            return (tester == null) ? null : (Form)tester.TheObject;
        }

        #region Control Actions

        public static void ClickButton(string name, Form form)
        {
            var tsBtn = new ButtonTester(name, form);
            if (tsBtn.Count > 1) {
                // FIXME: Find out why sometimes the search returns
                // two components where there is only one (MediaViewerWinTests)
                tsBtn[0].FireEvent("Click");
            } else {
                tsBtn.FireEvent("Click");
            }
        }

        public static void ClickToolStripButton(string name, Form form)
        {
            var tsBtn = new ToolStripButtonTester(name, form);
            if (tsBtn.Count > 1) {
                // FIXME: Find out why sometimes the search returns
                // two components where there is only one (MediaViewerWinTests)
                tsBtn[0].FireEvent("Click");
            } else {
                tsBtn.FireEvent("Click");
            }
        }

        public static void ClickToolStripMenuItem(string name, Form form)
        {
            var tsMenuItem = new ToolStripMenuItemTester(name, form);
            tsMenuItem.Click();
        }

        public static void ClickRadioButton(string name, Form form)
        {
            var radBtn = new RadioButtonTester(name, form);
            radBtn.Click();
        }

        public static void SelectTab(string name, Form form, int value)
        {
            var tabCtl = new TabControlTester(name, form);
            tabCtl.SelectTab(value);
        }

        public static void SelectCombo(string name, Form form, int value)
        {
            var combo = new ComboBoxTester(name, form);
            combo.Select(value);
        }

        public static void EnterCombo(string name, Form form, string value)
        {
            var combo = new ComboBoxTester(name, form);
            combo.Enter(value);
        }

        public static void EnterText(string name, Form form, string value)
        {
            var textBox = new TextBoxTester(name, form);
            textBox.Enter(value);
        }

        public static void EnterRichText(string name, Form form, string value)
        {
            var textBox = new RichTextBoxTester(name, form);
            textBox.Enter(value);
        }

        public static void EnterMaskedText(string name, Form form, string value)
        {
            var textBox = new MaskedTextBoxTester(name, form);
            textBox.Enter(value);
        }

        public static void SelectSheetListItem(string name, Form form, int value)
        {
            var sheetTester = new GKSheetListTester(name, form);
            sheetTester.Properties.ListView.SelectItem(value);
        }

        public static void CheckBox(string name, Form form, bool value)
        {
            var chk = new CheckBoxTester(name, form);
            chk.Properties.Checked = value;
        }

        public static void CheckRadioButton(string name, Form form, bool value)
        {
            var radBtn = new RadioButtonTester(name, form);
            radBtn.Properties.Checked = value;
        }

        public static void KeyDownForm(string formName, Keys keyData)
        {
            var formTester = new FormTester(formName);
            formTester.FireEvent("KeyDown", new KeyEventArgs(keyData));
        }

        #endregion

        #region Dialogs Handlers

        public static void MessageBox_YesHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.Yes);
        }

        public static void MessageBox_NoHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.No);
        }

        public static void MessageBox_OkHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.OK);
        }

        public static void MessageBox_CancelHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.Cancel);
        }

        public static void PrintDialog_Handler(string name, IntPtr ptr, Form form)
        {
            form.Close();
        }

        public static void OpenFile_Cancel_Handler(string name, IntPtr hWnd, Form form)
        {
            var openDlg = new OpenFileDialogTester(hWnd);
            openDlg.ClickCancel();
        }

        public static void SaveFile_Cancel_Handler(string name, IntPtr hWnd, Form form)
        {
            var saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.ClickCancel();
        }

        public static void PrepareFileSave(string fileName, IntPtr hWnd)
        {
            fileName = TestUtils.GetTempFilePath(fileName);

            var saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.SaveFile(fileName);
            saveDlg.SaveFile();
        }

        public static void Dialog_Cancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        #endregion

        #region FileSave Handlers

        public static void SaveFileGED_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.ged", hWnd);
        }

        public static void SaveFileJPG_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.jpg", hWnd);
        }

        public static void SaveFileEMF_Handler(string name, IntPtr hWnd, Form form)
        {
            PrepareFileSave("test.emf", hWnd);
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

        #endregion

        protected static NUnitFormTest fFormTest;

        public static void SetModalFormHandler(NUnitFormTest formTest, ModalFormHandler modalFormHandler)
        {
            fFormTest = formTest;
            fFormTest.ModalFormHandler = modalFormHandler;
        }

        protected void NotesSheet_Handler(GDMRecordWithEvents record, Form dlg)
        {
            Assert.AreEqual(0, record.Notes.Count);
            CustomWindowTest.SetCreateItemHandler(this, CustomWindowTest.NoteAdd_Mini_Handler);
            ClickToolStripButton("fNotesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.Notes.Count);

            SelectSheetListItem("fNotesList", dlg, 0);
            ModalFormHandler = CustomWindowTest.NoteAdd_Mini_Handler;
            ClickToolStripButton("fNotesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, record.Notes.Count);

            SelectSheetListItem("fNotesList", dlg, 0);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fNotesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, record.Notes.Count);
        }

        protected void MediaSheet_Handler(GDMRecordWithEvents record, Form dlg)
        {
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
        }

        protected void SourceCitSheet_Handler(GDMRecordWithEvents record, Form dlg)
        {
            Assert.AreEqual(0, record.SourceCitations.Count);
            ModalFormHandler = CustomWindowTest.SourceCitEditDlg_AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.SourceCitations.Count);

            SelectSheetListItem("fSourcesList", dlg, 0);
            ModalFormHandler = CustomWindowTest.SourceCitEditDlg_AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, record.SourceCitations.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fSourcesList", dlg, 0);
            ClickToolStripButton("fSourcesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, record.SourceCitations.Count);
        }

        #region Temporary handlers for transfer UI tests from NUnitForms to controller's tests

        public static void GroupAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("edName", form, "sample group");

            ClickButton("btnAccept", form);
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

        public static void NoteAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterRichText("txtNote", form, "sample text");
            //Assert.AreEqual("sample text", txtNote.Text);

            ClickButton("btnAccept", form);
        }

        public static void SexCheckDlgTests_AcceptM_Handler(string name, IntPtr ptr, Form form)
        {
            CheckRadioButton("rbMale", form, true);
            ClickButton("btnAccept", form);
        }

        public static void SexCheckDlgTests_AcceptF_Handler(string name, IntPtr ptr, Form form)
        {
            CheckRadioButton("rbFemale", form, true);
            ClickButton("btnAccept", form);
        }

        public static void LanguageSelectDlg_Accept_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        public static void LocationAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "sample location");

            ClickButton("btnAccept", form);
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

        public static void RelationshipCalculatorDlg_Handler(string name, IntPtr ptr, Form form)
        {
            RelationshipCalculatorDlg dlg = (RelationshipCalculatorDlg)form;
            var baseContext = dlg.Base.Context;

            Assert.IsTrue(baseContext.Tree.RecordsCount > 1);

            GDMIndividualRecord iRec1 = baseContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(baseContext.Tree, iRec1, false));
            GDMIndividualRecord iRec2 = baseContext.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            Assert.IsNotNull(iRec2);
            Assert.AreEqual("Ivanova Maria Petrovna", GKUtils.GetRecordName(baseContext.Tree, iRec2, false));

            AppHost.TEST_MODE = true; // FIXME: dirty hack

            CustomWindowTest.SetSelectItemHandler(0);
            ClickButton("btnRec1Select", form);
            CustomWindowTest.SetSelectItemHandler(1);
            ClickButton("btnRec2Select", form);

            var txtResult = new TextBoxTester("txtResult", form);
            // default is not Russian culture
            Assert.AreEqual("Ivanov Ivan Ivanovich is husband of Ivanova Maria Petrovna", txtResult.Text); // :D

            ClickButton("btnClose", form);
        }

        public static void SourceCitEditDlg_AcceptModalHandler(string name, IntPtr ptr, Form form)
        {
            SelectCombo("cmbSource", form, 0);
            ClickButton("btnAccept", form);
        }

        public static void PersonalNameEditAdd_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtSurname", form, "sample surname");

            ClickButton("btnAccept", form);
        }

        public static void PersonalNameEditEdit_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtSurname", form, "sample surname2");

            ClickButton("btnAccept", form);
        }

        public static void TaskAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //EnterText("edName", form, "sample group");

            ClickButton("btnAccept", form);
        }

        private static int RSD_ItemIndex;

        private static void RSD_SelectItem_Handler(string name, IntPtr ptr, Form form)
        {
            EnterCombo("txtFastFilter", form, "*");
            //dlg.SetTarget(TargetMode.tmNone, null, GDMSex.svUnknown);

            var listRecords = new GKRecordsViewTester("fListRecords", form);
            listRecords.Properties.SelectItem(RSD_ItemIndex);

            ClickButton("btnSelect", form);
        }

        public static void SetSelectItemHandler(int itemIndex)
        {
            RSD_ItemIndex = itemIndex;
            SetModalFormHandler(fFormTest, RSD_SelectItem_Handler);
        }

        private static ModalFormHandler RSD_SubHandler;

        private static void RSD_CreateItem_Handler(string name, IntPtr ptr, Form form)
        {
            SetModalFormHandler(fFormTest, RSD_SubHandler);
            ClickButton("btnCreate", form);
        }

        public static void SetCreateItemHandler(NUnitFormTest formTest, ModalFormHandler createHandler)
        {
            RSD_SubHandler = createHandler;
            SetModalFormHandler(formTest, RSD_CreateItem_Handler);
        }

        public static void TreeFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        public static void CommonFilterDlg_btnReset_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnReset", form);
            ClickButton("btnAccept", form);
        }

        public static void CommonFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            CommonFilterDlg cfDlg = ((CommonFilterDlg)form);
            Assert.IsNotNull(cfDlg.Base);

            IRecordsListModel listMan = cfDlg.ListMan;

            SelectTab("tabsFilters", form, 0);

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

            // Fail: AmbiguousMatch?!
            //dataGridView1.FireEvent("Scroll", new ScrollEventArgs(ScrollEventType.SmallIncrement, 1));

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

        #endregion
    }
}

#endif
