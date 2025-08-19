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
using System.Windows.Forms;
using GDModel;
using GKCore;
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

        public static void ClickButton(string name, string form)
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

        public static void EnterNumeric(string name, Form form, int value)
        {
            var nud = new NumericUpDownTester(name, form);
            nud.EnterValue(value);
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

        private static string fOpenedFileName;

        public static void SetOpenedFile(NUnitFormTest formTest, string fileName)
        {
            fOpenedFileName = fileName;
            formTest.ModalFormHandler = OpenFile_Accept_Handler;
        }

        public static void OpenFile_Accept_Handler(string name, IntPtr hWnd, Form form)
        {
            var openDlg = new OpenFileDialogTester(hWnd);
            openDlg.OpenFile(fOpenedFileName);
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

        #region InputBox Handlers

        public static void InputBox_Add_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtValue", form, "sample add");
            ClickButton("btnAccept", form);
        }

        public static void InputBox_Edit_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtValue", form, "sample edit");
            ClickButton("btnAccept", form);
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
            RecordSelectDlgTests.SetCreateItemHandler(this, CustomWindowTest.NoteAdd_Mini_Handler);
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
            try {
                Assert.AreEqual(0, record.MultimediaLinks.Count);
                RecordSelectDlgTests.SetCreateItemHandler(this, MediaEditDlgTests.MultimediaRecord_Add_Handler);
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
            }
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

        public static void LanguageEditDlg_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        public static void FilePropertiesDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            FilePropertiesDlg dlg = (FilePropertiesDlg)form;
            var baseContext = dlg.Base.Context;

            EnterText("txtName", form, "sample text");

            SetModalFormHandler(fFormTest, CustomWindowTest.LanguageEditDlg_Handler);
            ClickButton("btnLangEdit", form);

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

        public static void AboutDlg_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnClose", form);
        }

        public static void CloseModalHandler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnClose", form);
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

            RecordSelectDlgTests.SetSelectItemHandler(0);
            ClickButton("btnRec1Select", form);
            RecordSelectDlgTests.SetSelectItemHandler(1);
            ClickButton("btnRec2Select", form);

            var txtResult = new TextBoxTester("txtResult", form);
            // default is not Russian culture
            Assert.AreEqual("Ivanov Ivan Ivanovich is husband of Ivanova Maria Petrovna", txtResult.Text); // :D

            ClickButton("btnClose", form);
        }

        public static void SourceAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtShortTitle", form, "sample text");

            ClickButton("btnAccept", form);
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

        #endregion
    }
}

#endif
