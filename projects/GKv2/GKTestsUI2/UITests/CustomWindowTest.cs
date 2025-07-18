﻿/*
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
            RecordSelectDlgTests.SetCreateItemHandler(this, NoteEditDlgTests.NoteAdd_Mini_Handler);
            ClickToolStripButton("fNotesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.Notes.Count);

            SelectSheetListItem("fNotesList", dlg, 0);
            ModalFormHandler = NoteEditDlgTests.NoteAdd_Mini_Handler;
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
            ModalFormHandler = SourceCitEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, record.SourceCitations.Count);

            SelectSheetListItem("fSourcesList", dlg, 0);
            ModalFormHandler = SourceCitEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fSourcesList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, record.SourceCitations.Count);

            ModalFormHandler = MessageBox_YesHandler;
            SelectSheetListItem("fSourcesList", dlg, 0);
            ClickToolStripButton("fSourcesList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, record.SourceCitations.Count);
        }
    }
}

#endif
