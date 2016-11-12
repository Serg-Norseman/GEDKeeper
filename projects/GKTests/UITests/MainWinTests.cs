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
using GKUI;
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
            // call to AboutDlg, closing in AboutDlg_Handler
            ExpectModal("AboutDlg", "AboutDlg_Handler");
            ClickToolStripMenuItem("miAbout", fMainWin);

            // create an empty base
            ClickToolStripButton("tbFileNew", fMainWin);

            // call to StatsWin (required the base)
            ClickToolStripButton("tbStats", fMainWin);

            // call to SlideshowWin (required the base)
            ClickToolStripMenuItem("miSlideshow", fMainWin);
        }

        public void AboutDlg_Handler()
        {
            ClickButton("btnClose", "AboutDlg");
        }

        public void TestFormNoDataHandler()
        {
            var messageBoxTester = new MessageBoxTester("Message");
            if (messageBoxTester != null)
            {
                messageBoxTester.ClickOk();
            }
        }

        /*[Test]
        public void Test_Misc()
        {
            Assert.AreEqual(fBase, _frm.Base);
            Assert.AreEqual(fNoteRecord, _frm.NoteRecord);
        }

        public void DlgHandler()
        {
            //var btnCancel = new ButtonTester("btnCancel", "NoteEditDlg");
            //btnCancel.Click();
        }

        [Test]
        public void Test_btnCancel()
        {
            var btnCancel = new ButtonTester("btnCancel");
            btnCancel.Click();
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            var txtNote = new TextBoxTester("txtNote");
            txtNote.Enter("sample text");
            Assert.AreEqual("sample text", txtNote.Text);

            var btnAccept = new ButtonTester("btnAccept");
            btnAccept.Click();

            Assert.AreEqual("sample text\r\n", fNoteRecord.Note.Text);
        }*/

        /*[Test]
        public void TestData()
        {
            // CheckBoxTester uncheckBoxTester = new CheckBoxTester("aPanelName.checkBoxName", "MyFormName");
            // RadioButtonTester radioTester = new RadioButtonTester("mainFormControlName.panelName.radioButtonName",  "MyFormName");

            var txtInput = new TextBoxTester("txtInput") {["Text"] = "2+2"};
            var txtOutput = new TextBoxTester("txtOutput");
            Assert.AreEqual("2+2", txtInput.Text);
            
            var btnRes = new ButtonTester("btnRes");
            btnRes.Click();
            Assert.AreEqual("4", txtOutput.Text);
        }*/
        
        /*[Test]
        public void TestNoData()
        {
            ExpectModal("Message", new ModalFormActivated(TestFormNoDataHandler));
            var nameTextbox = new TextBoxTester("txtName");
            nameTextbox["Text"] = string.Empty;
            Assert.AreEqual(string.Empty, nameTextbox.Text);
            var okButton = new ButtonTester("btnOK");
            okButton.Click();
            Assert.IsFalse(form.DialogResult == DialogResult.OK);
        }

        [Test]
        public void TestData()
        {
            var nameTextbox = new TextBoxTester("txtName");
            nameTextbox["Text"] = "abcdefg";
            Assert.AreEqual("abcdefg", nameTextbox.Text);
            var okButton = new ButtonTester("btnOK");
            okButton.Click();
            Assert.IsTrue(form.DialogResult == DialogResult.OK);
        }

        public void TestFormNoDataHandler()
        {
            var messageBoxTester = new MessageBoxTester("Message");
            if (messageBoxTester != null)
            {
                messageBoxTester.ClickOk();
            }
        }*/
    }
}

#endif
