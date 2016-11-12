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
    /// 
    /// </summary>
    [TestFixture]
    public class MainWinTests : CustomWindowTest
    {
        public MainWinTests()
        {
        }

        //private IBaseContext fContext;
        //private GEDCOMNoteRecord fNoteRecord;
        //private IBaseWindow fBase;

        private MainWin _frm;

        public override void Setup()
        {
            base.Setup();

            //fBase = new BaseWindowMock();
            //fContext = fBase.Context;
            //fNoteRecord = new GEDCOMNoteRecord(fContext.Tree, fContext.Tree, "", "");

            //ExpectModal("NoteEditDlg", "DlgHandler");
            _frm = new MainWin();
            //_frm.NoteRecord = fNoteRecord;
            //_frm.ShowDialog();
            _frm.Show();
        }

        [STAThread]
        [Test]
        public void Test_Common()
        {
            // call to AboutDlg, closing in AboutDlg_Handler
            ExpectModal("AboutDlg", "AboutDlg_Handler");
            ClickToolStripMenuItem("miAbout", _frm);

            // create an empty base
            ClickToolStripButton("tbFileNew", _frm);

            // call to StatsWin (required the base)
            ClickToolStripButton("tbStats", _frm);

            // call to SlideshowWin (required the base)
            ClickToolStripMenuItem("miSlideshow", _frm);
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
        public void Test_EnterTextAndAccept()
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
