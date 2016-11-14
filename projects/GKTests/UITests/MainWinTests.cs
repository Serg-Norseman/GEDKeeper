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
using System.Threading;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKUI;
using GKUI.Controls;
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
        private IBaseWindow fCurWin;

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
            Wait();
            ExpectModal("AboutDlg", "AboutDlg_Handler");
            ClickToolStripMenuItem("miAbout", fMainWin);


            // Stage 2.1: GetCurrentFile()
            Assert.IsNull(fMainWin.GetCurrentFile(), "Stage 2.1");

            // Stage 2.2: create an empty base
            Wait();
            ClickToolStripButton("tbFileNew", fMainWin);

            // Stage 2.3: GetCurrentFile()
            fCurWin = fMainWin.GetCurrentFile();
            Assert.IsNotNull(fCurWin, "Stage 2.3");

            // Stage 2.4: fill context for sample data
            TestStubs.FillContext(fCurWin.Context);
            fCurWin.UpdateView();

            // Stage 2.5: select first individual record in base
            Wait();
            fCurWin.SelectRecordByXRef("I1");
            Assert.AreEqual("I1", ((BaseWin) fCurWin).GetSelectedPerson().XRef);

            // Stage 3: call to FilePropertiesDlg
            Wait();
            ModalFormHandler = FilePropertiesDlg_btnCancel_Handler; // FilePropertiesDlg.Cancel
            ClickToolStripMenuItem("miFileProperties", fMainWin);
            Wait();
            ModalFormHandler = FilePropertiesDlg_btnAccept_Handler; // FilePropertiesDlg.Accept
            ClickToolStripMenuItem("miFileProperties", fMainWin);


            // Stage 4: call to OptionsDlg
            Wait();
            ModalFormHandler = OptionsDlg_btnCancel_Handler; // OptionsDlg.Cancel
            ClickToolStripMenuItem("miOptions", fMainWin);
            Wait();
            ModalFormHandler = OptionsDlg_btnAccept_Handler; // OptionsDlg.Accept
            ClickToolStripMenuItem("miOptions", fMainWin);


            // Stage 5: calls to the different Editors
            Wait();
            for (GEDCOMRecordType rt = GEDCOMRecordType.rtIndividual; rt <= GEDCOMRecordType.rtLocation; rt++) {
                fCurWin.ShowRecordsTab(rt);

                Wait();
                ModalFormHandler = EditorDlg_btnCancel_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                Wait();
                ModalFormHandler = EditorDlg_btnAccept_Handler;
                ClickToolStripButton("tbRecordAdd", fMainWin);

                Wait();
                ModalFormHandler = EditorDlg_btnCancel_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);

                Wait();
                ModalFormHandler = EditorDlg_btnAccept_Handler;
                ClickToolStripButton("tbRecordEdit", fMainWin);
            }


            // Stage 23: call to PersonsFilterDlg
            Wait();
            ModalFormHandler = PersonsFilterDlg_btnCancel_Handler; // PersonsFilterDlg.Cancel
            ClickToolStripMenuItem("miFilter", fMainWin);
            Wait();
            ModalFormHandler = PersonsFilterDlg_btnAccept_Handler; // PersonsFilterDlg.Accept
            ClickToolStripMenuItem("miFilter", fMainWin);


            // Stage 24: call to TreeToolsWin
            Wait();
            ModalFormHandler = TreeToolsWin_Handler;
            ClickToolStripMenuItem("miTreeTools", fMainWin);


            // Stage 25: call to CircleChartWin (required the base, selected person)
            Wait();
            ClickToolStripMenuItem("miAncestorsCircle", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is CircleChartWin, "Stage 25");
            fMainWin.ActiveMdiChild.Close();

            // Stage 26: call to CircleChartWin (required the base, selected person)
            Wait();
            ClickToolStripMenuItem("miDescendantsCircle", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is CircleChartWin, "Stage 26");
            fMainWin.ActiveMdiChild.Close();


            // Stage 27: call to TreeChartWin (required the base, selected person)
            Wait();
            ClickToolStripMenuItem("miTreeAncestors", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is TreeChartWin, "Stage 27");
            fMainWin.ActiveMdiChild.Close();


            // Stage 28: call to TreeChartWin (required the base, selected person)
            Wait();
            ClickToolStripMenuItem("miTreeDescendants", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is TreeChartWin, "Stage 28");
            fMainWin.ActiveMdiChild.Close();


            // Stage 29: call to TreeChartWin (required the base, selected person)
            Wait();
            ClickToolStripMenuItem("miTreeBoth", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is TreeChartWin, "Stage 29");
            fMainWin.ActiveMdiChild.Close();


            // Stage 30: call to StatsWin (required the base)
            Wait();
            ClickToolStripButton("tbStats", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is StatisticsWin, "Stage 30");
            fMainWin.ActiveMdiChild.Close();


            // Stage 31: call to SlideshowWin (required the base)
            Wait();
            ClickToolStripMenuItem("miSlideshow", fMainWin);
            Assert.IsTrue(fMainWin.ActiveMdiChild is SlideshowWin, "Stage 31");
            fMainWin.ActiveMdiChild.Close();


            // Stage 32: call to ScriptEditWin (required the base)
            Wait();
            ModalFormHandler = ScriptEditWin_Handler;
            ClickToolStripMenuItem("miScripts", fMainWin);
            //Assert.IsTrue(fMainWin.ActiveMdiChild is SlideshowWin, "Stage 32");
            //fMainWin.ActiveMdiChild.Close();


            // Stage 33: call to OrganizerWin
            Wait();
            ModalFormHandler = OrganizerWin_Handler;
            ClickToolStripMenuItem("miOrganizer", fMainWin);


            // Stage 34: call to RelationshipCalculatorDlg
            Wait();
            ModalFormHandler = RelationshipCalculatorDlg_Handler;
            ClickToolStripMenuItem("miRelationshipCalculator", fMainWin);


            // Stage 50: close Base
            Wait();
            ClickToolStripMenuItem("miFileClose", fMainWin);
        }

        #region AboutDlg handler

        public void AboutDlg_Handler()
        {
            ClickButton("btnClose", "AboutDlg");
        }

        #endregion

        #region FilePropertiesDlg handlers

        public void FilePropertiesDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            Assert.AreEqual(fCurWin, ((IBaseEditor)form).Base);

            ClickButton("btnCancel", form);
        }

        public void FilePropertiesDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);

            ClickButton("btnAccept", form);

            GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region OptionsDlg handlers

        public void OptionsDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            //Assert.AreEqual(fCurWin, ((IBaseEditor)form).Base);

            ClickButton("btnCancel", form);
        }

        public void OptionsDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            /*var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);*/

            ClickButton("btnAccept", form);

            //GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            //Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region PersonsFilterDlg handlers

        public void PersonsFilterDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            //Assert.AreEqual(fCurWin, ((IBaseEditor)form).Base);

            ClickButton("btnCancel", form);
        }

        public void PersonsFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            /*var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);*/

            ClickButton("btnAccept", form);

            //GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            //Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region ScriptEditWin handlers

        public void ScriptEditWin_Handler(string name, IntPtr ptr, Form form)
        {
            form.Close();
        }

        #endregion

        #region TreeToolsWin handlers

        public void TreeToolsWin_Handler(string name, IntPtr ptr, Form form)
        {
            form.Close();
        }

        #endregion

        #region OrganizerWin handlers

        public void OrganizerWin_Handler(string name, IntPtr ptr, Form form)
        {
            form.Close();
        }

        #endregion

        #region RelationshipCalculatorDlg handlers

        public void RelationshipCalculatorDlg_Handler(string name, IntPtr ptr, Form form)
        {
            form.Close();
        }

        #endregion

        #region EditorDlg handlers

        public void EditorDlg_btnCancel_Handler(string name, IntPtr ptr, Form form)
        {
            //Assert.AreEqual(fCurWin, ((IBaseEditor)form).Base);

            ClickButton("btnCancel", form);
        }

        public void EditorDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            /*var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);*/

            ClickButton("btnAccept", form);

            //GEDCOMSubmitterRecord submitter = fCurWin.Context.Tree.Header.Submitter.Value as GEDCOMSubmitterRecord;
            //Assert.AreEqual("sample text", submitter.Name.StringValue);
        }

        #endregion

        #region Service

        private void Wait()
        {
            #if !CI_MODE
            Application.DoEvents();
            Thread.Sleep(1000);
            #endif
        }

        #endregion

        /*[Test]
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
