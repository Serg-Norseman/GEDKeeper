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

#if !MONO && !DIS_NUF

using System;
using System.Reflection;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.ControlTesters;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Framework;

namespace GKFlowInputPlugin
{
    public class FITestLangMan : ILangMan
    {
        private static readonly string[] LSDefList = new string[] {
            "",
            "Stream input...",
            "Male",
            "Female",
            "Surname",
            "Name",
            "Patronymic",
            "Close",
            "Append",
            "Note",
            "Source",
            "Page",
            "Birth date",
            "Death date",
            "Father",
            "Mother",
            "Spouse",
            "Full name",
            "Birth place",
            "Death place",
            "Age",
            "Birth",
            "Death",
            "?",
            "Simple input",
            "Source input",
            "Source type",
            "Revision book",
            "Parish register",
            "Year",
            "Settlement",
            "Event date",
            "Event type",
            "Link",
            "Comment",
            "Person",
            "Godparent",
            "Child",
            "Number of name components is less than three.",
            "Basic person (\"Person\") is not defined by the first",
            "Year of the source is incorrect",
            "Marriage",
        };

        public string LS(Enum lsid)
        {
            int idx = ((IConvertible)lsid).ToInt32(null);
            return LSDefList[idx];
        }

        public bool LoadFromFile(string fileName, Assembly resAssembly)
        {
            return true;
        }
    }

    [TestFixture]
    public class FlowInputTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private FlowInputDlg fDialog;
        private ILangMan fLangMan;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub(false);

            fLangMan = new FITestLangMan();
            var plugin = new TestPlugin(fLangMan);
            fDialog = new FlowInputDlg(plugin, fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Close()
        {
            ClickButton("btnClose", fDialog);
        }

        [Test]
        public void Test_SimpleInput()
        {
            SelectTab("PageControl1", fDialog, 0);

            EnterMaskedText("EditBirthDate", fDialog, "01/01/1890");
            EnterMaskedText("EditDeathDate", fDialog, "01/01/1890");

            ClickButton("btnMale", fDialog);
            ClickButton("btnMale", fDialog);

            ModalFormHandler = MessageBox_OkHandler; // error
            ClickButton("btnParse", fDialog);


            EnterText("EditName", fDialog, "Иванов Иван Иванович");
            EnterText("MemoNote", fDialog, "note text");

            ClickButton("btnParse", fDialog);

            Assert.AreEqual(2, fBase.Context.Tree.RecordsCount); // indiRec + noteRec

            GDMIndividualRecord iRec1 = fBase.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Иван Иванович Иванов", iRec1.GetPrimaryFullName());
        }

        [Test]
        public void Test_SourceInput_Error()
        {
            SelectTab("PageControl1", fDialog, 1);

            ClickRadioButton("rbSK_Met", fDialog); // MK
            SelectCombo("cbEventType", fDialog, 0); // birth event

            ModalFormHandler = MessageBox_OkHandler; // error
            ClickButton("btnParse", fDialog);

            Assert.AreEqual(0, fBase.Context.Tree.RecordsCount);
        }

        [Test]
        public void Test_SourceInput_MK_Birth()
        {
            SelectTab("PageControl1", fDialog, 1);

            ClickRadioButton("rbSK_Met", fDialog); // MK
            SelectCombo("cbEventType", fDialog, 0); // birth event

            var dataGridView1 = new DataGridViewTester("dataGridView1", fDialog);
            dataGridView1.EnterCell(0, 0, fLangMan.LS(PLS.PLPerson));
            dataGridView1.EnterCell(0, 1, "Иван");
            dataGridView1.EnterCell(0, 2, "Иванович");
            dataGridView1.EnterCell(0, 3, "Иванов");
            dataGridView1.EnterCell(0, 4, "25");
            dataGridView1.EnterCell(0, 5, "test comment");

            EnterCombo("cbSource", fDialog, "test source");
            EnterText("edSourceYear", fDialog, "1890");
            EnterText("edPage", fDialog, "12");
            EnterText("edPlace", fDialog, "Сосновка");

            ModalFormHandler = SexCheckDlgTests.SexCheckDlgTests_AcceptM_Handler; // NamesTable not available

            ClickButton("btnParse", fDialog);

            Assert.AreEqual(3, fBase.Context.Tree.RecordsCount); // newSourceRec + newIndiRec + newNoteRec

            GDMIndividualRecord iRec1 = fBase.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Иван Иванович Иванов", iRec1.GetPrimaryFullName());
        }

        [Test]
        public void Test_SourceInput_MK_Death()
        {
            SelectTab("PageControl1", fDialog, 1);

            ClickRadioButton("rbSK_Met", fDialog); // MK
            SelectCombo("cbEventType", fDialog, 1); // death event

            var dataGridView1 = new DataGridViewTester("dataGridView1", fDialog);
            dataGridView1.EnterCell(0, 0, fLangMan.LS(PLS.PLPerson));
            dataGridView1.EnterCell(0, 1, "Анна");
            dataGridView1.EnterCell(0, 2, "Васильевна");
            dataGridView1.EnterCell(0, 3, "Иванова");
            dataGridView1.EnterCell(0, 4, "90");
            dataGridView1.EnterCell(0, 5, "test comment");

            EnterCombo("cbSource", fDialog, "test source");
            EnterText("edSourceYear", fDialog, "1890");
            EnterText("edPage", fDialog, "12");
            EnterText("edPlace", fDialog, "Сосновка");

            ModalFormHandler = SexCheckDlgTests.SexCheckDlgTests_AcceptF_Handler; // NamesTable not available

            ClickButton("btnParse", fDialog);

            Assert.AreEqual(3, fBase.Context.Tree.RecordsCount); // newSourceRec + newIndiRec + newNoteRec

            GDMIndividualRecord iRec1 = fBase.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Анна Васильевна Иванова", iRec1.GetPrimaryFullName());
        }

        [Test]
        public void Test_SourceInput_MK_Marr()
        {
            SelectTab("PageControl1", fDialog, 1);

            ClickRadioButton("rbSK_Met", fDialog); // MK
            SelectCombo("cbEventType", fDialog, 2); // marr event

            var dataGridView1 = new DataGridViewTester("dataGridView1", fDialog);
            dataGridView1.Properties.Rows.Add(2);
            dataGridView1.EnterCell(0, 0, fLangMan.LS(PLS.PLPerson));
            dataGridView1.EnterCell(0, 1, "Иван");
            dataGridView1.EnterCell(0, 2, "Иванович");
            dataGridView1.EnterCell(0, 3, "Иванов");
            dataGridView1.EnterCell(0, 4, "20");
            dataGridView1.EnterCell(0, 5, "test comment");
            dataGridView1.EnterCell(1, 0, fLangMan.LS(PLS.Spouse));
            dataGridView1.EnterCell(1, 1, "Анна");
            dataGridView1.EnterCell(1, 2, "Васильевна");
            dataGridView1.EnterCell(1, 3, "Иванова");
            dataGridView1.EnterCell(1, 4, "19");
            dataGridView1.EnterCell(1, 5, "test comment2");

            EnterCombo("cbSource", fDialog, "test source");
            EnterText("edSourceYear", fDialog, "1890");
            EnterText("edPage", fDialog, "12");
            EnterText("edPlace", fDialog, "Сосновка");

            ClickButton("btnParse", fDialog);

            Assert.AreEqual(6, fBase.Context.Tree.RecordsCount); // source + indi1 + note1 + family + indi2 + note2

            GDMIndividualRecord iRec1 = fBase.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Иван Иванович Иванов", iRec1.GetPrimaryFullName());

            GDMIndividualRecord iRec2 = fBase.Context.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            Assert.IsNotNull(iRec2);
            Assert.AreEqual("Анна Васильевна Иванова", iRec2.GetPrimaryFullName());
        }

        [Test]
        public void Test_SourceInput_RS()
        {
            SelectTab("PageControl1", fDialog, 1);

            ClickRadioButton("rbSK_Rev", fDialog); // RS

            var dataGridView1 = new DataGridViewTester("dataGridView1", fDialog);
            dataGridView1.Properties.Rows.Add(5);
            dataGridView1.EnterCell(0, 0, fLangMan.LS(PLS.PLPerson));
            dataGridView1.EnterCell(0, 1, "Иван");
            dataGridView1.EnterCell(0, 2, "Иванович");
            dataGridView1.EnterCell(0, 3, "Иванов");
            dataGridView1.EnterCell(0, 4, "20");
            dataGridView1.EnterCell(0, 5, "test comment");
            dataGridView1.EnterCell(1, 0, fLangMan.LS(PLS.Spouse));
            dataGridView1.EnterCell(1, 1, "Анна");
            dataGridView1.EnterCell(1, 2, "Васильевна");
            dataGridView1.EnterCell(1, 3, "Иванова");
            dataGridView1.EnterCell(1, 4, "19");
            dataGridView1.EnterCell(1, 5, "test comment2");
            dataGridView1.EnterCell(2, 0, fLangMan.LS(PLS.Father));
            dataGridView1.EnterCell(2, 1, "Иван");
            dataGridView1.EnterCell(2, 2, "Петрович");
            dataGridView1.EnterCell(2, 3, "Иванов");
            dataGridView1.EnterCell(2, 4, "40");
            dataGridView1.EnterCell(2, 5, "");
            dataGridView1.EnterCell(3, 0, fLangMan.LS(PLS.Child));
            dataGridView1.EnterCell(3, 1, "Василий");
            dataGridView1.EnterCell(3, 2, "Иванович");
            dataGridView1.EnterCell(3, 3, "Иванов");
            dataGridView1.EnterCell(3, 4, "1");
            dataGridView1.EnterCell(3, 5, "");
            dataGridView1.EnterCell(4, 0, fLangMan.LS(PLS.PLGodparent));
            dataGridView1.EnterCell(4, 1, "Иван");
            dataGridView1.EnterCell(4, 2, "Григорьевич");
            dataGridView1.EnterCell(4, 3, "Иванов");
            dataGridView1.EnterCell(4, 4, "60");
            dataGridView1.EnterCell(4, 5, "");

            EnterCombo("cbSource", fDialog, "test source");
            EnterText("edSourceYear", fDialog, "1890");
            EnterText("edPage", fDialog, "12");
            EnterText("edPlace", fDialog, "Сосновка");

            ModalFormHandler = SexCheckDlgTests.SexCheckDlgTests_AcceptM_Handler; // NamesTable not available
            ClickButton("btnParse", fDialog);

            Assert.AreEqual(10, fBase.Context.Tree.RecordsCount);

            GDMIndividualRecord iRec1 = fBase.Context.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Иван Иванович Иванов", iRec1.GetPrimaryFullName());

            GDMIndividualRecord iRec2 = fBase.Context.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            Assert.IsNotNull(iRec2);
            Assert.AreEqual("Анна Васильевна Иванова", iRec2.GetPrimaryFullName());

            GDMIndividualRecord iRec3 = fBase.Context.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec3);
            Assert.AreEqual("Иван Петрович Иванов", iRec3.GetPrimaryFullName());

            GDMIndividualRecord iRec4 = fBase.Context.Tree.XRefIndex_Find("I4") as GDMIndividualRecord;
            Assert.IsNotNull(iRec4);
            Assert.AreEqual("Василий Иванович Иванов", iRec4.GetPrimaryFullName());
        }
    }
}

#endif
