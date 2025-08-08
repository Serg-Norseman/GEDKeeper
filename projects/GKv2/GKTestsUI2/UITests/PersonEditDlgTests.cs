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
using System.Threading;
using System.Windows.Forms;
using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKTests;
using GKTests.Stubs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class PersonEditDlgTests : CustomWindowTest
    {
        private GDMIndividualRecord fIndividualRecord;
        private IBaseWindow fBase;
        private PersonEditDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fIndividualRecord = fBase.Context.CreatePersonEx("Ivan", "", "Smith", GDMSex.svMale, true);

            fDialog = new PersonEditDlg(fBase);
            fDialog.IndividualRecord = fIndividualRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fIndividualRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fIndividualRecord, fDialog.IndividualRecord);
            Assert.AreEqual(null, fDialog.Target);
            Assert.AreEqual(TargetMode.tmNone, fDialog.TargetMode);

            //EnterText("txtSurname", "sample text");

            // empty individual parents, no effects
            ClickButton("btnFatherSel", fDialog);
            ClickButton("btnMotherSel", fDialog);

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fIndividualRecord.PersonalNames[0].Pieces.Surname);
        }

        private void TestDeleteSheetListItem(string sheetListName, int itemIndex)
        {
            SelectSheetListItem(sheetListName, fDialog, itemIndex);
            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton(sheetListName + "_ToolBar_btnDelete", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Common()
        {
            GDMIndividualRecord indiRecord = fDialog.IndividualRecord;

            SelectCombo("cmbSex", fDialog, 1); // male

            var tabs = new TabControlTester("tabsData", fDialog);
            var tabsOther = new TabControlTester("tabsOther", fDialog);

            var cmbRestriction = new ComboBoxTester("cmbRestriction", fDialog);
            cmbRestriction.Select(3);
            cmbRestriction.Select(2);
            cmbRestriction.Select(1);
            cmbRestriction.Select(0);

            var txtSurname = new TextBoxTester("txtSurname", fDialog);
            txtSurname.FireEvent("KeyDown", new KeyEventArgs(Keys.Down | Keys.Control));

            // parents
            /*RecordSelectDlgTests.SetCreateItemHandler(this, FamilyEditDlgTests.FamilyAdd_Mini_Handler);
            ClickButton("btnParentsAdd", fDialog);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnParentsDelete", fDialog);*/

            // father
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svMale);
            ClickButton("btnFatherAdd", fDialog);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnFatherDelete", fDialog);

            // mother
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svFemale);
            ClickButton("btnMotherAdd", fDialog);
            ModalFormHandler = MessageBox_YesHandler;
            ClickButton("btnMotherDelete", fDialog);

            ClickButton("btnNameCopy", fDialog);

            // events
            tabs.SelectTab(0);
            Assert.AreEqual(1, indiRecord.Events.Count);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(2, indiRecord.Events.Count);

            SelectSheetListItem("fEventsList", fDialog, 1);
            SetModalFormHandler(this, EventEditDlgTests.EventEditDlg_Select_Handler);
            ClickToolStripButton("fEventsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(2, indiRecord.Events.Count);

            TestDeleteSheetListItem("fEventsList", 1);
            Assert.AreEqual(1, indiRecord.Events.Count);

            // spouses
            tabs.SelectTab(1);
            Assert.AreEqual(0, indiRecord.SpouseToFamilyLinks.Count);
            ModalFormHandler = FamilyEditDlgTests.SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRecord.SpouseToFamilyLinks.Count);

            SelectSheetListItem("fSpousesList", fDialog, 0);
            ModalFormHandler = FamilyEditDlgTests.SpouseEdit_Handler;
            ClickToolStripButton("fSpousesList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(1, indiRecord.SpouseToFamilyLinks.Count);

            TestDeleteSheetListItem("fSpousesList", 0);
            Assert.AreEqual(0, indiRecord.SpouseToFamilyLinks.Count);

            // names
            tabs.SelectTab(2);
            Assert.AreEqual(1, indiRecord.PersonalNames.Count);
            ModalFormHandler = PersonalNameEditDlgTests.NameEditAdd_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(2, indiRecord.PersonalNames.Count);
            Assert.AreEqual("sample surname", indiRecord.PersonalNames[1].Surname);

            SelectSheetListItem("fNamesList", fDialog, 1);
            ModalFormHandler = PersonalNameEditDlgTests.NameEditEdit_Handler;
            ClickToolStripButton("fNamesList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual(2, indiRecord.PersonalNames.Count);
            Assert.AreEqual("sample surname2", indiRecord.PersonalNames[1].Surname);

            TestDeleteSheetListItem("fNamesList", 1);
            Assert.AreEqual(1, indiRecord.PersonalNames.Count);

            // associations
            /*tabs.SelectTab(3);
            Assert.AreEqual(0, indiRecord.Associations.Count);
            ModalFormHandler = AssociationEditDlgTests.AcceptModalHandler;
            ClickToolStripButton("fAssociationsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRecord.Associations.Count);
            Assert.AreEqual("sample relation", indiRecord.Associations[0].Relation);

            TestDeleteSheetListItem("fAssociationsList", 0);
            Assert.AreEqual(0, indiRecord.Associations.Count);*/

            // groups
            tabs.SelectTab(6);
            tabsOther.SelectTab(1);
            Assert.AreEqual(0, indiRecord.Groups.Count);
            RecordSelectDlgTests.SetCreateItemHandler(this, GroupEditDlgTests.GroupAdd_Mini_Handler);
            ClickToolStripButton("fGroupsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRecord.Groups.Count);
            Assert.AreEqual("sample group", fBase.Context.Tree.GetPtrValue<GDMGroupRecord>(indiRecord.Groups[0]).GroupName);

            // FIXME
            //TestDeleteSheetListItem("fGroupsList", 0);
            //Assert.AreEqual(0, indiRecord.Groups.Count);

            // userrefs
            /*tabs.SelectTab(8);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);
            ModalFormHandler = UserRefEditDlgTests.AcceptHandler;
            ClickToolStripButton("fUserRefList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, indiRecord.UserReferences.Count);
            Assert.AreEqual("sample reference", indiRecord.UserReferences[0].StringValue);

            TestDeleteSheetListItem("fUserRefList", 0);
            Assert.AreEqual(0, indiRecord.UserReferences.Count);*/

            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        private static GDMSex fNeedIndividualSex;

        public static void IndividualAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "test");
            SelectCombo("cmbSex", form, (int)fNeedIndividualSex);

            ClickButton("btnAccept", form);
        }

        public static void IndividualEdit_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "test2");

            ClickButton("btnAccept", form);
        }

        public static void SetCreateIndividualHandler(NUnitFormTest formTest, GDMSex needIndividualSex)
        {
            fNeedIndividualSex = needIndividualSex;
            RecordSelectDlgTests.SetCreateItemHandler(formTest, IndividualAdd_Mini_Handler);
        }

        #endregion
    }
}

#endif
