/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

#if !MONO

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class AddressEditDlgTests : CustomWindowTest
    {
        private GDMAddress fAddress;
        private IBaseWindow fBase;
        private AddressEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fAddress = new GDMAddress();

            fAddress.AddWebPage("test");
            fAddress.AddPhoneNumber("test");
            fAddress.AddEmailAddress("test");
            fAddress.AddFaxNumber("test");

            fDialog = new AddressEditDlg(fBase);
            fDialog.Address = fAddress;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fAddress.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fAddress, fDialog.Address);

            EnterText("txtCountry", fDialog, "sample text");
            EnterText("txtState", fDialog, "sample text");

            // Test for adding phone
            SelectTab("tabsAddrData", fDialog, 1);
            ModalFormHandler = InputBox_Add_Handler;
            ClickToolStripButton("fPhonesList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual("sample add", fAddress.PhoneNumbers[1].StringValue);

            SelectSheetListItem("fPhonesList", fDialog, 1);
            ModalFormHandler = InputBox_Edit_Handler;
            ClickToolStripButton("fPhonesList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual("sample edit", fAddress.PhoneNumbers[1].StringValue);

            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fPhonesList_ToolBar_btnDelete", fDialog);

            // Test for adding mail
            SelectTab("tabsAddrData", fDialog, 2);
            ModalFormHandler = InputBox_Add_Handler;
            ClickToolStripButton("fMailsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual("sample add", fAddress.EmailAddresses[1].StringValue);

            SelectSheetListItem("fMailsList", fDialog, 1);
            ModalFormHandler = InputBox_Edit_Handler;
            ClickToolStripButton("fMailsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual("sample edit", fAddress.EmailAddresses[1].StringValue);

            ModalFormHandler = MessageBox_YesHandler;
            ClickToolStripButton("fMailsList_ToolBar_btnDelete", fDialog);

            // Test for adding webpage
            SelectTab("tabsAddrData", fDialog, 3);
            ModalFormHandler = InputBox_Add_Handler;
            ClickToolStripButton("fWebsList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual("sample add", fAddress.WebPages[1].StringValue);

            SelectSheetListItem("fWebsList", fDialog, 1);
            ModalFormHandler = InputBox_Edit_Handler;
            ClickToolStripButton("fWebsList_ToolBar_btnEdit", fDialog);
            Assert.AreEqual("sample edit", fAddress.WebPages[1].StringValue);

            //ModalFormHandler = MessageBox_YesHandler;
            //ClickToolStripButton("fWebsList_ToolBar_btnDelete", fDialog);


            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fAddress.AddressCountry);
            Assert.AreEqual("sample text", fAddress.AddressState);
        }

        #region Handlers for external tests

        public static void AddressEditDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            var addrDlg = (AddressEditDlg)form;
            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
