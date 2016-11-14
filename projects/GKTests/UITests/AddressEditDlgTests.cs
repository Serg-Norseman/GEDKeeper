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
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests.Mocks;
using GKUI.Dialogs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class AddressEditDlgTests : CustomWindowTest
    {
        private IBaseContext fContext;
        private GEDCOMAddress fAddress;
        private IBaseWindow fBase;
        private AddressEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fContext = fBase.Context;
            fAddress = new GEDCOMAddress(fContext.Tree, null, "", "");

            fAddress.AddWebPage("test");
            fAddress.AddPhoneNumber("test");
            fAddress.AddEmailAddress("test");
            fAddress.AddFaxNumber("test");

            //ExpectModal("AddressEditDlg", "DlgHandler");
            fDialog = new AddressEditDlg(fBase);
            fDialog.Address = fAddress;
            //fDialog.ShowDialog();
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_btnCancel()
        {
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fAddress, fDialog.Address);

            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            var txtCountry = new TextBoxTester("txtCountry");
            txtCountry.Enter("sample text");
            Assert.AreEqual("sample text", txtCountry.Text);

            var txtState = new TextBoxTester("txtState");
            txtState.Enter("sample text");
            Assert.AreEqual("sample text", txtState.Text);

            //var tabs = new TabControlTester("tabsAddrData");
            //tabs.SelectTab(1);

            // Test for adding phone
            ModalFormHandler = InputBoxHandler;
            var btnAddTester = new ToolStripButtonTester("fPhonesList_ToolBar_btnAdd", fDialog);
            btnAddTester.Click();
            Assert.AreEqual("sample text", fAddress.PhoneNumbers[1].StringValue);

            // Test for adding mail
            ModalFormHandler = InputBoxHandler;
            btnAddTester = new ToolStripButtonTester("fMailsList_ToolBar_btnAdd", fDialog);
            btnAddTester.Click();
            Assert.AreEqual("sample text", fAddress.EmailAddresses[1].StringValue);

            // Test for adding webpage
            ModalFormHandler = InputBoxHandler;
            btnAddTester = new ToolStripButtonTester("fWebsList_ToolBar_btnAdd", fDialog);
            btnAddTester.Click();
            Assert.AreEqual("sample text", fAddress.WebPages[1].StringValue);

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fAddress.AddressCountry);
            Assert.AreEqual("sample text", fAddress.AddressState);
        }

        public void InputBoxHandler(string name, IntPtr ptr, Form form)
        {
            var txtValue = new TextBoxTester("txtValue", form);
            txtValue.Enter("sample text");
            Assert.AreEqual("sample text", txtValue.Text);

            var tsBtn = new ButtonTester("btnAccept", form);
            tsBtn.FireEvent("Click");
        }
    }
}

#endif
