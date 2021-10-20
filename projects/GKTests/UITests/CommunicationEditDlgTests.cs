/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class CommunicationEditDlgTests : CustomWindowTest
    {
        private IBaseContext fContext;
        private GDMCommunicationRecord fCommunicationRecord;
        private IBaseWindow fBase;
        private CommunicationEditDlg fDialog;

        public override void Setup()
        {
            fBase = new BaseWindowStub();
            fContext = fBase.Context;
            fCommunicationRecord = new GDMCommunicationRecord(fContext.Tree);

            fDialog = new CommunicationEditDlg(fBase);
            fDialog.Communication = fCommunicationRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fCommunicationRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fCommunicationRecord, fDialog.Communication);

            EnterText("txtName", fDialog, "sample text");
            SelectCombo("cmbCorrType", fDialog, 1);
            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fCommunicationRecord.CommName);
            Assert.AreEqual(GDMCommunicationType.ctEMail, fCommunicationRecord.CommunicationType);
            Assert.AreEqual("", fCommunicationRecord.Date.StringValue);
        }

        [Test]
        public void Test_EnterDataDatesAndApply()
        {
            Assert.AreEqual(fCommunicationRecord, fDialog.Communication);

            EnterText("txtName", fDialog, "sample text");
            EnterMaskedText("txtDate", fDialog, "02.02.2000");
            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fCommunicationRecord.CommName);
            Assert.AreEqual("02 FEB 2000", fCommunicationRecord.Date.StringValue);
        }

        #region Handlers for external tests

        public static void CommunicationAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //EnterText("edName", form, "sample group");

            ClickButton("btnAccept", form);
        }

        public static void CommunicationEditDlg_Handler(CommunicationEditDlg dlg)
        {
            PersonEditDlgTests.SetCreateIndividualHandler(fFormTest, GDMSex.svMale);
            ClickButton("btnPersonAdd", dlg);

            ClickButton("btnAccept", dlg);
        }

        #endregion
    }
}

#endif
