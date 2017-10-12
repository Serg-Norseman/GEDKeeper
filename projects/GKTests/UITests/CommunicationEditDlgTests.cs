/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests.ControlTesters;
using GKTests.Mocks;
using GKUI.Forms;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class CommunicationEditDlgTests : CustomWindowTest
    {
        private IBaseContext fContext;
        private GEDCOMCommunicationRecord fCommunicationRecord;
        private IBaseWindow fBase;
        private CommunicationEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fContext = fBase.Context;
            fCommunicationRecord = new GEDCOMCommunicationRecord(fContext.Tree, fContext.Tree, "", "");

            fDialog = new CommunicationEditDlg();
            fDialog.InitDialog(fBase);
            fDialog.Communication = fCommunicationRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fCommunicationRecord, fDialog.Communication);

            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");

            var cmbCorrType = new ComboBoxTester("cmbCorrType", fDialog);
            cmbCorrType.Select(1);

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fCommunicationRecord.CommName);
            Assert.AreEqual(GKCommunicationType.ctEMail, fCommunicationRecord.CommunicationType);
            Assert.AreEqual("", fCommunicationRecord.Date.StringValue);
        }

        [Test]
        public void Test_EnterDataDatesAndApply()
        {
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fCommunicationRecord, fDialog.Communication);

            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");

            var txtDate = new MaskedTextBoxTester("txtDate", fDialog);
            txtDate.Enter("20.02.2000");

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fCommunicationRecord.CommName);
            Assert.AreEqual("20 FEB 2000", fCommunicationRecord.Date.StringValue);
        }
    }
}

#endif
