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
    public class ResearchEditDlgTests : CustomWindowTest
    {
        private IBaseContext fContext;
        private GEDCOMResearchRecord fResearchRecord;
        private IBaseWindow fBase;
        private ResearchEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fContext = fBase.Context;
            fResearchRecord = new GEDCOMResearchRecord(fContext.Tree, fContext.Tree, "", "");

            //ExpectModal("ResearchEditDlg", "DlgHandler");
            fDialog = new ResearchEditDlg(fBase);
            fDialog.Research = fResearchRecord;
            //_frm.ShowDialog();
            fDialog.Show();
        }

        [Test]
        public void Test_Misc()
        {
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fResearchRecord, fDialog.Research);
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
            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);

            /*var txtAuthor = new TextBoxTester("txtAuthor");
            txtAuthor.Enter("sample text");
            Assert.AreEqual("sample text", txtAuthor.Text);*/

            var btnAccept = new ButtonTester("btnAccept");
            btnAccept.Click();

            Assert.AreEqual("sample text", fResearchRecord.ResearchName);
            //Assert.AreEqual("sample text\r\n", fResearchRecord.Originator.Text);
        }
    }
}

#endif
