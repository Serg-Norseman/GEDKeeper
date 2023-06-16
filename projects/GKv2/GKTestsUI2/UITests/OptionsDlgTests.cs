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
using System.Windows.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class OptionsDlgTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private OptionsDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();

            fDialog = new OptionsDlg(AppHost.Instance);
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
            /*EnterCombo("cmbRelation", "sample text");
            Assert.AreEqual("sample text", cmbRelation.Text);*/

            /*EnterText("txtAuthor", "sample text");
            Assert.AreEqual("sample text", txtAuthor.Text);*/

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fListMan.Relation);
            //Assert.AreEqual("sample text\r\n", fTaskRecord.Originator.Text);
        }

        #region Handlers for external tests

        public static void OptionsDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            var optDlg = ((OptionsDlg)form);

            optDlg.SetPage(OptionsPage.opCommon);

            optDlg.SetPage(OptionsPage.opTreeChart);
            CheckBox("chkPortraitsVisible", form, false);
            CheckBox("chkPortraitsVisible", form, true);

            optDlg.SetPage(OptionsPage.opCircleChart);

            optDlg.SetPage(OptionsPage.opInterface);
            CheckBox("chkExtendWomanSurnames", form, true);
            CheckBox("chkExtendWomanSurnames", form, false);

            optDlg.SetPage(OptionsPage.opPedigree);

            ClickButton("btnColumnUp", form);
            ClickButton("btnColumnDown", form);
            ClickButton("btnDefList", form);

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
