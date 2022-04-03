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

using System.Windows.Forms;
using GKCore.Interfaces;
using GKCore.Lists;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Framework;
using NUnit.Extensions.Forms;

namespace GKUI.Forms
{
    /// <summary>
    ///
    /// </summary>
    [TestFixture]
    public class PersonsFilterDlgTests : CustomWindowTest
    {
        private IListManager fListMan;
        private IBaseWindow fBase;
        private PersonsFilterDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fListMan = new IndividualListMan(fBase.Context);

            fDialog = new PersonsFilterDlg(fBase, fListMan);
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
        public void Test_Reset()
        {
            ClickButton("btnReset", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fBase, fDialog.Base);

            /*EnterCombo("cmbRelation", "sample text");
            Assert.AreEqual("sample text", cmbRelation.Text);*/

            /*EnterText("txtAuthor", "sample text");
            Assert.AreEqual("sample text", txtAuthor.Text);*/

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fListMan.Relation);
            //Assert.AreEqual("sample text\r\n", fTaskRecord.Originator.Text);
        }

        #region Handlers for external tests

        public static void PersonsFilterDlg_Handler(Form form)
        {
            PersonsFilterDlg pfDlg = (PersonsFilterDlg)form;

            SelectTab("tabsFilters", form, 1);

            ClickRadioButton("rgLife.rbAliveBefore", form);
            ClickRadioButton("rgLife.rbAll", form);

            var rbSexMale = new RadioButtonTester("rbSexMale", form);
            rbSexMale.Properties.Checked = true;

            var rbOnlyLive = new RadioButtonTester("rbOnlyLive", form);
            rbOnlyLive.Properties.Checked = true;

            EnterCombo("txtName", form, "*Ivan*");

            EnterCombo("cmbResidence", form, "*test place*");

            EnterCombo("cmbEventVal", form, "*test event*");

            EnterCombo("cmbGroup", form, "- any -");

            EnterCombo("cmbSource", form, "- any -");
        }

        #endregion
    }
}

#endif
