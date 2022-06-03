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
using GKCore.Interfaces;
using GKCore.Lists;
using GKTests;
using GKTests.ControlTesters;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    ///
    /// </summary>
    [TestFixture]
    public class CommonFilterDlgTests : CustomWindowTest
    {
        private IBaseContext fContext;
        private IListManager fListMan;
        private IBaseWindow fBase;
        private CommonFilterDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fContext = fBase.Context;
            fListMan = new IndividualListMan(fContext);

            fDialog = new CommonFilterDlg(fBase, fListMan);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            Assert.AreEqual(fBase, fDialog.Base);

            ClickButton("btnReset", fDialog);

            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void CommonFilterDlg_btnReset_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnReset", form);
            ClickButton("btnAccept", form);
        }

        public static void CommonFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            CommonFilterDlg cfDlg = ((CommonFilterDlg)form);
            Assert.IsNotNull(cfDlg.Base);

            IListManager listMan = cfDlg.ListMan;

            SelectTab("tabsFilters", form, 0);

            var dataGridView1 = new DataGridViewTester("dataGridView1", form);
            dataGridView1.SelectCell(0, 0);
            dataGridView1.Properties.BeginEdit(false);
            dataGridView1.Properties.EndEdit();
            dataGridView1.SelectCell(0, 1);
            dataGridView1.Properties.BeginEdit(false);
            dataGridView1.Properties.EndEdit();
            dataGridView1.SelectCell(0, 2);
            dataGridView1.Properties.BeginEdit(false);
            dataGridView1.Properties.EndEdit();

            // Fail: AmbiguousMatch?!
            //dataGridView1.FireEvent("Scroll", new ScrollEventArgs(ScrollEventType.SmallIncrement, 1));

            if (form is PersonsFilterDlg) {
                PersonsFilterDlgTests.PersonsFilterDlg_Handler(form);
            }

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
