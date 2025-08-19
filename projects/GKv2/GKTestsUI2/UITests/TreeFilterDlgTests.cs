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
using System.Windows.Forms;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class TreeFilterDlgTests : CustomWindowTest
    {
        private BaseContext fContext;
        private ChartFilter fChartFilter;
        private IBaseWindow fBase;
        private TreeFilterDlg fDialog;

        public override void Setup()
        {
            CommonTests.InitUITest();

            fBase = new BaseWindowStub();
            fContext = fBase.Context;
            fChartFilter = new ChartFilter();

            fDialog = new TreeFilterDlg(fBase);
            fDialog.Filter = fChartFilter;
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
            Assert.IsNotNull(fDialog.Filter);

            ClickRadioButton("rbCutPersons", fDialog);

            ClickRadioButton("rbCutYears", fDialog);

            ClickRadioButton("rbCutNone", fDialog);

            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void TreeFilterDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
