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

using GKCore.Charts;
using GKCore.Interfaces;
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
    public class TreeFilterDlgTests : CustomWindowTest
    {
        private IBaseContext fContext;
        private ChartFilter fChartFilter;
        private IBaseWindow fBase;
        private TreeFilterDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
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
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.IsNotNull(fDialog.Filter);

            var rbCutPersons = new RadioButtonTester("rbCutPersons", fDialog);
            rbCutPersons.Click();

            var rbCutYears = new RadioButtonTester("rbCutYears", fDialog);
            rbCutYears.Click();

            var rbCutNone = new RadioButtonTester("rbCutNone", fDialog);
            rbCutNone.Click();

            ClickButton("btnAccept", fDialog);
        }
    }
}

#endif
