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
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class LocationEditDlgTests : CustomWindowTest
    {
        private GDMLocationRecord fLocationRecord;
        private IBaseWindow fBase;
        private LocationEditDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fLocationRecord = new GDMLocationRecord(fBase.Context.Tree);

            fDialog = new LocationEditDlg(fBase);
            fDialog.LocationRecord = fLocationRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fLocationRecord.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            SelectTab("tabsData", fDialog, 0);

            EnterText("txtName", fDialog, "Moscow");

            var listGeoCoords = new ListViewTester("ListGeoCoords", fDialog);
            listGeoCoords.FireEvent("Click", new EventArgs());

            ClickButton("btnSearch", fDialog);
            ClickButton("btnSelect", fDialog);
            ClickButton("btnSelectName", fDialog);
            ClickButton("btnShowOnMap", fDialog);

            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void LocationAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "sample location");

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
