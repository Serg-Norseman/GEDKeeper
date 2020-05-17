/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GKTests;
using GKUI.Forms;
using GKUI.Components;
using NUnit.Framework;
using NUnit.Extensions.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class LocationEditDlgTests : CustomWindowTest
    {

        #region Handlers for external tests

        public static void LocationAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "sample location");

            ClickButton("btnAccept", form);
        }

        public static void LocationEditDlg_Handler(LocationEditDlg dlg)
        {
            SelectTab("tabsData", dlg, 0);

            EnterText("txtName", dlg, "Moscow");

            var listGeoCoords = new ListViewTester("ListGeoCoords", dlg);
            listGeoCoords.FireEvent("Click", new EventArgs());

            ClickButton("btnSearch", dlg);
            ClickButton("btnSelect", dlg);
            ClickButton("btnSelectName", dlg);
            ClickButton("btnShowOnMap", dlg);

            ClickButton("btnAccept", dlg);
        }

        #endregion
    }
}

#endif
