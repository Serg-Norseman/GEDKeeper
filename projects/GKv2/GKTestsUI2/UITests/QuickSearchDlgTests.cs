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

using System.Windows.Forms;
using GKTests;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class QuickSearchDlgTests : CustomWindowTest
    {

        #region Handlers for external tests

        public static void QuickSearch_Test(NUnitFormTest formTest, Form mainWin)
        {
            ClickToolStripMenuItem("miSearch", mainWin);

            var searchPanel = new FormTester("QuickSearchDlg");
            var frm = (QuickSearchDlg)searchPanel.Properties;

            // handlers for empty text
            ClickButton("btnPrev", frm);
            ClickButton("btnNext", frm);

            EnterText("txtSearchPattern", frm, "John");
            // handlers for entered text? - msgbox processing

            // NoMatchesFound error msg
            SetModalFormHandler(formTest, MessageBox_OkHandler);
            KeyDownForm(frm.Name, Keys.Enter);

            SetModalFormHandler(formTest, MessageBox_OkHandler);
            KeyDownForm(frm.Name, Keys.Enter | Keys.Shift);

            KeyDownForm(frm.Name, Keys.Escape);
        }

        #endregion
    }
}

#endif
