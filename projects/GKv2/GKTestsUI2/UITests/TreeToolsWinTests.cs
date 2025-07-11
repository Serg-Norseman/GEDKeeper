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
using GKTests;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class TreeToolsWinTests : CustomWindowTest
    {

        #region Handlers for external tests

        public static void TreeCompareDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            ClickRadioButton("radMatchInternal", form);
            ClickButton("btnMatch", form);

            ClickRadioButton("radAnalysis", form);
            ClickButton("btnMatch", form);

            ClickRadioButton("radMathExternal", form);

            SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            ClickButton("btnFileChoose", form);
            //ClickButton("btnMatch", form);

            form.Close();
        }

        public static void TreeMergeDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            ClickButton("btnTreeMerge", form);

            form.Close();
        }

        public static void TreeSplitDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var frm = (TTTreeSplitDlg)form;
            var tabs = new TabControlTester("tabsTools", form);

            // FIXME
            //ClickButton("btnSelectFamily", form);
            //ClickButton("btnSelectAncestors", form);
            //ClickButton("btnSelectDescendants", form);
            //ClickButton("btnSelectAll", form);

            SetModalFormHandler(fFormTest, SaveFile_Cancel_Handler);
            ClickButton("btnSave", form);

            try {
                SetModalFormHandler(fFormTest, SaveFileGED_Handler);
                ClickButton("btnSave", form);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.ged"));
            }

            form.Close();
        }

        public static void RecMergeDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var frm = (TTRecMergeDlg)form;
            var tabs = new TabControlTester("tabsTools", form);

            CheckBox("chkBookmarkMerged", form, true);
            CheckBox("chkBookmarkMerged", form, false);

            CheckRadioButton("radPersons", form, true);

            RecordSelectDlgTests.SetSelectItemHandler(0);
            ClickButton("btnRec1Select", form);

            RecordSelectDlgTests.SetSelectItemHandler(1);
            ClickButton("btnRec2Select", form);

            ClickButton("btnAutoSearch", form);

            ClickButton("btnSkip", form);

            form.Close();
        }

        public static void FamilyGroupsDlg_Handler(CustomWindowTest formTest, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            ClickButton("btnAnalyseGroups", form);

            form.Close();
        }

        public static void TreeCheckDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            ClickButton("btnAnalyseBase", form);
            ClickButton("btnBaseRepair", form);

            form.Close();
        }

        public static void PatSearchDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            EnterNumeric("edMinGens", form, 1);

            ClickButton("btnPatSearch", form);

            ClickButton("btnSetPatriarch", form);

            ClickButton("btnPatriarchsDiagram", form);
            var pvWin = new FormTester("PatriarchsViewerWin");
            pvWin.Close();

            form.Close();
        }

        public static void PlacesManagerDlg_Handler(string name, IntPtr ptr, Form form)
        {
            var tabs = new TabControlTester("tabsTools", form);

            ClickButton("btnAnalysePlaces", form);
            ClickButton("btnIntoList", form);

            form.Close();
        }

        #endregion
    }
}

#endif
