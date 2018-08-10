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

using System;
using System.Windows.Forms;
using NUnit.Extensions.Forms;

namespace GKTests
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class CustomWindowTest : NUnitFormTest
    {
        public static void ClickButton(string name, Form form)
        {
            var tsBtn = new ButtonTester(name, form);
            if (tsBtn.Count > 1) {
                // FIXME: Find out why sometimes the search returns
                // two components where there is only one (MediaViewerWinTests)
                tsBtn[0].FireEvent("Click");
            } else {
                tsBtn.FireEvent("Click");
            }
        }

        public static void ClickButton(string name, string form)
        {
            var tsBtn = new ButtonTester(name, form);
            if (tsBtn.Count > 1) {
                // FIXME: Find out why sometimes the search returns
                // two components where there is only one (MediaViewerWinTests)
                tsBtn[0].FireEvent("Click");
            } else {
                tsBtn.FireEvent("Click");
            }
        }

        public static void ClickToolStripButton(string name, Form form)
        {
            var tsBtn = new ToolStripButtonTester(name, form);
            if (tsBtn.Count > 1) {
                // FIXME: Find out why sometimes the search returns
                // two components where there is only one (MediaViewerWinTests)
                tsBtn[0].FireEvent("Click");
            } else {
                tsBtn.FireEvent("Click");
            }
        }

        public static void ClickToolStripMenuItem(string name, Form form)
        {
            var tsMenuItem = new ToolStripMenuItemTester(name, form);
            tsMenuItem.Click();
        }

        public static void ClickRadioButton(string name, Form form)
        {
            var radBtn = new RadioButtonTester(name, form);
            radBtn.Click();
        }


        public static void KeyDownForm(string formName, Keys keyData)
        {
            var formTester = new FormTester(formName);
            formTester.FireEvent("KeyDown", new KeyEventArgs(keyData));
        }

        public static void MessageBox_YesHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.Yes);
        }

        public static void MessageBox_NoHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.No);
        }

        public static void MessageBox_OkHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.OK);
        }

        public static void MessageBox_CancelHandler(string name, IntPtr ptr, Form form)
        {
            MessageBoxTester messageBox = new MessageBoxTester(ptr);
            messageBox.SendCommand(MessageBoxTester.Command.Cancel);
        }

        public static void PrepareFileSave(string fileName, IntPtr hWnd)
        {
            fileName = TestUtils.GetTempFilePath(fileName);

            var saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.SaveFile(fileName);
            saveDlg.SaveFile();
        }

        public static Form GetActiveForm(string formName)
        {
            var tester = new FormTester(formName);
            return (tester == null) ? null : (Form)tester.TheObject;
        }

        public static void OpenFile_Cancel_Handler(string name, IntPtr hWnd, Form form)
        {
            var openDlg = new OpenFileDialogTester(hWnd);
            openDlg.ClickCancel();
        }

        public static void SaveFile_Cancel_Handler(string name, IntPtr hWnd, Form form)
        {
            var saveDlg = new SaveFileDialogTester(hWnd);
            saveDlg.ClickCancel();
        }

        public static void Dialog_Cancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }
    }
}

#endif
