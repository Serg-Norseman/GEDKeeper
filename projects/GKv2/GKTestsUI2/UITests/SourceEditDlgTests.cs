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
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class SourceEditDlgTests : CustomWindowTest
    {
        private GDMSourceRecord fSourceRecord;
        private IBaseWindow fBase;
        private SourceEditDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fSourceRecord = new GDMSourceRecord(fBase.Context.Tree);

            fDialog = new SourceEditDlg(fBase);
            fDialog.SourceRecord = fSourceRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fSourceRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fSourceRecord, fDialog.SourceRecord);

            EnterText("txtShortTitle", fDialog, "sample text");
            EnterText("txtAuthor", fDialog, "sample text");

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fSourceRecord.ShortTitle);
            Assert.AreEqual("sample text", fSourceRecord.Originator.Lines.Text);
        }

        [Test]
        public void Test_Common()
        {
            GDMSourceRecord srcRecord = fDialog.SourceRecord;
            SelectTab("tabsData", fDialog, 2);

            // repositories
            Assert.AreEqual(0, srcRecord.RepositoryCitations.Count);
            RecordSelectDlgTests.SetCreateItemHandler(this, TaskEditDlgTests.TaskAdd_Mini_Handler);
            ClickToolStripButton("fRepositoriesList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, srcRecord.RepositoryCitations.Count);

            SelectSheetListItem("fRepositoriesList", fDialog, 0);
            SetModalFormHandler(this, MessageBox_YesHandler);
            ClickToolStripButton("fRepositoriesList_ToolBar_btnDelete", fDialog);
            Assert.AreEqual(0, srcRecord.RepositoryCitations.Count);

            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void SourceAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtShortTitle", form, "sample text");

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
