/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#if !MONO && !DIS_NUF

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.ControlTesters;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class GroupEditDlgTests : CustomWindowTest
    {
        private GDMGroupRecord fGroupRecord;
        private IBaseWindow fBase;
        private GroupEditDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fGroupRecord = new GDMGroupRecord(fBase.Context.Tree);

            fDialog = new GroupEditDlg(fBase);
            fDialog.GroupRecord = fGroupRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fGroupRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fGroupRecord, fDialog.GroupRecord);

            var sheetTester = new GKSheetListTester("fMembersList", fDialog);
            //EnumSet<SheetButton> buttons = sheetTester.Properties.Buttons;
            //Assert.IsTrue(buttons.ContainsAll(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump));
            Assert.IsFalse(sheetTester.Properties.ReadOnly);

            EnterText("edName", fDialog, "sample text");

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fGroupRecord.GroupName);
        }

        [Test]
        public void Test_Common()
        {
            GDMGroupRecord groupRecord = fDialog.GroupRecord;

            // members
            Assert.AreEqual(0, groupRecord.Members.Count);
            PersonEditDlgTests.SetCreateIndividualHandler(this, GDMSex.svMale);
            ClickToolStripButton("fMembersList_ToolBar_btnAdd", fDialog);
            Assert.AreEqual(1, groupRecord.Members.Count);

            SelectSheetListItem("fMembersList", fDialog, 0);
            SetModalFormHandler(this, MessageBox_YesHandler);
            ClickToolStripButton("fMembersList_ToolBar_btnDelete", fDialog);
            Assert.AreEqual(0, groupRecord.Members.Count);

            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void GroupAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("edName", form, "sample group");

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
