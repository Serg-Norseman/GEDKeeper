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

#if !__MonoCS__

using System;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests;
using GKTests.ControlTesters;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Extensions.Forms;
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
            base.Setup();

            fBase = new BaseWindowStub();
            fGroupRecord = new GDMGroupRecord(fBase.Context.Tree);

            fDialog = new GroupEditDlg(fBase);
            fDialog.Group = fGroupRecord;
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
            Assert.AreEqual(fGroupRecord, fDialog.Group);

            var sheetTester = new GKSheetListTester("fMembersList", fDialog);
            //EnumSet<SheetButton> buttons = sheetTester.Properties.Buttons;
            //Assert.IsTrue(buttons.ContainsAll(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump));
            Assert.IsFalse(sheetTester.Properties.ReadOnly);

            EnterText("edName", fDialog, "sample text");

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fGroupRecord.GroupName);
        }

        #region Handlers for external tests

        public static void GroupAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("edName", form, "sample group");

            ClickButton("btnAccept", form);
        }

        public static void GroupEditDlg_Handler(GroupEditDlg dlg)
        {
            GDMGroupRecord groupRecord = dlg.Group;

            // members
            Assert.AreEqual(0, groupRecord.Members.Count);
            RecordSelectDlgTests.SetSelectItemHandler(fFormTest, 0);
            ClickToolStripButton("fMembersList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, groupRecord.Members.Count);

            SetModalFormHandler(fFormTest, MessageBox_YesHandler);
            SelectSheetListItem("fMembersList", dlg, 0);
            ClickToolStripButton("fMembersList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, groupRecord.Members.Count);

            ClickButton("btnAccept", dlg);
        }

        #endregion
    }
}

#endif
