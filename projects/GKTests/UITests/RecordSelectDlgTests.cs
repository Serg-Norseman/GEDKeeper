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
using GDModel;
using GKCore.Interfaces;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Framework;
using NUnit.Extensions.Forms;
using GKTests.ControlTesters;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class RecordSelectDlgTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private RecordSelectDlg fDialog;

        public override void Setup()
        {
            fBase = new BaseWindowStub();

            fDialog = new RecordSelectDlg(fBase, GDMRecordType.rtIndividual);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            Assert.AreEqual("*", fDialog.FastFilter);
            Assert.AreEqual(null, fDialog.ResultRecord);

            ClickButton("btnCancel", fDialog);
        }

        #region Handlers for external tests

        public static void RecordSelectDlg_Cancel_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnCancel", form);
        }

        private static int RSD_ItemIndex;

        private static void RSD_SelectItem_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtFastFilter", form, "*");

            var listRecords = new GKRecordsViewTester("fListRecords", form);
            listRecords.Properties.SelectItem(RSD_ItemIndex);

            ClickButton("btnSelect", form);
        }

        public static void SetSelectItemHandler(int itemIndex)
        {
            RSD_ItemIndex = itemIndex;
            SetModalFormHandler(fFormTest, RSD_SelectItem_Handler);
        }

        private static ModalFormHandler RSD_SubHandler;

        private static void RSD_CreateItem_Handler(string name, IntPtr ptr, Form form)
        {
            SetModalFormHandler(fFormTest, RSD_SubHandler);
            ClickButton("btnCreate", form);
        }

        public static void SetCreateItemHandler(NUnitFormTest formTest, ModalFormHandler createHandler)
        {
            RSD_SubHandler = createHandler;
            SetModalFormHandler(formTest, RSD_CreateItem_Handler);
        }

        #endregion
    }
}

#endif
