﻿/*
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
using GKCore;
using GKCore.Design;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class FilePropertiesDlgTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private FilePropertiesDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();

            fDialog = new FilePropertiesDlg(fBase);
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
            EnterText("txtName", fDialog, "sample text");

            ClickButton("btnAccept", fDialog);

            GDMSubmitterRecord submitter = fBase.Context.Tree.GetPtrValue<GDMSubmitterRecord>( fBase.Context.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        #region Handlers for external tests

        public static void FilePropertiesDlg_btnAccept_Handler(string name, IntPtr ptr, Form form)
        {
            FilePropertiesDlg dlg = (FilePropertiesDlg)form;
            IBaseContext baseContext = dlg.Base.Context;

            EnterText("txtName", form, "sample text");

            SetModalFormHandler(fFormTest, LanguageEditDlgTests.LanguageEditDlg_Handler);
            ClickButton("btnLangEdit", form);

            ClickButton("btnAccept", form);

            GDMSubmitterRecord submitter = baseContext.Tree.GetPtrValue<GDMSubmitterRecord>(baseContext.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        #endregion
    }
}

#endif
