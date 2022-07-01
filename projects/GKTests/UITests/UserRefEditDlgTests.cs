/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

#if !MONO

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
    /// Isolated test of dialogue (UserRefEditDlg), without the ability
    /// to add or change references to other records.
    /// </summary>
    [TestFixture]
    public class UserRefEditDlgTests : CustomWindowTest
    {
        private GDMUserReference fUserRef;
        private IBaseWindow fBase;
        private UserRefEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitUITest();

            fBase = new BaseWindowStub();
            fUserRef = new GDMUserReference();

            fDialog = new UserRefEditDlg(fBase);
            fDialog.UserReference = fUserRef;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fUserRef.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fUserRef, fDialog.UserReference);

            EnterCombo("cmbRef", fDialog, "sample text");
            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fUserRef.StringValue);
        }

        #region Handlers for external tests

        public static void AcceptHandler(string name, IntPtr ptr, Form form)
        {
            EnterCombo("cmbRef", form, "sample reference");
            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
