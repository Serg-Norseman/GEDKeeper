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
using GKTests;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class SexCheckDlgTests : CustomWindowTest
    {
        private SexCheckDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fDialog = new SexCheckDlg();
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            fDialog.IndividualName = "test name";
            Assert.AreEqual("test name", fDialog.IndividualName);

            fDialog.Sex = GDMSex.svMale;
            Assert.AreEqual(GDMSex.svMale, fDialog.Sex);

            fDialog.Sex = GDMSex.svFemale;
            Assert.AreEqual(GDMSex.svFemale, fDialog.Sex);

            fDialog.Sex = GDMSex.svUnknown;
            Assert.AreEqual(GDMSex.svUnknown, fDialog.Sex);

            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void SexCheckDlgTests_AcceptM_Handler(string name, IntPtr ptr, Form form)
        {
            CheckRadioButton("rbMale", form, true);
            ClickButton("btnAccept", form);
        }

        public static void SexCheckDlgTests_AcceptF_Handler(string name, IntPtr ptr, Form form)
        {
            CheckRadioButton("rbFemale", form, true);
            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
