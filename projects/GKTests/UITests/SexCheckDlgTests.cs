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
using GKCommon.GEDCOM;
using GKUI.Dialogs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
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
            base.Setup();

            fDialog = new SexCheckDlg();
            fDialog.Show();
        }

        [Test]
        public void Test_Cancel()
        {
            fDialog.IndividualName = "test name";
            Assert.AreEqual("test name", fDialog.IndividualName);

            fDialog.Sex = GEDCOMSex.svMale;
            Assert.AreEqual(GEDCOMSex.svMale, fDialog.Sex);

            fDialog.Sex = GEDCOMSex.svFemale;
            Assert.AreEqual(GEDCOMSex.svFemale, fDialog.Sex);

            fDialog.Sex = GEDCOMSex.svNone;
            Assert.AreEqual(GEDCOMSex.svNone, fDialog.Sex);

            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            ClickButton("btnAccept", fDialog);
        }

        #region Handlers for external tests

        public static void SexCheckDlgTests_Accept_Handler(string name, IntPtr ptr, Form form)
        {
            var rbMale = new RadioButtonTester("rbMale", form);
            rbMale.Properties.Checked = true;

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
