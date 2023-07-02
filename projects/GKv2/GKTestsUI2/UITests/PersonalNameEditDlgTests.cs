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
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class PersonalNameEditDlgTests : CustomWindowTest
    {
        private GDMIndividualRecord fPerson;
        private GDMPersonalName fPersonalName;
        private IBaseWindow fBase;
        private PersonalNameEditDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fPerson = new GDMIndividualRecord(fBase.Context.Tree);
            fPersonalName = new GDMPersonalName();

            fDialog = new PersonalNameEditDlg(fBase);
            fDialog.IndividualRecord = fPerson;
            fDialog.PersonalName = fPersonalName;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fPersonalName.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fPersonalName, fDialog.PersonalName);

            EnterText("txtSurname", fDialog, "sample text");

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fPersonalName.Surname);
        }

        #region Handlers for external tests

        public static void NameEditAdd_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtSurname", form, "sample surname");

            ClickButton("btnAccept", form);
        }

        public static void NameEditEdit_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtSurname", form, "sample surname2");

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
