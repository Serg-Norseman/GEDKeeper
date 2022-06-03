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
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Framework;
using NUnit.Extensions.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class PersonEditDlgTests : CustomWindowTest
    {
        private GDMIndividualRecord fIndividualRecord;
        private IBaseWindow fBase;
        private PersonEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fIndividualRecord = fBase.Context.CreatePersonEx("Ivan", "", "Smith", GDMSex.svMale, true);

            fDialog = new PersonEditDlg(fBase);
            fDialog.IndividualRecord = fIndividualRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fIndividualRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fIndividualRecord, fDialog.IndividualRecord);
            Assert.AreEqual(null, fDialog.Target);
            Assert.AreEqual(TargetMode.tmNone, fDialog.TargetMode);

            //EnterText("txtSurname", "sample text");

            // empty individual parents, no effects
            ClickButton("btnFatherSel", fDialog);
            ClickButton("btnMotherSel", fDialog);

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fIndividualRecord.PersonalNames[0].Pieces.Surname);
        }

        #region Handlers for external tests

        private static GDMSex fNeedIndividualSex;

        private static void IndividualAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "test");
            SelectCombo("cmbSex", form, (int)fNeedIndividualSex);

            ClickButton("btnAccept", form);
        }

        public static void SetCreateIndividualHandler(NUnitFormTest formTest, GDMSex needIndividualSex)
        {
            fNeedIndividualSex = needIndividualSex;
            RecordSelectDlgTests.SetCreateItemHandler(formTest, IndividualAdd_Mini_Handler);
        }

        #endregion
    }
}

#endif
