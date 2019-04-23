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

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class PersonEditDlgTests : CustomWindowTest
    {
        private GEDCOMIndividualRecord fIndividualRecord;
        private IBaseWindow fBase;
        private PersonEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowStub();
            fIndividualRecord = fBase.Context.CreatePersonEx("Ivan", "", "Smith", GEDCOMSex.svMale, true);

            fDialog = new PersonEditDlg(fBase);
            fDialog.Person = fIndividualRecord;
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
            Assert.AreEqual(fIndividualRecord, fDialog.Person);
            Assert.AreEqual(null, fDialog.Target);
            Assert.AreEqual(TargetMode.tmNone, fDialog.TargetMode);

            //EnterText("txtSurname", "sample text");

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fIndividualRecord.PersonalNames[0].Pieces.Surname);
        }
    }
}

#endif
