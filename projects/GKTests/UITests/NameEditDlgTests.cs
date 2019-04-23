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
using GKUI.Providers;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class NameEditDlgTests : CustomWindowTest
    {
        private NameEntry fNameEntry;
        private IBaseWindow fBase;
        private NameEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fNameEntry = new NameEntry();

            fDialog = new NameEditDlg();
            fDialog.IName = null;
            fDialog.IName = fNameEntry;
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
            Assert.AreEqual(fNameEntry, fDialog.IName);

            SelectCombo("cmbSex", fDialog, 1); // male
            EnterText("txtName", fDialog, "Ivan");
            EnterText("txtFPatr", fDialog, "Ivanovna");
            EnterText("txtMPatr", fDialog, "Ivanovich");
            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("Ivan", fNameEntry.Name);
            Assert.AreEqual("Ivanovich", fNameEntry.M_Patronymic);
            Assert.AreEqual("Ivanovna", fNameEntry.F_Patronymic);
            Assert.AreEqual(GEDCOMSex.svMale, fNameEntry.Sex);
        }

        #region Handlers for external tests
        #endregion
    }
}

#endif
