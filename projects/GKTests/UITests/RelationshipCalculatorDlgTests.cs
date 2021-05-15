/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Interfaces;
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
    public class RelationshipCalculatorDlgTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private RelationshipCalculatorDlg fDialog;

        public override void Setup()
        {
            fBase = new BaseWindowStub();

            fDialog = new RelationshipCalculatorDlg(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            ClickButton("btnClose", fDialog);
        }

        #region Handlers for external tests

        public static void RelationshipCalculatorDlg_Handler(string name, IntPtr ptr, Form form)
        {
            RelationshipCalculatorDlg dlg = (RelationshipCalculatorDlg)form;
            IBaseContext baseContext = dlg.Base.Context;

            Assert.IsTrue(baseContext.Tree.RecordsCount > 1);

            GDMIndividualRecord iRec1 = baseContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(baseContext.Tree, iRec1, false));
            GDMIndividualRecord iRec2 = baseContext.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
            Assert.IsNotNull(iRec2);
            Assert.AreEqual("Ivanova Maria Petrovna", GKUtils.GetRecordName(baseContext.Tree, iRec2, false));

            WFAppHost.TEST_MODE = true; // FIXME: dirty hack

            RecordSelectDlgTests.SetSelectItemHandler(0);
            ClickButton("btnRec1Select", form);
            RecordSelectDlgTests.SetSelectItemHandler(1);
            ClickButton("btnRec2Select", form);

            var txtResult = new TextBoxTester("txtResult", form);
            // default is not Russian culture
            Assert.AreEqual("Ivanova Maria Petrovna is wife of Ivanov Ivan Ivanovich", txtResult.Text); // :D

            ClickButton("btnClose", form);
        }

        #endregion
    }
}

#endif
