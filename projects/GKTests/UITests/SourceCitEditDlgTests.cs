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
    /// 
    /// </summary>
    [TestFixture]
    public class SourceCitEditDlgTests : CustomWindowTest
    {
        private GDMSourceCitation fSourceCitation;
        private IBaseWindow fBase;
        private SourceCitEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitUITest();

            fBase = new BaseWindowStub();
            fSourceCitation = new GDMSourceCitation();

            fDialog = new SourceCitEditDlg(fBase);
            fDialog.SourceCitation = fSourceCitation;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fSourceCitation.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fSourceCitation, fDialog.SourceCitation);

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX/SelectRecord)

            ModalFormHandler = Dialog_Cancel_Handler;
            ClickButton("btnSourceAdd", fDialog);

            //ClickButton("btnAccept", fDialog); // bug?!
        }

        #region Handlers for external tests

        public static void AcceptModalHandler(string name, IntPtr ptr, Form form)
        {
            SelectCombo("cmbSource", form, 0);
            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
