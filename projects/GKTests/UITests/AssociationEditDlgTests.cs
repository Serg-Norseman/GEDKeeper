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
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class AssociationEditDlgTests : CustomWindowTest
    {
        private GEDCOMAssociation fAssociation;
        private IBaseWindow fBase;
        private AssociationEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowStub();
            fAssociation = new GEDCOMAssociation(fBase.Context.Tree, null);

            fDialog = new AssociationEditDlg(fBase);
            fDialog.Association = fAssociation;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fAssociation.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fAssociation, fDialog.Association);

            var cmbRelation = new ComboBoxTester("cmbRelation");
            cmbRelation.Enter("sample text");
            Assert.AreEqual("sample text", cmbRelation.Text);

            // TODO: click and select Individual reference
            //ModalFormHandler = RecordSelectDlg_Cancel_Handler;
            //ClickButton("btnPersonAdd", fDialog);

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fAssociation.Relation);
            Assert.AreEqual(null, fAssociation.Individual);
        }

        #region Handlers for external tests

        public static void AcceptModalHandler(string name, IntPtr ptr, Form form)
        {
            var cmbRelation = new ComboBoxTester("cmbRelation", form);
            cmbRelation.Enter("sample relation");

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
