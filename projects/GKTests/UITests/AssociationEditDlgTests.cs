/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKTests.Mocks;
using GKUI.Dialogs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
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

            fBase = new BaseWindowMock();
            fAssociation = new GEDCOMAssociation(fBase.Context.Tree, null, "", "");

            fDialog = new AssociationEditDlg(fBase);
            fDialog.Association = fAssociation;
            fDialog.Show();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fAssociation, fDialog.Association);

            var cmbRelation = new ComboBoxTester("cmbRelation");
            cmbRelation.Enter("sample text");
            Assert.AreEqual("sample text", cmbRelation.Text);

            // TODO: click and select Individual reference
            /*var txtAuthor = new TextBoxTester("txtAuthor");
            txtAuthor.Enter("sample text");
            Assert.AreEqual("sample text", txtAuthor.Text);*/

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fAssociation.Relation);
            Assert.AreEqual(null, fAssociation.Individual);
        }
    }
}

#endif
