/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class AssociationEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_AssociationEditDlgController()
        {
            IAssociationEditDlg view = CreateMockView();
            var controller = new AssociationEditDlgController(view);
            Assert.IsNotNull(controller);
            controller.ApplyTheme();

            var baseWin = GetBaseSubst();
            controller.InitView(baseWin);
            Assert.AreEqual(baseWin, controller.Base);

            var association = new GDMAssociation(); // for xref pointers to work
            controller.Association = association;
            Assert.AreEqual(association, controller.Association);

            // the association is empty
            controller.UpdateView();

            // the relation is empty, an exception will be thrown, accept will be false
            Assert.IsFalse(controller.Accept());

            // substitutes of values
            var relValue = "test relation";
            _cmbRelation.Text.Returns(relValue);
            var relPerson = baseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relPerson);

            controller.SetPerson();

            Assert.IsTrue(controller.Accept());
            controller.UpdateView();
            Assert.AreEqual(relValue, association.Relation);
            Assert.AreEqual(relPerson, baseWin.Context.Tree.GetPtrValue(association));
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnPersonAdd;
        private ILabel _lblRelation;
        private ILabel _lblPerson;
        private IComboBox _cmbRelation;
        private ITextBox _txtPerson;

        private IAssociationEditDlg CreateMockView()
        {
            var view = Substitute.For<IAssociationEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblRelation = SubstCtl<ILabel>(view, "lblRelation");
            _cmbRelation = SubstCtl<IComboBox>(view, "cmbRelation");
            _lblPerson = SubstCtl<ILabel>(view, "lblPerson");
            _txtPerson = SubstCtl<ITextBox>(view, "txtPerson");
            _btnPersonAdd = SubstCtl<IButton>(view, "btnPersonAdd");
            return view;
        }
    }
}
