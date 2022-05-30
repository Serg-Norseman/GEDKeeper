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

using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Types;
using GKTests;
using GKUI.Platform;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class ControllerTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);
        }

        private static void SubstituteControl<T>(IView dialog, string ctlName) where T : class, IControl
        {
            var substControl = Substitute.For<T>();
            dialog.GetControl(ctlName).Returns(substControl);
        }

        [Test]
        public void Test_AssociationEditDlgController()
        {
            var view = Substitute.For<IAssociationEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblRelation");
            SubstituteControl<IComboBox>(view, "cmbRelation");
            SubstituteControl<ILabel>(view, "lblPerson");
            SubstituteControl<ITextBox>(view, "txtPerson");
            SubstituteControl<IButton>(view, "btnPersonAdd");

            Assert.IsNull(view.Association);
            Assert.IsNotNull(view.Person);
            Assert.IsNotNull(view.Relation);

            var baseContext = Substitute.For<IBaseContext>();
            baseContext.Tree.Returns(new GDMTree());
            var baseWin = Substitute.For<IBaseWindow>();
            baseWin.Context.Returns(baseContext);
            var tree = baseContext.Tree;
            var association = new GDMAssociation(); // for xref pointers to work

            var controller = new AssociationEditDlgController(view);
            Assert.IsNotNull(controller);

            controller.Init(baseWin);
            Assert.AreEqual(baseWin, controller.Base);

            controller.Association = association;
            Assert.AreEqual(association, controller.Association);

            // the association is empty
            controller.UpdateView();

            // the relation is empty, an exception will be thrown, accept will be false
            Assert.IsFalse(controller.Accept());

            var relValue = "test relation";
            var relPerson = tree.CreateIndividual();

            // substitutes of values
            view.Relation.Text.Returns(relValue);
            baseWin.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown).Returns(relPerson);

            controller.SetPerson();

            Assert.IsTrue(controller.Accept());
            Assert.AreEqual(relValue, association.Relation);
            Assert.AreEqual(relPerson, tree.GetPtrValue(association));
        }

        [Test]
        public void Test_LanguageEditDlgController()
        {
            var view = Substitute.For<ILanguageEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblLanguage");
            SubstituteControl<IComboBox>(view, "cmbLanguage");

            Assert.AreEqual(GDMLanguageID.Unknown, view.LanguageID);

            var baseWin = Substitute.For<IBaseWindow>();
            var tree = new GDMTree();

            var controller = new LanguageEditDlgController(view);
            Assert.IsNotNull(controller);

            controller.Init(baseWin);
            Assert.AreEqual(baseWin, controller.Base);

            controller.LanguageID = GDMLanguageID.Akkadian;
            Assert.AreEqual(GDMLanguageID.Akkadian, controller.LanguageID);

            var langValue = GDMLanguageID.AngloSaxon;

            // substitutes of values
            view.LanguageCombo.GetSelectedTag<GDMLanguageID>().Returns(langValue);

            Assert.IsTrue(controller.Accept());
            Assert.AreEqual(langValue, controller.LanguageID);
        }

        [Test]
        public void Test_UserRefEditDlgController()
        {
            var dlg = Substitute.For<IUserRefEditDlg>();
            SubstituteControl<IButton>(dlg, "btnAccept");
            SubstituteControl<IButton>(dlg, "btnCancel");
            SubstituteControl<ILabel>(dlg, "lblReference");
            SubstituteControl<IComboBox>(dlg, "cmbRef");
            SubstituteControl<ILabel>(dlg, "lblRefType");
            SubstituteControl<IComboBox>(dlg, "cmbRefType");

            var controllerInstance = new UserRefEditDlgController(dlg);
            var userRef = new GDMUserReference();

            controllerInstance.UserReference = userRef;
            Assert.AreEqual(userRef, controllerInstance.UserReference);

            dlg.Ref.Text = "sample text2";
            dlg.RefType.Text = "sample text3";

            controllerInstance.Accept();

            Assert.AreEqual("sample text2", userRef.StringValue);
            Assert.AreEqual("sample text3", userRef.ReferenceType);
        }
    }
}
