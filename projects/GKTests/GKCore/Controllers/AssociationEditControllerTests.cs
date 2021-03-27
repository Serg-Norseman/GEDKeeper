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

using System;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Types;
using GKTests;
using GKUI;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class AssociationEditControllerTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);
        }

        [Test]
        public void Test_AssociationEditDlgController()
        {
            var view = Substitute.For<IAssociationEditDlg>();
            Assert.IsNull(view.Association);
            Assert.IsNotNull(view.Person);
            Assert.IsNotNull(view.Relation);

            var baseWin = Substitute.For<IBaseWindow>();
            //var baseContext = new BaseContext(null);
            //baseWin.Context.Returns(baseContext);
            var tree = new GDMTree();
            //var tree = baseContext.Tree;
            var association = new GDMAssociation(tree); // for xref pointers to work

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
    }
}
