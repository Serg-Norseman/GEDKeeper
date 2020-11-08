/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GKTests.Stubs;
using GKUI;
//using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class AssociationEditControllerTests
    {
        private GDMAssociation fAssociation;
        private IBaseWindow fBase;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fAssociation = new GDMAssociation(null);
        }

        [Test]
        public void Test_Common()
        {
            /*var view = Substitute.For<IAssociationEditDlg>();

            Assert.IsNull(view.Association);
            Assert.IsNotNull(view.Person);
            Assert.IsNotNull(view.Relation);

            var controller = new AssociationEditDlgController(view);
            Assert.IsNotNull(controller);

            controller.Association = fAssociation;
            Assert.AreEqual(fAssociation, controller.Association);

            controller.Accept();*/

            //controller.SetPerson();

            /*Assert.Throws(typeof(ArgumentNullException), () => { AppHost.Instance.LoadBase(null, null); });*/
        }
    }
}
