/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKTests;
using GKUI.Providers;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class NamesTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);
        }

        [Test]
        public void Test_Names_01()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_names_01.ged")) {
                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("Александра Анатольевна Лазорева (Иванова)", iRec1.GetPrimaryFullName());
                // std-surn exists and double, but sub-surn has only second part
                // sub-givn exists, but sub-patn is not
                var parts = GKUtils.GetNameParts(iRec1);
                Assert.AreEqual("Иванова", parts.Surname);
                Assert.AreEqual("Александра", parts.Name);
                Assert.AreEqual("Анатольевна", parts.Patronymic);

                GDMIndividualRecord iRec2 = ctx.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
                Assert.IsNotNull(iRec2);
                Assert.AreEqual("Петр Константинович Лазорев", iRec2.GetPrimaryFullName());
                // std-surn exists, but sub-surn is not
                // sub-givn exists, but sub-patn is not
                parts = GKUtils.GetNameParts(iRec2);
                Assert.AreEqual("Лазорев", parts.Surname);
                Assert.AreEqual("Петр", parts.Name);
                Assert.AreEqual("Константинович", parts.Patronymic);
            }
        }

        [Test]
        public void Test_Names_02()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_names_02.ged")) {
                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("Анна Сидоровна Иванова (Петрова)", iRec1.GetPrimaryFullName());
                // std-surn exists and double, and sub-surn same
                // sub-givn and sub-patn exists
                var parts = GKUtils.GetNameParts(iRec1);
                Assert.AreEqual("Иванова (Петрова)", parts.Surname);
                Assert.AreEqual("Анна", parts.Name);
                Assert.AreEqual("Сидоровна", parts.Patronymic);

                GDMIndividualRecord iRec2 = ctx.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
                Assert.IsNotNull(iRec2);
                Assert.AreEqual("Аглая Федоровна Иванова", iRec2.GetPrimaryFullName());
                // std-surn exists (maiden), and sub-surn same, and sub-marn exists (married)
                // sub-givn and sub-patn exists
                parts = GKUtils.GetNameParts(iRec2);
                Assert.AreEqual("Иванова", parts.Surname);
                Assert.AreEqual("Лескова", parts.MarriedSurname);
                Assert.AreEqual("Аглая", parts.Name);
                Assert.AreEqual("Федоровна", parts.Patronymic);
            }
        }
    }
}
