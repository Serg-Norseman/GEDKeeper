﻿/*
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
using GKCore.Cultures;
using GKCore.Interfaces;
using GKTests;
using GKUI.Providers;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class CulturesTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);

            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }


        [Test]
        public void Test_AncientCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            var culture = new AncientCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));

            Assert.AreEqual("Ivanova Anna Ivanovna", culture.GetPossessiveName(iRec));
        }

        [Test]
        public void Test_IcelandCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            var culture = new IcelandCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic());
            Assert.IsFalse(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));
        }

        [Test]
        public void Test_BritishCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            var culture = new BritishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));
        }

        [Test]
        public void Test_SwedishCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            var culture = new SwedishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic());
            Assert.IsTrue(culture.HasSurname());
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));
        }

        [Test]
        public void Test_RussianCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            ICulture rusCulture = new RussianCulture();
            Assert.IsNotNull(rusCulture);
            Assert.IsTrue(rusCulture.HasPatronymic());
            Assert.IsTrue(rusCulture.HasSurname());

            string[] surnames = rusCulture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { rusCulture.GetSurnames(null); });


            Assert.AreEqual("Иванов", rusCulture.NormalizeSurname("Иванова", true));
            Assert.AreEqual("Бельский", rusCulture.NormalizeSurname("Бельская", true));
            Assert.AreEqual("Грозный", rusCulture.NormalizeSurname("Грозная", true));
            Assert.AreEqual("Иванов", rusCulture.NormalizeSurname("Иванов", false));
            Assert.AreEqual("Бельский", rusCulture.NormalizeSurname("Бельский", false));
            Assert.AreEqual("Грозный", rusCulture.NormalizeSurname("Грозный", false));

            Assert.AreEqual("?", rusCulture.NormalizeSurname(null, false));
            Assert.AreEqual("?", rusCulture.NormalizeSurname("", false));
            Assert.AreEqual("?", rusCulture.NormalizeSurname("(Иванова)", false));

            Assert.AreEqual("Иванова", rusCulture.GetMarriedSurname("Иванов"));
            Assert.AreEqual("Бельская", rusCulture.GetMarriedSurname("Бельский"));
            Assert.AreEqual("Грозная", rusCulture.GetMarriedSurname("Грозный"));

            Assert.AreEqual("?", rusCulture.GetMarriedSurname(""));
            Assert.AreEqual("?", rusCulture.GetMarriedSurname(null));

            string[] snms = rusCulture.GetSurnames("Бельская (Иванова)", true);
            Assert.AreEqual(2, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);
            Assert.AreEqual("Иванов", snms[1]);

            snms = rusCulture.GetSurnames("Бельская", true);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);

            snms = rusCulture.GetSurnames("Бельский", false);
            Assert.AreEqual(1, snms.Length);
            Assert.AreEqual("Бельский", snms[0]);


            GDMSex sx = rusCulture.GetSex("Мария", "Петровна", false);
            Assert.AreEqual(GDMSex.svFemale, sx);

            sx = rusCulture.GetSex("Иван", "Петрович", false);
            Assert.AreEqual(GDMSex.svMale, sx);

            Assert.AreEqual(GDMSex.svUnknown, rusCulture.GetSex("", "", false));

            Assert.AreEqual("Иванова Ивана Ивановича", rusCulture.GetPossessiveName("Иванов Иван Иванович"));

            GDMIndividualRecord iRec2 = fContext.Tree.CreateIndividual();
            GDMPersonalName persName = new GDMPersonalName(iRec2);
            persName.SetNameParts("Иван Иванович", "Иванов", "");
            iRec2.PersonalNames.Add(persName);

            Assert.AreEqual("Иванова Ивана Ивановича", rusCulture.GetPossessiveName(iRec2));
        }
    }
}
