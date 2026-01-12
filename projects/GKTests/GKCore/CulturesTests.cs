/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GDModel;
using GKTests;
using NUnit.Framework;

namespace GKCore.Cultures
{
    [TestFixture]
    public class CulturesTests
    {
        private BaseContext fContext;

        public CulturesTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public async Task Test_AncientCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            Assert.IsNotNull(iRec);

            var culture = new AncientCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic);
            Assert.IsFalse(culture.HasSurname);
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, await culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));

            Assert.AreEqual("Ivanova Anna Ivanovna", culture.GetPossessiveName(iRec));
        }

        [Test]
        public async Task Test_IcelandCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            Assert.IsNotNull(iRec);

            var culture = new IcelandCulture();
            Assert.IsNotNull(culture);
            Assert.IsTrue(culture.HasPatronymic);
            Assert.IsFalse(culture.HasSurname);
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, await culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));
        }

        [Test]
        public async Task Test_BritishCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            Assert.IsNotNull(iRec);

            var culture = new BritishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic);
            Assert.IsTrue(culture.HasSurname);
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, await culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));
        }

        [Test]
        public async Task Test_SwedishCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            Assert.IsNotNull(iRec);

            var culture = new SwedishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic);
            Assert.IsTrue(culture.HasSurname);
            Assert.AreEqual("Alef", culture.NormalizeSurname("Alef", false));
            Assert.AreEqual("Alef", culture.GetMarriedSurname("Alef"));
            Assert.AreEqual(GDMSex.svUnknown, await culture.GetSex("Alef", "", false));

            var surnames = culture.GetSurnames(iRec);
            Assert.AreEqual(1, surnames.Length);
            Assert.AreEqual("Ivanova", surnames[0]);
            Assert.Throws(typeof(ArgumentNullException), () => { culture.GetSurnames(null); });
            Assert.AreEqual("Ivanov Ivan", culture.GetPossessiveName("Ivanov Ivan"));
        }

        [Test]
        public async Task Test_RussianCulture()
        {
            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            Assert.IsNotNull(iRec);

            ICulture rusCulture = new RussianCulture();
            Assert.IsNotNull(rusCulture);
            Assert.IsTrue(rusCulture.HasPatronymic);
            Assert.IsTrue(rusCulture.HasSurname);

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

            Assert.AreEqual("Глухих", rusCulture.GetMarriedSurname("Глухих"));

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


            GDMSex sx = await rusCulture.GetSex("Мария", "Петровна", false);
            Assert.AreEqual(GDMSex.svFemale, sx);

            sx = await rusCulture.GetSex("Иван", "Петрович", false);
            Assert.AreEqual(GDMSex.svMale, sx);

            Assert.AreEqual(GDMSex.svUnknown, await rusCulture.GetSex("", "", false));

            Assert.AreEqual("Иванова Ивана Ивановича", rusCulture.GetPossessiveName("Иванов Иван Иванович"));

            GDMIndividualRecord iRec2 = fContext.Tree.CreateIndividual();
            GDMPersonalName persName = new GDMPersonalName();
            persName.ParseString("Иван Иванович /Иванов/");
            iRec2.PersonalNames.Add(persName);

            Assert.AreEqual("Иванова Ивана Ивановича", rusCulture.GetPossessiveName(iRec2));
        }

        [Test]
        public void Test_PolishCulture()
        {
            ICulture culture = new PolishCulture();
            Assert.IsNotNull(culture);
            Assert.IsFalse(culture.HasPatronymic);
            Assert.IsTrue(culture.HasSurname);

            Assert.AreEqual("Kowalski", culture.NormalizeSurname("Kowalska", true));

            Assert.AreEqual("?", culture.NormalizeSurname(null, false));
            Assert.AreEqual("?", culture.NormalizeSurname("", false));
            Assert.AreEqual("?", culture.NormalizeSurname("(Kowalska)", false));
        }
    }
}
