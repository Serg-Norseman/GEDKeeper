// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using GDModel;
using GKTests;
using NUnit.Framework;

namespace GKCore.Names
{
    [TestFixture]
    public class NamesTests
    {
        private readonly BaseContext fContext;

        public NamesTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_NamesTable()
        {
            var namesTable = new NamesTable();
            Assert.IsNotNull(namesTable);

            NameEntry nameEntry = namesTable.AddName("Ivan");
            Assert.IsNotNull(nameEntry);
            Assert.AreEqual("Ivan", nameEntry.Name);

            nameEntry = namesTable.FindName("Ivan");
            Assert.IsNotNull(nameEntry);

            string pat = namesTable.GetPatronymicByName("Ivan", GDMSex.svMale);
            Assert.IsNull(pat);

            string name = namesTable.GetNameByPatronymic("Ivanovich");
            Assert.AreEqual("", name);

            GDMSex sex = namesTable.GetSexByName("Ivan");
            Assert.AreEqual(GDMSex.svUnknown, sex);

            namesTable.SetName("Ivan", "Ivanovich", GDMSex.svMale);
            namesTable.SetName("Ivan", "Ivanovna", GDMSex.svFemale);

            pat = namesTable.GetPatronymicByName("Ivan", GDMSex.svMale);
            Assert.AreEqual("Ivanovich", pat);

            pat = namesTable.GetPatronymicByName("Ivan", GDMSex.svFemale);
            Assert.AreEqual("Ivanovna", pat);

            name = namesTable.GetNameByPatronymic("Ivanovich");
            Assert.AreEqual("Ivan", name);

            name = namesTable.GetNameByPatronymic("Ivanovna");
            Assert.AreEqual("Ivan", name);

            namesTable.SetNameSex("Maria", GDMSex.svFemale);
            sex = namesTable.GetSexByName("Maria");
            Assert.AreEqual(GDMSex.svFemale, sex);

            namesTable.SetName("", "", GDMSex.svUnknown);
            namesTable.SetNameSex("", GDMSex.svUnknown);

            namesTable.SetName("Anna", "Ivanovna", GDMSex.svFemale);
            sex = namesTable.GetSexByName("Anna");
            Assert.AreEqual(GDMSex.svFemale, sex);

            GDMIndividualRecord iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I3");
            Assert.IsNotNull(iRec);
            namesTable.ImportNames(fContext, iRec);

            namesTable.ImportNames(null, null);

            sex = namesTable.GetSexByName("Anna");
            Assert.AreEqual(GDMSex.svFemale, sex);

            string namesFile = TestUtils.GetTempFilePath("names.txt", out _);
            try {
                namesTable.SaveToFile(namesFile);
                namesTable.LoadFromFile(namesFile);
            } finally {
                TestUtils.RemoveTestFile(namesFile);
            }
        }

        [Test]
        public void Test_Names_01()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_names_01.ged")) {
                GDMIndividualRecord iRec1 = ctx.Tree.FindXRef<GDMIndividualRecord>("I1");
                Assert.IsNotNull(iRec1);

                // first stage, NAME reads: surname "Лазорева (Иванова)"
                // second stage, SURN reads: surname "Иванова"
                // subordinate SURN tag overrides surname from NAME tag
                // inappropriate information is lost
                Assert.AreEqual("Анна Мария Анатольевна Иванова", iRec1.GetPrimaryFullName());

                // std-surn exists and double, but sub-surn has only second part
                // sub-givn exists, but sub-patn is not
                var parts = GKUtils.GetNameParts(ctx.Tree, iRec1);
                Assert.AreEqual("Иванова", parts.Surname);
                Assert.AreEqual("Анна Мария", parts.Name);
                Assert.AreEqual("Анатольевна", parts.Patronymic);

                GDMIndividualRecord iRec2 = ctx.Tree.FindXRef<GDMIndividualRecord>("I2");
                Assert.IsNotNull(iRec2);
                Assert.AreEqual("Петр Константинович Лазорев", iRec2.GetPrimaryFullName());
                // std-surn exists, but sub-surn is not
                // sub-givn exists, but sub-patn is not
                parts = GKUtils.GetNameParts(ctx.Tree, iRec2);
                Assert.AreEqual("Лазорев", parts.Surname);
                Assert.AreEqual("Петр", parts.Name);
                Assert.AreEqual("Константинович", parts.Patronymic);
            }
        }

        [Test]
        public void Test_Names_02()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_names_02.ged")) {
                GDMIndividualRecord iRec1 = ctx.Tree.FindXRef<GDMIndividualRecord>("I1");
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("Анна Сидоровна Иванова (Петрова)", iRec1.GetPrimaryFullName());
                // std-surn exists and double, and sub-surn same
                // sub-givn and sub-patn exists
                var parts = GKUtils.GetNameParts(ctx.Tree, iRec1);
                Assert.AreEqual("Иванова (Петрова)", parts.Surname);
                Assert.AreEqual("Анна", parts.Name);
                Assert.AreEqual("Сидоровна", parts.Patronymic);

                GDMIndividualRecord iRec2 = ctx.Tree.FindXRef<GDMIndividualRecord>("I2");
                Assert.IsNotNull(iRec2);
                Assert.AreEqual("Аглая Федоровна Иванова", iRec2.GetPrimaryFullName());
                // std-surn exists (maiden), and sub-surn same, and sub-marn exists (married)
                // sub-givn and sub-patn exists
                parts = GKUtils.GetNameParts(ctx.Tree, iRec2);
                Assert.AreEqual("Иванова", parts.Surname);
                Assert.AreEqual("Лескова", parts.MarriedSurname);
                Assert.AreEqual("Аглая", parts.Name);
                Assert.AreEqual("Федоровна", parts.Patronymic);
            }
        }

        [Test]
        public void Test_Names_03()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_names_03.ged")) {
                GDMIndividualRecord iRec1 = ctx.Tree.FindXRef<GDMIndividualRecord>("I1");
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("MaleName1 MaleName2 MaleSurname", iRec1.GetPrimaryFullName());
                // std-surn exists and double, and sub-surn same
                // sub-givn and sub-patn exists
                var parts = GKUtils.GetNameParts(ctx.Tree, iRec1);
                Assert.AreEqual("MaleSurname", parts.Surname);
                Assert.AreEqual("MaleName1 MaleName2", parts.Name);
            }
        }
    }
}
