/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using System.IO;
using System.Reflection;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKTests
{
    public static class TestUtils
    {
        public static void InitGEDCOMProviderTest()
        {
#if NETCOREAPP3_1_OR_GREATER
            // support for legacy encodings
            System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance);
#endif

            // forced initialization of GEDCOM tag property tables
            GKUtils.InitGEDCOM();

            // checking initialization of GEDCOM tag properties
            var tagProps = GEDCOMTagsTable.GetTagProps("BIRT");
            Assert.IsNotNull(tagProps);
            Assert.AreEqual((int)GEDCOMTagType.BIRT, tagProps.TagId);
        }

        public static void InitUITest()
        {
            InitGEDCOMProviderTest();
            AppHost.TEST_MODE = true;

            // GlobalOptions -> IGraphicsProviderEx
            AppHostStub.ConfigureBootstrap();

            LangMan.DefInit();

            AppHost.Container.Register<IProgressDialog, ProgressStub>(GKCore.IoC.LifeCycle.Singleton, true);
        }

        public static BaseContext CreateContext(IBaseWindow baseWin = null)
        {
            BaseContext context = new BaseContext(baseWin);
            Assert.IsNotNull(context);
            Assert.IsNotNull(context.Tree);
            return context;
        }

        /// <summary>
        /// Generating a basic set of test data.
        /// </summary>
        public static void FillContext(IBaseContext context)
        {
            // a null result if the record is not defined
            GDMCustomEvent evt = context.CreateEventEx(null, GEDCOMTagName.BIRT, "xxxxx", "xxxxx");
            Assert.IsNull(evt);

            // first individual (I1)
            GDMIndividualRecord iRec = context.CreatePersonEx("Ivan", "Ivanovich", "Ivanov", GDMSex.svMale, true);
            Assert.IsNotNull(iRec);

            evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("28 DEC 1990"); // birth of i1
            evt.Place.StringValue = "Ivanovo";

            GDMCustomEvent evtd = context.CreateEventEx(iRec, GEDCOMTagName.DEAT, "28 DEC 2010", "Ivanovo");
            Assert.IsNotNull(evtd);

            // second individual, wife (I2)
            GDMIndividualRecord iRec2 = context.CreatePersonEx("Maria", "Petrovna", "Ivanova", GDMSex.svFemale, true);
            evt = iRec2.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("17 MAR 1991"); // birth of i2
            evt.Place.StringValue = "Ivanovo";

            iRec.AddAssociation("spouse", iRec2);

            // third individual, child (I3)
            GDMIndividualRecord iRec3 = context.CreatePersonEx("Anna", "Ivanovna", "Ivanova", GDMSex.svFemale, true);
            evt = iRec3.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("11 FEB 2010");
            evt.Place.StringValue = "Ivanovo";

            // their family
            GDMFamilyRecord famRec = context.Tree.CreateFamily();
            Assert.IsNotNull(famRec);
            famRec.AddSpouse(iRec);
            famRec.AddSpouse(iRec2);
            famRec.AddChild(iRec3);
            // diff of spouses' ages < 3 monthes

            context.CreateEventEx(famRec, GEDCOMTagName.MARR, "01 JAN 2000", "unknown");

            // individual outside the family (I4)
            GDMIndividualRecord iRec4 = context.CreatePersonEx("Alex", "", "Petrov", GDMSex.svMale, true);
            evt = iRec4.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("15 JUN 1989");
            evt.Place.StringValue = "Far Forest";

            evt = context.CreateEventEx(iRec4, GEDCOMTagName.RESI, "12 FEB", "Far Forest");
            Assert.IsNotNull(evt);

            // fifth (I5)
            GDMIndividualRecord iRec5 = context.CreatePersonEx("Anna", "", "Jones", GDMSex.svFemale, false);
            Assert.IsNotNull(iRec5);

            // sixth (I6)
            GDMIndividualRecord iRec6 = context.CreatePersonEx("Mary", "", "Jones", GDMSex.svFemale, false);
            Assert.IsNotNull(iRec6);
            evt = context.CreateEventEx(iRec6, GEDCOMTagName.BIRT, "12 FEB 1650", "Far Forest");

            GDMFamilyRecord famRec2 = context.Tree.CreateFamily();
            Assert.IsNotNull(famRec2);
            famRec2.AddSpouse(iRec3);
            //famRec2.AddSpouse(iRec4);
            famRec2.AddChild(iRec5);
            famRec2.AddChild(iRec6);

            // group for tests
            GDMGroupRecord groupRec = context.Tree.CreateGroup();
            groupRec.GroupName = "GroupTest";
            Assert.IsNotNull(groupRec, "group1 != null");
            groupRec.AddMember(iRec);

            // location for tests
            GDMLocationRecord locRec = context.Tree.CreateLocation();
            locRec.LocationName = "Test Location";
            locRec.Map.Lati = 5.11111;
            locRec.Map.Long = 7.99999;
            Assert.IsNotNull(locRec, "locRec != null");

            // repository for tests
            GDMRepositoryRecord repoRec = context.Tree.CreateRepository();
            repoRec.RepositoryName = "Test repository";
            Assert.IsNotNull(repoRec, "repoRec != null");

            // research for tests
            GDMResearchRecord resRec = context.Tree.CreateResearch();
            resRec.ResearchName = "Test research";
            Assert.IsNotNull(resRec, "resRec != null");

            // source for tests
            GDMSourceRecord srcRec = context.Tree.CreateSource();
            srcRec.ShortTitle = "Test source";
            Assert.IsNotNull(srcRec, "srcRec != null");
            iRec.AddSource(srcRec, "p1", 0);

            // note for tests
            GDMNoteRecord noteRec = context.Tree.CreateNote();
            noteRec.SetNoteText("Test note");
            Assert.IsNotNull(noteRec, "noteRec != null");
            iRec.AddNote(noteRec);

            // task for tests
            GDMTaskRecord tskRec = context.Tree.CreateTask();
            tskRec.Goal = "Test task";
            Assert.IsNotNull(tskRec, "tskRec != null");

            // media for tests
            GDMMultimediaRecord mediaRec = context.Tree.CreateMultimedia();
            mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
            GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];

            fileRef.Title = "Test multimedia";
            fileRef.LinkFile("sample.png");
            Assert.IsNotNull(mediaRec, "mediaRec != null");
            iRec.AddMultimedia(mediaRec);

            // communication for tests
            GDMCommunicationRecord commRec = context.Tree.CreateCommunication();
            commRec.CommName = "Test communication";
            Assert.IsNotNull(commRec, "commRec != null");
        }

        public static GDMDate ParseGDT(string dtx)
        {
            GDMDate result = new GDMDate();
            result.ParseString(dtx);
            return result;
        }

        public static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "dd.MM.yyyy", CultureInfo.InvariantCulture);
        }

        public static DateTime ParseDTX(string dtx)
        {
            return DateTime.ParseExact(dtx, "dd.MM.yyyy HH:mm", CultureInfo.InvariantCulture);
        }

        public static string GetTempFilePath(string fileName)
        {
            fileName = GKUtils.GetTempDir() + fileName;

            if (File.Exists(fileName)) File.Delete(fileName); // for local tests!

            return fileName;
        }

        public static string PrepareTestFile(string resName)
        {
            string fileName = GetTempFilePath(resName);

            using (Stream inStream = TestUtils.LoadResourceStream(resName)) {
                int size = (int)inStream.Length;
                byte[] buffer = new byte[size];
                int res = inStream.Read(buffer, 0, size);
                if (res == size) {
                    File.WriteAllBytes(fileName, buffer);
                } else {
                    throw new Exception("Resource stream not fully read");
                }
            }

            return fileName;
        }

        public static void RemoveTestFile(string fileName)
        {
            try {
                File.Delete(fileName);
            } catch (Exception) {
            }
        }

        public static void RemoveTestDirectory(string dirName)
        {
            try {
                Directory.Delete(dirName, true);
            } catch (Exception) {
            }
        }

        public static Stream LoadResourceStream(string resName)
        {
            Assembly assembly = typeof(CoreTests).Assembly;
            return assembly.GetManifestResourceStream("GKTests.Resources." + resName);
        }

        public static IBaseContext LoadResourceGEDCOMFile(string resName)
        {
            BaseContext ctx = new BaseContext(null);

            using (Stream stream = TestUtils.LoadResourceStream(resName)) {
                var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                gedcomProvider.LoadFromStreamExt(stream, stream);
            }

            return ctx;
        }

        /// <summary>
        /// Support function. Parse GEDCOM string.
        /// </summary>
        public static GDMIndividualRecord ParseIndiRec(string text)
        {
            GDMTree tree = new GDMTree();
            GEDCOMProvider gp = new GEDCOMProvider(tree);
            try {
                gp.LoadFromString(text);
            } catch (Exception) {
            }
            Assert.AreEqual(1, tree.RecordsCount);
            GDMIndividualRecord rec = tree[0] as GDMIndividualRecord;
            Assert.IsNotNull(rec);
            return rec;
        }
    }
}
