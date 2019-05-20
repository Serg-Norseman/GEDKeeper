/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Text;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using NUnit.Framework;

namespace GKTests
{
    public static class TestUtils
    {
        public static BaseContext CreateContext(/*IBaseWindow baseWin = null*/)
        {
            BaseContext context = new BaseContext(/*baseWin*/null);
            Assert.IsNotNull(context);
            Assert.IsNotNull(context.Tree);
            return context;
        }

        public static void FillContext(IBaseContext context)
        {
            // a null result if the record is not defined
            GEDCOMCustomEvent evt = context.CreateEventEx(null, GEDCOMTagType.BIRT, "xxxxx", "xxxxx");
            Assert.IsNull(evt);

            // first individual (I1)
            GEDCOMIndividualRecord iRec = context.CreatePersonEx("Ivan", "Ivanovich", "Ivanov", GEDCOMSex.svMale, true);
            Assert.IsNotNull(iRec);

            evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("28 DEC 1990");
            evt.Place.StringValue = "Ivanovo";

            GEDCOMCustomEvent evtd = context.CreateEventEx(iRec, GEDCOMTagType.DEAT, "28 DEC 2010", "Ivanovo");
            Assert.IsNotNull(evtd);

            // second individual, wife (I2)
            GEDCOMIndividualRecord iRec2 = context.CreatePersonEx("Maria", "Petrovna", "Ivanova", GEDCOMSex.svFemale, true);
            evt = iRec2.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("17 MAR 1991");
            evt.Place.StringValue = "Ivanovo";

            iRec.AddAssociation("spouse", iRec2);

            // third individual, child (I3)
            GEDCOMIndividualRecord iRec3 = context.CreatePersonEx("Anna", "Ivanovna", "Ivanova", GEDCOMSex.svFemale, true);
            evt = iRec3.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("11 FEB 2010");
            evt.Place.StringValue = "Ivanovo";

            // their family
            GEDCOMFamilyRecord famRec = context.Tree.CreateFamily();
            Assert.IsNotNull(famRec);
            famRec.AddSpouse(iRec);
            famRec.AddSpouse(iRec2);
            famRec.AddChild(iRec3);

            context.CreateEventEx(famRec, GEDCOMTagType.MARR, "01 JAN 2000", "unknown");

            // individual outside the family (I4)
            GEDCOMIndividualRecord iRec4 = context.CreatePersonEx("Alex", "", "Petrov", GEDCOMSex.svMale, true);
            evt = iRec4.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);
            evt.Date.ParseString("15 JUN 1989");
            evt.Place.StringValue = "Far Forest";

            evt = context.CreateEventEx(iRec4, GEDCOMTagType.RESI, "12 FEB", "Far Forest");
            Assert.IsNotNull(evt);

            // fifth (I5)
            GEDCOMIndividualRecord iRec5 = context.CreatePersonEx("Anna", "", "Jones", GEDCOMSex.svFemale, false);
            Assert.IsNotNull(iRec5);

            // sixth (I6)
            GEDCOMIndividualRecord iRec6 = context.CreatePersonEx("Mary", "", "Jones", GEDCOMSex.svFemale, false);
            Assert.IsNotNull(iRec6);
            evt = context.CreateEventEx(iRec6, GEDCOMTagType.BIRT, "12 FEB 1650", "Far Forest");

            GEDCOMFamilyRecord famRec2 = context.Tree.CreateFamily();
            Assert.IsNotNull(famRec2);
            famRec2.AddSpouse(iRec3);
            //famRec2.AddSpouse(iRec4);
            famRec2.AddChild(iRec5);
            famRec2.AddChild(iRec6);

            // group for tests
            GEDCOMGroupRecord groupRec = context.Tree.CreateGroup();
            groupRec.GroupName = "GroupTest";
            Assert.IsNotNull(groupRec, "group1 != null");
            groupRec.AddMember(iRec);

            // location for tests
            GEDCOMLocationRecord locRec = context.Tree.CreateLocation();
            locRec.LocationName = "Test Location";
            locRec.Map.Lati = 5.11111;
            locRec.Map.Long = 7.99999;
            Assert.IsNotNull(locRec, "locRec != null");

            // repository for tests
            GEDCOMRepositoryRecord repoRec = context.Tree.CreateRepository();
            repoRec.RepositoryName = "Test repository";
            Assert.IsNotNull(repoRec, "repoRec != null");

            // research for tests
            GEDCOMResearchRecord resRec = context.Tree.CreateResearch();
            resRec.ResearchName = "Test research";
            Assert.IsNotNull(resRec, "resRec != null");

            // source for tests
            GEDCOMSourceRecord srcRec = context.Tree.CreateSource();
            srcRec.ShortTitle = "Test source";
            Assert.IsNotNull(srcRec, "srcRec != null");
            iRec.AddSource(srcRec, "p1", 0);

            // note for tests
            GEDCOMNoteRecord noteRec = context.Tree.CreateNote();
            noteRec.SetNoteText("Test note");
            Assert.IsNotNull(noteRec, "noteRec != null");
            iRec.AddNote(noteRec);

            // task for tests
            GEDCOMTaskRecord tskRec = context.Tree.CreateTask();
            tskRec.Goal = "Test task";
            Assert.IsNotNull(tskRec, "tskRec != null");

            // media for tests
            GEDCOMMultimediaRecord mediaRec = context.Tree.CreateMultimedia();
            mediaRec.FileReferences.Add(new GEDCOMFileReferenceWithTitle(mediaRec, GEDCOMTagType.FILE, ""));
            GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];

            fileRef.Title = "Test multimedia";
            fileRef.LinkFile("sample.png");
            Assert.IsNotNull(mediaRec, "mediaRec != null");
            iRec.AddMultimedia(mediaRec);

            // communication for tests
            GEDCOMCommunicationRecord commRec = context.Tree.CreateCommunication();
            commRec.CommName = "Test communication";
            Assert.IsNotNull(commRec, "commRec != null");
        }

        public static string GetTagStreamText(GEDCOMTag tag, int level)
        {
            string result;
            using (MemoryStream stm = new MemoryStream()) {
                using (StreamWriter fs = new StreamWriter(stm)) {
                    tag.SaveToStream(fs, level);

                    fs.Flush();

                    result = Encoding.ASCII.GetString(stm.ToArray());
                }
            }
            return result;
        }

        public static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "dd.MM.yyyy", CultureInfo.InvariantCulture);
        }

        public static string GetTempFilePath(string fileName)
        {
            #if !__MonoCS__
            fileName = GKUtils.GetTempDir() + fileName;
            #else
            fileName = GKUtils.GetHomePath() + fileName;
            #endif

            if (File.Exists(fileName)) File.Delete(fileName); // for local tests!

            return fileName;
        }

        public static string PrepareTestFile(string resName)
        {
            string fileName = GetTempFilePath(resName);

            Assembly assembly = typeof(CoreTests).Assembly;
            using (Stream inStream = assembly.GetManifestResourceStream("GKTests.Resources." + resName)) {
                long size = inStream.Length;
                byte[] buffer = new byte[size];
                int res = inStream.Read(buffer, 0, (int)size);
                File.WriteAllBytes(fileName, buffer);
            }

            return fileName;
        }

        public static IBaseContext LoadResourceGEDCOMFile(string resName)
        {
            BaseContext ctx = new BaseContext(null);

            Assembly assembly = typeof(CoreTests).Assembly;
            using (Stream stream = assembly.GetManifestResourceStream("GKTests.Resources." + resName)) {
                var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                gedcomProvider.LoadFromStreamExt(stream, stream);
            }

            return ctx;
        }
    }
}
