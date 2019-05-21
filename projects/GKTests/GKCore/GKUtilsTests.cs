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
using System.IO;
using BSLib;
using GKCommon.GEDCOM;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKUI.Providers;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class GKUtilsTests
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
        public void Test_GetXPath()
        {
            string st = GKUtils.GetTempDir();
            Assert.IsTrue(Directory.Exists(st));

            string appPath = GKUtils.GetAppPath();
            Assert.IsTrue(Directory.Exists(appPath));

            Assert.AreEqual(appPath + "plugins" + Path.DirectorySeparatorChar, GKUtils.GetPluginsPath());
        }

        [Test]
        public void Test_GetXIndex()
        {
            Assert.AreEqual(1, GKUtils.GetPersonEventIndex(GEDCOMTagType.BIRT));
            Assert.AreEqual(2, GKUtils.GetFamilyEventIndex(GEDCOMTagType.MARR));
        }

        [Test]
        public void Test_GetAttributeValue()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string st1 = GKUtils.GetAttributeValue(null, GEDCOMTagType.BIRT);
            Assert.AreEqual("", st1);

            st1 = GKUtils.GetAttributeValue(iRec, GEDCOMTagType.BIRT);
            Assert.AreEqual("", st1);
        }

        [Test]
        public void Test_GEDCOMEventToDateStr()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            GEDCOMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            Assert.IsNotNull(evt);

            string st2 = GKUtils.GEDCOMEventToDateStr(null, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("", st2);

            st2 = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("1990.12.28", st2);

            evt.Cause = "test cause";
            st2 = GKUtils.GetEventCause(evt);
            Assert.AreEqual("test cause", st2);

            string ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
            Assert.AreEqual("28.12.1990", ds);

            ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY_MM_DD, false);
            Assert.AreEqual("1990.12.28", ds);

            ds = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfYYYY, false);
            Assert.AreEqual("1990", ds);

            ds = GKUtils.GEDCOMEventToDateStr(null, DateFormat.dfYYYY, false);
            Assert.AreEqual("", ds);
        }

        [Test]
        public void Test_GetBirthPlace()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string st3 = GKUtils.GetBirthPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
        }

        [Test]
        public void Test_GetDeathPlace()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string st3 = GKUtils.GetDeathPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetDeathPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
        }

        [Test]
        public void Test_GetResidencePlace()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string st3 = GKUtils.GetResidencePlace(null, false);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetResidencePlace(iRec, true);
            Assert.AreEqual("", st3);
        }

        [Test]
        public void Test_GetNameParts()
        {
            string surname = "", name = "", patronymic = "";
            Assert.Throws(typeof(ArgumentNullException), () => { var parts = GKUtils.GetNameParts(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SetNameParts(null, surname, name, patronymic); });
            Assert.Throws(typeof(ArgumentNullException), () => { var parts = GKUtils.GetNameParts(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNameParts(null, null, true); });
        }

        [Test]
        public void Test_GetRecordName()
        {
            GEDCOMRecord rec = fContext.Tree.XRefIndex_Find("I1");
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("F1");
            Assert.AreEqual("Ivanov Ivan Ivanovich - Ivanova Maria Petrovna", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("G1");
            Assert.AreEqual("GroupTest", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("L1");
            Assert.AreEqual("Test Location", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("R1");
            Assert.AreEqual("Test repository", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("RS1");
            Assert.AreEqual("Test research", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("S1");
            Assert.AreEqual("Test source", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("N1");
            Assert.AreEqual("Test note", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("TK1");
            Assert.AreEqual("Test task", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("O1");
            Assert.AreEqual("Test multimedia", GKUtils.GetRecordName(rec, false));

            rec = fContext.Tree.XRefIndex_Find("CM1");
            Assert.AreEqual("Test communication", GKUtils.GetRecordName(rec, false));
        }

        [Test]
        public void Test_PrepareHeader()
        {
            GKUtils.PrepareHeader(fContext.Tree, "c:\\test.ged", GEDCOMCharacterSet.csUTF8, true);
            Assert.AreEqual(0, fContext.Tree.Header.FileRevision);

            GKUtils.PrepareHeader(fContext.Tree, "c:\\test.ged", GEDCOMCharacterSet.csUTF8, false);
            Assert.AreEqual(1, fContext.Tree.Header.FileRevision);
        }

        [Test]
        public void Test_GetAgeStr()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string age = GKUtils.GetAgeStr(null, 0);
            Assert.AreEqual("", age);

            age = GKUtils.GetAgeStr(iRec, -1);
            Assert.AreEqual("20", age);
            
            age = GKUtils.GetAgeStr(iRec, 2005);
            Assert.AreEqual("15", age);
        }

        [Test]
        public void Test_Matches()
        {
            bool res = GKUtils.MatchesMask("abrakadabra", "*kad*");
            Assert.IsTrue(res);

            res = GKUtils.MatchesMask("abrakadabra", "*test*");
            Assert.IsFalse(res);
        }

        [Test]
        public void Test_GetRectUID()
        {
            Assert.AreEqual("0F000F00D700D700CCDC", GKUtils.GetRectUID(15, 15, 215, 215));
        }

        [Test]
        public void Test_GetPedigreeLifeStr()
        {
            Assert.Throws(typeof(ArgumentNullException), () => {
                GKUtils.GetPedigreeLifeStr(null, PedigreeFormat.Compact);
            });

            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            Assert.AreEqual(" (*28.12.1990, Ivanovo +28.12.2010, Ivanovo)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact));
            Assert.AreEqual(" (28.12.1990 - 28.12.2010)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Excess));            

            iRec = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;

            Assert.AreEqual(" (*11.02.2010, Ivanovo)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact));
            Assert.AreEqual(" (11.02.2010)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Excess));            
        }

        [Test]
        public void Test_InitExtData()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.InitExtData(null); });
            GKUtils.InitExtData(fContext.Tree);

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.InitExtCounts(null, 0); });
            GKUtils.InitExtCounts(fContext.Tree, 0);
        }

        [Test]
        public void Test_GetXGoalStr()
        {
            Assert.AreEqual("", GKUtils.GetTaskGoalStr(null));
            Assert.AreEqual("", GKUtils.GetGoalStr(GDMGoalType.gtIndividual, null));

            var rec = fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            Assert.IsNotNull(rec);
            Assert.AreEqual("Test task", GKUtils.GetTaskGoalStr(rec));
        }

        [Test]
        public void Test_GetDaysForBirth()
        {
            Assert.AreEqual(-1, GKUtils.GetDaysForBirth(null));
        }

        [Test]
        public void Test_GetCorresponderStr()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(null, fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(fContext.Tree, null, false); });
        }

        [Test]
        public void Test_GetAncestorsCount()
        {
            GEDCOMIndividualRecord iRec5 = fContext.Tree.XRefIndex_Find("I6") as GEDCOMIndividualRecord;

            GKUtils.InitExtCounts(fContext.Tree, -1);
            Assert.AreEqual(0, GKUtils.GetAncestorsCount(null));
            Assert.AreEqual(3, GKUtils.GetAncestorsCount(iRec5) - 1);
        }

        [Test]
        public void Test_GetDescendantsCount()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            GKUtils.InitExtCounts(fContext.Tree, -1);
            Assert.AreEqual(0, GKUtils.GetDescendantsCount(null));
            Assert.AreEqual(3, GKUtils.GetDescendantsCount(iRec) - 1);
        }

        [Test]
        public void Test_GetDescGenerations()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            Assert.AreEqual(2, GKUtils.GetDescGenerations(iRec));
        }

        [Test]
        public void Test_GetMarriagesCount()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            Assert.AreEqual(1, GKUtils.GetMarriagesCount(iRec));
        }

        [Test]
        public void Test_GetSpousesDiff()
        {
            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;

            Assert.AreEqual(1, GKUtils.GetSpousesDiff(famRec));
        }

        [Test]
        public void Test_GetFirstborn()
        {
            GEDCOMIndividualRecord iRec1 = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GEDCOMIndividualRecord iRec3 = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;

            Assert.AreEqual(iRec3, GKUtils.GetFirstborn(iRec1));
            Assert.AreEqual(20, GKUtils.GetFirstbornAge(iRec1, iRec3));
        }

        [Test]
        public void Test_GetMarriageAge()
        {
            GEDCOMIndividualRecord iRec1 = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            // specially bad date also for CheckBase functions
            Assert.AreEqual(10, GKUtils.GetMarriageAge(iRec1));
        }

        [Test]
        public void Test_GetStoreFolder()
        {
            var sep = Path.DirectorySeparatorChar;
            Assert.AreEqual("unknown" + sep, GKUtils.GetStoreFolder(MultimediaKind.mkNone));
            Assert.AreEqual("images" + sep, GKUtils.GetStoreFolder(MultimediaKind.mkImage));
            Assert.AreEqual("audio" + sep, GKUtils.GetStoreFolder(MultimediaKind.mkAudio));
            Assert.AreEqual("texts" + sep, GKUtils.GetStoreFolder(MultimediaKind.mkText));
            Assert.AreEqual("video" + sep, GKUtils.GetStoreFolder(MultimediaKind.mkVideo));
        }

        [Test]
        public void Test_FileCanBeArchived()
        {
            string sourFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");
            Assert.IsTrue(GKUtils.FileCanBeArchived(sourFile));
        }

        [Test]
        public void Test_X()
        {
        }

        [Test]
        public void Test_CommonX()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventName(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventCause(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventDesc(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetAttributeStr(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null, "", ""); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNickString(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNameString(null, false, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SetMarriedSurname(null, ""); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetStoreType(null); });
        }

        [Test]
        public void Test_GetBirthDateD()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            Assert.IsNull(GKUtils.GetBirthDate(null));

            GEDCOMCustomDate date = GKUtils.GetBirthDate(iRec);
            Assert.IsNotNull(date);
            Assert.AreEqual("28 DEC 1990", date.StringValue);
        }

        [Test]
        public void Test_GetBirthDateS()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string st3;
            st3 = GKUtils.GetBirthDate(null, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("28.12.1990", st3);
        }

        [Test]
        public void Test_GetDeathDate()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            string st3;
            st3 = GKUtils.GetDeathDate(null, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("", st3);
            
            st3 = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
            Assert.AreEqual("28.12.2010", st3);
        }

        [Test]
        public void Test_GetLifeStr()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            Assert.AreEqual("", GKUtils.GetLifeStr(null));

            string st3;
            st3 = GKUtils.GetLifeStr(iRec);
            Assert.AreEqual(" (28.12.1990 - 28.12.2010)", st3);
        }

        [Test]
        public void Test_CompactDate()
        {
            string dst = GKUtils.CompactDate("__.__.2013");
            Assert.AreEqual("2013", dst);
        }

        [Test]
        public void Test_GetSexBySign()
        {
            GEDCOMSex sex;
            sex = GKUtils.GetSexBySign('F');
            Assert.AreEqual(GEDCOMSex.svFemale, sex);
            sex = GKUtils.GetSexBySign('M');
            Assert.AreEqual(GEDCOMSex.svMale, sex);
            sex = GKUtils.GetSexBySign('U');
            Assert.AreEqual(GEDCOMSex.svUndetermined, sex);
        }

        [Test]
        public void Test_HyperLink()
        {
            string st1 = GKUtils.HyperLink("@X001@", "test", 0);
            Assert.AreEqual("[url=" + "@X001@" + "]" + "test" + "[/url]", st1);

            st1 = GKUtils.HyperLink("@X001@", "", 0);
            Assert.AreEqual("[url=" + "@X001@" + "]" + "???" + "[/url]", st1);
        }

        [Test]
        public void Test_GetContainerName()
        {
            #if !__MonoCS__
            Assert.AreEqual("test.zip", GKUtils.GetContainerName("c:\\temp\\test.ged", true)); // archive
            Assert.AreEqual("test\\", GKUtils.GetContainerName("c:\\temp\\test.ged", false)); // storage
            #endif
        }

        [Test]
        public void Test_GetMarriageDate()
        {
            GEDCOMCustomDate dtx = GKUtils.GetMarriageDate(null);
            Assert.IsNull(dtx);
        }

        [Test]
        public void Test_TruncateStrings()
        {
            string test = GKUtils.TruncateStrings(new StringList("sample text for truncate"), 10);
            Assert.AreEqual("sample tex...", test);
        }

        [Test]
        public void Test_GetMultimediaKind()
        {
            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfNone));
            Assert.AreEqual(MultimediaKind.mkImage, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfBMP));
            Assert.AreEqual(MultimediaKind.mkText, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfTXT));
            Assert.AreEqual(MultimediaKind.mkAudio, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfWAV));
            Assert.AreEqual(MultimediaKind.mkVideo, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfAVI));
            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GEDCOMMultimediaFormat.mfOLE));
        }

        [Test]
        public void Test_ShowXInfo()
        {
            StringList summary = new StringList();

            summary.Clear();
            GKUtils.ShowFamilyInfo(fContext, null, null);
            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            GKUtils.ShowFamilyInfo(fContext, famRec, summary);

            summary.Clear();
            GKUtils.ShowGroupInfo(null, null);
            GEDCOMGroupRecord grpRec = fContext.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            GKUtils.ShowGroupInfo(grpRec, summary);

            summary.Clear();
            GKUtils.ShowMultimediaInfo(null, null);
            GEDCOMMultimediaRecord mmRec = fContext.Tree.XRefIndex_Find("O1") as GEDCOMMultimediaRecord;
            GKUtils.ShowMultimediaInfo(mmRec, summary);

            summary.Clear();
            GKUtils.ShowNoteInfo(null, null);
            GEDCOMNoteRecord noteRec = fContext.Tree.XRefIndex_Find("N1") as GEDCOMNoteRecord;
            GKUtils.ShowNoteInfo(noteRec, summary);

            summary.Clear();
            GKUtils.ShowPersonInfo(fContext, null, null);
            GEDCOMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GKUtils.ShowPersonInfo(fContext, indRec, summary);

            summary.Clear();
            GKUtils.ShowSourceInfo(null, null);
            GEDCOMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            GKUtils.ShowSourceInfo(srcRec, summary);

            summary.Clear();
            GKUtils.ShowRepositoryInfo(null, null);
            GEDCOMRepositoryRecord repRec = fContext.Tree.XRefIndex_Find("R1") as GEDCOMRepositoryRecord;
            GKUtils.ShowRepositoryInfo(repRec, summary);

            summary.Clear();
            GKUtils.ShowResearchInfo(null, null);
            GEDCOMResearchRecord resRec = fContext.Tree.XRefIndex_Find("RS1") as GEDCOMResearchRecord;
            GKUtils.ShowResearchInfo(resRec, summary);

            summary.Clear();
            GKUtils.ShowTaskInfo(null, null);
            GEDCOMTaskRecord taskRec = fContext.Tree.XRefIndex_Find("TK1") as GEDCOMTaskRecord;
            GKUtils.ShowTaskInfo(taskRec, summary);

            summary.Clear();
            GKUtils.ShowCommunicationInfo(null, null);
            GEDCOMCommunicationRecord commRec = fContext.Tree.XRefIndex_Find("CM1") as GEDCOMCommunicationRecord;
            GKUtils.ShowCommunicationInfo(commRec, summary);

            summary.Clear();
            GKUtils.ShowLocationInfo(null, null);
            GEDCOMLocationRecord locRec = fContext.Tree.XRefIndex_Find("L1") as GEDCOMLocationRecord;
            GKUtils.ShowLocationInfo(locRec, summary);
        }

        [Test]
        public void Test_GetNameString_ExtendedWomanSurnames()
        {
            // Anna Jones
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I5") as GEDCOMIndividualRecord;

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
            Assert.AreEqual("Jones Anna", GKUtils.GetNameString(iRec, true, false));

            GKUtils.SetMarriedSurname(iRec, "Smith");

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
            Assert.AreEqual("Jones Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMarried;
            Assert.AreEqual("Smith Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden_Married;
            Assert.AreEqual("Jones (Smith) Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfMarried_Maiden;
            Assert.AreEqual("Smith (Jones) Anna", GKUtils.GetNameString(iRec, true, false));

            GlobalOptions.Instance.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
        }

        [Test]
        public void Test_GetNormalizeDate()
        {
            Assert.AreEqual("", GKUtils.GetNormalizeDate("", "mm/dd/yyyy"));
            Assert.AreEqual("30.01.1980", GKUtils.GetNormalizeDate("01/30/1980", "mm/dd/yyyy"));
            Assert.AreEqual("30.01.1980", GKUtils.GetNormalizeDate("1980/01/30", "yyyy/mm/dd"));
        }

        [Test]
        public void Test_GetRegionalDate()
        {
            Assert.AreEqual("", GKUtils.GetRegionalDate("", "mm/dd/yyyy"));
            Assert.AreEqual("01/30/1980", GKUtils.GetRegionalDate("30.01.1980", "mm/dd/yyyy"));
            Assert.AreEqual("1980/01/30", GKUtils.GetRegionalDate("30.01.1980", "yyyy/mm/dd"));
        }
    }
}
