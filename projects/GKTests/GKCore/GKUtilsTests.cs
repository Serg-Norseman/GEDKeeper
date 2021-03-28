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
using System.IO;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKUI;
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
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);
            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
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
            Assert.AreEqual(1, GKUtils.GetPersonEventIndex(GEDCOMTagName.BIRT));
            Assert.AreEqual(2, GKUtils.GetFamilyEventIndex(GEDCOMTagName.MARR));
        }

        [Test]
        public void Test_GetAttributeValue()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            string st1 = GKUtils.GetAttributeValue(null, GEDCOMTagName.BIRT);
            Assert.AreEqual("", st1);

            st1 = GKUtils.GetAttributeValue(iRec, GEDCOMTagName.BIRT);
            Assert.AreEqual("", st1);
        }

        [Test]
        public void Test_GEDCOMEventToDateStr()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.BIRT);
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
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            string st3 = GKUtils.GetBirthPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetBirthPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
        }

        [Test]
        public void Test_GetDeathPlace()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            string st3 = GKUtils.GetDeathPlace(null);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetDeathPlace(iRec);
            Assert.AreEqual("Ivanovo", st3);
        }

        [Test]
        public void Test_GetResidencePlace()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            string st3 = GKUtils.GetResidencePlace(null, false);
            Assert.AreEqual("", st3);

            st3 = GKUtils.GetResidencePlace(iRec, true);
            Assert.AreEqual("", st3);
        }

        [Test]
        public void Test_GetNameParts()
        {
            string surname = "", name = "", patronymic = "";
            Assert.Throws(typeof(ArgumentNullException), () => { var parts = GKUtils.GetNameParts(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SetNameParts(null, surname, name, patronymic); });
            Assert.Throws(typeof(ArgumentNullException), () => { var parts = GKUtils.GetNameParts(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNameParts(null, null, true); });
        }

        [Test]
        public void Test_GetRecordName()
        {
            GDMRecord rec = fContext.Tree.XRefIndex_Find("I1");
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("F1");
            Assert.AreEqual("Ivanov Ivan Ivanovich - Ivanova Maria Petrovna", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("G1");
            Assert.AreEqual("GroupTest", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("L1");
            Assert.AreEqual("Test Location", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("R1");
            Assert.AreEqual("Test repository", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("RS1");
            Assert.AreEqual("Test research", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("S1");
            Assert.AreEqual("Test source", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("N1");
            Assert.AreEqual("Test note", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("TK1");
            Assert.AreEqual("Test task", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("O1");
            Assert.AreEqual("Test multimedia", GKUtils.GetRecordName(fContext.Tree, rec, false));

            rec = fContext.Tree.XRefIndex_Find("CM1");
            Assert.AreEqual("Test communication", GKUtils.GetRecordName(fContext.Tree, rec, false));
        }

        [Test]
        public void Test_PrepareHeader()
        {
            GKUtils.PrepareHeader(fContext.Tree, "c:\\test.ged", GEDCOMCharacterSet.csUTF8, true);
            Assert.AreEqual(0, fContext.Tree.Header.File.Revision);

            GKUtils.PrepareHeader(fContext.Tree, "c:\\test.ged", GEDCOMCharacterSet.csUTF8, false);
            Assert.AreEqual(1, fContext.Tree.Header.File.Revision);
        }

        [Test]
        public void Test_GetAgeStr()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

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

            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual(" (*28.12.1990, Ivanovo +28.12.2010, Ivanovo)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact));
            Assert.AreEqual(" (28.12.1990 - 28.12.2010)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Excess));            

            iRec = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;

            Assert.AreEqual(" (*11.02.2010, Ivanovo)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact));
            Assert.AreEqual(" (11.02.2010)", GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Excess));            
        }

        [Test]
        public void Test_GetXGoalStr()
        {
            Assert.AreEqual("", GKUtils.GetTaskGoalStr(null, null));
            Assert.AreEqual("", GKUtils.GetGoalStr(fContext.Tree, GDMGoalType.gtIndividual, null));

            var rec = fContext.Tree.XRefIndex_Find("TK1") as GDMTaskRecord;
            Assert.IsNotNull(rec);
            Assert.AreEqual("Test task", GKUtils.GetTaskGoalStr(fContext.Tree, rec));
        }

        [Test]
        public void Test_GetDaysForBirth()
        {
            Assert.AreEqual(-1, GKUtils.GetDaysForBirth(null));
        }

        [Test]
        public void Test_GetCorresponderStr()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(null, fContext.Tree.XRefIndex_Find("CM1") as GDMCommunicationRecord, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetCorresponderStr(fContext.Tree, null, false); });
        }

        [Test]
        public void Test_GetAncestorsCount()
        {
            GDMIndividualRecord iRec5 = fContext.Tree.XRefIndex_Find("I6") as GDMIndividualRecord;

            Assert.AreEqual(0, GKUtils.GetAncestorsCount(fContext.Tree, null));
            Assert.AreEqual(3, GKUtils.GetAncestorsCount(fContext.Tree, iRec5) - 1);
        }

        [Test]
        public void Test_GetDescendantsCount()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual(0, GKUtils.GetDescendantsCount(fContext.Tree, null));
            Assert.AreEqual(3, GKUtils.GetDescendantsCount(fContext.Tree, iRec) - 1);
        }

        [Test]
        public void Test_GetDescGenerations()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual(2, GKUtils.GetDescGenerations(fContext.Tree, iRec));
        }

        [Test]
        public void Test_GetMarriagesCount()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual(1, GKUtils.GetMarriagesCount(iRec));
        }

        [Test]
        public void Test_GetSpousesDiff()
        {
            GDMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GDMFamilyRecord;

            Assert.AreEqual(1, GKUtils.GetSpousesDiff(fContext.Tree, famRec));
        }

        [Test]
        public void Test_GetFirstborn()
        {
            GDMIndividualRecord iRec1 = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            GDMIndividualRecord iRec3 = fContext.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;

            Assert.AreEqual(iRec3, GKUtils.GetFirstborn(fContext.Tree, iRec1));
            Assert.AreEqual(20, GKUtils.GetFirstbornAge(iRec1, iRec3));
        }

        [Test]
        public void Test_GetMarriageAge()
        {
            GDMIndividualRecord iRec1 = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            // specially bad date also for CheckBase functions
            Assert.AreEqual(10, GKUtils.GetMarriageAge(fContext.Tree, iRec1));
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
            try {
                Assert.IsTrue(GKUtils.FileCanBeArchived(sourFile));
            } finally {
                TestUtils.RemoveTestFile(sourFile);
            }
        }

        [Test]
        public void Test_CommonX()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventName(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventCause(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetEventDesc(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetAttributeStr(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetFamilyString(null, null, "", ""); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNickString(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetNameString(null, false, false); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.SetMarriedSurname(null, ""); });
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.GetStoreType(null); });
        }

        [Test]
        public void Test_GetBirthDateD()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.IsNull(GKUtils.GetBirthDate(null));

            GDMCustomDate date = GKUtils.GetBirthDate(iRec);
            Assert.IsNotNull(date);
            Assert.AreEqual("28 DEC 1990", date.StringValue);
        }

        [Test]
        public void Test_GetBirthDateS()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual("", GKUtils.GetBirthDate(null, DateFormat.dfDD_MM_YYYY, true));

            Assert.AreEqual("28.12.1990", GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true));
        }

        [Test]
        public void Test_GetDeathDate()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual("", GKUtils.GetDeathDate(null, DateFormat.dfDD_MM_YYYY, true));
            
            Assert.AreEqual("28.12.2010", GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true));
        }

        [Test]
        public void Test_GetLifeStr()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

            Assert.AreEqual("", GKUtils.GetLifeStr(null));

            Assert.AreEqual(" (28.12.1990 - 28.12.2010)", GKUtils.GetLifeStr(iRec));
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
            Assert.AreEqual(GDMSex.svFemale, GKUtils.GetSexBySign('F'));
            Assert.AreEqual(GDMSex.svMale, GKUtils.GetSexBySign('M'));
            Assert.AreEqual(GDMSex.svUnknown, GKUtils.GetSexBySign('U'));
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
            GDMCustomDate dtx = GKUtils.GetMarriageDate(null);
            Assert.IsNull(dtx);
        }

        [Test]
        public void Test_TruncateStrings()
        {
            string test = GKUtils.TruncateStrings(new GDMLines("sample text for truncate"), 10);
            Assert.AreEqual("sample tex...", test);
        }

        [Test]
        public void Test_GetMultimediaKind()
        {
            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GDMMultimediaFormat.mfNone));
            Assert.AreEqual(MultimediaKind.mkImage, GKUtils.GetMultimediaKind(GDMMultimediaFormat.mfBMP));
            Assert.AreEqual(MultimediaKind.mkText, GKUtils.GetMultimediaKind(GDMMultimediaFormat.mfTXT));
            Assert.AreEqual(MultimediaKind.mkAudio, GKUtils.GetMultimediaKind(GDMMultimediaFormat.mfWAV));
            Assert.AreEqual(MultimediaKind.mkVideo, GKUtils.GetMultimediaKind(GDMMultimediaFormat.mfAVI));
            Assert.AreEqual(MultimediaKind.mkNone, GKUtils.GetMultimediaKind(GDMMultimediaFormat.mfOLE));
        }

        [Test]
        public void Test_ShowXInfo()
        {
            StringList summary = new StringList();

            summary.Clear();
            GKUtils.ShowFamilyInfo(fContext, null, null);
            GDMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GDMFamilyRecord;
            GKUtils.ShowFamilyInfo(fContext, famRec, summary);

            summary.Clear();
            GKUtils.ShowGroupInfo(null, null, null);
            GDMGroupRecord grpRec = fContext.Tree.XRefIndex_Find("G1") as GDMGroupRecord;
            GKUtils.ShowGroupInfo(fContext, grpRec, summary);

            summary.Clear();
            GKUtils.ShowMultimediaInfo(null, null, null);
            GDMMultimediaRecord mmRec = fContext.Tree.XRefIndex_Find("O1") as GDMMultimediaRecord;
            GKUtils.ShowMultimediaInfo(fContext, mmRec, summary);

            summary.Clear();
            GKUtils.ShowNoteInfo(null, null, null);
            GDMNoteRecord noteRec = fContext.Tree.XRefIndex_Find("N1") as GDMNoteRecord;
            GKUtils.ShowNoteInfo(fContext, noteRec, summary);

            summary.Clear();
            GKUtils.ShowPersonInfo(fContext, null, null);
            GDMIndividualRecord indRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            GKUtils.ShowPersonInfo(fContext, indRec, summary);

            summary.Clear();
            GKUtils.ShowSourceInfo(null, null, null);
            GDMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GDMSourceRecord;
            GKUtils.ShowSourceInfo(fContext, srcRec, summary);

            summary.Clear();
            GKUtils.ShowRepositoryInfo(null, null, null);
            GDMRepositoryRecord repRec = fContext.Tree.XRefIndex_Find("R1") as GDMRepositoryRecord;
            GKUtils.ShowRepositoryInfo(fContext, repRec, summary);

            summary.Clear();
            GKUtils.ShowResearchInfo(null, null, null);
            GDMResearchRecord resRec = fContext.Tree.XRefIndex_Find("RS1") as GDMResearchRecord;
            GKUtils.ShowResearchInfo(fContext, resRec, summary);

            summary.Clear();
            GKUtils.ShowTaskInfo(null, null, null);
            GDMTaskRecord taskRec = fContext.Tree.XRefIndex_Find("TK1") as GDMTaskRecord;
            GKUtils.ShowTaskInfo(fContext, taskRec, summary);

            summary.Clear();
            GKUtils.ShowCommunicationInfo(null, null, null);
            GDMCommunicationRecord commRec = fContext.Tree.XRefIndex_Find("CM1") as GDMCommunicationRecord;
            GKUtils.ShowCommunicationInfo(fContext, commRec, summary);

            summary.Clear();
            GKUtils.ShowLocationInfo(null, null, null);
            GDMLocationRecord locRec = fContext.Tree.XRefIndex_Find("L1") as GDMLocationRecord;
            GKUtils.ShowLocationInfo(fContext, locRec, summary);
        }

        [Test]
        public void Test_GetNameString_ExtendedWomanSurnames()
        {
            // Anna Jones
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I5") as GDMIndividualRecord;

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

        [Test]
        public void Test_GetRecordContent()
        {
            GKUtils.GetRecordContent(null, null, null);
        }
    }
}
