/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Text;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using NUnit.Framework;

namespace GKTests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class GedcomTests
    {
        BaseContext _context;

        [TestFixtureSetUp]
        public void SetUp()
        {
            _context = TestStubs.CreateContext();
            GEDCOMTree tree = _context.Tree;
            
            TestStubs.FillContext(_context);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        #region True Tests

        [Test]
        public void GEDCOMAux_Tests()
        {
            GEDCOMIndividualRecord iRec = _context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            //

            GEDCOMCustomEvent evt, evtd;

            evt = iRec.FindEvent("BIRT");
            Assert.IsNotNull(evt);

            evtd = iRec.FindEvent("DEAT");
            Assert.IsNotNull(evtd);

            GEDCOMCustomEventTest(evt, "28.12.1990");

            Assert.IsNotNull(evt.Detail.Address);
        }

        [Test]
        public void GEDCOMUtils_Tests()
        {
            GEDCOMObject obj = new GEDCOMObject();
            obj.ExtData = this;
            Assert.AreEqual(obj.ExtData, this);
            obj.Dispose();

            //

            string str;
            str = GEDCOMUtils.TrimLeft("	test1");
            Assert.AreEqual("test1", str);

            str = GEDCOMUtils.TrimLeft(null);
            Assert.AreEqual("", str);

            str = GEDCOMUtils.TrimRight("test2		");
            Assert.AreEqual("test2", str);

            str = GEDCOMUtils.TrimRight(null);
            Assert.AreEqual("", str);
            
            //

            string gcStr;

            Assert.AreEqual("", GEDCOMUtils.StrToGEDCOMDate(null, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMUtils.StrToGEDCOMDate("20/12/1980", false));

            gcStr = GEDCOMUtils.StrToGEDCOMDate("__/12/1980", false);
            Assert.AreEqual("DEC 1980", gcStr);
            
            Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMUtils.StrToGEDCOMDate("1980", true); });
            
            gcStr = GEDCOMUtils.StrToGEDCOMDate("1980", false);
            Assert.AreEqual("", gcStr);
            
            // other
            string st = "ivan";
            st = GEDCOMUtils.NormalizeName(st);
            Assert.AreEqual("Ivan", st);
            
            st = GEDCOMUtils.NormalizeName(null);
            Assert.AreEqual("", st);

            //

            GEDCOMUtils.TagProperties props = GEDCOMUtils.GetTagProps("ADDR");
            Assert.IsNotNull(props);
            Assert.IsTrue(props.EmptySkip);

            props = GEDCOMUtils.GetTagProps("test");
            Assert.IsNull(props);

            //

            Assert.AreEqual("I12", GEDCOMUtils.CleanXRef("@I12@"), "CleanXRef(@I12@)");
            Assert.AreEqual("@I12@", GEDCOMUtils.EncloseXRef("I12"), "EncloseXRef(I12)");

            //
            string s1 = "abcd 12345 efgh";
            string s2;
            s2 = GEDCOMUtils.ExtractString(s1, out st, "");
            Assert.AreEqual("abcd", st);
            Assert.AreEqual(" 12345 efgh", s2);

            s2 = GEDCOMUtils.ExtractDelimiter(s2, 0);
            Assert.AreEqual("12345 efgh", s2);

            //

            string s3 = GEDCOMUtils.ExtractDelimiter("    abrvalg", 2);
            Assert.AreEqual("  abrvalg", s3);

            s3 = GEDCOMUtils.ExtractDotDelimiter("....abrvalg", 2);
            Assert.AreEqual("..abrvalg", s3);

            //

            s3 = GEDCOMUtils.ExtractString("  abrvalg", out st, "test");
            Assert.AreEqual("test", st);
            Assert.AreEqual("  abrvalg", s3);

            s3 = GEDCOMUtils.ExtractString("", out st, "test");
            Assert.AreEqual("test", st);
            Assert.AreEqual("", s3);

            //

            int N;
            s2 = GEDCOMUtils.ExtractNumber(s2, out N, true, 0);
            Assert.AreEqual(" efgh", s2);
            Assert.AreEqual(12345, N);

            s2 = GEDCOMUtils.ExtractNumber("x12345", out N, true, 54321);
            Assert.AreEqual("x12345", s2);
            Assert.AreEqual(54321, N);

            s2 = GEDCOMUtils.ExtractNumber("", out N, true, 1111);
            Assert.AreEqual("", s2);
            Assert.AreEqual(1111, N);

            Assert.Throws(typeof(EGEDCOMException), () => { GEDCOMUtils.ExtractNumber("num", out N, false, 2222); });

            //

            string xref;
            s2 = GEDCOMUtils.ExtractXRef("@I101@ sample", out xref, true, "");
            Assert.AreEqual(" sample", s2);
            Assert.AreEqual("I101", xref);

            s2 = GEDCOMUtils.ExtractXRef("", out xref, true, "test");
            Assert.AreEqual("", s2);
            Assert.AreEqual("test", xref);

            s2 = GEDCOMUtils.ExtractXRef("@sample", out xref, true, "test");
            Assert.AreEqual("@sample", s2);
            Assert.AreEqual("test", xref);

            Assert.Throws(typeof(EGEDCOMException), () => { GEDCOMUtils.ExtractXRef("", out xref, false, "test"); });

            Assert.Throws(typeof(EGEDCOMException), () => { GEDCOMUtils.ExtractXRef("@sample", out xref, false, "test"); });

            //

            Assert.IsFalse(GEDCOMUtils.IsDigit('F'), "IsDigit(F)");
            Assert.IsTrue(GEDCOMUtils.IsDigit('9'), "IsDigit(9)");

            Assert.IsFalse(GEDCOMUtils.IsDigits("f09"), "IsDigits(f09)");
            Assert.IsTrue(GEDCOMUtils.IsDigits("99"), "IsDigits(99)");

            Assert.AreEqual("M", GEDCOMUtils.GetSexStr(GEDCOMSex.svMale), "GetSexStr(svMale)");
            Assert.AreEqual("F", GEDCOMUtils.GetSexStr(GEDCOMSex.svFemale), "GetSexStr(svFemale)");
            Assert.AreEqual("U", GEDCOMUtils.GetSexStr(GEDCOMSex.svUndetermined), "GetSexStr(svUndetermined)");
            Assert.AreEqual("", GEDCOMUtils.GetSexStr(GEDCOMSex.svNone), "GetSexStr(svNone)");

            Assert.AreEqual(GEDCOMSex.svMale, GEDCOMUtils.GetSexVal("M"), "GetSexVal(svMale)");
            Assert.AreEqual(GEDCOMSex.svFemale, GEDCOMUtils.GetSexVal("F"), "GetSexVal(svFemale)");
            Assert.AreEqual(GEDCOMSex.svUndetermined, GEDCOMUtils.GetSexVal("U"), "GetSexVal(svUndetermined)");
            Assert.AreEqual(GEDCOMSex.svNone, GEDCOMUtils.GetSexVal(""), "GetSexVal(svNone)");
            Assert.AreEqual(GEDCOMSex.svNone, GEDCOMUtils.GetSexVal("unk"), "GetSexVal(unk)");

            Assert.AreEqual("ASCII", GEDCOMUtils.GetCharacterSetStr(GEDCOMCharacterSet.csASCII));
            Assert.AreEqual("ANSEL", GEDCOMUtils.GetCharacterSetStr(GEDCOMCharacterSet.csANSEL));
            Assert.AreEqual("UNICODE", GEDCOMUtils.GetCharacterSetStr(GEDCOMCharacterSet.csUNICODE));
            Assert.AreEqual("UTF-8", GEDCOMUtils.GetCharacterSetStr(GEDCOMCharacterSet.csUTF8));

            Assert.AreEqual(GEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("ASCII"));
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("ANSI"));
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("IBMPC"));
            Assert.AreEqual(GEDCOMCharacterSet.csANSEL, GEDCOMUtils.GetCharacterSetVal("ANSEL"));
            Assert.AreEqual(GEDCOMCharacterSet.csUNICODE, GEDCOMUtils.GetCharacterSetVal("UNICODE"));
            Assert.AreEqual(GEDCOMCharacterSet.csUTF8, GEDCOMUtils.GetCharacterSetVal("UTF-8"));
            Assert.AreEqual(GEDCOMCharacterSet.csUTF8, GEDCOMUtils.GetCharacterSetVal("UTF8"));
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal(""));
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("unk"));

            //

            Assert.AreEqual(GKResearchPriority.rpLow, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpLow)));
            Assert.AreEqual(GKResearchPriority.rpNormal, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpNormal)));
            Assert.AreEqual(GKResearchPriority.rpHigh, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpHigh)));
            Assert.AreEqual(GKResearchPriority.rpTop, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpTop)));
            Assert.AreEqual(GKResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpNone)));
            Assert.AreEqual(GKResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal("unk"));

            //

            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opYes, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag.opYes)));
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opNo, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag.opNo)));
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag.opNone)));
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal("unk"));

            //

            Assert.AreEqual(GKResearchStatus.rsInProgress, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsInProgress)));
            Assert.AreEqual(GKResearchStatus.rsOnHold, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsOnHold)));
            Assert.AreEqual(GKResearchStatus.rsProblems, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsProblems)));
            Assert.AreEqual(GKResearchStatus.rsCompleted, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsCompleted)));
            Assert.AreEqual(GKResearchStatus.rsWithdrawn, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsWithdrawn)));
            Assert.AreEqual(GKResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsDefined)));
            Assert.AreEqual(GKResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(""));
            Assert.AreEqual(GKResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal("unk"));

            //

            Assert.AreEqual(GEDCOMNameType.ntNone, GEDCOMUtils.GetNameTypeVal("unk"));
            Assert.AreEqual(GEDCOMNameType.ntNone, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntNone)));
            Assert.AreEqual(GEDCOMNameType.ntAka, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntAka)));
            Assert.AreEqual(GEDCOMNameType.ntBirth, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntBirth)));
            Assert.AreEqual(GEDCOMNameType.ntImmigrant, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntImmigrant)));
            Assert.AreEqual(GEDCOMNameType.ntMaiden, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntMaiden)));
            Assert.AreEqual(GEDCOMNameType.ntMarried, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntMarried)));

            //

            Assert.AreEqual(GKCommunicationType.ctCall, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctCall)));
            Assert.AreEqual(GKCommunicationType.ctEMail, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctEMail)));
            Assert.AreEqual(GKCommunicationType.ctFax, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctFax)));
            Assert.AreEqual(GKCommunicationType.ctLetter, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctLetter)));
            Assert.AreEqual(GKCommunicationType.ctTape, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctTape)));
            Assert.AreEqual(GKCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctVisit)));
            Assert.AreEqual(GKCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(""));
            Assert.AreEqual(GKCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal("unk"));

            //

            Assert.AreEqual(GEDCOMChildLinkageStatus.clChallenged, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clChallenged)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clDisproven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clDisproven)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clProven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clProven)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clNone)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal("unk"));

            //

            Assert.AreEqual(GEDCOMPedigreeLinkageType.plAdopted, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plAdopted)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plBirth, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plBirth)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plFoster, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plFoster)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plSealing, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plSealing)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plNone)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal("unk"));

            //

            Assert.AreEqual(GEDCOMRestriction.rnConfidential, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnConfidential)));
            Assert.AreEqual(GEDCOMRestriction.rnLocked, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnLocked)));
            Assert.AreEqual(GEDCOMRestriction.rnPrivacy, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnPrivacy)));
            Assert.AreEqual(GEDCOMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnNone)));
            Assert.AreEqual(GEDCOMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal("unk"));

            //

            Assert.AreEqual(GEDCOMMediaType.mtNone, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtNone)));
            Assert.AreEqual(GEDCOMMediaType.mtAudio, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtAudio)));
            Assert.AreEqual(GEDCOMMediaType.mtBook, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtBook)));
            Assert.AreEqual(GEDCOMMediaType.mtCard, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtCard)));
            Assert.AreEqual(GEDCOMMediaType.mtElectronic, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtElectronic)));
            Assert.AreEqual(GEDCOMMediaType.mtFiche, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtFiche)));
            Assert.AreEqual(GEDCOMMediaType.mtFilm, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtFilm)));
            Assert.AreEqual(GEDCOMMediaType.mtMagazine, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtMagazine)));
            Assert.AreEqual(GEDCOMMediaType.mtManuscript, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtManuscript)));
            Assert.AreEqual(GEDCOMMediaType.mtMap, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtMap)));
            Assert.AreEqual(GEDCOMMediaType.mtNewspaper, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtNewspaper)));
            Assert.AreEqual(GEDCOMMediaType.mtPhoto, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtPhoto)));
            Assert.AreEqual(GEDCOMMediaType.mtTombstone, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtTombstone)));
            Assert.AreEqual(GEDCOMMediaType.mtVideo, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtVideo)));
            Assert.AreEqual(GEDCOMMediaType.mtUnknown, GEDCOMUtils.GetMediaTypeVal("sample"));

            //

            Assert.AreEqual(GEDCOMMultimediaFormat.mfNone, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfNone)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfBMP, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfBMP)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfGIF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfGIF)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfJPG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfJPG)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfOLE, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfOLE)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPCX, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfPCX)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTIF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfTIF)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfWAV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfWAV)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTXT, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfTXT)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfRTF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfRTF)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfAVI, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfAVI)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTGA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfTGA)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPNG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfPNG)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMPG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfMPG)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfHTM, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfHTM)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfRAW, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfRAW)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMP3, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfMP3)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfWMA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfWMA)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfUnknown, GEDCOMUtils.GetMultimediaFormatVal("xxx"));

            //

            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsNone, GEDCOMUtils.GetSpouseSealingDateStatusVal("unk"));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsNone, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsNone)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsCanceled, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsCanceled)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsCompleted, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsCompleted)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsExcluded, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsExcluded)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsDNS, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsDNS)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsDNSCAN, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsDNSCAN)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsPre1970, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsPre1970)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsSubmitted, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsSubmitted)));
            Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsUncleared, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus.sdsUncleared)));

            //

            Assert.AreEqual(Encoding.GetEncoding(1251), GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csASCII));
            Assert.AreEqual(Encoding.Unicode, GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csUNICODE));
            Assert.AreEqual(Encoding.UTF8, GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csUTF8));
            //Assert.IsInstanceOf(typeof(AnselEncoding), GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csANSEL));
            Assert.AreEqual(Encoding.GetEncoding(1251), GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csANSEL));

            //

            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsNone, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsNone)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsChild, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsChild)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsCompleted, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsCompleted)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsExcluded, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsExcluded)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsPre1970, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsPre1970)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsStillborn, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsStillborn)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsSubmitted, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsSubmitted)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsUncleared, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsUncleared)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsNone, GEDCOMUtils.GetBaptismDateStatusVal("unk"));

            //

            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsNone, GEDCOMUtils.GetEndowmentDateStatusVal("unk"));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsNone, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsNone)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsChild, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsChild)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsCompleted, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsCompleted)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsExcluded, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsExcluded)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsInfant, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsInfant)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsPre1970, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsPre1970)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsStillborn, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsStillborn)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsSubmitted, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsSubmitted)));
            Assert.AreEqual(GEDCOMEndowmentDateStatus.edsUncleared, GEDCOMUtils.GetEndowmentDateStatusVal(GEDCOMUtils.GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus.edsUncleared)));

            //

            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsNone, GEDCOMUtils.GetChildSealingDateStatusVal("unk"));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsNone, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsNone)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsBIC, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsBIC)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsExcluded, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsExcluded)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsPre1970, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsPre1970)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsStillborn, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsStillborn)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsSubmitted, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsSubmitted)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsUncleared, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsUncleared)));
        }

        [Test]
        public void _EmptyTests()
        {
        }

        private GEDCOMTag TagConstructorTest(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return null;
        }

        [Test]
        public void GEDCOMFactory_Tests()
        {
            TagConstructor tagConst = new TagConstructor(TagConstructorTest);
            Assert.AreEqual(null, tagConst.Invoke(null, null, "x", "x"));

            //

            GEDCOMFactory f = GEDCOMFactory.GetInstance();
            Assert.IsNotNull(f, "f != null");

            f.RegisterTag("DATE", GEDCOMDateValue.Create);

            GEDCOMTag tag = f.CreateTag(null, null, "DATE", "");
            Assert.IsNotNull(tag, "tag != null");

            tag = f.CreateTag(null, null, "TEST", "");
            Assert.IsNull(tag, "tag == null");
        }

        [Test]
        public void GEDCOMMathes_Tests()
        {
            GEDCOMTree tree = new GEDCOMTree();
            Assert.IsNotNull(tree);

            GEDCOMIndividualRecord ind1, ind2;
            GEDCOMCustomEvent ev1, ev2;
            GEDCOMDateValue dtVal1, dtVal2;

            ind1 = tree.CreateIndividual();
            ind1.Sex = GEDCOMSex.svMale;
            GEDCOMPersonalName pn = ind1.AddPersonalName(new GEDCOMPersonalName(tree, ind1, "", ""));
            pn.SetNameParts("Ivan Ivanov", "Fedoroff", "");

            ind2 = tree.CreateIndividual();
            ind2.Sex = GEDCOMSex.svMale;
            pn = ind2.AddPersonalName(new GEDCOMPersonalName(tree, ind2, "", ""));
            pn.SetNameParts("Ivan Ivanovich", "Fedoroff", "");

            ev1 = new GEDCOMIndividualEvent(tree, ind1, "BIRT", "");
            dtVal1 = ev1.Detail.Date;
            ind1.AddEvent(ev1);

            ev2 = new GEDCOMIndividualEvent(tree, ind2, "BIRT", "");
            dtVal2 = ev2.Detail.Date;
            ind2.AddEvent(ev2);

            float res;
            MatchParams mParams;
            mParams.NamesIndistinctThreshold = 1.0f;
            mParams.DatesCheck = true;
            mParams.RusNames = true;
            mParams.YearsInaccuracy = 0;

            // null
            res = dtVal1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            // null
            res = ev1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            // dtVal1 -> dtVal2, delta = 0
            dtVal1.SetDateTime(DateTime.Parse("10.10.2013"));
            dtVal2.SetDateTime(DateTime.Parse("10.10.2013"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(100.0f, res);

            // ev1 -> ev2, delta = 0
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(100.0f, res);

            // dtVal1 -> dtVal2, delta = 3
            mParams.YearsInaccuracy = 3;

            dtVal2.SetDateTime(DateTime.Parse("10.10.2015"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(100.0f, res);

            // ev1 -> ev2, delta = 3
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(100.0f, res);

            dtVal2.SetDateTime(DateTime.Parse("10.10.2009"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(0.0f, res);

            // ev1 -> ev2, delta = 3
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(0.0f, res);

            // //

            res = ind1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            res = ind1.IsMatch(ind2, mParams);
            Assert.AreEqual(0.0f, res);

            // Ivanov - Ivanov(ich) : 3 chars of difference -> 0.88
            mParams.NamesIndistinctThreshold = 0.85f;
            mParams.YearsInaccuracy = 4;

            res = ind1.IsMatch(ind2, mParams);
            Assert.AreEqual(100.0f, res);
        }

        [Test]
        public void GEDCOMData_Tests()
        {
            using (GEDCOMData data = GEDCOMData.Create(null, null, "", "") as GEDCOMData) {
                Assert.IsNotNull(data);
                
                data.Agency = "test agency";
                Assert.AreEqual("test agency", data.Agency);
                
                GEDCOMTag evenTag = data.AddTag("EVEN", "", null);
                Assert.IsNotNull(evenTag);
                
                GEDCOMEvent evt = data.Events[0];
                Assert.AreEqual(evenTag, evt);
                
                data.Clear();
                Assert.IsTrue(data.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                data.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, data.Owner);
            }
        }

        [Test]
        public void GEDCOMEvent_Tests()
        {
            using (GEDCOMEvent evt = GEDCOMEvent.Create(null, null, "", "") as GEDCOMEvent)
            {
                Assert.IsNotNull(evt);
                
                Assert.IsNotNull(evt.Date);
                
                Assert.IsNotNull(evt.Place);
            }
        }

        [Test]
        public void GEDCOMDateStatus_Tests()
        {
            using (GEDCOMDateStatus dateStatus = GEDCOMDateStatus.Create(null, null, "", "") as GEDCOMDateStatus)
            {
                Assert.IsNotNull(dateStatus);

                Assert.IsNotNull(dateStatus.ChangeDate);
            }
        }

        [Test]
        public void GEDCOMIndividualOrdinance_Tests()
        {
            using (GEDCOMIndividualOrdinance iOrd = GEDCOMIndividualOrdinance.Create(null, null, "", "") as GEDCOMIndividualOrdinance)
            {
                Assert.IsNotNull(iOrd);
                
                Assert.IsNotNull(iOrd.Date);
                
                iOrd.TempleCode = "temple code";
                Assert.AreEqual("temple code", iOrd.TempleCode);
                
                iOrd.Place = "test place";
                Assert.AreEqual("test place", iOrd.Place);
                
                iOrd.BaptismDateStatus = GEDCOMBaptismDateStatus.bdsCompleted;
                Assert.AreEqual(GEDCOMBaptismDateStatus.bdsCompleted, iOrd.BaptismDateStatus);
                
                Assert.IsNotNull(iOrd.BaptismChangeDate);
                
                iOrd.EndowmentDateStatus = GEDCOMEndowmentDateStatus.edsExcluded;
                Assert.AreEqual(GEDCOMEndowmentDateStatus.edsExcluded, iOrd.EndowmentDateStatus);
                
                Assert.IsNotNull(iOrd.EndowmentChangeDate);
                
                Assert.IsNotNull(iOrd.Family);
                
                iOrd.ChildSealingDateStatus = GEDCOMChildSealingDateStatus.cdsPre1970;
                Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsPre1970, iOrd.ChildSealingDateStatus);
                
                Assert.IsNotNull(iOrd.ChildSealingChangeDate);
                
                Assert.IsNotNull(iOrd.DateStatus);
            }
        }

        [Test]
        public void GEDCOMSpouseSealing_Tests()
        {
            using (GEDCOMSpouseSealing spouseSealing = GEDCOMSpouseSealing.Create(null, null, "", "") as GEDCOMSpouseSealing)
            {
                Assert.IsNotNull(spouseSealing);
                
                Assert.IsNotNull(spouseSealing.Date);
                
                spouseSealing.TempleCode = "temple code";
                Assert.AreEqual("temple code", spouseSealing.TempleCode);
                
                spouseSealing.Place = "test place";
                Assert.AreEqual("test place", spouseSealing.Place);
                
                spouseSealing.SpouseSealingDateStatus = GEDCOMSpouseSealingDateStatus.sdsCanceled;
                Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsCanceled, spouseSealing.SpouseSealingDateStatus);
                
                Assert.IsNotNull(spouseSealing.SpouseSealingChangeDate);
                
                Assert.IsNotNull(spouseSealing.DateStatus);
            }
        }

        [Test]
        public void XRefReplacer_Tests()
        {
            using (XRefReplacer replacer = new XRefReplacer())
            {
                Assert.IsNotNull(replacer);

                GEDCOMIndividualRecord iRec = _context.CreatePersonEx("ivan", "ivanovich", "ivanov", GEDCOMSex.svMale, false);
                replacer.AddXRef(iRec, "I210", iRec.XRef);

                string newXRef = replacer.FindNewXRef("I210");
                Assert.AreEqual(iRec.XRef, newXRef);

                newXRef = replacer.FindNewXRef("I310");
                Assert.AreEqual("I310", newXRef);

                for (int i = 0; i < replacer.Count; i++) {
                    XRefReplacer.XRefEntry xre = replacer[i];
                    Assert.AreEqual(iRec, xre.Rec);
                }
            }
        }

        [Test]
        public void UDN1_Tests()
        {
            UDN emptyUDN = UDN.CreateEmpty();

            // BIRT: "28 DEC 1990"
            GEDCOMIndividualRecord iRec = _context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            //Assert.AreEqual(EmptyUDN, GEDCOMUtils.GetUDN(null));
            Assert.AreEqual(emptyUDN, GEDCOMUtils.GetUDN(null, ""));
            //Assert.AreEqual(EmptyUDN, GEDCOMUtils.GetUDN("0102"));

            UDN testUDN = GEDCOMUtils.GetUDN(iRec, "BIRT");
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            testUDN = GEDCOMUtils.GetUDN("28/12/1990");
            Assert.AreEqual("1990/12/28", testUDN.ToString());
        }

        [Test]
        public void UDN2_Tests()
        {
            UDN emptyUDN = UDN.CreateEmpty();
            Assert.IsTrue(emptyUDN.IsEmpty());

            using (GEDCOMDateValue dateVal = new GEDCOMDateValue(null, null, "", "")) {
                dateVal.ParseString("28 DEC 1990");
                UDN testUDN = dateVal.GetUDN();
                Assert.AreEqual("1990/12/28", testUDN.ToString());

                dateVal.ParseString("ABT 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/1/20", testUDN.ToString());

                dateVal.ParseString("CAL 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/1/20", testUDN.ToString());

                dateVal.ParseString("EST 20 DEC 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/12/20", testUDN.ToString());

                dateVal.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("2013/1/14", testUDN.ToString());

                dateVal.ParseString("BEF 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("<2013/1/20", testUDN.ToString());

                dateVal.ParseString("AFT 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual(">2013/1/20", testUDN.ToString());

                //
                UDN testUDNAft = UDN.CreateAfter(UDNCalendarType.ctGregorian, 1900, 11, 0);
                Assert.AreEqual(">1900/11/??", testUDNAft.ToString());

                UDN testUDNBef = UDN.CreateBefore(UDNCalendarType.ctGregorian, 1900, 0, 17);
                Assert.AreEqual("<1900/??/17", testUDNBef.ToString());

                UDN testUDNApp = UDN.CreateApproximate(UDNCalendarType.ctGregorian, 0, 5, 14);
                Assert.AreEqual("~????/5/14", testUDNApp.ToString());

                Assert.AreEqual(-1, testUDNBef.CompareTo(testUDNAft));
                Assert.AreEqual(-1, testUDNBef.CompareTo(null));

                UDN testUDN2 = (UDN)testUDNApp.Clone();
                Assert.AreEqual("~????/5/14", testUDN2.ToString());
                Assert.IsFalse(testUDN2.IsEmpty());

                //dateVal.ParseString("28 DEC 1990");
                //testUDN = dateVal.GetUDN();
                //Assert.AreEqual("1990/12/28", testUDN.ToString());
            }
        }

        [Test]
        public void GEDCOMTagWithLists_Tests()
        {
            // GEDCOMTagWithLists protected class, derived - GEDCOMEventDetail
            using (GEDCOMPlace tag = GEDCOMPlace.Create(null, null, "", "") as GEDCOMPlace)
            {
                Assert.IsNotNull(tag);

                Assert.IsNotNull(tag.Notes);
                Assert.IsNotNull(tag.SourceCitations);
                Assert.IsNotNull(tag.MultimediaLinks);

                Assert.IsNull(tag.AddNote(null));
                Assert.IsNull(tag.AddSource(null, "page", 1));
                Assert.IsNull(tag.AddMultimedia(null));

                Assert.IsNotNull(tag.AddNote(new GEDCOMNoteRecord(null, null, "", "")));
                Assert.IsNotNull(tag.AddSource(new GEDCOMSourceRecord(null, null, "", ""), "page", 1));
                Assert.IsNotNull(tag.AddMultimedia(new GEDCOMMultimediaRecord(null, null, "", "")));
            }
        }

        [Test]
        public void GEDCOMChangeDate_Tests()
        {
            using (GEDCOMChangeDate cd = GEDCOMChangeDate.Create(null, null, "CHAN", "") as GEDCOMChangeDate)
            {
                Assert.IsNotNull(cd);

                Assert.IsNotNull(cd.Notes);

                DateTime dtNow = DateTime.Now;
                dtNow = dtNow.AddTicks(-dtNow.Ticks % 10000000);
                cd.ChangeDateTime = dtNow;

                DateTime dtx = cd.ChangeDateTime;
                Assert.AreEqual(dtNow, dtx);

                GEDCOMTime time = cd.ChangeTime;
                Assert.AreEqual(dtNow.Second, time.Seconds);
                Assert.AreEqual(dtNow.Minute, time.Minutes);
                Assert.AreEqual(dtNow.Hour, time.Hour);
                Assert.AreEqual(dtNow.Millisecond, time.Fraction);

                time.Seconds = 11;
                Assert.AreEqual(11, time.Seconds);
                time.Minutes = 22;
                Assert.AreEqual(22, time.Minutes);
                time.Hour = 12;
                Assert.AreEqual(12, time.Hour);
                
                Assert.AreEqual("12:22:11", time.StringValue);
                
                Assert.AreEqual(DateTime.Now.Date.ToString("yyyy.MM.dd") + " 12:22:11", cd.ToString());

                Assert.IsFalse(time.IsEmpty());
                time.Clear();
                Assert.IsTrue(time.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMTime_Tests()
        {
            using (GEDCOMTime time = new GEDCOMTime(null, null, "TIME", "20:20:20.100"))
            {
                Assert.IsNotNull(time, "time != null");

                Assert.AreEqual(20, time.Hour);
                Assert.AreEqual(20, time.Minutes);
                Assert.AreEqual(20, time.Seconds);
                Assert.AreEqual(100, time.Fraction);

                time.Fraction = 200;
                Assert.AreEqual(200, time.Fraction);

                Assert.AreEqual("20:20:20.200", time.StringValue);

                time.Hour = 0;
                time.Minutes = 0;
                time.Seconds = 0;
                Assert.AreEqual("", time.StringValue);
            }
        }

        [Test]
        public void GEDCOMDateExact_Tests()
        {
            using (GEDCOMDateExact dtx1 = new GEDCOMDateExact(null, null, "DATE", "20 JAN 2013"))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                DateTime dt = ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");

                //dtx1.DateCalendar = GEDCOMCalendar.dcFrench;
                Assert.AreEqual(GEDCOMCalendar.dcGregorian, dtx1.DateCalendar);

                dtx1.Day = 21;
                Assert.AreEqual(21, dtx1.Day);

                dtx1.Month = "SEP";
                Assert.AreEqual("SEP", dtx1.Month);

                dtx1.Year = 1812;
                Assert.AreEqual(1812, dtx1.Year);

                dtx1.YearBC = true;
                Assert.AreEqual(true, dtx1.YearBC);

                dtx1.YearModifier = "2";
                Assert.AreEqual("2", dtx1.YearModifier);

                //
                dtx1.ParseString("01 FEB 1934/11B.C.");
                Assert.AreEqual(01, dtx1.Day);
                Assert.AreEqual("FEB", dtx1.Month);
                Assert.AreEqual(1934, dtx1.Year);
                Assert.AreEqual("11", dtx1.YearModifier);
                Assert.AreEqual(true, dtx1.YearBC);
                dtx1.ParseString("01 FEB 1934/11B.C.");
                Assert.AreEqual("01 FEB 1934/11B.C.", dtx1.StringValue);

                // gregorian

                dtx1.SetGregorian(1, 1, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcGregorian, dtx1.DateCalendar);
                Assert.AreEqual("01 JAN 1980", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.SetGregorian(1, "X", 1980, "", false); });

                // julian

                dtx1.SetJulian(1, "JAN", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcJulian, dtx1.DateCalendar);

                dtx1.SetJulian(1, 3, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcJulian, dtx1.DateCalendar);
                Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx1.StringValue);
                dtx1.ParseString("@#DJULIAN@ 01 MAR 1980");
                Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx1.StringValue);

                using (GEDCOMDateExact dtx2 = new GEDCOMDateExact(null, null, "DATE", ""))
                {
                    Assert.IsNotNull(dtx2, "dtx2 != null");

                    dtx2.Assign(null);
                    Assert.AreEqual("", dtx2.StringValue);
                    Assert.AreEqual(new DateTime(0), dtx2.GetDateTime());

                    Assert.IsFalse(dtx2.IsValidDate());

                    dtx2.Assign(dtx1);
                    Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx2.StringValue);

                    Assert.IsTrue(dtx2.IsValidDate());
                }

                // hebrew

                dtx1.SetHebrew(1, "TSH", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcHebrew, dtx1.DateCalendar);

                dtx1.SetHebrew(1, 2, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcHebrew, dtx1.DateCalendar);
                Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", dtx1.StringValue);
                dtx1.ParseString("@#DHEBREW@ 01 CSH 1980");
                Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.SetHebrew(1, "X", 1980, false); });

                // french

                dtx1.SetFrench(1, "VEND", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcFrench, dtx1.DateCalendar);

                dtx1.SetFrench(1, 2, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcFrench, dtx1.DateCalendar);
                Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", dtx1.StringValue);
                dtx1.ParseString("@#DFRENCH R@ 01 BRUM 1980");
                Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.SetFrench(1, "X", 1980, false); });

                // roman

                dtx1.SetRoman(1, "JAN", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcRoman, dtx1.DateCalendar);

                dtx1.SetUnknown(1, "JAN", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcUnknown, dtx1.DateCalendar);
            }
        }

        [Test]
        public void GEDCOMDateRange_Tests()
        {
            using (GEDCOMDateRange dtx1 = new GEDCOMDateRange(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());

                int year; ushort month, day; bool yearBC;
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(-1, year);
                Assert.AreEqual(0, month);
                Assert.AreEqual(0, day);
                Assert.AreEqual(false, yearBC);

                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(04, day);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("BEF 20 JAN 2013");
                Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(20, day);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("AFT 20 JAN 2013");
                Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(20, day);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMDatePeriod_Tests()
        {
            using (GEDCOMDatePeriod dtx1 = new GEDCOMDatePeriod(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());

                int year; ushort month, day; bool yearBC;
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(-1, year);
                Assert.AreEqual(0, month);
                Assert.AreEqual(0, day);
                Assert.AreEqual(false, yearBC);

                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(04, day);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 04 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013 TO 04 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("04.01.2013"), dtx1.Date);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("FROM 04 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("04.01.2013"), dtx1.Date);
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(04, day);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("TO 23 JAN 2013");
                Assert.AreEqual("TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("23.01.2013"), dtx1.Date);
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(23, day);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMDateValue_Tests()
        {
            // check empty dateval match
            using (GEDCOMDateValue dtx1 = new GEDCOMDateValue(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                using (GEDCOMDateValue dtx2 = new GEDCOMDateValue(null, null, "DATE", ""))
                {
                    Assert.IsNotNull(dtx2, "dtx1 != null");

                    Assert.AreEqual(0.0f, dtx1.IsMatch(dtx2, new MatchParams()));
                }
            }

            // check GetDateParts
            using (GEDCOMDateValue dtx1 = new GEDCOMDateValue(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                int year; ushort month, day; bool yearBC;
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(-1, year);
                Assert.AreEqual(0, month);
                Assert.AreEqual(0, day);
                Assert.AreEqual(false, yearBC);

                dtx1.ParseString("20 JAN 2013");
                dtx1.GetDateParts(out year, out month, out day, out yearBC);
                Assert.AreEqual(2013, year);
                Assert.AreEqual(1, month);
                Assert.AreEqual(20, day);
                Assert.AreEqual(false, yearBC);
            }

            using (GEDCOMDateValue dtx1 = new GEDCOMDateValue(null, null, "DATE", "20 JAN 2013"))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                DateTime dt = ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");

                dtx1.ParseString("INT 20 JAN 2013 (today)");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
                Assert.AreEqual((dtx1.Value as GEDCOMDateInterpreted).DatePhrase, "today");
                (dtx1.Value as GEDCOMDateInterpreted).DatePhrase = "now";
                Assert.AreEqual(dtx1.StringValue, "INT 20 JAN 2013 (now)");
                (dtx1.Value as GEDCOMDateInterpreted).DatePhrase = "(yesterday)";
                Assert.AreEqual(dtx1.StringValue, "INT 20 JAN 2013 (yesterday)");

                // TODO: not passed
                //dtx1.ParseString("INT 20 JAN 2013 (today (yesterday))");
                //Assert.AreEqual(dtx1.StringValue, "INT 20 JAN 2013 (yesterday)");
                
                string st;
                
                st = "ABT 20 JAN 2013";
                dtx1.ParseString(st);
                Assert.IsTrue(dtx1.Date.Equals(dt));
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(((GEDCOMDateApproximated)dtx1.Value).Approximated, GEDCOMApproximated.daAbout);
                
                st = "CAL 20 JAN 2013";
                dtx1.ParseString(st);
                Assert.AreEqual(dtx1.Date, dt);
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(((GEDCOMDateApproximated)dtx1.Value).Approximated, GEDCOMApproximated.daCalculated);
                
                st = "EST 20 DEC 2013";
                dtx1.ParseString(st);
                Assert.AreEqual(dtx1.Date, ParseDT("20.12.2013"));
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(((GEDCOMDateApproximated)dtx1.Value).Approximated, GEDCOMApproximated.daEstimated);

                ((GEDCOMDateApproximated)dtx1.Value).Approximated = GEDCOMApproximated.daCalculated;
                Assert.AreEqual(dtx1.StringValue, "CAL 20 DEC 2013");

                ((GEDCOMDateApproximated)dtx1.Value).Approximated = GEDCOMApproximated.daExact;
                Assert.AreEqual(dtx1.StringValue, "20 DEC 2013");

                using (GEDCOMDateValue dtx2 = new GEDCOMDateValue(null, null, "DATE", "19 JAN 2013"))
                {
                    int res = dtx1.CompareTo(dtx2);
                    Assert.AreEqual(1, res);
                }
                
                int res1 = dtx1.CompareTo(null);
                Assert.AreEqual(-1, res1);

                //
                
                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual("04 JAN 2013", (dtx1.Value as GEDCOMDatePeriod).DateFrom.StringValue);
                Assert.AreEqual("23 JAN 2013", (dtx1.Value as GEDCOMDatePeriod).DateTo.StringValue);
                dtx1.Clear();
                Assert.IsTrue(dtx1.IsEmpty());

                dtx1.ParseString("BEF 20 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);

                dtx1.ParseString("AFT 20 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);

                dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
                Assert.AreEqual("04 JAN 2013", (dtx1.Value as GEDCOMDateRange).After.StringValue);
                Assert.AreEqual("25 JAN 2013", (dtx1.Value as GEDCOMDateRange).Before.StringValue);
                dtx1.Clear();
                Assert.IsTrue(dtx1.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                dtx1.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, dtx1.Owner);
            }
        }

        [Test]
        public void GEDCOMAddress_Tests()
        {
            using (GEDCOMAddress addr = GEDCOMAddress.Create(null, null, "ADDR", "") as GEDCOMAddress)
            {
                Assert.IsNotNull(addr, "addr != null");

                addr.SetAddressText("test");
                Assert.AreEqual("test", addr.Address.Text.Trim());

                addr.Address = new StringList("This\r\naddress\r\ntest");
                Assert.AreEqual("This\r\naddress\r\ntest", addr.Address.Text.Trim());
                Assert.AreEqual("This", addr.Address[0]);
                Assert.AreEqual("address", addr.Address[1]);
                Assert.AreEqual("test", addr.Address[2]);

                addr.AddTag("PHON", "8 911 101 99 99", null);
                Assert.AreEqual("8 911 101 99 99", addr.PhoneNumbers[0].StringValue);

                addr.AddTag("EMAIL", "test@mail.com", null);
                Assert.AreEqual("test@mail.com", addr.EmailAddresses[0].StringValue);

                addr.AddTag("FAX", "abrakadabra", null);
                Assert.AreEqual("abrakadabra", addr.FaxNumbers[0].StringValue);

                addr.AddTag("WWW", "http://test.com", null);
                Assert.AreEqual("http://test.com", addr.WebPages[0].StringValue);

                // stream test
                string buf = TagStreamTest(addr);
                Assert.AreEqual(buf, "0 ADDR This\r\n"+"1 CONT address\r\n"+"1 CONT test\r\n"
                                +"0 PHON 8 911 101 99 99\r\n"
                                +"0 EMAIL test@mail.com\r\n"
                                +"0 FAX abrakadabra\r\n"
                                +"0 WWW http://test.com\r\n");

                addr.AddPhoneNumber("8 911 101 33 33");
                Assert.AreEqual("8 911 101 33 33", addr.PhoneNumbers[1].StringValue);

                addr.AddEmailAddress("test@mail.ru");
                Assert.AreEqual("test@mail.ru", addr.EmailAddresses[1].StringValue);

                addr.AddFaxNumber("abrakadabra");
                Assert.AreEqual("abrakadabra", addr.FaxNumbers[1].StringValue);

                addr.AddWebPage("http://test.ru");
                Assert.AreEqual("http://test.ru", addr.WebPages[1].StringValue);

                //

                addr.AddressLine1 = "test1";
                Assert.AreEqual("test1", addr.AddressLine1);

                addr.AddressLine2 = "test2";
                Assert.AreEqual("test2", addr.AddressLine2);

                addr.AddressLine3 = "test3";
                Assert.AreEqual("test3", addr.AddressLine3);

                addr.AddressCity = "test4";
                Assert.AreEqual("test4", addr.AddressCity);

                addr.AddressState = "test5";
                Assert.AreEqual("test5", addr.AddressState);

                addr.AddressCountry = "test6";
                Assert.AreEqual("test6", addr.AddressCountry);

                addr.AddressPostalCode = "test7";
                Assert.AreEqual("test7", addr.AddressPostalCode);

                using (GEDCOMAddress addr2 = GEDCOMAddress.Create(null, null, "ADDR", "") as GEDCOMAddress)
                {
                    addr2.Assign(addr);

                    Assert.AreEqual("This\r\naddress\r\ntest", addr2.Address.Text.Trim());
                    Assert.AreEqual("8 911 101 99 99", addr2.PhoneNumbers[0].StringValue);
                    Assert.AreEqual("test@mail.com", addr2.EmailAddresses[0].StringValue);
                    Assert.AreEqual("abrakadabra", addr2.FaxNumbers[0].StringValue);
                    Assert.AreEqual("http://test.com", addr2.WebPages[0].StringValue);
                    Assert.AreEqual("8 911 101 33 33", addr2.PhoneNumbers[1].StringValue);
                    Assert.AreEqual("test@mail.ru", addr2.EmailAddresses[1].StringValue);
                    Assert.AreEqual("abrakadabra", addr2.FaxNumbers[1].StringValue);
                    Assert.AreEqual("http://test.ru", addr2.WebPages[1].StringValue);
                    Assert.AreEqual("test1", addr2.AddressLine1);
                    Assert.AreEqual("test2", addr2.AddressLine2);
                    Assert.AreEqual("test3", addr2.AddressLine3);
                    Assert.AreEqual("test4", addr2.AddressCity);
                    Assert.AreEqual("test5", addr2.AddressState);
                    Assert.AreEqual("test6", addr2.AddressCountry);
                    Assert.AreEqual("test7", addr2.AddressPostalCode);
                }

                addr.SetAddressArray(new string[] {"test11", "test21", "test31"});
                Assert.AreEqual("test11", addr.Address[0]);
                Assert.AreEqual("test21", addr.Address[1]);
                Assert.AreEqual("test31", addr.Address[2]);

                Assert.IsFalse(addr.IsEmpty());
                addr.Clear();
                Assert.IsTrue(addr.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                addr.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, addr.Owner);
            }
        }

        [Test]
        public void GEDCOMAlias_Tests()
        {
            using (GEDCOMAlias alias = GEDCOMAlias.Create(null, null, "ALIA", "") as GEDCOMAlias)
            {
                Assert.IsNotNull(alias, "alias != null");
            }
        }

        [Test]
        public void GEDCOMAssociation_Tests()
        {
            using (GEDCOMAssociation association = GEDCOMAssociation.Create(null, null, "ASSO", "") as GEDCOMAssociation) {
                Assert.IsNotNull(association);

                Assert.IsNotNull(association.SourceCitations);
                
                Assert.IsNotNull(association.Notes); // for GEDCOMPointerWithNotes
                
                association.Relation = "This is test relation";
                Assert.AreEqual("This is test relation", association.Relation);

                association.Individual = null;
                Assert.IsNull(association.Individual);

                GEDCOMTag tag = association.AddTag("SOUR", "xxx", null);
                Assert.IsNotNull(tag);
                Assert.IsTrue(tag is GEDCOMSourceCitation);

                Assert.IsFalse(association.IsEmpty());
                association.Clear();
                Assert.IsTrue(association.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                association.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, association.Owner);
            }
        }

        [Test]
        public void GEDCOMUserRef_Tests()
        {
            using (GEDCOMUserReference userRef = GEDCOMUserReference.Create(null, null, "REFN", "") as GEDCOMUserReference)
            {
                Assert.IsNotNull(userRef);

                userRef.ReferenceType = "test";
                Assert.AreEqual("test", userRef.ReferenceType);
            }
        }

        private void OnTreeChange(object sender, EventArgs e) {}
        private void OnTreeChanging(object sender, EventArgs e) {}
        private void OnTreeProgress(object sender, int progress) {}

        [Test]
        public void GEDCOMTree_Tests()
        {
            GEDCOMTree tree = new GEDCOMTree();
            Assert.IsNotNull(tree);


            // Tests of event handlers
            tree.OnChange += OnTreeChange;
            //Assert.AreEqual(OnTreeChange, tree.OnChange);
            tree.OnChange -= OnTreeChange;
            //Assert.AreEqual(null, tree.OnChange);
            tree.OnChanging += OnTreeChanging;
            //Assert.AreEqual(OnTreeChanging, tree.OnChanging);
            tree.OnChanging -= OnTreeChanging;
            //Assert.AreEqual(null, tree.OnChanging);
            tree.OnProgress += OnTreeProgress;
            //Assert.AreEqual(OnTreeProgress, tree.OnProgress);
            tree.OnProgress -= OnTreeProgress;
            //Assert.AreEqual(null, tree.OnProgress);


            // Tests of determine GEDCOM-format
            Assert.AreEqual(GEDCOMFormat.gf_Unknown, tree.GetGEDCOMFormat());
            tree.Header.Source = "GENBOX";
            Assert.AreEqual(GEDCOMFormat.gf_GENBOX, tree.GetGEDCOMFormat());

            //

            Assert.IsNotNull(tree.GetSubmitter());

            GEDCOMRecord rec;

            GEDCOMIndividualRecord iRec = tree.CreateIndividual();
            Assert.IsNotNull(iRec, "CreateIndividual() != null");

            string xref = iRec.XRef;
            rec = tree.XRefIndex_Find(xref);
            Assert.IsNotNull(rec);
            Assert.AreEqual(xref, rec.XRef);

            string uid = iRec.UID;
            rec = tree.FindUID(uid);
            Assert.IsNotNull(rec);
            Assert.AreEqual(uid, rec.UID);
            Assert.IsNull(tree.FindUID(""));

            //
            GEDCOMFamilyRecord famRec = tree.CreateFamily();
            Assert.IsNotNull(famRec, "CreateFamily() != null");
            GEDCOMFamilyRecordTest(famRec, iRec);

            //
            GEDCOMNoteRecord noteRec = tree.CreateNote();
            Assert.IsNotNull(noteRec, "CreateNote() != null");
            GEDCOMNoteRecordTest(noteRec, iRec);

            //
            GEDCOMRepositoryRecord repRec = tree.CreateRepository();
            Assert.IsNotNull(repRec, "CreateRepository() != null");

            //
            GEDCOMSourceRecord srcRec = tree.CreateSource();
            Assert.IsNotNull(srcRec, "CreateSource() != null");
            GEDCOMSourceRecordTest(srcRec, iRec, repRec);

            //
            GEDCOMMultimediaRecord mmRec = tree.CreateMultimedia();
            Assert.IsNotNull(mmRec, "CreateMultimedia() != null");
            GEDCOMMultimediaRecordTest(mmRec, iRec);
            
            //

            GEDCOMRecord sbmrRec = tree.AddRecord(GEDCOMSubmitterRecord.Create(tree, tree, "", "") as GEDCOMRecord);
            Assert.IsNotNull(sbmrRec, "sbmrRec != null");
            sbmrRec.InitNew();
            string submXRef = sbmrRec.XRef;

            //

            GEDCOMSubmissionRecord submRec = tree.AddRecord(GEDCOMSubmissionRecord.Create(tree, tree, "", "") as GEDCOMRecord) as GEDCOMSubmissionRecord;
            Assert.IsNotNull(submRec, "rec1 != null");
            submRec.InitNew();
            GEDCOMSubmissionRecordTest(submRec, submXRef);

            //
            GEDCOMGroupRecord groupRec = tree.CreateGroup();
            Assert.IsNotNull(groupRec, "CreateGroup() != null");

            //
            GEDCOMTaskRecord taskRec = tree.CreateTask();
            Assert.IsNotNull(taskRec, "CreateTask() != null");

            //
            GEDCOMCommunicationRecord commRec = tree.CreateCommunication();
            Assert.IsNotNull(commRec, "CreateCommunication() != null");

            //
            GEDCOMResearchRecord resRec = tree.CreateResearch();
            Assert.IsNotNull(resRec, "CreateResearch() != null");
            GEDCOMResearchRecordTest(resRec, commRec, taskRec, groupRec);

            //
            GEDCOMLocationRecord locRec = tree.CreateLocation();
            Assert.IsNotNull(locRec, "CreateLocation() != null");


            tree.Pack();


            int size = 0;
            var enum1 = tree.GetEnumerator(GEDCOMRecordType.rtNone);
            GEDCOMRecord rec1;
            while (enum1.MoveNext(out rec1)) {
                size++;
            }
            Assert.AreEqual(14, size);

            for (int i = 0; i < tree.RecordsCount; i++) {
                GEDCOMRecord rec2 = tree[i];
                Assert.IsNotNull(rec2);

                string xref2 = rec2.XRef;
                GEDCOMRecord rec3 = tree.XRefIndex_Find(xref2);
                Assert.IsNotNull(rec3);
                Assert.AreEqual(rec2, rec3);

                /*string uid = rec2.UID;
				GEDCOMRecord rec4 = tree.FindUID(uid);
				Assert.IsNotNull(rec4);
				Assert.AreEqual(rec2, rec4);*/

                int idx = tree.IndexOf(rec2);
                Assert.AreEqual(i, idx);
            }

            tree.DeleteFamilyRecord(famRec);
            tree.DeleteNoteRecord(noteRec);
            tree.DeleteSourceRecord(srcRec);
            tree.DeleteGroupRecord(groupRec);
            tree.DeleteLocationRecord(locRec);
            tree.DeleteResearchRecord(resRec);
            tree.DeleteCommunicationRecord(commRec);
            tree.DeleteTaskRecord(taskRec);
            tree.DeleteMediaRecord(mmRec);
            tree.DeleteIndividualRecord(iRec);
            tree.DeleteRepositoryRecord(repRec);

            tree.Clear();
            Assert.AreEqual(0, tree.RecordsCount);

            tree.State = GEDCOMState.osReady;
            Assert.AreEqual(GEDCOMState.osReady, tree.State);

            tree.SetFileName("testfile.ged");
            Assert.AreEqual("testfile.ged", tree.FileName);


            // Tests of GEDCOMTree.Extract()
            using (GEDCOMTree tree2 = new GEDCOMTree()) {
                GEDCOMIndividualRecord iRec2 = tree.AddRecord(GEDCOMIndividualRecord.Create(tree2, tree2, "", "") as GEDCOMRecord) as GEDCOMIndividualRecord;
                Assert.IsNotNull(iRec2);
                iRec2.InitNew();

                tree2.AddRecord(iRec2);
                int rIdx = tree2.IndexOf(iRec2);
                Assert.IsTrue(rIdx >= 0);
                GEDCOMRecord extractedRec = tree2.Extract(rIdx);
                Assert.AreEqual(iRec2, extractedRec);
                Assert.IsTrue(tree2.IndexOf(iRec2) < 0);
            }
        }

        [Test]
        public void GEDCOMHeader_Tests()
        {
            GEDCOMHeader headRec = _context.Tree.Header;

            headRec.Notes = new StringList("This notes test");
            Assert.AreEqual("This notes test", headRec.Notes[0]);

            headRec.CharacterSet = GEDCOMCharacterSet.csASCII;
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, headRec.CharacterSet);

            headRec.CharacterSetVersion = "1x";
            Assert.AreEqual("1x", headRec.CharacterSetVersion);

            headRec.Copyright = "copyright";
            Assert.AreEqual("copyright", headRec.Copyright);

            headRec.Source = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.Source);

            headRec.ReceivingSystemName = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.ReceivingSystemName);

            headRec.Language = "Russian";
            Assert.AreEqual("Russian", headRec.Language);

            headRec.GEDCOMVersion = "5.5";
            Assert.AreEqual("5.5", headRec.GEDCOMVersion);

            headRec.GEDCOMForm = "LINEAGE-LINKED";
            Assert.AreEqual("LINEAGE-LINKED", headRec.GEDCOMForm);

            headRec.FileName = "testfile.ged";
            Assert.AreEqual("testfile.ged", headRec.FileName);

            DateTime dtx = DateTime.Now;
            dtx = dtx.AddTicks(-dtx.Ticks % 10000000);
            headRec.TransmissionDateTime = dtx;
            Assert.AreEqual(dtx, headRec.TransmissionDateTime);

            headRec.FileRevision = 113;
            Assert.AreEqual(113, headRec.FileRevision);

            headRec.PlaceHierarchy = "test11";
            Assert.AreEqual("test11", headRec.PlaceHierarchy);

            Assert.IsNotNull(headRec.SourceBusinessAddress);

            headRec.SourceBusinessName = "test23";
            Assert.AreEqual("test23", headRec.SourceBusinessName);

            headRec.SourceProductName = "test33";
            Assert.AreEqual("test33", headRec.SourceProductName);

            headRec.SourceVersion = "test44";
            Assert.AreEqual("test44", headRec.SourceVersion);

            Assert.IsNotNull(headRec.Submission);

            Assert.IsFalse(headRec.IsEmpty());
            headRec.Clear();
            Assert.IsTrue(headRec.IsEmpty());
        }

        [Test]
        public void GEDCOMMap_Tests()
        {
            using (GEDCOMMap map = GEDCOMMap.Create(null, null, "", "") as GEDCOMMap) {
                map.Lati = 5.11111;
                Assert.AreEqual(5.11111, map.Lati);

                map.Long = 7.99999;
                Assert.AreEqual(7.99999, map.Long);
            }
        }

        [Test]
        public void GEDCOMIndividualRecord_Tests()
        {
            GEDCOMIndividualRecord indiRec = _context.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;

            GEDCOMRecordTest(indiRec);

            Assert.IsNotNull(indiRec.Aliases);
            Assert.IsNotNull(indiRec.AncestorsInterest);
            Assert.IsNotNull(indiRec.Associations);
            Assert.IsNotNull(indiRec.DescendantsInterest);
            Assert.IsNotNull(indiRec.IndividualOrdinances);
            Assert.IsNotNull(indiRec.Submittors);
            Assert.IsNotNull(indiRec.UserReferences); // for GEDCOMRecord

            Assert.Throws(typeof(ArgumentException), () => { indiRec.AddEvent(GEDCOMFamilyEvent.Create(null, null, "", "") as GEDCOMCustomEvent); });

            GEDCOMIndividualRecord father, mother;
            indiRec.GetParents(out father, out mother);
            Assert.IsNull(father);
            Assert.IsNull(mother);

            indiRec.Sex = GEDCOMSex.svMale;
            Assert.AreEqual(GEDCOMSex.svMale, indiRec.Sex);

            indiRec.Restriction = GEDCOMRestriction.rnLocked;
            Assert.AreEqual(GEDCOMRestriction.rnLocked, indiRec.Restriction);

            indiRec.Patriarch = true;
            Assert.AreEqual(true, indiRec.Patriarch);
            indiRec.Patriarch = false;
            Assert.AreEqual(false, indiRec.Patriarch);

            indiRec.Bookmark = true;
            Assert.AreEqual(true, indiRec.Bookmark);
            indiRec.Bookmark = false;
            Assert.AreEqual(false, indiRec.Bookmark);

            indiRec.AncestralFileNumber = "test11";
            Assert.AreEqual("test11", indiRec.AncestralFileNumber);

            indiRec.PermanentRecordFileNumber = "test22";
            Assert.AreEqual("test22", indiRec.PermanentRecordFileNumber);

            using (GEDCOMIndividualRecord copyIndi = new GEDCOMIndividualRecord(null, null, "", "")) {
                Assert.IsNotNull(copyIndi);

                Assert.Throws(typeof(ArgumentException), () => { copyIndi.Assign(null); });

                copyIndi.Assign(indiRec);
                Assert.AreEqual(100.0f, indiRec.IsMatch(copyIndi, new MatchParams()));
            }


            Assert.IsFalse(indiRec.IsEmpty());
            indiRec.Clear();
            Assert.IsTrue(indiRec.IsEmpty());

            float ca = indiRec.GetCertaintyAssessment();
            Assert.AreEqual(0.0f, ca);


            Assert.IsNull(indiRec.GetPrimaryMultimediaLink());
            GEDCOMMultimediaLink mmLink = indiRec.SetPrimaryMultimediaLink(null);
            Assert.IsNull(mmLink);
            GEDCOMMultimediaRecord mmRec = _context.Tree.CreateMultimedia();
            mmLink = indiRec.SetPrimaryMultimediaLink(mmRec);
            Assert.IsNotNull(mmLink);
            mmLink = indiRec.GetPrimaryMultimediaLink();
            Assert.AreEqual(mmRec, mmLink.Value);


            Assert.AreEqual(-1, indiRec.IndexOfGroup(null));
            Assert.AreEqual(-1, indiRec.IndexOfSpouse(null));


            GEDCOMIndividualRecord indi2 = _context.Tree.XRefIndex_Find("I2") as GEDCOMIndividualRecord;
            GEDCOMAssociation asso = indiRec.AddAssociation("test", indi2);
            Assert.IsNotNull(asso);
        }

        [Test]
        public void GEDCOMPersonalName_Tests()
        {
            GEDCOMIndividualRecord iRec = _context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            GEDCOMPersonalName pName = iRec.PersonalNames[0];
            string first, surname;
            pName.GetNameParts(out first, out surname);
            Assert.AreEqual("Ivanov", surname);
            Assert.AreEqual("Ivan Ivanovich", first);

//			GEDCOMPersonalNamePieces pieces = pName.Pieces;
//			Assert.AreEqual(pieces.Surname, "surname");
//			Assert.AreEqual(pieces.Name, "name");
//			Assert.AreEqual(pieces.PatronymicName, "patr");

            string name, patr;
            iRec.GetNameParts(out surname, out name, out patr);
            Assert.AreEqual("Ivanov", surname);
            Assert.AreEqual("Ivan", name);
            Assert.AreEqual("Ivanovich", patr);


            GEDCOMPersonalName persName;

            persName = GEDCOMPersonalName.Create(iRec.Owner, iRec, "", "") as GEDCOMPersonalName;
            iRec.AddPersonalName(persName);

            persName = iRec.PersonalNames[0];
            persName.NameType = GEDCOMNameType.ntBirth;
            Assert.AreEqual(GEDCOMNameType.ntBirth, persName.NameType);

            //

            persName.SetNameParts("Petr", "Ivanov", "Fedoroff");

            //persName.Surname = "Ivanov";
            Assert.AreEqual("Petr", persName.FirstPart);
            Assert.AreEqual("Ivanov", persName.Surname);
            Assert.AreEqual("Fedoroff", persName.LastPart);

            Assert.AreEqual("Petr Ivanov Fedoroff", persName.FullName);

            persName.Surname = "Test";
            Assert.AreEqual("Test", persName.Surname);

            //

            GEDCOMPersonalNamePieces pnPieces = persName.Pieces;
            
            pnPieces.Prefix = "Prefix";
            Assert.AreEqual("Prefix", pnPieces.Prefix);

            pnPieces.Given = "Given";
            Assert.AreEqual("Given", pnPieces.Given);

            pnPieces.Nickname = "Nickname";
            Assert.AreEqual("Nickname", pnPieces.Nickname);

            pnPieces.SurnamePrefix = "SurnamePrefix";
            Assert.AreEqual("SurnamePrefix", pnPieces.SurnamePrefix);

            pnPieces.Surname = "Surname";
            Assert.AreEqual("Surname", pnPieces.Surname);

            pnPieces.Suffix = "Suffix";
            Assert.AreEqual("Suffix", pnPieces.Suffix);

            pnPieces.PatronymicName = "PatronymicName";
            Assert.AreEqual("PatronymicName", pnPieces.PatronymicName);

            pnPieces.MarriedName = "MarriedName";
            Assert.AreEqual("MarriedName", pnPieces.MarriedName);

            pnPieces.ReligiousName = "ReligiousName";
            Assert.AreEqual("ReligiousName", pnPieces.ReligiousName);

            pnPieces.CensusName = "CensusName";
            Assert.AreEqual("CensusName", pnPieces.CensusName);

            persName.Pack();

            string buf = TagStreamTest(persName);
            Assert.AreEqual("1 NAME Petr /Test/ Fedoroff\r\n"+
                            "2 TYPE birth\r\n"+
                            "2 NPFX Prefix\r\n"+
                            "2 GIVN Given\r\n"+
                            "2 NICK Nickname\r\n"+
                            "2 SPFX SurnamePrefix\r\n"+
                            "2 SURN Surname\r\n"+
                            "2 NSFX Suffix\r\n"+
                            "2 _PATN PatronymicName\r\n"+
                            "2 _MARN MarriedName\r\n"+
                            "2 _RELN ReligiousName\r\n"+
                            "2 _CENN CensusName\r\n", buf);

            using (GEDCOMPersonalName nameCopy = new GEDCOMPersonalName(iRec.Owner, iRec, "", "")) {
                iRec.AddPersonalName(nameCopy);
                nameCopy.Assign(persName);

                string buf2 = TagStreamTest(nameCopy);
                Assert.AreEqual("1 NAME Petr /Test/ Fedoroff\r\n"+
                                "2 TYPE birth\r\n"+
                                "2 NPFX Prefix\r\n"+
                                "2 GIVN Given\r\n"+
                                "2 NICK Nickname\r\n"+
                                "2 SPFX SurnamePrefix\r\n"+
                                "2 SURN Surname\r\n"+
                                "2 NSFX Suffix\r\n"+
                                "2 _PATN PatronymicName\r\n"+
                                "2 _MARN MarriedName\r\n"+
                                "2 _RELN ReligiousName\r\n"+
                                "2 _CENN CensusName\r\n", buf2);

                iRec.PersonalNames.Delete(nameCopy);
            }

            using (GEDCOMPersonalName name1 = new GEDCOMPersonalName(null, null, "", "")) {
                Assert.AreEqual("", name1.FirstPart);
                Assert.AreEqual("", name1.Surname);

                string fp, sp;
                name1.GetNameParts(out fp, out sp);
                Assert.AreEqual("", fp);
                Assert.AreEqual("", sp);

                Assert.AreEqual(0.0f, name1.IsMatch(null));

                using (GEDCOMPersonalName name2 = new GEDCOMPersonalName(null, null, "", "")) {
                    Assert.AreEqual(0.0f, name1.IsMatch(name2));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan", "Dub", "");
                    Assert.AreEqual(100.0f, name1.IsMatch(name2));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan", "Dub2", "");
                    Assert.AreEqual(12.5f, name1.IsMatch(name2));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan2", "Dub", "");
                    Assert.AreEqual(50.0f, name1.IsMatch(name2));
                }
            }

            persName.Clear();
            Assert.IsTrue(persName.IsEmpty());
        }

        [Test]
        public void GEDCOMFileReference_Tests()
        {
            using (GEDCOMFileReference fileRef = new GEDCOMFileReference(null, null, "", "")) {
                fileRef.MediaType = GEDCOMMediaType.mtAudio;
                Assert.AreEqual(GEDCOMMediaType.mtAudio, fileRef.MediaType);
            }

            Assert.AreEqual(GEDCOMMultimediaFormat.mfUnknown, GEDCOMFileReference.RecognizeFormat(""));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfUnknown, GEDCOMFileReference.RecognizeFormat("sample.xxx"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfBMP, GEDCOMFileReference.RecognizeFormat("sample.BMP"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfGIF, GEDCOMFileReference.RecognizeFormat("sample.Gif"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfJPG, GEDCOMFileReference.RecognizeFormat("sample.jpg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfJPG, GEDCOMFileReference.RecognizeFormat("sample.Jpeg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfOLE, GEDCOMFileReference.RecognizeFormat("sample.ole"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPCX, GEDCOMFileReference.RecognizeFormat("sample.pCx"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTIF, GEDCOMFileReference.RecognizeFormat("sample.TiF"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTIF, GEDCOMFileReference.RecognizeFormat("sample.tiff"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfWAV, GEDCOMFileReference.RecognizeFormat("sample.wav"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTXT, GEDCOMFileReference.RecognizeFormat("sample.txt"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfRTF, GEDCOMFileReference.RecognizeFormat("sample.rtf"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfAVI, GEDCOMFileReference.RecognizeFormat("sample.AvI"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTGA, GEDCOMFileReference.RecognizeFormat("sample.TGA"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPNG, GEDCOMFileReference.RecognizeFormat("sample.png"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMPG, GEDCOMFileReference.RecognizeFormat("sample.mpg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMPG, GEDCOMFileReference.RecognizeFormat("sample.mpeg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfHTM, GEDCOMFileReference.RecognizeFormat("sample.htm"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfHTM, GEDCOMFileReference.RecognizeFormat("sample.html"));
        }

        [Test]
        public void GEDCOMGroupRecord_Tests2()
        {
            using (GEDCOMGroupRecord grpRec = GEDCOMGroupRecord.Create(null, null, "", "") as GEDCOMGroupRecord)
            {
                Assert.IsNotNull(grpRec);
            }
        }

        [Test]
        public void GEDCOMGroupRecord_Tests()
        {
            GEDCOMGroupRecord groupRec = _context.Tree.XRefIndex_Find("G1") as GEDCOMGroupRecord;
            GEDCOMIndividualRecord member = _context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            groupRec.GroupName = "Test Group";
            Assert.AreEqual("Test Group", groupRec.GroupName);

            groupRec.DeleteTag("_UID");
            groupRec.DeleteTag("CHAN");
            string buf = TagStreamTest(groupRec);
            Assert.AreEqual("0 @G1@ _GROUP\r\n1 NAME Test Group\r\n", buf);

            bool res = groupRec.AddMember(null);
            Assert.IsFalse(res);

            res = groupRec.RemoveMember(null);
            Assert.IsFalse(res);

            Assert.AreEqual(-1, groupRec.IndexOfMember(null));

            groupRec.AddMember(member);
            Assert.AreEqual(0, groupRec.IndexOfMember(member));

            groupRec.RemoveMember(member);
            Assert.AreEqual(-1, groupRec.IndexOfMember(member));

            Assert.IsFalse(groupRec.IsEmpty());
            groupRec.Clear();
            Assert.IsTrue(groupRec.IsEmpty());
        }

        #endregion

        #region Partial Tests

        [Test]
        public void GEDCOMCustomEvent_Tests()
        {
            using (GEDCOMIndividualAttribute customEvent = GEDCOMIndividualAttribute.Create(null, null, "", "") as GEDCOMIndividualAttribute)
            {
                Assert.IsNotNull(customEvent);

                StringList strs = new StringList("test");
                customEvent.PhysicalDescription = strs;
                Assert.AreEqual(strs.Text, customEvent.PhysicalDescription.Text);

                customEvent.AddTag("EMAIL", "email", null);
                Assert.AreEqual("email", customEvent.Detail.Address.EmailAddresses[0].StringValue);

                customEvent.Pack();

                GEDCOMTree otherTree = new GEDCOMTree();
                customEvent.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, customEvent.Owner);
            }

            using (GEDCOMIndividualEvent customEvent = GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMIndividualEvent)
            {
                Assert.IsNotNull(customEvent);

                // stream test
                customEvent.SetName("BIRT");
                customEvent.Detail.Date.ParseString("20 SEP 1970");
                customEvent.Detail.Place.StringValue = "test place";
                string buf = TagStreamTest(customEvent);
                Assert.AreEqual("0 BIRT\r\n"+
                                "1 DATE 20 SEP 1970\r\n"+
                                "1 PLAC test place\r\n", buf);

                using (GEDCOMIndividualEvent copyEvent = GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMIndividualEvent)
                {
                    Assert.IsNotNull(copyEvent);
                    copyEvent.Assign(customEvent);

                    string buf1 = TagStreamTest(copyEvent);
                    Assert.AreEqual("0 BIRT\r\n"+
                                    "1 DATE 20 SEP 1970\r\n"+
                                    "1 PLAC test place\r\n", buf1);
                }

                customEvent.AddTag("EMAIL", "email", null);
                Assert.AreEqual("email", customEvent.Detail.Address.EmailAddresses[0].StringValue);

                customEvent.Pack();

                GEDCOMTree otherTree = new GEDCOMTree();
                customEvent.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, customEvent.Owner);
            }

            using (GEDCOMFamilyEvent customEvent = GEDCOMFamilyEvent.Create(null, null, "", "") as GEDCOMFamilyEvent)
            {
                Assert.IsNotNull(customEvent);

                customEvent.AddTag("EMAIL", "email", null);
                Assert.AreEqual("email", customEvent.Detail.Address.EmailAddresses[0].StringValue);

                customEvent.Pack();

                GEDCOMTree otherTree = new GEDCOMTree();
                customEvent.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, customEvent.Owner);
            }
        }

        public static void GEDCOMCustomEventTest(GEDCOMCustomEvent evt, string dateTest)
        {
            GEDCOMEventDetailTest(evt.Detail, dateTest);

            Assert.AreEqual(evt.Detail.Date.GetDateTime(), ParseDT(dateTest));
        }

        [Test]
        public void GEDCOMPlaceTest()
        {
            using (GEDCOMPlace place = GEDCOMPlace.Create(null, null, "", "") as GEDCOMPlace) {
                place.Form = "abrakadabra";
                Assert.AreEqual("abrakadabra", place.Form);

                Assert.IsNotNull(place.Map);
                Assert.IsNotNull(place.Location);
            }
        }

        private static void GEDCOMEventDetailTest(GEDCOMEventDetail detail, string dateTest)
        {
            Assert.AreEqual(ParseDT(dateTest), detail.Date.Date);
            Assert.AreEqual("Ivanovo", detail.Place.StringValue);

            Assert.IsNotNull(detail.Place);

            detail.Agency = "test agency";
            Assert.AreEqual("test agency", detail.Agency);

            detail.Classification = "test type";
            Assert.AreEqual("test type", detail.Classification);

            detail.Cause = "test cause";
            Assert.AreEqual("test cause", detail.Cause);

            detail.ReligiousAffilation = "test aff";
            Assert.AreEqual("test aff", detail.ReligiousAffilation);

            detail.Restriction = GEDCOMRestriction.rnLocked;
            Assert.AreEqual(GEDCOMRestriction.rnLocked, detail.Restriction);
        }

        [Test]
        public void GEDCOMTag_Test()
        {
            using (GEDCOMTag tag = GEDCOMTag.Create(null, null, "", "")) {
                Assert.AreEqual(-1, tag.IndexOfTag(null));
            }
        }

        private static void GEDCOMRecordTest(GEDCOMRecord rec)
        {
            rec.AutomatedRecordID = "test11";
            Assert.AreEqual("test11", rec.AutomatedRecordID);

            Assert.AreEqual(GEDCOMRecordType.rtIndividual, rec.RecordType);

            Assert.AreEqual(4, rec.GetId());
            Assert.AreEqual("4", rec.GetXRefNum());

            Assert.AreEqual(-1, rec.IndexOfSource(null));
        }

        [Test]
        public void GEDCOMFamilyRecord_Tests()
        {
            using (GEDCOMFamilyRecord famRec = GEDCOMFamilyRecord.Create(_context.Tree, _context.Tree, "", "") as GEDCOMFamilyRecord)
            {
                Assert.IsNotNull(famRec);

                GEDCOMIndividualRecord unkInd = new GEDCOMIndividualRecord(null, null, "", "");
                unkInd.Sex = GEDCOMSex.svUndetermined;
                Assert.IsFalse(famRec.AddSpouse(unkInd));
            }
        }

        private static void GEDCOMFamilyRecordTest(GEDCOMFamilyRecord famRec, GEDCOMIndividualRecord indiv)
        {
            Assert.IsNotNull(famRec.Submitter);
            Assert.IsNotNull(famRec.SpouseSealings);

            famRec.Restriction = GEDCOMRestriction.rnLocked;
            Assert.AreEqual(GEDCOMRestriction.rnLocked, famRec.Restriction);

            famRec.AddChild(indiv);
            Assert.AreEqual(0, famRec.IndexOfChild(indiv));

            // stream test
            famRec.DeleteTag("_UID");
            famRec.DeleteTag("CHAN");
            string buf = TagStreamTest(famRec);
            Assert.AreEqual("0 @F1@ FAM\r\n"+
                            "1 SUBM\r\n"+
                            "1 RESN locked\r\n"+
                            "1 CHIL @I1@\r\n", buf);

            // Integrity test
            GEDCOMChildToFamilyLink childLink = indiv.ChildToFamilyLinks[0];
            Assert.IsNotNull(childLink.Family);

            famRec.RemoveChild(indiv);
            Assert.AreEqual(-1, famRec.IndexOfChild(indiv));

            //

            Assert.Throws(typeof(ArgumentException), () => { famRec.AddEvent(GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMCustomEvent); });

            //

            famRec.Husband.Value = indiv;
            Assert.AreEqual(indiv, famRec.GetHusband());
            famRec.Husband.Value = null;

            //

            famRec.Wife.Value = indiv;
            Assert.AreEqual(indiv, famRec.GetWife());
            famRec.Wife.Value = null;

            //

            indiv.Sex = GEDCOMSex.svMale;
            famRec.AddSpouse(indiv);
            Assert.AreEqual(0, indiv.IndexOfSpouse(famRec));
            GEDCOMSpouseToFamilyLinkTest(indiv.SpouseToFamilyLinks[0]);
            Assert.IsNull(famRec.GetSpouseBy(indiv));
            famRec.RemoveSpouse(indiv);

            indiv.Sex = GEDCOMSex.svFemale;
            famRec.AddSpouse(indiv);
            Assert.AreEqual(0, indiv.IndexOfSpouse(famRec));
            GEDCOMSpouseToFamilyLinkTest(indiv.SpouseToFamilyLinks[0]);
            Assert.IsNull(famRec.GetSpouseBy(indiv));
            famRec.RemoveSpouse(indiv);

            //

            famRec.SortChilds();

            //

            famRec.AddChild(null);
            famRec.RemoveChild(null);
            famRec.AddSpouse(null);
            famRec.RemoveSpouse(null);

            Assert.IsFalse(famRec.IsEmpty());
            famRec.Clear();
            Assert.IsTrue(famRec.IsEmpty());
        }

        [Test]
        public void GEDCOMChildToFamilyLink_Tests()
        {
            using (GEDCOMChildToFamilyLink childLink = GEDCOMChildToFamilyLink.Create(null, null, "", "") as GEDCOMChildToFamilyLink)
            {
                Assert.IsNotNull(childLink);

                childLink.ChildLinkageStatus = GEDCOMChildLinkageStatus.clChallenged;
                Assert.AreEqual(GEDCOMChildLinkageStatus.clChallenged, childLink.ChildLinkageStatus);

                childLink.PedigreeLinkageType = GEDCOMPedigreeLinkageType.plFoster;
                Assert.AreEqual(GEDCOMPedigreeLinkageType.plFoster, childLink.PedigreeLinkageType);
            }
        }

        private static void GEDCOMSpouseToFamilyLinkTest(GEDCOMSpouseToFamilyLink spouseLink)
        {
            Assert.IsNotNull(spouseLink.Family);
            
            using (spouseLink = GEDCOMSpouseToFamilyLink.Create(null, null, "", "") as GEDCOMSpouseToFamilyLink)
            {
                Assert.IsNotNull(spouseLink);
            }
        }

        [Test]
        public void GEDCOMSourceRecord_Tests()
        {
            // check match
            using (GEDCOMSourceRecord src1 = GEDCOMSourceRecord.Create(null, null, "", "") as GEDCOMSourceRecord)
            {
                Assert.IsNotNull(src1, "src1 != null");

                using (GEDCOMSourceRecord src2 = new GEDCOMSourceRecord(null, null, "", ""))
                {
                    Assert.IsNotNull(src2, "src2 != null");

                    Assert.AreEqual(0.0f, src1.IsMatch(null, new MatchParams()));

                    // empty records
                    Assert.AreEqual(100.0f, src1.IsMatch(src2, new MatchParams()));

                    // filled records
                    src1.FiledByEntry = "test source";
                    src2.FiledByEntry = "test source";
                    Assert.AreEqual(100.0f, src1.IsMatch(src2, new MatchParams()));
                }
            }
        }

        private static void GEDCOMSourceRecordTest(GEDCOMSourceRecord sourRec, GEDCOMIndividualRecord indiv, GEDCOMRepositoryRecord repRec)
        {
            Assert.IsNotNull(sourRec.Data);
            
            sourRec.FiledByEntry = "This is test source";
            Assert.AreEqual("This is test source", sourRec.FiledByEntry);

            //
            sourRec.Originator = new StringList("author");
            Assert.AreEqual("author", sourRec.Originator.Text.Trim());
            
            sourRec.Title = new StringList("title");
            Assert.AreEqual("title", sourRec.Title.Text.Trim());
            
            sourRec.Publication = new StringList("publication");
            Assert.AreEqual("publication", sourRec.Publication.Text.Trim());
            
            sourRec.Text = new StringList("sample");
            Assert.AreEqual("sample", sourRec.Text.Text.Trim());

            //
            sourRec.SetOriginatorArray(new string[] {"author"});
            Assert.AreEqual("author", sourRec.Originator.Text.Trim());
            
            sourRec.SetTitleArray(new string[] {"title"});
            Assert.AreEqual("title", sourRec.Title.Text.Trim());
            
            sourRec.SetPublicationArray(new string[] {"publication"});
            Assert.AreEqual("publication", sourRec.Publication.Text.Trim());
            
            sourRec.SetTextArray(new string[] {"sample"});
            Assert.AreEqual("sample", sourRec.Text.Text.Trim());
            
            //
            GEDCOMSourceCitationTest(sourRec, indiv);
            GEDCOMRepositoryCitationTest(sourRec, repRec);

            sourRec.DeleteTag("_UID");
            sourRec.DeleteTag("CHAN");
            string buf = TagStreamTest(sourRec);
            Assert.AreEqual("0 @S1@ SOUR\r\n"+
                            "1 DATA\r\n"+
                            "1 ABBR This is test source\r\n"+
                            "1 AUTH author\r\n"+
                            "1 TITL title\r\n"+
                            "1 PUBL publication\r\n"+
                            "1 TEXT sample\r\n"+
                            "1 REPO @R1@\r\n", buf);

            //
            Assert.IsFalse(sourRec.IsEmpty());
            sourRec.Clear();
            Assert.IsTrue(sourRec.IsEmpty());
        }

        [Test]
        public void GEDCOMSourceCitation_Tests()
        {
            using (GEDCOMSourceCitation srcCit = GEDCOMSourceCitation.Create(null, null, "", "") as GEDCOMSourceCitation) {
                Assert.IsNotNull(srcCit);
            }
        }

        private static void GEDCOMSourceCitationTest(GEDCOMSourceRecord sourRec, GEDCOMIndividualRecord indiv)
        {
            GEDCOMSourceCitation srcCit = indiv.AddSource(sourRec, "p2", 3);

            int idx = indiv.IndexOfSource(sourRec);
            Assert.AreEqual(0, idx);

            Assert.AreEqual("p2", srcCit.Page);
            Assert.AreEqual(3, srcCit.CertaintyAssessment);

            Assert.IsTrue(srcCit.IsPointer, "srcCit.IsPointer");

            Assert.IsFalse(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer

            srcCit.Clear();
            srcCit.Value = null;

            Assert.IsTrue(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer
            
            StringList strs = new StringList("test");
            srcCit.Description = strs;
            
            strs = srcCit.Description;
            Assert.AreEqual("test\r\n", strs.Text);
        }

        private static void GEDCOMRepositoryCitationTest(GEDCOMSourceRecord sourRec, GEDCOMRepositoryRecord repRec)
        {
            GEDCOMRepositoryCitation repCit = sourRec.AddRepository(repRec);

            Assert.IsFalse(repCit.IsEmpty(), "repCit.IsEmpty()"); // its pointer
        }

        [Test]
        public void GEDCOMResearchRecord_Tests()
        {
            using (GEDCOMResearchRecord resRec = GEDCOMResearchRecord.Create(null, null, "", "") as GEDCOMResearchRecord) {
            }
        }

        private static void GEDCOMResearchRecordTest(GEDCOMResearchRecord resRec, GEDCOMCommunicationRecord commRec, GEDCOMTaskRecord taskRec, GEDCOMGroupRecord groupRec)
        {
            Assert.IsNotNull(resRec.Communications);
            Assert.IsNotNull(resRec.Groups);
            Assert.IsNotNull(resRec.Tasks);
            
            resRec.ResearchName = "Test Research";
            Assert.AreEqual("Test Research", resRec.ResearchName);
            
            resRec.Priority = GKResearchPriority.rpNormal;
            Assert.AreEqual(GKResearchPriority.rpNormal, resRec.Priority);
            
            resRec.Status = GKResearchStatus.rsOnHold;
            Assert.AreEqual(GKResearchStatus.rsOnHold, resRec.Status);
            
            resRec.StartDate.Date = ParseDT("20.01.2013");
            Assert.AreEqual(ParseDT("20.01.2013"), resRec.StartDate.Date);
            
            resRec.StopDate.Date = ParseDT("21.01.2013");
            Assert.AreEqual(ParseDT("21.01.2013"), resRec.StopDate.Date);
            
            resRec.Percent = 33;
            Assert.AreEqual(33, resRec.Percent);

            resRec.DeleteTag("_UID");
            resRec.DeleteTag("CHAN");
            string buf = TagStreamTest(resRec);
            Assert.AreEqual("0 @RS1@ _RESEARCH\r\n"+
                            "1 NAME Test Research\r\n"+
                            "1 _PRIORITY normal\r\n"+
                            "1 _STATUS onhold\r\n"+
                            "1 _STARTDATE 20 JAN 2013\r\n"+
                            "1 _STOPDATE 21 JAN 2013\r\n"+
                            "1 _PERCENT 33\r\n", buf);

            Assert.AreEqual(-1, resRec.IndexOfCommunication(null));
            resRec.AddCommunication(commRec);
            resRec.RemoveCommunication(commRec);
            resRec.RemoveCommunication(null);

            Assert.AreEqual(-1, resRec.IndexOfTask(null));
            resRec.AddTask(taskRec);
            resRec.RemoveTask(taskRec);
            resRec.RemoveTask(null);

            Assert.AreEqual(-1, resRec.IndexOfGroup(null));
            resRec.AddGroup(groupRec);
            resRec.RemoveGroup(groupRec);
            resRec.RemoveGroup(null);

            Assert.IsFalse(resRec.IsEmpty());
            resRec.Clear();
            Assert.IsTrue(resRec.IsEmpty());
        }

        [Test]
        public void GEDCOMRepositoryRecord_Tests()
        {
            using (GEDCOMRepositoryRecord repoRec = GEDCOMRepositoryRecord.Create(_context.Tree, _context.Tree, "", "") as GEDCOMRepositoryRecord)
            {
                Assert.IsNotNull(repoRec);

                repoRec.InitNew();
                repoRec.RepositoryName = "Test Repository";
                Assert.AreEqual("Test Repository", repoRec.RepositoryName);

                Assert.IsNotNull(repoRec.Address);

                repoRec.DeleteTag("_UID");
                repoRec.DeleteTag("CHAN");
                string buf = TagStreamTest(repoRec);
                Assert.AreEqual("0 @R2@ REPO\r\n"+
                                "1 NAME Test Repository\r\n"+
                                "1 ADDR\r\n", buf);

                Assert.IsFalse(repoRec.IsEmpty());
                repoRec.Clear();
                Assert.IsTrue(repoRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMMultimediaRecord_Tests()
        {
            using (GEDCOMMultimediaRecord mmRec = GEDCOMMultimediaRecord.Create(null, null, "", "") as GEDCOMMultimediaRecord)
            {
                Assert.IsNotNull(mmRec);
            }
        }

        private static void GEDCOMMultimediaRecordTest(GEDCOMMultimediaRecord mediaRec, GEDCOMIndividualRecord indiv)
        {
            Assert.AreEqual("", mediaRec.GetFileTitle());

            mediaRec.AddTag("FILE", "", null);
            GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            Assert.IsNotNull(fileRef);
            
            fileRef.Title = "File Title 2";
            Assert.AreEqual("File Title 2", fileRef.Title);
            
            fileRef.LinkFile("sample.png");
            fileRef.MediaType = GEDCOMMediaType.mtManuscript;
            Assert.AreEqual("sample.png", fileRef.StringValue);
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPNG, fileRef.MultimediaFormat);
            Assert.AreEqual(GEDCOMMediaType.mtManuscript, fileRef.MediaType);
            
            string title = mediaRec.GetFileTitle();
            Assert.AreEqual("File Title 2", title);

            mediaRec.DeleteTag("_UID");
            mediaRec.DeleteTag("CHAN");
            string buf = TagStreamTest(mediaRec);
            Assert.AreEqual("0 @O1@ OBJE\r\n"+
                            "1 FILE sample.png\r\n"+
                            "2 TITL File Title 2\r\n"+
                            "2 FORM png\r\n"+
                            "3 TYPE manuscript\r\n", buf);
            
            GEDCOMMultimediaLinkTest(mediaRec, indiv);
            
            Assert.IsFalse(mediaRec.IsEmpty());
            mediaRec.Clear();
            Assert.IsTrue(mediaRec.IsEmpty());
        }

        [Test]
        public void GEDCOMMultimediaLink_Tests()
        {
            using (GEDCOMMultimediaLink mmLink = GEDCOMMultimediaLink.Create(null, null, "", "") as GEDCOMMultimediaLink) {
                Assert.IsNotNull(mmLink);
                Assert.IsTrue(mmLink.IsEmpty());


                // extensions
                Assert.IsFalse(mmLink.IsPrimaryCutout);
                mmLink.IsPrimaryCutout = true;
                Assert.IsTrue(mmLink.IsPrimaryCutout);

                mmLink.CutoutPosition.Value = ExtRect.Create(10, 15, 500, 600);
                ExtRect rt = mmLink.CutoutPosition.Value;
                Assert.AreEqual(10, rt.Left);
                Assert.AreEqual(15, rt.Top);
                Assert.AreEqual(500, rt.Right);
                Assert.AreEqual(600, rt.Bottom);

                Assert.AreEqual(10, mmLink.CutoutPosition.X1);
                Assert.AreEqual(15, mmLink.CutoutPosition.Y1);
                Assert.AreEqual(500, mmLink.CutoutPosition.X2);
                Assert.AreEqual(600, mmLink.CutoutPosition.Y2);

                mmLink.CutoutPosition.X1 = 10;
                mmLink.CutoutPosition.Y1 = 10;
                mmLink.CutoutPosition.X2 = 300;
                mmLink.CutoutPosition.Y2 = 400;
                Assert.AreEqual(10, mmLink.CutoutPosition.X1);
                Assert.AreEqual(10, mmLink.CutoutPosition.Y1);
                Assert.AreEqual(300, mmLink.CutoutPosition.X2);
                Assert.AreEqual(400, mmLink.CutoutPosition.Y2);

                mmLink.CutoutPosition.ParseString("11 15 576 611");
                Assert.IsFalse(mmLink.CutoutPosition.IsEmpty());
                Assert.AreEqual("11 15 576 611", mmLink.CutoutPosition.StringValue);

                mmLink.CutoutPosition.Clear();
                Assert.IsTrue(mmLink.CutoutPosition.IsEmpty());
                Assert.AreEqual("", mmLink.CutoutPosition.StringValue);
            }
        }

        private static void GEDCOMMultimediaLinkTest(GEDCOMMultimediaRecord mediaRec, GEDCOMIndividualRecord indiv)
        {
            GEDCOMMultimediaLink mmLink = indiv.AddMultimedia(mediaRec);

            Assert.IsNotNull(mmLink.FileReferences);

            mmLink.Title = "Title1";
            Assert.AreEqual("Title1", mmLink.Title);

            string buf = TagStreamTest(mmLink);
            Assert.AreEqual("1 OBJE @O1@\r\n"+
                            "2 TITL Title1\r\n", buf);

            Assert.IsTrue(mmLink.IsPointer, "mmLink.IsPointer");

            mmLink.IsPrimary = true;
            Assert.IsTrue(mmLink.IsPrimary, "mmLink.IsPrimary");

            Assert.IsFalse(mmLink.IsEmpty(), "mmLink.IsEmpty()"); // its pointer

            mmLink.Clear();
        }

        private static void GEDCOMSubmissionRecordTest(GEDCOMSubmissionRecord submRec, string submitterXRef)
        {
            submRec.FamilyFileName = "FamilyFileName";
            Assert.AreEqual("FamilyFileName", submRec.FamilyFileName);

            submRec.TempleCode = "TempleCode";
            Assert.AreEqual("TempleCode", submRec.TempleCode);

            submRec.GenerationsOfAncestors = 11;
            Assert.AreEqual(11, submRec.GenerationsOfAncestors);

            submRec.GenerationsOfDescendants = 77;
            Assert.AreEqual(77, submRec.GenerationsOfDescendants);

            submRec.OrdinanceProcessFlag = GEDCOMOrdinanceProcessFlag.opYes;
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opYes, submRec.OrdinanceProcessFlag);
            
            submRec.AddTag("SUBM", GEDCOMUtils.EncloseXRef(submitterXRef), null);
            GEDCOMSubmitterRecord subr = submRec.Submitter.Value as GEDCOMSubmitterRecord;
            Assert.IsNotNull(subr);
            
            
            Assert.IsFalse(submRec.IsEmpty());
            submRec.Clear();
            Assert.IsTrue(submRec.IsEmpty());
        }

        [Test]
        public void GEDCOMSubmitterRecord_Tests()
        {
            using (GEDCOMSubmitterRecord subrRec = GEDCOMSubmitterRecord.Create(null, null, "", "") as GEDCOMSubmitterRecord) {
                subrRec.Name.StringValue = "Test Submitter";
                Assert.AreEqual("Test Submitter", subrRec.Name.StringValue);

                subrRec.RegisteredReference = "regref";
                Assert.AreEqual("regref", subrRec.RegisteredReference);

                subrRec.AddTag("LANG", "RUS", null);
                Assert.AreEqual("RUS", subrRec.Languages[0].StringValue);

                subrRec.SetLanguage(1, "ENG");
                Assert.AreEqual("ENG", subrRec.Languages[1].StringValue);

                Assert.IsNotNull(subrRec.Address);


                Assert.IsFalse(subrRec.IsEmpty());
                subrRec.Clear();
                Assert.IsTrue(subrRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMCommunicationRecord_Test()
        {
            GEDCOMIndividualRecord iRec = _context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            using (GEDCOMCommunicationRecord comRec = GEDCOMCommunicationRecord.Create(_context.Tree, _context.Tree, "", "") as GEDCOMCommunicationRecord) {
                comRec.CommName = "Test Communication";
                Assert.AreEqual("Test Communication", comRec.CommName);

                comRec.CommunicationType = GKCommunicationType.ctFax;
                Assert.AreEqual(GKCommunicationType.ctFax, comRec.CommunicationType);

                comRec.Date.Date = ParseDT("23.01.2013");
                Assert.AreEqual(ParseDT("23.01.2013"), comRec.Date.Date);

                comRec.SetCorresponder(GKCommunicationDir.cdFrom, iRec);

                GKCommunicationDir dir;
                GEDCOMIndividualRecord corr;
                comRec.GetCorresponder(out dir, out corr);
                Assert.AreEqual(GKCommunicationDir.cdFrom, dir);
                Assert.AreEqual(iRec, corr);

                comRec.SetCorresponder(GKCommunicationDir.cdTo, iRec);
                comRec.GetCorresponder(out dir, out corr);
                Assert.AreEqual(GKCommunicationDir.cdTo, dir);
                Assert.AreEqual(iRec, corr);

                Assert.IsFalse(comRec.IsEmpty());
                comRec.Clear();
                Assert.IsTrue(comRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMLocationRecord_Test()
        {
            using (GEDCOMLocationRecord locRec = GEDCOMLocationRecord.Create(null, null, "", "") as GEDCOMLocationRecord) {
                locRec.LocationName = "Test Location";
                Assert.AreEqual("Test Location", locRec.LocationName);

                Assert.IsNotNull(locRec.Map);

                Assert.IsFalse(locRec.IsEmpty());
                locRec.Clear();
                Assert.IsTrue(locRec.IsEmpty());
            }
        }

        private static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "dd.MM.yyyy", CultureInfo.InvariantCulture);
        }

        [Test]
        public void GEDCOMTaskRecord_Tests()
        {
            GEDCOMIndividualRecord iRec = _context.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            GEDCOMFamilyRecord famRec = _context.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            Assert.IsNotNull(famRec);

            GEDCOMSourceRecord srcRec = _context.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            Assert.IsNotNull(srcRec);

            using (GEDCOMTaskRecord taskRec = GEDCOMTaskRecord.Create(_context.Tree, _context.Tree, "", "") as GEDCOMTaskRecord)
            {
                Assert.IsNotNull(taskRec);

                GKGoalType gType;
                GEDCOMRecord gRec;

                taskRec.Priority = GKResearchPriority.rpNormal;
                Assert.AreEqual(GKResearchPriority.rpNormal, taskRec.Priority);

                taskRec.StartDate.Date = ParseDT("20.01.2013");
                Assert.AreEqual(ParseDT("20.01.2013"), taskRec.StartDate.Date);

                taskRec.StopDate.Date = ParseDT("21.01.2013");
                Assert.AreEqual(ParseDT("21.01.2013"), taskRec.StopDate.Date);

                taskRec.Goal = "Test Goal";
                Assert.AreEqual("Test Goal", taskRec.Goal);
                taskRec.GetTaskGoal(out gType, out gRec);
                Assert.AreEqual(GKGoalType.gtOther, gType);
                Assert.AreEqual(null, gRec);

                taskRec.Goal = iRec.XRef;
                taskRec.GetTaskGoal(out gType, out gRec);
                Assert.AreEqual(GKGoalType.gtIndividual, gType);
                Assert.AreEqual(iRec, gRec);

                taskRec.Goal = famRec.XRef;
                taskRec.GetTaskGoal(out gType, out gRec);
                Assert.AreEqual(GKGoalType.gtFamily, gType);
                Assert.AreEqual(famRec, gRec);

                taskRec.Goal = srcRec.XRef;
                taskRec.GetTaskGoal(out gType, out gRec);
                Assert.AreEqual(GKGoalType.gtSource, gType);
                Assert.AreEqual(srcRec, gRec);

                Assert.IsFalse(taskRec.IsEmpty());
                taskRec.Clear();
                Assert.IsTrue(taskRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMNotes_Tests()
        {
            using (GEDCOMNotes notes = GEDCOMNotes.Create(null, null, "", "") as GEDCOMNotes) {
                Assert.IsTrue(notes.IsEmpty());
                notes.Notes = new StringList("Test note");
                Assert.IsFalse(notes.IsEmpty());
                Assert.AreEqual("Test note\r\n", notes.Notes.Text);
            }
        }

        [Test]
        public void GEDCOMNoteRecord_Tests()
        {
            using (GEDCOMNoteRecord noteRec = GEDCOMNoteRecord.Create(null, null, "", "") as GEDCOMNoteRecord) {
                noteRec.AddNoteText("text");
                Assert.AreEqual("text", noteRec.Note.Text.Trim());

                Assert.Throws(typeof(ArgumentNullException), () => { noteRec.SetNoteText(null); });

                noteRec.SetNoteText("Test text");
                Assert.AreEqual("Test text", noteRec.Note.Text.Trim());

                using (GEDCOMNoteRecord noteRec2 = GEDCOMNoteRecord.Create(null, null, "", "") as GEDCOMNoteRecord) {
                    noteRec2.SetNoteText("Test text");
                    Assert.AreEqual("Test text", noteRec2.Note.Text.Trim());

                    Assert.AreEqual(100.0f, noteRec.IsMatch(noteRec2, new MatchParams()));

                    Assert.IsFalse(noteRec2.IsEmpty());
                    noteRec2.Clear();
                    Assert.IsTrue(noteRec2.IsEmpty());

                    Assert.AreEqual(0.0f, noteRec.IsMatch(noteRec2, new MatchParams()));

                    Assert.AreEqual(0.0f, noteRec.IsMatch(null, new MatchParams()));
                }
            }
        }

        private static void GEDCOMNoteRecordTest(GEDCOMNoteRecord noteRec, GEDCOMIndividualRecord indiv)
        {
            noteRec.SetNotesArray(new string[] { "This", "notes", "test" });
            
            string ctx = GKUtils.MergeStrings(noteRec.Note);
            Assert.AreEqual("This notes test", ctx);

            noteRec.Note = new StringList("This\r\nnotes2\r\ntest2");
            Assert.AreEqual("This", noteRec.Note[0]);
            Assert.AreEqual("notes2", noteRec.Note[1]);
            Assert.AreEqual("test2", noteRec.Note[2]);
            
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.MergeStrings(null); });
            
            ctx = GKUtils.MergeStrings(noteRec.Note);
            Assert.AreEqual("This notes2 test2", ctx);
            
            noteRec.Clear();
            noteRec.AddNoteText("Test text");
            Assert.AreEqual("Test text", noteRec.Note.Text.Trim());
            
            GEDCOMNotesTest(noteRec, indiv);

            Assert.IsFalse(noteRec.IsEmpty());
            noteRec.Clear();
            Assert.IsTrue(noteRec.IsEmpty());
        }

        private static void GEDCOMNotesTest(GEDCOMNoteRecord noteRec, GEDCOMIndividualRecord indiv)
        {
            GEDCOMNotes notes = indiv.AddNote(noteRec);
            
            Assert.AreEqual(notes.Notes.Text, noteRec.Note.Text);
            
            Assert.IsTrue(notes.IsPointer, "notes.IsPointer");
            
            Assert.IsFalse(notes.IsEmpty()); // its pointer
            
            notes.Clear();
        }
        
        #endregion
        
        #region Private Aux functions
        
        private static string TagStreamTest(GEDCOMTag tag)
        {
            string result;
            
            using (MemoryStream stm = new MemoryStream()) {
                using (StreamWriter fs = new StreamWriter(stm)) {
                    tag.SaveToStream(fs);
                    
                    fs.Flush();
                    
                    result = Encoding.ASCII.GetString(stm.ToArray());
                }
            }
            
            return result;
        }
        
        #endregion
    }
}
