/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System.Text;
using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GEDCOMUtilsTests
    {
        [Test]
        public void Test_TrimLeft()
        {
            string str;
            str = GEDCOMUtils.TrimLeft("	test1");
            Assert.AreEqual("test1", str);

            str = GEDCOMUtils.TrimLeft(null);
            Assert.AreEqual("", str);
        }

        [Test]
        public void Test_TrimRight()
        {
            string str;
            str = GEDCOMUtils.TrimRight("test2		");
            Assert.AreEqual("test2", str);

            str = GEDCOMUtils.TrimRight(null);
            Assert.AreEqual("", str);
        }

        [Test]
        public void Test_GEDCOMObject()
        {
            GEDCOMObject obj = new GEDCOMObject();
            obj.ExtData = this;
            Assert.AreEqual(obj.ExtData, this);
            obj.Dispose();
        }

        [Test]
        public void Test_XRef_CleanEnclose()
        {
            Assert.AreEqual("I12", GEDCOMUtils.CleanXRef("@I12@"), "CleanXRef(@I12@)");
            Assert.AreEqual("@I12@", GEDCOMUtils.EncloseXRef("I12"), "EncloseXRef(I12)");
        }

        [Test]
        public void Test_ExtractDelimiter()
        {
            string res;
            res = GEDCOMUtils.ExtractDelimiter(" 12345 efgh", 0);
            Assert.AreEqual("12345 efgh", res);

            res = GEDCOMUtils.ExtractDelimiter("    abrvalg", 2);
            Assert.AreEqual("  abrvalg", res);
        }

        [Test]
        public void Test_GEDCOMEnumSx()
        {
            Assert.AreEqual("M", GEDCOMUtils.GetSexStr(GEDCOMSex.svMale), "GetSexStr(svMale)");
            Assert.AreEqual("F", GEDCOMUtils.GetSexStr(GEDCOMSex.svFemale), "GetSexStr(svFemale)");
            Assert.AreEqual("U", GEDCOMUtils.GetSexStr(GEDCOMSex.svUndetermined), "GetSexStr(svUndetermined)");
            Assert.AreEqual("", GEDCOMUtils.GetSexStr(GEDCOMSex.svNone), "GetSexStr(svNone)");

            Assert.AreEqual(GEDCOMSex.svMale, GEDCOMUtils.GetSexVal("M"), "GetSexVal(svMale)");
            Assert.AreEqual(GEDCOMSex.svFemale, GEDCOMUtils.GetSexVal("F"), "GetSexVal(svFemale)");
            Assert.AreEqual(GEDCOMSex.svUndetermined, GEDCOMUtils.GetSexVal("U"), "GetSexVal(svUndetermined)");
            Assert.AreEqual(GEDCOMSex.svNone, GEDCOMUtils.GetSexVal(""), "GetSexVal(svNone)");
            Assert.AreEqual(GEDCOMSex.svNone, GEDCOMUtils.GetSexVal("unk"), "GetSexVal(unk)");
        }

        [Test]
        public void Test_GEDCOMEnumRP()
        {
            Assert.AreEqual(GKResearchPriority.rpLow, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpLow)));
            Assert.AreEqual(GKResearchPriority.rpNormal, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpNormal)));
            Assert.AreEqual(GKResearchPriority.rpHigh, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpHigh)));
            Assert.AreEqual(GKResearchPriority.rpTop, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpTop)));
            Assert.AreEqual(GKResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GKResearchPriority.rpNone)));
            Assert.AreEqual(GKResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumOPF()
        {
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opYes, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag.opYes)));
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opNo, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag.opNo)));
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag.opNone)));
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumRS()
        {
            Assert.AreEqual(GKResearchStatus.rsInProgress, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsInProgress)));
            Assert.AreEqual(GKResearchStatus.rsOnHold, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsOnHold)));
            Assert.AreEqual(GKResearchStatus.rsProblems, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsProblems)));
            Assert.AreEqual(GKResearchStatus.rsCompleted, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsCompleted)));
            Assert.AreEqual(GKResearchStatus.rsWithdrawn, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsWithdrawn)));
            Assert.AreEqual(GKResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GKResearchStatus.rsDefined)));
            Assert.AreEqual(GKResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(""));
            Assert.AreEqual(GKResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEncoding()
        {
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

            Assert.AreEqual(Encoding.GetEncoding(1251), GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csASCII));
            Assert.AreEqual(Encoding.Unicode, GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csUNICODE));
            Assert.AreEqual(Encoding.UTF8, GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csUTF8));
            Assert.IsInstanceOf(typeof(AnselEncoding), GEDCOMUtils.GetEncodingByCharacterSet(GEDCOMCharacterSet.csANSEL));
        }

        [Test]
        public void Test_GEDCOMEnumNT()
        {
            Assert.AreEqual(GEDCOMNameType.ntNone, GEDCOMUtils.GetNameTypeVal("unk"));
            Assert.AreEqual(GEDCOMNameType.ntNone, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntNone)));
            Assert.AreEqual(GEDCOMNameType.ntAka, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntAka)));
            Assert.AreEqual(GEDCOMNameType.ntBirth, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntBirth)));
            Assert.AreEqual(GEDCOMNameType.ntImmigrant, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntImmigrant)));
            Assert.AreEqual(GEDCOMNameType.ntMaiden, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntMaiden)));
            Assert.AreEqual(GEDCOMNameType.ntMarried, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GEDCOMNameType.ntMarried)));
        }

        [Test]
        public void Test_GEDCOMEnumCT()
        {
            Assert.AreEqual(GKCommunicationType.ctCall, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctCall)));
            Assert.AreEqual(GKCommunicationType.ctEMail, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctEMail)));
            Assert.AreEqual(GKCommunicationType.ctFax, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctFax)));
            Assert.AreEqual(GKCommunicationType.ctLetter, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctLetter)));
            Assert.AreEqual(GKCommunicationType.ctTape, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctTape)));
            Assert.AreEqual(GKCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GKCommunicationType.ctVisit)));
            Assert.AreEqual(GKCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(""));
            Assert.AreEqual(GKCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumCLS()
        {
            Assert.AreEqual(GEDCOMChildLinkageStatus.clChallenged, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clChallenged)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clDisproven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clDisproven)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clProven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clProven)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GEDCOMChildLinkageStatus.clNone)));
            Assert.AreEqual(GEDCOMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumPLT()
        {
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plAdopted, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plAdopted)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plBirth, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plBirth)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plFoster, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plFoster)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plSealing, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plSealing)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType.plNone)));
            Assert.AreEqual(GEDCOMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumRestr()
        {
            Assert.AreEqual(GEDCOMRestriction.rnConfidential, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnConfidential)));
            Assert.AreEqual(GEDCOMRestriction.rnLocked, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnLocked)));
            Assert.AreEqual(GEDCOMRestriction.rnPrivacy, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnPrivacy)));
            Assert.AreEqual(GEDCOMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GEDCOMRestriction.rnNone)));
            Assert.AreEqual(GEDCOMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumMT()
        {
            Assert.AreEqual(GEDCOMMediaType.mtUnknown, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GEDCOMMediaType.mtUnknown)));
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
        }

        [Test]
        public void Test_GEDCOMEnumMF()
        {
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
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPSD, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfPSD)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPDF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfPDF)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMP4, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfMP4)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfOGV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfOGV)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMKA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfMKA)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfWMV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfWMV)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMKV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfMKV)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMOV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GEDCOMMultimediaFormat.mfMOV)));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfUnknown, GEDCOMUtils.GetMultimediaFormatVal("xxx"));
        }

        [Test]
        public void Test_GEDCOMEnumSSDS()
        {
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
        }

        [Test]
        public void Test_GEDCOMEnumBDS()
        {
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsNone, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsNone)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsChild, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsChild)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsCompleted, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsCompleted)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsExcluded, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsExcluded)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsPre1970, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsPre1970)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsStillborn, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsStillborn)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsSubmitted, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsSubmitted)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsUncleared, GEDCOMUtils.GetBaptismDateStatusVal(GEDCOMUtils.GetBaptismDateStatusStr(GEDCOMBaptismDateStatus.bdsUncleared)));
            Assert.AreEqual(GEDCOMBaptismDateStatus.bdsNone, GEDCOMUtils.GetBaptismDateStatusVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumEDS()
        {
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
        }

        [Test]
        public void Test_GEDCOMEnumCSDS()
        {
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsNone, GEDCOMUtils.GetChildSealingDateStatusVal("unk"));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsNone, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsNone)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsBIC, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsBIC)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsExcluded, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsExcluded)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsPre1970, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsPre1970)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsStillborn, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsStillborn)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsSubmitted, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsSubmitted)));
            Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsUncleared, GEDCOMUtils.GetChildSealingDateStatusVal(GEDCOMUtils.GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus.cdsUncleared)));
        }
    }
}
