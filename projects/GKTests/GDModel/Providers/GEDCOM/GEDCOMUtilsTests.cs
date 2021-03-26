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
using System.Text;
using GDModel;
using NUnit.Framework;

namespace GDModel.Providers.GEDCOM
{
    [TestFixture]
    public class GEDCOMUtilsTests
    {
        [Test]
        public void Test_GetSignByRecord()
        {
            Assert.AreEqual(string.Empty, GEDCOMUtils.GetSignByRecord(null));
        }

        [Test]
        public void Test_GetXRefNumber()
        {
            Assert.AreEqual(-1, GEDCOMUtils.GetXRefNumber(""));
        }

        [Test]
        public void Test_Trim()
        {
            string str;
            str = GEDCOMUtils.Trim("    test1");
            Assert.AreEqual("test1", str);

            str = GEDCOMUtils.Trim("test2        ");
            Assert.AreEqual("test2", str);

            str = GEDCOMUtils.Trim("   test3        ");
            Assert.AreEqual("test3", str);

            str = GEDCOMUtils.Trim(null);
            Assert.AreEqual("", str);
        }

        [Test]
        public void Test_XRef_CleanEnclose()
        {
            Assert.AreEqual("I12", GEDCOMUtils.CleanXRef("@I12@"), "CleanXRef(@I12@)");
            Assert.AreEqual("@I12@", GEDCOMUtils.EncloseXRef("I12"), "EncloseXRef(I12)");
        }

        [Test]
        public void Test_GEDCOMEnumSx()
        {
            Assert.AreEqual("M", GEDCOMUtils.GetSexStr(GDMSex.svMale), "GetSexStr(svMale)");
            Assert.AreEqual("F", GEDCOMUtils.GetSexStr(GDMSex.svFemale), "GetSexStr(svFemale)");
            Assert.AreEqual("U", GEDCOMUtils.GetSexStr(GDMSex.svUnknown), "GetSexStr(svUndetermined)");
            Assert.AreEqual("X", GEDCOMUtils.GetSexStr(GDMSex.svIntersex), "GetSexStr(svIntersex)");

            Assert.AreEqual(GDMSex.svMale, GEDCOMUtils.GetSexVal("M"), "GetSexVal(svMale)");
            Assert.AreEqual(GDMSex.svFemale, GEDCOMUtils.GetSexVal("F"), "GetSexVal(svFemale)");
            Assert.AreEqual(GDMSex.svUnknown, GEDCOMUtils.GetSexVal("U"), "GetSexVal(svUndetermined)");
            Assert.AreEqual(GDMSex.svUnknown, GEDCOMUtils.GetSexVal(""), "GetSexVal(svNone)");
            Assert.AreEqual(GDMSex.svUnknown, GEDCOMUtils.GetSexVal("unk"), "GetSexVal(unk)");
            Assert.AreEqual(GDMSex.svIntersex, GEDCOMUtils.GetSexVal("X"));
        }

        [Test]
        public void Test_GEDCOMEnumRP()
        {
            Assert.AreEqual(GDMResearchPriority.rpLow, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GDMResearchPriority.rpLow)));
            Assert.AreEqual(GDMResearchPriority.rpNormal, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GDMResearchPriority.rpNormal)));
            Assert.AreEqual(GDMResearchPriority.rpHigh, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GDMResearchPriority.rpHigh)));
            Assert.AreEqual(GDMResearchPriority.rpTop, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GDMResearchPriority.rpTop)));
            Assert.AreEqual(GDMResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(GDMResearchPriority.rpNone)));
            Assert.AreEqual(GDMResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumOPF()
        {
            Assert.AreEqual(GDMOrdinanceProcessFlag.opYes, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GDMOrdinanceProcessFlag.opYes)));
            Assert.AreEqual(GDMOrdinanceProcessFlag.opNo, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GDMOrdinanceProcessFlag.opNo)));
            Assert.AreEqual(GDMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(GDMOrdinanceProcessFlag.opNone)));
            Assert.AreEqual(GDMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumRS()
        {
            Assert.AreEqual(GDMResearchStatus.rsInProgress, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GDMResearchStatus.rsInProgress)));
            Assert.AreEqual(GDMResearchStatus.rsOnHold, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GDMResearchStatus.rsOnHold)));
            Assert.AreEqual(GDMResearchStatus.rsProblems, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GDMResearchStatus.rsProblems)));
            Assert.AreEqual(GDMResearchStatus.rsCompleted, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GDMResearchStatus.rsCompleted)));
            Assert.AreEqual(GDMResearchStatus.rsWithdrawn, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GDMResearchStatus.rsWithdrawn)));
            Assert.AreEqual(GDMResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(GDMResearchStatus.rsDefined)));
            Assert.AreEqual(GDMResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(""));
            Assert.AreEqual(GDMResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal("unk"));
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
            Assert.AreEqual(GDMNameType.ntNone, GEDCOMUtils.GetNameTypeVal("unk"));
            Assert.AreEqual(GDMNameType.ntNone, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GDMNameType.ntNone)));
            Assert.AreEqual(GDMNameType.ntAka, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GDMNameType.ntAka)));
            Assert.AreEqual(GDMNameType.ntBirth, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GDMNameType.ntBirth)));
            Assert.AreEqual(GDMNameType.ntImmigrant, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GDMNameType.ntImmigrant)));
            Assert.AreEqual(GDMNameType.ntMaiden, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GDMNameType.ntMaiden)));
            Assert.AreEqual(GDMNameType.ntMarried, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(GDMNameType.ntMarried)));
        }

        [Test]
        public void Test_GEDCOMEnumCT()
        {
            Assert.AreEqual(GDMCommunicationType.ctCall, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GDMCommunicationType.ctCall)));
            Assert.AreEqual(GDMCommunicationType.ctEMail, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GDMCommunicationType.ctEMail)));
            Assert.AreEqual(GDMCommunicationType.ctFax, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GDMCommunicationType.ctFax)));
            Assert.AreEqual(GDMCommunicationType.ctLetter, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GDMCommunicationType.ctLetter)));
            Assert.AreEqual(GDMCommunicationType.ctTape, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GDMCommunicationType.ctTape)));
            Assert.AreEqual(GDMCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(GDMCommunicationType.ctVisit)));
            Assert.AreEqual(GDMCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(""));
            Assert.AreEqual(GDMCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumCLS()
        {
            Assert.AreEqual(GDMChildLinkageStatus.clChallenged, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GDMChildLinkageStatus.clChallenged)));
            Assert.AreEqual(GDMChildLinkageStatus.clDisproven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GDMChildLinkageStatus.clDisproven)));
            Assert.AreEqual(GDMChildLinkageStatus.clProven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GDMChildLinkageStatus.clProven)));
            Assert.AreEqual(GDMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(GDMChildLinkageStatus.clNone)));
            Assert.AreEqual(GDMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumPLT()
        {
            Assert.AreEqual(GDMPedigreeLinkageType.plAdopted, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GDMPedigreeLinkageType.plAdopted)));
            Assert.AreEqual(GDMPedigreeLinkageType.plBirth, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GDMPedigreeLinkageType.plBirth)));
            Assert.AreEqual(GDMPedigreeLinkageType.plFoster, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GDMPedigreeLinkageType.plFoster)));
            Assert.AreEqual(GDMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(GDMPedigreeLinkageType.plNone)));
            Assert.AreEqual(GDMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumRestr()
        {
            Assert.AreEqual(GDMRestriction.rnConfidential, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GDMRestriction.rnConfidential)));
            Assert.AreEqual(GDMRestriction.rnLocked, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GDMRestriction.rnLocked)));
            Assert.AreEqual(GDMRestriction.rnPrivacy, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GDMRestriction.rnPrivacy)));
            Assert.AreEqual(GDMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(GDMRestriction.rnNone)));
            Assert.AreEqual(GDMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal("unk"));
        }

        [Test]
        public void Test_GEDCOMEnumLang()
        {
            Assert.AreEqual(GDMLanguageID.Unknown, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Unknown)));

            Assert.AreEqual(GDMLanguageID.AncientGreek, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.AncientGreek)));

            Assert.AreEqual(GDMLanguageID.Esperanto, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Esperanto)));

            Assert.AreEqual(GDMLanguageID.Russian, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Russian)));

            Assert.AreEqual(GDMLanguageID.Sumerian, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Sumerian)));

            Assert.AreEqual(GDMLanguageID.Urdu, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Urdu)));
            Assert.AreEqual(GDMLanguageID.Vietnamese, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Vietnamese)));
            Assert.AreEqual(GDMLanguageID.Wendic, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Wendic)));

            Assert.AreEqual(GDMLanguageID.Yiddish, GEDCOMUtils.GetLanguageVal(GEDCOMUtils.GetLanguageStr(GDMLanguageID.Yiddish)));

            for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                string strVal = GEDCOMUtils.GetLanguageStr(lid);
                GDMLanguageID val = GEDCOMUtils.GetLanguageVal(strVal);
                Assert.AreEqual(lid, val);
            }
        }

        [Test]
        public void Test_GEDCOMEnumMT()
        {
            Assert.AreEqual(GDMMediaType.mtUnknown, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtUnknown)));
            Assert.AreEqual(GDMMediaType.mtAudio, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtAudio)));
            Assert.AreEqual(GDMMediaType.mtBook, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtBook)));
            Assert.AreEqual(GDMMediaType.mtCard, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtCard)));
            Assert.AreEqual(GDMMediaType.mtElectronic, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtElectronic)));
            Assert.AreEqual(GDMMediaType.mtFiche, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtFiche)));
            Assert.AreEqual(GDMMediaType.mtFilm, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtFilm)));
            Assert.AreEqual(GDMMediaType.mtMagazine, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtMagazine)));
            Assert.AreEqual(GDMMediaType.mtManuscript, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtManuscript)));
            Assert.AreEqual(GDMMediaType.mtMap, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtMap)));
            Assert.AreEqual(GDMMediaType.mtNewspaper, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtNewspaper)));
            Assert.AreEqual(GDMMediaType.mtPhoto, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtPhoto)));
            Assert.AreEqual(GDMMediaType.mtTombstone, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtTombstone)));
            Assert.AreEqual(GDMMediaType.mtVideo, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(GDMMediaType.mtVideo)));
            Assert.AreEqual(GDMMediaType.mtUnknown, GEDCOMUtils.GetMediaTypeVal("sample"));
        }

        [Test]
        public void Test_GEDCOMEnumMF()
        {
            Assert.AreEqual(GDMMultimediaFormat.mfNone, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfNone)));
            Assert.AreEqual(GDMMultimediaFormat.mfBMP, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfBMP)));
            Assert.AreEqual(GDMMultimediaFormat.mfGIF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfGIF)));
            Assert.AreEqual(GDMMultimediaFormat.mfJPG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfJPG)));
            Assert.AreEqual(GDMMultimediaFormat.mfOLE, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfOLE)));
            Assert.AreEqual(GDMMultimediaFormat.mfPCX, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfPCX)));
            Assert.AreEqual(GDMMultimediaFormat.mfTIF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfTIF)));
            Assert.AreEqual(GDMMultimediaFormat.mfWAV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfWAV)));
            Assert.AreEqual(GDMMultimediaFormat.mfTXT, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfTXT)));
            Assert.AreEqual(GDMMultimediaFormat.mfRTF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfRTF)));
            Assert.AreEqual(GDMMultimediaFormat.mfAVI, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfAVI)));
            Assert.AreEqual(GDMMultimediaFormat.mfTGA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfTGA)));
            Assert.AreEqual(GDMMultimediaFormat.mfPNG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfPNG)));
            Assert.AreEqual(GDMMultimediaFormat.mfMPG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfMPG)));
            Assert.AreEqual(GDMMultimediaFormat.mfHTM, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfHTM)));
            Assert.AreEqual(GDMMultimediaFormat.mfRAW, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfRAW)));
            Assert.AreEqual(GDMMultimediaFormat.mfMP3, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfMP3)));
            Assert.AreEqual(GDMMultimediaFormat.mfWMA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfWMA)));
            Assert.AreEqual(GDMMultimediaFormat.mfPSD, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfPSD)));
            Assert.AreEqual(GDMMultimediaFormat.mfPDF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfPDF)));
            Assert.AreEqual(GDMMultimediaFormat.mfMP4, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfMP4)));
            Assert.AreEqual(GDMMultimediaFormat.mfOGV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfOGV)));
            Assert.AreEqual(GDMMultimediaFormat.mfMKA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfMKA)));
            Assert.AreEqual(GDMMultimediaFormat.mfWMV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfWMV)));
            Assert.AreEqual(GDMMultimediaFormat.mfMKV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfMKV)));
            Assert.AreEqual(GDMMultimediaFormat.mfMOV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(GDMMultimediaFormat.mfMOV)));
            Assert.AreEqual(GDMMultimediaFormat.mfUnknown, GEDCOMUtils.GetMultimediaFormatVal("xxx"));
        }

        [Test]
        public void Test_GEDCOMEnumMarriageStatus()
        {
            Assert.AreEqual(GDMMarriageStatus.Unknown, GEDCOMUtils.GetMarriageStatusVal("unk"));
            Assert.AreEqual(GDMMarriageStatus.MarrRegistered, GEDCOMUtils.GetMarriageStatusVal(GEDCOMUtils.GetMarriageStatusStr(GDMMarriageStatus.MarrRegistered)));
            Assert.AreEqual(GDMMarriageStatus.MarrNotRegistered, GEDCOMUtils.GetMarriageStatusVal(GEDCOMUtils.GetMarriageStatusStr(GDMMarriageStatus.MarrNotRegistered)));
            Assert.AreEqual(GDMMarriageStatus.MarrDivorced, GEDCOMUtils.GetMarriageStatusVal(GEDCOMUtils.GetMarriageStatusStr(GDMMarriageStatus.MarrDivorced)));
        }

        [Test]
        public void Test_ParseGEDCOMPointer()
        {
            using (var ptr = new GDMPointer(null)) {
                string remainder = ptr.ParseString("  @I1111@ test");
                Assert.AreEqual("I1111", ptr.XRef);
                Assert.AreEqual(" test", remainder);

                remainder = ptr.ParseString("  @#I1111@ test21");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("@#I1111@ test21", remainder);

                remainder = ptr.ParseString("    test2");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("test2", remainder);

                remainder = ptr.ParseString("    ");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("", remainder);

                remainder = ptr.ParseString("");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("", remainder);
            }
        }

        [Test]
        public void Test_ParseGEDCOMTime()
        {
            using (GDMTime time = new GDMTime(null)) {
                Assert.IsNotNull(time, "time != null");
                time.ParseString("20:20:20.100");

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

        // Line format: <level>_<@xref@>_<tag>_<value> (for test's purpose)
        private static int ParseTag(string str, out int tagLevel, out string tagXRef, out string tagName, out string tagValue)
        {
            var strTok = new GEDCOMParser(str, false);
            return GEDCOMUtils.ParseTag(strTok, out tagLevel, out tagXRef, out tagName, out tagValue);
        }

        [Test]
        public void Test_ParseTag()
        {
            string str;
            int tagLevel2, res2;
            string tagXRef2, tagName2, tagValue2;

            str = "0 HEAD";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("HEAD", tagName2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(2, res2);

            str = "0 @SUB1@ SUBM";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("SUB1", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(3, res2);

            str = "0 @SUB1@ SUBM testVal";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("SUB1", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("testVal", tagValue2);
            Assert.AreEqual(4, res2);

            str = "1 SUBM @SUB1@";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(1, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("@SUB1@", tagValue2);
            Assert.AreEqual(3, res2);

            str = "    1 SUBM @SUB1@";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(1, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("@SUB1@", tagValue2);
            Assert.AreEqual(3, res2);

            str = "2 DATE FROM 20 JAN 1979 TO 15 MAY 2012";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(2, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual(GEDCOMTagName.DATE, tagName2);
            Assert.AreEqual("FROM 20 JAN 1979 TO 15 MAY 2012", tagValue2);
            Assert.AreEqual(3, res2);


            str = "    test test test (FTB line with error)";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual("    test test test (FTB line with error)", tagValue2);
            Assert.AreEqual(-1, res2);

            str = "        ";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("", tagName2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(-2, res2);

            str = "";
            res2 = ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(-2, res2);
        }

        [Test]
        public void Test_ParseXRefPointer()
        {
            string xref;
            string rest = GEDCOMUtils.ParseXRefPointer(" @I001@", out xref);
            Assert.AreEqual("", rest);
            Assert.AreEqual("I001", xref);

            rest = GEDCOMUtils.ParseXRefPointer(" ptr text", out xref);
            Assert.AreEqual("ptr text", rest);
            Assert.AreEqual("", xref);

            rest = GEDCOMUtils.ParseXRefPointer(" ", out xref);
            Assert.AreEqual("", rest);
            Assert.AreEqual("", xref);

            rest = GEDCOMUtils.ParseXRefPointer("", out xref);
            Assert.AreEqual("", rest);
            Assert.AreEqual("", xref);
        }

        [Test]
        public void Test_SetTagStringsL()
        {
            var tag = new GDMValueTag(null, GEDCOMTagsTable.Lookup("TEST"), "");
            Assert.IsNotNull(tag);

            // very long string, 248"A" and " BBB BBBB"
            var strings = new GDMLines( "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA BBB BBBB" );

            GEDCOMUtils.SetTagStrings(null, strings);

            GEDCOMUtils.SetTagStrings(tag, strings);

            Assert.AreEqual(248, tag.StringValue.Length);

            var strList = GEDCOMUtils.GetTagStrings(tag);
            Assert.IsNotNull(strList);
            Assert.AreEqual(1, strList.Count);
            Assert.AreEqual(strings.Text, strList.Text);
        }

        [Test]
        public void Test_GetGeoCoord()
        {
            Assert.AreEqual(0.0, GEDCOMUtils.GetGeoCoord(null, GEDCOMGeoCoord.Lati));

            Assert.AreEqual(+0.005, GEDCOMUtils.GetGeoCoord("N0.005", GEDCOMGeoCoord.Lati));
            Assert.AreEqual(+0.005, GEDCOMUtils.GetGeoCoord("E0.005", GEDCOMGeoCoord.Lati));
            Assert.AreEqual(-0.005, GEDCOMUtils.GetGeoCoord("S0.005", GEDCOMGeoCoord.Lati));
            Assert.AreEqual(-0.005, GEDCOMUtils.GetGeoCoord("W0.005", GEDCOMGeoCoord.Lati));
        }
    }
}
