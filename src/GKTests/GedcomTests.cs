using System;
using System.IO;
using System.Text;

using ExtUtils;
using GedCom551;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class GedcomTests
	{
		#region True Tests

		[Test]
		public void GEDCOMUtils_Tests()
		{
			GEDCOMObject obj = new GEDCOMObject();
			obj.ExtData = this;
			Assert.AreEqual(obj.ExtData, this);
			SysUtils.Free(obj);

			//
			
			GEDCOMUtils.TagProperties props = GEDCOMUtils.GetTagProps("ADDR");
			Assert.IsTrue(props.EmptySkip);
			
			props = GEDCOMUtils.GetTagProps("test");
			Assert.IsFalse(props.EmptySkip);
			
			//
			
			Assert.AreEqual("I12", GEDCOMUtils.CleanXRef("@I12@"), "CleanXRef(@I12@)");
			Assert.AreEqual("@I12@", GEDCOMUtils.EncloseXRef("I12"), "EncloseXRef(I12)");

			//
			string s1 = "abcd 12345 efgh";
			string s2, st;
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

			try
			{
				s2 = GEDCOMUtils.ExtractNumber("num", out N, false, 2222);
			}
			catch (EGEDCOMException)
			{
			}
			catch (Exception)
			{
				throw;
			}

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
			
			try
			{
				s2 = GEDCOMUtils.ExtractXRef("", out xref, false, "test");
			}
			catch (EGEDCOMException)
			{
			}
			catch (Exception)
			{
				throw;
			}

			try
			{
				s2 = GEDCOMUtils.ExtractXRef("@sample", out xref, false, "test");
			}
			catch (EGEDCOMException)
			{
			}
			catch (Exception)
			{
				throw;
			}

			//
			Assert.IsFalse(GEDCOMUtils.IsDigit('F'), "IsDigit(F)");
			Assert.IsTrue(GEDCOMUtils.IsDigit('9'), "IsDigit(9)");

			Assert.IsFalse(GEDCOMUtils.IsDigits("f09"), "IsDigits(f09)");
			Assert.IsTrue(GEDCOMUtils.IsDigits("99"), "IsDigits(99)");

			Assert.AreEqual("M", GEDCOMUtils.GetSexStr(TGEDCOMSex.svMale), "GetSexStr(svMale)");
			Assert.AreEqual("F", GEDCOMUtils.GetSexStr(TGEDCOMSex.svFemale), "GetSexStr(svFemale)");
			Assert.AreEqual("U", GEDCOMUtils.GetSexStr(TGEDCOMSex.svUndetermined), "GetSexStr(svUndetermined)");
			Assert.AreEqual("", GEDCOMUtils.GetSexStr(TGEDCOMSex.svNone), "GetSexStr(svNone)");
			
			Assert.AreEqual(TGEDCOMSex.svMale, GEDCOMUtils.GetSexVal("M"), "GetSexVal(svMale)");
			Assert.AreEqual(TGEDCOMSex.svFemale, GEDCOMUtils.GetSexVal("F"), "GetSexVal(svFemale)");
			Assert.AreEqual(TGEDCOMSex.svUndetermined, GEDCOMUtils.GetSexVal("U"), "GetSexVal(svUndetermined)");
			Assert.AreEqual(TGEDCOMSex.svNone, GEDCOMUtils.GetSexVal(""), "GetSexVal(svNone)");
			
			Assert.AreEqual("ASCII", GEDCOMUtils.GetCharacterSetStr(TGEDCOMCharacterSet.csASCII));
			Assert.AreEqual("ANSEL", GEDCOMUtils.GetCharacterSetStr(TGEDCOMCharacterSet.csANSEL));
			Assert.AreEqual("UNICODE", GEDCOMUtils.GetCharacterSetStr(TGEDCOMCharacterSet.csUNICODE));
			Assert.AreEqual("UTF-8", GEDCOMUtils.GetCharacterSetStr(TGEDCOMCharacterSet.csUTF8));
			
			Assert.AreEqual(TGEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("ASCII"));
			Assert.AreEqual(TGEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("ANSI"));
			Assert.AreEqual(TGEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal("IBMPC"));
			Assert.AreEqual(TGEDCOMCharacterSet.csANSEL, GEDCOMUtils.GetCharacterSetVal("ANSEL"));
			Assert.AreEqual(TGEDCOMCharacterSet.csUNICODE, GEDCOMUtils.GetCharacterSetVal("UNICODE"));
			Assert.AreEqual(TGEDCOMCharacterSet.csUTF8, GEDCOMUtils.GetCharacterSetVal("UTF-8"));
			Assert.AreEqual(TGEDCOMCharacterSet.csUTF8, GEDCOMUtils.GetCharacterSetVal("UTF8"));
			Assert.AreEqual(TGEDCOMCharacterSet.csASCII, GEDCOMUtils.GetCharacterSetVal(""));
			
			//

			Assert.AreEqual(TResearchPriority.rpLow, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(TResearchPriority.rpLow)));
			Assert.AreEqual(TResearchPriority.rpNormal, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(TResearchPriority.rpNormal)));
			Assert.AreEqual(TResearchPriority.rpHigh, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(TResearchPriority.rpHigh)));
			Assert.AreEqual(TResearchPriority.rpTop, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(TResearchPriority.rpTop)));
			Assert.AreEqual(TResearchPriority.rpNone, GEDCOMUtils.GetPriorityVal(GEDCOMUtils.GetPriorityStr(TResearchPriority.rpNone)));
			
			//

			Assert.AreEqual(TGEDCOMOrdinanceProcessFlag.opYes, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(TGEDCOMOrdinanceProcessFlag.opYes)));
			Assert.AreEqual(TGEDCOMOrdinanceProcessFlag.opNo, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(TGEDCOMOrdinanceProcessFlag.opNo)));
			Assert.AreEqual(TGEDCOMOrdinanceProcessFlag.opNone, GEDCOMUtils.GetOrdinanceProcessFlagVal(GEDCOMUtils.GetOrdinanceProcessFlagStr(TGEDCOMOrdinanceProcessFlag.opNone)));
			
			//

			Assert.AreEqual(TResearchStatus.rsInProgress, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(TResearchStatus.rsInProgress)));
			Assert.AreEqual(TResearchStatus.rsOnHold, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(TResearchStatus.rsOnHold)));
			Assert.AreEqual(TResearchStatus.rsProblems, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(TResearchStatus.rsProblems)));
			Assert.AreEqual(TResearchStatus.rsCompleted, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(TResearchStatus.rsCompleted)));
			Assert.AreEqual(TResearchStatus.rsWithdrawn, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(TResearchStatus.rsWithdrawn)));
			Assert.AreEqual(TResearchStatus.rsDefined, GEDCOMUtils.GetStatusVal(GEDCOMUtils.GetStatusStr(TResearchStatus.rsDefined)));
			
			//

			Assert.AreEqual(TGEDCOMNameType.ntNone, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(TGEDCOMNameType.ntNone)));
			Assert.AreEqual(TGEDCOMNameType.ntAka, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(TGEDCOMNameType.ntAka)));
			Assert.AreEqual(TGEDCOMNameType.ntBirth, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(TGEDCOMNameType.ntBirth)));
			Assert.AreEqual(TGEDCOMNameType.ntImmigrant, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(TGEDCOMNameType.ntImmigrant)));
			Assert.AreEqual(TGEDCOMNameType.ntMaiden, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(TGEDCOMNameType.ntMaiden)));
			Assert.AreEqual(TGEDCOMNameType.ntMarried, GEDCOMUtils.GetNameTypeVal(GEDCOMUtils.GetNameTypeStr(TGEDCOMNameType.ntMarried)));

			//

			Assert.AreEqual(TCommunicationType.ctCall, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(TCommunicationType.ctCall)));
			Assert.AreEqual(TCommunicationType.ctEMail, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(TCommunicationType.ctEMail)));
			Assert.AreEqual(TCommunicationType.ctFax, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(TCommunicationType.ctFax)));
			Assert.AreEqual(TCommunicationType.ctLetter, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(TCommunicationType.ctLetter)));
			Assert.AreEqual(TCommunicationType.ctTape, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(TCommunicationType.ctTape)));
			Assert.AreEqual(TCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(GEDCOMUtils.GetCommunicationTypeStr(TCommunicationType.ctVisit)));
			Assert.AreEqual(TCommunicationType.ctVisit, GEDCOMUtils.GetCommunicationTypeVal(""));

			//

			Assert.AreEqual(TGEDCOMChildLinkageStatus.clChallenged, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(TGEDCOMChildLinkageStatus.clChallenged)));
			Assert.AreEqual(TGEDCOMChildLinkageStatus.clDisproven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(TGEDCOMChildLinkageStatus.clDisproven)));
			Assert.AreEqual(TGEDCOMChildLinkageStatus.clProven, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(TGEDCOMChildLinkageStatus.clProven)));
			Assert.AreEqual(TGEDCOMChildLinkageStatus.clNone, GEDCOMUtils.GetChildLinkageStatusVal(GEDCOMUtils.GetChildLinkageStatusStr(TGEDCOMChildLinkageStatus.clNone)));

			//

			Assert.AreEqual(TGEDCOMPedigreeLinkageType.plAdopted, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType.plAdopted)));
			Assert.AreEqual(TGEDCOMPedigreeLinkageType.plBirth, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType.plBirth)));
			Assert.AreEqual(TGEDCOMPedigreeLinkageType.plFoster, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType.plFoster)));
			Assert.AreEqual(TGEDCOMPedigreeLinkageType.plSealing, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType.plSealing)));
			Assert.AreEqual(TGEDCOMPedigreeLinkageType.plNone, GEDCOMUtils.GetPedigreeLinkageTypeVal(GEDCOMUtils.GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType.plNone)));

			//

			Assert.AreEqual(TGEDCOMRestriction.rnConfidential, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(TGEDCOMRestriction.rnConfidential)));
			Assert.AreEqual(TGEDCOMRestriction.rnLocked, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(TGEDCOMRestriction.rnLocked)));
			Assert.AreEqual(TGEDCOMRestriction.rnPrivacy, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(TGEDCOMRestriction.rnPrivacy)));
			Assert.AreEqual(TGEDCOMRestriction.rnNone, GEDCOMUtils.GetRestrictionVal(GEDCOMUtils.GetRestrictionStr(TGEDCOMRestriction.rnNone)));

			//

			Assert.AreEqual(TGEDCOMMediaType.mtNone, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtNone)));
			Assert.AreEqual(TGEDCOMMediaType.mtAudio, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtAudio)));
			Assert.AreEqual(TGEDCOMMediaType.mtBook, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtBook)));
			Assert.AreEqual(TGEDCOMMediaType.mtCard, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtCard)));
			Assert.AreEqual(TGEDCOMMediaType.mtElectronic, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtElectronic)));
			Assert.AreEqual(TGEDCOMMediaType.mtFiche, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtFiche)));
			Assert.AreEqual(TGEDCOMMediaType.mtFilm, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtFilm)));
			Assert.AreEqual(TGEDCOMMediaType.mtMagazine, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtMagazine)));
			Assert.AreEqual(TGEDCOMMediaType.mtManuscript, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtManuscript)));
			Assert.AreEqual(TGEDCOMMediaType.mtMap, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtMap)));
			Assert.AreEqual(TGEDCOMMediaType.mtNewspaper, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtNewspaper)));
			Assert.AreEqual(TGEDCOMMediaType.mtPhoto, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtPhoto)));
			Assert.AreEqual(TGEDCOMMediaType.mtTombstone, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtTombstone)));
			Assert.AreEqual(TGEDCOMMediaType.mtVideo, GEDCOMUtils.GetMediaTypeVal(GEDCOMUtils.GetMediaTypeStr(TGEDCOMMediaType.mtVideo)));
			Assert.AreEqual(TGEDCOMMediaType.mtUnknown, GEDCOMUtils.GetMediaTypeVal("sample"));

			//

			Assert.AreEqual(TGEDCOMMultimediaFormat.mfNone, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfNone)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfBMP, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfBMP)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfGIF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfGIF)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfJPG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfJPG)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfOLE, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfOLE)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfPCX, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfPCX)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTIF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfTIF)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfWAV, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfWAV)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTXT, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfTXT)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfRTF, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfRTF)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfAVI, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfAVI)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTGA, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfTGA)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfPNG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfPNG)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfMPG, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfMPG)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfHTM, GEDCOMUtils.GetMultimediaFormatVal(GEDCOMUtils.GetMultimediaFormatStr(TGEDCOMMultimediaFormat.mfHTM)));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfUnknown, GEDCOMUtils.GetMultimediaFormatVal("xxx"));

			//

			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsNone, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsNone)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsCanceled, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsCanceled)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsCompleted, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsCompleted)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsExcluded, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsExcluded)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsDNS, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsDNS)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsDNSCAN, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsDNSCAN)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsPre1970, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsPre1970)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsSubmitted, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsSubmitted)));
			Assert.AreEqual(TGEDCOMSpouseSealingDateStatus.sdsUncleared, GEDCOMUtils.GetSpouseSealingDateStatusVal(GEDCOMUtils.GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus.sdsUncleared)));

			//

			Assert.AreEqual(Encoding.GetEncoding(1251), GEDCOMUtils.GetEncodingByCharacterSet(TGEDCOMCharacterSet.csASCII));
			Assert.AreEqual(Encoding.Unicode, GEDCOMUtils.GetEncodingByCharacterSet(TGEDCOMCharacterSet.csUNICODE));
			Assert.AreEqual(Encoding.UTF8, GEDCOMUtils.GetEncodingByCharacterSet(TGEDCOMCharacterSet.csUTF8));
			Assert.AreEqual(Encoding.Default, GEDCOMUtils.GetEncodingByCharacterSet(TGEDCOMCharacterSet.csANSEL));
		}

		[Test]
		public void GEDCOMFactory_Tests()
		{
			GEDCOMFactory f = GEDCOMFactory.GetInstance();
			Assert.IsNotNull(f, "f != null");

			f.RegisterTag("DATE", TGEDCOMDateValue.Create);

			TGEDCOMTag tag = f.CreateTag(null, null, "DATE", "");
			Assert.IsNotNull(tag, "tag != null");

			tag = f.CreateTag(null, null, "TEST", "");
			Assert.IsNull(tag, "tag == null");
		}

		[Test]
		public void GEDCOMMathes_Tests()
		{
			TGEDCOMTree tree = new TGEDCOMTree();
			Assert.IsNotNull(tree);

			TGEDCOMIndividualRecord ind1, ind2;
			TGEDCOMCustomEvent ev1, ev2;
			TGEDCOMDateValue dtVal1, dtVal2;

			ind1 = tree.aux_CreateIndividual("Ivan", "Fedoroff", "Ivanov", TGEDCOMSex.svMale);
			ind2 = tree.aux_CreateIndividual("Ivan", "Fedoroff", "Ivanovich", TGEDCOMSex.svMale);

			ev1 = new TGEDCOMIndividualEvent(tree, ind1, "BIRT", "");
			dtVal1 = ev1.Detail.Date;
			ind1.AddIndividualEvent(ev1);

			ev2 = new TGEDCOMIndividualEvent(tree, ind2, "BIRT", "");
			dtVal2 = ev2.Detail.Date;
			ind2.AddIndividualEvent(ev2);

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
		public void GEDCOMChangeDate_Tests()
		{
			TGEDCOMChangeDate cd = new TGEDCOMChangeDate(null, null, "CHAN", "");
			Assert.IsNotNull(cd);

			DateTime dtNow = DateTime.Now;
			dtNow = dtNow.AddTicks(-dtNow.Ticks % 10000000);
			cd.ChangeDateTime = dtNow;
			
			DateTime dtx = cd.ChangeDateTime;
			Assert.AreEqual(dtNow, dtx);
			
			TGEDCOMTime time = cd.ChangeTime;
			Assert.AreEqual(dtNow.Second, time.Seconds);
			Assert.AreEqual(dtNow.Minute, time.Minutes);
			Assert.AreEqual(dtNow.Hour, time.Hour);

			time.Seconds = 11;
			Assert.AreEqual(11, time.Seconds);
			time.Minutes = 22;
			Assert.AreEqual(22, time.Minutes);
			time.Hour = 12;
			Assert.AreEqual(12, time.Hour);
			
			Assert.AreEqual("12:22:11", time.StringValue);

			Assert.IsFalse(time.IsEmpty());
			time.Clear();
			Assert.IsTrue(time.IsEmpty());
		}

		[Test]
		public void GEDCOMDateExact_Tests()
		{
			TGEDCOMDateExact dtx1 = new TGEDCOMDateExact(null, null, "DATE", "20 JAN 2013");
			Assert.IsNotNull(dtx1, "dtx1 != null");

			DateTime dt = DateTime.Parse("20.01.2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			
			//dtx1.DateCalendar = TGEDCOMCalendar.dcFrench;
			//Assert.AreEqual(TGEDCOMCalendar.dcFrench, dtx1.DateCalendar);
			
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
		}

		[Test]
		public void GEDCOMDateValue_Tests()
		{
			TGEDCOMDateValue dtx1 = new TGEDCOMDateValue(null, null, "DATE", "20 JAN 2013");
			Assert.IsNotNull(dtx1, "dtx1 != null");

			DateTime dt = DateTime.Parse("20.01.2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			
			dtx1.ParseString("INT 20 JAN 2013 (today)");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			Assert.AreEqual((dtx1.Value as TGEDCOMDateInterpreted).DatePhrase, "today");
			(dtx1.Value as TGEDCOMDateInterpreted).DatePhrase = "now";
			Assert.AreEqual(dtx1.StringValue, "INT 20 JAN 2013 (now)");
			(dtx1.Value as TGEDCOMDateInterpreted).DatePhrase = "(yesterday)";
			Assert.AreEqual(dtx1.StringValue, "INT 20 JAN 2013 (yesterday)");

			//FIXME: не проходит
			//dtx1.ParseString("INT 20 JAN 2013 (today (yesterday))");
			//Assert.AreEqual(dtx1.StringValue, "INT 20 JAN 2013 (yesterday)");
			
			string st;
			
			st = "ABT 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.IsTrue(dtx1.Date.Equals(dt));
			Assert.AreEqual(st, dtx1.StringValue);
			Assert.AreEqual(((TGEDCOMDateApproximated)dtx1.Value).Approximated, TGEDCOMApproximated.daAbout);
			
			st = "CAL 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.AreEqual(dtx1.Date, dt);
			Assert.AreEqual(st, dtx1.StringValue);
			Assert.AreEqual(((TGEDCOMDateApproximated)dtx1.Value).Approximated, TGEDCOMApproximated.daCalculated);
			
			st = "EST 20 DEC 2013";
			dtx1.ParseString(st);
			Assert.AreEqual(dtx1.Date, DateTime.Parse("20.12.2013"));
			Assert.AreEqual(st, dtx1.StringValue);
			Assert.AreEqual(((TGEDCOMDateApproximated)dtx1.Value).Approximated, TGEDCOMApproximated.daEstimated);

			((TGEDCOMDateApproximated)dtx1.Value).Approximated = TGEDCOMApproximated.daCalculated;
			Assert.AreEqual(dtx1.StringValue, "CAL 20 DEC 2013");

			((TGEDCOMDateApproximated)dtx1.Value).Approximated = TGEDCOMApproximated.daExact;
			Assert.AreEqual(dtx1.StringValue, "20 DEC 2013");
			
			//
			
			dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty());
			Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
			Assert.AreEqual("04 JAN 2013", (dtx1.Value as TGEDCOMDatePeriod).DateFrom.StringValue);
			Assert.AreEqual("23 JAN 2013", (dtx1.Value as TGEDCOMDatePeriod).DateTo.StringValue);

			dtx1.ParseString("BEF 20 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty());
			Assert.AreEqual(DateTime.Parse("20.01.2013"), dtx1.Date);
			Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);

			dtx1.ParseString("AFT 20 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty());
			Assert.AreEqual(DateTime.Parse("20.01.2013"), dtx1.Date);
			Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);

			dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty());
			Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
			Assert.AreEqual("04 JAN 2013", (dtx1.Value as TGEDCOMDateRange).After.StringValue);
			Assert.AreEqual("25 JAN 2013", (dtx1.Value as TGEDCOMDateRange).Before.StringValue);
			
			dtx1.Clear();
			Assert.IsTrue(dtx1.IsEmpty());
		}

		[Test]
		public void GEDCOMAddress_Tests()
		{
			TGEDCOMAddress addr = TGEDCOMAddress.Create(null, null, "ADDR", "") as TGEDCOMAddress;
			Assert.IsNotNull(addr, "addr != null");
			
			GEDCOMAddressTest(addr, true);
		}

		[Test]
		public void GEDCOMAlias_Tests()
		{
			TGEDCOMTag alias = TGEDCOMAlias.Create(null, null, "ALIA", "");
			Assert.IsNotNull(alias, "alias != null");
		}

		[Test]
		public void GEDCOMAssociation_Tests()
		{
			TGEDCOMAssociation association = TGEDCOMAssociation.Create(null, null, "ASSO", "") as TGEDCOMAssociation;
			Assert.IsNotNull(association);

			association.Relation = "This is test relation";
			Assert.AreEqual("This is test relation", association.Relation);

			Assert.IsFalse(association.IsEmpty());
			association.Clear();
			Assert.IsTrue(association.IsEmpty());
		}

		[Test]
		public void GEDCOMTree_Tests()
		{
			TGEDCOMTree tree = new TGEDCOMTree();
			Assert.IsNotNull(tree);
			
			GEDCOMHeaderRecordTest(tree.Header);
			
			TGEDCOMRecord rec;
			
			TGEDCOMIndividualRecord iRec = tree.AddRecord(TGEDCOMIndividualRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMIndividualRecord;
			Assert.IsNotNull(iRec);
			iRec.InitNew();
			GEDCOMIndividualRecordTest(iRec);

			string xref = iRec.XRef;
			rec = tree.XRefIndex_Find(xref);
			Assert.IsNotNull(rec);
			Assert.AreEqual(xref, rec.XRef);
			
			//
			
			TGEDCOMFamilyRecord famRec = tree.AddRecord(TGEDCOMFamilyRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMFamilyRecord;
			Assert.IsNotNull(famRec, "rec1 != null");
			famRec.InitNew();
			GEDCOMFamilyRecordTest(famRec, iRec);
			
			//
			
			rec = tree.AddRecord(TGEDCOMNoteRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			GEDCOMNoteRecordTest(rec as TGEDCOMNoteRecord, iRec);
			
			//
			
			TGEDCOMRepositoryRecord repRec = tree.AddRecord(TGEDCOMRepositoryRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMRepositoryRecord;
			Assert.IsNotNull(repRec, "rec1 != null");
			repRec.InitNew();
			GEDCOMRepositoryRecordTest(repRec);
			
			//
			
			TGEDCOMSourceRecord srcRec = tree.AddRecord(TGEDCOMSourceRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMSourceRecord;
			Assert.IsNotNull(srcRec, "rec1 != null");
			srcRec.InitNew();
			GEDCOMSourceRecordTest(srcRec, iRec, repRec);
			
			//
			
			rec = tree.AddRecord(TGEDCOMMultimediaRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			GEDCOMMultimediaRecordTest(rec as TGEDCOMMultimediaRecord, iRec);
			
			//
			
			rec = tree.AddRecord(TGEDCOMSubmitterRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			GEDCOMSubmitterRecordTest(rec as TGEDCOMSubmitterRecord);
			string submXRef = rec.XRef;
			
			//
			
			rec = tree.AddRecord(TGEDCOMSubmissionRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			GEDCOMSubmissionRecordTest(rec as TGEDCOMSubmissionRecord, submXRef);
			
			//
			
			TGEDCOMGroupRecord groupRec = tree.AddRecord(TGEDCOMGroupRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMGroupRecord;
			Assert.IsNotNull(groupRec, "rec1 != null");
			groupRec.InitNew();
			GEDCOMGroupRecordTest(groupRec, iRec);
						
			//
			
			TGEDCOMTaskRecord taskRec = tree.AddRecord(TGEDCOMTaskRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMTaskRecord;
			Assert.IsNotNull(taskRec, "rec1 != null");
			taskRec.InitNew();
			GEDCOMTaskRecordTest(taskRec, iRec, famRec, srcRec);
			
			//
			
			TGEDCOMCommunicationRecord commRec = tree.AddRecord(TGEDCOMCommunicationRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMCommunicationRecord;
			Assert.IsNotNull(commRec, "rec1 != null");
			commRec.InitNew();
			GEDCOMCommunicationRecordTest(commRec, iRec);
			
			//
			
			TGEDCOMResearchRecord resRec = tree.AddRecord(TGEDCOMResearchRecord.Create(tree, tree, "", "") as TGEDCOMRecord) as TGEDCOMResearchRecord;
			Assert.IsNotNull(resRec, "rec1 != null");
			resRec.InitNew();
			GEDCOMResearchRecordTest(resRec, commRec, taskRec, groupRec);
			
			//

			rec = tree.AddRecord(TGEDCOMLocationRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			GEDCOMLocationRecordTest(rec as TGEDCOMLocationRecord);
			
			//

			rec = tree.aux_CreateFamily();
			Assert.IsNotNull(rec, "rec1 != null");
			
			rec = tree.aux_CreateNote();
			Assert.IsNotNull(rec, "rec1 != null");
			
			rec = tree.aux_CreateSource();
			Assert.IsNotNull(rec, "rec1 != null");
			
			rec = tree.aux_CreateGroup();
			Assert.IsNotNull(rec, "rec1 != null");


			tree.Pack();
		}

		#endregion

		#region Public Partial Tests

		public static void GEDCOMAddressTest(TGEDCOMAddress addr, bool checkStream)
		{
			addr.aux_SetAddressValue("test");
			Assert.AreEqual("test", addr.Address.Text.Trim());

			addr.Address = new StringList("This\r\naddress\r\ntest");
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
			
			if (checkStream) {
				string buf = TagStreamTest(addr);
				Assert.AreEqual(buf, "0 ADDR This\r\n"+"1 CONT address\r\n"+"1 CONT test\r\n"
				                +"0 PHON 8 911 101 99 99\r\n"
				                +"0 EMAIL test@mail.com\r\n"
				                +"0 FAX abrakadabra\r\n"
				                +"0 WWW http://test.com\r\n");
			}
			
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

			addr.SetAddressArray(new string[] {"test11", "test21", "test31"});
			Assert.AreEqual("test11", addr.Address[0]);
			Assert.AreEqual("test21", addr.Address[1]);
			Assert.AreEqual("test31", addr.Address[2]);

			Assert.IsFalse(addr.IsEmpty());
			addr.Clear();
			Assert.IsTrue(addr.IsEmpty());
		}

		public static void GEDCOMMapTest(TGEDCOMMap map)
		{
			map.Lati = 5.11111;
			map.Long = 7.99999;
			
			Assert.AreEqual(5.11111, map.Lati);
			Assert.AreEqual(7.99999, map.Long);
		}

		#endregion

		#region Private Partial Tests

		private void GEDCOMHeaderRecordTest(TGEDCOMHeader headRec)
		{
			headRec.Notes = new StringList("This notes test");
			Assert.AreEqual("This notes test", headRec.Notes[0]);

			headRec.CharacterSet = TGEDCOMCharacterSet.csASCII;
			Assert.AreEqual(TGEDCOMCharacterSet.csASCII, headRec.CharacterSet);

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

			GEDCOMAddressTest(headRec.SourceBusinessAddress, false);

			headRec.SourceBusinessName = "test23";
			Assert.AreEqual("test23", headRec.SourceBusinessName);

			headRec.SourceProductName = "test33";
			Assert.AreEqual("test33", headRec.SourceProductName);

			headRec.SourceVersion = "test44";
			Assert.AreEqual("test44", headRec.SourceVersion);

			
			Assert.IsFalse(headRec.IsEmpty());
			headRec.Clear();
			Assert.IsTrue(headRec.IsEmpty());
		}

		private void GEDCOMRecordTest(TGEDCOMRecord rec)
		{
			rec.AutomatedRecordID = "test11";
			Assert.AreEqual("test11", rec.AutomatedRecordID);
			
			Assert.AreEqual(TGEDCOMRecordType.rtIndividual, rec.RecordType);
			
			Assert.AreEqual(1, rec.aux_GetId());
			Assert.AreEqual("1", rec.aux_GetXRefNum());
		}

		private void GEDCOMPersonalNameTest(TGEDCOMIndividualRecord indiRec)
		{
			TGEDCOMPersonalName persName;

			persName = TGEDCOMPersonalName.Create(indiRec.Owner, indiRec, "", "") as TGEDCOMPersonalName;
			indiRec.AddPersonalName(persName);
 
			persName = indiRec.PersonalNames[0];
			persName.NameType = TGEDCOMNameType.ntBirth;
			Assert.AreEqual(TGEDCOMNameType.ntBirth, persName.NameType);
			
			///
			
			persName.SetNameParts("Ivan", "Ivanov", "Fedoroff");
			
			//persName.Surname = "Ivanov";
			Assert.AreEqual("Ivan", persName.FirstPart);
			Assert.AreEqual("Ivanov", persName.Surname);
			Assert.AreEqual("Fedoroff", persName.LastPart);
			
			Assert.AreEqual("Ivan Ivanov Fedoroff", persName.FullName);
			
			///
			
			TGEDCOMPersonalNamePieces pnPieces = persName.Pieces;
			
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
			
			persName.Clear();
			Assert.IsTrue(persName.IsEmpty());
		}

		private void GEDCOMIndividualRecordTest(TGEDCOMIndividualRecord indiRec)
		{
			this.GEDCOMRecordTest(indiRec);

			this.GEDCOMPersonalNameTest(indiRec);

			indiRec.Restriction = TGEDCOMRestriction.rnLocked;
			Assert.AreEqual(TGEDCOMRestriction.rnLocked, indiRec.Restriction);

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
			
			
			Assert.IsFalse(indiRec.IsEmpty());
			indiRec.Clear();
			Assert.IsTrue(indiRec.IsEmpty());
		}

		private void GEDCOMFamilyRecordTest(TGEDCOMFamilyRecord famRec, TGEDCOMIndividualRecord indiv)
		{
			famRec.Restriction = TGEDCOMRestriction.rnLocked;
			Assert.AreEqual(TGEDCOMRestriction.rnLocked, famRec.Restriction);

			famRec.aux_AddChild(indiv);
			Assert.AreEqual(0, famRec.IndexOfChild(indiv));

			this.GEDCOMChildToFamilyLinkTest(indiv.ChildToFamilyLinks[0]);

			famRec.aux_RemoveChild(indiv);
			Assert.AreEqual(-1, famRec.IndexOfChild(indiv));

			//
			
			famRec.Husband.Value = indiv;
			Assert.AreEqual(indiv, famRec.Husband.Value);
			famRec.Husband.Value = null;

			famRec.Wife.Value = indiv;
			Assert.AreEqual(indiv, famRec.Wife.Value);
			famRec.Wife.Value = null;

			//
			indiv.Sex = TGEDCOMSex.svMale;
			famRec.aux_AddSpouse(indiv);
			this.GEDCOMSpouseToFamilyLinkTest(indiv.SpouseToFamilyLinks[0]);
			//

			Assert.IsFalse(famRec.IsEmpty());
			famRec.Clear();
			Assert.IsTrue(famRec.IsEmpty());
		}

		private void GEDCOMChildToFamilyLinkTest(TGEDCOMChildToFamilyLink childLink)
		{
			childLink.ChildLinkageStatus = TGEDCOMChildLinkageStatus.clChallenged;
			Assert.AreEqual(TGEDCOMChildLinkageStatus.clChallenged, childLink.ChildLinkageStatus);

			childLink.PedigreeLinkageType = TGEDCOMPedigreeLinkageType.plFoster;
			Assert.AreEqual(TGEDCOMPedigreeLinkageType.plFoster, childLink.PedigreeLinkageType);
			
			Assert.IsNotNull(childLink.Family);
		}

		private void GEDCOMSpouseToFamilyLinkTest(TGEDCOMSpouseToFamilyLink spouseLink)
		{
			Assert.IsNotNull(spouseLink.Family);
		}

		private void GEDCOMSourceRecordTest(TGEDCOMSourceRecord sourRec, TGEDCOMIndividualRecord indiv, TGEDCOMRepositoryRecord repRec)
		{
			sourRec.FiledByEntry = "This is test source";
			Assert.AreEqual("This is test source", sourRec.FiledByEntry);
			
			sourRec.Originator = new StringList("author");
			Assert.AreEqual("author", sourRec.Originator.Text.Trim());
			
			sourRec.Title = new StringList("title");
			Assert.AreEqual("title", sourRec.Title.Text.Trim());
			
			sourRec.Publication = new StringList("publication");
			Assert.AreEqual("publication", sourRec.Publication.Text.Trim());
			
			sourRec.Text = new StringList("sample");
			Assert.AreEqual("sample", sourRec.Text.Text.Trim());
			
			this.GEDCOMSourceCitationTest(sourRec, indiv);
			this.GEDCOMRepositoryCitationTest(sourRec, repRec);
			
			Assert.IsFalse(sourRec.IsEmpty());
			sourRec.Clear();
			Assert.IsTrue(sourRec.IsEmpty());
		}

		private void GEDCOMSourceCitationTest(TGEDCOMSourceRecord sourRec, TGEDCOMIndividualRecord indiv)
		{
			TGEDCOMSourceCitation srcCit = indiv.aux_AddSource(sourRec, "p2", 3);
			
			Assert.AreEqual("p2", srcCit.Page);
			Assert.AreEqual(3, srcCit.CertaintyAssessment);
			
			Assert.IsTrue(srcCit.IsPointer, "srcCit.IsPointer");

			Assert.IsFalse(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer
		}

		private void GEDCOMRepositoryCitationTest(TGEDCOMSourceRecord sourRec, TGEDCOMRepositoryRecord repRec)
		{
			TGEDCOMRepositoryCitation repCit = sourRec.aux_AddRepository(repRec);
			
			Assert.IsFalse(repCit.IsEmpty(), "repCit.IsEmpty()"); // its pointer
		}

		private void GEDCOMResearchRecordTest(TGEDCOMResearchRecord resRec, TGEDCOMCommunicationRecord commRec, TGEDCOMTaskRecord taskRec, TGEDCOMGroupRecord groupRec)
		{
			resRec.ResearchName = "Test Research";
			Assert.AreEqual("Test Research", resRec.ResearchName);
			
			resRec.Priority = TResearchPriority.rpNormal;
			Assert.AreEqual(TResearchPriority.rpNormal, resRec.Priority);
			
			resRec.Status = TResearchStatus.rsOnHold;
			Assert.AreEqual(TResearchStatus.rsOnHold, resRec.Status);
			
			resRec.StartDate.Date = DateTime.Parse("20.01.2013");
			Assert.AreEqual(DateTime.Parse("20.01.2013"), resRec.StartDate.Date);
			
			resRec.StopDate.Date = DateTime.Parse("21.01.2013");
			Assert.AreEqual(DateTime.Parse("21.01.2013"), resRec.StopDate.Date);
			
			resRec.Percent = 33;
			Assert.AreEqual(33, resRec.Percent);
			
			resRec.aux_AddCommunication(commRec);
			resRec.aux_RemoveCommunication(commRec);
			
			resRec.aux_AddTask(taskRec);
			resRec.aux_RemoveTask(taskRec);
			
			resRec.aux_AddGroup(groupRec);
			resRec.aux_RemoveGroup(groupRec);
			
			Assert.IsFalse(resRec.IsEmpty());
			resRec.Clear();
			Assert.IsTrue(resRec.IsEmpty());
		}

		private void GEDCOMRepositoryRecordTest(TGEDCOMRepositoryRecord repoRec)
		{
			repoRec.RepositoryName = "Test Repository";
			Assert.AreEqual("Test Repository", repoRec.RepositoryName);

			GEDCOMAddressTest(repoRec.Address, false);


			Assert.IsFalse(repoRec.IsEmpty());
			repoRec.Clear();
			Assert.IsTrue(repoRec.IsEmpty());
		}

		private void GEDCOMMultimediaRecordTest(TGEDCOMMultimediaRecord mediaRec, TGEDCOMIndividualRecord indiv)
		{
			mediaRec.AddTag("FILE", "", null);
			TGEDCOMFileReferenceWithTitle file_ref = mediaRec.FileReferences[0];
			Assert.IsNotNull(file_ref);
			
			file_ref.Title = "File Title 2";
			Assert.AreEqual("File Title 2", file_ref.Title);
			
			file_ref.LinkFile("sample.png");
			file_ref.MediaType = TGEDCOMMediaType.mtManuscript;
			Assert.AreEqual("sample.png", file_ref.StringValue);
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfPNG, file_ref.MultimediaFormat);
			Assert.AreEqual(TGEDCOMMediaType.mtManuscript, file_ref.MediaType);
			
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfUnknown, TGEDCOMFileReference.RecognizeFormat(""));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfUnknown, TGEDCOMFileReference.RecognizeFormat("sample.xxx"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfBMP, TGEDCOMFileReference.RecognizeFormat("sample.BMP"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfGIF, TGEDCOMFileReference.RecognizeFormat("sample.Gif"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfJPG, TGEDCOMFileReference.RecognizeFormat("sample.jpg"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfJPG, TGEDCOMFileReference.RecognizeFormat("sample.Jpeg"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfOLE, TGEDCOMFileReference.RecognizeFormat("sample.ole"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfPCX, TGEDCOMFileReference.RecognizeFormat("sample.pCx"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTIF, TGEDCOMFileReference.RecognizeFormat("sample.TiF"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTIF, TGEDCOMFileReference.RecognizeFormat("sample.tiff"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfWAV, TGEDCOMFileReference.RecognizeFormat("sample.wav"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTXT, TGEDCOMFileReference.RecognizeFormat("sample.txt"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfRTF, TGEDCOMFileReference.RecognizeFormat("sample.rtf"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfAVI, TGEDCOMFileReference.RecognizeFormat("sample.AvI"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfTGA, TGEDCOMFileReference.RecognizeFormat("sample.TGA"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfPNG, TGEDCOMFileReference.RecognizeFormat("sample.png"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfMPG, TGEDCOMFileReference.RecognizeFormat("sample.mpg"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfMPG, TGEDCOMFileReference.RecognizeFormat("sample.mpeg"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfHTM, TGEDCOMFileReference.RecognizeFormat("sample.htm"));
			Assert.AreEqual(TGEDCOMMultimediaFormat.mfHTM, TGEDCOMFileReference.RecognizeFormat("sample.html"));
			
			this.GEDCOMMultimediaLinkTest(mediaRec, indiv);
			
			Assert.IsFalse(mediaRec.IsEmpty());
			mediaRec.Clear();
			Assert.IsTrue(mediaRec.IsEmpty());
		}

		private void GEDCOMMultimediaLinkTest(TGEDCOMMultimediaRecord mediaRec, TGEDCOMIndividualRecord indiv)
		{
			TGEDCOMMultimediaLink mmLink = indiv.aux_AddMultimedia(mediaRec);
			
			mmLink.Title = "Title1";
			Assert.AreEqual("Title1", mmLink.Title);
			
			Assert.IsTrue(mmLink.IsPointer, "mmLink.IsPointer");
			
			mmLink.IsPrimary = true;
			Assert.IsTrue(mmLink.IsPrimary, "mmLink.IsPrimary");

			Assert.IsFalse(mmLink.IsEmpty(), "mmLink.IsEmpty()"); // its pointer
		}

		private void GEDCOMSubmissionRecordTest(TGEDCOMSubmissionRecord submRec, string submitterXRef)
		{
			submRec.FamilyFileName = "FamilyFileName";
			Assert.AreEqual("FamilyFileName", submRec.FamilyFileName);

			submRec.TempleCode = "TempleCode";
			Assert.AreEqual("TempleCode", submRec.TempleCode);

			submRec.GenerationsOfAncestors = 11;
			Assert.AreEqual(11, submRec.GenerationsOfAncestors);

			submRec.GenerationsOfDescendants = 77;
			Assert.AreEqual(77, submRec.GenerationsOfDescendants);

			submRec.OrdinanceProcessFlag = TGEDCOMOrdinanceProcessFlag.opYes;
			Assert.AreEqual(TGEDCOMOrdinanceProcessFlag.opYes, submRec.OrdinanceProcessFlag);
			
			submRec.AddTag("SUBM", GEDCOMUtils.EncloseXRef(submitterXRef), null);
			TGEDCOMSubmitterRecord subr = submRec.Submitter.Value as TGEDCOMSubmitterRecord;
			Assert.IsNotNull(subr);
			
			
			Assert.IsFalse(submRec.IsEmpty());
			submRec.Clear();
			Assert.IsTrue(submRec.IsEmpty());
		}

		private void GEDCOMSubmitterRecordTest(TGEDCOMSubmitterRecord subrRec)
		{
			subrRec.Name.StringValue = "Test Submitter";
			Assert.AreEqual("Test Submitter", subrRec.Name.StringValue);
			
			subrRec.RegisteredReference = "regref";
			Assert.AreEqual("regref", subrRec.RegisteredReference);
			
			subrRec.AddTag("LANG", "RUS", null);
			Assert.AreEqual("RUS", subrRec.Languages[0].StringValue);
			
			subrRec.SetLanguage(1, "ENG");
			Assert.AreEqual("ENG", subrRec.Languages[1].StringValue);
			
			GEDCOMAddressTest(subrRec.Address, false);
			
			
			Assert.IsFalse(subrRec.IsEmpty());
			subrRec.Clear();
			Assert.IsTrue(subrRec.IsEmpty());
		}

		private void GEDCOMGroupRecordTest(TGEDCOMGroupRecord groupRec, TGEDCOMIndividualRecord member)
		{
			groupRec.GroupName = "Test Group";
			Assert.AreEqual("Test Group", groupRec.GroupName);
			
			groupRec.DeleteTag("_UID");
			string buf = TagStreamTest(groupRec);
			Assert.AreEqual("0 @G1@ _GROUP\r\n1 NAME Test Group\r\n", buf);
			
			bool res = groupRec.aux_AddMember(null);
			Assert.IsFalse(res);
			
			res = groupRec.aux_RemoveMember(null);
			Assert.IsFalse(res);
			
			Assert.AreEqual(-1, groupRec.IndexOfMember(null));
			
			groupRec.aux_AddMember(member);
			Assert.AreEqual(0, groupRec.IndexOfMember(member));
			
			groupRec.aux_RemoveMember(member);
			Assert.AreEqual(-1, groupRec.IndexOfMember(member));
			
			//Assert.AreEqual(-1, groupRec.IndexOfMember(null));
			
			Assert.IsFalse(groupRec.IsEmpty());
			groupRec.Clear();
			Assert.IsTrue(groupRec.IsEmpty());
		}

		private void GEDCOMCommunicationRecordTest(TGEDCOMCommunicationRecord comRec, TGEDCOMIndividualRecord iRec)
		{
			comRec.CommName = "Test Communication";
			Assert.AreEqual("Test Communication", comRec.CommName);

			comRec.CommunicationType = TCommunicationType.ctFax;
			Assert.AreEqual(TCommunicationType.ctFax, comRec.CommunicationType);
			
			comRec.Date.Date = DateTime.Parse("23.01.2013");
			Assert.AreEqual(DateTime.Parse("23.01.2013"), comRec.Date.Date);
			
			TCommunicationDir dir = TCommunicationDir.cdFrom;
			TGEDCOMIndividualRecord corr = null;

			comRec.SetCorresponder(TCommunicationDir.cdFrom, iRec);
			comRec.GetCorresponder(ref dir, ref corr);
			Assert.AreEqual(TCommunicationDir.cdFrom, dir);
			Assert.AreEqual(iRec, corr);
			
			comRec.SetCorresponder(TCommunicationDir.cdTo, iRec);
			comRec.GetCorresponder(ref dir, ref corr);
			Assert.AreEqual(TCommunicationDir.cdTo, dir);
			Assert.AreEqual(iRec, corr);
			
			
			Assert.IsFalse(comRec.IsEmpty());
			comRec.Clear();
			Assert.IsTrue(comRec.IsEmpty());
		}

		private void GEDCOMLocationRecordTest(TGEDCOMLocationRecord locRec)
		{
			locRec.LocationName = "Test Location";
			Assert.AreEqual("Test Location", locRec.LocationName);
			
			GEDCOMMapTest(locRec.Map);			
			
			Assert.IsFalse(locRec.IsEmpty());
			locRec.Clear();
			Assert.IsTrue(locRec.IsEmpty());
		}

		private void GEDCOMTaskRecordTest(TGEDCOMTaskRecord taskRec, TGEDCOMIndividualRecord indiv, TGEDCOMFamilyRecord famRec, TGEDCOMSourceRecord srcRec)
		{
			TGoalType gType;
			TGEDCOMRecord gRec;

			taskRec.Priority = TResearchPriority.rpNormal;
			Assert.AreEqual(TResearchPriority.rpNormal, taskRec.Priority);
			
			taskRec.StartDate.Date = DateTime.Parse("20.01.2013");
			Assert.AreEqual(DateTime.Parse("20.01.2013"), taskRec.StartDate.Date);
			
			taskRec.StopDate.Date = DateTime.Parse("21.01.2013");
			Assert.AreEqual(DateTime.Parse("21.01.2013"), taskRec.StopDate.Date);

			taskRec.Goal = "Test Goal";
			Assert.AreEqual("Test Goal", taskRec.Goal);
			taskRec.aux_GetTaskGoal(out gType, out gRec);
			Assert.AreEqual(TGoalType.gtOther, gType);
			Assert.AreEqual(null, gRec);
			
			taskRec.Goal = indiv.XRef;
			taskRec.aux_GetTaskGoal(out gType, out gRec);
			Assert.AreEqual(TGoalType.gtIndividual, gType);
			Assert.AreEqual(indiv, gRec);
			
			taskRec.Goal = famRec.XRef;
			taskRec.aux_GetTaskGoal(out gType, out gRec);
			Assert.AreEqual(TGoalType.gtFamily, gType);
			Assert.AreEqual(famRec, gRec);
			
			taskRec.Goal = srcRec.XRef;
			taskRec.aux_GetTaskGoal(out gType, out gRec);
			Assert.AreEqual(TGoalType.gtSource, gType);
			Assert.AreEqual(srcRec, gRec);
			
			Assert.IsFalse(taskRec.IsEmpty());
			taskRec.Clear();
			Assert.IsTrue(taskRec.IsEmpty());
		}

		private void GEDCOMNoteRecordTest(TGEDCOMNoteRecord noteRec, TGEDCOMIndividualRecord indiv)
		{
			noteRec.SetNotesArray(new string[] { "This", "notes", "test" });
			
			string ctx = GKCore.GKUtils.ConStrings(noteRec.Note);
			Assert.AreEqual("This notes test", ctx);

			noteRec.Note = new StringList("This\r\nnotes2\r\ntest2");
			Assert.AreEqual("This", noteRec.Note[0]);
			Assert.AreEqual("notes2", noteRec.Note[1]);
			Assert.AreEqual("test2", noteRec.Note[2]);
			
			ctx = GKCore.GKUtils.ConStrings(noteRec.Note);
			Assert.AreEqual("This notes2 test2", ctx);
			
			noteRec.Clear();
			noteRec.aux_AddNoteText("Test text");
			Assert.AreEqual("Test text", noteRec.Note.Text.Trim());
			
			this.GEDCOMNotesTest(noteRec, indiv);
			
			Assert.IsFalse(noteRec.IsEmpty());
			noteRec.Clear();
			Assert.IsTrue(noteRec.IsEmpty());
		}

		private void GEDCOMNotesTest(TGEDCOMNoteRecord noteRec, TGEDCOMIndividualRecord indiv)
		{
			TGEDCOMNotes notes = indiv.aux_AddNote(noteRec);
			
			Assert.IsTrue(notes.IsPointer, "notes.IsPointer");
			
			Assert.IsFalse(notes.IsEmpty()); // its pointer
		}
		
		#endregion
		
		#region Private Aux functions
		
		private static string TagStreamTest(TGEDCOMTag tag)
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
