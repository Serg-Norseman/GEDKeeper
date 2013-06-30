using System;
using System.Text;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
    [Serializable]
	public class EGEDCOMException : Exception
	{
		public EGEDCOMException()
		{
		}

		public EGEDCOMException(string message) : base(message)
		{
		}
	}

	public class TGEDCOMObject
	{
		public const char GEDCOMDelimiter = ' ';
		public const char GEDCOMYearModifierSeparator = '/';
		public const string GEDCOMYearBC = "B.C."; // const restored
		public const char GEDCOMPointerDelimiter = '@';
		//const string GEDCOMNewLine = "#13#10";

		// deprecated
		//public const byte GEDCOMMaxPhoneNumbers = 3;
		//public const byte GEDCOMMaxEmailAddresses = 3;
		//public const byte GEDCOMMaxFaxNumbers = 3;
		//public const byte GEDCOMMaxWebPages = 3;
		//public const byte GEDCOMMaxLanguages = 3;

		public static readonly string[] BloodGroups = new string[] { "(I) O+", "(I) O-", "(II) A+", "(II) A-", "(III) B+", "(III) B-", "(IV) AB+", "(IV) AB-" };

		private object FExtData;

		public object ExtData
		{
			get	{ return this.FExtData;	}
			set	{ this.FExtData = value; }
		}

		protected string ExtractDelimiter(string S, int Max)
		{
			string Result = S;

			if (Result != null)
			{
				while (Result.Length > 0 && Result[0] == ' ')
				{
					Result = Result.Remove(0, 1);
					if (Max > 0)
					{
						Max--;
						if (Max == 0) break;
					}
				}
			}

			return Result;
		}

		protected string ExtractDotDelimiter(string S, int Max)
		{
			string Result = S;

			if (Result != null)
			{
				while (Result.Length > 0 && Result[0] == '.')
				{
					Result = Result.Remove(0, 1);
					if (Max > 0)
					{
						Max--;
						if (Max == 0) break;
					}
				}
			}

			return Result;
		}

		protected string ExtractString(string S, out string AString, string ADefault)
		{
			string result = S;

			if (!string.IsNullOrEmpty(result)) {
				int I = 0;
				while (I < result.Length && result[I] != ' ') {
					I++;
				}

				if (I > 0) {
					AString = result.Substring(0, I);
					result = result.Remove(0, I);
				} else {
					AString = ADefault;
				}
			} else {
				AString = ADefault;
			}

			return result;
		}

		protected string ExtractXRef(string S, out string AXRef, bool NoException, string ADefault)
		{
			string result = S;

			if (!string.IsNullOrEmpty(result) && result[0] == '@') {
				int P = result.IndexOf('@', 1);
				if (P > 0) {
					AXRef = result.Substring(1, P - 1);
					result = result.Remove(0, P + 1);
				} else {
					if (!NoException) {
						throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", S));
					}
					AXRef = ADefault;
				}
			} else {
				if (!NoException) {
					throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", S));
				}
				AXRef = ADefault;
			}

			return result;
		}

		public static string CleanXRef(string XRef)
		{
			string result = XRef;

			if (!string.IsNullOrEmpty(result)) {
				if (result[0] == '@') {
					result = result.Remove(0, 1);
				}

				if (result.Length > 0 && result[result.Length - 1] == '@') {
					result = result.Remove(result.Length - 1, 1);
				}
			}

			return result;
		}

		public static string EncloseXRef(string XRef)
		{
			if (!string.IsNullOrEmpty(XRef)) {
				if (XRef[0] != '@') {
					XRef = "@" + XRef;
				}

				if (XRef[XRef.Length - 1] != '@') {
					XRef += "@";
				}
			}
			return XRef;
		}

		public static string ExtractNumber(string S, out int N, bool NoException, int ADefault)
		{
			string result = S;

			if (!string.IsNullOrEmpty(result)) {
				int I = 0;
				while (I < result.Length && IsDigit(result[I])) {
					I++;
				}

				if (I > 0) {
					N = int.Parse(result.Substring(0, I));
					result = result.Remove(0, I);
				} else {
					if (!NoException) {
						throw new EGEDCOMException(string.Format("The string {0} doesn't start with a valid number", S));
					}
					N = ADefault;
				}
			} else {
				N = ADefault;
			}

			return result;
		}

		public static bool IsDigit(char C)
		{
			return C >= '0' && C <= '9';
		}

		public static bool IsDigits(string S)
		{
			bool res = false;

			if (!string.IsNullOrEmpty(S))
			{
				int I;
				for (I = 1; I <= S.Length; I++)
				{
					char c = S[I - 1];
					if (c < '0' || c >= ':')
					{
						break;
					}
				}
				res = (I > S.Length);
			}

			return res;
		}

		public void Free()
		{
			SysUtils.Free(this);
		}

		public static Encoding GetEncodingByCharacterSet(TGEDCOMCharacterSet cs)
		{
			Encoding res;
			switch (cs) {
				case TGEDCOMCharacterSet.csASCII:
					res = Encoding.GetEncoding(1251);
					break;
				case TGEDCOMCharacterSet.csUNICODE:
					res = Encoding.Unicode;
					break;
				case TGEDCOMCharacterSet.csUTF8:
					res = Encoding.UTF8;
					break;
				default:
					res = Encoding.Default; // FIXME: ANSEL, etc
					break;
			}
			return res;
		}

		protected TGEDCOMRestriction GetRestrictionVal(string S)
		{
			TGEDCOMRestriction res;
			if (S == "CONFIDENTIAL")
			{
				res = TGEDCOMRestriction.rnConfidential;
			}
			else if (S == "LOCKED")
			{
				res = TGEDCOMRestriction.rnLocked;
			}
			else if (S == "PRIVACY")
			{
				res = TGEDCOMRestriction.rnPrivacy;
			}
			else
			{
				res = TGEDCOMRestriction.rnNone;
			}
			return res;
		}

		protected string GetRestrictionStr(TGEDCOMRestriction val)
		{
			string S;
			switch (val) {
				case TGEDCOMRestriction.rnConfidential:
					S = "confidential";
					break;
				case TGEDCOMRestriction.rnLocked:
					S = "locked";
					break;
				case TGEDCOMRestriction.rnPrivacy:
					S = "privacy";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected TGEDCOMPedigreeLinkageType GetPedigreeLinkageTypeVal(string S)
		{
			TGEDCOMPedigreeLinkageType Result;
			if (S == "adopted")
			{
				Result = TGEDCOMPedigreeLinkageType.plAdopted;
			}
			else if (S == "birth")
			{
				Result = TGEDCOMPedigreeLinkageType.plBirth;
			}
			else if (S == "foster")
			{
				Result = TGEDCOMPedigreeLinkageType.plFoster;
			}
			else if (S == "sealing")
			{
				Result = TGEDCOMPedigreeLinkageType.plSealing;
			}
			else
			{
				Result = TGEDCOMPedigreeLinkageType.plNone;
			}
			return Result;
		}

		protected string GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType val)
		{
			string S;
			switch (val) {
				case TGEDCOMPedigreeLinkageType.plAdopted:
					S = "adopted";
					break;
				case TGEDCOMPedigreeLinkageType.plBirth:
					S = "birth";
					break;
				case TGEDCOMPedigreeLinkageType.plFoster:
					S = "foster";
					break;
				case TGEDCOMPedigreeLinkageType.plSealing:
					S = "sealing";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected TGEDCOMChildLinkageStatus GetChildLinkageStatusVal(string S)
		{
			TGEDCOMChildLinkageStatus Result;
			if (S == "challenged")
			{
				Result = TGEDCOMChildLinkageStatus.clChallenged;
			}
			else if (S == "disproven")
			{
				Result = TGEDCOMChildLinkageStatus.clDisproven;
			}
			else if (S == "proven")
			{
				Result = TGEDCOMChildLinkageStatus.clProven;
			}
			else
			{
				Result = TGEDCOMChildLinkageStatus.clNone;
			}
			return Result;
		}

		protected string GetChildLinkageStatusStr(TGEDCOMChildLinkageStatus val)
		{
			string S;
			switch (val) {
				case TGEDCOMChildLinkageStatus.clChallenged:
					S = "challenged";
					break;
				case TGEDCOMChildLinkageStatus.clDisproven:
					S = "disproven";
					break;
				case TGEDCOMChildLinkageStatus.clProven:
					S = "proven";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected TCommunicationType GetCommunicationTypeVal(string S)
		{
			TCommunicationType Result;
			if (S == "call")
			{
				Result = TCommunicationType.ctCall;
			}
			else if (S == "email")
			{
				Result = TCommunicationType.ctEMail;
			}
			else if (S == "fax")
			{
				Result = TCommunicationType.ctFax;
			}
			else if (S == "letter")
			{
				Result = TCommunicationType.ctLetter;
			}
			else if (S == "tape")
			{
				Result = TCommunicationType.ctTape;
			}
			else if (S == "visit")
			{
				Result = TCommunicationType.ctVisit;
			}
			else
			{
				Result = TCommunicationType.ctVisit;
			}
			return Result;
		}

		protected string GetCommunicationTypeStr(TCommunicationType Value)
		{
			string S = "";
			switch (Value) {
				case TCommunicationType.ctCall:
					S = "call";
					break;
				case TCommunicationType.ctEMail:
					S = "email";
					break;
				case TCommunicationType.ctFax:
					S = "fax";
					break;
				case TCommunicationType.ctLetter:
					S = "letter";
					break;
				case TCommunicationType.ctTape:
					S = "tape";
					break;
				case TCommunicationType.ctVisit:
					S = "visit";
					break;
			}
			return S;
		}

		protected TGEDCOMMultimediaFormat GetMultimediaFormatVal(string S)
		{
			TGEDCOMMultimediaFormat Result;
			if (S == "")
			{
				Result = TGEDCOMMultimediaFormat.mfNone;
			}
			else if (S == "BMP")
			{
				Result = TGEDCOMMultimediaFormat.mfBMP;
			}
			else if (S == "GIF")
			{
				Result = TGEDCOMMultimediaFormat.mfGIF;
			}
			else if (S == "JPG")
			{
				Result = TGEDCOMMultimediaFormat.mfJPG;
			}
			else if (S == "OLE")
			{
				Result = TGEDCOMMultimediaFormat.mfOLE;
			}
			else if (S == "PCX")
			{
				Result = TGEDCOMMultimediaFormat.mfPCX;
			}
			else if (S == "TIF")
			{
				Result = TGEDCOMMultimediaFormat.mfTIF;
			}
			else if (S == "WAV")
			{
				Result = TGEDCOMMultimediaFormat.mfWAV;
			}
			else if (S == "TXT")
			{
				Result = TGEDCOMMultimediaFormat.mfTXT;
			}
			else if (S == "RTF")
			{
				Result = TGEDCOMMultimediaFormat.mfRTF;
			}
			else if (S == "AVI")
			{
				Result = TGEDCOMMultimediaFormat.mfAVI;
			}
			else if (S == "TGA")
			{
				Result = TGEDCOMMultimediaFormat.mfTGA;
			}
			else if (S == "PNG")
			{
				Result = TGEDCOMMultimediaFormat.mfPNG;
			}
			else if (S == "MPG")
			{
				Result = TGEDCOMMultimediaFormat.mfMPG;
			}
			else if (S == "HTM")
			{
				Result = TGEDCOMMultimediaFormat.mfHTM;
			}
			else
			{
				Result = TGEDCOMMultimediaFormat.mfUnknown;
			}
			return Result;
		}

		protected string GetMultimediaFormatStr(TGEDCOMMultimediaFormat Value)
		{
			string S;
			switch (Value) {
				case TGEDCOMMultimediaFormat.mfBMP:
					S = "bmp";
					break;
				case TGEDCOMMultimediaFormat.mfGIF:
					S = "gif";
					break;
				case TGEDCOMMultimediaFormat.mfJPG:
					S = "jpg";
					break;
				case TGEDCOMMultimediaFormat.mfOLE:
					S = "ole";
					break;
				case TGEDCOMMultimediaFormat.mfPCX:
					S = "pcx";
					break;
				case TGEDCOMMultimediaFormat.mfTIF:
					S = "tif";
					break;
				case TGEDCOMMultimediaFormat.mfWAV:
					S = "wav";
					break;
				case TGEDCOMMultimediaFormat.mfTXT:
					S = "txt";
					break;
				case TGEDCOMMultimediaFormat.mfRTF:
					S = "rtf";
					break;
				case TGEDCOMMultimediaFormat.mfAVI:
					S = "avi";
					break;
				case TGEDCOMMultimediaFormat.mfTGA:
					S = "tga";
					break;
				case TGEDCOMMultimediaFormat.mfPNG:
					S = "png";
					break;
				case TGEDCOMMultimediaFormat.mfMPG:
					S = "mpg";
					break;
				case TGEDCOMMultimediaFormat.mfHTM:
					S = "htm";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected TGEDCOMMediaType GetMediaTypeVal(string S)
		{
			TGEDCOMMediaType Result;
			if (S == "")
			{
				Result = TGEDCOMMediaType.mtNone;
			}
			else if (S == "AUDIO")
			{
				Result = TGEDCOMMediaType.mtAudio;
			}
			else if (S == "BOOK")
			{
				Result = TGEDCOMMediaType.mtBook;
			}
			else if (S == "CARD")
			{
				Result = TGEDCOMMediaType.mtCard;
			}
			else if (S == "ELECTRONIC")
			{
				Result = TGEDCOMMediaType.mtElectronic;
			}
			else if (S == "FICHE")
			{
				Result = TGEDCOMMediaType.mtFiche;
			}
			else if (S == "FILM")
			{
				Result = TGEDCOMMediaType.mtFilm;
			}
			else if (S == "MAGAZINE")
			{
				Result = TGEDCOMMediaType.mtMagazine;
			}
			else if (S == "MANUSCRIPT")
			{
				Result = TGEDCOMMediaType.mtManuscript;
			}
			else if (S == "MAP")
			{
				Result = TGEDCOMMediaType.mtMap;
			}
			else if (S == "NEWSPAPER")
			{
				Result = TGEDCOMMediaType.mtNewspaper;
			}
			else if (S == "PHOTO")
			{
				Result = TGEDCOMMediaType.mtPhoto;
			}
			else if (S == "TOMBSTONE")
			{
				Result = TGEDCOMMediaType.mtTombstone;
			}
			else if (S == "VIDEO")
			{
				Result = TGEDCOMMediaType.mtVideo;
			}
			else
			{
				Result = TGEDCOMMediaType.mtUnknown;
			}
			return Result;
		}

		protected string GetMediaTypeStr(TGEDCOMMediaType Value)
		{
			string S;
			switch (Value) {
				case TGEDCOMMediaType.mtAudio:
					S = "audio";
					break;
				case TGEDCOMMediaType.mtBook:
					S = "book";
					break;
				case TGEDCOMMediaType.mtCard:
					S = "card";
					break;
				case TGEDCOMMediaType.mtElectronic:
					S = "electronic";
					break;
				case TGEDCOMMediaType.mtFiche:
					S = "fiche";
					break;
				case TGEDCOMMediaType.mtFilm:
					S = "film";
					break;
				case TGEDCOMMediaType.mtMagazine:
					S = "magazine";
					break;
				case TGEDCOMMediaType.mtManuscript:
					S = "manuscript";
					break;
				case TGEDCOMMediaType.mtMap:
					S = "map";
					break;
				case TGEDCOMMediaType.mtNewspaper:
					S = "newspaper";
					break;
				case TGEDCOMMediaType.mtPhoto:
					S = "photo";
					break;
				case TGEDCOMMediaType.mtTombstone:
					S = "tombstone";
					break;
				case TGEDCOMMediaType.mtVideo:
					S = "video";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected TGEDCOMNameType GetNameTypeVal(string S)
		{
			TGEDCOMNameType Result;
			if (S == "aka")
			{
				Result = TGEDCOMNameType.ntAka;
			}
			else if (S == "birth")
			{
				Result = TGEDCOMNameType.ntBirth;
			}
			else if (S == "immigrant")
			{
				Result = TGEDCOMNameType.ntImmigrant;
			}
			else if (S == "maiden")
			{
				Result = TGEDCOMNameType.ntMaiden;
			}
			else if (S == "married")
			{
				Result = TGEDCOMNameType.ntMarried;
			}
			else
			{
				Result = TGEDCOMNameType.ntNone;
			}
			return Result;
		}

		protected string GetNameTypeStr(TGEDCOMNameType Value)
		{
			string S = "";
			switch (Value) {
				case TGEDCOMNameType.ntNone:
					S = "";
					break;
				case TGEDCOMNameType.ntAka:
					S = "aka";
					break;
				case TGEDCOMNameType.ntBirth:
					S = "birth";
					break;
				case TGEDCOMNameType.ntImmigrant:
					S = "immigrant";
					break;
				case TGEDCOMNameType.ntMaiden:
					S = "maiden";
					break;
				case TGEDCOMNameType.ntMarried:
					S = "married";
					break;
			}
			return S;
		}

		protected TResearchStatus GetStatusVal(string S)
		{
			TResearchStatus Result;
			if (S == "inprogress")
			{
				Result = TResearchStatus.rsInProgress;
			}
			else if (S == "onhold")
			{
				Result = TResearchStatus.rsOnHold;
			}
			else if (S == "problems")
			{
				Result = TResearchStatus.rsProblems;
			}
			else if (S == "completed")
			{
				Result = TResearchStatus.rsCompleted;
			}
			else if (S == "withdrawn")
			{
				Result = TResearchStatus.rsWithdrawn;
			}
			else
			{
				Result = TResearchStatus.rsDefined;
			}
			return Result;
		}

		protected string GetStatusStr(TResearchStatus Value)
		{
			string S = "";
			switch (Value) {
				case TResearchStatus.rsDefined:
					S = "defined";
					break;
				case TResearchStatus.rsInProgress:
					S = "inprogress";
					break;
				case TResearchStatus.rsOnHold:
					S = "onhold";
					break;
				case TResearchStatus.rsProblems:
					S = "problems";
					break;
				case TResearchStatus.rsCompleted:
					S = "completed";
					break;
				case TResearchStatus.rsWithdrawn:
					S = "withdrawn";
					break;
			}
			return S;
		}

		protected TGEDCOMSpouseSealingDateStatus GetSpouseSealingDateStatusVal(string S)
		{
			TGEDCOMSpouseSealingDateStatus Result;
			if (S == "CANCELED")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsCanceled;
			}
			else if (S == "COMPLETED")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsExcluded;
			}
			else if (S == "DNS")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsDNS;
			}
			else if (S == "DNS/CAN")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsDNSCAN;
			}
			else if (S == "PRE-1970")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsPre1970;
			}
			else if (S == "SUBMITTED")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsUncleared;
			}
			else
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsNone;
			}
			return Result;
		}

		protected string GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus Value)
		{
			string S;
			switch (Value) {
				case TGEDCOMSpouseSealingDateStatus.sdsCanceled:
					S = "CANCELED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsCompleted:
					S = "COMPLETED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsExcluded:
					S = "EXCLUDED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsDNS:
					S = "DNS";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsDNSCAN:
					S = "DNS/CAN";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsPre1970:
					S = "PRE-1970";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsSubmitted:
					S = "SUBMITTED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsUncleared:
					S = "UNCLEARED";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected TGEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string S)
		{
			TGEDCOMOrdinanceProcessFlag Result;
			if (S == "YES")
			{
				Result = TGEDCOMOrdinanceProcessFlag.opYes;
			}
			else if (S == "NO")
			{
				Result = TGEDCOMOrdinanceProcessFlag.opNo;
			}
			else
			{
				Result = TGEDCOMOrdinanceProcessFlag.opNone;
			}
			return Result;
		}

		protected string GetOrdinanceProcessFlagStr(TGEDCOMOrdinanceProcessFlag Value)
		{
			string S;
			switch (Value) {
				case TGEDCOMOrdinanceProcessFlag.opNone:
					S = "";
					break;
				case TGEDCOMOrdinanceProcessFlag.opYes:
					S = "yes";
					break;
				case TGEDCOMOrdinanceProcessFlag.opNo:
					S = "no";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

		protected string GetPriorityStr(TResearchPriority val)
		{
			string S = "";
			switch (val)
			{
				case TResearchPriority.rpNone:
					S = "";
					break;
				case TResearchPriority.rpLow:
					S = "low";
					break;
				case TResearchPriority.rpNormal:
					S = "normal";
					break;
				case TResearchPriority.rpHigh:
					S = "high";
					break;
				case TResearchPriority.rpTop:
					S = "top";
					break;
			}
			return S;
		}

		protected TResearchPriority GetPriorityVal(string S)
		{
			TResearchPriority result;

			if (S == "low")
			{
				result = TResearchPriority.rpLow;
			}
			else if (S == "normal")
			{
				result = TResearchPriority.rpNormal;
			}
			else if (S == "high")
			{
				result = TResearchPriority.rpHigh;
			}
			else if (S == "top")
			{
				result = TResearchPriority.rpTop;
			}
			else
			{
				result = TResearchPriority.rpNone;
			}
			
			return result;
		}

		protected string GetCharacterSetStr(TGEDCOMCharacterSet Value)
		{
			string S = "";
			switch (Value) {
				case TGEDCOMCharacterSet.csASCII:
					S = "ASCII";
					break;
				case TGEDCOMCharacterSet.csANSEL:
					S = "ANSEL";
					break;
				case TGEDCOMCharacterSet.csUNICODE:
					S = "UNICODE";
					break;
				case TGEDCOMCharacterSet.csUTF8:
					S = "UTF-8";
					break;
			}
			return S;
		}

		protected TGEDCOMCharacterSet GetCharacterSetVal(string S)
		{
			TGEDCOMCharacterSet Result;

			string SU = S.ToUpperInvariant();
			switch (SU) {
				case "ASCII":
				case "ANSI":
				case "IBMPC":
					Result = TGEDCOMCharacterSet.csASCII;
					break;
				case "ANSEL":
					Result = TGEDCOMCharacterSet.csANSEL;
					break;
				case "UNICODE":
					Result = TGEDCOMCharacterSet.csUNICODE;
					break;
				case "UTF8":
				case "UTF-8":
					Result = TGEDCOMCharacterSet.csUTF8;
					break;
				default:
					Result = TGEDCOMCharacterSet.csANSEL;
					break;
			}
			
			/*if (S == "ASCII" || S == "ANSI" || S == "IBMPC")
			{
				Result = TGEDCOMCharacterSet.csASCII;
			}
			else if (S == "ANSEL")
			{
				Result = TGEDCOMCharacterSet.csANSEL;
			}
			else if (S == "UNICODE")
			{
				Result = TGEDCOMCharacterSet.csUNICODE;
			}
			else if (S == "UTF8" || S == "UTF-8")
			{
				Result = TGEDCOMCharacterSet.csUTF8;
			}
			else
			{
				Result = TGEDCOMCharacterSet.csANSEL;
			}*/
			return Result;
		}

		protected TGEDCOMSex GetSexVal(string S)
		{
			TGEDCOMSex Result;

			switch (S) {
				case "M":
					Result = TGEDCOMSex.svMale;
					break;
				case "F":
					Result = TGEDCOMSex.svFemale;
					break;
				case "U":
					Result = TGEDCOMSex.svUndetermined;
					break;
				default:
					Result = TGEDCOMSex.svNone;
					break;
			}
			
			return Result;
		}

		protected string GetSexStr(TGEDCOMSex Value)
		{
			string S = "";
			
			switch (Value) {
				case TGEDCOMSex.svMale:
					S = "M";
					break;
				case TGEDCOMSex.svFemale:
					S = "F";
					break;
				case TGEDCOMSex.svUndetermined:
					S = "U";
					break;
				default:
					S = "";
					break;
			}
			
			return S;
		}
		
	}
}
