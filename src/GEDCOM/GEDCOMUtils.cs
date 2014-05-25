using System;
using System.Text;

using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public static class GEDCOMUtils
	{
		#region Tag properties
		
		public struct TagProperties
		{
			public readonly string Name;
			public readonly bool EmptySkip;

			public TagProperties(string name, bool emptySkip) {
				this.Name = name;
				this.EmptySkip = emptySkip;
			}
		}

		private static readonly TagProperties[] TagBase;


		static GEDCOMUtils()
		{
            TagProperties[] array = new TagProperties[26];

			array[0] = new TagProperties("", false);
			array[1] = new TagProperties("ADDR", true);
			array[2] = new TagProperties("AGNC", true);
			array[3] = new TagProperties("AUTH", true);
			array[4] = new TagProperties("CAUS", true);
			array[5] = new TagProperties("CHAN", true);
			array[6] = new TagProperties("CITY", true);
			array[7] = new TagProperties("CTRY", true);
			array[8] = new TagProperties("DATE", true);
			array[9] = new TagProperties("PAGE", true);
			array[10] = new TagProperties("PLAC", true);
			array[11] = new TagProperties("POST", true);
			array[12] = new TagProperties("PUBL", true);
			array[13] = new TagProperties("RESN", true);
			array[14] = new TagProperties("STAE", true);
			array[15] = new TagProperties("TEXT", true);
			array[16] = new TagProperties("TIME", true);
			array[17] = new TagProperties("TYPE", true);
			array[18] = new TagProperties("SUBM", true);
			array[19] = new TagProperties("NPFX", true);
			array[20] = new TagProperties("GIVN", true);
			array[21] = new TagProperties("NICK", true);
			array[22] = new TagProperties("SPFX", true);
			array[23] = new TagProperties("SURN", true);
			array[24] = new TagProperties("NSFX", true);
			array[25] = new TagProperties("_LOC", true);

            GEDCOMUtils.TagBase = array;
		}

		public static TagProperties GetTagProps(string tagName)
		{
			TagProperties result;

			int num = TagBase.Length - 1;
			for (int i = 1; i <= num; i++)
			{
				if (TagBase[i].Name == tagName)
				{
					result = TagBase[i];
					return result;
				}
			}

			result = TagBase[0];
			return result;
		}

		#endregion
		
		#region Parse functions

		public static string ExtractDelimiter(string str, int max)
		{
			string result = str;

			if (result != null)
			{
				while (result.Length > 0 && result[0] == ' ')
				{
					result = result.Remove(0, 1);
					if (max > 0)
					{
						max--;
						if (max == 0) break;
					}
				}
			}

			return result;
		}

		public static string ExtractDotDelimiter(string str, int max)
		{
			string result = str;

			if (result != null)
			{
				while (result.Length > 0 && result[0] == '.')
				{
					result = result.Remove(0, 1);
					if (max > 0)
					{
						max--;
						if (max == 0) break;
					}
				}
			}

			return result;
		}

		public static string ExtractString(string str, out string value, string defValue)
		{
			string result = str;

			if (!string.IsNullOrEmpty(result)) {
				int I = 0;
				while (I < result.Length && result[I] != ' ') {
					I++;
				}

				if (I > 0) {
					value = result.Substring(0, I);
					result = result.Remove(0, I);
				} else {
					value = defValue;
				}
			} else {
				value = defValue;
			}

			return result;
		}

		public static string ExtractXRef(string str, out string aXRef, bool noException, string defValue)
		{
			string result = str;

			if (!string.IsNullOrEmpty(result) && result[0] == '@') {
				int p = result.IndexOf('@', 1);
				if (p > 0) {
					aXRef = result.Substring(1, p - 1);
					result = result.Remove(0, p + 1);
				} else {
					if (!noException) {
						throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", str));
					}
					aXRef = defValue;
				}
			} else {
				if (!noException) {
					throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", str));
				}
				aXRef = defValue;
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

		public static string ExtractNumber(string str, out int value, bool noException, int defValue)
		{
			string result = str;

			if (!string.IsNullOrEmpty(result)) {
				int I = 0;
				while (I < result.Length && IsDigit(result[I])) {
					I++;
				}

				if (I > 0) {
					value = int.Parse(result.Substring(0, I));
					result = result.Remove(0, I);
				} else {
					if (!noException) {
						throw new EGEDCOMException(string.Format("The string {0} doesn't start with a valid number", str));
					}
					value = defValue;
				}
			} else {
				value = defValue;
			}

			return result;
		}

		public static bool IsDigit(char chr)
		{
			return chr >= '0' && chr <= '9';
		}

		public static bool IsDigits(string str)
		{
			bool res = false;

			if (!string.IsNullOrEmpty(str))
			{
				int I;
				for (I = 1; I <= str.Length; I++)
				{
					char c = str[I - 1];
					if (c < '0' || c >= ':')
					{
						break;
					}
				}
				res = (I > str.Length);
			}

			return res;
		}

		#endregion
		
		#region GEDCOM Enums processing
		
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
					res = Encoding.Default; // TODO: ANSEL, etc
					break;
			}
			return res;
		}

		public static TGEDCOMRestriction GetRestrictionVal(string str)
		{
			TGEDCOMRestriction res;
			str = str.Trim().ToUpperInvariant();
			
			if (str == "CONFIDENTIAL")
			{
				res = TGEDCOMRestriction.rnConfidential;
			}
			else if (str == "LOCKED")
			{
				res = TGEDCOMRestriction.rnLocked;
			}
			else if (str == "PRIVACY")
			{
				res = TGEDCOMRestriction.rnPrivacy;
			}
			else
			{
				res = TGEDCOMRestriction.rnNone;
			}
			return res;
		}

		public static string GetRestrictionStr(TGEDCOMRestriction value)
		{
			string s;
			switch (value) {
				case TGEDCOMRestriction.rnConfidential:
					s = "confidential";
					break;
				case TGEDCOMRestriction.rnLocked:
					s = "locked";
					break;
				case TGEDCOMRestriction.rnPrivacy:
					s = "privacy";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static TGEDCOMPedigreeLinkageType GetPedigreeLinkageTypeVal(string str)
		{
			TGEDCOMPedigreeLinkageType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "adopted")
			{
				result = TGEDCOMPedigreeLinkageType.plAdopted;
			}
			else if (str == "birth")
			{
				result = TGEDCOMPedigreeLinkageType.plBirth;
			}
			else if (str == "foster")
			{
				result = TGEDCOMPedigreeLinkageType.plFoster;
			}
			else if (str == "sealing")
			{
				result = TGEDCOMPedigreeLinkageType.plSealing;
			}
			else
			{
				result = TGEDCOMPedigreeLinkageType.plNone;
			}
			return result;
		}

		public static string GetPedigreeLinkageTypeStr(TGEDCOMPedigreeLinkageType value)
		{
			string s;
			switch (value) {
				case TGEDCOMPedigreeLinkageType.plAdopted:
					s = "adopted";
					break;
				case TGEDCOMPedigreeLinkageType.plBirth:
					s = "birth";
					break;
				case TGEDCOMPedigreeLinkageType.plFoster:
					s = "foster";
					break;
				case TGEDCOMPedigreeLinkageType.plSealing:
					s = "sealing";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static TGEDCOMChildLinkageStatus GetChildLinkageStatusVal(string str)
		{
			TGEDCOMChildLinkageStatus result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "challenged")
			{
				result = TGEDCOMChildLinkageStatus.clChallenged;
			}
			else if (str == "disproven")
			{
				result = TGEDCOMChildLinkageStatus.clDisproven;
			}
			else if (str == "proven")
			{
				result = TGEDCOMChildLinkageStatus.clProven;
			}
			else
			{
				result = TGEDCOMChildLinkageStatus.clNone;
			}
			return result;
		}

		public static string GetChildLinkageStatusStr(TGEDCOMChildLinkageStatus value)
		{
			string s;
			switch (value) {
				case TGEDCOMChildLinkageStatus.clChallenged:
					s = "challenged";
					break;
				case TGEDCOMChildLinkageStatus.clDisproven:
					s = "disproven";
					break;
				case TGEDCOMChildLinkageStatus.clProven:
					s = "proven";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static TCommunicationType GetCommunicationTypeVal(string str)
		{
			TCommunicationType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "call")
			{
				result = TCommunicationType.ctCall;
			}
			else if (str == "email")
			{
				result = TCommunicationType.ctEMail;
			}
			else if (str == "fax")
			{
				result = TCommunicationType.ctFax;
			}
			else if (str == "letter")
			{
				result = TCommunicationType.ctLetter;
			}
			else if (str == "tape")
			{
				result = TCommunicationType.ctTape;
			}
			else if (str == "visit")
			{
				result = TCommunicationType.ctVisit;
			}
			else
			{
				result = TCommunicationType.ctVisit;
			}
			return result;
		}

		public static string GetCommunicationTypeStr(TCommunicationType value)
		{
			string s = "";
			switch (value) {
				case TCommunicationType.ctCall:
					s = "call";
					break;
				case TCommunicationType.ctEMail:
					s = "email";
					break;
				case TCommunicationType.ctFax:
					s = "fax";
					break;
				case TCommunicationType.ctLetter:
					s = "letter";
					break;
				case TCommunicationType.ctTape:
					s = "tape";
					break;
				case TCommunicationType.ctVisit:
					s = "visit";
					break;
			}
			return s;
		}

		public static TGEDCOMMultimediaFormat GetMultimediaFormatVal(string str)
		{
			TGEDCOMMultimediaFormat result;
			str = str.Trim().ToUpperInvariant();
			
			if (str == "")
			{
				result = TGEDCOMMultimediaFormat.mfNone;
			}
			else if (str == "BMP")
			{
				result = TGEDCOMMultimediaFormat.mfBMP;
			}
			else if (str == "GIF")
			{
				result = TGEDCOMMultimediaFormat.mfGIF;
			}
			else if (str == "JPG")
			{
				result = TGEDCOMMultimediaFormat.mfJPG;
			}
			else if (str == "OLE")
			{
				result = TGEDCOMMultimediaFormat.mfOLE;
			}
			else if (str == "PCX")
			{
				result = TGEDCOMMultimediaFormat.mfPCX;
			}
			else if (str == "TIF")
			{
				result = TGEDCOMMultimediaFormat.mfTIF;
			}
			else if (str == "WAV")
			{
				result = TGEDCOMMultimediaFormat.mfWAV;
			}
			else if (str == "TXT")
			{
				result = TGEDCOMMultimediaFormat.mfTXT;
			}
			else if (str == "RTF")
			{
				result = TGEDCOMMultimediaFormat.mfRTF;
			}
			else if (str == "AVI")
			{
				result = TGEDCOMMultimediaFormat.mfAVI;
			}
			else if (str == "TGA")
			{
				result = TGEDCOMMultimediaFormat.mfTGA;
			}
			else if (str == "PNG")
			{
				result = TGEDCOMMultimediaFormat.mfPNG;
			}
			else if (str == "MPG")
			{
				result = TGEDCOMMultimediaFormat.mfMPG;
			}
			else if (str == "HTM")
			{
				result = TGEDCOMMultimediaFormat.mfHTM;
			}
			else if (str == "RAW")
			{
				result = TGEDCOMMultimediaFormat.mfRAW;
			}
			else if (str == "MP3")
			{
				result = TGEDCOMMultimediaFormat.mfMP3;
			}
			else if (str == "WMA")
			{
				result = TGEDCOMMultimediaFormat.mfWMA;
			}
			else
			{
				result = TGEDCOMMultimediaFormat.mfUnknown;
			}
			return result;
		}

		public static string GetMultimediaFormatStr(TGEDCOMMultimediaFormat value)
		{
			string s;
			switch (value) {
				case TGEDCOMMultimediaFormat.mfBMP:
					s = "bmp";
					break;
				case TGEDCOMMultimediaFormat.mfGIF:
					s = "gif";
					break;
				case TGEDCOMMultimediaFormat.mfJPG:
					s = "jpg";
					break;
				case TGEDCOMMultimediaFormat.mfOLE:
					s = "ole";
					break;
				case TGEDCOMMultimediaFormat.mfPCX:
					s = "pcx";
					break;
				case TGEDCOMMultimediaFormat.mfTIF:
					s = "tif";
					break;
				case TGEDCOMMultimediaFormat.mfWAV:
					s = "wav";
					break;
				case TGEDCOMMultimediaFormat.mfTXT:
					s = "txt";
					break;
				case TGEDCOMMultimediaFormat.mfRTF:
					s = "rtf";
					break;
				case TGEDCOMMultimediaFormat.mfAVI:
					s = "avi";
					break;
				case TGEDCOMMultimediaFormat.mfTGA:
					s = "tga";
					break;
				case TGEDCOMMultimediaFormat.mfPNG:
					s = "png";
					break;
				case TGEDCOMMultimediaFormat.mfMPG:
					s = "mpg";
					break;
				case TGEDCOMMultimediaFormat.mfHTM:
					s = "htm";
					break;
				case TGEDCOMMultimediaFormat.mfRAW:
					s = "raw";
					break;
				case TGEDCOMMultimediaFormat.mfMP3:
					s = "mp3";
					break;
				case TGEDCOMMultimediaFormat.mfWMA:
					s = "wma";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static TGEDCOMMediaType GetMediaTypeVal(string str)
		{
			TGEDCOMMediaType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "")
			{
				result = TGEDCOMMediaType.mtNone;
			}
			else if (str == "audio")
			{
				result = TGEDCOMMediaType.mtAudio;
			}
			else if (str == "book")
			{
				result = TGEDCOMMediaType.mtBook;
			}
			else if (str == "card")
			{
				result = TGEDCOMMediaType.mtCard;
			}
			else if (str == "electronic")
			{
				result = TGEDCOMMediaType.mtElectronic;
			}
			else if (str == "fiche")
			{
				result = TGEDCOMMediaType.mtFiche;
			}
			else if (str == "film")
			{
				result = TGEDCOMMediaType.mtFilm;
			}
			else if (str == "magazine")
			{
				result = TGEDCOMMediaType.mtMagazine;
			}
			else if (str == "manuscript")
			{
				result = TGEDCOMMediaType.mtManuscript;
			}
			else if (str == "map")
			{
				result = TGEDCOMMediaType.mtMap;
			}
			else if (str == "newspaper")
			{
				result = TGEDCOMMediaType.mtNewspaper;
			}
			else if (str == "photo")
			{
				result = TGEDCOMMediaType.mtPhoto;
			}
			else if (str == "tombstone")
			{
				result = TGEDCOMMediaType.mtTombstone;
			}
			else if (str == "video")
			{
				result = TGEDCOMMediaType.mtVideo;
			}
			else
			{
				result = TGEDCOMMediaType.mtUnknown;
			}
			return result;
		}

		public static string GetMediaTypeStr(TGEDCOMMediaType value)
		{
			string s;
			switch (value) {
				case TGEDCOMMediaType.mtAudio:
					s = "audio";
					break;
				case TGEDCOMMediaType.mtBook:
					s = "book";
					break;
				case TGEDCOMMediaType.mtCard:
					s = "card";
					break;
				case TGEDCOMMediaType.mtElectronic:
					s = "electronic";
					break;
				case TGEDCOMMediaType.mtFiche:
					s = "fiche";
					break;
				case TGEDCOMMediaType.mtFilm:
					s = "film";
					break;
				case TGEDCOMMediaType.mtMagazine:
					s = "magazine";
					break;
				case TGEDCOMMediaType.mtManuscript:
					s = "manuscript";
					break;
				case TGEDCOMMediaType.mtMap:
					s = "map";
					break;
				case TGEDCOMMediaType.mtNewspaper:
					s = "newspaper";
					break;
				case TGEDCOMMediaType.mtPhoto:
					s = "photo";
					break;
				case TGEDCOMMediaType.mtTombstone:
					s = "tombstone";
					break;
				case TGEDCOMMediaType.mtVideo:
					s = "video";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static TGEDCOMNameType GetNameTypeVal(string str)
		{
			TGEDCOMNameType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "aka")
			{
				result = TGEDCOMNameType.ntAka;
			}
			else if (str == "birth")
			{
				result = TGEDCOMNameType.ntBirth;
			}
			else if (str == "immigrant")
			{
				result = TGEDCOMNameType.ntImmigrant;
			}
			else if (str == "maiden")
			{
				result = TGEDCOMNameType.ntMaiden;
			}
			else if (str == "married")
			{
				result = TGEDCOMNameType.ntMarried;
			}
			else
			{
				result = TGEDCOMNameType.ntNone;
			}
			return result;
		}

		public static string GetNameTypeStr(TGEDCOMNameType value)
		{
			string s = "";
			switch (value) {
				case TGEDCOMNameType.ntNone:
					s = "";
					break;
				case TGEDCOMNameType.ntAka:
					s = "aka";
					break;
				case TGEDCOMNameType.ntBirth:
					s = "birth";
					break;
				case TGEDCOMNameType.ntImmigrant:
					s = "immigrant";
					break;
				case TGEDCOMNameType.ntMaiden:
					s = "maiden";
					break;
				case TGEDCOMNameType.ntMarried:
					s = "married";
					break;
			}
			return s;
		}

		public static TResearchStatus GetStatusVal(string str)
		{
			TResearchStatus result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "inprogress")
			{
				result = TResearchStatus.rsInProgress;
			}
			else if (str == "onhold")
			{
				result = TResearchStatus.rsOnHold;
			}
			else if (str == "problems")
			{
				result = TResearchStatus.rsProblems;
			}
			else if (str == "completed")
			{
				result = TResearchStatus.rsCompleted;
			}
			else if (str == "withdrawn")
			{
				result = TResearchStatus.rsWithdrawn;
			}
			else
			{
				result = TResearchStatus.rsDefined;
			}
			return result;
		}

		public static string GetStatusStr(TResearchStatus value)
		{
			string s = "";
			switch (value) {
				case TResearchStatus.rsDefined:
					s = "defined";
					break;
				case TResearchStatus.rsInProgress:
					s = "inprogress";
					break;
				case TResearchStatus.rsOnHold:
					s = "onhold";
					break;
				case TResearchStatus.rsProblems:
					s = "problems";
					break;
				case TResearchStatus.rsCompleted:
					s = "completed";
					break;
				case TResearchStatus.rsWithdrawn:
					s = "withdrawn";
					break;
			}
			return s;
		}

		public static TGEDCOMSpouseSealingDateStatus GetSpouseSealingDateStatusVal(string str)
		{
			TGEDCOMSpouseSealingDateStatus result;
			str = str.Trim().ToUpperInvariant();
			
			if (str == "CANCELED")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsCanceled;
			}
			else if (str == "COMPLETED")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsCompleted;
			}
			else if (str == "EXCLUDED")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsExcluded;
			}
			else if (str == "DNS")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsDNS;
			}
			else if (str == "DNS/CAN")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsDNSCAN;
			}
			else if (str == "PRE-1970")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsPre1970;
			}
			else if (str == "SUBMITTED")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsSubmitted;
			}
			else if (str == "UNCLEARED")
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsUncleared;
			}
			else
			{
				result = TGEDCOMSpouseSealingDateStatus.sdsNone;
			}
			return result;
		}

		public static string GetSpouseSealingDateStatusStr(TGEDCOMSpouseSealingDateStatus value)
		{
			string str;
			switch (value) {
				case TGEDCOMSpouseSealingDateStatus.sdsCanceled:
					str = "CANCELED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsCompleted:
					str = "COMPLETED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsExcluded:
					str = "EXCLUDED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsDNS:
					str = "DNS";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsDNSCAN:
					str = "DNS/CAN";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsPre1970:
					str = "PRE-1970";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsSubmitted:
					str = "SUBMITTED";
					break;
				case TGEDCOMSpouseSealingDateStatus.sdsUncleared:
					str = "UNCLEARED";
					break;
				default:
					str = "";
					break;
			}
			return str;
		}

		public static TGEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string str)
		{
			TGEDCOMOrdinanceProcessFlag result;
			str = str.Trim().ToUpperInvariant(); // FIXME
			
			if (str == "YES")
			{
				result = TGEDCOMOrdinanceProcessFlag.opYes;
			}
			else if (str == "NO")
			{
				result = TGEDCOMOrdinanceProcessFlag.opNo;
			}
			else
			{
				result = TGEDCOMOrdinanceProcessFlag.opNone;
			}
			return result;
		}

		public static string GetOrdinanceProcessFlagStr(TGEDCOMOrdinanceProcessFlag value)
		{
			string str = "";
			switch (value) {
				case TGEDCOMOrdinanceProcessFlag.opNone:
					str = "";
					break;
				case TGEDCOMOrdinanceProcessFlag.opYes:
					str = "yes";
					break;
				case TGEDCOMOrdinanceProcessFlag.opNo:
					str = "no";
					break;
			}
			return str;
		}

		public static string GetPriorityStr(TResearchPriority value)
		{
			string str = "";
			switch (value) {
				case TResearchPriority.rpNone:
					str = "";
					break;
				case TResearchPriority.rpLow:
					str = "low";
					break;
				case TResearchPriority.rpNormal:
					str = "normal";
					break;
				case TResearchPriority.rpHigh:
					str = "high";
					break;
				case TResearchPriority.rpTop:
					str = "top";
					break;
			}
			return str;
		}

		public static TResearchPriority GetPriorityVal(string str)
		{
			TResearchPriority result;
			string SU = str.Trim().ToLowerInvariant();

			if (SU == "low")
			{
				result = TResearchPriority.rpLow;
			}
			else if (SU == "normal")
			{
				result = TResearchPriority.rpNormal;
			}
			else if (SU == "high")
			{
				result = TResearchPriority.rpHigh;
			}
			else if (SU == "top")
			{
				result = TResearchPriority.rpTop;
			}
			else
			{
				result = TResearchPriority.rpNone;
			}
			
			return result;
		}

		public static string GetCharacterSetStr(TGEDCOMCharacterSet value)
		{
			string str = "";
			switch (value) {
				case TGEDCOMCharacterSet.csASCII:
					str = "ASCII";
					break;
				case TGEDCOMCharacterSet.csANSEL:
					str = "ANSEL";
					break;
				case TGEDCOMCharacterSet.csUNICODE:
					str = "UNICODE";
					break;
				case TGEDCOMCharacterSet.csUTF8:
					str = "UTF-8";
					break;
			}
			return str;
		}

		public static TGEDCOMCharacterSet GetCharacterSetVal(string str)
		{
			TGEDCOMCharacterSet result;
			string SU = str.ToUpperInvariant();

			if (SU == "ASCII" || SU == "ANSI" || SU == "IBMPC")
			{
				result = TGEDCOMCharacterSet.csASCII;
			}
			else if (SU == "ANSEL")
			{
				result = TGEDCOMCharacterSet.csANSEL;
			}
			else if (SU == "UNICODE")
			{
				result = TGEDCOMCharacterSet.csUNICODE;
			}
			else if (SU == "UTF8" || SU == "UTF-8")
			{
				result = TGEDCOMCharacterSet.csUTF8;
			}
			else
			{
				result = TGEDCOMCharacterSet.csASCII;
			}
			
			return result;
		}

		public static TGEDCOMSex GetSexVal(string str)
		{
			TGEDCOMSex result;
			string SU = str.Trim().ToUpperInvariant();

			switch (SU) {
				case "M":
					result = TGEDCOMSex.svMale;
					break;
				case "F":
					result = TGEDCOMSex.svFemale;
					break;
				case "U":
					result = TGEDCOMSex.svUndetermined;
					break;
				default:
					result = TGEDCOMSex.svNone;
					break;
			}
			
			return result;
		}

		public static string GetSexStr(TGEDCOMSex value)
		{
			string str;
			
			switch (value) {
				case TGEDCOMSex.svMale:
					str = "M";
					break;
				case TGEDCOMSex.svFemale:
					str = "F";
					break;
				case TGEDCOMSex.svUndetermined:
					str = "U";
					break;
				default:
					str = "";
					break;
			}
			
			return str;
		}
		
		#endregion
		
		#region Other

		public static string NormalizeName(string s)
		{
			if (string.IsNullOrEmpty(s)) return "";
			
			string st = s.Trim().ToLower();
			char f = Char.ToUpper(st[0]);
			st = f + st.Substring(1);
			return st;
		}

		public static string StrToGEDCOMDate(string aDate, bool aException)
		{
			string result = "";

			if (aDate.IndexOf("/") >= 0) aDate = aDate.Replace("/", ".");
			if (aDate.IndexOf("_") >= 0) aDate = aDate.Replace("_", " ");

			string[] dt_parts = aDate.Split('.');
			if (dt_parts.Length < 3)
			{
				if (aException)
				{
                    throw new GEDCOMDateException(string.Format("GKUtils.StrToGEDCOMDate(): date format is invalid {0}", aDate));
				}
			}
			else
			{
				string pd = dt_parts[0].Trim();
				string pm = dt_parts[1].Trim();
				string py = dt_parts[2].Trim();

				if (pd != "") result = result + pd + " ";
				if (pm != "") result = result + TGEDCOMCustomDate.GEDCOMMonthArray[SysUtils.ParseInt(pm, 1) - 1] + " ";
				if (py != "") result += py;
			}
			return result;
		}

		#endregion
	}
}
