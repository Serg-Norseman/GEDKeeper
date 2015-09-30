using System;
using System.Text;
using ExtUtils;
using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
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

			int num = TagBase.Length;
			for (int i = 1; i < num; i++)
			{
				if (TagBase[i].Name == tagName) {
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
		
		public static Encoding GetEncodingByCharacterSet(GEDCOMCharacterSet cs)
		{
			Encoding res;
			switch (cs) {
				case GEDCOMCharacterSet.csASCII:
					res = Encoding.GetEncoding(1251);
					break;
				case GEDCOMCharacterSet.csANSEL:
					res = new AnselEncoding();
					break;
				case GEDCOMCharacterSet.csUNICODE:
					res = Encoding.Unicode;
					break;
				case GEDCOMCharacterSet.csUTF8:
					res = Encoding.UTF8;
					break;
				default:
					res = Encoding.Default;
					break;
			}
			return res;
		}

		public static GEDCOMRestriction GetRestrictionVal(string str)
		{
			GEDCOMRestriction res;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "confidential")
			{
				res = GEDCOMRestriction.rnConfidential;
			}
			else if (str == "locked")
			{
				res = GEDCOMRestriction.rnLocked;
			}
			else if (str == "privacy")
			{
				res = GEDCOMRestriction.rnPrivacy;
			}
			else
			{
				res = GEDCOMRestriction.rnNone;
			}
			return res;
		}

		public static string GetRestrictionStr(GEDCOMRestriction value)
		{
			string s;
			switch (value) {
				case GEDCOMRestriction.rnConfidential:
					s = "confidential";
					break;
				case GEDCOMRestriction.rnLocked:
					s = "locked";
					break;
				case GEDCOMRestriction.rnPrivacy:
					s = "privacy";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static GEDCOMPedigreeLinkageType GetPedigreeLinkageTypeVal(string str)
		{
			GEDCOMPedigreeLinkageType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "adopted")
			{
				result = GEDCOMPedigreeLinkageType.plAdopted;
			}
			else if (str == "birth")
			{
				result = GEDCOMPedigreeLinkageType.plBirth;
			}
			else if (str == "foster")
			{
				result = GEDCOMPedigreeLinkageType.plFoster;
			}
			else if (str == "sealing")
			{
				result = GEDCOMPedigreeLinkageType.plSealing;
			}
			else
			{
				result = GEDCOMPedigreeLinkageType.plNone;
			}
			return result;
		}

		public static string GetPedigreeLinkageTypeStr(GEDCOMPedigreeLinkageType value)
		{
			string s;
			switch (value) {
				case GEDCOMPedigreeLinkageType.plAdopted:
					s = "adopted";
					break;
				case GEDCOMPedigreeLinkageType.plBirth:
					s = "birth";
					break;
				case GEDCOMPedigreeLinkageType.plFoster:
					s = "foster";
					break;
				case GEDCOMPedigreeLinkageType.plSealing:
					s = "sealing";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static GEDCOMChildLinkageStatus GetChildLinkageStatusVal(string str)
		{
			GEDCOMChildLinkageStatus result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "challenged")
			{
				result = GEDCOMChildLinkageStatus.clChallenged;
			}
			else if (str == "disproven")
			{
				result = GEDCOMChildLinkageStatus.clDisproven;
			}
			else if (str == "proven")
			{
				result = GEDCOMChildLinkageStatus.clProven;
			}
			else
			{
				result = GEDCOMChildLinkageStatus.clNone;
			}
			return result;
		}

		public static string GetChildLinkageStatusStr(GEDCOMChildLinkageStatus value)
		{
			string s;
			switch (value) {
				case GEDCOMChildLinkageStatus.clChallenged:
					s = "challenged";
					break;
				case GEDCOMChildLinkageStatus.clDisproven:
					s = "disproven";
					break;
				case GEDCOMChildLinkageStatus.clProven:
					s = "proven";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static GKCommunicationType GetCommunicationTypeVal(string str)
		{
			GKCommunicationType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "call")
			{
				result = GKCommunicationType.ctCall;
			}
			else if (str == "email")
			{
				result = GKCommunicationType.ctEMail;
			}
			else if (str == "fax")
			{
				result = GKCommunicationType.ctFax;
			}
			else if (str == "letter")
			{
				result = GKCommunicationType.ctLetter;
			}
			else if (str == "tape")
			{
				result = GKCommunicationType.ctTape;
			}
			else if (str == "visit")
			{
				result = GKCommunicationType.ctVisit;
			}
			else
			{
				result = GKCommunicationType.ctVisit;
			}
			return result;
		}

		public static string GetCommunicationTypeStr(GKCommunicationType value)
		{
			string s = "";
			switch (value) {
				case GKCommunicationType.ctCall:
					s = "call";
					break;
				case GKCommunicationType.ctEMail:
					s = "email";
					break;
				case GKCommunicationType.ctFax:
					s = "fax";
					break;
				case GKCommunicationType.ctLetter:
					s = "letter";
					break;
				case GKCommunicationType.ctTape:
					s = "tape";
					break;
				case GKCommunicationType.ctVisit:
					s = "visit";
					break;
			}
			return s;
		}

		public static GEDCOMMultimediaFormat GetMultimediaFormatVal(string str)
		{
			GEDCOMMultimediaFormat result;
			str = str.Trim().ToUpperInvariant();
			
			if (str == "")
			{
				result = GEDCOMMultimediaFormat.mfNone;
			}
			else if (str == "BMP")
			{
				result = GEDCOMMultimediaFormat.mfBMP;
			}
			else if (str == "GIF")
			{
				result = GEDCOMMultimediaFormat.mfGIF;
			}
			else if (str == "JPG")
			{
				result = GEDCOMMultimediaFormat.mfJPG;
			}
			else if (str == "OLE")
			{
				result = GEDCOMMultimediaFormat.mfOLE;
			}
			else if (str == "PCX")
			{
				result = GEDCOMMultimediaFormat.mfPCX;
			}
			else if (str == "TIF")
			{
				result = GEDCOMMultimediaFormat.mfTIF;
			}
			else if (str == "WAV")
			{
				result = GEDCOMMultimediaFormat.mfWAV;
			}
			else if (str == "TXT")
			{
				result = GEDCOMMultimediaFormat.mfTXT;
			}
			else if (str == "RTF")
			{
				result = GEDCOMMultimediaFormat.mfRTF;
			}
			else if (str == "AVI")
			{
				result = GEDCOMMultimediaFormat.mfAVI;
			}
			else if (str == "TGA")
			{
				result = GEDCOMMultimediaFormat.mfTGA;
			}
			else if (str == "PNG")
			{
				result = GEDCOMMultimediaFormat.mfPNG;
			}
			else if (str == "MPG")
			{
				result = GEDCOMMultimediaFormat.mfMPG;
			}
			else if (str == "HTM")
			{
				result = GEDCOMMultimediaFormat.mfHTM;
			}
			else if (str == "RAW")
			{
				result = GEDCOMMultimediaFormat.mfRAW;
			}
			else if (str == "MP3")
			{
				result = GEDCOMMultimediaFormat.mfMP3;
			}
			else if (str == "WMA")
			{
				result = GEDCOMMultimediaFormat.mfWMA;
			}
			else
			{
				result = GEDCOMMultimediaFormat.mfUnknown;
			}
			return result;
		}

		public static string GetMultimediaFormatStr(GEDCOMMultimediaFormat value)
		{
			string s;
			switch (value) {
				case GEDCOMMultimediaFormat.mfBMP:
					s = "bmp";
					break;
				case GEDCOMMultimediaFormat.mfGIF:
					s = "gif";
					break;
				case GEDCOMMultimediaFormat.mfJPG:
					s = "jpg";
					break;
				case GEDCOMMultimediaFormat.mfOLE:
					s = "ole";
					break;
				case GEDCOMMultimediaFormat.mfPCX:
					s = "pcx";
					break;
				case GEDCOMMultimediaFormat.mfTIF:
					s = "tif";
					break;
				case GEDCOMMultimediaFormat.mfWAV:
					s = "wav";
					break;
				case GEDCOMMultimediaFormat.mfTXT:
					s = "txt";
					break;
				case GEDCOMMultimediaFormat.mfRTF:
					s = "rtf";
					break;
				case GEDCOMMultimediaFormat.mfAVI:
					s = "avi";
					break;
				case GEDCOMMultimediaFormat.mfTGA:
					s = "tga";
					break;
				case GEDCOMMultimediaFormat.mfPNG:
					s = "png";
					break;
				case GEDCOMMultimediaFormat.mfMPG:
					s = "mpg";
					break;
				case GEDCOMMultimediaFormat.mfHTM:
					s = "htm";
					break;
				case GEDCOMMultimediaFormat.mfRAW:
					s = "raw";
					break;
				case GEDCOMMultimediaFormat.mfMP3:
					s = "mp3";
					break;
				case GEDCOMMultimediaFormat.mfWMA:
					s = "wma";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static GEDCOMMediaType GetMediaTypeVal(string str)
		{
			GEDCOMMediaType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "")
			{
				result = GEDCOMMediaType.mtNone;
			}
			else if (str == "audio")
			{
				result = GEDCOMMediaType.mtAudio;
			}
			else if (str == "book")
			{
				result = GEDCOMMediaType.mtBook;
			}
			else if (str == "card")
			{
				result = GEDCOMMediaType.mtCard;
			}
			else if (str == "electronic")
			{
				result = GEDCOMMediaType.mtElectronic;
			}
			else if (str == "fiche")
			{
				result = GEDCOMMediaType.mtFiche;
			}
			else if (str == "film")
			{
				result = GEDCOMMediaType.mtFilm;
			}
			else if (str == "magazine")
			{
				result = GEDCOMMediaType.mtMagazine;
			}
			else if (str == "manuscript")
			{
				result = GEDCOMMediaType.mtManuscript;
			}
			else if (str == "map")
			{
				result = GEDCOMMediaType.mtMap;
			}
			else if (str == "newspaper")
			{
				result = GEDCOMMediaType.mtNewspaper;
			}
			else if (str == "photo")
			{
				result = GEDCOMMediaType.mtPhoto;
			}
			else if (str == "tombstone")
			{
				result = GEDCOMMediaType.mtTombstone;
			}
			else if (str == "video")
			{
				result = GEDCOMMediaType.mtVideo;
			}
			else
			{
				result = GEDCOMMediaType.mtUnknown;
			}
			return result;
		}

		public static string GetMediaTypeStr(GEDCOMMediaType value)
		{
			string s;
			switch (value) {
				case GEDCOMMediaType.mtAudio:
					s = "audio";
					break;
				case GEDCOMMediaType.mtBook:
					s = "book";
					break;
				case GEDCOMMediaType.mtCard:
					s = "card";
					break;
				case GEDCOMMediaType.mtElectronic:
					s = "electronic";
					break;
				case GEDCOMMediaType.mtFiche:
					s = "fiche";
					break;
				case GEDCOMMediaType.mtFilm:
					s = "film";
					break;
				case GEDCOMMediaType.mtMagazine:
					s = "magazine";
					break;
				case GEDCOMMediaType.mtManuscript:
					s = "manuscript";
					break;
				case GEDCOMMediaType.mtMap:
					s = "map";
					break;
				case GEDCOMMediaType.mtNewspaper:
					s = "newspaper";
					break;
				case GEDCOMMediaType.mtPhoto:
					s = "photo";
					break;
				case GEDCOMMediaType.mtTombstone:
					s = "tombstone";
					break;
				case GEDCOMMediaType.mtVideo:
					s = "video";
					break;
				default:
					s = "";
					break;
			}
			return s;
		}

		public static GEDCOMNameType GetNameTypeVal(string str)
		{
			GEDCOMNameType result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "aka")
			{
				result = GEDCOMNameType.ntAka;
			}
			else if (str == "birth")
			{
				result = GEDCOMNameType.ntBirth;
			}
			else if (str == "immigrant")
			{
				result = GEDCOMNameType.ntImmigrant;
			}
			else if (str == "maiden")
			{
				result = GEDCOMNameType.ntMaiden;
			}
			else if (str == "married")
			{
				result = GEDCOMNameType.ntMarried;
			}
			else
			{
				result = GEDCOMNameType.ntNone;
			}
			return result;
		}

		public static string GetNameTypeStr(GEDCOMNameType value)
		{
			string s = "";
			switch (value) {
				case GEDCOMNameType.ntNone:
					s = "";
					break;
				case GEDCOMNameType.ntAka:
					s = "aka";
					break;
				case GEDCOMNameType.ntBirth:
					s = "birth";
					break;
				case GEDCOMNameType.ntImmigrant:
					s = "immigrant";
					break;
				case GEDCOMNameType.ntMaiden:
					s = "maiden";
					break;
				case GEDCOMNameType.ntMarried:
					s = "married";
					break;
			}
			return s;
		}

		public static GKResearchStatus GetStatusVal(string str)
		{
			GKResearchStatus result;
			str = str.Trim().ToLowerInvariant();
			
			if (str == "inprogress")
			{
				result = GKResearchStatus.rsInProgress;
			}
			else if (str == "onhold")
			{
				result = GKResearchStatus.rsOnHold;
			}
			else if (str == "problems")
			{
				result = GKResearchStatus.rsProblems;
			}
			else if (str == "completed")
			{
				result = GKResearchStatus.rsCompleted;
			}
			else if (str == "withdrawn")
			{
				result = GKResearchStatus.rsWithdrawn;
			}
			else
			{
				result = GKResearchStatus.rsDefined;
			}
			return result;
		}

		public static string GetStatusStr(GKResearchStatus value)
		{
			string s = "";
			switch (value) {
				case GKResearchStatus.rsDefined:
					s = "defined";
					break;
				case GKResearchStatus.rsInProgress:
					s = "inprogress";
					break;
				case GKResearchStatus.rsOnHold:
					s = "onhold";
					break;
				case GKResearchStatus.rsProblems:
					s = "problems";
					break;
				case GKResearchStatus.rsCompleted:
					s = "completed";
					break;
				case GKResearchStatus.rsWithdrawn:
					s = "withdrawn";
					break;
			}
			return s;
		}

		public static GEDCOMSpouseSealingDateStatus GetSpouseSealingDateStatusVal(string str)
		{
			GEDCOMSpouseSealingDateStatus result;
			str = str.Trim().ToUpperInvariant();
			
			if (str == "CANCELED")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsCanceled;
			}
			else if (str == "COMPLETED")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsCompleted;
			}
			else if (str == "EXCLUDED")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsExcluded;
			}
			else if (str == "DNS")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsDNS;
			}
			else if (str == "DNS/CAN")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsDNSCAN;
			}
			else if (str == "PRE-1970")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsPre1970;
			}
			else if (str == "SUBMITTED")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsSubmitted;
			}
			else if (str == "UNCLEARED")
			{
				result = GEDCOMSpouseSealingDateStatus.sdsUncleared;
			}
			else
			{
				result = GEDCOMSpouseSealingDateStatus.sdsNone;
			}
			return result;
		}

		public static string GetSpouseSealingDateStatusStr(GEDCOMSpouseSealingDateStatus value)
		{
			string str;
			switch (value) {
				case GEDCOMSpouseSealingDateStatus.sdsCanceled:
					str = "CANCELED";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsCompleted:
					str = "COMPLETED";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsExcluded:
					str = "EXCLUDED";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsDNS:
					str = "DNS";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsDNSCAN:
					str = "DNS/CAN";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsPre1970:
					str = "PRE-1970";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsSubmitted:
					str = "SUBMITTED";
					break;
				case GEDCOMSpouseSealingDateStatus.sdsUncleared:
					str = "UNCLEARED";
					break;
				default:
					str = "";
					break;
			}
			return str;
		}

		public static GEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string str)
		{
			GEDCOMOrdinanceProcessFlag result;
			str = str.Trim().ToUpperInvariant(); // FIXME
			
			if (str == "YES")
			{
				result = GEDCOMOrdinanceProcessFlag.opYes;
			}
			else if (str == "NO")
			{
				result = GEDCOMOrdinanceProcessFlag.opNo;
			}
			else
			{
				result = GEDCOMOrdinanceProcessFlag.opNone;
			}
			return result;
		}

		public static string GetOrdinanceProcessFlagStr(GEDCOMOrdinanceProcessFlag value)
		{
			string str = "";
			switch (value) {
				case GEDCOMOrdinanceProcessFlag.opNone:
					str = "";
					break;
				case GEDCOMOrdinanceProcessFlag.opYes:
					str = "yes";
					break;
				case GEDCOMOrdinanceProcessFlag.opNo:
					str = "no";
					break;
			}
			return str;
		}

		public static string GetPriorityStr(GKResearchPriority value)
		{
			string str = "";
			switch (value) {
				case GKResearchPriority.rpNone:
					str = "";
					break;
				case GKResearchPriority.rpLow:
					str = "low";
					break;
				case GKResearchPriority.rpNormal:
					str = "normal";
					break;
				case GKResearchPriority.rpHigh:
					str = "high";
					break;
				case GKResearchPriority.rpTop:
					str = "top";
					break;
			}
			return str;
		}

		public static GKResearchPriority GetPriorityVal(string str)
		{
			GKResearchPriority result;
			string SU = str.Trim().ToLowerInvariant();

			if (SU == "low")
			{
				result = GKResearchPriority.rpLow;
			}
			else if (SU == "normal")
			{
				result = GKResearchPriority.rpNormal;
			}
			else if (SU == "high")
			{
				result = GKResearchPriority.rpHigh;
			}
			else if (SU == "top")
			{
				result = GKResearchPriority.rpTop;
			}
			else
			{
				result = GKResearchPriority.rpNone;
			}
			
			return result;
		}

		public static string GetCharacterSetStr(GEDCOMCharacterSet value)
		{
			string str = "";
			switch (value) {
				case GEDCOMCharacterSet.csASCII:
					str = "ASCII";
					break;
				case GEDCOMCharacterSet.csANSEL:
					str = "ANSEL";
					break;
				case GEDCOMCharacterSet.csUNICODE:
					str = "UNICODE";
					break;
				case GEDCOMCharacterSet.csUTF8:
					str = "UTF-8";
					break;
			}
			return str;
		}

		public static GEDCOMCharacterSet GetCharacterSetVal(string str)
		{
			GEDCOMCharacterSet result;
			string SU = str.ToUpperInvariant();

			if (SU == "ASCII" || SU == "ANSI" || SU == "IBMPC")
			{
				result = GEDCOMCharacterSet.csASCII;
			}
			else if (SU == "ANSEL")
			{
				result = GEDCOMCharacterSet.csANSEL;
			}
			else if (SU == "UNICODE")
			{
				result = GEDCOMCharacterSet.csUNICODE;
			}
			else if (SU == "UTF8" || SU == "UTF-8")
			{
				result = GEDCOMCharacterSet.csUTF8;
			}
			else
			{
				result = GEDCOMCharacterSet.csASCII;
			}
			
			return result;
		}

		public static GEDCOMSex GetSexVal(string str)
		{
			GEDCOMSex result;
			string SU = str.Trim().ToUpperInvariant();

			switch (SU) {
				case "M":
					result = GEDCOMSex.svMale;
					break;
				case "F":
					result = GEDCOMSex.svFemale;
					break;
				case "U":
					result = GEDCOMSex.svUndetermined;
					break;
				default:
					result = GEDCOMSex.svNone;
					break;
			}
			
			return result;
		}

		public static string GetSexStr(GEDCOMSex value)
		{
			string str;
			
			switch (value) {
				case GEDCOMSex.svMale:
					str = "M";
					break;
				case GEDCOMSex.svFemale:
					str = "F";
					break;
				case GEDCOMSex.svUndetermined:
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
				if (pm != "") result = result + GEDCOMCustomDate.GEDCOMMonthArray[SysUtils.ParseInt(pm, 1) - 1] + " ";
				if (py != "") result += py;
			}
			return result;
		}

		#endregion
	}
}
