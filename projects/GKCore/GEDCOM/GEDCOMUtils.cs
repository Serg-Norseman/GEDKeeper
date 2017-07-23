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

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public class GEDCOMEnumHelper<T> where T : struct, IComparable, IFormattable, IConvertible
    {
        private readonly string[] fStrValues;
        private readonly Dictionary<string, int> fValues;
        private readonly T fDefaultValue;
        private readonly bool fCaseSensitive;

        protected GEDCOMEnumHelper(string[] strValues, T defaultValue, bool caseSensitive = false)
        {
            Type enumType = typeof(T);
            if (!enumType.IsEnum)
                throw new ArgumentException(string.Format("{0} is not of type Enum", enumType.Name));

            T[] enums = (T[]) Enum.GetValues(enumType);

            if (enums.Length != strValues.Length) {
                throw new ArgumentException("Arguments are not compatible");
            }

            fStrValues = strValues;
            fDefaultValue = defaultValue;
            fCaseSensitive = caseSensitive;

            fValues = new Dictionary<string, int>(enums.Length);
            for (int i = 0; i < enums.Length; i++)
            {
                fValues.Add(strValues[i], i);
            }
        }

        public string GetStrValue(T enumVal)
        {
            int idx = (int)((IConvertible)enumVal);

            if (idx < 0 || idx >= fStrValues.Length) {
                return string.Empty;
            } else {
                return fStrValues[idx];
            }
        }

        public T GetEnumValue(string key)
        {
            if (!fCaseSensitive) {
                key = key.Trim().ToLower(CultureInfo.InvariantCulture);
            }

            int result;
            if (fValues.TryGetValue(key, out result)) {
                return (T)((IConvertible)result);
            } else {
                return fDefaultValue;
            }
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public static class GEDCOMUtils
    {
        #region Parse functions

        public static string TrimLeft(string str)
        {
            if (string.IsNullOrEmpty(str)) return "";

            int len = str.Length;
            int i = 1;
            while (i <= len && str[i - 1] <= ' ') i++;

            string result;
            if (i > len) {
                result = "";
            } else {
                result = ((i != 1) ? str.Substring(i - 1) : str);
            }
            return result;
        }

        public static string TrimRight(string str)
        {
            if (string.IsNullOrEmpty(str)) return "";

            int len = str.Length;
            int i = len;
            while (i > 0 && str[i - 1] <= ' ') i--;

            string result = ((i != len) ? str.Substring(0, i) : str);
            return result;
        }

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

        public static string CleanXRef(string xref)
        {
            string result = xref;

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

        public static string EncloseXRef(string xref)
        {
            if (!string.IsNullOrEmpty(xref)) {
                if (xref[0] != '@') {
                    xref = "@" + xref;
                }

                if (xref[xref.Length - 1] != '@') {
                    xref += "@";
                }
            }
            return xref;
        }

        #endregion

        #region GEDCOM Enums processing

        public static Encoding GetEncodingByCharacterSet(GEDCOMCharacterSet cs)
        {
            Encoding res = Encoding.Default;

            switch (cs) {
                case GEDCOMCharacterSet.csANSEL:
                    // [16/03/2016] not supported
                    //res = new AnselEncoding();
                    //break;

                case GEDCOMCharacterSet.csASCII:
                    res = Encoding.GetEncoding(1251);
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    res = Encoding.Unicode;
                    break;

                case GEDCOMCharacterSet.csUTF8:
                    res = Encoding.UTF8;
                    break;
            }

            return res;
        }

        public static GEDCOMRestriction GetRestrictionVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMRestriction.rnNone;

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
            if (string.IsNullOrEmpty(str)) return GEDCOMPedigreeLinkageType.plNone;

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
            if (string.IsNullOrEmpty(str)) return GEDCOMChildLinkageStatus.clNone;

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
            if (string.IsNullOrEmpty(str)) return GKCommunicationType.ctVisit;

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
            if (string.IsNullOrEmpty(str)) return GEDCOMMultimediaFormat.mfNone;

            GEDCOMMultimediaFormat result;
            str = str.Trim().ToLowerInvariant();
            
            if (str == "bmp")
            {
                result = GEDCOMMultimediaFormat.mfBMP;
            }
            else if (str == "gif")
            {
                result = GEDCOMMultimediaFormat.mfGIF;
            }
            else if (str == "jpg" || str == "jpeg")
            {
                result = GEDCOMMultimediaFormat.mfJPG;
            }
            else if (str == "ole")
            {
                result = GEDCOMMultimediaFormat.mfOLE;
            }
            else if (str == "pcx")
            {
                result = GEDCOMMultimediaFormat.mfPCX;
            }
            else if (str == "tif" || str == "tiff")
            {
                result = GEDCOMMultimediaFormat.mfTIF;
            }
            else if (str == "wav")
            {
                result = GEDCOMMultimediaFormat.mfWAV;
            }
            else if (str == "txt")
            {
                result = GEDCOMMultimediaFormat.mfTXT;
            }
            else if (str == "rtf")
            {
                result = GEDCOMMultimediaFormat.mfRTF;
            }
            else if (str == "avi")
            {
                result = GEDCOMMultimediaFormat.mfAVI;
            }
            else if (str == "tga")
            {
                result = GEDCOMMultimediaFormat.mfTGA;
            }
            else if (str == "png")
            {
                result = GEDCOMMultimediaFormat.mfPNG;
            }
            else if (str == "mpg" || str == "mpeg")
            {
                result = GEDCOMMultimediaFormat.mfMPG;
            }
            else if (str == "htm" || str == "html")
            {
                result = GEDCOMMultimediaFormat.mfHTM;
            }
            else if (str == "raw")
            {
                result = GEDCOMMultimediaFormat.mfRAW;
            }
            else if (str == "mp3")
            {
                result = GEDCOMMultimediaFormat.mfMP3;
            }
            else if (str == "wma")
            {
                result = GEDCOMMultimediaFormat.mfWMA;
            }
            else if (str == "psd")
            {
                result = GEDCOMMultimediaFormat.mfPSD;
            }
            else if (str == "pdf")
            {
                result = GEDCOMMultimediaFormat.mfPDF;
            }
            else if (str == "mp4")
            {
                result = GEDCOMMultimediaFormat.mfMP4;
            }
            else if (str == "ogv")
            {
                result = GEDCOMMultimediaFormat.mfOGV;
            }
            else if (str == "mka")
            {
                result = GEDCOMMultimediaFormat.mfMKA;
            }
            else if (str == "wmv")
            {
                result = GEDCOMMultimediaFormat.mfWMV;
            }
            else if (str == "mkv")
            {
                result = GEDCOMMultimediaFormat.mfMKV;
            }
            else if (str == "mov")
            {
                result = GEDCOMMultimediaFormat.mfMOV;
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
                case GEDCOMMultimediaFormat.mfPSD:
                    s = "psd";
                    break;
                case GEDCOMMultimediaFormat.mfPDF:
                    s = "pdf";
                    break;
                case GEDCOMMultimediaFormat.mfMP4:
                    s = "mp4";
                    break;
                case GEDCOMMultimediaFormat.mfOGV:
                    s = "ogv";
                    break;
                case GEDCOMMultimediaFormat.mfMKA:
                    s = "mka";
                    break;
                case GEDCOMMultimediaFormat.mfWMV:
                    s = "wmv";
                    break;
                case GEDCOMMultimediaFormat.mfMKV:
                    s = "mkv";
                    break;
                case GEDCOMMultimediaFormat.mfMOV:
                    s = "mov";
                    break;
                default:
                    s = "";
                    break;
            }
            return s;
        }

        public static GEDCOMMediaType GetMediaTypeVal(string str)
        {
            GEDCOMMediaType result = GEDCOMMediaType.mtUnknown;
            if (string.IsNullOrEmpty(str)) return result;

            str = str.Trim().ToLower(CultureInfo.InvariantCulture);

            if (string.Equals(str, "audio"))
            {
                result = GEDCOMMediaType.mtAudio;
            }
            else if (string.Equals(str, "book"))
            {
                result = GEDCOMMediaType.mtBook;
            }
            else if (string.Equals(str, "card"))
            {
                result = GEDCOMMediaType.mtCard;
            }
            else if (string.Equals(str, "electronic"))
            {
                result = GEDCOMMediaType.mtElectronic;
            }
            else if (string.Equals(str, "fiche"))
            {
                result = GEDCOMMediaType.mtFiche;
            }
            else if (string.Equals(str, "film"))
            {
                result = GEDCOMMediaType.mtFilm;
            }
            else if (string.Equals(str, "magazine"))
            {
                result = GEDCOMMediaType.mtMagazine;
            }
            else if (string.Equals(str, "manuscript"))
            {
                result = GEDCOMMediaType.mtManuscript;
            }
            else if (string.Equals(str, "map"))
            {
                result = GEDCOMMediaType.mtMap;
            }
            else if (string.Equals(str, "newspaper"))
            {
                result = GEDCOMMediaType.mtNewspaper;
            }
            else if (string.Equals(str, "photo"))
            {
                result = GEDCOMMediaType.mtPhoto;
            }
            else if (string.Equals(str, "tombstone"))
            {
                result = GEDCOMMediaType.mtTombstone;
            }
            else if (string.Equals(str, "video"))
            {
                result = GEDCOMMediaType.mtVideo;
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
            if (string.IsNullOrEmpty(str)) return GEDCOMNameType.ntNone;

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
            if (string.IsNullOrEmpty(str)) return GKResearchStatus.rsDefined;

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
            if (string.IsNullOrEmpty(str)) return GEDCOMSpouseSealingDateStatus.sdsNone;

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

        public static GEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlagVal(string su)
        {
            if (string.IsNullOrEmpty(su)) return GEDCOMOrdinanceProcessFlag.opNone;

            GEDCOMOrdinanceProcessFlag result;
            su = su.Trim().ToUpperInvariant(); // FIXME
            
            if (su == "YES")
            {
                result = GEDCOMOrdinanceProcessFlag.opYes;
            }
            else if (su == "NO")
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
            if (string.IsNullOrEmpty(str)) return GKResearchPriority.rpNone;

            string su = str.Trim().ToLowerInvariant();
            GKResearchPriority result;

            if (su == "low")
            {
                result = GKResearchPriority.rpLow;
            }
            else if (su == "normal")
            {
                result = GKResearchPriority.rpNormal;
            }
            else if (su == "high")
            {
                result = GKResearchPriority.rpHigh;
            }
            else if (su == "top")
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
            if (string.IsNullOrEmpty(str)) return GEDCOMCharacterSet.csASCII;

            string su = str.ToUpperInvariant();
            GEDCOMCharacterSet result;

            if (su == "ASCII" || su == "ANSI" || su == "IBMPC")
            {
                result = GEDCOMCharacterSet.csASCII;
            }
            else if (su == "ANSEL")
            {
                result = GEDCOMCharacterSet.csANSEL;
            }
            else if (su == "UNICODE")
            {
                result = GEDCOMCharacterSet.csUNICODE;
            }
            else if (su == "UTF8" || su == "UTF-8")
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
            if (string.IsNullOrEmpty(str)) return GEDCOMSex.svNone;

            string su = str.Trim().ToUpperInvariant();
            GEDCOMSex result;

            switch (su) {
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


        public static GEDCOMBaptismDateStatus GetBaptismDateStatusVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMBaptismDateStatus.bdsNone;

            string su = str.Trim().ToUpperInvariant();
            GEDCOMBaptismDateStatus result;

            if (su == "CHILD")
            {
                result = GEDCOMBaptismDateStatus.bdsChild;
            }
            else if (su == "COMPLETED")
            {
                result = GEDCOMBaptismDateStatus.bdsCompleted;
            }
            else if (su == "EXCLUDED")
            {
                result = GEDCOMBaptismDateStatus.bdsExcluded;
            }
            else if (su == "PRE-1970")
            {
                result = GEDCOMBaptismDateStatus.bdsPre1970;
            }
            else if (su == "STILLBORN")
            {
                result = GEDCOMBaptismDateStatus.bdsStillborn;
            }
            else if (su == "SUBMITTED")
            {
                result = GEDCOMBaptismDateStatus.bdsSubmitted;
            }
            else if (su == "UNCLEARED")
            {
                result = GEDCOMBaptismDateStatus.bdsUncleared;
            }
            else
            {
                result = GEDCOMBaptismDateStatus.bdsNone;
            }
            return result;
        }

        public static string GetBaptismDateStatusStr(GEDCOMBaptismDateStatus value)
        {
            string str = "";
            switch (value)
            {
                case GEDCOMBaptismDateStatus.bdsChild:
                    str = "CHILD";
                    break;

                case GEDCOMBaptismDateStatus.bdsCompleted:
                    str = "COMPLETED";
                    break;

                case GEDCOMBaptismDateStatus.bdsExcluded:
                    str = "EXCLUDED";
                    break;

                case GEDCOMBaptismDateStatus.bdsPre1970:
                    str = "PRE-1970";
                    break;

                case GEDCOMBaptismDateStatus.bdsStillborn:
                    str = "STILLBORN";
                    break;

                case GEDCOMBaptismDateStatus.bdsSubmitted:
                    str = "SUBMITTED";
                    break;

                case GEDCOMBaptismDateStatus.bdsUncleared:
                    str = "UNCLEARED";
                    break;
            }

            return str;
        }

        public static GEDCOMEndowmentDateStatus GetEndowmentDateStatusVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMEndowmentDateStatus.edsNone;

            string su = str.Trim().ToUpperInvariant();
            GEDCOMEndowmentDateStatus result;

            if (su == "CHILD")
            {
                result = GEDCOMEndowmentDateStatus.edsChild;
            }
            else if (su == "COMPLETED")
            {
                result = GEDCOMEndowmentDateStatus.edsCompleted;
            }
            else if (su == "EXCLUDED")
            {
                result = GEDCOMEndowmentDateStatus.edsExcluded;
            }
            else if (su == "INFANT")
            {
                result = GEDCOMEndowmentDateStatus.edsInfant;
            }
            else if (su == "PRE-1970")
            {
                result = GEDCOMEndowmentDateStatus.edsPre1970;
            }
            else if (su == "STILLBORN")
            {
                result = GEDCOMEndowmentDateStatus.edsStillborn;
            }
            else if (su == "SUBMITTED")
            {
                result = GEDCOMEndowmentDateStatus.edsSubmitted;
            }
            else if (su == "UNCLEARED")
            {
                result = GEDCOMEndowmentDateStatus.edsUncleared;
            }
            else
            {
                result = GEDCOMEndowmentDateStatus.edsNone;
            }
            return result;
        }

        public static string GetEndowmentDateStatusStr(GEDCOMEndowmentDateStatus value)
        {
            string str = "";
            
            switch (value)
            {
                case GEDCOMEndowmentDateStatus.edsChild:
                    str = "CHILD";
                    break;

                case GEDCOMEndowmentDateStatus.edsCompleted:
                    str = "COMPLETED";
                    break;

                case GEDCOMEndowmentDateStatus.edsExcluded:
                    str = "EXCLUDED";
                    break;

                case GEDCOMEndowmentDateStatus.edsInfant:
                    str = "INFANT";
                    break;

                case GEDCOMEndowmentDateStatus.edsPre1970:
                    str = "PRE-1970";
                    break;

                case GEDCOMEndowmentDateStatus.edsStillborn:
                    str = "STILLBORN";
                    break;

                case GEDCOMEndowmentDateStatus.edsSubmitted:
                    str = "SUBMITTED";
                    break;

                case GEDCOMEndowmentDateStatus.edsUncleared:
                    str = "UNCLEARED";
                    break;
            }

            return str;
        }

        public static GEDCOMChildSealingDateStatus GetChildSealingDateStatusVal(string str)
        {
            if (string.IsNullOrEmpty(str)) return GEDCOMChildSealingDateStatus.cdsNone;

            string su = str.Trim().ToUpperInvariant();
            GEDCOMChildSealingDateStatus result;

            if (su == "BIC")
            {
                result = GEDCOMChildSealingDateStatus.cdsBIC;
            }
            else if (su == "EXCLUDED")
            {
                result = GEDCOMChildSealingDateStatus.cdsExcluded;
            }
            else if (su == "PRE-1970")
            {
                result = GEDCOMChildSealingDateStatus.cdsPre1970;
            }
            else if (su == "STILLBORN")
            {
                result = GEDCOMChildSealingDateStatus.cdsStillborn;
            }
            else if (su == "SUBMITTED")
            {
                result = GEDCOMChildSealingDateStatus.cdsSubmitted;
            }
            else if (su == "UNCLEARED")
            {
                result = GEDCOMChildSealingDateStatus.cdsUncleared;
            }
            else
            {
                result = GEDCOMChildSealingDateStatus.cdsNone;
            }

            return result;
        }

        public static string GetChildSealingDateStatusStr(GEDCOMChildSealingDateStatus value)
        {
            string str = "";

            switch (value)
            {
                case GEDCOMChildSealingDateStatus.cdsBIC:
                    str = "BIC";
                    break;

                case GEDCOMChildSealingDateStatus.cdsExcluded:
                    str = "EXCLUDED";
                    break;

                case GEDCOMChildSealingDateStatus.cdsPre1970:
                    str = "PRE-1970";
                    break;

                case GEDCOMChildSealingDateStatus.cdsStillborn:
                    str = "STILLBORN";
                    break;

                case GEDCOMChildSealingDateStatus.cdsSubmitted:
                    str = "SUBMITTED";
                    break;

                case GEDCOMChildSealingDateStatus.cdsUncleared:
                    str = "UNCLEARED";
                    break;
            }

            return str;
        }
        
        #endregion
    }
}
