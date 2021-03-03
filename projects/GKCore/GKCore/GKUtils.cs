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
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Cultures;
using GKCore.Import;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using Ude;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public static class GKUtils
    {
        #region Aux functions

        public static bool ImplementsInterface(Type type, Type ifaceType)
        {
            Type[] intf = type.GetInterfaces();
            for (int i = 0; i < intf.Length; i++) {
                if (intf[i] == ifaceType) {
                    return true;
                }
            }
            return false;
        }

        public static void LoadExtFile(string fileName, string args = "")
        {
            #if !CI_MODE
            if (File.Exists(fileName)) {
                Process.Start(new ProcessStartInfo("file://"+fileName) { UseShellExecute = true, Arguments = args });
            } else {
                Process.Start(fileName);
            }
            #endif
        }

        public static string SexChar(GDMSex sex)
        {
            string ss = SexStr(sex);
            return string.IsNullOrEmpty(ss) ? "?" : new string(ss[0], 1);
        }

        public static string SexStr(GDMSex sex)
        {
            return LangMan.LS(GKData.SexData[(int)sex].NameId);
        }

        public static GDMSex GetSexBySign(char sexSign)
        {
            GDMSex result = GDMSex.svUnknown;
            switch (sexSign) {
                case 'F':
                    result = GDMSex.svFemale;
                    break;
                case 'M':
                    result = GDMSex.svMale;
                    break;
                case 'U':
                    result = GDMSex.svUnknown;
                    break;
                case 'X':
                    result = GDMSex.svIntersex;
                    break;
            }
            return result;
        }

        public static string MergeStrings(GDMLines strings)
        {
            if (strings == null)
                throw new ArgumentNullException("strings");

            StringBuilder result = new StringBuilder();

            int num = strings.Count;
            for (int i = 0; i < num; i++) {
                if (result.Length != 0) result.Append(" ");
                result.Append(strings[i].Trim());
            }

            return result.ToString();
        }

        public static string TruncateStrings(GDMLines value, int size)
        {
            string s = string.Empty;

            if (value != null && value.Count != 0) {
                if (size < value[0].Length) {
                    s = value[0].Substring(0, size) + "...";
                } else {
                    s = value[0];
                }
            }

            return s;
        }

        public static StringList GetLocationLinks(GDMTree tree, GDMLocationRecord locRec)
        {
            var linksList = new StringList();

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecordWithEvents evsRec = tree[i] as GDMRecordWithEvents;

                if (evsRec != null) {
                    int num2 = evsRec.Events.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMCustomEvent evt = evsRec.Events[j];

                        if (evt.Place.Location.Value == locRec) {
                            linksList.AddObject(GetRecordName(evsRec, true) + ", " + GetEventName(evt).ToLower(), evsRec);
                        }
                    }
                }
            }

            return linksList;
        }

        public static string HyperLink(string xref, string text, int num)
        {
            string result = "";

            if (!string.IsNullOrEmpty(xref) && string.IsNullOrEmpty(text)) {
                text = "???";
            }

            if (!string.IsNullOrEmpty(xref) && !string.IsNullOrEmpty(text)) {
                result = "[url=" + xref + "]" + text + "[/url]";
            }

            return result;
        }

        public static string GetRecordName(GDMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                string sign = "";

                if (signed) {
                    GDMRecordType recordType = record.RecordType;
                    if (recordType != GDMRecordType.rtIndividual) {
                        if (recordType == GDMRecordType.rtFamily || (byte)recordType - (byte)GDMRecordType.rtMultimedia < (byte)GDMRecordType.rtResearch)
                        {
                            sign = LangMan.LS(GKData.RecordTypes[(int)record.RecordType]) + ": ";
                        }
                    } else {
                        sign = "";
                    }
                }

                string st;
                switch (record.RecordType) {
                    case GDMRecordType.rtIndividual:
                        st = GetNameString(((GDMIndividualRecord)record), true, false);
                        break;
                    case GDMRecordType.rtFamily:
                        st = GetFamilyString((GDMFamilyRecord)record);
                        break;
                    case GDMRecordType.rtNote:
                        st = ((GDMNoteRecord)record).Lines[0]; // TODO: bad solution?!
                        break;
                    case GDMRecordType.rtMultimedia:
                        st = ((GDMMultimediaRecord)record).FileReferences[0].Title;
                        break;
                    case GDMRecordType.rtSource:
                        st = ((GDMSourceRecord)record).ShortTitle;
                        break;
                    case GDMRecordType.rtRepository:
                        st = ((GDMRepositoryRecord)record).RepositoryName;
                        break;
                    case GDMRecordType.rtGroup:
                        st = ((GDMGroupRecord)record).GroupName;
                        break;
                    case GDMRecordType.rtResearch:
                        st = ((GDMResearchRecord)record).ResearchName;
                        break;
                    case GDMRecordType.rtTask:
                        st = GetTaskGoalStr((GDMTaskRecord)record);
                        break;
                    case GDMRecordType.rtCommunication:
                        st = ((GDMCommunicationRecord)record).CommName;
                        break;
                    case GDMRecordType.rtLocation:
                        st = ((GDMLocationRecord)record).LocationName;
                        break;
                    default:
                        st = record.XRef;
                        break;
                }

                result = sign + st;
            }

            return result;
        }

        public static string GenRecordLink(GDMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                result = HyperLink(record.XRef, GetRecordName(record, signed), 0);
            }

            return result;
        }

        public static Tuple<string, string> GenRecordLinkTuple(GDMRecord record, bool signed)
        {
            if (record != null) {
                string recName = GetRecordName(record, signed);
                string recLink = HyperLink(record.XRef, recName, 0);
                return new Tuple<string, string>(recName, recLink);
            } else {
                return new Tuple<string, string>(string.Empty, string.Empty);
            }
        }

        public static string GetCorresponderStr(GDMTree tree, GDMCommunicationRecord commRec, bool aLink)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (commRec == null)
                throw new ArgumentNullException("commRec");

            string result = "";
            var corr = commRec.Corresponder.Individual;

            if (corr != null) {
                string nm = GetNameString(corr, true, false);
                if (aLink) {
                    nm = HyperLink(corr.XRef, nm, 0);
                }
                result = "[ " + LangMan.LS(GKData.CommunicationDirs[(int)commRec.CommDirection]) + " ] " + nm;
            }
            return result;
        }

        public static string GetTaskGoalStr(GDMTaskRecord taskRec)
        {
            if (taskRec == null) return string.Empty;
            
            string result = "";
            
            var goal = taskRec.GetTaskGoal();

            switch (goal.GoalType) {
                case GDMGoalType.gtIndividual:
                case GDMGoalType.gtFamily:
                case GDMGoalType.gtSource:
                    result = GetGoalStr(goal.GoalType, goal.GoalRec);
                    break;

                case GDMGoalType.gtOther:
                    result = taskRec.Goal;
                    break;
            }

            if (goal.GoalType != GDMGoalType.gtOther)
            {
                result = "[" + LangMan.LS(GKData.GoalNames[(int)goal.GoalType]) + "] " + result;
            }

            return result;
        }

        public static string GetGoalStr(GDMGoalType gt, GDMRecord tempRec)
        {
            if (tempRec == null) return string.Empty;

            switch (gt)
            {
                case GDMGoalType.gtIndividual:
                    return GetNameString(((GDMIndividualRecord)tempRec), true, false);

                case GDMGoalType.gtFamily:
                    return GetFamilyString(tempRec as GDMFamilyRecord);

                case GDMGoalType.gtSource:
                    return ((GDMSourceRecord)tempRec).ShortTitle;
            }

            return string.Empty;
        }

        #endregion

        #region Encoding

        public static string GetRectUID(int x1, int y1, int x2, int y2)
        {
            byte[] bx1 = BitConverter.GetBytes((ushort)x1);
            byte[] by1 = BitConverter.GetBytes((ushort)y1);
            byte[] bx2 = BitConverter.GetBytes((ushort)x2);
            byte[] by2 = BitConverter.GetBytes((ushort)y2);

            byte[] buffer = new byte[8];
            Buffer.BlockCopy(bx1, 0, buffer, 0, 2);
            Buffer.BlockCopy(by1, 0, buffer, 2, 2);
            Buffer.BlockCopy(bx2, 0, buffer, 4, 2);
            Buffer.BlockCopy(by2, 0, buffer, 6, 2);

            return GEDCOMUtils.EncodeUID(buffer);
        }

        public static CharsetResult DetectCharset(Stream stream, int bufferSize = 32768)
        {
            var result = new CharsetResult();

            ICharsetDetector cdet = new CharsetDetector();
            byte[] buffer = new byte[bufferSize];
            int read = stream.Read(buffer, 0, buffer.Length);
            if (read > 0) {
                cdet.Feed(buffer, 0, read);
                cdet.DataEnd();
                stream.Seek(0, SeekOrigin.Begin);

                result.Charset = cdet.Charset;
                result.Confidence = cdet.Confidence;
            } else {
                result.Charset = null;
                result.Confidence = 0.0f;
            }

            return result;
        }

        public static StreamReader GetDetectedStreamReader(Stream stream)
        {
            // TODO: total search and fix references to Encoding.GetEncoding(1251)
            // TODO: implement detection of encoding
            StreamReader reader = new StreamReader(stream, Encoding.GetEncoding(1251));
            return reader;
        }

        #endregion

        #region Match functions

        public static string PrepareMask(string mask)
        {
            string regexStr = "";

            if (!string.IsNullOrEmpty(mask)) {
                // double star evokes monstrous lags
                mask = mask.Replace("**", "*").Replace("??", "?");

                int curPos = 0;
                int len = mask.Length;

                char[] syms = "*?|".ToCharArray();

                while (curPos < len) {
                    int I = mask.IndexOfAny(syms, curPos);
                    if (I < curPos) break;
                    if (I > curPos) {
                        string part = mask.Substring(curPos, I - curPos);
                        regexStr += Regex.Escape(part);
                    }

                    switch (mask[I]) {
                        case '*':
                            regexStr += ".*";
                            break;
                        case '?':
                            regexStr += ".";
                            break;
                        case '|':
                            regexStr += "|";
                            break;
                    }

                    curPos = I + 1;
                }

                if (curPos < len) {
                    string part = mask.Substring(curPos, len - curPos);
                    regexStr += Regex.Escape(part);
                }
            }

            return regexStr;
        }

        public const RegexOptions RegexOpts = RegexOptions.Compiled | RegexOptions.IgnoreCase;

        public static Regex InitMaskRegex(string mask)
        {
            Regex result = null;

            string regexStr = PrepareMask(mask);
            if (!string.IsNullOrEmpty(regexStr)) {
                result = new Regex(regexStr, RegexOpts);
            }

            return result;
        }

        public static bool MatchesRegex(string str, Regex regex)
        {
            return (regex != null) && regex.IsMatch(str, 0);
        }

        public static bool MatchesMask(string str, string mask)
        {
            if (string.IsNullOrEmpty(str) || string.IsNullOrEmpty(mask)) {
                return false;
            }

            if (mask == "*") {
                return true;
            }

            // Regex.IsMatch() has caching
            return Regex.IsMatch(str, PrepareMask(mask), RegexOpts);
        }

        #endregion

        #region Event Utils

        public static string GetAttributeValue(GDMIndividualRecord iRec, string attrName)
        {
            if (iRec == null) return string.Empty;

            GDMCustomEvent attr = iRec.FindEvent(attrName);
            string result = ((attr == null) ? "" : attr.StringValue);
            return result;
        }

        public static PersonEventKind GetPersonEventKindBySign(string sign)
        {
            int idx = GetPersonEventIndex(sign);
            return (idx < 0) ? PersonEventKind.ekFact : GKData.PersonEvents[idx].Kind;
        }

        public static int GetPersonEventIndex(string sign)
        {
            int res = -1;

            for (int i = 0; i < GKData.PersonEvents.Length; i++)
            {
                if (GKData.PersonEvents[i].Sign == sign)
                {
                    res = i;
                    break;
                }
            }

            return res;
        }

        public static int GetFamilyEventIndex(string sign)
        {
            int res = -1;

            for (int i = 0; i < GKData.FamilyEvents.Length; i++)
            {
                if (GKData.FamilyEvents[i].Sign == sign)
                {
                    res = i;
                    break;
                }
            }

            return res;
        }

        public static string GetEventName(GDMCustomEvent evt)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            string result = "";

            var evtName = evt.GetTagName();
            if (evt is GDMIndividualEvent || evt is GDMIndividualAttribute) {
                int ev = GetPersonEventIndex(evtName);
                if (ev == 0) {
                    result = !string.IsNullOrEmpty(evt.Classification) ? evt.Classification : LangMan.LS(GKData.PersonEvents[ev].Name);
                } else {
                    result = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evtName;
                }
            } else if (evt is GDMFamilyEvent) {
                int ev = GetFamilyEventIndex(evtName);
                if (ev == 0) {
                    result = evt.Classification;
                } else {
                    result = (ev > 0) ? LangMan.LS(GKData.FamilyEvents[ev].Name) : evtName;
                }
            }

            return result;
        }

        public static string GetAttributeStr(GDMIndividualAttribute iAttr)
        {
            if (iAttr == null)
                throw new ArgumentNullException("iAttr");

            var attrName = iAttr.GetTagName();
            int idx = GetPersonEventIndex(attrName);
            string st;
            if (idx == 0) {
                st = iAttr.Classification;
            } else {
                st = (idx > 0) ? LangMan.LS(GKData.PersonEvents[idx].Name) : attrName;
            }

            string place = iAttr.Place.StringValue;
            if (place != "") {
                place = " [" + place + "]";
            }
            return st + ": " + iAttr.StringValue + place;
        }

        public static string GetEventDesc(GDMCustomEvent evt, bool hyperLink = true)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            string dt = GEDCOMEventToDateStr(evt, GlobalOptions.Instance.DefDateFormat, false);
            string place = evt.Place.StringValue;
            GDMLocationRecord location = evt.Place.Location.Value as GDMLocationRecord;

            if (place != "" && location != null && hyperLink) {
                place = HyperLink(location.XRef, place, 0);
            }

            string result;

            if (dt == "" && place == "") {
                result = "?";
            } else {
                if (dt == "") {
                    result = place;
                } else {
                    if (place == "") {
                        result = dt;
                    } else {
                        result = dt + ", " + place;
                    }
                }
            }

            return result;
        }

        public static string GetEventCause(GDMCustomEvent evt)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            string result = evt.Cause;

            if (!string.IsNullOrEmpty(evt.Agency)) {
                if (result != "") {
                    result += " ";
                }
                result = result + "[" + evt.Agency + "]";
            }

            return result;
        }

        #endregion

        #region Date functions

        /// <summary>
        /// The result of the function is a "normalized date", delimited by '.' and fixed order of parts: "dd.mm.yyyy". 
        /// The pattern and regional date contain the delimiter '/'. 
        /// The pattern defines the position of the parts in a regional date format.
        /// </summary>
        /// <param name="regionalDate">date similar "01/20/1970"</param>
        /// <param name="pattern">pattern similar "mm/dd/yyyy"</param>
        /// <returns>normalized date as "dd.mm.yyyy"</returns>
        public static string GetNormalizeDate(string regionalDate, string pattern)
        {
            if (string.IsNullOrEmpty(regionalDate)) return string.Empty;

            string[] regionalParts = regionalDate.Split('/');
            string[] patternParts = pattern.Split('/');
            string[] resultParts = new string[3];

            for (int i = 0; i < patternParts.Length; i++) {
                string part = patternParts[i];
                switch (part[0]) {
                    case 'd':
                        resultParts[0] = regionalParts[i];
                        break;

                    case 'm':
                        resultParts[1] = regionalParts[i];
                        break;

                    case 'y':
                        resultParts[2] = regionalParts[i];
                        break;
                }
            }

            string result = string.Join(".", resultParts);
            return result;
        }

        /// <summary>
        /// The result of the function is a "regional date", delimited by '/' and regional order of parts: "mm/dd/yyyy" 
        /// or any other. The pattern and regional date contain the delimiter '/'. 
        /// The pattern defines the position of the parts in a regional date format.
        /// </summary>
        /// <param name="normalizeDate">date with format "dd.mm.yyyy"</param>
        /// <param name="pattern">pattern similar "mm/dd/yyyy"</param>
        /// <returns>regional date as "mm/dd/yyyy"</returns>
        public static string GetRegionalDate(string normalizeDate, string pattern)
        {
            if (string.IsNullOrEmpty(normalizeDate)) return string.Empty;

            string[] normalizeParts = normalizeDate.Split('.');
            string[] patternParts = pattern.Split('/');
            string[] resultParts = new string[3];

            for (int i = 0; i < patternParts.Length; i++) {
                string part = patternParts[i];
                switch (part[0]) {
                    case 'd':
                        resultParts[i] = normalizeParts[0];
                        break;

                    case 'm':
                        resultParts[i] = normalizeParts[1];
                        break;

                    case 'y':
                        resultParts[i] = normalizeParts[2];
                        break;
                }
            }

            string result = string.Join("/", resultParts);
            return result;
        }

        public static string GEDCOMEventToDateStr(GDMCustomEvent evt, DateFormat format, bool sign)
        {
            return (evt == null) ? string.Empty : evt.Date.GetDisplayStringExt(format, sign, false);
        }

        public static string CompactDate(string date)
        {
            string result = date;
            while (result.IndexOf("__.") == 0) result = result.Remove(0, 3);
            return result;
        }

        public static GDMCustomDate GetBirthDate(GDMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            GDMCustomDate result = ((evt == null) ? null : evt.Date.Value);
            return result;
        }

        public static string GetBirthDate(GDMIndividualRecord iRec, DateFormat dateFormat, bool compact)
        {
            if (iRec == null) return string.Empty;

            GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            string result = ((evt == null) ? "" : GEDCOMEventToDateStr(evt, dateFormat, false));
            if (compact) result = CompactDate(result);
            return result;
        }

        public static string GetDeathDate(GDMIndividualRecord iRec, DateFormat dateFormat, bool compact)
        {
            if (iRec == null) return string.Empty;

            GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.DEAT);
            string result = ((evt == null) ? "" : GEDCOMEventToDateStr(evt, dateFormat, false));
            if (compact) result = CompactDate(result);
            return result;
        }

        public static string GetLifeStr(GDMIndividualRecord iRec)
        {
            if (iRec == null) return string.Empty;

            string result = " (";

            string ds = GetBirthDate(iRec, GlobalOptions.Instance.DefDateFormat, false);
            if (ds == "")
            {
                ds = "?";
            }
            result += ds;

            ds = GetDeathDate(iRec, GlobalOptions.Instance.DefDateFormat, false);
            if (ds == "")
            {
                GDMCustomEvent ev = iRec.FindEvent(GEDCOMTagType.DEAT);
                if (ev != null)
                {
                    ds = "?";
                }
            }

            if (ds != "")
            {
                result = result + " - " + ds;
            }

            result += ")";
            return result;
        }

        public static void GetEventDatePlace(GDMIndividualRecord iRec, string eventName, DateFormat dateFormat,
                                             bool compact, bool markUnkDate, out string dateStr, out string placeStr)
        {
            dateStr = string.Empty;
            placeStr = string.Empty;

            if (iRec != null) {
                GDMCustomEvent evt = iRec.FindEvent(eventName);
                if (evt != null) {
                    dateStr = GEDCOMEventToDateStr(evt, dateFormat, false);
                    if (dateStr != "") {
                        if (compact) dateStr = CompactDate(dateStr);
                    }
                    if (dateStr == "" && markUnkDate) {
                        dateStr = "?";
                    }

                    placeStr = GetPlaceStr(evt, false);
                }
            }
        }

        public static string GetPedigreeLifeStr(GDMIndividualRecord iRec, PedigreeFormat fmt)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result = "";

            switch (fmt) {
                case PedigreeFormat.Excess:
                    {
                        string ds = GetBirthDate(iRec, GlobalOptions.Instance.DefDateFormat, true);
                        if (ds == "")
                        {
                            ds = "?";
                        }
                        result += ds;
                        ds = GetDeathDate(iRec, GlobalOptions.Instance.DefDateFormat, true);
                        if (ds == "")
                        {
                            GDMCustomEvent ev = iRec.FindEvent(GEDCOMTagType.DEAT);
                            if (ev != null)
                            {
                                ds = "?";
                            }
                        }
                        if (ds != "")
                        {
                            result = result + " - " + ds;
                        }
                    }
                    break;

                case PedigreeFormat.Compact:
                    {
                        string ds, ps;

                        GetEventDatePlace(iRec, GEDCOMTagName.BIRT, GlobalOptions.Instance.DefDateFormat, true, true, out ds, out ps);
                        if (ps != "") {
                            if (ds != "") {
                                ds += ", ";
                            }
                            ds += ps;
                        }
                        if (ds != "") {
                            ds = ImportUtils.STD_BIRTH_SIGN + ds;
                        }
                        result += ds;

                        GetEventDatePlace(iRec, GEDCOMTagName.DEAT, GlobalOptions.Instance.DefDateFormat, true, true, out ds, out ps);
                        if (ps != "") {
                            if (ds != "") {
                                ds += ", ";
                            }
                            ds += ps;
                        }
                        if (ds != "") {
                            ds = ImportUtils.STD_DEATH_SIGN + ds;
                            result = result + " " + ds;
                        }
                    }
                    break;
            }

            result = result.Trim();
            result = (result == "") ? "" : " (" + result + ")";
            return result;
        }

        public static int GetChronologicalYear(GDMCustomEvent evt)
        {
            return (evt == null) ? 0 : evt.Date.GetChronologicalYear();
        }

        public static int GetEventsYearsDiff(GDMCustomEvent ev1, GDMCustomEvent ev2, bool currentEnd)
        {
            int result = -1;

            try {
                int dt1 = GetChronologicalYear(ev1);
                int dt2 = GetChronologicalYear(ev2);

                if (currentEnd && dt2 == 0) {
                    dt2 = DateTime.Now.Year;
                }

                if (dt1 != 0 && dt2 != 0) {
                    result = Math.Abs(dt2 - dt1);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetEventsYearsDiff()", ex);
            }

            return result;
        }

        public static string GetLifeExpectancyStr(GDMIndividualRecord iRec)
        {
            int result = GetLifeExpectancy(iRec);
            return (result == -1) ? "" : result.ToString();
        }

        public static int GetLifeExpectancy(GDMIndividualRecord iRec)
        {
            int result = -1;
            if (iRec == null) return result;

            try {
                var lifeDates = iRec.GetLifeDates();
                result = GetEventsYearsDiff(lifeDates.BirthEvent, lifeDates.DeathEvent, false);
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetLifeExpectancy()", ex);
            }

            return result;
        }

        public static string GetAgeStr(GDMIndividualRecord iRec, int toYear)
        {
            int result = GetAge(iRec, toYear);
            return (result == -1) ? "" : result.ToString();
        }

        public static int GetAge(GDMIndividualRecord iRec, int toYear)
        {
            int result = -1;
            if (iRec == null) return result;

            try {
                var lifeDates = iRec.GetLifeDates();

                if (toYear == -1) {
                    result = GetEventsYearsDiff(lifeDates.BirthEvent, lifeDates.DeathEvent, lifeDates.DeathEvent == null);
                } else {
                    int birthYear = GetChronologicalYear(lifeDates.BirthEvent);
                    if (birthYear != 0) {
                        result = toYear - birthYear;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetAge()", ex);
            }

            return result;
        }

        public static GDMCustomDate GetMarriageDate(GDMFamilyRecord fRec)
        {
            if (fRec == null) {
                return null;
            }

            GDMCustomEvent evt = fRec.FindEvent(GEDCOMTagType.MARR);
            return ((evt == null) ? null : evt.Date.Value);
        }

        public static string GetMarriageDateStr(GDMFamilyRecord fRec, DateFormat dateFormat)
        {
            GDMCustomDate date = GetMarriageDate(fRec);
            return (date == null) ? string.Empty : date.GetDisplayStringExt(dateFormat, false, false);
        }

        /// <summary>
        /// Get number of days remained until the birthday of the specified
        /// person.
        /// </summary>
        /// <param name="iRec">A person record to check.</param>
        /// <returns>Number of days remained until the birthday of
        /// the <paramref name="iRec" />. The caller must ignore this value if
        /// the method returns -1.</returns>
        public static int GetDaysForBirth(GDMIndividualRecord iRec)
        {
            int distance = -1;

            if (iRec != null) {
                int bdD, bdM, bdY;

                try {
                    GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.DEAT);
                    if (evt == null) {
                        evt = iRec.FindEvent(GEDCOMTagType.BIRT);
                        if (evt != null) {
                            var dt = evt.Date.Value as GDMDate;
                            if (dt != null) {
                                bdM = dt.Month;
                                bdD = dt.Day;

                                if (bdM != 0 && bdD != 0) {
                                    DateTime dtNow = DateTime.Now.Date;
                                    int curY = dtNow.Year;
                                    int curM = dtNow.Month;
                                    int curD = dtNow.Day;
                                    double dt2 = curY + bdM / 12.0 + bdD / 12.0 / 31.0;
                                    double dt3 = curY + curM / 12.0 + curD / 12.0 / 31.0;
                                    bdY = (dt2 < dt3) ? (curY + 1) : curY;

                                    // There are valid birthdays on February 29th in leap years. 
                                    // For other years, we need a correction for an acceptable day.
                                    if (bdD == 29 && bdM == 2 && !DateTime.IsLeapYear(bdY)) {
                                        bdD -= 1;
                                    }

                                    distance = DateHelper.DaysBetween(dtNow, new DateTime(bdY, bdM, bdD));
                                }
                            }
                        }
                    }
                } catch (Exception ex) {
                    Logger.WriteError("GKUtils.GetDaysForBirth()", ex);
                }
            }

            return distance;
        }

        #endregion

        #region Places functions

        public static string GetBirthPlace(GDMIndividualRecord iRec)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent(GEDCOMTagType.BIRT), false);
        }

        public static string GetDeathPlace(GDMIndividualRecord iRec)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent(GEDCOMTagType.DEAT), false);
        }

        public static string GetResidencePlace(GDMIndividualRecord iRec, bool includeAddress)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent(GEDCOMTagType.RESI), includeAddress);
        }

        public static string GetPlaceStr(GDMCustomEvent evt, bool includeAddress)
        {
            if (evt == null) return string.Empty;

            string result = evt.Place.StringValue;

            if (includeAddress)
            {
                string resi = evt.StringValue;
                string addr = evt.Address.Lines.Text.Trim();
                if (resi != "" && addr != "")
                {
                    resi += ", ";
                }
                resi += addr;
                if (resi != "")
                {
                    result = result + " [" + resi + "]";
                }
            }

            return result;
        }

        #endregion

        #region Individual functions

        public static int GetAncestorsCount(GDMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null) {
                int val = (int)iRec.ExtData;

                if (val < 0) {
                    val = 1;

                    GDMFamilyRecord family = iRec.GetParentsFamily();
                    if (family != null) {
                        GDMIndividualRecord anc;

                        anc = family.Husband.Individual;
                        val += GetAncestorsCount(anc);

                        anc = family.Wife.Individual;
                        val += GetAncestorsCount(anc);
                    }

                    iRec.ExtData = val;
                }

                result = val;
            }

            return result;
        }

        public static int GetDescendantsCount(GDMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null) {
                int val = (int)iRec.ExtData;
                if (val < 0) {
                    val = 1;

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++) {
                        GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                        int num2 = family.Children.Count;
                        for (int j = 0; j < num2; j++) {
                            GDMIndividualRecord child = family.Children[j].Individual;
                            val += GetDescendantsCount(child);
                        }
                    }
                    iRec.ExtData = val;
                }
                result = val;
            }

            return result;
        }

        private static int GetDescGens_Recursive(GDMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null) {
                int max = 0;

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = family.Children[j].Individual;
                        int res = GetDescGens_Recursive(child);
                        if (max < res) {
                            max = res;
                        }
                    }
                }
                result = 1 + max;
            }

            return result;
        }

        public static int GetDescGenerations(GDMIndividualRecord iRec)
        {
            return GetDescGens_Recursive(iRec) - 1;
        }

        public static int GetMarriagesCount(GDMIndividualRecord iRec)
        {
            int result = ((iRec == null) ? 0 : iRec.SpouseToFamilyLinks.Count);
            return result;
        }

        public static int GetSpousesDiff(GDMFamilyRecord fRec)
        {
            int result = -1;

            try {
                if (fRec != null) {
                    GDMIndividualRecord husb = fRec.Husband.Individual;
                    GDMIndividualRecord wife = fRec.Wife.Individual;

                    if (husb != null && wife != null) {
                        GDMCustomEvent evH = husb.FindEvent(GEDCOMTagType.BIRT);
                        GDMCustomEvent evW = wife.FindEvent(GEDCOMTagType.BIRT);

                        result = GetEventsYearsDiff(evH, evW, false);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetSpousesDiff()", ex);
            }

            return result;
        }

        public static GDMIndividualRecord GetFirstborn(GDMIndividualRecord iRec)
        {
            GDMIndividualRecord iChild = null;
            if (iRec == null) return iChild;

            try {
                int firstYear = 0;

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
                    if (family == null) continue;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = family.Children[j].Individual;
                        if (child == null) continue;

                        GDMCustomEvent evt = child.FindEvent(GEDCOMTagType.BIRT);
                        if (evt == null) continue;

                        int childYear = evt.GetChronologicalYear();

                        if (firstYear == 0) {
                            firstYear = childYear;
                            iChild = child;
                        } else {
                            if (firstYear > childYear) {
                                firstYear = childYear;
                                iChild = child;
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetFirstborn()", ex);
            }
            return iChild;
        }

        public static int GetFirstbornAge(GDMIndividualRecord iRec, GDMIndividualRecord iChild)
        {
            int result = 0;
            if (iRec == null || iChild == null) return result;

            try {
                GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.BIRT);
                if (evt == null) return result;
                int parentYear = evt.GetChronologicalYear();

                evt = iChild.FindEvent(GEDCOMTagType.BIRT);
                if (evt == null) return result;
                int childYear = evt.GetChronologicalYear();

                if (parentYear != 0 && childYear != 0) {
                    result = (childYear - parentYear);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetFirstbornAge()", ex);
            }
            return result;
        }

        public static int GetMarriageAge(GDMIndividualRecord iRec)
        {
            int result = 0;
            if (iRec == null) return result;

            try {
                int firstYear = 0;

                GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagType.BIRT);
                if (evt != null) {
                    int mainYear = evt.GetChronologicalYear();

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++) {
                        GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
                        if (family == null) continue;

                        GDMCustomEvent marrEvt = family.FindEvent(GEDCOMTagType.MARR);
                        if (marrEvt == null) continue;

                        int spouseYear = marrEvt.GetChronologicalYear();

                        if (firstYear == 0) {
                            firstYear = spouseYear;
                        } else {
                            if (firstYear > spouseYear) {
                                firstYear = spouseYear;
                            }
                        }
                    }

                    if (mainYear != 0 && firstYear != 0) {
                        result = (firstYear - mainYear);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetMarriageAge()", ex);
            }
            return result;
        }

        #endregion

        #region Tree utils

        public static void InitExtData(GDMTree tree)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];
                rec.ExtData = null;
            }
        }

        public static void InitExtCounts(GDMTree tree, int value)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];

                if (rec is GDMIndividualRecord) {
                    rec.ExtData = value;
                }
            }
        }

        public static void PrepareHeader(GDMTree tree, string fileName, GEDCOMCharacterSet charSet, bool zeroRev)
        {
            GDMHeader header = tree.Header;

            string subm = header.Submitter.XRef;
            int oldRev = header.File.Revision;
            GDMLanguageID langId = header.Language;

            header.Clear();
            header.Source.StringValue = "GEDKeeper";
            header.ReceivingSystemName = "GEDKeeper";
            header.CharacterSet.Value = charSet;
            header.Language = langId;
            header.GEDCOM.Version = "5.5.1";
            header.GEDCOM.Form = "LINEAGE-LINKED";
            header.File.StringValue = Path.GetFileName(fileName);
            header.TransmissionDateTime = DateTime.Now;

            header.Source.Version = GKData.APP_FORMAT_CURVER.ToString();

            if (zeroRev) {
                header.File.Revision = 0;
            } else {
                header.File.Revision = oldRev + 1;
            }

            if (!string.IsNullOrEmpty(subm)) {
                header.Submitter.XRef = subm;
            }
        }

        #endregion

        #region Folder functions

        public static string GetTempDir()
        {
            string tempPath = Environment.GetEnvironmentVariable("TEMP");
            return tempPath + Path.DirectorySeparatorChar;
        }

        public static string GetAppPath()
        {
            Assembly asm = Assembly.GetEntryAssembly();
            if (asm == null) {
                asm = Assembly.GetExecutingAssembly();
            }

            Module[] mods = asm.GetModules();
            string fn = mods[0].FullyQualifiedName;
            return Path.GetDirectoryName(fn) + Path.DirectorySeparatorChar;
        }

        public static string GetPluginsPath()
        {
            string appPath = GetAppPath();
            return appPath + "plugins" + Path.DirectorySeparatorChar;
        }

        public static string GetLangsPath()
        {
            string appPath = GetAppPath();
            return appPath + "locales" + Path.DirectorySeparatorChar;
        }

        public static string GetHelpPath(string langSign)
        {
            string appPath = GetLangsPath();
            return appPath + "help_" + langSign + Path.DirectorySeparatorChar;
        }

        public static string GetBackgroundsPath()
        {
            string appPath = GetAppPath();
            return appPath + "backgrounds" + Path.DirectorySeparatorChar;
        }

        public static string GetHomePath()
        {
            string homePath = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
            return homePath + Path.DirectorySeparatorChar;
        }

        public static void CopyFile(FileInfo source, FileInfo target, IProgressController progressController)
        {
            using (var sourceStm = source.OpenRead()) {
                CopyFile(sourceStm, target, progressController);
            }
        }

        public static void CopyFile(Stream sourceStm, FileInfo target, IProgressController progressController)
        {
            const int bufferSize = 1024 * 1024; // 1MB
            byte[] buffer = new byte[bufferSize];
            int progress = 0, reportedProgress = 0, read = 0;
            long len = sourceStm.Length;
            float flen = len;

            using (var targetStm = target.OpenWrite()) {
                targetStm.SetLength(sourceStm.Length);
                for (long size = 0; size < len; size += read) {
                    if (progressController != null) {
                        progress = (int)((size / flen) * 100);
                        if (progress != reportedProgress) {
                            reportedProgress = progress;
                            progressController.ProgressStep(reportedProgress);
                        }
                    }

                    read = sourceStm.Read(buffer, 0, bufferSize);
                    targetStm.Write(buffer, 0, read);
                }
            }
        }

        public static Stream LoadResourceStream(string resName)
        {
            return LoadResourceStream(typeof(GKUtils), resName);
        }

        public static Stream LoadResourceStream(Type baseType, string resName)
        {
            Assembly assembly = baseType.Assembly;
            return assembly.GetManifestResourceStream(resName);
        }

        public static string GetRelativePath(string fromFileName, string toFileName)
        {
            var fromPath = Path.GetDirectoryName(fromFileName)+ Path.DirectorySeparatorChar;

            var fromUri = new Uri(fromPath);
            var toUri = new Uri(toFileName);

            var relativeUri = fromUri.MakeRelativeUri(toUri);
            var relativePath = Uri.UnescapeDataString(relativeUri.ToString());

            return relativePath.Replace('/', Path.DirectorySeparatorChar);
        }

        #endregion

        #region Show information summary

        private static void ShowAddressSummary(GDMAddress address, StringList summary)
        {
            if (address != null && !address.IsEmpty() && summary != null)
            {
                summary.Add("    " + LangMan.LS(LSID.LSID_Address) + ":");

                string ts = "";
                if (address.AddressCountry != "")
                {
                    ts = ts + address.AddressCountry + ", ";
                }
                if (address.AddressState != "")
                {
                    ts = ts + address.AddressState + ", ";
                }
                if (address.AddressCity != "")
                {
                    ts += address.AddressCity;
                }
                if (ts != "")
                {
                    summary.Add("    " + ts);
                }

                ts = "";
                if (address.AddressPostalCode != "")
                {
                    ts = ts + address.AddressPostalCode + ", ";
                }
                if (address.Lines.Text.Trim() != "")
                {
                    ts += address.Lines.Text.Trim();
                }
                if (ts != "")
                {
                    summary.Add("    " + ts);
                }

                int num = address.PhoneNumbers.Count;
                for (int i = 0; i < num; i++) {
                    summary.Add("    " + address.PhoneNumbers[i].StringValue);
                }

                int num2 = address.EmailAddresses.Count;
                for (int i = 0; i < num2; i++) {
                    summary.Add("    " + address.EmailAddresses[i].StringValue);
                }

                int num3 = address.WebPages.Count;
                for (int i = 0; i < num3; i++) {
                    summary.Add("    " + address.WebPages[i].StringValue);
                }
            }
        }

        private static void ShowDetailCause(GDMCustomEvent evt, StringList summary)
        {
            string cause = GetEventCause(evt);
            if (summary != null && !string.IsNullOrEmpty(cause)) {
                summary.Add("    " + cause);
            }
        }

        private static void ShowEventDetailInfo(GDMCustomEvent eventDetail, StringList summary)
        {
            if (eventDetail == null)
                throw new ArgumentNullException("eventDetail");

            if (summary != null && eventDetail.SourceCitations.Count != 0) {
                summary.Add("   " + LangMan.LS(LSID.LSID_RPSources) + " (" + eventDetail.SourceCitations.Count.ToString() + "):");

                int num = eventDetail.SourceCitations.Count;
                for (int i = 0; i < num; i++) {
                    GDMSourceCitation cit = eventDetail.SourceCitations[i];
                    GDMSourceRecord sourceRec = cit.Value as GDMSourceRecord;
                    if (sourceRec == null) continue;

                    string nm = "\"" + sourceRec.ShortTitle + "\"";
                    if (!string.IsNullOrEmpty(cit.Page)) {
                        nm = nm + ", " + cit.Page;
                    }
                    summary.Add("     " + HyperLink(sourceRec.XRef, nm, 0));
                }
            }
        }

        private static void ShowEvent(GDMRecord subj, StringList aToList, GDMRecord aRec, GDMCustomEvent evt)
        {
            if (subj is GDMNoteRecord) {
                int num = evt.Notes.Count;
                for (int i = 0; i < num; i++) {
                    if (evt.Notes[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else
                if (subj is GDMMultimediaRecord) {
                int num2 = evt.MultimediaLinks.Count;
                for (int i = 0; i < num2; i++) {
                    if (evt.MultimediaLinks[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else if (subj is GDMSourceRecord) {
                int num3 = evt.SourceCitations.Count;
                for (int i = 0; i < num3; i++) {
                    if (evt.SourceCitations[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, evt.SourceCitations[i]);
                    }
                }
            }
        }

        private static void ShowLink(GDMRecord aSubject, StringList aToList, GDMRecord aRec, GDMTag aTag, GDMPointer aExt)
        {
            string prefix;
            if (aSubject is GDMSourceRecord && aExt != null) {
                GDMSourceCitation cit = (aExt as GDMSourceCitation);
                if (cit != null && !string.IsNullOrEmpty(cit.Page)) {
                    prefix = cit.Page + ": ";
                } else {
                    prefix = "";
                }
            } else {
                prefix = "";
            }

            string suffix;
            if (aTag is GDMCustomEvent) {
                suffix = ", " + GetEventName((GDMCustomEvent) aTag).ToLower();
            } else {
                suffix = "";
            }
            aToList.Add("    " + prefix + GenRecordLink(aRec, true) + suffix);
        }

        private static void ShowPersonExtInfo(GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            /*if (tree == null || iRec == null || summary == null) return;

            summary.Add("");
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
        		GEDCOMRecord rec = tree[i];
                if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)rec;

                bool first = true;
                for (int k = 0, cnt = ir.Associations.Count; k < cnt; k++) {
                    GEDCOMAssociation asso = ir.Associations[k];

                    if (asso.Individual == iRec) {
                        if (first) {
                            summary.Add(LangMan.LS(LSID.LSID_Associations) + ":");
                            first = false;
                        }
                        summary.Add("    " + HyperLink(ir.XRef, ir.GetNameString(true, false), 0));
                    }
                }
            }*/
        }

        private static void ShowPersonNamesakes(GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            try {
                StringList namesakes = new StringList();
                try {
                    string st = GetNameString(iRec, true, false);

                    int num3 = tree.RecordsCount;
                    for (int i = 0; i < num3; i++) {
                        GDMRecord rec = tree[i];

                        if (rec is GDMIndividualRecord && rec != iRec) {
                            GDMIndividualRecord relPerson = (GDMIndividualRecord) rec;

                            string unk = GetNameString(relPerson, true, false);
                            if (st == unk) {
                                namesakes.AddObject(unk + GetLifeStr(relPerson), relPerson);
                            }
                        }
                    }

                    if (namesakes.Count > 0) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Namesakes) + ":");

                        int num4 = namesakes.Count;
                        for (int i = 0; i < num4; i++) {
                            GDMIndividualRecord relPerson = (GDMIndividualRecord)namesakes.GetObject(i);
                            
                            summary.Add("    " + HyperLink(relPerson.XRef, namesakes[i], 0));
                        }
                    }
                }
                finally {
                    namesakes.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowPersonNamesakes()", ex);
            }
        }

        private static void ShowSubjectLinks(GDMRecord aInRecord, GDMRecord subject, StringList aToList)
        {
            try {
                int num;

                if (subject is GDMNoteRecord) {
                    num = aInRecord.Notes.Count;
                    for (int i = 0; i < num; i++) {
                        if (aInRecord.Notes[i].Value == subject) {
                            ShowLink(subject, aToList, aInRecord, null, null);
                        }
                    }
                } else if (subject is GDMMultimediaRecord) {
                    num = aInRecord.MultimediaLinks.Count;
                    for (int i = 0; i < num; i++) {
                        if (aInRecord.MultimediaLinks[i].Value == subject) {
                            ShowLink(subject, aToList, aInRecord, null, null);
                        }
                    }
                } else if (subject is GDMSourceRecord) {
                    num = aInRecord.SourceCitations.Count;
                    for (int i = 0; i < num; i++) {
                        if (aInRecord.SourceCitations[i].Value == subject) {
                            ShowLink(subject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
                        }
                    }
                }

                var recordWithEvents = aInRecord as GDMRecordWithEvents;
                if (recordWithEvents != null) {
                    GDMRecordWithEvents evsRec = recordWithEvents;

                    num = evsRec.Events.Count;
                    for (int i = 0; i < num; i++) {
                        ShowEvent(subject, aToList, evsRec, evsRec.Events[i]);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSubjectLinks()", ex);
            }
        }

        public static void SearchRecordLinks(List<GDMObject> linksList, GDMRecord inRecord, GDMRecord searchRec)
        {
            try {
                int num;
                switch (searchRec.RecordType) {
                    case GDMRecordType.rtNote:
                        num = inRecord.Notes.Count;
                        for (int i = 0; i < num; i++) {
                            var notes = inRecord.Notes[i];
                            if (notes.Value == searchRec) {
                                linksList.Add(notes);
                            }
                        }
                        break;

                    case GDMRecordType.rtMultimedia:
                        num = inRecord.MultimediaLinks.Count;
                        for (int i = 0; i < num; i++) {
                            var mmLink = inRecord.MultimediaLinks[i];
                            if (mmLink.Value == searchRec) {
                                linksList.Add(mmLink);
                            }
                        }
                        break;

                    case GDMRecordType.rtSource:
                        num = inRecord.SourceCitations.Count;
                        for (int i = 0; i < num; i++) {
                            var sourCit = inRecord.SourceCitations[i];
                            if (sourCit.Value == searchRec) {
                                linksList.Add(sourCit);
                            }
                        }
                        break;
                }

                /*var recordWithEvents = aInRecord as GEDCOMRecordWithEvents;
                if (recordWithEvents != null) {
                    GEDCOMRecordWithEvents evsRec = recordWithEvents;

                    num = evsRec.Events.Count;
                    for (int i = 0; i < num; i++) {
                        ShowEvent(subject, linksList, evsRec, evsRec.Events[i]);
                    }
                }*/
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.SearchRecordLinks()", ex);
            }
        }

        public static void SearchRecordLinks(List<GDMObject> linksList, GDMTree tree, GDMRecord searchRec)
        {
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                SearchRecordLinks(linksList, tree[i], searchRec);
            }
        }

        private static void RecListMediaRefresh(GDMRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.MultimediaLinks.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPMultimedia) + " (" + record.MultimediaLinks.Count.ToString() + "):");

                    int num = record.MultimediaLinks.Count;
                    for (int i = 0; i < num; i++) {
                        GDMMultimediaLink mmLink = record.MultimediaLinks[i];
                        GDMMultimediaRecord mmRec = mmLink.Value as GDMMultimediaRecord;
                        if (mmRec == null || mmRec.FileReferences.Count == 0) continue;

                        string st = mmRec.FileReferences[0].Title;
                        summary.Add("  " + HyperLink(mmRec.XRef, st, 0) + " (" +
                                    HyperLink("view_" + mmRec.XRef, LangMan.LS(LSID.LSID_MediaView), 0) + ")");
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListMediaRefresh()", ex);
            }
        }

        private static void RecListNotesRefresh(GDMRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.Notes.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPNotes) + " (" + record.Notes.Count.ToString() + "):");

                    int num = record.Notes.Count;
                    for (int i = 0; i < num; i++) {
                        if (i > 0) {
                            summary.Add("");
                        }

                        GDMNotes note = record.Notes[i];

                        int num2 = note.Lines.Count;
                        for (int k = 0; k < num2; k++) {
                            string st = note.Lines[k];
                            summary.Add(st);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListNotesRefresh()", ex);
            }
        }

        private static void RecListSourcesRefresh(GDMRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.SourceCitations.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPSources) + " (" + record.SourceCitations.Count.ToString() + "):");

                    int num = record.SourceCitations.Count;
                    for (int i = 0; i < num; i++) {
                        GDMSourceCitation cit = record.SourceCitations[i];
                        GDMSourceRecord sourceRec = cit.Value as GDMSourceRecord;
                        if (sourceRec == null) continue;

                        string nm = "\"" + sourceRec.ShortTitle + "\"";

                        if (cit.Page != "") {
                            nm = nm + ", " + cit.Page;
                        }

                        summary.Add("  " + HyperLink(sourceRec.XRef, nm, 0));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListSourcesRefresh()", ex);
            }
        }

        private static void RecListAssociationsRefresh(GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.Associations.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Associations) + ":");

                    int num = record.Associations.Count;
                    for (int i = 0; i < num; i++) {
                        GDMAssociation ast = record.Associations[i];
                        string nm = ((ast.Individual == null) ? "" : GetNameString(ast.Individual, true, false));
                        string xref = ((ast.Individual == null) ? "" : ast.Individual.XRef);

                        summary.Add("    " + ast.Relation + " " + HyperLink(xref, nm, 0));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListAssociationsRefresh()", ex);
            }
        }

        private static void RecListIndividualEventsRefresh(GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.Events.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Events) + ":");

                    int num = record.Events.Count;
                    for (int i = 0; i < num; i++) {
                        summary.Add("");

                        GDMCustomEvent evt = record.Events[i];
                        string st = GetEventName(evt);

                        string sv = "";
                        if (evt.StringValue != "") {
                            sv = evt.StringValue + ", ";
                        }
                        summary.Add("  " + st + ": " + sv + GetEventDesc(evt));

                        ShowDetailCause(evt, summary);
                        ShowAddressSummary(evt.Address, summary);
                        ShowEventDetailInfo(evt, summary);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListIndividualEventsRefresh()", ex);
            }
        }

        private static void RecListFamilyEventsRefresh(GDMFamilyRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.Events.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Events) + ":");

                    int num = record.Events.Count;
                    for (int i = 0; i < num; i++) {
                        summary.Add("");

                        GDMFamilyEvent evt = (GDMFamilyEvent)record.Events[i];

                        string st = GetEventName(evt);
                        summary.Add("  " + st + ": " + GetEventDesc(evt));

                        ShowDetailCause(evt, summary);
                        ShowEventDetailInfo(evt, summary);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListFamilyEventsRefresh()", ex);
            }
        }

        private static void RecListGroupsRefresh(GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.Groups.Count != 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPGroups) + ":");

                    int num = record.Groups.Count;
                    for (int i = 0; i < num; i++) {
                        GDMPointer ptr = record.Groups[i];
                        GDMGroupRecord grp = ptr.Value as GDMGroupRecord;
                        if (grp == null) continue;

                        summary.Add("    " + HyperLink(grp.XRef, grp.GroupName, 0));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListGroupsRefresh()", ex);
            }
        }

        //

        public static void ShowFamilyInfo(IBaseContext baseContext, GDMFamilyRecord familyRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (familyRec != null) {
                        summary.Add("");

                        GDMIndividualRecord spRec = familyRec.Husband.Individual;
                        string st = ((spRec == null) ? LangMan.LS(LSID.LSID_UnkMale) : HyperLink(spRec.XRef, GetNameString(spRec, true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Husband) + ": " + st + GetLifeStr(spRec));

                        spRec = familyRec.Wife.Individual;
                        st = ((spRec == null) ? LangMan.LS(LSID.LSID_UnkFemale) : HyperLink(spRec.XRef, GetNameString(spRec, true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Wife) + ": " + st + GetLifeStr(spRec));

                        summary.Add("");
                        if (familyRec.Children.Count != 0) {
                            summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                        }

                        int num = familyRec.Children.Count;
                        for (int i = 0; i < num; i++) {
                            var child = familyRec.Children[i].Individual;
                            summary.Add("    " + HyperLink(child.XRef, GetNameString(child, true, false), 0) + GetLifeStr(child));
                        }
                        summary.Add("");

                        RecListFamilyEventsRefresh(familyRec, summary);
                        RecListNotesRefresh(familyRec, summary);
                        RecListMediaRefresh(familyRec, summary);
                        RecListSourcesRefresh(familyRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowFamilyInfo()" , ex);
            }
        }

        public static void ShowGroupInfo(GDMGroupRecord groupRec, StringList summary)
        {
            if (summary == null) return;

            try {
                StringList mbrList = new StringList();
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (groupRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + groupRec.GroupName + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Members) + " (" + groupRec.Members.Count.ToString() + "):");

                        int num = groupRec.Members.Count;
                        for (int i = 0; i < num; i++) {
                            GDMPointer ptr = groupRec.Members[i];
                            GDMIndividualRecord member = (GDMIndividualRecord)ptr.Value;

                            mbrList.AddObject(GetNameString(member, true, false), member);
                        }
                        mbrList.Sort();

                        int num2 = mbrList.Count;
                        for (int i = 0; i < num2; i++) {
                            GDMIndividualRecord member = (GDMIndividualRecord)mbrList.GetObject(i);

                            summary.Add("    " + HyperLink(member.XRef, mbrList[i], i + 1));
                        }

                        RecListNotesRefresh(groupRec, summary);
                        RecListMediaRefresh(groupRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                    mbrList.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowGroupInfo()", ex);
            }
        }

        public static void ShowMultimediaInfo(GDMMultimediaRecord mediaRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (mediaRec != null) {
                        GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
                        string mediaTitle = (fileRef == null) ? LangMan.LS(LSID.LSID_Unknown) : fileRef.Title;

                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + mediaTitle + "[/size][/b][/u]");
                        summary.Add("");
                        if (fileRef != null) {
                            summary.Add("[ " + HyperLink("view_" + mediaRec.XRef, LangMan.LS(LSID.LSID_View), 0) + " ]");
                        }
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GDMTree tree = mediaRec.GetTree();
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++) {
                            ShowSubjectLinks(tree[i], mediaRec, summary);
                        }

                        RecListNotesRefresh(mediaRec, summary);
                        RecListSourcesRefresh(mediaRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowMultimediaInfo()", ex);
            }
        }

        public static void ShowNoteInfo(GDMNoteRecord noteRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (noteRec != null) {
                        summary.Add("");
                        summary.AddStrings(noteRec.Lines.ToArray());
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GDMTree tree = noteRec.GetTree();
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++) {
                            ShowSubjectLinks(tree[i], noteRec, summary);
                        }
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowNoteInfo()", ex);
            }
        }

        public static void ShowPersonInfo(IBaseContext baseContext, GDMIndividualRecord iRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                summary.Clear();
                try {
                    if (iRec != null) {
                        GDMTree tree = iRec.GetTree();

                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + GetNameString(iRec, true, true) + "[/size][/u][/b]");
                        summary.Add(LangMan.LS(LSID.LSID_Sex) + ": " + SexStr(iRec.Sex));
                        try {
                            GDMIndividualRecord father, mother;
                            GDMFamilyRecord fam = iRec.GetParentsFamily();
                            if (fam == null) {
                                father = null;
                                mother = null;
                            } else {
                                father = fam.Husband.Individual;
                                mother = fam.Wife.Individual;
                            }

                            if (father != null || mother != null) {
                                summary.Add("");
                                summary.Add(LangMan.LS(LSID.LSID_Parents) + ":");

                                string st;

                                st = (father == null) ? LangMan.LS(LSID.LSID_UnkMale) : HyperLink(father.XRef, GetNameString(father, true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Father) + ": " + st + GetLifeStr(father));

                                st = (mother == null) ? LangMan.LS(LSID.LSID_UnkFemale) : HyperLink(mother.XRef, GetNameString(mother, true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Mother) + ": " + st + GetLifeStr(mother));
                            }
                        } catch (Exception ex) {
                            Logger.WriteError("GKUtils.ShowPersonInfo().Parents()", ex);
                        }

                        try {
                            int num = iRec.SpouseToFamilyLinks.Count;
                            for (int i = 0; i < num; i++) {
                                GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
                                if (family == null) continue;
                                if (!baseContext.IsRecordAccess(family.Restriction)) continue;

                                string st;
                                GDMIndividualRecord spRec;
                                string unk;
                                if (iRec.Sex == GDMSex.svMale) {
                                    spRec = family.Wife.Individual;
                                    st = LangMan.LS(LSID.LSID_Wife) + ": ";
                                    unk = LangMan.LS(LSID.LSID_UnkFemale);
                                } else {
                                    spRec = family.Husband.Individual;
                                    st = LangMan.LS(LSID.LSID_Husband) + ": ";
                                    unk = LangMan.LS(LSID.LSID_UnkMale);
                                }
                                string marr = GetMarriageDateStr(family, GlobalOptions.Instance.DefDateFormat);
                                if (marr != "") {
                                    marr = LangMan.LS(LSID.LSID_LMarriage) + " " + marr;
                                } else {
                                    marr = LangMan.LS(LSID.LSID_LFamily);
                                }

                                summary.Add("");
                                if (spRec != null) {
                                    st = st + HyperLink(spRec.XRef, GetNameString(spRec, true, false), 0) + " (" + HyperLink(family.XRef, marr, 0) + ")";
                                } else {
                                    st = st + unk + " (" + HyperLink(family.XRef, marr, 0) + ")";
                                }
                                summary.Add(st);

                                int chNum = family.Children.Count;
                                if (chNum != 0) {
                                    summary.Add("");
                                    summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");

                                    for (int k = 0; k < chNum; k++) {
                                        GDMIndividualRecord child = family.Children[k].Individual;
                                        if (child == null) continue;

                                        summary.Add("    " + HyperLink(child.XRef, GetNameString(child, true, false), 0) + GetLifeStr(child));
                                    }
                                }
                            }
                        } catch (Exception ex) {
                            Logger.WriteError("GKUtils.ShowPersonInfo().Families()", ex);
                        }

                        RecListIndividualEventsRefresh(iRec, summary);
                        RecListNotesRefresh(iRec, summary);
                        RecListMediaRefresh(iRec, summary);
                        RecListSourcesRefresh(iRec, summary);
                        RecListAssociationsRefresh(iRec, summary);
                        RecListGroupsRefresh(iRec, summary);

                        ShowPersonNamesakes(tree, iRec, summary);
                        ShowPersonExtInfo(tree, iRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowPersonInfo()", ex);
            }
        }

        public static void ShowSourceInfo(GDMSourceRecord sourceRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                StringList linkList = new StringList();
                try {
                    summary.Clear();
                    if (sourceRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + sourceRec.ShortTitle + "[/size][/b][/u]");
                        summary.Add("");
                        summary.AddMultiline(LangMan.LS(LSID.LSID_Author) + ": " + sourceRec.Originator.Lines.Text.Trim());
                        summary.AddMultiline(LangMan.LS(LSID.LSID_Title) + ": \"" + sourceRec.Title.Lines.Text.Trim() + "\"");
                        summary.AddMultiline(LangMan.LS(LSID.LSID_Publication) + ": \"" + sourceRec.Publication.Lines.Text.Trim() + "\"");
                        summary.AddMultiline(LangMan.LS(LSID.LSID_Text) + ": \"" + sourceRec.Text.Lines.Text.Trim() + "\"");

                        if (sourceRec.RepositoryCitations.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPRepositories) + ":");

                            int num = sourceRec.RepositoryCitations.Count;
                            for (int i = 0; i < num; i++) {
                                GDMRepositoryRecord rep = (GDMRepositoryRecord)sourceRec.RepositoryCitations[i].Value;

                                summary.Add("    " + HyperLink(rep.XRef, rep.RepositoryName, 0));
                            }
                        }

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GDMTree tree = sourceRec.GetTree();

                        int num2 = tree.RecordsCount;
                        for (int j = 0; j < num2; j++) {
                            ShowSubjectLinks(tree[j], sourceRec, linkList);
                        }

                        linkList.Sort();

                        int num3 = linkList.Count;
                        for (int j = 0; j < num3; j++) {
                            summary.Add(linkList[j]);
                        }

                        RecListNotesRefresh(sourceRec, summary);
                        RecListMediaRefresh(sourceRec, summary);
                    }
                } finally {
                    linkList.Dispose();
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSourceInfo()", ex);
            }
        }

        public static void ShowRepositoryInfo(GDMRepositoryRecord repositoryRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (repositoryRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + repositoryRec.RepositoryName.Trim() + "[/size][/b][/u]");
                        summary.Add("");

                        ShowAddressSummary(repositoryRec.Address, summary);

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_RPSources) + ":");

                        var sortedSources = new List<Tuple<string, string>>();
                        GDMTree tree = repositoryRec.GetTree();
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++) {
                            GDMRecord rec = tree[i];

                            if (rec.RecordType == GDMRecordType.rtSource) {
                                GDMSourceRecord srcRec = (GDMSourceRecord) rec;

                                int num2 = srcRec.RepositoryCitations.Count;
                                for (int j = 0; j < num2; j++) {
                                    if (srcRec.RepositoryCitations[j].Value == repositoryRec) {
                                        sortedSources.Add(GenRecordLinkTuple(srcRec, false));
                                    }
                                }
                            }
                        }
                        sortedSources.Sort();
                        foreach (var tpl in sortedSources) {
                            summary.Add("    " + tpl.Item2);
                        }

                        RecListNotesRefresh(repositoryRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowRepositoryInfo()", ex);
            }
        }

        public static void ShowResearchInfo(GDMResearchRecord researchRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (researchRec != null) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Title) + ": [u][b][size=+1]\"" + researchRec.ResearchName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)researchRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_Status) + ": " + LangMan.LS(GKData.StatusNames[(int)researchRec.Status]) + " (" + researchRec.Percent.ToString() + "%)");
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + researchRec.StartDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + researchRec.StopDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        if (researchRec.Tasks.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPTasks) + ":");

                            int num = researchRec.Tasks.Count;
                            for (int i = 0; i < num; i++) {
                                GDMTaskRecord taskRec = researchRec.Tasks[i].Value as GDMTaskRecord;
                                summary.Add("    " + GenRecordLink(taskRec, false));
                            }
                        }

                        if (researchRec.Communications.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPCommunications) + ":");

                            int num2 = researchRec.Communications.Count;
                            for (int i = 0; i < num2; i++) {
                                GDMCommunicationRecord corrRec = researchRec.Communications[i].Value as GDMCommunicationRecord;
                                summary.Add("    " + GenRecordLink(corrRec, false));
                            }
                        }

                        if (researchRec.Groups.Count != 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPGroups) + ":");

                            int num3 = researchRec.Groups.Count;
                            for (int i = 0; i < num3; i++) {
                                GDMGroupRecord grp = (GDMGroupRecord)researchRec.Groups[i].Value;

                                summary.Add("    " + HyperLink(grp.XRef, grp.GroupName, 0));
                            }
                        }

                        RecListNotesRefresh(researchRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowResearchInfo()", ex);
            }
        }

        public static void ShowTaskInfo(GDMTaskRecord taskRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (taskRec != null) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Goal) + ": [u][b][size=+1]" + GetTaskGoalStr(taskRec) + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + taskRec.StartDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + taskRec.StopDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        RecListNotesRefresh(taskRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowTaskInfo()", ex);
            }
        }

        public static void ShowCommunicationInfo(GDMCommunicationRecord commRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (commRec != null) {
                        GDMTree tree = commRec.GetTree();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Theme) + ": [u][b][size=+1]\"" + commRec.CommName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Corresponder) + ": " + GetCorresponderStr(tree, commRec, true));
                        summary.Add(LangMan.LS(LSID.LSID_Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]));
                        summary.Add(LangMan.LS(LSID.LSID_Date) + ": " + commRec.Date.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        RecListNotesRefresh(commRec, summary);
                        RecListMediaRefresh(commRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowCommunicationInfo()", ex);
            }
        }

        public static void ShowLocationInfo(GDMLocationRecord locRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                StringList linkList = null;
                try {
                    summary.Clear();
                    if (locRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + locRec.LocationName.Trim() + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Latitude) + ": " + locRec.Map.Lati);
                        summary.Add(LangMan.LS(LSID.LSID_Longitude) + ": " + locRec.Map.Long);

                        GDMTree tree = locRec.GetTree();

                        linkList = GetLocationLinks(tree, locRec);

                        if (linkList.Count > 0) {
                            linkList.Sort();

                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                            int num = linkList.Count;
                            for (int i = 0; i < num; i++) {
                                GDMRecord rec = linkList.GetObject(i) as GDMRecord;
                                summary.Add("    " + HyperLink(rec.XRef, linkList[i], 0));
                            }
                        }

                        RecListNotesRefresh(locRec, summary);
                        RecListMediaRefresh(locRec, summary);
                    }
                } finally {
                    if (linkList != null) linkList.Dispose();
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowLocationInfo()", ex);
            }
        }

        public static void GetRecordContent(IBaseContext baseContext, GDMRecord record, StringList ctx)
        {
            if (record != null && ctx != null) {
                try {
                    switch (record.RecordType) {
                        case GDMRecordType.rtIndividual:
                            GKUtils.ShowPersonInfo(baseContext, record as GDMIndividualRecord, ctx);
                            break;

                        case GDMRecordType.rtFamily:
                            GKUtils.ShowFamilyInfo(baseContext, record as GDMFamilyRecord, ctx);
                            break;

                        case GDMRecordType.rtNote:
                            GKUtils.ShowNoteInfo(record as GDMNoteRecord, ctx);
                            break;

                        case GDMRecordType.rtMultimedia:
                            GKUtils.ShowMultimediaInfo(record as GDMMultimediaRecord, ctx);
                            break;

                        case GDMRecordType.rtSource:
                            GKUtils.ShowSourceInfo(record as GDMSourceRecord, ctx);
                            break;

                        case GDMRecordType.rtRepository:
                            GKUtils.ShowRepositoryInfo(record as GDMRepositoryRecord, ctx);
                            break;

                        case GDMRecordType.rtGroup:
                            GKUtils.ShowGroupInfo(record as GDMGroupRecord, ctx);
                            break;

                        case GDMRecordType.rtResearch:
                            GKUtils.ShowResearchInfo(record as GDMResearchRecord, ctx);
                            break;

                        case GDMRecordType.rtTask:
                            GKUtils.ShowTaskInfo(record as GDMTaskRecord, ctx);
                            break;

                        case GDMRecordType.rtCommunication:
                            GKUtils.ShowCommunicationInfo(record as GDMCommunicationRecord, ctx);
                            break;

                        case GDMRecordType.rtLocation:
                            GKUtils.ShowLocationInfo(record as GDMLocationRecord, ctx);
                            break;
                    }
                } catch (Exception ex) {
                    Logger.WriteError("GKUtils.GetRecordContext()", ex);
                }
            }
        }

        #endregion

        #region Multimedia support (static)

        public static string GetStoreFolder(MultimediaKind mmKind)
        {
            string result = "";
            switch (mmKind)
            {
                case MultimediaKind.mkNone:
                    result = "unknown";
                    break;

                case MultimediaKind.mkImage:
                    result = "images";
                    break;

                case MultimediaKind.mkAudio:
                    result = "audio";
                    break;

                case MultimediaKind.mkText:
                    result = "texts";
                    break;

                case MultimediaKind.mkVideo:
                    result = "video";
                    break;
            }
            return result + Path.DirectorySeparatorChar;
        }

        public static MultimediaKind GetMultimediaKind(GDMMultimediaFormat format)
        {
            switch (format)
            {
                case GDMMultimediaFormat.mfNone:
                    return MultimediaKind.mkNone;

                case GDMMultimediaFormat.mfBMP:
                case GDMMultimediaFormat.mfGIF:
                case GDMMultimediaFormat.mfJPG:
                case GDMMultimediaFormat.mfPCX: // .net isn't supports
                case GDMMultimediaFormat.mfTIF:
                case GDMMultimediaFormat.mfTGA: // .net isn't supports
                case GDMMultimediaFormat.mfPNG:
                case GDMMultimediaFormat.mfRAW: // .net isn't supports
                case GDMMultimediaFormat.mfPSD: // .net isn't supports
                    return MultimediaKind.mkImage;

                case GDMMultimediaFormat.mfTXT:
                case GDMMultimediaFormat.mfRTF:
                case GDMMultimediaFormat.mfHTM:
                case GDMMultimediaFormat.mfPDF:
                    return MultimediaKind.mkText;

                case GDMMultimediaFormat.mfWAV:
                case GDMMultimediaFormat.mfMP3:
                case GDMMultimediaFormat.mfWMA:
                case GDMMultimediaFormat.mfMKA:
                    return MultimediaKind.mkAudio;

                case GDMMultimediaFormat.mfAVI:
                case GDMMultimediaFormat.mfMPG:
                case GDMMultimediaFormat.mfMP4:
                case GDMMultimediaFormat.mfOGV:
                case GDMMultimediaFormat.mfWMV:
                case GDMMultimediaFormat.mfMKV:
                case GDMMultimediaFormat.mfMOV:
                    return MultimediaKind.mkVideo;

                case GDMMultimediaFormat.mfOLE:
                case GDMMultimediaFormat.mfUnknown:
                default:
                    return MultimediaKind.mkNone;
            }
        }

        public static MediaStoreType GetStoreTypeEx(GDMFileReference fileReference)
        {
            if (fileReference == null)
                throw new ArgumentNullException("fileReference");

            string fileRef = fileReference.StringValue;

            MediaStoreType result;
            if (fileRef.IndexOf(GKData.GKStoreTypes[2].Sign) == 0) {
                result = MediaStoreType.mstArchive;
            } else if (fileRef.IndexOf(GKData.GKStoreTypes[1].Sign) == 0) {
                result = MediaStoreType.mstStorage;
            } else if (fileRef.IndexOf(GKData.GKStoreTypes[3].Sign) == 0) {
                result = MediaStoreType.mstRelativeReference;
            } else {
                result = MediaStoreType.mstReference;
            }
            return result;
        }

        public static MediaStore GetStoreType(GDMFileReference fileReference)
        {
            if (fileReference == null)
                throw new ArgumentNullException("fileReference");

            string fileName = fileReference.StringValue;
            MediaStoreType storeType = GetStoreTypeEx(fileReference);

            if (storeType != MediaStoreType.mstReference) {
                fileName = fileName.Remove(0, 4);
            }

            return new MediaStore(storeType, fileName);
        }

        public static bool UseEmbeddedViewer(GDMMultimediaFormat format)
        {
            MultimediaKind mmKind = GKUtils.GetMultimediaKind(format);

            switch (mmKind) {
                case MultimediaKind.mkImage:
                    return !(format == GDMMultimediaFormat.mfPCX || format == GDMMultimediaFormat.mfTGA ||
                             format == GDMMultimediaFormat.mfRAW || format == GDMMultimediaFormat.mfPSD);

                case MultimediaKind.mkVideo:
                case MultimediaKind.mkAudio:
                    return GlobalOptions.Instance.EmbeddedMediaPlayer;

                case MultimediaKind.mkText:
                    return (format == GDMMultimediaFormat.mfTXT || format == GDMMultimediaFormat.mfRTF ||
                            format == GDMMultimediaFormat.mfHTM);

                case MultimediaKind.mkNone:
                default:
                    return false;
            }
        }

        #endregion

        #region Archives support (static)

        public static string GetContainerName(string fileName, bool arc)
        {
            string result = Path.GetFileNameWithoutExtension(fileName);
            if (arc) {
                result += ".zip";
            } else {
                result += Path.DirectorySeparatorChar;
            }
            return result;
        }

        public static bool FileCanBeArchived(string fileName)
        {
            GDMMultimediaFormat fileFmt = GDMFileReference.RecognizeFormat(fileName);

            FileInfo info = new FileInfo(fileName);
            double fileSize = (((double)info.Length / 1024) / 1024); // mb

            MultimediaKind mKind = GetMultimediaKind(fileFmt);
            return ((mKind == MultimediaKind.mkImage || mKind == MultimediaKind.mkText) && fileSize <= 10);
        }

        #endregion

        #region Names processing

        public static string GetFamilyString(GDMFamilyRecord family)
        {
            if (family == null)
                throw new ArgumentNullException("family");

            return GetFamilyString(family, LangMan.LS(LSID.LSID_UnkMale), LangMan.LS(LSID.LSID_UnkFemale));
        }

        public static string GetFamilyString(GDMFamilyRecord family, string unkHusband, string unkWife)
        {
            if (family == null)
                throw new ArgumentNullException("family");

            string result = "";

            GDMIndividualRecord spouse = family.Husband.Individual;
            if (spouse == null) {
                if (unkHusband == null) unkHusband = "?";
                result += unkHusband;
            } else {
                result += GetNameString(spouse, true, false);
            }

            result += " - ";

            spouse = family.Wife.Individual;
            if (spouse == null) {
                if (unkWife == null) unkWife = "?";
                result += unkWife;
            } else {
                result += GetNameString(spouse, true, false);
            }

            return result;
        }

        public static string GetNickString(GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result;
            if (iRec.PersonalNames.Count > 0)
            {
                GDMPersonalName np = iRec.PersonalNames[0];
                result = np.Pieces.Nickname;
            }
            else
            {
                result = "";
            }
            return result;
        }

        // TODO: what if you want to display only a surname that is missing?
        private static string GetFmtSurname(GDMIndividualRecord iRec, GDMPersonalName personalName, string defSurname)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (personalName == null)
                throw new ArgumentNullException("personalName");

            string result;

            WomanSurnameFormat wsFmt = GlobalOptions.Instance.WomanSurnameFormat;
            if (iRec.Sex == GDMSex.svFemale && wsFmt != WomanSurnameFormat.wsfNotExtend) {
                switch (wsFmt) {
                    case WomanSurnameFormat.wsfMaiden_Married:
                        result = defSurname;
                        if (personalName.Pieces.MarriedName.Length > 0) {
                            if (result.Length > 0) result += " ";
                            result += "(" + personalName.Pieces.MarriedName + ")";
                        }
                        break;

                    case WomanSurnameFormat.wsfMarried_Maiden:
                        result = personalName.Pieces.MarriedName;
                        if (defSurname.Length > 0) {
                            if (result.Length > 0) result += " ";
                            result += "(" + defSurname + ")";
                        }
                        break;

                    case WomanSurnameFormat.wsfMaiden:
                        result = defSurname; // by default GEDCOMPersonalName.Surname is maiden surname
                        break;

                    case WomanSurnameFormat.wsfMarried:
                        result = personalName.Pieces.MarriedName;
                        break;

                    default:
                        result = defSurname;
                        break;
                }
            } else {
                result = defSurname;
            }

            return result;
        }

        public static string GetNameString(GDMIndividualRecord iRec, GDMPersonalName np, bool firstSurname, bool includePieces)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (np == null)
                throw new ArgumentNullException("np");

            string result;

            string firstPart = np.FirstPart;
            string surname = np.Surname;

            surname = GetFmtSurname(iRec, np, surname);

            if (firstSurname) {
                result = surname + " " + firstPart;
            } else {
                result = firstPart + " " + surname;
            }

            if (includePieces) {
                string nick = np.Pieces.Nickname;
                if (!string.IsNullOrEmpty(nick)) result = result + " [" + nick + "]";
            }

            return result;
        }

        public static string GetNameString(GDMIndividualRecord iRec, bool firstSurname, bool includePieces)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result;
            if (iRec.PersonalNames.Count > 0) {
                GDMPersonalName np = iRec.PersonalNames[0];
                result = GetNameString(iRec, np, firstSurname, includePieces);
            } else {
                result = "";
            }
            return result;
        }

        public static void SetMarriedSurname(GDMIndividualRecord iRec, string marriedSurname)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GDMPersonalName personalName;
            if (iRec.PersonalNames.Count <= 0) {
                personalName = iRec.AddPersonalName(new GDMPersonalName(iRec));
            } else {
                personalName = iRec.PersonalNames[0];
            }

            personalName.Pieces.MarriedName = marriedSurname.Trim();
        }

        public static void SetNameParts(GDMPersonalName personalName, string surname, string name, string patronymic)
        {
            if (personalName == null)
                throw new ArgumentNullException("personalName");

            surname = surname.Trim();
            name = name.Trim();
            patronymic = patronymic.Trim();

            personalName.SetNameParts(name + " " + patronymic, surname, personalName.LastPart);
            personalName.Pieces.Surname = surname;
            personalName.Pieces.Given = name;
            personalName.Pieces.PatronymicName = patronymic;
        }

        public static NamePartsRet GetNameParts(GDMIndividualRecord iRec, GDMPersonalName personalName, bool formatted = true)
        {
            ICulture culture = DefineCulture(iRec, personalName);

            NamePartsRet nameParts = culture.GetNameParts(personalName);

            if (formatted) {
                nameParts.Surname = GetFmtSurname(iRec, personalName, nameParts.Surname);
            }

            return nameParts;
        }

        public static NamePartsRet GetNameParts(GDMIndividualRecord iRec, bool formatted = true)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (iRec.PersonalNames.Count > 0) {
                return GetNameParts(iRec, iRec.PersonalNames[0], formatted);
            } else {
                return new NamePartsRet();
            }
        }

        public static ICulture DefineCulture(GDMIndividualRecord iRec, GDMPersonalName personalName)
        {
            GDMLanguageID langID;

            // first priority - local langID from name
            if (personalName != null) {
                langID = personalName.Language;
            } else {
                langID = GDMLanguageID.Unknown;
            }

            // second priority - global langID from tree
            if (langID == GDMLanguageID.Unknown && iRec != null) {
                GDMTree tree = iRec.GetTree();
                if (tree != null) {
                    langID = tree.Header.Language;
                }
            }

            return CulturesPool.DefineCulture(langID);
        }

        #endregion
    }
}
