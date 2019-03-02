/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public static class GKUtils
    {
        #region Aux functions

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

        public static string SexChar(GEDCOMSex sex)
        {
            string ss = SexStr(sex);
            return string.IsNullOrEmpty(ss) ? "?" : new string(ss[0], 1);
        }

        public static string SexStr(GEDCOMSex sex)
        {
            return LangMan.LS(GKData.SexData[(int)sex].NameId);
        }

        public static GEDCOMSex GetSexBySign(char sexSign)
        {
            GEDCOMSex result = GEDCOMSex.svNone;
            
            switch (sexSign) {
                case 'F':
                    result = GEDCOMSex.svFemale;
                    break;
                case 'M':
                    result = GEDCOMSex.svMale;
                    break;
                case 'U':
                    result = GEDCOMSex.svUndetermined;
                    break;
            }

            return result;
        }

        public static string MergeStrings(StringList strings)
        {
            if (strings == null)
                throw new ArgumentNullException("strings");

            StringBuilder result = new StringBuilder();

            int num = strings.Count;
            for (int i = 0; i < num; i++)
            {
                if (result.Length != 0) result.Append(" ");
                result.Append(strings[i].Trim());
            }

            return result.ToString();
        }

        public static string TruncateStrings(StringList value, int size)
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

        public static StringList GetLocationLinks(GEDCOMTree tree, GEDCOMLocationRecord locRec)
        {
            var linksList = new StringList();

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = tree[i];

                if (rec is GEDCOMRecordWithEvents)
                {
                    GEDCOMRecordWithEvents evsRec = rec as GEDCOMRecordWithEvents;

                    int num2 = evsRec.Events.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMCustomEvent evt = evsRec.Events[j];

                        if (evt.Place.Location.Value == locRec)
                        {
                            linksList.AddObject(GetRecordName(rec, true) + ", " + GetEventName(evt).ToLower(), rec);
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

        public static string GetRecordName(GEDCOMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                string sign = "";

                if (signed) {
                    GEDCOMRecordType recordType = record.RecordType;
                    if (recordType != GEDCOMRecordType.rtIndividual) {
                        if (recordType == GEDCOMRecordType.rtFamily || (byte)recordType - (byte)GEDCOMRecordType.rtMultimedia < (byte)GEDCOMRecordType.rtResearch)
                        {
                            sign = LangMan.LS(GKData.RecordTypes[(int)record.RecordType]) + ": ";
                        }
                    } else {
                        sign = "";
                    }
                }

                string st;
                switch (record.RecordType) {
                    case GEDCOMRecordType.rtIndividual:
                        st = GetNameString(((GEDCOMIndividualRecord)record), true, false);
                        break;
                    case GEDCOMRecordType.rtFamily:
                        st = GetFamilyString((GEDCOMFamilyRecord)record);
                        break;
                    case GEDCOMRecordType.rtNote:
                        st = ((GEDCOMNoteRecord)record).Note[0]; // TODO: bad solution?!
                        break;
                    case GEDCOMRecordType.rtMultimedia:
                        st = ((GEDCOMMultimediaRecord)record).FileReferences[0].Title;
                        break;
                    case GEDCOMRecordType.rtSource:
                        st = ((GEDCOMSourceRecord)record).FiledByEntry;
                        break;
                    case GEDCOMRecordType.rtRepository:
                        st = ((GEDCOMRepositoryRecord)record).RepositoryName;
                        break;
                    case GEDCOMRecordType.rtGroup:
                        st = ((GEDCOMGroupRecord)record).GroupName;
                        break;
                    case GEDCOMRecordType.rtResearch:
                        st = ((GEDCOMResearchRecord)record).ResearchName;
                        break;
                    case GEDCOMRecordType.rtTask:
                        st = GetTaskGoalStr((GEDCOMTaskRecord)record);
                        break;
                    case GEDCOMRecordType.rtCommunication:
                        st = ((GEDCOMCommunicationRecord)record).CommName;
                        break;
                    case GEDCOMRecordType.rtLocation:
                        st = ((GEDCOMLocationRecord)record).LocationName;
                        break;
                    default:
                        st = record.XRef;
                        break;
                }

                result = sign + st;
            }

            return result;
        }

        public static string GenRecordLink(GEDCOMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                result = HyperLink(record.XRef, GetRecordName(record, signed), 0);
            }

            return result;
        }

        public static string GetCorresponderStr(GEDCOMTree tree, GEDCOMCommunicationRecord commRec, bool aLink)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (commRec == null)
                throw new ArgumentNullException("commRec");

            string result = "";
            var corr = commRec.GetCorresponder();

            if (corr.Corresponder != null)
            {
                string nm = GetNameString(corr.Corresponder, true, false);
                if (aLink)
                {
                    nm = HyperLink(corr.Corresponder.XRef, nm, 0);
                }
                result = "[ " + LangMan.LS(GKData.CommunicationDirs[(int)corr.CommDir]) + " ] " + nm;
            }
            return result;
        }

        public static string GetTaskGoalStr(GEDCOMTaskRecord taskRec)
        {
            if (taskRec == null) return string.Empty;
            
            string result = "";
            
            var goal = taskRec.GetTaskGoal();

            switch (goal.GoalType) {
                case GKGoalType.gtIndividual:
                case GKGoalType.gtFamily:
                case GKGoalType.gtSource:
                    result = GetGoalStr(goal.GoalType, goal.GoalRec);
                    break;

                case GKGoalType.gtOther:
                    result = taskRec.Goal;
                    break;
            }

            if (goal.GoalType != GKGoalType.gtOther)
            {
                result = "[" + LangMan.LS(GKData.GoalNames[(int)goal.GoalType]) + "] " + result;
            }

            return result;
        }

        public static string GetGoalStr(GKGoalType gt, GEDCOMRecord tempRec)
        {
            if (tempRec == null) return string.Empty;

            switch (gt)
            {
                case GKGoalType.gtIndividual:
                    return GetNameString(((GEDCOMIndividualRecord)tempRec), true, false);

                case GKGoalType.gtFamily:
                    return GetFamilyString(tempRec as GEDCOMFamilyRecord);

                case GKGoalType.gtSource:
                    return ((GEDCOMSourceRecord)tempRec).FiledByEntry;
            }

            return string.Empty;
        }

        #endregion

        #region Encoding

        public static string EncodeUID(byte[] binaryKey)
        {
            StringBuilder result = new StringBuilder(36);
            byte checkA = 0;
            byte checkB = 0;

            int num = binaryKey.Length;
            for (int i = 0; i < num; i++)
            {
                byte val = binaryKey[i];
                checkA = unchecked((byte)(checkA + (uint)val));
                checkB = unchecked((byte)(checkB + (uint)checkA));
                result.Append(val.ToString("X2"));
            }

            result.Append(checkA.ToString("X2"));
            result.Append(checkB.ToString("X2"));

            return result.ToString();
        }

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

            return GKUtils.EncodeUID(buffer);
        }

        #endregion

        #region Match functions

        private static string PrepareMask(string mask)
        {
            string regexStr = "";

            if (!string.IsNullOrEmpty(mask)) {
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

        private const RegexOptions RegexOpts = RegexOptions.Singleline | RegexOptions.Compiled |
                                               RegexOptions.IgnoreCase | RegexOptions.CultureInvariant;

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
            // Regex.IsMatch() has caching
            return Regex.IsMatch(str, PrepareMask(mask), RegexOpts);
        }

        #endregion

        #region Event Utils

        public static string GetAttributeValue(GEDCOMIndividualRecord iRec, string attrName)
        {
            if (iRec == null) return string.Empty;

            GEDCOMCustomEvent attr = iRec.FindEvent(attrName);
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

        public static int GetMarriageStatusIndex(string sign)
        {
            int res = 0;

            for (int i = 0; i < GKData.MarriageStatus.Length; i++)
            {
                if (GKData.MarriageStatus[i].StatSign == sign)
                {
                    res = i;
                    break;
                }
            }

            return res;
        }

        public static string GetEventName(GEDCOMCustomEvent evt)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            string result = "";

            if (evt is GEDCOMIndividualEvent || evt is GEDCOMIndividualAttribute)
            {
                int ev = GetPersonEventIndex(evt.Name);
                if (ev == 0) {
                    result = evt.Classification;
                } else {
                    result = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evt.Name;
                }
            }
            else if (evt is GEDCOMFamilyEvent)
            {
                int ev = GetFamilyEventIndex(evt.Name);
                if (ev == 0) {
                    result = evt.Classification;
                } else {
                    result = (ev > 0) ? LangMan.LS(GKData.FamilyEvents[ev].Name) : evt.Name;
                }
            }

            return result;
        }

        public static string GetAttributeStr(GEDCOMIndividualAttribute iAttr)
        {
            if (iAttr == null)
                throw new ArgumentNullException("iAttr");

            int idx = GetPersonEventIndex(iAttr.Name);
            string st;
            if (idx == 0)
            {
                st = iAttr.Classification;
            }
            else
            {
                st = (idx > 0) ? LangMan.LS(GKData.PersonEvents[idx].Name) : iAttr.Name;
            }

            string place = iAttr.Place.StringValue;
            if (place != "")
            {
                place = " [" + place + "]";
            }
            return st + ": " + iAttr.StringValue + place;
        }

        public static string GetEventDesc(GEDCOMCustomEvent evt, bool hyperLink = true)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            string dt = GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
            string place = evt.Place.StringValue;
            GEDCOMLocationRecord location = evt.Place.Location.Value as GEDCOMLocationRecord;

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

        public static string GetEventCause(GEDCOMCustomEvent evt)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            string result = evt.Cause;

            if (evt.Agency != "")
            {
                if (result != "")
                {
                    result += " ";
                }
                result = result + "[" + evt.Agency + "]";
            }

            return result;
        }

        #endregion

        #region Date functions

        public static string GEDCOMEventToDateStr(GEDCOMCustomEvent evt, DateFormat format, bool sign)
        {
            return (evt == null) ? string.Empty : evt.Date.GetDisplayStringExt(format, sign, false);
        }

        public static string CompactDate(string date)
        {
            string result = date;
            while (result.IndexOf("__.") == 0) result = result.Remove(0, 3);
            return result;
        }

        public static GEDCOMCustomDate GetBirthDate(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
            GEDCOMCustomDate result = ((evt == null) ? null : evt.Date.Value);
            return result;
        }

        public static string GetBirthDate(GEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
        {
            if (iRec == null) return string.Empty;

            GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
            string result = ((evt == null) ? "" : GEDCOMEventToDateStr(evt, dateFormat, false));
            if (compact) result = CompactDate(result);
            return result;
        }

        public static string GetDeathDate(GEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
        {
            if (iRec == null) return string.Empty;

            GEDCOMCustomEvent evt = iRec.FindEvent("DEAT");
            string result = ((evt == null) ? "" : GEDCOMEventToDateStr(evt, dateFormat, false));
            if (compact) result = CompactDate(result);
            return result;
        }

        public static string GetLifeStr(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return string.Empty;

            string result = " (";

            string ds = GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, false);
            if (ds == "")
            {
                ds = "?";
            }
            result += ds;

            ds = GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, false);
            if (ds == "")
            {
                GEDCOMCustomEvent ev = iRec.FindEvent("DEAT");
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

        public static void GetEventDatePlace(GEDCOMIndividualRecord iRec, string eventName, DateFormat dateFormat,
                                             bool compact, bool markUnkDate, out string dateStr, out string placeStr)
        {
            dateStr = string.Empty;
            placeStr = string.Empty;

            if (iRec != null) {
                GEDCOMCustomEvent evt = iRec.FindEvent(eventName);
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

        public static string GetPedigreeLifeStr(GEDCOMIndividualRecord iRec, PedigreeFormat fmt)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result = "";

            switch (fmt) {
                case PedigreeFormat.Excess:
                    {
                        string ds = GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
                        if (ds == "")
                        {
                            ds = "?";
                        }
                        result += ds;
                        ds = GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
                        if (ds == "")
                        {
                            GEDCOMCustomEvent ev = iRec.FindEvent("DEAT");
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

                        GetEventDatePlace(iRec, "BIRT", DateFormat.dfDD_MM_YYYY, true, true, out ds, out ps);
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

                        GetEventDatePlace(iRec, "DEAT", DateFormat.dfDD_MM_YYYY, true, true, out ds, out ps);
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

        public static int GetChronologicalYear(GEDCOMCustomEvent evt)
        {
            return (evt == null) ? 0 : evt.Date.GetChronologicalYear();
        }

        public static int GetEventsYearsDiff(GEDCOMCustomEvent ev1, GEDCOMCustomEvent ev2, bool currentEnd)
        {
            int result = -1;

            try
            {
                int dt1 = GetChronologicalYear(ev1);
                int dt2 = GetChronologicalYear(ev2);

                if (currentEnd && dt2 == 0)
                {
                    dt2 = DateTime.Now.Year;
                }

                if (dt1 != 0 && dt2 != 0)
                {
                    result = Math.Abs(dt2 - dt1);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetEventsYearsDiff(): " + ex.Message);
            }

            return result;
        }

        public static string GetLifeExpectancyStr(GEDCOMIndividualRecord iRec)
        {
            int result = GetLifeExpectancy(iRec);
            return (result == -1) ? "" : result.ToString();
        }

        public static int GetLifeExpectancy(GEDCOMIndividualRecord iRec)
        {
            int result = -1;
            if (iRec == null) return result;

            try
            {
                var lifeDates = iRec.GetLifeDates();
                result = GetEventsYearsDiff(lifeDates.BirthEvent, lifeDates.DeathEvent, false);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetLifeExpectancy(): " + ex.Message);
            }

            return result;
        }

        public static string GetAgeStr(GEDCOMIndividualRecord iRec, int toYear)
        {
            int result = GetAge(iRec, toYear);
            return (result == -1) ? "" : result.ToString();
        }

        public static int GetAge(GEDCOMIndividualRecord iRec, int toYear)
        {
            int result = -1;
            if (iRec == null) return result;

            try
            {
                var lifeDates = iRec.GetLifeDates();

                if (toYear == -1) {
                    result = GetEventsYearsDiff(lifeDates.BirthEvent, lifeDates.DeathEvent, lifeDates.DeathEvent == null);
                } else {
                    int birthYear = GetChronologicalYear(lifeDates.BirthEvent);
                    if (birthYear != 0) {
                        result = toYear - birthYear;
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetAge(): " + ex.Message);
            }

            return result;
        }

        public static GEDCOMCustomDate GetMarriageDate(GEDCOMFamilyRecord fRec)
        {
            if (fRec == null) {
                return null;
            }

            GEDCOMCustomEvent evt = fRec.FindEvent("MARR");
            return ((evt == null) ? null : evt.Date.Value);
        }

        public static string GetMarriageDateStr(GEDCOMFamilyRecord fRec, DateFormat dateFormat)
        {
            GEDCOMCustomDate date = GetMarriageDate(fRec);
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
        public static int GetDaysForBirth(GEDCOMIndividualRecord iRec)
        {
            int distance = -1;

            if (iRec != null) {
                int bdD, bdM, bdY;

                try {
                    GEDCOMCustomEvent evt = iRec.FindEvent("DEAT");
                    if (evt == null) {
                        evt = iRec.FindEvent("BIRT");
                        if (evt != null) {
                            var dt = evt.Date.Value as GEDCOMDate;
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
                    Logger.LogWrite("GKUtils.GetDaysForBirth(): " + ex.Message);
                }
            }

            return distance;
        }

        #endregion

        #region Places functions

        public static string GetBirthPlace(GEDCOMIndividualRecord iRec)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent("BIRT"), false);
        }

        public static string GetDeathPlace(GEDCOMIndividualRecord iRec)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent("DEAT"), false);
        }

        public static string GetResidencePlace(GEDCOMIndividualRecord iRec, bool includeAddress)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent("RESI"), includeAddress);
        }

        public static string GetPlaceStr(GEDCOMCustomEvent evt, bool includeAddress)
        {
            if (evt == null) return string.Empty;

            string result = evt.Place.StringValue;

            if (includeAddress)
            {
                string resi = evt.StringValue;
                string addr = evt.Address.Address.Text.Trim();
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

        public static int GetAncestorsCount(GEDCOMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null) {
                int val = (int)iRec.ExtData;

                if (val < 0) {
                    val = 1;

                    GEDCOMFamilyRecord family = iRec.GetParentsFamily();
                    if (family != null) {
                        GEDCOMIndividualRecord anc;

                        anc = family.GetHusband();
                        val += GetAncestorsCount(anc);

                        anc = family.GetWife();
                        val += GetAncestorsCount(anc);
                    }

                    iRec.ExtData = val;
                }

                result = val;
            }

            return result;
        }

        public static int GetDescendantsCount(GEDCOMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null)
            {
                int val = (int)iRec.ExtData;
                if (val < 0)
                {
                    val = 1;

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                        int num2 = family.Children.Count;
                        for (int j = 0; j < num2; j++)
                        {
                            GEDCOMIndividualRecord iChild = family.Children[j].Value as GEDCOMIndividualRecord;
                            val += GetDescendantsCount(iChild);
                        }
                    }
                    iRec.ExtData = val;
                }
                result = val;
            }

            return result;
        }

        private static int GetDescGens_Recursive(GEDCOMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null)
            {
                int max = 0;

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord iChild = family.Children[j].Value as GEDCOMIndividualRecord;
                        int res = GetDescGens_Recursive(iChild);
                        if (max < res)
                        {
                            max = res;
                        }
                    }
                }
                result = 1 + max;
            }

            return result;
        }

        public static int GetDescGenerations(GEDCOMIndividualRecord iRec)
        {
            return GetDescGens_Recursive(iRec) - 1;
        }

        public static int GetMarriagesCount(GEDCOMIndividualRecord iRec)
        {
            int result = ((iRec == null) ? 0 : iRec.SpouseToFamilyLinks.Count);
            return result;
        }

        public static int GetSpousesDiff(GEDCOMFamilyRecord fRec)
        {
            int result = -1;

            try
            {
                if (fRec != null) {
                    GEDCOMIndividualRecord husb = fRec.GetHusband();
                    GEDCOMIndividualRecord wife = fRec.GetWife();

                    if (husb != null && wife != null)
                    {
                        GEDCOMCustomEvent evH = husb.FindEvent("BIRT");
                        GEDCOMCustomEvent evW = wife.FindEvent("BIRT");

                        result = GetEventsYearsDiff(evH, evW, false);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetSpousesDiff(): " + ex.Message);
            }

            return result;
        }

        public static GEDCOMIndividualRecord GetFirstborn(GEDCOMIndividualRecord iRec)
        {
            GEDCOMIndividualRecord iChild = null;
            if (iRec == null) return iChild;

            try
            {
                int firstYear = 0;

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Children[j].Value as GEDCOMIndividualRecord;
                        if (child == null) continue;

                        GEDCOMCustomEvent evt = child.FindEvent("BIRT");
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
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetFirstborn(): " + ex.Message);
            }
            return iChild;
        }

        public static int GetFirstbornAge(GEDCOMIndividualRecord iRec, GEDCOMIndividualRecord iChild)
        {
            int result = 0;
            if (iRec == null || iChild == null) return result;

            try
            {
                GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
                if (evt == null) return result;
                int parentYear = evt.GetChronologicalYear();

                evt = iChild.FindEvent("BIRT");
                if (evt == null) return result;
                int childYear = evt.GetChronologicalYear();

                if (parentYear != 0 && childYear != 0) {
                    result = (childYear - parentYear);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetFirstbornAge(): " + ex.Message);
            }
            return result;
        }

        public static int GetMarriageAge(GEDCOMIndividualRecord iRec)
        {
            int result = 0;
            if (iRec == null) return result;

            try
            {
                int firstYear = 0;

                GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
                if (evt != null)
                {
                    int mainYear = evt.GetChronologicalYear();

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                        GEDCOMCustomEvent marrEvt = family.FindEvent("MARR");
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
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetMarriageAge(): " + ex.Message);
            }
            return result;
        }

        #endregion

        #region Tree utils

        public static void InitExtData(GEDCOMTree tree)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = tree[i];
                rec.ExtData = null;
            }
        }

        public static void InitExtCounts(GEDCOMTree tree, int value)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = tree[i];

                if (rec is GEDCOMIndividualRecord) {
                    rec.ExtData = value;
                }
            }
        }

        public static void PrepareHeader(GEDCOMTree tree, string fileName, GEDCOMCharacterSet charSet, bool zeroRev)
        {
            GEDCOMHeader header = tree.Header;

            string subm = header.GetTagStringValue(GEDCOMTagType.SUBM);
            int oldRev = header.FileRevision;
            GEDCOMLanguageID langId = header.Language.Value;

            header.Clear();
            header.Source = "GEDKeeper";
            header.ReceivingSystemName = "GEDKeeper";
            header.CharacterSet = charSet;
            header.Language.Value = langId;
            header.GEDCOMVersion = "5.5.1";
            header.GEDCOMForm = "LINEAGE-LINKED";
            header.FileName = Path.GetFileName(fileName);
            header.TransmissionDateTime = DateTime.Now;

            header.SourceVersion = GKData.APP_FORMAT_CURVER.ToString();

            if (zeroRev) {
                header.FileRevision = 0;
            } else {
                header.FileRevision = oldRev + 1;
            }

            if (subm != "") {
                header.SetTagStringValue(GEDCOMTagType.SUBM, subm);
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

        #endregion

        #region Show information summary

        private static void ShowAddressSummary(GEDCOMAddress address, StringList summary)
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
                if (address.Address.Text.Trim() != "")
                {
                    ts += address.Address.Text.Trim();
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

        private static void ShowDetailCause(GEDCOMCustomEvent evt, StringList summary)
        {
            string cause = GetEventCause(evt);
            if (summary != null && cause != "")
            {
                summary.Add("    " + cause);
            }
        }

        private static void ShowEventDetailInfo(GEDCOMCustomEvent eventDetail, StringList summary)
        {
            if (eventDetail == null)
                throw new ArgumentNullException("eventDetail");

            if (summary != null && eventDetail.SourceCitations.Count != 0)
            {
                summary.Add("    " + LangMan.LS(LSID.LSID_RPSources) + " (" + eventDetail.SourceCitations.Count.ToString() + "):");

                int num = eventDetail.SourceCitations.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMSourceCitation cit = eventDetail.SourceCitations[i];
                    GEDCOMSourceRecord sourceRec = cit.Value as GEDCOMSourceRecord;
                    if (sourceRec == null) continue;

                    string nm = "\"" + sourceRec.FiledByEntry + "\"";
                    if (cit.Page != "")
                    {
                        nm = nm + ", " + cit.Page;
                    }
                    summary.Add("      " + HyperLink(sourceRec.XRef, nm, 0));
                }
            }
        }

        private static void ShowEvent(GEDCOMRecord subj, StringList aToList, GEDCOMRecord aRec, GEDCOMCustomEvent evt)
        {
            if (subj is GEDCOMNoteRecord) {
                int num = evt.Notes.Count;
                for (int i = 0; i < num; i++) {
                    if (evt.Notes[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else
                if (subj is GEDCOMMultimediaRecord) {
                int num2 = evt.MultimediaLinks.Count;
                for (int i = 0; i < num2; i++) {
                    if (evt.MultimediaLinks[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else if (subj is GEDCOMSourceRecord) {
                int num3 = evt.SourceCitations.Count;
                for (int i = 0; i < num3; i++) {
                    if (evt.SourceCitations[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, evt.SourceCitations[i]);
                    }
                }
            }
        }

        private static void ShowLink(GEDCOMRecord aSubject, StringList aToList, GEDCOMRecord aRec, GEDCOMTag aTag, GEDCOMPointer aExt)
        {
            string prefix;
            if (aSubject is GEDCOMSourceRecord && aExt != null) {
                GEDCOMSourceCitation cit = (aExt as GEDCOMSourceCitation);
                if (cit != null && cit.Page != "") {
                    prefix = cit.Page + ": ";
                } else {
                    prefix = "";
                }
            } else {
                prefix = "";
            }

            string suffix;
            if (aTag is GEDCOMCustomEvent) {
                suffix = ", " + GetEventName((GEDCOMCustomEvent) aTag).ToLower();
            } else {
                suffix = "";
            }
            aToList.Add("    " + prefix + GenRecordLink(aRec, true) + suffix);
        }

        private static void ShowPersonExtInfo(GEDCOMTree tree, GEDCOMIndividualRecord iRec, StringList summary)
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

        private static void ShowPersonNamesakes(GEDCOMTree tree, GEDCOMIndividualRecord iRec, StringList summary)
        {
            try {
                StringList namesakes = new StringList();
                try {
                    string st = GetNameString(iRec, true, false);

                    int num3 = tree.RecordsCount;
                    for (int i = 0; i < num3; i++) {
                        GEDCOMRecord rec = tree[i];

                        if (rec is GEDCOMIndividualRecord && rec != iRec)
                        {
                            GEDCOMIndividualRecord relPerson = (GEDCOMIndividualRecord) rec;

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
                        for (int i = 0; i < num4; i++)
                        {
                            GEDCOMIndividualRecord relPerson = (GEDCOMIndividualRecord)namesakes.GetObject(i);
                            
                            summary.Add("    " + HyperLink(relPerson.XRef, namesakes[i], 0));
                        }
                    }
                }
                finally {
                    namesakes.Dispose();
                }
            }
            catch (Exception ex) {
                Logger.LogWrite("GKUtils.ShowPersonNamesakes(): " + ex.Message);
            }
        }

        private static void ShowSubjectLinks(GEDCOMRecord aInRecord, GEDCOMRecord subject, StringList aToList)
        {
            try
            {
                int num;

                if (subject is GEDCOMNoteRecord) {
                    num = aInRecord.Notes.Count;
                    for (int i = 0; i < num; i++) {
                        if (aInRecord.Notes[i].Value == subject) {
                            ShowLink(subject, aToList, aInRecord, null, null);
                        }
                    }
                } else if (subject is GEDCOMMultimediaRecord) {
                    num = aInRecord.MultimediaLinks.Count;
                    for (int i = 0; i < num; i++) {
                        if (aInRecord.MultimediaLinks[i].Value == subject) {
                            ShowLink(subject, aToList, aInRecord, null, null);
                        }
                    }
                } else if (subject is GEDCOMSourceRecord) {
                    num = aInRecord.SourceCitations.Count;
                    for (int i = 0; i < num; i++) {
                        if (aInRecord.SourceCitations[i].Value == subject) {
                            ShowLink(subject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
                        }
                    }
                }

                var recordWithEvents = aInRecord as GEDCOMRecordWithEvents;
                if (recordWithEvents != null) {
                    GEDCOMRecordWithEvents evsRec = recordWithEvents;

                    num = evsRec.Events.Count;
                    for (int i = 0; i < num; i++) {
                        ShowEvent(subject, aToList, evsRec, evsRec.Events[i]);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowSubjectLinks(): " + ex.Message);
            }
        }

        public static void SearchRecordLinks(List<GEDCOMObject> linksList, GEDCOMRecord inRecord, GEDCOMRecord searchRec)
        {
            try {
                int num;
                switch (searchRec.RecordType) {
                    case GEDCOMRecordType.rtNote:
                        num = inRecord.Notes.Count;
                        for (int i = 0; i < num; i++) {
                            var notes = inRecord.Notes[i];
                            if (notes.Value == searchRec) {
                                linksList.Add(notes);
                            }
                        }
                        break;

                    case GEDCOMRecordType.rtMultimedia:
                        num = inRecord.MultimediaLinks.Count;
                        for (int i = 0; i < num; i++) {
                            var mmLink = inRecord.MultimediaLinks[i];
                            if (mmLink.Value == searchRec) {
                                linksList.Add(mmLink);
                            }
                        }
                        break;

                    case GEDCOMRecordType.rtSource:
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
                Logger.LogWrite("GKUtils.SearchRecordLinks(): " + ex.Message);
            }
        }

        public static void SearchRecordLinks(List<GEDCOMObject> linksList, GEDCOMTree tree, GEDCOMRecord searchRec)
        {
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                SearchRecordLinks(linksList, tree[i], searchRec);
            }
        }

        private static void RecListMediaRefresh(GEDCOMRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.MultimediaLinks.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPMultimedia) + " (" + record.MultimediaLinks.Count.ToString() + "):");

                    int num = record.MultimediaLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMMultimediaLink mmLink = record.MultimediaLinks[i];
                        GEDCOMMultimediaRecord mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                        if (mmRec == null || mmRec.FileReferences.Count == 0) continue;

                        string st = mmRec.FileReferences[0].Title;
                        summary.Add("  " + HyperLink(mmRec.XRef, st, 0) + " (" +
                                    HyperLink("view_" + mmRec.XRef, LangMan.LS(LSID.LSID_MediaView), 0) + ")");
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListMediaRefresh(): " + ex.Message);
            }
        }

        private static void RecListNotesRefresh(GEDCOMRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.Notes.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPNotes) + " (" + record.Notes.Count.ToString() + "):");

                    int num = record.Notes.Count;
                    for (int i = 0; i < num; i++)
                    {
                        if (i > 0) {
                            summary.Add("");
                        }

                        GEDCOMNotes note = record.Notes[i];

                        int num2 = note.Notes.Count;
                        for (int k = 0; k < num2; k++)
                        {
                            string st = note.Notes[k];
                            summary.Add(st);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListNotesRefresh(): " + ex.Message);
            }
        }

        private static void RecListSourcesRefresh(GEDCOMRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.SourceCitations.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPSources) + " (" + record.SourceCitations.Count.ToString() + "):");

                    int num = record.SourceCitations.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMSourceCitation cit = record.SourceCitations[i];
                        GEDCOMSourceRecord sourceRec = cit.Value as GEDCOMSourceRecord;
                        if (sourceRec == null) continue;

                        string nm = "\"" + sourceRec.FiledByEntry + "\"";

                        if (cit.Page != "") {
                            nm = nm + ", " + cit.Page;
                        }

                        summary.Add("  " + HyperLink(sourceRec.XRef, nm, 0));
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListSourcesRefresh(): " + ex.Message);
            }
        }

        private static void RecListAssociationsRefresh(GEDCOMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.Associations.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Associations) + ":");

                    int num = record.Associations.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMAssociation ast = record.Associations[i];
                        string nm = ((ast.Individual == null) ? "" : GetNameString(ast.Individual, true, false));
                        string xref = ((ast.Individual == null) ? "" : ast.Individual.XRef);

                        summary.Add("    " + ast.Relation + " " + HyperLink(xref, nm, 0));
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListAssociationsRefresh(): " + ex.Message);
            }
        }

        private static void RecListIndividualEventsRefresh(GEDCOMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.Events.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Events) + ":");

                    int num = record.Events.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMCustomEvent evt = record.Events[i];
                        string st = GetEventName(evt);

                        string sv = "";
                        if (evt.StringValue != "")
                        {
                            sv = evt.StringValue + ", ";
                        }
                        summary.Add(st + ": " + sv + GetEventDesc(evt));

                        ShowDetailCause(evt, summary);
                        ShowAddressSummary(evt.Address, summary);
                        ShowEventDetailInfo(evt, summary);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListIndividualEventsRefresh(): " + ex.Message);
            }
        }

        private static void RecListFamilyEventsRefresh(GEDCOMFamilyRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.Events.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Events) + ":");

                    int num = record.Events.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyEvent evt = (GEDCOMFamilyEvent)record.Events[i];

                        string st = GetEventName(evt);
                        summary.Add(st + ": " + GetEventDesc(evt));

                        ShowDetailCause(evt, summary);
                        ShowEventDetailInfo(evt, summary);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListFamilyEventsRefresh(): " + ex.Message);
            }
        }

        private static void RecListGroupsRefresh(GEDCOMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try
            {
                if (record.Groups.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_RPGroups) + ":");

                    int num = record.Groups.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMPointer ptr = record.Groups[i];
                        GEDCOMGroupRecord grp = ptr.Value as GEDCOMGroupRecord;
                        if (grp == null) continue;

                        summary.Add("    " + HyperLink(grp.XRef, grp.GroupName, 0));
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListGroupsRefresh(): " + ex.Message);
            }
        }

        //

        public static void ShowFamilyInfo(IBaseContext baseContext, GEDCOMFamilyRecord familyRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (familyRec != null)
                    {
                        summary.Add("");

                        GEDCOMIndividualRecord irec = familyRec.GetHusband();
                        string st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkMale) : HyperLink(irec.XRef, GetNameString(irec, true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Husband) + ": " + st + GetLifeStr(irec));

                        irec = familyRec.GetWife();
                        st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkFemale) : HyperLink(irec.XRef, GetNameString(irec, true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Wife) + ": " + st + GetLifeStr(irec));

                        summary.Add("");
                        if (familyRec.Children.Count != 0)
                        {
                            summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                        }

                        int num = familyRec.Children.Count;
                        for (int i = 0; i < num; i++)
                        {
                            irec = (GEDCOMIndividualRecord)familyRec.Children[i].Value;
                            
                            summary.Add("    " + HyperLink(irec.XRef, GetNameString(irec, true, false), 0) + GetLifeStr(irec));
                        }
                        summary.Add("");

                        RecListFamilyEventsRefresh(familyRec, summary);
                        RecListNotesRefresh(familyRec, summary);
                        RecListMediaRefresh(familyRec, summary);
                        RecListSourcesRefresh(familyRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowFamilyInfo(): " + ex.Message);
            }
        }
        
        public static void ShowGroupInfo(GEDCOMGroupRecord groupRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                StringList mbrList = new StringList();
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (groupRec != null)
                    {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + groupRec.GroupName + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Members) + " (" + groupRec.Members.Count.ToString() + "):");

                        int num = groupRec.Members.Count;
                        for (int i = 0; i < num; i++)
                        {
                            GEDCOMPointer ptr = groupRec.Members[i];
                            GEDCOMIndividualRecord member = (GEDCOMIndividualRecord)ptr.Value;
                            
                            mbrList.AddObject(GetNameString(member, true, false), member);
                        }
                        mbrList.Sort();

                        int num2 = mbrList.Count;
                        for (int i = 0; i < num2; i++)
                        {
                            GEDCOMIndividualRecord member = (GEDCOMIndividualRecord)mbrList.GetObject(i);
                            
                            summary.Add("    " + HyperLink(member.XRef, mbrList[i], i + 1));
                        }

                        RecListNotesRefresh(groupRec, summary);
                        RecListMediaRefresh(groupRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                    mbrList.Dispose();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowGroupInfo(): " + ex.Message);
            }
        }

        public static void ShowMultimediaInfo(GEDCOMMultimediaRecord mediaRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (mediaRec != null)
                    {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + mediaRec.FileReferences[0].Title + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add("[ " + HyperLink("view_" + mediaRec.XRef, LangMan.LS(LSID.LSID_View), 0) + " ]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = mediaRec.Owner;
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++)
                        {
                            ShowSubjectLinks(tree[i], mediaRec, summary);
                        }

                        RecListNotesRefresh(mediaRec, summary);
                        RecListSourcesRefresh(mediaRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowMultimediaInfo(): " + ex.Message);
            }
        }

        public static void ShowNoteInfo(GEDCOMNoteRecord noteRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (noteRec != null)
                    {
                        summary.Add("");
                        summary.AddStrings(noteRec.Note);
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = noteRec.Owner;
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++)
                        {
                            ShowSubjectLinks(tree[i], noteRec, summary);
                        }
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowNoteInfo(): " + ex.Message);
            }
        }

        public static void ShowPersonInfo(IBaseContext baseContext, GEDCOMIndividualRecord iRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                summary.Clear();
                try
                {
                    if (iRec != null)
                    {
                        GEDCOMTree tree = iRec.Owner;

                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + GetNameString(iRec, true, true) + "[/size][/u][/b]");
                        summary.Add(LangMan.LS(LSID.LSID_Sex) + ": " + SexStr(iRec.Sex));
                        try
                        {
                            GEDCOMIndividualRecord father, mother;
                            GEDCOMFamilyRecord fam = iRec.GetParentsFamily();
                            if (fam == null) {
                                father = null;
                                mother = null;
                            } else {
                                father = fam.GetHusband();
                                mother = fam.GetWife();
                            }

                            if (father != null || mother != null)
                            {
                                summary.Add("");
                                summary.Add(LangMan.LS(LSID.LSID_Parents) + ":");

                                string st;

                                st = (father == null) ? LangMan.LS(LSID.LSID_UnkMale) : HyperLink(father.XRef, GetNameString(father, true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Father) + ": " + st + GetLifeStr(father));

                                st = (mother == null) ? LangMan.LS(LSID.LSID_UnkFemale) : HyperLink(mother.XRef, GetNameString(mother, true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Mother) + ": " + st + GetLifeStr(mother));
                            }
                        }
                        catch (Exception ex)
                        {
                            Logger.LogWrite("GKUtils.ShowPersonInfo().Parents(): " + ex.Message);
                        }

                        try
                        {
                            int num = iRec.SpouseToFamilyLinks.Count;
                            for (int i = 0; i < num; i++)
                            {
                                GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
                                if (family == null) continue;
                                if (!baseContext.IsRecordAccess(family.Restriction)) continue;

                                string st;
                                GEDCOMPointer sp;
                                string unk;
                                if (iRec.Sex == GEDCOMSex.svMale)
                                {
                                    sp = family.Wife;
                                    st = LangMan.LS(LSID.LSID_Wife) + ": ";
                                    unk = LangMan.LS(LSID.LSID_UnkFemale);
                                }
                                else
                                {
                                    sp = family.Husband;
                                    st = LangMan.LS(LSID.LSID_Husband) + ": ";
                                    unk = LangMan.LS(LSID.LSID_UnkMale);
                                }
                                string marr = GetMarriageDateStr(family, DateFormat.dfDD_MM_YYYY);
                                if (marr != "")
                                {
                                    marr = LangMan.LS(LSID.LSID_LMarriage) + " " + marr;
                                }
                                else
                                {
                                    marr = LangMan.LS(LSID.LSID_LFamily);
                                }

                                GEDCOMIndividualRecord relPerson = sp.Value as GEDCOMIndividualRecord;
                                
                                summary.Add("");
                                if (relPerson != null)
                                {
                                    st = st + HyperLink(relPerson.XRef, GetNameString(relPerson, true, false), 0) + " (" + HyperLink(family.XRef, marr, 0) + ")";
                                }
                                else
                                {
                                    st = st + unk + " (" + HyperLink(family.XRef, marr, 0) + ")";
                                }
                                summary.Add(st);
                                
                                if (family.Children.Count != 0)
                                {
                                    summary.Add("");
                                    summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                                }

                                int num2 = family.Children.Count;
                                for (int k = 0; k < num2; k++)
                                {
                                    relPerson = (GEDCOMIndividualRecord)family.Children[k].Value;
                                    
                                    summary.Add("    " + HyperLink(relPerson.XRef, GetNameString(relPerson, true, false), 0) + GetLifeStr(relPerson));
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            Logger.LogWrite("GKUtils.ShowPersonInfo().Families(): " + ex.Message);
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
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowPersonInfo(): " + ex.Message);
            }
        }

        public static void ShowSourceInfo(GEDCOMSourceRecord sourceRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                StringList linkList = new StringList();
                try
                {
                    summary.Clear();
                    if (sourceRec != null)
                    {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + sourceRec.FiledByEntry + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Author) + ": " + sourceRec.Originator.Text.Trim());
                        summary.Add(LangMan.LS(LSID.LSID_Title) + ": \"" + sourceRec.Title.Text.Trim() + "\"");
                        summary.Add(LangMan.LS(LSID.LSID_Publication) + ": \"" + sourceRec.Publication.Text.Trim() + "\"");

                        if (sourceRec.RepositoryCitations.Count > 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPRepositories) + ":");

                            int num = sourceRec.RepositoryCitations.Count;
                            for (int i = 0; i < num; i++)
                            {
                                GEDCOMRepositoryRecord rep = (GEDCOMRepositoryRecord)sourceRec.RepositoryCitations[i].Value;

                                summary.Add("    " + HyperLink(rep.XRef, rep.RepositoryName, 0));
                            }
                        }

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = sourceRec.Owner;

                        int num2 = tree.RecordsCount;
                        for (int j = 0; j < num2; j++)
                        {
                            ShowSubjectLinks(tree[j], sourceRec, linkList);
                        }

                        linkList.Sort();

                        int num3 = linkList.Count;
                        for (int j = 0; j < num3; j++)
                        {
                            summary.Add(linkList[j]);
                        }

                        RecListNotesRefresh(sourceRec, summary);
                        RecListMediaRefresh(sourceRec, summary);
                    }
                }
                finally
                {
                    linkList.Dispose();
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowSourceInfo(): " + ex.Message);
            }
        }

        public static void ShowRepositoryInfo(GEDCOMRepositoryRecord repositoryRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (repositoryRec != null)
                    {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + repositoryRec.RepositoryName.Trim() + "[/size][/b][/u]");
                        summary.Add("");

                        ShowAddressSummary(repositoryRec.Address, summary);

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_RPSources) + ":");

                        GEDCOMTree tree = repositoryRec.Owner;

                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++)
                        {
                            GEDCOMRecord rec = tree[i];

                            if (rec.RecordType == GEDCOMRecordType.rtSource)
                            {
                                GEDCOMSourceRecord srcRec = (GEDCOMSourceRecord) rec;

                                int num2 = srcRec.RepositoryCitations.Count;
                                for (int j = 0; j < num2; j++)
                                {
                                    if (srcRec.RepositoryCitations[j].Value == repositoryRec)
                                    {
                                        summary.Add("    " + GenRecordLink(srcRec, false));
                                    }
                                }
                            }
                        }

                        RecListNotesRefresh(repositoryRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowRepositoryInfo(): " + ex.Message);
            }
        }

        public static void ShowResearchInfo(GEDCOMResearchRecord researchRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (researchRec != null)
                    {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Title) + ": [u][b][size=+1]\"" + researchRec.ResearchName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)researchRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_Status) + ": " + LangMan.LS(GKData.StatusNames[(int)researchRec.Status]) + " (" + researchRec.Percent.ToString() + "%)");
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + researchRec.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + researchRec.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY));

                        if (researchRec.Tasks.Count > 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPTasks) + ":");

                            int num = researchRec.Tasks.Count;
                            for (int i = 0; i < num; i++)
                            {
                                GEDCOMTaskRecord taskRec = researchRec.Tasks[i].Value as GEDCOMTaskRecord;
                                summary.Add("    " + GenRecordLink(taskRec, false));
                            }
                        }

                        if (researchRec.Communications.Count > 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPCommunications) + ":");

                            int num2 = researchRec.Communications.Count;
                            for (int i = 0; i < num2; i++)
                            {
                                GEDCOMCommunicationRecord corrRec = researchRec.Communications[i].Value as GEDCOMCommunicationRecord;
                                summary.Add("    " + GenRecordLink(corrRec, false));
                            }
                        }

                        if (researchRec.Groups.Count != 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPGroups) + ":");

                            int num3 = researchRec.Groups.Count;
                            for (int i = 0; i < num3; i++)
                            {
                                GEDCOMGroupRecord grp = (GEDCOMGroupRecord)researchRec.Groups[i].Value;

                                summary.Add("    " + HyperLink(grp.XRef, grp.GroupName, 0));
                            }
                        }

                        RecListNotesRefresh(researchRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowResearchInfo(): " + ex.Message);
            }
        }

        public static void ShowTaskInfo(GEDCOMTaskRecord taskRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (taskRec != null)
                    {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Goal) + ": [u][b][size=+1]" + GetTaskGoalStr(taskRec) + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + taskRec.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + taskRec.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY));

                        RecListNotesRefresh(taskRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowTaskInfo(): " + ex.Message);
            }
        }

        public static void ShowCommunicationInfo(GEDCOMCommunicationRecord commRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                try
                {
                    summary.Clear();
                    if (commRec != null)
                    {
                        GEDCOMTree tree = commRec.Owner;

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Theme) + ": [u][b][size=+1]\"" + commRec.CommName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Corresponder) + ": " + GetCorresponderStr(tree, commRec, true));
                        summary.Add(LangMan.LS(LSID.LSID_Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]));
                        summary.Add(LangMan.LS(LSID.LSID_Date) + ": " + commRec.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY));

                        RecListNotesRefresh(commRec, summary);
                        RecListMediaRefresh(commRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowCommunicationInfo(): " + ex.Message);
            }
        }

        public static void ShowLocationInfo(GEDCOMLocationRecord locRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                StringList linkList = null;
                try
                {
                    summary.Clear();
                    if (locRec != null)
                    {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + locRec.LocationName.Trim() + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Latitude) + ": " + locRec.Map.Lati);
                        summary.Add(LangMan.LS(LSID.LSID_Longitude) + ": " + locRec.Map.Long);

                        GEDCOMTree tree = locRec.Owner;

                        linkList = GetLocationLinks(tree, locRec);

                        if (linkList.Count > 0)
                        {
                            linkList.Sort();

                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                            int num = linkList.Count;
                            for (int i = 0; i < num; i++)
                            {
                                GEDCOMRecord rec = linkList.GetObject(i) as GEDCOMRecord;
                                summary.Add("    " + HyperLink(rec.XRef, linkList[i], 0));
                            }
                        }

                        RecListNotesRefresh(locRec, summary);
                        RecListMediaRefresh(locRec, summary);
                    }
                }
                finally
                {
                    if (linkList != null) linkList.Dispose();
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.ShowLocationInfo(): " + ex.Message);
            }
        }

        public static void GetRecordContent(IBaseContext baseContext, GEDCOMRecord record, StringList ctx)
        {
            if (record != null && ctx != null)
            {
                try
                {
                    switch (record.RecordType)
                    {
                        case GEDCOMRecordType.rtIndividual:
                            GKUtils.ShowPersonInfo(baseContext, record as GEDCOMIndividualRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtFamily:
                            GKUtils.ShowFamilyInfo(baseContext, record as GEDCOMFamilyRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtNote:
                            GKUtils.ShowNoteInfo(record as GEDCOMNoteRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtMultimedia:
                            GKUtils.ShowMultimediaInfo(record as GEDCOMMultimediaRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtSource:
                            GKUtils.ShowSourceInfo(record as GEDCOMSourceRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtRepository:
                            GKUtils.ShowRepositoryInfo(record as GEDCOMRepositoryRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtGroup:
                            GKUtils.ShowGroupInfo(record as GEDCOMGroupRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtResearch:
                            GKUtils.ShowResearchInfo(record as GEDCOMResearchRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtTask:
                            GKUtils.ShowTaskInfo(record as GEDCOMTaskRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtCommunication:
                            GKUtils.ShowCommunicationInfo(record as GEDCOMCommunicationRecord, ctx);
                            break;

                        case GEDCOMRecordType.rtLocation:
                            GKUtils.ShowLocationInfo(record as GEDCOMLocationRecord, ctx);
                            break;
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogWrite("GKUtils.GetRecordContext(): " + ex.Message);
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

        public static MultimediaKind GetMultimediaKind(GEDCOMMultimediaFormat format)
        {
            switch (format)
            {
                case GEDCOMMultimediaFormat.mfNone:
                    return MultimediaKind.mkNone;

                case GEDCOMMultimediaFormat.mfBMP:
                case GEDCOMMultimediaFormat.mfGIF:
                case GEDCOMMultimediaFormat.mfJPG:
                case GEDCOMMultimediaFormat.mfPCX:
                case GEDCOMMultimediaFormat.mfTIF:
                case GEDCOMMultimediaFormat.mfTGA:
                case GEDCOMMultimediaFormat.mfPNG:
                case GEDCOMMultimediaFormat.mfRAW:
                case GEDCOMMultimediaFormat.mfPSD:
                    return MultimediaKind.mkImage;

                case GEDCOMMultimediaFormat.mfTXT:
                case GEDCOMMultimediaFormat.mfRTF:
                case GEDCOMMultimediaFormat.mfHTM:
                case GEDCOMMultimediaFormat.mfPDF:
                    return MultimediaKind.mkText;

                case GEDCOMMultimediaFormat.mfWAV:
                case GEDCOMMultimediaFormat.mfMP3:
                case GEDCOMMultimediaFormat.mfWMA:
                case GEDCOMMultimediaFormat.mfMKA:
                    return MultimediaKind.mkAudio;

                case GEDCOMMultimediaFormat.mfAVI:
                case GEDCOMMultimediaFormat.mfMPG:
                case GEDCOMMultimediaFormat.mfMP4:
                case GEDCOMMultimediaFormat.mfOGV:
                case GEDCOMMultimediaFormat.mfWMV:
                case GEDCOMMultimediaFormat.mfMKV:
                case GEDCOMMultimediaFormat.mfMOV:
                    return MultimediaKind.mkVideo;

                case GEDCOMMultimediaFormat.mfOLE:
                case GEDCOMMultimediaFormat.mfUnknown:
                default:
                    return MultimediaKind.mkNone;
            }
        }

        public static MediaStoreType GetStoreTypeEx(GEDCOMFileReference fileReference)
        {
            if (fileReference == null)
                throw new ArgumentNullException("fileReference");

            string fileRef = fileReference.StringValue;

            MediaStoreType result;
            if (fileRef.IndexOf(GKData.GKStoreTypes[2].Sign) == 0) {
                result = MediaStoreType.mstArchive;
            } else if (fileRef.IndexOf(GKData.GKStoreTypes[1].Sign) == 0) {
                result = MediaStoreType.mstStorage;
            } else {
                result = MediaStoreType.mstReference;
            }
            return result;
        }

        public static MediaStore GetStoreType(GEDCOMFileReference fileReference)
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
            GEDCOMMultimediaFormat fileFmt = GEDCOMFileReference.RecognizeFormat(fileName);

            FileInfo info = new FileInfo(fileName);
            double fileSize = (((double)info.Length / 1024) / 1024); // mb

            MultimediaKind mKind = GetMultimediaKind(fileFmt);
            return ((mKind == MultimediaKind.mkImage || mKind == MultimediaKind.mkText) && fileSize <= 10);
        }

        #endregion

        #region Names processing

        public static string GetFamilyString(GEDCOMFamilyRecord family)
        {
            if (family == null)
                throw new ArgumentNullException("family");

            return GetFamilyString(family, LangMan.LS(LSID.LSID_UnkMale), LangMan.LS(LSID.LSID_UnkFemale));
        }

        public static string GetFamilyString(GEDCOMFamilyRecord family, string unkHusband, string unkWife)
        {
            if (family == null)
                throw new ArgumentNullException("family");

            string result = "";

            GEDCOMIndividualRecord spouse = family.GetHusband();
            if (spouse == null)
            {
                if (unkHusband == null) unkHusband = "?";
                result += unkHusband;
            }
            else
            {
                result += GetNameString(spouse, true, false);
            }

            result += " - ";

            spouse = family.GetWife();
            if (spouse == null)
            {
                if (unkWife == null) unkWife = "?";
                result += unkWife;
            }
            else
            {
                result += GetNameString(spouse, true, false);
            }

            return result;
        }

        public static string GetNickString(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result;
            if (iRec.PersonalNames.Count > 0)
            {
                GEDCOMPersonalName np = iRec.PersonalNames[0];
                result = np.Pieces.Nickname;
            }
            else
            {
                result = "";
            }
            return result;
        }

        // TODO: what if you want to display only a surname that is missing?
        private static string GetFmtSurname(GEDCOMIndividualRecord iRec, GEDCOMPersonalName personalName, string defSurname)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (personalName == null)
                throw new ArgumentNullException("personalName");

            string result;

            WomanSurnameFormat wsFmt = GlobalOptions.Instance.WomanSurnameFormat;
            if (iRec.Sex == GEDCOMSex.svFemale && wsFmt != WomanSurnameFormat.wsfNotExtend) {
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

        public static string GetNameString(GEDCOMIndividualRecord iRec, GEDCOMPersonalName np, bool firstSurname, bool includePieces)
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

        public static string GetNameString(GEDCOMIndividualRecord iRec, bool firstSurname, bool includePieces)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result;
            if (iRec.PersonalNames.Count > 0) {
                GEDCOMPersonalName np = iRec.PersonalNames[0];
                result = GetNameString(iRec, np, firstSurname, includePieces);
            } else {
                result = "";
            }
            return result;
        }

        public static void SetMarriedSurname(GEDCOMIndividualRecord iRec, string marriedSurname)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GEDCOMPersonalName personalName;
            if (iRec.PersonalNames.Count <= 0) {
                personalName = iRec.AddPersonalName(new GEDCOMPersonalName(iRec.Owner, iRec, "", ""));
            } else {
                personalName = iRec.PersonalNames[0];
            }

            personalName.Pieces.MarriedName = marriedSurname.Trim();
        }

        public static void SetNameParts(GEDCOMPersonalName personalName, string surname, string name, string patronymic)
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

        public static NamePartsRet GetNameParts(GEDCOMIndividualRecord iRec, GEDCOMPersonalName personalName, bool formatted = true)
        {
            if (personalName == null)
                throw new ArgumentNullException("personalName");

            // extracting standard parts
            string stdSurname, stdName, stdPatronymic;
            string firstPart = personalName.FirstPart;
            stdSurname = personalName.Surname;
            string[] parts = firstPart.Split(' ');
            if (parts.Length > 1) {
                stdName = parts[0];
                stdPatronymic = parts[1];
            } else {
                stdName = firstPart;
                stdPatronymic = "";
            }

            // extracting sub-tags parts (high priority if any)
            string surname = personalName.Pieces.Surname;
            string name = personalName.Pieces.Given;
            string patronymic = personalName.Pieces.PatronymicName;
            string marriedSurname = personalName.Pieces.MarriedName;
            parts = name.Split(' ');
            if (!string.IsNullOrEmpty(name) && string.IsNullOrEmpty(patronymic) && parts.Length > 1) {
                name = parts[0];
                patronymic = parts[1];
            }

            surname = !string.IsNullOrEmpty(surname) ? surname : stdSurname;
            name = !string.IsNullOrEmpty(name) ? name : stdName;
            patronymic = !string.IsNullOrEmpty(patronymic) ? patronymic : stdPatronymic;

            if (formatted) {
                surname = GetFmtSurname(iRec, personalName, surname);
            }

            return new NamePartsRet(surname, marriedSurname, name, patronymic);
        }

        public static NamePartsRet GetNameParts(GEDCOMIndividualRecord iRec, bool formatted = true)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (iRec.PersonalNames.Count > 0) {
                return GetNameParts(iRec, iRec.PersonalNames[0], formatted);
            } else {
                return new NamePartsRet("", "", "");
            }
        }

        public sealed class NamePartsRet
        {
            // Simple or maiden surname
            public string Surname;

            public string Name;
            public string Patronymic;

            public string MarriedSurname;

            public NamePartsRet(string surname, string name, string patronymic)
            {
                Surname = surname;
                Name = name;
                Patronymic = patronymic;
            }

            public NamePartsRet(string maidenSurname, string marriedSurname, string name, string patronymic)
            {
                Surname = maidenSurname;
                MarriedSurname = marriedSurname;
                Name = name;
                Patronymic = patronymic;
            }
        }

        #endregion
    }
}
