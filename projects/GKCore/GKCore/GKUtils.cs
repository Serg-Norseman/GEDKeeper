/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

#define CALENDAR_DIFFERENCE_YEARS

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Net;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Cultures;
using GKCore.Design.Controls;
using GKCore.Import;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using UtfUnknown;

namespace GKCore
{
    /// <summary>
    ///
    /// </summary>
    public static class GKUtils
    {
        #region Aux functions

        /// <summary>
        /// Forced call of GEDCOMProvider static constructor.
        /// This is important for a number of tests that require initialization of the GEDCOM tag table.
        /// And at the start of the application, before loading any GEDCOM files.
        /// </summary>
        public static void InitGEDCOM()
        {
            RuntimeHelpers.RunClassConstructor(typeof(GEDCOMProvider).TypeHandle);
        }

        public static List<string> GetCountries()
        {
            var countries = new List<string>();
            foreach (CultureInfo culture in CultureInfo.GetCultures(CultureTypes.SpecificCultures)) {
                try {
                    RegionInfo regionInfo = new RegionInfo(culture.Name);
                    string ctry = regionInfo.TwoLetterISORegionName;
                    if (!countries.Contains(ctry))
                        countries.Add(ctry);
                } catch {
                }
            }
            countries.Sort();
            return countries;
        }

        public static void LoadExtFile(string fileName, string args = "")
        {
            try {
#if !CI_MODE
#if OS_LINUX || OS_FREEBSD
                var proc = new Process();
                proc.EnableRaisingEvents = false;
                proc.StartInfo.FileName = "xdg-open";
                proc.StartInfo.Arguments = string.Join("", "\"", fileName, "\"");
                proc.Start();
#else
                if (File.Exists(fileName)) {
                    Process.Start(new ProcessStartInfo("file://" + fileName) { UseShellExecute = true, Arguments = args });
                } else {
                    Process.Start(new ProcessStartInfo(fileName) { UseShellExecute = true });
                }
#endif
#endif
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.LoadExtFile()", ex);
            }
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

        public static string MergeStrings(GDMLines strings, int maxLength = -1)
        {
            if (strings == null)
                throw new ArgumentNullException("strings");

            StringBuilder result = new StringBuilder();

            int num = strings.Count;
            for (int i = 0; i < num; i++) {
                int curLen = result.Length;
                if (maxLength > 0 && curLen > maxLength) break;

                if (curLen != 0) result.Append(" ");
                result.Append(strings[i].Trim());
            }

            if (maxLength > 0 && result.Length > maxLength) {
                result.Remove(maxLength, result.Length - maxLength);
                result.Append("…");
            }

            return result.ToString();
        }

        public static string TruncateStrings(GDMLines value, int size)
        {
            string s = string.Empty;

            if (value != null && value.Count != 0) {
                if (size < value[0].Length) {
                    s = value[0].Substring(0, size) + "…";
                } else {
                    s = value[0];
                }
            }

            return s;
        }

        public static void SaveFilter(string flt, StringList filters)
        {
            if (filters == null) return;

            flt = flt.Trim();
            if (flt != "" && flt != "*" && filters.IndexOf(flt) < 0) filters.Add(flt);
        }

        public static void RemoveFilter(string flt, StringList filters)
        {
            if (filters == null) return;

            int idx = filters.IndexOf(flt);
            if (idx >= 0) filters.Delete(idx);
        }

        public static StringList GetLocationLinks(GDMTree tree, GDMLocationRecord locRec)
        {
            var linksList = new StringList();
            if (locRec == null) return linksList;

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var evsRec = tree[i] as GDMRecordWithEvents;
                if (evsRec == null || !evsRec.HasEvents) continue;

                int num2 = evsRec.Events.Count;
                for (int j = 0; j < num2; j++) {
                    GDMCustomEvent evt = evsRec.Events[j];

                    if (evt.HasPlace && evt.Place.Location.XRef == locRec.XRef) {
                        linksList.AddObject(GetRecordName(tree, evsRec, true) + ", " + GetEventNameLd(evt), evsRec);
                    }
                }
            }

            return linksList;
        }

        public static void GetLocationSubordinateLinks(GDMTree tree, GDMLocationRecord locRec, List<GDMLocationRecord> linksList)
        {
            if (locRec == null || linksList == null) return;

            for (int i = 0, num = tree.RecordsCount; i < num; i++) {
                var locTemp = tree[i] as GDMLocationRecord;
                if (locTemp == null) continue;

                for (int j = 0, num2 = locTemp.TopLevels.Count; j < num2; j++) {
                    var topLink = locTemp.TopLevels[j];

                    if (topLink.XRef == locRec.XRef) {
                        linksList.Add(locTemp);
                        break;
                    }
                }
            }
        }

        public static StringList GetLocationSubordinatesList(GDMTree tree, GDMLocationRecord locRec)
        {
            var linksList = new List<GDMLocationRecord>();
            GetLocationSubordinateLinks(tree, locRec, linksList);

            var result = new StringList();
            for (int i = 0, num = linksList.Count; i < num; i++) {
                var locTemp = linksList[i];
                result.AddObject(GetRecordName(tree, locTemp, false), locTemp);
            }
            return result;
        }

        public static void GetLocationRecursiveSubordinateLinks(GDMTree tree, GDMLocationRecord locRec, HashSet<GDMLocationRecord> linksList, bool excludeEmpty = false)
        {
            if (!excludeEmpty || !locRec.Map.IsEmpty())
                linksList.Add(locRec);

            var tempList = new List<GDMLocationRecord>();
            GetLocationSubordinateLinks(tree, locRec, tempList);
            for (int i = 0, num = tempList.Count; i < num; i++) {
                GetLocationRecursiveSubordinateLinks(tree, tempList[i], linksList, excludeEmpty);
            }
        }

        public static void GetLocationIndividuals(GDMTree tree, GDMLocationRecord locRec, HashSet<GDMIndividualRecord> individualRecords)
        {
            if (locRec == null) return;

            for (int i = 0, num = tree.RecordsCount; i < num; i++) {
                var indiRec = tree[i] as GDMIndividualRecord;
                if (indiRec == null || !indiRec.HasEvents) continue;

                for (int j = 0, num2 = indiRec.Events.Count; j < num2; j++) {
                    GDMCustomEvent evt = indiRec.Events[j];

                    if (evt.HasPlace && evt.Place.Location.XRef == locRec.XRef) {
                        individualRecords.Add(indiRec);
                        break;
                    }
                }
            }
        }

        public static void GetIndividualLocations(GDMTree tree, GDMIndividualRecord indiRec, HashSet<GDMLocationRecord> locationRecords)
        {
            if (indiRec == null || !indiRec.HasEvents) return;

            for (int j = 0, num2 = indiRec.Events.Count; j < num2; j++) {
                GDMCustomEvent evt = indiRec.Events[j];
                if (!evt.HasPlace) continue;

                var locRec = tree.GetPtrValue<GDMLocationRecord>(evt.Place.Location);
                if (locRec != null) locationRecords.Add(locRec);
            }
        }

        public static string HyperLink(string xref, string text)
        {
            string result;

            if (!string.IsNullOrEmpty(xref) && string.IsNullOrEmpty(text)) {
                text = "???";
            }

            if (!string.IsNullOrEmpty(xref) && !string.IsNullOrEmpty(text)) {
                result = string.Concat("[url=", xref, "]", text, "[/url]");
            } else {
                result = "";
            }

            return result;
        }

        /// <summary>
        /// Wraps all hyperlinks in the text with substrings A and B.
        /// </summary>
        public static string MakeLinks(string text)
        {
            if (string.IsNullOrEmpty(text))
                return text;

            //const string pattern = @"(https?://[^""'\s]+)";
            const string pattern = @"(?<!\]|\=|\])(https?://[^""'\s]+)(?!\[|\])";

            return Regex.Replace(text, pattern, match => $"[url={match.Value}]{match.Value}[/url]");
        }

        // TODO: greedy function, move to BaseContext
        public static string GetRecordName(GDMTree tree, GDMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                string sign = "";

                if (signed) {
                    GDMRecordType recordType = record.RecordType;
                    if (recordType != GDMRecordType.rtIndividual) {
                        if (recordType == GDMRecordType.rtFamily || (byte)recordType - (byte)GDMRecordType.rtMultimedia < (byte)GDMRecordType.rtResearch) {
                            sign = LangMan.LS(GKData.RecordTypes[(int)record.RecordType].Name) + ": ";
                        }
                    } else {
                        sign = "";
                    }
                }

                string st;
                switch (record.RecordType) {
                    case GDMRecordType.rtIndividual:
                        st = GetNameString(((GDMIndividualRecord)record), false);
                        break;
                    case GDMRecordType.rtFamily:
                        st = GetFamilyString(tree, (GDMFamilyRecord)record);
                        break;
                    case GDMRecordType.rtNote:
                        st = ((GDMNoteRecord)record).Lines[0]; // TODO: bad solution?!
                        break;
                    case GDMRecordType.rtMultimedia:
                        st = ((GDMMultimediaRecord)record).GetFileTitle();
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
                        st = GetTaskGoalStr(tree, (GDMTaskRecord)record);
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

        public static string GenRecordLink(GDMTree tree, GDMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                result = HyperLink(record.XRef, GetRecordName(tree, record, signed));
            }

            return result;
        }

        private class LinksList : List<Tuple<string, string>> { }

        public static Tuple<string, string> GenRecordLinkTuple(GDMTree tree, GDMRecord record, bool signed, string prefix = "", string suffix = "")
        {
            if (record != null) {
                string recName = GetRecordName(tree, record, signed);
                string recLink = prefix + HyperLink(record.XRef, recName) + suffix;
                // prefix always sortable
                return new Tuple<string, string>(prefix + recName, recLink);
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
            var corr = tree.GetPtrValue(commRec.Corresponder);

            if (corr != null) {
                string nm = GetNameString(corr, false);
                if (aLink) {
                    nm = HyperLink(corr.XRef, nm);
                }
                result = "[ " + LangMan.LS(GKData.CommunicationDirs[(int)commRec.CommDirection]) + " ] " + nm;
            }
            return result;
        }

        public sealed class TaskGoalRet
        {
            public readonly GDMGoalType GoalType;
            public readonly string GoalXRef;
            public readonly GDMRecord GoalRec;

            public TaskGoalRet(GDMGoalType goalType, string goalXRef, GDMRecord goalRec)
            {
                GoalType = goalType;
                GoalXRef = goalXRef;
                GoalRec = goalRec;
            }
        }

        // TODO: greedy function, move to BaseContext
        public static TaskGoalRet GetTaskGoal(GDMTree tree, GDMTaskRecord taskRec)
        {
            string goalXRef = string.Empty;
            GDMRecord goalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(taskRec.Goal));

            GDMGoalType goalType;
            if (goalRec == null) {
                goalType = GDMGoalType.gtOther;
            } else {
                switch (goalRec.RecordType) {
                    case GDMRecordType.rtIndividual:
                        goalType = GDMGoalType.gtIndividual;
                        break;
                    case GDMRecordType.rtFamily:
                        goalType = GDMGoalType.gtFamily;
                        break;
                    case GDMRecordType.rtSource:
                        goalType = GDMGoalType.gtSource;
                        break;
                    default:
                        goalType = GDMGoalType.gtOther;
                        break;
                }
            }

            return new TaskGoalRet(goalType, goalXRef, goalRec);
        }

        public static string GetTaskGoalStr(GDMTree tree, GDMTaskRecord taskRec)
        {
            if (tree == null || taskRec == null) return string.Empty;

            string result = "";

            var goal = GetTaskGoal(tree, taskRec);

            switch (goal.GoalType) {
                case GDMGoalType.gtIndividual:
                case GDMGoalType.gtFamily:
                case GDMGoalType.gtSource:
                    result = GetGoalStr(tree, goal.GoalType, goal.GoalRec);
                    break;

                case GDMGoalType.gtOther:
                    result = taskRec.Goal;
                    break;
            }

            if (goal.GoalType != GDMGoalType.gtOther) {
                result = "[" + LangMan.LS(GKData.GoalNames[(int)goal.GoalType]) + "] " + result;
            }

            return result;
        }

        public static string GetGoalStr(GDMTree tree, GDMGoalType gt, GDMRecord tempRec)
        {
            if (tempRec == null) return string.Empty;

            switch (gt) {
                case GDMGoalType.gtIndividual:
                    return GetNameString(((GDMIndividualRecord)tempRec), false);

                case GDMGoalType.gtFamily:
                    return GetFamilyString(tree, tempRec as GDMFamilyRecord);

                case GDMGoalType.gtSource:
                    return ((GDMSourceRecord)tempRec).ShortTitle;
            }

            return string.Empty;
        }

        public static List<GDMGroupRecord> GetGroups(GDMTree tree)
        {
            var result = new List<GDMGroupRecord>();

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = tree[i] as GDMGroupRecord;
                if (rec != null) {
                    result.Add(rec);
                }
            }
            result.Sort((a, b) => -b.GroupName.CompareTo(a.GroupName));

            return result;
        }

        public static List<GDMSourceRecord> GetSources(GDMTree tree)
        {
            var result = new List<GDMSourceRecord>();

            for (int i = 0; i < tree.RecordsCount; i++) {
                var rec = tree[i] as GDMSourceRecord;
                if (rec != null) {
                    result.Add(rec);
                }
            }
            result.Sort((a, b) => -b.ShortTitle.CompareTo(a.ShortTitle));

            return result;
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
            var result = new CharsetResult(null, 0.0f);

            byte[] buffer = new byte[bufferSize];
            int read = stream.Read(buffer, 0, buffer.Length);
            if (read > 0) {
                var detRes = CharsetDetector.DetectFromBytes(buffer, 0, read);
                stream.Seek(0, SeekOrigin.Begin);

                var cdet = detRes.Detected;
                if (cdet != null) {
                    result.Charset = cdet.EncodingName;
                    result.Confidence = cdet.Confidence;
                }
            }

            return result;
        }

        public static Encoding DetectEncoding(string fileName)
        {
            using (var file = File.OpenRead(fileName))
                return DetectEncoding(file);
        }

        public static Encoding DetectEncoding(Stream stream)
        {
            Encoding defaultEncoding;
            try {
                var chRes = DetectCharset(stream);
                defaultEncoding = (chRes.Charset == null) ? Encoding.UTF8 : Encoding.GetEncoding(chRes.Charset);
            } catch {
                defaultEncoding = Encoding.UTF8;
            }

            return defaultEncoding;
        }

        public static StreamReader GetDetectedStreamReader(Stream stream)
        {
            var defaultEncoding = DetectEncoding(stream);

            StreamReader reader = new StreamReader(stream, defaultEncoding);
            return reader;
        }

        #endregion

        #region Match functions

        public static string PrepareQSF(string mask)
        {
            if (string.IsNullOrEmpty(mask))
                return mask;

            if (mask[0] != '*')
                mask = '*' + mask;

            if (mask[mask.Length - 1] != '*')
                mask = mask + '*';

            mask = mask.Replace(' ', '*');

            return mask;
        }

        private static readonly char[] MaskDelimiters = { '*', '?', '|' };

        public static string PrepareMask(string mask)
        {
            string regexStr = "";

            if (!string.IsNullOrEmpty(mask)) {
                // double star evokes monstrous lags
                mask = mask.Replace("**", "*").Replace("??", "?");

                int curPos = 0;
                int len = mask.Length;

                while (curPos < len) {
                    int I = mask.IndexOfAny(MaskDelimiters, curPos);
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

        public static Regex InitMaskRegex(string mask, bool ignoreCase = true)
        {
            Regex result = null;

            string regexStr = PrepareMask(mask);
            if (!string.IsNullOrEmpty(regexStr)) {
                RegexOptions regexOpts = RegexOptions.Compiled;
                if (ignoreCase) regexOpts |= RegexOptions.IgnoreCase;

                result = new Regex(regexStr, regexOpts);
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

        public static int StrCompareEx(string str1, string str2)
        {
            double val1, val2;
            bool v1 = double.TryParse(str1, out val1);
            bool v2 = double.TryParse(str2, out val2);

            int result;
            if (v1 && v2) {
                if (val1 < val2) {
                    result = -1;
                } else if (val1 > val2) {
                    result = +1;
                } else {
                    result = 0;
                }
            } else {
                result = string.Compare(str1, str2, false);
                if (str1 != "" && str2 == "") {
                    result = -1;
                } else if (str1 == "" && str2 != "") {
                    result = +1;
                }
            }
            return result;
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

        public static bool IsAttribute(GDMCustomEvent evt)
        {
            var eventDef = AppHost.EventDefinitions.Find(evt);
            return (eventDef != null && eventDef.Kind == EventKind.ekFact);
        }

        public static EventKind GetPredefinedEventKind(string tag)
        {
            var predefEvents = GKData.PredefinedEvents;
            for (int i = 0; i < predefEvents.Length; i++) {
                var predefEvt = predefEvents[i];
                if (predefEvt.Tag == tag) {
                    return predefEvt.Kind;
                }
            }

            return EventKind.ekFact;
        }

        /// <summary>
        /// Localized event name.
        /// </summary>
        /// <param name="evt"></param>
        /// <returns></returns>
        public static string GetEventNameLd(GDMCustomEvent evt)
        {
            string evtName = GetEventName(evt);
            return LocaleOptions.Instance.AlwaysCapitalizeNouns() ? evtName : evtName.ToLower();
        }

        public static string GetEventName(GDMCustomEvent evt)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            var eventDef = AppHost.EventDefinitions.Find(evt);
            string result = (eventDef != null) ? eventDef.DisplayName : evt.GetTagName();
            return result;
        }

        public static string GetEventPlaceAndAttributeValues(GDMCustomEvent evt)
        {
            string st = evt.HasPlace ? evt.Place.StringValue : string.Empty;

            if (evt.StringValue != "") {
                st = st + " [" + evt.StringValue + "]";
            }

            return st;
        }

        public static string GetAttributeStr(GDMIndividualAttribute iAttr)
        {
            if (iAttr == null)
                throw new ArgumentNullException("iAttr");

            string st = GetEventName(iAttr);

            string place = string.Empty;
            if (iAttr.HasPlace) {
                place = iAttr.Place.StringValue;
                if (place != "") {
                    place = string.Concat(" [", place, "]");
                }
            }
            return string.Concat(st, ": ", iAttr.StringValue, place);
        }

        public static string GetEventStr(GDMCustomEvent evt)
        {
            string st = GetEventName(evt);
            string dt = GEDCOMEventToDateStr(evt, GlobalOptions.Instance.DefDateFormat, false);

            string attrVal = (IsAttribute(evt) && !string.IsNullOrEmpty(evt.StringValue)) ? string.Format(" `{0}`", evt.StringValue) : string.Empty;

            string result = string.Concat(dt, ": ", st, attrVal, ".");
            if (evt.HasPlace && evt.Place.StringValue != "") {
                result = string.Concat(result, " ", LangMan.LS(LSID.Place), ": ", evt.Place.StringValue);
            }
            return result;
        }

        public static string GetEventDesc(GDMTree tree, GDMCustomEvent evt, bool hyperLink = true)
        {
            if (evt == null)
                throw new ArgumentNullException("evt");

            var globOpts = GlobalOptions.Instance;
            string dt = GEDCOMEventToDateStr(evt, globOpts.DefDateFormat, globOpts.ShowDatesSign);

            string place = string.Empty;
            if (evt.HasPlace) {
                place = evt.Place.StringValue;
                GDMLocationRecord location = tree.GetPtrValue<GDMLocationRecord>(evt.Place.Location);

                if (place != "" && location != null && hyperLink) {
                    place = HyperLink(location.XRef, place);
                }
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
                result = string.Concat(result, "[", evt.Agency, "]");
            }

            return result;
        }

        #endregion

        #region Date functions

        public static string GetDateMask(string regionalDatePattern)
        {
            // "00/00/0000"
            string result = regionalDatePattern.Replace('d', '0').Replace('m', '0').Replace('y', '0');
            return result;
        }

        public static string GetShortDatePattern()
        {
            var culture = CultureInfo.CurrentCulture; // work
            //var culture = new CultureInfo("en-US"); // debug
            //var culture = new CultureInfo("hu-HU"); // debug

            var dtf = culture.DateTimeFormat;
            var dateSeparators = dtf.DateSeparator.ToCharArray();

            // may contain a period, a dash, and a slash
            var result = dtf.ShortDatePattern.ToLowerInvariant();
            Logger.WriteInfo(string.Format("ShortDatePattern: {0}", result));

            // normalize
            string[] parts = result.Split(dateSeparators, StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < parts.Length; i++) {
                string part = parts[i];
                char firstChar = part[0];
                switch (firstChar) {
                    case 'd':
                    case 'm':
                        if (part.Length < 2) {
                            part = part.PadRight(2, firstChar);
                        }
                        break;

                    case 'y':
                        if (part.Length < 4) {
                            part = part.PadRight(4, firstChar);
                        }
                        break;
                }
                parts[i] = part;
            }
            result = string.Join("/", parts);

            return result;
        }

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
            try {
                string[] resultParts = new string[3];

                if (!string.IsNullOrEmpty(regionalDate)) {
                    string[] regionalParts = regionalDate.Split('/');
                    string[] patternParts = pattern.Split('/');

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
                }

                string result = string.Join(".", resultParts);
                return result;
            } catch (Exception ex) {
                Logger.WriteError(string.Format("GKUtils.GetNormalizeDate({0}, {1})", regionalDate, pattern), ex);
                return string.Empty;
            }
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

            try {
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
            } catch (Exception ex) {
                Logger.WriteError(string.Format("GKUtils.GetRegionalDate({0}, {1})", normalizeDate, pattern), ex);
                return string.Empty;
            }
        }

        public static string GEDCOMEventToDateStr(GDMCustomEvent evt, DateFormat format, bool sign, bool shorten = false)
        {
            return (evt == null) ? string.Empty : evt.Date.GetDisplayStringExt(format, sign, false, shorten);
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
            if (ds == "") {
                ds = "?";
            }
            result += ds;

            ds = GetDeathDate(iRec, GlobalOptions.Instance.DefDateFormat, false);
            if (ds == "") {
                GDMCustomEvent ev = iRec.FindEvent(GEDCOMTagType.DEAT);
                if (ev != null) {
                    ds = "?";
                }
            }

            if (ds != "") {
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
                case PedigreeFormat.Excess: {
                        string ds = GetBirthDate(iRec, GlobalOptions.Instance.DefDateFormat, true);
                        if (ds == "") {
                            ds = "?";
                        }
                        result += ds;
                        ds = GetDeathDate(iRec, GlobalOptions.Instance.DefDateFormat, true);
                        if (ds == "") {
                            GDMCustomEvent ev = iRec.FindEvent(GEDCOMTagType.DEAT);
                            if (ev != null) {
                                ds = "?";
                            }
                        }
                        if (ds != "") {
                            result = result + " - " + ds;
                        }
                    }
                    break;

                case PedigreeFormat.Compact: {
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

        public static int GetChronologicalYear(GDMDateValue dateVal)
        {
            return (dateVal == null) ? 0 : dateVal.GetChronologicalYear();
        }

        public static int GetChronologicalYear(GDMCustomEvent evt)
        {
            return (evt == null) ? 0 : evt.Date.GetChronologicalYear();
        }

        /// <summary>
        /// Calculate the difference in years between two dates.
        /// </summary>
        /// <param name="ev1">Starting event.</param>
        /// <param name="ev2">End event.</param>
        /// <param name="inference">If the 'inference' argument is given, then if there is no end date, it is possible to use the current year.</param>
        /// <returns></returns>
        public static int GetEventsYearsDiff(GDMCustomEvent ev1, GDMCustomEvent ev2, bool inference)
        {
            int result = -1;

            try {
#if !CALENDAR_DIFFERENCE_YEARS
                int dt1 = GetChronologicalYear(ev1);
                int dt2 = GetChronologicalYear(ev2);

                if (currentEnd && dt2 == 0) {
                    dt2 = DateTime.Now.Year;
                }

                if (dt1 != 0 && dt2 != 0) {
                    result = Math.Abs(dt2 - dt1);
                }
#else
                var udn1 = (ev1 == null) ? UDN.Unknown : ev1.Date.GetUDN();
                if (udn1.HasKnownYear()) {
                    DateTime dt1 = udn1.GetGregorianDateTime();

                    var udn2 = (ev2 == null) ? UDN.Unknown : ev2.Date.GetUDN();
                    DateTime dt2;
                    if (udn2.HasKnownYear()) {
                        dt2 = udn2.GetGregorianDateTime();
                        result = GetDifferenceInYears(dt1, dt2);
                    } else {
                        if (inference) {
                            dt2 = DateTime.Now;
                            result = GetDifferenceInYears(dt1, dt2);
                        }
                    }
                }
#endif
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetEventsYearsDiff()", ex);
            }

            return result;
        }

        public static int GetDifferenceInYears(DateTime startDate, DateTime endDate)
        {
            int offset = ((endDate.Month > startDate.Month) || (endDate.Month == startDate.Month && endDate.Day >= startDate.Day)) ? 1 : 0;
            int diff = endDate.Year - startDate.Year - 1 + offset;
            return (diff < 0) ? 0 : diff;
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
                var lifeDates = iRec.GetLifeEvents();
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
                var lifeDates = iRec.GetLifeEvents();
                result = GetAgeLD(lifeDates, toYear);
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetAge()", ex);
            }

            return result;
        }

        public static int GetAgeLD(GDMIndividualRecord.LifeEvents lifeDates, int toYear)
        {
            int result = -1;
            if (lifeDates == null) return result;

            try {
                if (toYear == -1) {
                    result = GetEventsYearsDiff(lifeDates.BirthEvent, lifeDates.DeathEvent, lifeDates.DeathEvent == null);
                } else {
                    int birthYear = GetChronologicalYear(lifeDates.BirthEvent);
                    if (birthYear != 0) {
                        result = toYear - birthYear;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetAgeLD()", ex);
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

        public static string GetMarriageDateStr(GDMFamilyRecord fRec, DateFormat dateFormat, bool sign)
        {
            GDMCustomDate date = GetMarriageDate(fRec);
            return (date == null) ? string.Empty : date.GetDisplayStringExt(dateFormat, sign, false);
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
        public static int GetDaysForBirth(GDMIndividualRecord iRec, bool onlyAlive, out int years, out bool anniversary)
        {
            int distance = -1;
            years = 0;
            anniversary = false;

            if (iRec == null) return distance;

            GDMCustomEvent evt;

            if (onlyAlive) {
                evt = iRec.FindEvent(GEDCOMTagType.DEAT);
                if (evt != null) return distance;
            }

            evt = iRec.FindEvent(GEDCOMTagType.BIRT);
            if (evt == null) return distance;

            var dt = evt.Date.Value as GDMDate;
            if (dt == null || !dt.IsValidDate()) return distance;

            distance = GetDaysFor(dt, DateTime.Now, out years, out anniversary);
            return distance;
        }

        public static int GetDaysFor(GDMDate dt, DateTime dtNow, out int years, out bool anniversary)
        {
            int distance = -1;
            years = 0;
            anniversary = false;

            try {
                if (dt != null && dt.IsValidDate()) {
                    int bdY = dt.Year;
                    int bdM = dt.Month;
                    int bdD = dt.Day;

                    int curY = dtNow.Year;
                    int curM = dtNow.Month;
                    int curD = dtNow.Day;

                    double bdN = bdM * 100 + bdD;
                    double curN = curM * 100 + curD;

                    if (bdN >= curN) {
                        years = curY - bdY;
                        anniversary = (years % 10 == 0) || (years % 25 == 0);

                        bdY = (bdN < curN) ? (curY + 1) : curY;
                        // There are valid birthdays on February 29th in leap years.
                        // For other years, we need a correction for an acceptable day.
                        if (bdD == 29 && bdM == 2 && !DateTime.IsLeapYear(bdY)) {
                            bdD -= 1;
                        }

                        distance = DateHelper.DaysBetween(dtNow.Date, new DateTime(bdY, bdM, bdD));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetDaysFor()", ex);
            }

            return distance;
        }

        public static string GetBirthTipMessage(ICulture culture, GDMIndividualRecord iRec, int days, int years, bool anniversary)
        {
            string nm = culture.GetPossessiveName(iRec);

            string tip;
            if (!anniversary) {
                switch (days) {
                    case 0:
                        tip = string.Format(LangMan.LS(LSID.BirthdayToday), nm);
                        break;
                    case 1:
                        tip = string.Format(LangMan.LS(LSID.BirthdayTomorrow), nm);
                        break;
                    default:
                        tip = string.Format(LangMan.LS(LSID.DaysRemained), nm, days);
                        break;
                }
            } else {
                switch (days) {
                    case 0:
                        tip = string.Format(LangMan.LS(LSID.AnniversaryToday), nm);
                        break;
                    case 1:
                        tip = string.Format(LangMan.LS(LSID.AnniversaryTomorrow), nm);
                        break;
                    default:
                        tip = string.Format(LangMan.LS(LSID.AnniversaryDaysRemained), nm, days);
                        break;
                }
            }
            return tip;
        }

        public static string GetCalendarSign(GDMCalendar calendar)
        {
            var globOptions = GlobalOptions.Instance;
            int ix = (int)calendar;
            if (!globOptions.LocalizedCalendarSignatures) {
                return GKData.DateCalendars[ix].Sign;
            } else {
                string signs = LangMan.LS(LSID.LocalizedCalendarSignaturesValues);
                string[] parts = signs.Split('|');
                return " " + ((ix < parts.Length) ? parts[ix] : GKData.DateCalendars[ix].Sign);
            }
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

        public static readonly char[] PLACE_DELIMITERS = new char[] { ',' };

        public static string GetPlaceStr(GDMCustomEvent evt, bool includeAddress, bool onlyLocality = false)
        {
            if (evt == null || !evt.HasPlace) return string.Empty;

            string result = evt.Place.StringValue;

            if (!string.IsNullOrEmpty(result) && onlyLocality) {
                string[] placeParts = result.Split(PLACE_DELIMITERS, StringSplitOptions.None);
                if (placeParts.Length > 1) {
                    bool reverseOrder = GlobalOptions.Instance.ReversePlaceEntitiesOrder;
                    result = ((reverseOrder) ? placeParts[0] : placeParts[placeParts.Length - 1]).Trim();
                }
            }

            if (includeAddress) {
                string resi = evt.StringValue;

                if (evt.HasAddress) {
                    string addrText = evt.Address.Lines.Text.Trim();
                    if (resi != "" && addrText != "") {
                        resi += ", ";
                    }
                    resi += addrText;
                }

                if (resi != "") {
                    result = result + " [" + resi + "]";
                }
            }

            return result;
        }

        public static string GetLocationNameExt(GDMLocationRecord locRec, GDMCustomDate date)
        {
            try {
                if (GlobalOptions.Instance.ExtendedLocations) {
                    return locRec.GetNameByDate(date, true);
                } else {
                    return locRec.LocationName;
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.GetLocationNameExt()", ex);
                return locRec.LocationName;
            }
        }

        #endregion

        #region Individual functions

        public static int GetAncestorsCount(GDMTree tree, GDMIndividualRecord iRec, int ancestorsLimit = -1)
        {
            var indiCounters = new GKVarCache<GDMIndividualRecord, int>(-1);
            return GetAncestorsCount(tree, iRec, indiCounters, 0, ancestorsLimit);
        }

        private static int GetAncestorsCount(GDMTree tree, GDMIndividualRecord iRec, GKVarCache<GDMIndividualRecord, int> counters, int gen, int ancestorsLimit)
        {
            if (ancestorsLimit != -1 && gen > ancestorsLimit) return 0;

            int result = 0;

            if (iRec != null) {
                int val = counters[iRec];

                if (val < 0) {
                    val = 1;

                    GDMFamilyRecord family = tree.GetParentsFamily(iRec);
                    if (family != null) {
                        GDMIndividualRecord anc;

                        anc = tree.GetPtrValue(family.Husband);
                        val += GetAncestorsCount(tree, anc, counters, gen + 1, ancestorsLimit);

                        anc = tree.GetPtrValue(family.Wife);
                        val += GetAncestorsCount(tree, anc, counters, gen + 1, ancestorsLimit);
                    }

                    counters[iRec] = val;
                }

                result = val;
            }

            return result;
        }

        public static int GetDescendantsCount(GDMTree tree, GDMIndividualRecord iRec, int descendantsLimit = -1)
        {
            var indiCounters = new GKVarCache<GDMIndividualRecord, int>(-1);
            return GetDescendantsCount(tree, iRec, indiCounters, 0, descendantsLimit);
        }

        private static int GetDescendantsCount(GDMTree tree, GDMIndividualRecord iRec, GKVarCache<GDMIndividualRecord, int> counters, int gen, int descendantsLimit)
        {
            if (descendantsLimit != -1 && gen > descendantsLimit) return 0;

            int result = 0;

            if (iRec != null) {
                int val = counters[iRec];
                if (val < 0) {
                    val = 1;

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++) {
                        GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);

                        int num2 = family.Children.Count;
                        for (int j = 0; j < num2; j++) {
                            GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);
                            val += GetDescendantsCount(tree, child, counters, gen + 1, descendantsLimit);
                        }
                    }
                    counters[iRec] = val;
                }
                result = val;
            }

            return result;
        }

        private static int GetDescGens_Recursive(GDMTree tree, GDMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null) {
                int max = 0;

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);
                        int res = GetDescGens_Recursive(tree, child);
                        if (max < res) {
                            max = res;
                        }
                    }
                }
                result = 1 + max;
            }

            return result;
        }

        public static int GetDescGenerations(GDMTree tree, GDMIndividualRecord iRec)
        {
            return GetDescGens_Recursive(tree, iRec) - 1;
        }

        public static int GetMarriagesCount(GDMIndividualRecord iRec)
        {
            int result = ((iRec == null) ? 0 : iRec.SpouseToFamilyLinks.Count);
            return result;
        }

        public static int GetSpousesDiff(GDMTree tree, GDMFamilyRecord fRec)
        {
            int result = -1;

            try {
                if (fRec != null) {
                    GDMIndividualRecord husb = tree.GetPtrValue(fRec.Husband);
                    GDMIndividualRecord wife = tree.GetPtrValue(fRec.Wife);

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

        public static GDMIndividualRecord GetFirstborn(GDMTree tree, GDMIndividualRecord iRec)
        {
            GDMIndividualRecord iChild = null;
            if (iRec == null) return iChild;

            try {
                int firstYear = 0;

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                    if (family == null) continue;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);
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

        public static int GetMarriageAge(GDMTree tree, GDMIndividualRecord iRec)
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
                        GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
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

        public static string GetGroupsStr(GDMTree tree, GDMIndividualRecord iRec)
        {
            var result = new StringBuilder();

            if (iRec.HasGroups) {
                var groups = iRec.Groups;
                int count = groups.Count;
                for (int idx = 0; idx < count; idx++) {
                    GDMGroupRecord grp = tree.GetPtrValue<GDMGroupRecord>(groups[idx]);
                    if (grp != null) {
                        if (idx > 0)
                            result.Append("; ");

                        result.Append(grp.GroupName);
                    }
                }
            }

            return result.ToString();
        }

        public static string GetCallNumbersStr(GDMRepositoryCitation repCit)
        {
            var result = new StringBuilder();

            if (repCit.HasCallNumbers) {
                var list = repCit.CallNumbers;
                int count = list.Count;
                for (int idx = 0; idx < count; idx++) {
                    var callNum = list[idx];
                    if (!string.IsNullOrEmpty(callNum.StringValue)) {
                        if (idx > 0)
                            result.Append("; ");

                        result.Append(callNum.StringValue);
                    }
                }
            }

            return result.ToString();
        }

        private static readonly float[] CA_VALUES = new float[] { 0.25f, 0.5f, 0.75f, 1.0f };

        private static void GetCertaintyVars(IGDMStructWithSourceCitations str, CertaintyAlgorithm algorithm, ref float result, ref float wsum)
        {
            for (int k = 0, num = str.SourceCitations.Count; k < num; k++) {
                var cit = str.SourceCitations[k];
                int ca = cit.GetValidCertaintyAssessment();
                float value = CA_VALUES[ca];

                switch (algorithm) {
                    case CertaintyAlgorithm.WeightedAverage:
                        int weight = (ca + 1);
                        result += (value * weight);
                        wsum += weight;
                        break;
                    case CertaintyAlgorithm.Average:
                        result += value;
                        wsum += 1;
                        break;
                    case CertaintyAlgorithm.Minimum:
                        result = Math.Min(result, value);
                        break;
                    case CertaintyAlgorithm.Maximum:
                        result = Math.Max(result, value);
                        break;
                }
            }
        }

        public static float GetCertaintyAssessment(IGDMRecordWithEvents record, CertaintyAlgorithm algorithm)
        {
            // variables initialization
            float result = 0;
            float wsum = 0;
            switch (algorithm) {
                case CertaintyAlgorithm.WeightedAverage:
                case CertaintyAlgorithm.Average:
                    result = 0;
                    wsum = 0;
                    break;

                case CertaintyAlgorithm.Minimum:
                    result = 1;
                    break;

                case CertaintyAlgorithm.Maximum:
                    result = 0;
                    break;
            }

            if (record.HasEvents) {
                for (int i = 0, num = record.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = record.Events[i];
                    if (evt.HasSourceCitations) {
                        GetCertaintyVars(evt, algorithm, ref result, ref wsum);
                    }
                }
            }

            if (record.HasSourceCitations) {
                GetCertaintyVars(record, algorithm, ref result, ref wsum);
            }

            // result finalization
            switch (algorithm) {
                case CertaintyAlgorithm.WeightedAverage:
                case CertaintyAlgorithm.Average:
                    if (wsum != 0.0f) {
                        result /= wsum;
                    } else {
                        result = 0.0f;
                    }
                    break;

                case CertaintyAlgorithm.Minimum:
                case CertaintyAlgorithm.Maximum:
                    break;
            }

            return result;
        }

        public static float GetCertaintyAssessment(IGDMRecordWithEvents record)
        {
            return GetCertaintyAssessment(record, GlobalOptions.Instance.CertaintyAlgorithm);
        }

        #endregion

        #region Tree utils

        public static void PrepareHeader(GDMTree tree, string fileName, GEDCOMCharacterSet charSet, bool zeroRev)
        {
            GDMHeader header = tree.Header;

            string subm = header.Submitter.XRef;
            int oldRev = header.File.Revision;
            string uid = header.File.UID;
            GDMLanguageID langId = header.Language;
            string note = header.Note.Lines.Text.Trim();

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
            header.File.UID = uid;

            if (zeroRev) {
                header.File.Revision = 0;
            } else {
                header.File.Revision = oldRev + 1;
            }

            if (!string.IsNullOrEmpty(subm)) {
                header.Submitter.XRef = subm;
            }

            if (!string.IsNullOrEmpty(note)) {
                header.Note.Lines.Text = note;
            }
        }

        #endregion

        #region Folder functions

        public static string GetTempDir()
        {
            string tempPath;
            if (SysUtils.IsUnix())
                tempPath = Environment.GetEnvironmentVariable("TMP") ?? "/tmp";
            else
                tempPath = Environment.GetEnvironmentVariable("TEMP");

            return tempPath + Path.DirectorySeparatorChar;
        }

        public static string GetBinPath()
        {
#if NET6_0_OR_GREATER
            string fn = Environment.ProcessPath;
#else
            var asm = SysUtils.GetExecutingAssembly();
            Module[] mods = asm.GetModules();
            string fn = mods[0].FullyQualifiedName;
#endif
            return Path.GetDirectoryName(fn) + Path.DirectorySeparatorChar;
        }

        public static string GetAppPath()
        {
            string result = Path.GetFullPath(Path.Combine(GetBinPath(), @".." + Path.DirectorySeparatorChar));
            return result;
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

        public static string GetCulturesPath()
        {
            string appPath = GetLangsPath();
            return appPath + "cultures" + Path.DirectorySeparatorChar;
        }

        public static string GetBackgroundsPath()
        {
            string appPath = GetAppPath();
            return appPath + "backgrounds" + Path.DirectorySeparatorChar;
        }

        public static string GetExternalsPath()
        {
            string appPath = GetAppPath();
            return appPath + "externals" + Path.DirectorySeparatorChar;
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
            int reportedProgress = 0, read = 0;
            long len = sourceStm.Length;
            float flen = len;

            using (var targetStm = target.OpenWrite()) {
                targetStm.SetLength(sourceStm.Length);
                for (long size = 0; size < len; size += read) {
                    if (progressController != null) {
                        int progress = (int)((size / flen) * 100);
                        if (progress != reportedProgress) {
                            reportedProgress = progress;
                            progressController.StepTo(reportedProgress);
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
            var fromPath = Path.GetDirectoryName(fromFileName) + Path.DirectorySeparatorChar;

            var fromUri = new Uri(fromPath);
            var toUri = new Uri(toFileName);

            var relativeUri = fromUri.MakeRelativeUri(toUri);
            var relativePath = Uri.UnescapeDataString(relativeUri.ToString());

            return relativePath.Replace('/', Path.DirectorySeparatorChar);
        }

        public static string GetImageFilter(bool svg)
        {
            string filter = LangMan.LS(LSID.TreeImagesFilter);

#if !NETCORE
            // Emf is not supported by Eto.Drawing
            filter += LangMan.LS(LSID.EmfFilter);
#endif

            if (svg) {
                filter += LangMan.LS(LSID.SvgFilter);
            }

            return filter;
        }

        #endregion

        #region Show information summary

        private static void ShowAddressSummary(GDMAddress address, StringList summary)
        {
            if (address != null && !address.IsEmpty() && summary != null) {
                summary.Add("    " + LangMan.LS(LSID.Address) + ":");

                string ts = "";
                if (address.AddressCountry != "") {
                    ts = ts + address.AddressCountry + ", ";
                }
                if (address.AddressState != "") {
                    ts = ts + address.AddressState + ", ";
                }
                if (address.AddressCity != "") {
                    ts += address.AddressCity;
                }
                if (ts != "") {
                    summary.Add("    " + ts);
                }

                ts = "";
                if (address.AddressPostalCode != "") {
                    ts = ts + address.AddressPostalCode + ", ";
                }
                if (address.Lines.Text.Trim() != "") {
                    ts += address.Lines.Text.Trim();
                }
                if (ts != "") {
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
                    summary.Add("    " + MakeLinks(address.WebPages[i].StringValue));
                }
            }
        }

        public static void ShowDetailCause(GDMCustomEvent evt, StringList summary)
        {
            string cause = GetEventCause(evt);
            if (summary != null && !string.IsNullOrEmpty(cause)) {
                summary.Add("    " + cause);
            }
        }

        private static void ShowEvent(GDMTree tree, GDMRecord subject, LinksList linksList, GDMRecord record, GDMCustomEvent evt)
        {
            switch (subject.RecordType) {
                case GDMRecordType.rtNote:
                    if (evt.HasNotes) {
                        for (int i = 0, num = evt.Notes.Count; i < num; i++) {
                            if (evt.Notes[i].XRef == subject.XRef) {
                                ShowLink(tree, subject, linksList, record, evt, null);
                            }
                        }
                    }
                    break;

                case GDMRecordType.rtMultimedia:
                    if (evt.HasMultimediaLinks) {
                        for (int i = 0, num = evt.MultimediaLinks.Count; i < num; i++) {
                            if (evt.MultimediaLinks[i].XRef == subject.XRef) {
                                ShowLink(tree, subject, linksList, record, evt, null);
                            }
                        }
                    }
                    break;

                case GDMRecordType.rtSource:
                    if (evt.HasSourceCitations) {
                        for (int i = 0, num = evt.SourceCitations.Count; i < num; i++) {
                            var sourCit = evt.SourceCitations[i];
                            if (sourCit.XRef == subject.XRef) {
                                ShowLink(tree, subject, linksList, record, evt, sourCit);
                            }
                        }
                    }
                    break;
            }
        }

        private static void ShowLink(GDMTree tree, GDMRecord aSubject, LinksList linksList, GDMRecord record, GDMTag aTag, GDMPointer aExt)
        {
            string prefix = "    ";
            if (aSubject is GDMSourceRecord && aExt is GDMSourceCitation cit) {
                if (!string.IsNullOrEmpty(cit.Page)) {
                    prefix += cit.Page + ": ";
                }
            }

            string suffix;
            if (aTag is GDMCustomEvent evt) {
                suffix = ", " + GetEventNameLd(evt);
            } else {
                suffix = "";
            }

            linksList.Add(GenRecordLinkTuple(tree, record, true, prefix, suffix));
        }

        public static int FindLinkStr(StringList list, string link)
        {
            if (list != null) {
                for (int i = 0, num = list.Count; i < num; i++) {
                    if (list[i].Contains(link)) {
                        return i;
                    }
                }
            }
            return -1;
        }

        public static void ExpandExtInfo(IBaseContext context, IHyperView sender, string linkName)
        {
            string xref = linkName.Remove(0, GKData.INFO_HREF_EXPAND_ASSO.Length);
            var iRec = context.Tree.FindXRef<GDMIndividualRecord>(xref);
            if (iRec is GDMIndividualRecord) {
                int lineIdx = GKUtils.FindLinkStr(sender.Lines, linkName);
                var strList = new StringList();
                GKUtils.ShowPersonExtInfo(context.Tree, iRec, strList, false);
                sender.Lines.AddStrings(strList);
                sender.Lines.Delete(lineIdx);
            }
        }

        public static void ShowPersonExtInfo(GDMTree tree, GDMIndividualRecord iRec, StringList summary, bool checkOpt = true)
        {
            if (tree == null || iRec == null || summary == null) return;

            if (checkOpt && !GlobalOptions.Instance.ShowIndiAssociations) return;

            bool first = true;
            summary.Add("");
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];
                if (rec.RecordType != GDMRecordType.rtIndividual) continue;

                GDMIndividualRecord ir = (GDMIndividualRecord)rec;
                if (!ir.HasAssociations) continue;

                for (int k = 0, cnt = ir.Associations.Count; k < cnt; k++) {
                    GDMAssociation asso = ir.Associations[k];

                    if (asso.XRef == iRec.XRef) {
                        if (first) {
                            summary.Add(LangMan.LS(LSID.Associations) + ":");
                            first = false;
                        }
                        summary.Add("    " + asso.Relation + ", " + HyperLink(ir.XRef, GetNameString(ir, true, false)));
                    }
                }
            }
        }

        private static void ShowPersonNamesakes(GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            if (!GlobalOptions.Instance.ShowIndiNamesakes) return;

            try {
                var namesakes = new List<string>();
                string st = GetNameString(iRec, false);

                int num3 = tree.RecordsCount;
                for (int i = 0; i < num3; i++) {
                    GDMRecord rec = tree[i];
                    if (rec.RecordType != GDMRecordType.rtIndividual || rec == iRec) continue;

                    GDMIndividualRecord relPerson = (GDMIndividualRecord)rec;
                    string unk = GetNameString(relPerson, false);
                    if (st == unk) {
                        namesakes.Add(HyperLink(relPerson.XRef, unk + GetLifeStr(relPerson)));
                    }
                }

                if (namesakes.Count > 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Namesakes) + ":");

                    int num4 = namesakes.Count;
                    for (int i = 0; i < num4; i++) {
                        summary.Add("    " + namesakes[i]);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowPersonNamesakes()", ex);
            }
        }

        private static void ShowSubjectLinks(GDMTree tree, GDMRecord subject, StringList summary, bool sort = false)
        {
            try {
                var subjectType = subject.RecordType;
                var linksList = new LinksList();

                for (int k = 0, num2 = tree.RecordsCount; k < num2; k++) {
                    GDMRecord record = tree[k];

                    switch (subjectType) {
                        case GDMRecordType.rtNote:
                            if (record.HasNotes) {
                                for (int i = 0, num = record.Notes.Count; i < num; i++) {
                                    if (record.Notes[i].XRef == subject.XRef) {
                                        ShowLink(tree, subject, linksList, record, null, null);
                                    }
                                }
                            }
                            break;

                        case GDMRecordType.rtMultimedia:
                            if (record.HasMultimediaLinks) {
                                for (int i = 0, num = record.MultimediaLinks.Count; i < num; i++) {
                                    if (record.MultimediaLinks[i].XRef == subject.XRef) {
                                        ShowLink(tree, subject, linksList, record, null, null);
                                    }
                                }
                            }
                            break;

                        case GDMRecordType.rtSource:
                            if (record.HasSourceCitations) {
                                for (int i = 0, num = record.SourceCitations.Count; i < num; i++) {
                                    var sourCit = record.SourceCitations[i];
                                    if (sourCit.XRef == subject.XRef) {
                                        ShowLink(tree, subject, linksList, record, null, sourCit);
                                    }
                                }
                            }
                            break;
                    }

                    if (record is GDMRecordWithEvents evsRec && evsRec.HasEvents) {
                        for (int i = 0, num = evsRec.Events.Count; i < num; i++) {
                            ShowEvent(tree, subject, linksList, evsRec, evsRec.Events[i]);
                        }
                    }
                }

                if (linksList.Count > 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Links) + ":");

                    if (sort) {
                        linksList.Sort();
                    }

                    for (int j = 0, num3 = linksList.Count; j < num3; j++) {
                        summary.Add(linksList[j].Item2);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSubjectLinks()", ex);
            }
        }

        public static void SearchRecordLinks(List<IGDMObject> linksList, GDMRecord inRecord, GDMRecord searchRec)
        {
            try {
                int num;
                switch (searchRec.RecordType) {
                    case GDMRecordType.rtNote:
                        num = inRecord.Notes.Count;
                        for (int i = 0; i < num; i++) {
                            var notes = inRecord.Notes[i];
                            if (notes.XRef == searchRec.XRef) {
                                linksList.Add(notes);
                            }
                        }
                        break;

                    case GDMRecordType.rtMultimedia:
                        num = inRecord.MultimediaLinks.Count;
                        for (int i = 0; i < num; i++) {
                            var mmLink = inRecord.MultimediaLinks[i];
                            if (mmLink.XRef == searchRec.XRef) {
                                linksList.Add(mmLink);
                            }
                        }
                        break;

                    case GDMRecordType.rtSource:
                        num = inRecord.SourceCitations.Count;
                        for (int i = 0; i < num; i++) {
                            var sourCit = inRecord.SourceCitations[i];
                            if (sourCit.XRef == searchRec.XRef) {
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

        public static void SearchRecordLinks(List<IGDMObject> linksList, GDMTree tree, GDMRecord searchRec)
        {
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                SearchRecordLinks(linksList, tree[i], searchRec);
            }
        }

        public static StringList SearchPortraits(GDMTree tree, GDMMultimediaRecord mmRec)
        {
            var result = new StringList();

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = tree[i];
                if (rec.RecordType == GDMRecordType.rtIndividual) {
                    var iRec = rec as GDMIndividualRecord;

                    int num2 = iRec.MultimediaLinks.Count;
                    for (int k = 0; k < num2; k++) {
                        var mmLink = iRec.MultimediaLinks[k];
                        if (mmLink.XRef == mmRec.XRef /*&& mmLink.IsPrimary*/) {
                            string indiName = GKUtils.GetNameString(iRec, false);
                            ExtRect region = mmLink.CutoutPosition.Value;
                            result.AddObject(indiName, region);
                        }
                    }
                }
            }

            return result;
        }

        private static void RecListMediaRefresh(IBaseContext baseContext, IGDMStructWithMultimediaLinks structWML, StringList summary, string indent = "")
        {
            if (structWML == null || summary == null) return;

            try {
                if (structWML.HasMultimediaLinks) {
                    summary.Add("");
                    summary.Add(indent + LangMan.LS(LSID.RPMultimedia) + " (" + structWML.MultimediaLinks.Count.ToString() + "):");

                    int num = structWML.MultimediaLinks.Count;
                    for (int i = 0; i < num; i++) {
                        GDMMultimediaLink mmLink = structWML.MultimediaLinks[i];
                        GDMMultimediaRecord mmRec = baseContext.Tree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                        if (mmRec == null || mmRec.FileReferences.Count == 0) continue;

                        string st = mmRec.FileReferences[0].Title;
                        summary.Add(indent + "  " + HyperLink(mmRec.XRef, st) + " (" +
                                    HyperLink(GKData.INFO_HREF_VIEW + mmRec.XRef, LangMan.LS(LSID.MediaView)) + ")");
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListMediaRefresh()", ex);
            }
        }

        private static void RecListNotesRefresh(IBaseContext baseContext, IGDMStructWithNotes structWN, StringList summary, string indent = "")
        {
            if (structWN == null || summary == null) return;

            try {
                if (structWN.HasNotes) {
                    summary.Add("");
                    summary.Add(indent + LangMan.LS(LSID.RPNotes) + " (" + structWN.Notes.Count.ToString() + "):");

                    int num = structWN.Notes.Count;
                    for (int i = 0; i < num; i++) {
                        if (i > 0) {
                            summary.Add("");
                        }

                        GDMLines noteLines = baseContext.Tree.GetNoteLines(structWN.Notes[i]);
                        summary.AddILines(noteLines, "    " + indent);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListNotesRefresh()", ex);
            }
        }

        private static void RecListSourcesRefresh(IBaseContext baseContext, IGDMStructWithSourceCitations structWSC, StringList summary, string indent = "")
        {
            if (structWSC == null || summary == null) return;

            try {
                if (structWSC.HasSourceCitations) {
                    if (structWSC is IGDMRecord) {
                        summary.Add("");
                    }

                    summary.Add(indent + LangMan.LS(LSID.RPSources) + " (" + structWSC.SourceCitations.Count.ToString() + "):");

                    int num = structWSC.SourceCitations.Count;
                    for (int i = 0; i < num; i++) {
                        GDMSourceCitation sourCit = structWSC.SourceCitations[i];
                        GDMSourceRecord sourceRec = baseContext.Tree.GetPtrValue<GDMSourceRecord>(sourCit);
                        if (sourceRec == null) continue;

                        string nm = "\"" + sourceRec.ShortTitle + "\"";
                        if (!string.IsNullOrEmpty(sourCit.Page)) {
                            nm = nm + ", " + sourCit.Page;
                        }
                        summary.Add(indent + "  " + HyperLink(sourceRec.XRef, nm));

                        var text = sourCit.Data.Text;
                        if (!text.IsEmpty()) {
                            summary.Add(indent + "    " + text.Lines.Text);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListSourcesRefresh()", ex);
            }
        }

        private static void RecListAssociationsRefresh(IBaseContext baseContext, GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasAssociations) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Associations) + ":");

                    int num = record.Associations.Count;
                    for (int i = 0; i < num; i++) {
                        GDMAssociation ast = record.Associations[i];
                        var relIndi = baseContext.Tree.GetPtrValue(ast);

                        string nm = ((relIndi == null) ? string.Empty : GetNameString(relIndi, false));
                        string xref = ((relIndi == null) ? string.Empty : relIndi.XRef);

                        summary.Add("    " + ast.Relation + " " + HyperLink(xref, nm));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListAssociationsRefresh()", ex);
            }
        }

        private static void RecListIndividualEventsRefresh(IBaseContext baseContext, GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasEvents) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Events) + ":");

                    int num = record.Events.Count;
                    for (int i = 0; i < num; i++) {
                        summary.Add("");

                        GDMCustomEvent evt = record.Events[i];
                        string st = GetEventName(evt);

                        string sv = GetFactValueStr(evt);
                        if (!string.IsNullOrEmpty(sv)) {
                            sv += ", ";
                        }
                        summary.Add("  " + st + ": " + sv + GetEventDesc(baseContext.Tree, evt));

                        ShowDetailCause(evt, summary);
                        if (evt.HasAddress) {
                            ShowAddressSummary(evt.Address, summary);
                        }

                        RecListSourcesRefresh(baseContext, evt, summary, "    ");
                        RecListNotesRefresh(baseContext, evt, summary, "    ");
                        RecListMediaRefresh(baseContext, evt, summary, "    ");
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListIndividualEventsRefresh()", ex);
            }
        }

        public static string GetFactValueStr(GDMCustomEvent evt)
        {
            string result = evt.StringValue;
            if (result.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                result = HyperLink(result, result);
            }
            return result;
        }

        private static void RecListFamilyEventsRefresh(IBaseContext baseContext, GDMFamilyRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasEvents) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Events) + ":");

                    int num = record.Events.Count;
                    for (int i = 0; i < num; i++) {
                        summary.Add("");

                        GDMFamilyEvent evt = (GDMFamilyEvent)record.Events[i];

                        string st = GetEventName(evt);
                        summary.Add("  " + st + ": " + GetEventDesc(baseContext.Tree, evt));

                        ShowDetailCause(evt, summary);
                        RecListSourcesRefresh(baseContext, evt, summary, "    ");
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListFamilyEventsRefresh()", ex);
            }
        }

        private static void RecListGroupsRefresh(IBaseContext baseContext, GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasGroups) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.RPGroups) + ":");

                    int num = record.Groups.Count;
                    for (int i = 0; i < num; i++) {
                        GDMPointer ptr = record.Groups[i];
                        GDMGroupRecord grp = baseContext.Tree.GetPtrValue<GDMGroupRecord>(ptr);
                        if (grp == null) continue;

                        summary.Add("    " + HyperLink(grp.XRef, grp.GroupName));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListGroupsRefresh()", ex);
            }
        }

        public static void ShowFamilyInfo(IBaseContext baseContext, GDMFamilyRecord familyRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (familyRec != null) {
                        summary.Add("");

                        GDMIndividualRecord spRec = baseContext.Tree.GetPtrValue(familyRec.Husband);
                        string st = ((spRec == null) ? LangMan.LS(LSID.UnkMale) : HyperLink(spRec.XRef, GetNameString(spRec, false)));
                        summary.Add(LangMan.LS(LSID.Husband) + ": " + st + GetLifeStr(spRec));

                        spRec = baseContext.Tree.GetPtrValue(familyRec.Wife);
                        st = ((spRec == null) ? LangMan.LS(LSID.UnkFemale) : HyperLink(spRec.XRef, GetNameString(spRec, false)));
                        summary.Add(LangMan.LS(LSID.Wife) + ": " + st + GetLifeStr(spRec));

                        summary.Add("");
                        if (familyRec.Children.Count != 0) {
                            summary.Add(LangMan.LS(LSID.Childs) + ":");
                        }

                        int num = familyRec.Children.Count;
                        for (int i = 0; i < num; i++) {
                            var child = baseContext.Tree.GetPtrValue(familyRec.Children[i]);
                            summary.Add("    " + HyperLink(child.XRef, GetNameString(child, false)) + GetLifeStr(child));
                        }
                        summary.Add("");

                        RecListFamilyEventsRefresh(baseContext, familyRec, summary);
                        RecListNotesRefresh(baseContext, familyRec, summary);
                        RecListMediaRefresh(baseContext, familyRec, summary);
                        RecListSourcesRefresh(baseContext, familyRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowFamilyInfo()", ex);
            }
        }

        public static void ShowGroupInfo(IBaseContext baseContext, GDMGroupRecord groupRec, StringList summary)
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
                        summary.Add(LangMan.LS(LSID.Members) + " (" + groupRec.Members.Count.ToString() + "):");

                        int num = groupRec.Members.Count;
                        for (int i = 0; i < num; i++) {
                            GDMPointer ptr = groupRec.Members[i];
                            var member = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(ptr);

                            mbrList.AddObject(GetNameString(member, false), member);
                        }
                        mbrList.Sort();

                        int num2 = mbrList.Count;
                        for (int i = 0; i < num2; i++) {
                            GDMIndividualRecord member = (GDMIndividualRecord)mbrList.GetObject(i);

                            summary.Add("    " + HyperLink(member.XRef, mbrList[i]));
                        }

                        RecListNotesRefresh(baseContext, groupRec, summary);
                        RecListMediaRefresh(baseContext, groupRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                    mbrList.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowGroupInfo()", ex);
            }
        }

        public static void ShowMultimediaInfo(IBaseContext baseContext, GDMMultimediaRecord mediaRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (mediaRec != null) {
                        GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
                        string mediaTitle = (fileRef == null) ? LangMan.LS(LSID.Unknown) : fileRef.Title;

                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + mediaTitle + "[/size][/b][/u]");
                        summary.Add("");
                        if (fileRef != null) {
                            summary.Add("( " + HyperLink(GKData.INFO_HREF_VIEW + mediaRec.XRef, LangMan.LS(LSID.View)) + " )");
                        }

                        ShowSubjectLinks(baseContext.Tree, mediaRec, summary);

                        RecListNotesRefresh(baseContext, mediaRec, summary);
                        RecListSourcesRefresh(baseContext, mediaRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowMultimediaInfo()", ex);
            }
        }

        private static void AddILines(this StringList summary, GDMLines lines, string indent = "")
        {
            if (lines == null) return;

            for (int k = 0, num2 = lines.Count; k < num2; k++) {
                summary.Add(indent + MakeLinks(lines[k]));
            }
        }

        public static void ShowNoteInfo(IBaseContext baseContext, GDMNoteRecord noteRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (noteRec != null) {
                        summary.Add("");
                        summary.AddILines(noteRec.Lines, "");

                        ShowSubjectLinks(baseContext.Tree, noteRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowNoteInfo()", ex);
            }
        }

        public static void ShowParentsInfo(GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            if (summary == null) return;

            try {
                for (int p = 0; p < iRec.ChildToFamilyLinks.Count; p++) {
                    var ctfLink = iRec.ChildToFamilyLinks[p];
                    var famRec = tree.GetPtrValue(ctfLink);

                    GDMIndividualRecord father, mother;
                    tree.GetSpouses(famRec, out father, out mother);

                    if (father != null || mother != null) {
                        var plType = ctfLink.PedigreeLinkageType;
                        string linkType =
                            (plType == GDMPedigreeLinkageType.plNone || plType == GDMPedigreeLinkageType.plBirth) ?
                            string.Empty : string.Format(" ({0})", LangMan.LS(GKData.ParentTypes[(int)plType]));

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Parents) + linkType + ":");

                        string st;

                        st = (father == null) ? LangMan.LS(LSID.UnkMale) : HyperLink(father.XRef, GetNameString(father, false));
                        summary.Add("  " + LangMan.LS(LSID.Father) + ": " + st + GetLifeStr(father));

                        st = (mother == null) ? LangMan.LS(LSID.UnkFemale) : HyperLink(mother.XRef, GetNameString(mother, false));
                        summary.Add("  " + LangMan.LS(LSID.Mother) + ": " + st + GetLifeStr(mother));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowParentsInfo()", ex);
            }
        }

        public static void ShowSpousesInfo(IBaseContext baseContext, GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            if (summary == null) return;

            try {
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                    if (family == null) continue;
                    if (!baseContext.IsRecordAccess(family.Restriction)) continue;

                    string st;
                    GDMIndividualRecord spRec;
                    string unk;
                    if (iRec.Sex == GDMSex.svMale) {
                        spRec = tree.GetPtrValue(family.Wife);
                        st = LangMan.LS(LSID.Wife) + ": ";
                        unk = LangMan.LS(LSID.UnkFemale);
                    } else {
                        spRec = tree.GetPtrValue(family.Husband);
                        st = LangMan.LS(LSID.Husband) + ": ";
                        unk = LangMan.LS(LSID.UnkMale);
                    }
                    string marr = GetMarriageDateStr(family, GlobalOptions.Instance.DefDateFormat);
                    if (marr != "") {
                        marr = LangMan.LS(LSID.LMarriage) + " " + marr;
                    } else {
                        marr = LangMan.LS(LSID.LFamily);
                    }

                    summary.Add("");
                    if (spRec != null) {
                        st = st + HyperLink(spRec.XRef, GetNameString(spRec, false)) + " (" + HyperLink(family.XRef, marr) + ")";
                    } else {
                        st = st + unk + " (" + HyperLink(family.XRef, marr) + ")";
                    }
                    summary.Add(st);

                    int chNum = family.Children.Count;
                    if (chNum != 0) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Childs) + ":");

                        for (int k = 0; k < chNum; k++) {
                            GDMIndividualRecord child = tree.GetPtrValue(family.Children[k]);
                            if (child == null) continue;

                            summary.Add("    " + HyperLink(child.XRef, GetNameString(child, false)) + GetLifeStr(child));
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSpousesInfo()", ex);
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
                        GDMTree tree = baseContext.Tree;

                        summary.Add("");

                        bool firstSurname = GlobalOptions.Instance.SurnameFirstInOrder;
                        for (int i = 0; i < iRec.PersonalNames.Count; i++) {
                            var persName = iRec.PersonalNames[i];
                            summary.Add("[u][b][size=+1]" + GetNameString(iRec, persName, firstSurname, true) + "[/size][/u][/b]");
                        }

                        summary.Add(LangMan.LS(LSID.Sex) + ": " + SexStr(iRec.Sex));

                        ShowParentsInfo(tree, iRec, summary);
                        ShowSpousesInfo(baseContext, tree, iRec, summary);

                        RecListIndividualEventsRefresh(baseContext, iRec, summary);
                        RecListNotesRefresh(baseContext, iRec, summary);
                        RecListMediaRefresh(baseContext, iRec, summary);
                        RecListSourcesRefresh(baseContext, iRec, summary);
                        RecListAssociationsRefresh(baseContext, iRec, summary);
                        RecListGroupsRefresh(baseContext, iRec, summary);

                        ShowPersonNamesakes(tree, iRec, summary);
                        ShowPersonExtInfo(tree, iRec, summary);

                        ShowRFN(iRec, summary);

                        summary.Add("");
                        summary.Add(HyperLink(GKData.INFO_HREF_EXPAND_ASSO + iRec.XRef, "[ + ] " + LangMan.LS(LSID.Associations)));
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowPersonInfo()", ex);
            }
        }

        private static void ShowRFN(GDMIndividualRecord iRec, StringList summary)
        {
            var rfnTag = iRec.FindTag("RFN", 0);
            if (rfnTag == null) return;

            var rfnVal = rfnTag.StringValue;
            if (string.IsNullOrEmpty(rfnVal)) return;

            var parts = rfnVal.Split(new char[] { ':' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length < 2) return;

            var resourceId = parts[0];
            var recordId = parts[1];

            var res = AppHost.ExtResources.FindURL(resourceId);
            if (res == null) return;

            var fullURL = res.URL + recordId;
            summary.Add("");
            summary.Add(HyperLink(fullURL, string.Format("{0}: {1}", res.Name, fullURL)));
        }

        private static void AddQValue(this StringList summary, string name, string value)
        {
            if (value == null) return;

            value = value.Trim();
            if (string.IsNullOrEmpty(value)) return;

            summary.AddMultiline(name + ": \"" + MakeLinks(value) + "\"");
        }

        public static void ShowSourceInfo(IBaseContext baseContext, GDMSourceRecord sourceRec, StringList summary, RecordContentType contentType)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (sourceRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + sourceRec.ShortTitle + "[/size][/b][/u]");
                        summary.Add("");
                        summary.AddQValue(LangMan.LS(LSID.Author), sourceRec.Originator.Lines.Text);
                        summary.AddQValue(LangMan.LS(LSID.Title), sourceRec.Title.Lines.Text);
                        summary.AddQValue(LangMan.LS(LSID.Publication), sourceRec.Publication.Lines.Text);
                        summary.AddQValue(LangMan.LS(LSID.Text), sourceRec.Text.Lines.Text);

                        if (sourceRec.RepositoryCitations.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPRepositories) + ":");

                            int num = sourceRec.RepositoryCitations.Count;
                            for (int i = 0; i < num; i++) {
                                GDMRepositoryRecord rep = baseContext.Tree.GetPtrValue<GDMRepositoryRecord>(sourceRec.RepositoryCitations[i]);

                                summary.Add("    " + HyperLink(rep.XRef, rep.RepositoryName));
                            }
                        }

                        ShowSubjectLinks(baseContext.Tree, sourceRec, summary, true);

                        RecListNotesRefresh(baseContext, sourceRec, summary);
                        RecListMediaRefresh(baseContext, sourceRec, summary);

                        summary.Add("");
                        summary.Add("");
                        if (contentType == RecordContentType.Full) {
                            summary.Add("[ " + HyperLink(GKData.INFO_HREF_FILTER_INDI + sourceRec.XRef, LangMan.LS(LSID.MIFilter)) + " ]");
                            summary.Add("");
                        }
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSourceInfo()", ex);
            }
        }

        public static void ShowRepositoryInfo(IBaseContext baseContext, GDMRepositoryRecord repositoryRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (repositoryRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + MakeLinks(repositoryRec.RepositoryName.Trim()) + "[/size][/b][/u]");
                        summary.Add("");

                        if (repositoryRec.HasAddress)
                            ShowAddressSummary(repositoryRec.Address, summary);

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.RPSources) + ":");

                        var sortedSources = new LinksList();
                        GDMTree tree = baseContext.Tree;
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++) {
                            GDMRecord rec = tree[i];

                            if (rec.RecordType == GDMRecordType.rtSource) {
                                GDMSourceRecord srcRec = (GDMSourceRecord)rec;

                                int num2 = srcRec.RepositoryCitations.Count;
                                for (int j = 0; j < num2; j++) {
                                    if (srcRec.RepositoryCitations[j].XRef == repositoryRec.XRef) {
                                        sortedSources.Add(GenRecordLinkTuple(baseContext.Tree, srcRec, false));
                                    }
                                }
                            }
                        }
                        sortedSources.Sort();
                        foreach (var tpl in sortedSources) {
                            summary.Add("    " + tpl.Item2);
                        }

                        RecListNotesRefresh(baseContext, repositoryRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowRepositoryInfo()", ex);
            }
        }

        public static void ShowResearchInfo(IBaseContext baseContext, GDMResearchRecord researchRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (researchRec != null) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Title) + ": [u][b][size=+1]\"" + researchRec.ResearchName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)researchRec.Priority]));
                        summary.Add(LangMan.LS(LSID.Status) + ": " + LangMan.LS(GKData.StatusNames[(int)researchRec.Status]) + " (" + researchRec.Percent.ToString() + "%)");
                        summary.Add(LangMan.LS(LSID.StartDate) + ": " + researchRec.StartDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));
                        summary.Add(LangMan.LS(LSID.StopDate) + ": " + researchRec.StopDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        if (researchRec.Tasks.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPTasks) + ":");

                            int num = researchRec.Tasks.Count;
                            for (int i = 0; i < num; i++) {
                                var taskRec = baseContext.Tree.GetPtrValue<GDMTaskRecord>(researchRec.Tasks[i]);
                                summary.Add("    " + GenRecordLink(baseContext.Tree, taskRec, false));
                            }
                        }

                        if (researchRec.Communications.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPCommunications) + ":");

                            int num2 = researchRec.Communications.Count;
                            for (int i = 0; i < num2; i++) {
                                var corrRec = baseContext.Tree.GetPtrValue<GDMCommunicationRecord>(researchRec.Communications[i]);
                                summary.Add("    " + GenRecordLink(baseContext.Tree, corrRec, false));
                            }
                        }

                        if (researchRec.Groups.Count != 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPGroups) + ":");

                            int num3 = researchRec.Groups.Count;
                            for (int i = 0; i < num3; i++) {
                                var grp = baseContext.Tree.GetPtrValue<GDMGroupRecord>(researchRec.Groups[i]);
                                summary.Add("    " + HyperLink(grp.XRef, grp.GroupName));
                            }
                        }

                        RecListNotesRefresh(baseContext, researchRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowResearchInfo()", ex);
            }
        }

        public static void ShowTaskInfo(IBaseContext baseContext, GDMTaskRecord taskRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (taskRec != null) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Goal) + ": [u][b][size=+1]" + GetTaskGoalStr(baseContext.Tree, taskRec) + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]));
                        summary.Add(LangMan.LS(LSID.StartDate) + ": " + taskRec.StartDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));
                        summary.Add(LangMan.LS(LSID.StopDate) + ": " + taskRec.StopDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        RecListNotesRefresh(baseContext, taskRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowTaskInfo()", ex);
            }
        }

        public static void ShowCommunicationInfo(IBaseContext baseContext, GDMCommunicationRecord commRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (commRec != null) {
                        GDMTree tree = baseContext.Tree;

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Theme) + ": [u][b][size=+1]\"" + commRec.CommName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Corresponder) + ": " + GetCorresponderStr(tree, commRec, true));
                        summary.Add(LangMan.LS(LSID.Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]));
                        summary.Add(LangMan.LS(LSID.Date) + ": " + commRec.Date.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        RecListNotesRefresh(baseContext, commRec, summary);
                        RecListMediaRefresh(baseContext, commRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowCommunicationInfo()", ex);
            }
        }

        public static void ShowLocationInfo(IBaseContext baseContext, GDMLocationRecord locRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                summary.Clear();

                StringList linkList = null;
                try {
                    if (locRec == null) return;

                    summary.Add("");

                    GlobalOptions glob = GlobalOptions.Instance;
                    for (int i = 0; i < locRec.Names.Count; i++) {
                        var locName = locRec.Names[i];
                        summary.Add("[u][b][size=+1]" + locName.StringValue + "[/size][/b][/u]");

                        string st = locName.Abbreviation;
                        if (!string.IsNullOrEmpty(st)) {
                            summary.Add("    " + st);
                        }

                        st = locName.Date.GetDisplayStringExt(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
                        if (!string.IsNullOrEmpty(st)) {
                            summary.Add("    " + st);
                        }

                        summary.Add("");
                    }

                    GDMTree tree = baseContext.Tree;

                    if (locRec.TopLevels.Count > 0) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.TopLevelLinks) + ":");

                        for (int i = 0; i < locRec.TopLevels.Count; i++) {
                            var topLev = locRec.TopLevels[i];
                            var topLoc = tree.GetPtrValue<GDMLocationRecord>(topLev);

                            string st = HyperLink(topLev.XRef, topLoc.GetNameByDate(topLev.Date.Value));
                            if (!string.IsNullOrEmpty(st)) {
                                summary.Add("    " + st);
                            }

                            st = topLev.Date.GetDisplayStringExt(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
                            if (!string.IsNullOrEmpty(st)) {
                                summary.Add("    " + st);
                            }

                            summary.Add("");
                        }
                    }

                    summary.Add(LangMan.LS(LSID.Latitude) + ": " + locRec.Map.Lati);
                    summary.Add(LangMan.LS(LSID.Longitude) + ": " + locRec.Map.Long);

                    var fullNames = locRec.GetFullNames(tree, ATDEnumeration.fStL, glob.EL_AbbreviatedNames);
                    if (fullNames.Count > 0) {
                        //linkList.Sort();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.History) + ":");

                        int num = fullNames.Count;
                        for (int i = 0; i < num; i++) {
                            var xName = fullNames[i];
                            summary.Add("    " + string.Format("{0}: {1}", xName.Date.GetDisplayStringExt(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar, false), xName.StringValue));
                        }
                    }

                    linkList = GetLocationLinks(tree, locRec);
                    if (linkList.Count > 0) {
                        linkList.Sort();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Links) + ":");

                        int num = linkList.Count;
                        for (int i = 0; i < num; i++) {
                            GDMRecord rec = linkList.GetObject(i) as GDMRecord;
                            summary.Add("    " + HyperLink(rec.XRef, linkList[i]));
                        }
                    }

                    RecListNotesRefresh(baseContext, locRec, summary);
                    RecListMediaRefresh(baseContext, locRec, summary);

                    var subLinks = GetLocationSubordinatesList(tree, locRec);
                    if (subLinks.Count > 0) {
                        subLinks.Sort();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.SubordinateLocationsLinks) + ":");

                        for (int i = 0, num = subLinks.Count; i < num; i++) {
                            GDMRecord rec = subLinks.GetObject(i) as GDMRecord;
                            summary.Add("    " + HyperLink(rec.XRef, subLinks[i]));
                        }
                    }

                    summary.Add("");
                    summary.Add(HyperLink(GKData.INFO_HREF_LOC_SUB + locRec.XRef, $"[ {LangMan.LS(LSID.MapOfPlaces)} ] "));

                    summary.Add("");
                    summary.Add(HyperLink(GKData.INFO_HREF_LOC_INDI + locRec.XRef, $"[ {LangMan.LS(LSID.MapOfPersons)} ] "));
                } finally {
                    if (linkList != null) linkList.Dispose();
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowLocationInfo()", ex);
            }
        }

        public static void GetRecordContent(IBaseContext baseContext, GDMRecord record, StringList ctx, RecordContentType contentType)
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
                            GKUtils.ShowNoteInfo(baseContext, record as GDMNoteRecord, ctx);
                            break;

                        case GDMRecordType.rtMultimedia:
                            GKUtils.ShowMultimediaInfo(baseContext, record as GDMMultimediaRecord, ctx);
                            break;

                        case GDMRecordType.rtSource:
                            GKUtils.ShowSourceInfo(baseContext, record as GDMSourceRecord, ctx, contentType);
                            break;

                        case GDMRecordType.rtRepository:
                            GKUtils.ShowRepositoryInfo(baseContext, record as GDMRepositoryRecord, ctx);
                            break;

                        case GDMRecordType.rtGroup:
                            GKUtils.ShowGroupInfo(baseContext, record as GDMGroupRecord, ctx);
                            break;

                        case GDMRecordType.rtResearch:
                            GKUtils.ShowResearchInfo(baseContext, record as GDMResearchRecord, ctx);
                            break;

                        case GDMRecordType.rtTask:
                            GKUtils.ShowTaskInfo(baseContext, record as GDMTaskRecord, ctx);
                            break;

                        case GDMRecordType.rtCommunication:
                            GKUtils.ShowCommunicationInfo(baseContext, record as GDMCommunicationRecord, ctx);
                            break;

                        case GDMRecordType.rtLocation:
                            GKUtils.ShowLocationInfo(baseContext, record as GDMLocationRecord, ctx);
                            break;
                    }
                } catch (Exception ex) {
                    Logger.WriteError("GKUtils.GetRecordContext()", ex);
                }
            }
        }

        public static string GetSourceRepositories(GDMTree tree, GDMSourceRecord sourceRecord)
        {
            string repositories = string.Empty;
            foreach (GDMRepositoryCitation repoCit in sourceRecord.RepositoryCitations) {
                GDMRepositoryRecord repoRec = tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                if (repoRec == null || string.IsNullOrEmpty(repoRec.RepositoryName)) continue;

                if (repositories.Length != 0) {
                    repositories += ", ";
                }
                repositories += repoRec.RepositoryName;
            }
            return repositories;
        }

        #endregion

        #region Multimedia support (static)

        public static string GetStoreFolder(MultimediaKind mmKind)
        {
            string result = "";
            switch (mmKind) {
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

                case MultimediaKind.mkOffice:
                    result = "office";
                    break;

                case MultimediaKind.mkArchive:
                    result = "archives";
                    break;
            }
            return result + Path.DirectorySeparatorChar;
        }

        public static MultimediaKind GetMultimediaKind(GDMMultimediaFormat format)
        {
            switch (format) {
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
                case GDMMultimediaFormat.mfWEBP: // .net isn't supports
                    return MultimediaKind.mkImage;

                case GDMMultimediaFormat.mfTXT:
                case GDMMultimediaFormat.mfRTF:
                case GDMMultimediaFormat.mfHTM:
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

                case GDMMultimediaFormat.mfPDF:
                case GDMMultimediaFormat.mfDJVU:
                case GDMMultimediaFormat.mfDOC:
                case GDMMultimediaFormat.mfDOCX:
                case GDMMultimediaFormat.mfXLS:
                case GDMMultimediaFormat.mfXLSX:
                case GDMMultimediaFormat.mfPPT:
                case GDMMultimediaFormat.mfPPTX:
                case GDMMultimediaFormat.mfODT:
                case GDMMultimediaFormat.mfODS:
                case GDMMultimediaFormat.mfODP:
                    return MultimediaKind.mkOffice;

                case GDMMultimediaFormat.mfZIP:
                case GDMMultimediaFormat.mfRAR:
                case GDMMultimediaFormat.mf7Z:
                    return MultimediaKind.mkArchive;

                case GDMMultimediaFormat.mfOLE:
                case GDMMultimediaFormat.mfUnknown:
                default:
                    return MultimediaKind.mkNone;
            }
        }

        public static MediaStoreType GetStoreTypeEx(string fileRef)
        {
            if (fileRef == null)
                throw new ArgumentNullException("fileReference");

            MediaStoreType result = MediaStoreType.mstReference;
            for (int i = 1; i <= 4; i++) {
                if (fileRef.StartsWith(GKData.GKStoreTypes[i].Sign, StringComparison.Ordinal)) {
                    result = (MediaStoreType)i;
                    break;
                }
            }
            return result;
        }

        public static MediaStore GetStoreType(GDMFileReference fileReference)
        {
            if (fileReference == null)
                throw new ArgumentNullException("fileReference");

            string fileName = fileReference.StringValue;
            MediaStoreType storeType = GetStoreTypeEx(fileName);

            if (storeType != MediaStoreType.mstReference && storeType != MediaStoreType.mstURL) {
                fileName = fileName.Remove(0, 4);
            }

            return new MediaStore(storeType, fileName);
        }

        public static bool UseEmbeddedViewer(GDMMultimediaFormat format)
        {
            MultimediaKind mmKind = GetMultimediaKind(format);

            switch (mmKind) {
                case MultimediaKind.mkImage:
                    return !(format == GDMMultimediaFormat.mfPCX || format == GDMMultimediaFormat.mfTGA ||
                             format == GDMMultimediaFormat.mfRAW || format == GDMMultimediaFormat.mfPSD);

                case MultimediaKind.mkVideo:
                case MultimediaKind.mkAudio:
                    return GlobalOptions.Instance.EmbeddedMediaPlayer && AppHost.Instance.HasFeatureSupport(Feature.MediaPlayer);

                case MultimediaKind.mkText:
                    return (format == GDMMultimediaFormat.mfTXT || format == GDMMultimediaFormat.mfRTF ||
                            format == GDMMultimediaFormat.mfHTM);

                case MultimediaKind.mkOffice:
                case MultimediaKind.mkArchive:
                case MultimediaKind.mkNone:
                default:
                    return false;
            }
        }

        public static bool IsPictureFormat(GDMFileReferenceWithTitle fileRef)
        {
            if (fileRef == null)
                return false;

            MultimediaKind mmKind = GetMultimediaKind(fileRef.GetMultimediaFormat());
            return (mmKind == MultimediaKind.mkImage);
        }

        public static bool MayContainPortrait(GDMMultimediaRecord mmRec)
        {
            if (mmRec == null || mmRec.FileReferences.Count == 0)
                return false;

            var mmRef = mmRec.FileReferences[0];
            if (!IsPictureFormat(mmRef))
                return false;

            switch (mmRef.MediaType) {
                case GDMMediaType.mtUnknown:
                case GDMMediaType.mtAudio:
                case GDMMediaType.mtManuscript:
                case GDMMediaType.mtMap:
                    return false;

                case GDMMediaType.mtBook:
                case GDMMediaType.mtCard:
                case GDMMediaType.mtElectronic:
                case GDMMediaType.mtFiche:
                case GDMMediaType.mtFilm:
                case GDMMediaType.mtMagazine:
                case GDMMediaType.mtNewspaper:
                case GDMMediaType.mtPhoto:
                case GDMMediaType.mtTombstone:
                case GDMMediaType.mtVideo:
                default:
                    return true;
            }
        }

        public static void InitSecurityProtocol()
        {
            try {
                // SecurityProtocolType.Ssl3 is not supported
#if MONO
                // Mono v4.6 doesn't contain SecurityProtocolType.Tls11
                const int Tls11 = 768;
                const int Tls12 = 3072;
                ServicePointManager.SecurityProtocol =
                    ServicePointManager.SecurityProtocol | SecurityProtocolType.Tls | (SecurityProtocolType)Tls11 | (SecurityProtocolType)Tls12;
#else
                ServicePointManager.SecurityProtocol =
                    ServicePointManager.SecurityProtocol | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
#endif
            } catch (Exception ex) {
                // crash on WinXP (.NET <= 4.0.3), TLS 1.2 not supported
                Logger.WriteError("GKUtils.InitSecurityProtocol()", ex);
            }
        }

        public static byte[] GetWebData(string uri)
        {
            InitSecurityProtocol();

            using (var webClient = new WebClient()) {
                webClient.Headers["User-Agent"] = string.Format("{0}/{1}", GKData.APP_TITLE, GKData.APP_VERSION);
                byte[] dataBytes = webClient.DownloadData(uri);
                return dataBytes;
            }
        }

        public static Stream GetWebStream(string uri)
        {
            byte[] dataBytes = GetWebData(uri);
            var ms = new MemoryStream();
            ms.Write(dataBytes, 0, dataBytes.Length);
            return ms;
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

        public static string UniformName(string val)
        {
            if (string.IsNullOrEmpty(val)) {
                return null;
            }

            StringBuilder str = new StringBuilder(val.ToLower());

            bool prevDelimiter = true;
            for (int i = 0; i < str.Length; i++) {
                char chr = str[i];
                if (chr == ' ' || chr == '-') {
                    prevDelimiter = true;
                } else {
                    if (prevDelimiter) {
                        str[i] = char.ToUpper(chr);
                        prevDelimiter = false;
                    }
                }
            }

            return str.ToString();
        }

        public static string GetFamilyString(GDMTree tree, GDMFamilyRecord family)
        {
            if (family == null)
                throw new ArgumentNullException("family");

            return GetFamilyString(tree, family, LangMan.LS(LSID.UnkMale), LangMan.LS(LSID.UnkFemale));
        }

        public static string GetFamilyString(GDMTree tree, GDMFamilyRecord family, string unkHusband, string unkWife)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (family == null)
                throw new ArgumentNullException("family");

            string husband, wife;

            GDMIndividualRecord spouse = tree.GetPtrValue(family.Husband);
            if (spouse == null) {
                husband = (unkHusband == null) ? "?" : unkHusband;
            } else {
                husband = GetNameString(spouse, false);
            }

            spouse = tree.GetPtrValue(family.Wife);
            if (spouse == null) {
                wife = (unkWife == null) ? "?" : unkWife;
            } else {
                wife = GetNameString(spouse, false);
            }

            return string.Concat(husband, " - ", wife);
        }

        public static string GetNickString(GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string result = (iRec.PersonalNames.Count > 0) ? iRec.PersonalNames[0].Nickname : string.Empty;
            return result;
        }

        public static string GetFmtSurname(GDMSex iSex, GDMPersonalName personalName, string defSurname)
        {
            if (personalName == null)
                throw new ArgumentNullException("personalName");

            string result;

            var globOpts = GlobalOptions.Instance;

            WomanSurnameFormat wsFmt = globOpts.WomanSurnameFormat;
            bool simpleSingle = globOpts.SimpleSingleSurnames;

            if (iSex == GDMSex.svFemale && wsFmt != WomanSurnameFormat.wsfNotExtend) {
                string marriedSurname = personalName.MarriedName;
                switch (wsFmt) {
                    case WomanSurnameFormat.wsfMaiden_Married:
                        result = defSurname;
                        simpleSingle = simpleSingle && string.IsNullOrEmpty(result);
                        if (marriedSurname.Length > 0) {
                            if (!simpleSingle) {
                                string op = (result.Length > 0) ? " (" : "(";
                                result = string.Concat(result, op, marriedSurname, ")");
                            } else {
                                result = marriedSurname;
                            }
                        }
                        break;

                    case WomanSurnameFormat.wsfMarried_Maiden:
                        result = marriedSurname;
                        simpleSingle = simpleSingle && string.IsNullOrEmpty(result);
                        if (defSurname.Length > 0) {
                            if (!simpleSingle) {
                                string op = (result.Length > 0) ? " (" : "(";
                                result = string.Concat(result, op, defSurname, ")");
                            } else {
                                result = defSurname;
                            }
                        }
                        break;

                    case WomanSurnameFormat.wsfMaiden:
                        result = !string.IsNullOrEmpty(defSurname) ? defSurname : marriedSurname; // by default GEDCOMPersonalName.Surname is maiden surname
                        break;

                    case WomanSurnameFormat.wsfMarried:
                        result = !string.IsNullOrEmpty(marriedSurname) ? marriedSurname : defSurname;
                        break;

                    default:
                        result = defSurname;
                        break;
                }
            } else {
                result = defSurname;
            }

            if (globOpts.SurnameInCapitals) {
                result = result.ToUpper();
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

            surname = GetFmtSurname(iRec.Sex, np, surname);

            if (firstSurname) {
                result = string.Concat(surname, " ", firstPart);
            } else {
                result = string.Concat(firstPart, " ", surname);
            }

            if (includePieces) {
                string nick = np.Nickname;
                if (!string.IsNullOrEmpty(nick)) result = string.Concat(result, " [", nick, "]");
            }

            return result.Trim();
        }

        private static GDMPersonalName GetPersonalNameByLang(GDMIndividualRecord iRec, GDMLanguageID defLang)
        {
            GDMPersonalName result;

            int count = iRec.PersonalNames.Count;
            if (count == 0) {
                result = null;
            } else if (defLang == GDMLanguageID.Unknown) {
                result = iRec.PersonalNames[0];
            } else {
                result = null;
                for (int i = 0; i < count; i++) {
                    var pn = iRec.PersonalNames[i];
                    if (pn.Language == defLang) {
                        result = pn;
                        break;
                    }
                }
                if (result == null) {
                    result = iRec.PersonalNames[0];
                }
            }

            return result;
        }

        public static string GetNameString(GDMIndividualRecord iRec, bool includePieces, GDMLanguageID defLang = GDMLanguageID.Unknown)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GDMPersonalName pn = GetPersonalNameByLang(iRec, defLang);
            string result = (pn == null) ? string.Empty : GetNameString(iRec, pn, GlobalOptions.Instance.SurnameFirstInOrder, includePieces);
            return result;
        }

        public static string GetNameString(GDMIndividualRecord iRec, bool firstSurname, bool includePieces, GDMLanguageID defLang = GDMLanguageID.Unknown)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GDMPersonalName pn = GetPersonalNameByLang(iRec, defLang);
            string result = (pn == null) ? string.Empty : GetNameString(iRec, pn, firstSurname, includePieces);
            return result;
        }

        public static void SetMarriedSurname(GDMIndividualRecord iRec, string marriedSurname)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GDMPersonalName personalName;
            if (iRec.PersonalNames.Count <= 0) {
                personalName = iRec.AddPersonalName(new GDMPersonalName());
            } else {
                personalName = iRec.PersonalNames[0];
            }

            personalName.MarriedName = marriedSurname.Trim();
        }

        public static void SetNameParts(GDMPersonalName personalName, string surname, string name, string patronymic)
        {
            if (personalName == null)
                throw new ArgumentNullException("personalName");

            personalName.Surname = surname.SafeTrim();
            personalName.Given = name.SafeTrim();
            personalName.PatronymicName = patronymic.SafeTrim();
        }

        public static NamePartsRet GetNameParts(GDMTree tree, GDMIndividualRecord iRec, GDMPersonalName personalName, bool formatted = true)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            ICulture culture = DefineCulture(tree, personalName);

            NamePartsRet nameParts = culture.GetNameParts(personalName);

            if (formatted) {
                nameParts.Surname = GetFmtSurname(iRec.Sex, personalName, nameParts.Surname);
            }

            return nameParts;
        }

        public static NamePartsRet GetNameParts(GDMTree tree, GDMIndividualRecord iRec, bool formatted = true, GDMLanguageID defLang = GDMLanguageID.Unknown)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GDMPersonalName pn = GetPersonalNameByLang(iRec, defLang);
            return (pn == null) ? NamePartsRet.Empty : GetNameParts(tree, iRec, pn, formatted);
        }

        public static ICulture DefineCulture(GDMTree tree, GDMPersonalName personalName)
        {
            // first priority - local langID from name
            GDMLanguageID langID = (personalName != null) ? personalName.Language : GDMLanguageID.Unknown;

            // second priority - global langID from tree
            if (langID == GDMLanguageID.Unknown && tree != null) {
                langID = tree.Header.Language;
            }

            return CulturesPool.DefineCulture(langID);
        }

        public static bool IsMatchesNames(GDMIndividualRecord iRec, Regex regexNames, bool allNames)
        {
            if (!allNames) {
                string recName = GetNameString(iRec, true, false);
                if (MatchesRegex(recName, regexNames)) {
                    return true;
                }
            } else {
                for (int k = 0; k < iRec.PersonalNames.Count; k++) {
                    var persName = iRec.PersonalNames[k];

                    string recName = GetNameString(iRec, persName, true, false);
                    if (MatchesRegex(recName, regexNames)) {
                        return true;
                    }
                }
            }
            return false;
        }

        #endregion

        #region TimeLine filters support

        public static void SetBaseExternalFilter(IBaseWindow baseWindow, FilterLifeMode mode, ExternalFilterHandler filterHandler)
        {
            if (baseWindow != null) {
                IRecordsListModel listMan = baseWindow.GetRecordsListManByType(GDMRecordType.rtIndividual);
                if (listMan != null) {
                    listMan.ExternalFilter = filterHandler;
                    ((IIndividualListFilter)listMan.Filter).FilterLifeMode = mode;
                }
                baseWindow.ApplyFilter(GDMRecordType.rtIndividual);
            }
        }

        public static void SetTimeLineYear(IBaseWindow baseWindow, int year)
        {
            if (baseWindow != null) {
                IRecordsListModel listMan = baseWindow.GetRecordsListManByType(GDMRecordType.rtIndividual);
                if (listMan != null) {
                    ((IndividualListFilter)listMan.Filter).TimeLineYear = year;
                }
            }
        }

        public static void CollectTimeLineData(IBaseWindow baseWindow, out int yearMin, out int yearMax)
        {
            yearMin = 10000;
            yearMax = 0;

            if (baseWindow == null) return;

            var tree = baseWindow.Context.Tree;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];
                if (rec.RecordType != GDMRecordType.rtIndividual) continue;

                GDMIndividualRecord iRec = (GDMIndividualRecord)rec;
                if (!iRec.HasEvents) continue;

                for (int k = 0, evNum = iRec.Events.Count; k < evNum; k++) {
                    GDMCustomEvent ev = iRec.Events[k];
                    var evtType = ev.GetTagType();

                    if (evtType == GEDCOMTagType.BIRT || evtType == GEDCOMTagType.DEAT) {
                        int year = ev.GetChronologicalYear();
                        if (year != 0) {
                            if (yearMin > year) yearMin = year;
                            if (yearMax < year) yearMax = year;
                        }
                    }
                }
            }
        }

        public static bool FilterTimeLine(GDMIndividualRecord iRec, int yearCurrent)
        {
            bool result = true;
            try {
                int bdy = iRec.GetChronologicalYear(GEDCOMTagName.BIRT);
                int ddy = iRec.GetChronologicalYear(GEDCOMTagName.DEAT);

                if (bdy != 0 && ddy == 0) {
                    ddy = bdy + GKData.PROVED_LIFE_LENGTH;
                }

                if (bdy == 0 && ddy != 0) {
                    bdy = ddy - GKData.PROVED_LIFE_LENGTH;
                }

                if (yearCurrent > 0) {
                    result = (yearCurrent >= bdy && yearCurrent <= ddy);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.FilterTimeLine()", ex);
            }
            return result;
        }

        #endregion

        #region Print support

        public static void SplitImage(List<ExtRect> pages, int imageWidth, int imageHeight, int pageWidth, int pageHeight)
        {
            int cols = DivideRoundingUp(imageWidth, pageWidth);
            int rows = DivideRoundingUp(imageHeight, pageHeight);

            for (int r = 0; r < rows; r++) {
                for (int c = 0; c < cols; c++) {
                    int lu_x = c * pageWidth;
                    int lu_y = r * pageHeight;
                    int rb_x = (int)Math.Min(imageWidth, lu_x + pageWidth);
                    int rb_y = (int)Math.Min(imageHeight, lu_y + pageHeight);
                    pages.Add(ExtRect.Create(lu_x, lu_y, rb_x, rb_y));
                }
            }
        }

        public static int DivideRoundingUp(int x, int y)
        {
            int remainder;
            int quotient = Math.DivRem(x, y, out remainder);
            return remainder == 0 ? quotient : quotient + 1;
        }

        #endregion
    }
}
