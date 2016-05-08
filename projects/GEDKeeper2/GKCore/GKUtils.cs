﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Types;
using GKUI.Controls;

namespace GKCore
{
    /// <summary>
    /// Localization: dirty
    /// </summary>
    public static class GKUtils
    {
        #region System functions

        public static bool IsSetBit(uint val, int pos)
        {
            return (val & (1 << pos)) != 0;
        }

        public static void LoadExtFile(string fileName)
        {
            if (File.Exists(fileName))
            {
                #if GK_LINUX
                Process.Start(new ProcessStartInfo("file://"+fileName) { UseShellExecute = true });
                #else
                Process.Start(fileName);
                #endif
            }
        }

        private static void IsRequired(ulong ver)
        {
            //1125908496973824
        }

        public static int GetKeyLayout()
        {
			if (SysInfo.IsUnix()) {
				// There is a bug in Mono: does not work this CurrentInputLanguage
				return CultureInfo.CurrentUICulture.KeyboardLayoutId;
			} else {
				InputLanguage currentLang = InputLanguage.CurrentInputLanguage;
				return currentLang.Culture.KeyboardLayoutId;
			}
        }

        public static void SetKeyLayout(int layout)
        {
            try {
                CultureInfo cultureInfo = new CultureInfo(layout);
                InputLanguage currentLang = InputLanguage.FromCulture(cultureInfo);
                InputLanguage.CurrentInputLanguage = currentLang;
            } catch (Exception ex) {
                Logger.LogWrite("GKUtils.SetKeyLayout(): " + ex.Message);
            }
        }

        public static void SendMail(string address, string subject, string body, string attach)
        {
            string mailto = string.Format("mailto:{0}?Subject={1}&Body={2}&Attach={3}", address, subject, body, "" + attach + ""); // Attach, Attachment
            Process.Start(mailto);
        }

        public static bool IsNetworkAvailable()
        {
            return System.Net.NetworkInformation.NetworkInterface.GetIsNetworkAvailable();
        }

        public static T GetAssemblyAttribute<T>(Assembly assembly) where T : Attribute
        {
            if (assembly == null)
            {
                throw new ArgumentNullException("assembly");
            }

            object[] attributes = assembly.GetCustomAttributes(typeof(T), false);
            if (attributes == null || attributes.Length == 0)
                return null;
            return SingleOrDefault(OfTypeIterator((T[])attributes));
        }
        
        private static IEnumerable<TResult> OfTypeIterator<TResult>(IEnumerable<TResult> source)
        {
            foreach (object current in source)
            {
                if (current is TResult)
                {
                    yield return (TResult)((object)current);
                }
            }
            yield break;
        }
        
        public static TSource SingleOrDefault<TSource>(IEnumerable<TSource> source)
        {
            if (source == null)
            {
                throw new ArgumentNullException("source");
            }

            IList<TSource> list = source as IList<TSource>;

            if (list != null)
            {
                switch (list.Count)
                {
                    case 0:
                        return default(TSource);
                    case 1:
                        return list[0];
                }
            }
            else
            {
                using (IEnumerator<TSource> enumerator = source.GetEnumerator())
                {
                    if (!enumerator.MoveNext())
                    {
                        TSource result = default(TSource);
                        return result;
                    }
                    TSource current = enumerator.Current;
                    if (!enumerator.MoveNext())
                    {
                        TSource result = current;
                        return result;
                    }
                }
            }

            throw new Exception("MoreThanOneElement");
        }

        public static T FirstOrDefault<T>(IList<T> list)
        {
            if (list == null) {
                throw new ArgumentNullException("list");
            }

            return (list.Count > 0) ? list[0] : default(T);
        }

        public static T LastOrDefault<T>(IList<T> list)
        {
            if (list == null) {
                throw new ArgumentNullException("list");
            }

            int count = list.Count;
            return (count > 0) ? list[count - 1] : default(T);
        }

        public static void GetAssemblyVersion(out string copyright, out string version)
        {
            copyright = "";
            version = "";

            Assembly assembly = Assembly.GetExecutingAssembly();

            object[] attributes = assembly.GetCustomAttributes(typeof(AssemblyCopyrightAttribute), false);
            if (attributes.Length != 0) copyright = ((AssemblyCopyrightAttribute)attributes[0]).Copyright;

            version = assembly.GetName().Version.ToString();
        }

        public static OpenFileDialog CreateOpenFileDialog(string title, string context, string filter, int filterIndex, string defaultExt,
                                                          bool multiSelect)
        {
            OpenFileDialog ofd = new OpenFileDialog();
            
            if (!string.IsNullOrEmpty(title))
                ofd.Title = title;

            if (!string.IsNullOrEmpty(context))
                ofd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter))
            {
                ofd.Filter = filter;

                if (filterIndex > 0) ofd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                ofd.DefaultExt = defaultExt;

            ofd.Multiselect = multiSelect;

            return ofd;
        }

        public static SaveFileDialog CreateSaveFileDialog(string title, string context, string filter, int filterIndex, string defaultExt,
                                                          string suggestedFileName)
        {
            SaveFileDialog sfd = new SaveFileDialog();

            if (!string.IsNullOrEmpty(title))
                sfd.Title = title;

            if (!string.IsNullOrEmpty(context))
                sfd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter))
            {
                sfd.Filter = filter;

                if(filterIndex > 0) sfd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                sfd.DefaultExt = defaultExt;

            if (!string.IsNullOrEmpty(suggestedFileName))
                sfd.FileName = suggestedFileName;

            return sfd;
        }

        #endregion

        #region Aux functions

        public static long Trunc(double value)
        {
            return (long)Math.Truncate(value);
        }

        public static double SafeDiv(double dividend, double divisor)
        {
            return (divisor == (double)0f) ? 0.0 : (dividend / divisor);
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

        public static bool IsRecordAccess(GEDCOMRestriction restriction, ShieldState shieldState)
        {
            bool result = false;

            switch (shieldState) {
                case ShieldState.Maximum:
                    result = (restriction != GEDCOMRestriction.rnConfidential && restriction != GEDCOMRestriction.rnPrivacy);
                    break;

                case ShieldState.Middle:
                    result = (restriction != GEDCOMRestriction.rnPrivacy);
                    break;

                case ShieldState.None:
                    result = true;
                    break;
            }

            return result;
        }

        public static string GetFamilyString(GEDCOMFamilyRecord family)
        {
            if (family == null)
            {
                throw new ArgumentNullException("family");
            }

            return family.GetFamilyString(LangMan.LS(LSID.LSID_UnkMale), LangMan.LS(LSID.LSID_UnkFemale));
        }

        public static string MergeStrings(StringList strings)
        {
            if (strings == null)
            {
                throw new ArgumentNullException("strings");
            }

            StringBuilder result = new StringBuilder();

            int num = strings.Count;
            for (int i = 0; i < num; i++)
            {
                if (result.Length != 0) result.Append(" ");
                result.Append(strings[i].Trim());
            }

            return result.ToString();
        }

        public static void GetLocationLinks(GEDCOMTree tree, GEDCOMLocationRecord locRec, ref StringList aList)
        {
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

                        if (evt.Detail.Place.Location.Value == locRec)
                        {
                            aList.Add(GenRecordLink(rec, true) + ", " + GetEventName(evt).ToLower());
                        }
                    }
                }
            }
        }

        public static string HyperLink(string xref, string text, int num)
        {
            string result = "~^" + xref;
            if (text != "")
            {
                result = result + ":" + text;
            }
            result += "~";
            return result;
        }

        public static string GenRecordLink(GEDCOMRecord record, bool signed)
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
                        st = ((GEDCOMIndividualRecord)record).GetNameString(true, false);
                        break;
                    case GEDCOMRecordType.rtFamily:
                        st = GetFamilyString((GEDCOMFamilyRecord)record);
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

                result = HyperLink(record.XRef, sign + st, 0);
            }

            return result;
        }

        public static string GetCorresponderStr(GEDCOMTree tree, GEDCOMCommunicationRecord commRec, bool aLink)
        {
            if (tree == null)
            {
                throw new ArgumentNullException("tree");
            }

            if (commRec == null)
            {
                throw new ArgumentNullException("commRec");
            }

            string result = "";
            GKCommunicationDir dir;
            GEDCOMIndividualRecord corresponder;
            commRec.GetCorresponder(out dir, out corresponder);

            if (corresponder != null)
            {
                string nm = corresponder.GetNameString(true, false);
                if (aLink)
                {
                    nm = HyperLink(corresponder.XRef, nm, 0);
                }
                result = "[" + LangMan.LS(GKData.CommunicationDirs[(int)dir]) + "] " + nm;
            }
            return result;
        }

        public static string GetTaskGoalStr(GEDCOMTaskRecord taskRec)
        {
            if (taskRec == null) return string.Empty;
            
            string result = "";
            
            GKGoalType gt;
            GEDCOMRecord tempRec;
            taskRec.GetTaskGoal(out gt, out tempRec);

            switch (gt) {
                case GKGoalType.gtIndividual:
                case GKGoalType.gtFamily:
                case GKGoalType.gtSource:
                    result = GetGoalStr(gt, tempRec);
                    break;

                case GKGoalType.gtOther:
                    result = taskRec.Goal;
                    break;
            }

            if (gt != GKGoalType.gtOther)
            {
                result = "[" + LangMan.LS(GKData.GoalNames[(int)gt]) + "] " + result;
            }

            return result;
        }

        public static string GetGoalStr(GKGoalType gt, GEDCOMRecord tempRec)
        {
            if (tempRec == null) return string.Empty;

            switch (gt)
            {
                case GKGoalType.gtIndividual:
                    return ((GEDCOMIndividualRecord)tempRec).GetNameString(true, false);

                case GKGoalType.gtFamily:
                    return GetFamilyString(tempRec as GEDCOMFamilyRecord);

                case GKGoalType.gtSource:
                    return ((GEDCOMSourceRecord)tempRec).FiledByEntry;
            }

            return string.Empty;
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
            PersonEventKind res = PersonEventKind.ekFact;

            for (int i = 0; i < GKData.PersonEvents.Length; i++)
            {
                if (GKData.PersonEvents[i].Sign == sign)
                {
                    res = GKData.PersonEvents[i].Kind;
                    break;
                }
            }

            return res;
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
            string result = "";

            if (evt is GEDCOMIndividualEvent || evt is GEDCOMIndividualAttribute)
            {
                int ev = GetPersonEventIndex(evt.Name);
                if (ev == 0) {
                    result = evt.Detail.Classification;
                } else {
                    result = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evt.Name;
                }
            }
            else if (evt is GEDCOMFamilyEvent)
            {
                int ev = GetFamilyEventIndex(evt.Name);
                if (ev == 0) {
                    result = evt.Detail.Classification;
                } else {
                    result = (ev > 0) ? LangMan.LS(GKData.FamilyEvents[ev].Name) : evt.Name;
                }
            }

            return result;
        }

        public static string GetAttributeStr(GEDCOMIndividualAttribute iAttr)
        {
            if (iAttr == null)
            {
                throw new ArgumentNullException("iAttr");
            }

            int idx = GetPersonEventIndex(iAttr.Name);
            string st;
            if (idx == 0)
            {
                st = iAttr.Detail.Classification;
            }
            else
            {
                st = (idx > 0) ? LangMan.LS(GKData.PersonEvents[idx].Name) : iAttr.Name;
            }

            string place = iAttr.Detail.Place.StringValue;
            if (place != "")
            {
                place = " [" + place + "]";
            }
            return st + ": " + iAttr.StringValue + place;
        }

        public static string GetEventDesc(GEDCOMCustomEvent evt, bool hyperLink = true)
        {
            if (evt == null)
            {
                throw new ArgumentNullException("evt");
            }

            string dt = GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
            string place = evt.Detail.Place.StringValue;
            GEDCOMLocationRecord location = evt.Detail.Place.Location.Value as GEDCOMLocationRecord;

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

        public static string GetEventCause(GEDCOMEventDetail eventDetail)
        {
            if (eventDetail == null)
            {
                throw new ArgumentNullException("eventDetail");
            }

            string result = "";

            if (eventDetail.Cause != "")
            {
                result += eventDetail.Cause;
            }

            if (eventDetail.Agency != "")
            {
                if (result != "")
                {
                    result += " ";
                }
                result = result + "[" + eventDetail.Agency + "]";
            }

            return result;
        }

        public static string GetIndividualEventName(GEDCOMCustomEvent evt)
        {
            if (evt == null)
            {
                throw new ArgumentNullException("evt");
            }

            string result;

            int ev = GetPersonEventIndex(evt.Name);
            if (ev == 0)
            {
                result = evt.Detail.Classification;
            }
            else
            {
                result = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evt.Name;
            }

            return result;
        }

        public static string GetFamilyEventName(GEDCOMFamilyEvent evt)
        {
            if (evt == null)
            {
                throw new ArgumentNullException("evt");
            }

            string result;

            int ev = GetFamilyEventIndex(evt.Name);
            if (ev == 0)
            {
                result = evt.Detail.Classification;
            }
            else
            {
                result = (ev > 0) ? LangMan.LS(GKData.FamilyEvents[ev].Name) : evt.Name;
            }

            return result;
        }

        #endregion

        #region Date functions

        public static int DaysBetween(DateTime now, DateTime then)
        {
            TimeSpan span = ((now < then) ? then - now : now - then);
            return span.Days;
        }

        private static readonly ushort[][] MONTH_DAYS = new ushort[][]
        {
            new ushort[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
            new ushort[] { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
        };

        public static ushort DaysInAMonth(ushort year, ushort month)
        {
            return MONTH_DAYS[(month == 2 && DateTime.IsLeapYear(year)) ? 1 : 0][month - 1];
        }

        public static string GetDateFmtString(GEDCOMDate date, DateFormat format, bool includeBC = false)
        {
            if (date == null)
            {
                throw new ArgumentNullException("date");
            }

            string result = "";
            int year;
            ushort month;
            ushort day;
            bool ybc;
            date.GetDateParts(out year, out month, out day, out ybc);

            if (year > 0 || month > 0 || day > 0)
            {
                switch (format) {
                    case DateFormat.dfDD_MM_YYYY:
                        result += day > 0 ? ConvHelper.AdjustNum(day, 2) + "." : "__.";
                        result += month > 0 ? ConvHelper.AdjustNum(month, 2) + "." : "__.";
                        result += year > 0 ? year.ToString().PadLeft(4, '_') : "____";
                        break;

                    case DateFormat.dfYYYY_MM_DD:
                        result += year > 0 ? year.ToString().PadLeft(4, '_') + "." : "____.";
                        result += month > 0 ? ConvHelper.AdjustNum(month, 2) + "." : "__.";
                        result += day > 0 ? ConvHelper.AdjustNum(day, 2) : "__";
                        break;

                    case DateFormat.dfYYYY:
                        if (year > 0) {
                            result = year.ToString().PadLeft(4, '_');
                        }
                        break;
                }
            }

            if (includeBC && ybc) {
                switch (format) {
                    case DateFormat.dfDD_MM_YYYY:
                        result = result + " BC";
                        break;
                    case DateFormat.dfYYYY_MM_DD:
                        result = "BC " + result;
                        break;
                    case DateFormat.dfYYYY:
                        result = "BC " + result;
                        break;
                }
            }

            return result;
        }

        public static string GetCustomDateFmtString(GEDCOMCustomDate date, DateFormat format, bool sign, bool showCalendar)
        {
            string result = "";

            if (date != null)
            {
                if (date is GEDCOMDate)
                {
                    GEDCOMDate dtx = date as GEDCOMDate;
                    result = GetDateFmtString(dtx, format, true);

                    if (dtx is GEDCOMDateApproximated)
                    {
                        if (sign && (dtx as GEDCOMDateApproximated).Approximated != GEDCOMApproximated.daExact) {
                            result = "~ " + result;
                        }
                    }

                    if (showCalendar)
                    {
                        result = result + GKData.Calendars[(int)dtx.DateCalendar];
                    }
                }
                else if (date is GEDCOMDateRange)
                {
                    GEDCOMDateRange range = date as GEDCOMDateRange;

                    if (range.After.StringValue == "" && range.Before.StringValue != "")
                    {
                        result = GetDateFmtString(range.Before, format, true);
                        if (sign) result = "< " + result;
                    }
                    else if (range.After.StringValue != "" && range.Before.StringValue == "")
                    {
                        result = GetDateFmtString(range.After, format, true);
                        if (sign) result += " >";
                    }
                    else if (range.After.StringValue != "" && range.Before.StringValue != "")
                    {
                        result = GetDateFmtString(range.After, format, true) + " - " + GetDateFmtString(range.Before, format, true);
                    }
                }
                else if (date is GEDCOMDatePeriod)
                {
                    GEDCOMDatePeriod period = date as GEDCOMDatePeriod;

                    if (period.DateFrom.StringValue != "" && period.DateTo.StringValue == "")
                    {
                        result = GetDateFmtString(period.DateFrom, format, true);
                        if (sign) result += " >";
                    }
                    else if (period.DateFrom.StringValue == "" && period.DateTo.StringValue != "")
                    {
                        result = GetDateFmtString(period.DateTo, format, true);
                        if (sign) result = "< " + result;
                    }
                    else if (period.DateFrom.StringValue != "" && period.DateTo.StringValue != "")
                    {
                        result = GetDateFmtString(period.DateFrom, format, true) + " - " + GetDateFmtString(period.DateTo, format, true);
                    }
                }
            }

            return result;
        }

        public static string GEDCOMEventToDateStr(GEDCOMCustomEvent evt, DateFormat format, bool sign)
        {
            return (evt == null) ? string.Empty : GetCustomDateFmtString(evt.Detail.Date.Value, format, sign, false);
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
            GEDCOMCustomDate result = ((evt == null) ? null : evt.Detail.Date.Value);
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

        public static string GetPedigreeLifeStr(GEDCOMIndividualRecord iRec, PedigreeFormat fmt)
        {
            if (iRec == null)
            {
                throw new ArgumentNullException("iRec");
            }

            string resStr = "";

            //PedigreeFormat fmt = this.fOptions.PedigreeOptions.Format;

            switch (fmt) {
                case PedigreeFormat.Excess:
                    {
                        string ds = GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
                        if (ds == "")
                        {
                            ds = "?";
                        }
                        resStr += ds;
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
                            resStr = resStr + " - " + ds;
                        }
                    }
                    break;

                case PedigreeFormat.Compact:
                    {
                        string ds = GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
                        string ps = GetBirthPlace(iRec);
                        if (ps != "")
                        {
                            if (ds != "")
                            {
                                ds += ", ";
                            }
                            ds += ps;
                        }
                        if (ds != "")
                        {
                            ds = "*" + ds;
                        }
                        resStr += ds;
                        ds = GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
                        ps = GetDeathPlace(iRec);
                        if (ps != "")
                        {
                            if (ds != "")
                            {
                                ds += ", ";
                            }
                            ds += ps;
                        }
                        if (ds != "")
                        {
                            ds = "+" + ds;
                        }
                        if (ds != "")
                        {
                            resStr = resStr + " " + ds;
                        }
                    }
                    break;
            }
            
            string result;
            if (resStr == "" || resStr == " ") {
                result = "";
            } else {
                result = " (" + resStr + ")";
            }
            return result;
        }

        public static int GetEventsYearsDiff(GEDCOMCustomEvent ev1, GEDCOMCustomEvent ev2, bool currentEnd)
        {
            int result = -1;

            try
            {
                AbsDate dt1 = GEDCOMUtils.GetAbstractDate(ev1);
                AbsDate dt2 = GEDCOMUtils.GetAbstractDate(ev2);

                if (currentEnd && !dt2.IsValid()) {
                    DateTime now = DateTime.Now;
                    dt2 = new AbsDate(now.Year, (ushort)now.Month, (ushort)now.Day, false);
                }

                if (dt1.IsValid() && dt2.IsValid()) {
                    result = Math.Abs(dt2.Year - dt1.Year);
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
                GEDCOMCustomEvent bd;
                GEDCOMCustomEvent dd;
                iRec.GetLifeDates(out bd, out dd);
                
                result = GetEventsYearsDiff(bd, dd, false);
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
                GEDCOMCustomEvent bd;
                GEDCOMCustomEvent dd;
                iRec.GetLifeDates(out bd, out dd);

                if (toYear == -1) {
                    result = GetEventsYearsDiff(bd, dd, dd == null);
                } else {
                    AbsDate birthDate = GEDCOMUtils.GetAbstractDate(bd);
                    if (birthDate.IsValid()) {
                        result = toYear - birthDate.Year;
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
            return ((evt == null) ? null : evt.Detail.Date.Value);
        }

        public static string GetMarriageDateStr(GEDCOMFamilyRecord fRec, DateFormat dateFormat)
        {
            GEDCOMCustomDate date = GetMarriageDate(fRec);
            return (date == null) ? string.Empty : GetCustomDateFmtString(date, dateFormat, false, false);
        }

        public static string GetDaysForBirth(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return string.Empty;

            string result = "";
            try
            {
                GEDCOMCustomEvent evt = iRec.FindEvent("DEAT");
                if (evt == null)
                {
                    evt = iRec.FindEvent("BIRT");
                    if (evt != null)
                    {
                        GEDCOMDate dt = evt.Detail.Date.Value as GEDCOMDate;
                        if (dt != null)
                        {
                            int bdY;
                            ushort bdM;
                            ushort bdD;
                            bool ybc;

                            dt.GetDateParts(out bdY, out bdM, out bdD, out ybc);
                            if (bdM > 0 && bdD > 0)
                            {
                                DateTime dtNow = DateTime.Now.Date;
                                ushort curY = (ushort)dtNow.Year;
                                ushort curM = (ushort)dtNow.Month;
                                ushort curD = (ushort)dtNow.Day;
                                double dt2 = (curY + bdM / 12.0 + bdD / 12.0 / 31.0);
                                double dt3 = (curY + curM / 12.0 + curD / 12.0 / 31.0);
                                if (dt2 < dt3)
                                {
                                    bdY = curY + 1;
                                }
                                else
                                {
                                    bdY = curY;
                                }
                                result = Convert.ToString(DaysBetween(dtNow, new DateTime(bdY, bdM, bdD)));
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.GetDaysForBirth(): " + ex.Message);
            }
            return result;
        }

        #endregion

        #region Places functions

        public static string GetBirthPlace(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return string.Empty;

            GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
            string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
            return result;
        }

        public static string GetDeathPlace(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return string.Empty;

            GEDCOMCustomEvent evt = iRec.FindEvent("DEAT");
            string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
            return result;
        }

        public static string GetResidencePlace(GEDCOMIndividualRecord iRec, bool includeAddress)
        {
            return (iRec == null) ? string.Empty : GetPlaceStr(iRec.FindEvent("RESI"), includeAddress);
        }

        public static string GetPlaceStr(GEDCOMCustomEvent aEvent, bool includeAddress)
        {
            string result;
            if (aEvent == null)
            {
                result = "";
            }
            else
            {
                result = aEvent.Detail.Place.StringValue;
                if (includeAddress)
                {
                    string resi = aEvent.StringValue;
                    string addr = aEvent.Detail.Address.Address.Text.Trim();
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
            }
            return result;
        }

        #endregion

        #region Individual functions

        public static int GetAncestorsCount(GEDCOMIndividualRecord iRec)
        {
            int result = 0;

            if (iRec != null)
            {
                int val = (int)iRec.ExtData;

                if (val < 0)
                {
                    val = 1;
                    if (iRec.ChildToFamilyLinks.Count > 0)
                    {
                        GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
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

                        int num2 = family.Childrens.Count;
                        for (int j = 0; j < num2; j++)
                        {
                            GEDCOMIndividualRecord iChild = family.Childrens[j].Value as GEDCOMIndividualRecord;
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

                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord iChild = family.Childrens[j].Value as GEDCOMIndividualRecord;
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

        public static int GetFirstbornAge(GEDCOMIndividualRecord iRec, out GEDCOMIndividualRecord iChild)
        {
            int result = 0;
            iChild = null;
            if (iRec == null) return result;

            try
            {
                AbsDate firstYear = AbsDate.Empty();

                GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
                if (evt != null)
                {
                    AbsDate parentYear = GEDCOMUtils.GetAbstractDate(evt);

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                        int num2 = family.Childrens.Count;
                        for (int j = 0; j < num2; j++)
                        {
                            GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)family.Childrens[j].Value;

                            evt = child.FindEvent("BIRT");
                            if (evt != null)
                            {
                                AbsDate childYear = GEDCOMUtils.GetAbstractDate(evt);

                                if (!firstYear.IsValid()) {
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

                    if (parentYear.IsValid() && firstYear.IsValid()) {
                        result = (firstYear.Year - parentYear.Year);
                    } else {
                        iChild = null;
                    }
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
                AbsDate firstYear = AbsDate.Empty();

                GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
                if (evt != null)
                {
                    AbsDate mainYear = GEDCOMUtils.GetAbstractDate(evt);

                    int num = iRec.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                        GEDCOMCustomEvent fEvent = family.FindEvent("MARR");
                        if (fEvent != null)
                        {
                            AbsDate spouseYear = GEDCOMUtils.GetAbstractDate(fEvent);

                            if (!firstYear.IsValid()) {
                                firstYear = spouseYear;
                            } else {
                                if (firstYear > spouseYear) {
                                    firstYear = spouseYear;
                                }
                            }
                        }
                    }

                    if (mainYear.IsValid() && firstYear.IsValid()) {
                        result = (firstYear.Year - mainYear.Year);
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
            if (tree == null) {
                throw new ArgumentNullException("tree");
            }

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = tree[i];
                rec.ExtData = null;
            }
        }

        public static void InitExtCounts(GEDCOMTree tree, int value)
        {
            if (tree == null) {
                throw new ArgumentNullException("tree");
            }

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = tree[i];

                if (rec is GEDCOMIndividualRecord) {
                    rec.ExtData = value;
                }
            }
        }

        #endregion
        
        #region Match functions

        public static Regex InitMaskRegex(string mask)
        {
            Regex result = null;

            if  (!string.IsNullOrEmpty(mask))
            {
                string regexStr = "";
                int curPos = 0;
                int len = mask.Length;
                
                while (curPos < len)
                {
                    int I = mask.IndexOfAny("*?".ToCharArray(), curPos);
                    if (I < curPos) break;
                    if (I > curPos) {
                        string part = mask.Substring(curPos, I - curPos);
                        regexStr += Regex.Escape(part);
                    }

                    char c = mask[I];
                    switch (c) {
                        case '*':
                            regexStr += ".*";
                            break;
                        case '?':
                            regexStr += ".";
                            break;
                    }

                    curPos = I + 1;
                }

                if (curPos < len) {
                    string part = mask.Substring(curPos, len - curPos);
                    regexStr += Regex.Escape(part);
                }

                result = new Regex(regexStr, RegexOptions.IgnoreCase);
            }

            return result;
        }

        public static bool MatchesRegex(string str, Regex regex)
        {
            return (regex != null) && regex.IsMatch(str);
        }

        public static bool MatchesMask(string str, string mask)
        {
            Regex regex = InitMaskRegex(mask);
            return MatchesRegex(str, regex);
        }

        #endregion

        #region Folder functions

        public static string GetTempDir()
        {
            return Environment.GetEnvironmentVariable("TEMP") + Path.DirectorySeparatorChar;
        }

        public static string GetAppPath()
        {
            Module[] mods = Assembly.GetExecutingAssembly().GetModules();
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

        #endregion

        #region UI functions

        public static ExtRect GetFormRect(Form form)
        {
            if (form == null) return ExtRect.CreateEmpty();

            int x = form.Left;
            int y = form.Top;
            int w = form.Width;
            int h = form.Height;
            Screen scr = Screen.PrimaryScreen;
            int mw = scr.WorkingArea.Width;
            int mh = scr.WorkingArea.Height;
            if (x < 0) x = 0;
            if (y < 0) y = 0;
            if (w > mw) w = mw;
            if (h > mh) h = mh;
            return ExtRect.Create(x, y, x + w, y + h);
        }

        public static void SetFormRect(Form form, ExtRect rt, FormWindowState winState)
        {
            // check for new and empty struct
            if (form != null && !rt.IsEmpty())
            {
                form.Left = rt.Left;
                form.Top = rt.Top;
                form.Width = rt.GetWidth();
                form.Height = rt.GetHeight();

                form.WindowState = winState;
            }
        }

        public static void ShowMessage(string msg)
        {
            MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
        }

        public static void ShowError(string msg)
        {
            MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxIcon.Hand);
        }

        public static DialogResult ShowQuestion(string msg)
        {
            return MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.YesNo, MessageBoxIcon.Question);
        }

        public static DialogResult ShowWarning(string msg)
        {
            return MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxIcon.Warning);
        }

        public static string RequireFilename(string filter)
        {
            string result = null;

            using (SaveFileDialog dlg = new SaveFileDialog())
            {
                dlg.Filter = filter;
                if (dlg.ShowDialog() == DialogResult.OK) {
                    result = dlg.FileName;
                }
            }

            return result;
        }

        public static GKListView CreateListView(Control parent)
        {
            if (parent == null)
            {
                throw new ArgumentNullException("parent");
            }

            GKListView listView;

            listView = new GKListView();
            listView.HideSelection = false;
            listView.LabelEdit = false;
            listView.FullRowSelect = true;
            listView.View = View.Details;
            listView.Dock = DockStyle.Fill;
            parent.Controls.Add(listView);

            return listView;
        }

        public static GKRecordsView CreateRecordsView(Control parent, GEDCOMTree tree, GEDCOMRecordType recType)
        {
            if (parent == null)
            {
                throw new ArgumentNullException("parent");
            }

            if (tree == null)
            {
                throw new ArgumentNullException("tree");
            }

            GKRecordsView recView;
            recView = new GKRecordsView();
            recView.HideSelection = false;
            recView.LabelEdit = false;
            recView.FullRowSelect = true;
            recView.View = View.Details;
            recView.Tree = tree;
            recView.RecordType = recType;
            recView.Dock = DockStyle.Fill;

            parent.Controls.Add(recView);
            parent.Controls.SetChildIndex(recView, 0);

            return recView;
        }

        public static bool GetInput(string prompt, ref string value)
        {
            bool res = GKInputBox.QueryText(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }

        public static bool GetPassword(string prompt, ref string value)
        {
            bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }

        #endregion

        #region Show information summary

        public static void ShowAddressSummary(GEDCOMAddress address, StringList summary)
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

        public static void ShowDetailCause(GEDCOMEventDetail eventDetail, StringList summary)
        {
            string cause = GetEventCause(eventDetail);
            if (summary != null && cause != "")
            {
                summary.Add("    " + cause);
            }
        }

        public static void ShowDetailInfo(GEDCOMEventDetail eventDetail, StringList summary)
        {
            if (eventDetail == null)
            {
                throw new ArgumentNullException("eventDetail");
            }

            if (summary != null && eventDetail.SourceCitations.Count != 0)
            {
                summary.Add("    " + LangMan.LS(LSID.LSID_RPSources) + " (" + eventDetail.SourceCitations.Count.ToString() + "):");

                int num = eventDetail.SourceCitations.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMSourceCitation cit = eventDetail.SourceCitations[i];
                    GEDCOMSourceRecord sourceRec = cit.Value as GEDCOMSourceRecord;
                    if (sourceRec != null)
                    {
                        string nm = "\"" + sourceRec.FiledByEntry + "\"";
                        if (cit.Page != "")
                        {
                            nm = nm + ", " + cit.Page;
                        }
                        summary.Add("      " + HyperLink(sourceRec.XRef, nm, 0));
                    }
                }
            }
        }

        public static void ShowEvent(GEDCOMRecord subj, StringList aToList, GEDCOMRecord aRec, GEDCOMCustomEvent evt)
        {
            if (subj is GEDCOMNoteRecord) {
                int num = evt.Detail.Notes.Count;
                for (int i = 0; i < num; i++) {
                    if (evt.Detail.Notes[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else
                if (subj is GEDCOMMultimediaRecord) {
                int num2 = evt.Detail.MultimediaLinks.Count;
                for (int i = 0; i < num2; i++) {
                    if (evt.Detail.MultimediaLinks[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else if (subj is GEDCOMSourceRecord) {
                int num3 = evt.Detail.SourceCitations.Count;
                for (int i = 0; i < num3; i++) {
                    if (evt.Detail.SourceCitations[i].Value == subj) {
                        ShowLink(subj, aToList, aRec, evt, evt.Detail.SourceCitations[i]);
                    }
                }
            }
        }

        public static void ShowLink(GEDCOMRecord aSubject, StringList aToList, GEDCOMRecord aRec, GEDCOMTag aTag, GEDCOMPointer aExt)
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
                suffix = ", " + GetEventName(aTag as GEDCOMCustomEvent).ToLower();
            } else {
                suffix = "";
            }
            aToList.Add("    " + prefix + GenRecordLink(aRec, true) + suffix);
        }

        public static void ShowPersonExtInfo(GEDCOMTree tree, GEDCOMIndividualRecord iRec, StringList summary)
        {
            //if (tree == null || iRec == null || summary == null) return;

            /*summary.Add("");
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

        public static void ShowPersonNamesakes(GEDCOMTree tree, GEDCOMIndividualRecord iRec, StringList summary)
        {
            try {
                StringList namesakes = new StringList();
                try {
                    string st = iRec.GetNameString(true, false);

                    int num3 = tree.RecordsCount;
                    for (int i = 0; i < num3; i++) {
                        GEDCOMRecord rec = tree[i];

                        if (rec is GEDCOMIndividualRecord && rec != iRec)
                        {
                            GEDCOMIndividualRecord relPerson = rec as GEDCOMIndividualRecord;

                            string unk = relPerson.GetNameString(true, false);
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

        public static void ShowSubjectLinks(GEDCOMRecord aInRecord, GEDCOMRecord subject, StringList aToList)
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

                if (aInRecord is GEDCOMRecordWithEvents) {
                    GEDCOMRecordWithEvents evsRec = aInRecord as GEDCOMRecordWithEvents;

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

        public static void RecListMediaRefresh(GEDCOMRecord record, StringList summary)
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

                        if (mmRec != null && mmRec.FileReferences.Count != 0)
                        {
                            string st = mmRec.FileReferences[0].Title;

                            summary.Add("  " + HyperLink(mmRec.XRef, st, 0) + " (" + HyperLink("view_" + mmRec.XRef, "просмотр", 0) + ")");
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListMediaRefresh(): " + ex.Message);
            }
        }

        public static void RecListNotesRefresh(GEDCOMRecord record, StringList summary)
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
                        GEDCOMNotes note = record.Notes[i];

                        int num2 = note.Notes.Count;
                        for (int k = 0; k < num2; k++)
                        {
                            string st = note.Notes[k];
                            summary.Add(st);
                        }

                        if (i < num) {
                            summary.Add("");
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListNotesRefresh(): " + ex.Message);
            }
        }

        public static void RecListSourcesRefresh(GEDCOMRecord record, StringList summary)
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

                        if (sourceRec != null)
                        {
                            string nm = "\"" + sourceRec.FiledByEntry + "\"";

                            if (cit.Page != "") {
                                nm = nm + ", " + cit.Page;
                            }

                            summary.Add("  " + HyperLink(sourceRec.XRef, nm, 0));
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListSourcesRefresh(): " + ex.Message);
            }
        }

        public static void RecListAssociationsRefresh(GEDCOMIndividualRecord record, StringList summary)
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
                        string nm = ((ast.Individual == null) ? "" : ast.Individual.GetNameString(true, false));
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

        public static void RecListIndividualEventsRefresh(GEDCOMIndividualRecord record, StringList summary)
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
                        string st = GetIndividualEventName(evt);

                        string sv = "";
                        if (evt.StringValue != "")
                        {
                            sv = evt.StringValue + ", ";
                        }
                        summary.Add(st + ": " + sv + GetEventDesc(evt));

                        ShowDetailCause(evt.Detail, summary);
                        ShowAddressSummary(evt.Detail.Address, summary);
                        ShowDetailInfo(evt.Detail, summary);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListIndividualEventsRefresh(): " + ex.Message);
            }
        }

        public static void RecListFamilyEventsRefresh(GEDCOMFamilyRecord record, StringList summary)
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

                        string st = GetFamilyEventName(evt);
                        summary.Add(st + ": " + GetEventDesc(evt));

                        ShowDetailCause(evt.Detail, summary);
                        ShowDetailInfo(evt.Detail, summary);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListFamilyEventsRefresh(): " + ex.Message);
            }
        }

        public static void RecListGroupsRefresh(GEDCOMIndividualRecord record, StringList summary)
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
                        if (grp != null)
                        {
                            summary.Add("    " + HyperLink(grp.XRef, grp.GroupName, 0));
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKUtils.RecListGroupsRefresh(): " + ex.Message);
            }
        }

        //

        public static void ShowFamilyInfo(GEDCOMFamilyRecord familyRec, StringList summary, ShieldState shieldState)
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
                        string st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkMale) : HyperLink(irec.XRef, irec.GetNameString(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Husband) + ": " + st + GetLifeStr(irec));

                        irec = familyRec.GetWife();
                        st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkFemale) : HyperLink(irec.XRef, irec.GetNameString(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Wife) + ": " + st + GetLifeStr(irec));

                        summary.Add("");
                        if (familyRec.Childrens.Count != 0)
                        {
                            summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                        }

                        int num = familyRec.Childrens.Count;
                        for (int i = 0; i < num; i++)
                        {
                            irec = (GEDCOMIndividualRecord)familyRec.Childrens[i].Value;
                            
                            summary.Add("    " + HyperLink(irec.XRef, irec.GetNameString(true, false), 0) + GetLifeStr(irec));
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
                        summary.Add("~ub+1~" + groupRec.GroupName + "~bu-1~");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Members) + " (" + groupRec.Members.Count.ToString() + "):");

                        int num = groupRec.Members.Count;
                        for (int i = 0; i < num; i++)
                        {
                            GEDCOMPointer ptr = groupRec.Members[i];
                            GEDCOMIndividualRecord member = (GEDCOMIndividualRecord)ptr.Value;
                            
                            mbrList.AddObject(member.GetNameString(true, false), member);
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
                        summary.Add("~ub+1~" + mediaRec.FileReferences[0].Title + "~bu-1~");
                        summary.Add("");
                        summary.Add("[ " + HyperLink("view_" + mediaRec.XRef, LangMan.LS(LSID.LSID_View), 0) + " ]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = mediaRec.Owner;
                        int num = tree.RecordsCount - 1;
                        for (int i = 0; i <= num; i++)
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
                        int num = tree.RecordsCount - 1;
                        for (int i = 0; i <= num; i++)
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

        public static void ShowPersonInfo(GEDCOMIndividualRecord iRec, StringList summary, ShieldState shieldState)
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
                        summary.Add("~ub+1~" + iRec.GetNameString(true, true) + "~bu-1~");
                        summary.Add(LangMan.LS(LSID.LSID_Sex) + ": " + SexStr(iRec.Sex));
                        try
                        {
                            GEDCOMIndividualRecord iFather, iMother;
                            iRec.GetParents(out iFather, out iMother);

                            if (iFather != null || iMother != null)
                            {
                                summary.Add("");
                                summary.Add(LangMan.LS(LSID.LSID_Parents) + ":");

                                string st;

                                st = (iFather == null) ? LangMan.LS(LSID.LSID_UnkMale) : HyperLink(iFather.XRef, iFather.GetNameString(true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Father) + ": " + st + GetLifeStr(iFather));

                                st = (iMother == null) ? LangMan.LS(LSID.LSID_UnkFemale) : HyperLink(iMother.XRef, iMother.GetNameString(true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Mother) + ": " + st + GetLifeStr(iMother));
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
                                if (family != null)
                                {
                                    if (IsRecordAccess(family.Restriction, shieldState))
                                    {
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
                                            st = st + HyperLink(relPerson.XRef, relPerson.GetNameString(true, false), 0) + " (" + HyperLink(family.XRef, marr, 0) + ")";
                                        }
                                        else
                                        {
                                            st = st + unk + " (" + HyperLink(family.XRef, marr, 0) + ")";
                                        }
                                        summary.Add(st);
                                        
                                        if (family.Childrens.Count != 0)
                                        {
                                            summary.Add("");
                                            summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                                        }

                                        int num2 = family.Childrens.Count;
                                        for (int k = 0; k < num2; k++)
                                        {
                                            relPerson = (GEDCOMIndividualRecord)family.Childrens[k].Value;
                                            
                                            summary.Add("    " + HyperLink(relPerson.XRef, relPerson.GetNameString(true, false), 0) + GetLifeStr(relPerson));
                                        }
                                    }
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
                        summary.Add("~ub+1~" + sourceRec.FiledByEntry + "~bu-1~");
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
                        summary.Add("~ub+1~" + repositoryRec.RepositoryName.Trim() + "~bu-1~");
                        summary.Add("");

                        ShowAddressSummary(repositoryRec.Address, summary);

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_RPSources) + ":");

                        GEDCOMTree tree = repositoryRec.Owner;

                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++)
                        {
                            GEDCOMRecord rec = tree[i];

                            if (rec is GEDCOMSourceRecord)
                            {
                                GEDCOMSourceRecord srcRec = rec as GEDCOMSourceRecord;

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
                        summary.Add(LangMan.LS(LSID.LSID_Title) + ": \"~ub+1~" + researchRec.ResearchName.Trim() + "~bu-1~\"");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)researchRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_Status) + ": " + LangMan.LS(GKData.StatusNames[(int)researchRec.Status]) + " (" + researchRec.Percent.ToString() + "%)");
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + GetDateFmtString(researchRec.StartDate, DateFormat.dfDD_MM_YYYY));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + GetDateFmtString(researchRec.StopDate, DateFormat.dfDD_MM_YYYY));

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
                        summary.Add(LangMan.LS(LSID.LSID_Goal) + ": ~ub+1~" + GetTaskGoalStr(taskRec) + "~bu-1~");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + GetDateFmtString(taskRec.StartDate, DateFormat.dfDD_MM_YYYY));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + GetDateFmtString(taskRec.StopDate, DateFormat.dfDD_MM_YYYY));

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
                        summary.Add(LangMan.LS(LSID.LSID_Theme) + ": \"~ub+1~" + commRec.CommName.Trim() + "~bu-1~\"");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Corresponder) + ": " + GetCorresponderStr(tree, commRec, true));
                        summary.Add(LangMan.LS(LSID.LSID_Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]));
                        summary.Add(LangMan.LS(LSID.LSID_Date) + ": " + GetDateFmtString(commRec.Date, DateFormat.dfDD_MM_YYYY));

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
                StringList linkList = new StringList();
                try
                {
                    summary.Clear();
                    if (locRec != null)
                    {
                        summary.Add("");
                        summary.Add("~ub+1~" + locRec.LocationName.Trim() + "~bu-1~");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Latitude) + ": " + locRec.Map.Lati);
                        summary.Add(LangMan.LS(LSID.LSID_Longitude) + ": " + locRec.Map.Long);

                        GEDCOMTree tree = locRec.Owner;

                        GetLocationLinks(tree, locRec, ref linkList);

                        if (linkList.Count > 0)
                        {
                            linkList.Sort();

                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                            int num = linkList.Count;
                            for (int i = 0; i < num; i++)
                            {
                                summary.Add("    " + linkList[i]);
                            }
                        }

                        RecListNotesRefresh(locRec, summary);
                        RecListMediaRefresh(locRec, summary);
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
                Logger.LogWrite("GKUtils.ShowLocationInfo(): " + ex.Message);
            }
        }

        #endregion

        #region Archives support

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

        
        #endregion
        
        #region Names processing

        public static string ClearSurname(string surname)
        {
            if (string.IsNullOrEmpty(surname)) return "";
            
            int p = surname.IndexOf(" (");
            string result = ((p >= 0) ? surname.Substring(0, p) : surname);
            return result;
        }

        public static string PrepareRusSurname(string f, bool aFemale)
        {
            if (string.IsNullOrEmpty(f) || (f[0] == '(' && f[f.Length - 1] == ')'))
            {
                f = "?";
            }
            else
            {
                if (aFemale)
                {
                    f = ClearSurname(f);

                    if (f.EndsWith("а")) {
                        f = f.Substring(0, f.Length - 1);
                    } else if (f.EndsWith("кая")) {
                        f = f.Substring(0, f.Length - 3) + "кий";
                    } else if (f.EndsWith("ная")) {
                        f = f.Substring(0, f.Length - 3) + "ный";
                    }
                }
            }

            return f;
        }

        public static string GetRusWifeSurname(string husbSurname)
        {
            const string consonants = "бвгджзклмнпрстфхцчшщ";
            //const string vowels = "абвгдежзиклмнопрстуфхцчшщьыъэюя";
            
            string res;
            if (string.IsNullOrEmpty(husbSurname)) {
                res = "?";
            } else {
                res = husbSurname;

                char lastSym = res[res.Length - 1];

                if (consonants.IndexOf(lastSym) >= 0) {
                    res = res + "а";
                } else if (res.EndsWith("кий")) {
                    res = res.Substring(0, res.Length - 3) + "кая";
                } else if (res.EndsWith("ный")) {
                    res = res.Substring(0, res.Length - 3) + "ная";
                }
            }

            return res;
        }

        public static string[] GetSurnames(string surname, bool female)
        {
            string[] result = new string[1];

            if (female) {
                surname = surname.Trim();
                int p = surname.IndexOf('(');
                if (p >= 0) {
                    string part = surname.Substring(0, p).Trim();
                    result[0] = PrepareRusSurname(part, female);
                    part = surname.Substring(p).Trim();
                    part = part.Substring(1, part.Length-2);

                    string[] parts = part.Split(',');
                    for (int i = 0; i < parts.Length; i++) {
                        string[] newres = new string[result.Length+1];
                        result.CopyTo(newres, 0);
                        result = newres;
                        result[result.Length-1] = PrepareRusSurname(parts[i].Trim(), female);
                    }
                } else {
                    result[0] = PrepareRusSurname(surname, female);
                }
            } else {
                result[0] = surname;
            }

            return result;
        }

        public static string[] GetSurnames(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
            {
                throw new ArgumentNullException("iRec");
            }

            string fam, nam, pat;
            iRec.GetNameParts(out fam, out nam, out pat);
            bool female = (iRec.Sex == GEDCOMSex.svFemale);

            return GetSurnames(fam, female);
        }

        private static bool StrContains(string str, char c)
        {
            return str.IndexOf(c) >= 0;
        }

        private const string FEM_ENDINGS = "ая";
        private const string MALE_ENDINGS = "вгдйлмнопр";
        
        public static GEDCOMSex GetSex(string iName, string iPat, bool canQuery)
        {
            GEDCOMSex result = GEDCOMSex.svNone;
            if (string.IsNullOrEmpty(iName)) return result;

            char nc = iName[iName.Length - 1];

            if (StrContains(FEM_ENDINGS, nc)) {
                if (!string.IsNullOrEmpty(iPat)) {
                    char pc = iPat[iPat.Length - 1];

                    if (StrContains(FEM_ENDINGS, pc)) {
                        result = GEDCOMSex.svFemale;
                    } else if (StrContains(MALE_ENDINGS, pc)) {
                        result = GEDCOMSex.svMale;
                    }
                }
            } else if (StrContains(MALE_ENDINGS, nc)) {
                result = GEDCOMSex.svMale;
            }

            if (result == GEDCOMSex.svNone && canQuery) {
                string fn = iName + " " + iPat;
                DialogResult res = ShowQuestion("Не определяется пол человека по имени \"" + fn + "\". Это мужской пол?");
                result = (res == DialogResult.Yes) ? GEDCOMSex.svMale : GEDCOMSex.svFemale;
            }

            return result;
        }

        #endregion

        #region Multimedia support

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
                    return MultimediaKind.mkImage;

                case GEDCOMMultimediaFormat.mfTXT:
                case GEDCOMMultimediaFormat.mfRTF:
                case GEDCOMMultimediaFormat.mfHTM:
                    return MultimediaKind.mkText;

                case GEDCOMMultimediaFormat.mfWAV:
                case GEDCOMMultimediaFormat.mfMP3:
                    return MultimediaKind.mkAudio;

                case GEDCOMMultimediaFormat.mfAVI:
                case GEDCOMMultimediaFormat.mfMPG:
                case GEDCOMMultimediaFormat.mfWMA:
                    return MultimediaKind.mkVideo;

                case GEDCOMMultimediaFormat.mfOLE:
                case GEDCOMMultimediaFormat.mfUnknown:
                default:
                    return MultimediaKind.mkNone;
            }
        }

        #endregion
    }
}
