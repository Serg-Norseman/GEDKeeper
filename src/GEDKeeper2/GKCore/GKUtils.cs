using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Types;
using GKUI.Controls;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public static class GKUtils
	{
		#region System functions
		
		public static T GetAssemblyAttribute<T>(System.Reflection.Assembly assembly) where T : Attribute
		{
            if (assembly == null)
            {
                throw new ArgumentNullException("assembly");
            }

            object[] attributes = assembly.GetCustomAttributes(typeof(T), false);
			if (attributes == null || attributes.Length == 0)
				return null;
			return SingleOrDefault(OfTypeIterator<T>((T[])attributes));
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

			if (list.Count > 0) {
				return list[0];
			}

			return default(T);
		}

		public static T LastOrDefault<T>(IList<T> list)
		{
			if (list == null) {
				throw new ArgumentNullException("list");
			}

			int count = list.Count;
			if (count > 0) {
				return list[count - 1];
			}

			return default(T);
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

		#endregion

		#region Aux functions

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

		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}

		public static bool IsRecordAccess(GEDCOMRestriction restriction, ShieldState shieldState)
		{
			bool result = false;

			switch (shieldState) {
				case ShieldState.ssMaximum:
					result = (restriction != GEDCOMRestriction.rnConfidential && restriction != GEDCOMRestriction.rnPrivacy);
					break;

				case ShieldState.ssMiddle:
					result = (restriction != GEDCOMRestriction.rnPrivacy);
					break;

				case ShieldState.ssNone:
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
							aList.Add(GKUtils.GenRecordLink(rec, true) + ", " + GKUtils.GetEventName(evt).ToLower());
						}
					}
				}
			}
		}

		public static string HyperLink(string XRef, string Text, int Num)
		{
			string result = "~^" + XRef;
			if (Text != "")
			{
				result = result + ":" + Text;
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
						st = (record as GEDCOMIndividualRecord).GetNameString(true, false);
						break;
					case GEDCOMRecordType.rtFamily:
						st = GKUtils.GetFamilyString(record as GEDCOMFamilyRecord);
						break;
					case GEDCOMRecordType.rtMultimedia:
						st = (record as GEDCOMMultimediaRecord).FileReferences[0].Title;
						break;
					case GEDCOMRecordType.rtSource:
						st = (record as GEDCOMSourceRecord).FiledByEntry;
						break;
					case GEDCOMRecordType.rtRepository:
						st = (record as GEDCOMRepositoryRecord).RepositoryName;
						break;
					case GEDCOMRecordType.rtGroup:
						st = (record as GEDCOMGroupRecord).GroupName;
						break;
					case GEDCOMRecordType.rtResearch:
						st = (record as GEDCOMResearchRecord).ResearchName;
						break;
					case GEDCOMRecordType.rtTask:
						st = GKUtils.GetTaskGoalStr(record as GEDCOMTaskRecord);
						break;
					case GEDCOMRecordType.rtCommunication:
						st = (record as GEDCOMCommunicationRecord).CommName;
						break;
					case GEDCOMRecordType.rtLocation:
						st = (record as GEDCOMLocationRecord).LocationName;
						break;
					default:
						st = record.XRef;
						break;
				}

                result = GKUtils.HyperLink(record.XRef, sign + st, 0);
			}

            return result;
		}

		public static string GetCorresponderStr(GEDCOMTree tree, GEDCOMCommunicationRecord commRec, bool aLink)
		{
			string result = "";
			GKCommunicationDir dir = GKCommunicationDir.cdFrom;
			GEDCOMIndividualRecord corresponder = null;
			commRec.GetCorresponder(ref dir, ref corresponder);
			if (corresponder != null)
			{
				string nm = corresponder.GetNameString(true, false);
				if (aLink)
				{
					nm = GKUtils.HyperLink(corresponder.XRef, nm, 0);
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
					result = (tempRec as GEDCOMIndividualRecord).GetNameString(true, false);
					break;
				case GKGoalType.gtFamily:
					result = GKUtils.GetFamilyString(tempRec as GEDCOMFamilyRecord);
					break;
				case GKGoalType.gtSource:
					result = (tempRec as GEDCOMSourceRecord).FiledByEntry;
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

		#endregion

		#region Event Utils
		
		public static string GetAttributeValue(GEDCOMIndividualRecord iRec, string attrName)
		{
			if (iRec == null) return string.Empty;

			GEDCOMCustomEvent attr = iRec.FindEvent(attrName);
			string result = ((attr == null) ? "" : attr.StringValue);
			return result;
		}

		public static PersonEventKind GetPersonEventKindBySign(string aSign)
		{
			PersonEventKind res = PersonEventKind.ekFact;

			for (int i = 0; i < GKData.PersonEvents.Length; i++)
			{
				if (GKData.PersonEvents[i].Sign == aSign)
				{
					res = GKData.PersonEvents[i].Kind;
					break;
				}
			}

			return res;
		}

		public static int GetPersonEventIndex(string aSign)
		{
			int res = -1;

			for (int i = 0; i < GKData.PersonEvents.Length; i++)
			{
				if (GKData.PersonEvents[i].Sign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static int GetFamilyEventIndex(string aSign)
		{
			int res = -1;

			for (int i = 0; i < GKData.FamilyEvents.Length; i++)
			{
				if (GKData.FamilyEvents[i].Sign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static int GetMarriageStatusIndex(string aSign)
		{
			int res = 0;

			for (int i = 0; i < GKData.MarriageStatus.Length; i++)
			{
				if (GKData.MarriageStatus[i].StatSign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static string GetEventName(GEDCOMCustomEvent aEvent)
		{
			string result = "";

			if (aEvent is GEDCOMIndividualEvent || aEvent is GEDCOMIndividualAttribute)
			{
				int ev = GKUtils.GetPersonEventIndex(aEvent.Name);
				if (ev == 0) {
					result = aEvent.Detail.Classification;
				} else {
					if (ev > 0) {
						result = LangMan.LS(GKData.PersonEvents[ev].Name);
					} else {
						result = aEvent.Name;
					}
				}
			}
			else if (aEvent is GEDCOMFamilyEvent)
			{
				int ev = GKUtils.GetFamilyEventIndex(aEvent.Name);
				if (ev == 0) {
					result = aEvent.Detail.Classification;
				} else {
					if (ev > 0) {
						result = LangMan.LS(GKData.FamilyEvents[ev].Name);
					} else {
						result = aEvent.Name;
					}
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

            int idx = GKUtils.GetPersonEventIndex(iAttr.Name);
			string st;
			if (idx == 0)
			{
				st = iAttr.Detail.Classification;
			}
			else
			{
				if (idx > 0)
				{
					st = LangMan.LS(GKData.PersonEvents[idx].Name);
				}
				else
				{
					st = iAttr.Name;
				}
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
                throw new ArgumentNullException("eventDetail");
            }

            string dt = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
			string place = evt.Detail.Place.StringValue;
			GEDCOMLocationRecord location = evt.Detail.Place.Location.Value as GEDCOMLocationRecord;

			if (place != "" && location != null && hyperLink) {
				place = GKUtils.HyperLink(location.XRef, place, 0);
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

            int ev = GKUtils.GetPersonEventIndex(evt.Name);
            if (ev == 0)
            {
                result = evt.Detail.Classification;
            }
            else
            {
                if (ev > 0)
                {
                	result = LangMan.LS(GKData.PersonEvents[ev].Name);
                }
                else
                {
                    result = evt.Name;
                }
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

            int ev = GKUtils.GetFamilyEventIndex(evt.Name);
            if (ev == 0)
            {
                result = evt.Detail.Classification;
            }
            else
            {
                if (ev > 0)
                {
                	result = LangMan.LS(GKData.FamilyEvents[ev].Name);
                }
                else
                {
                    result = evt.Name;
                }
            }

            return result;
        }

		#endregion

		#region Date functions

		public static string GEDCOMDateToStr(GEDCOMDate date, DateFormat format)
		{
            if (date == null)
            {
                throw new ArgumentNullException("date");
            }

            string result = "";
			int year;
			ushort month;
			ushort day;
			date.GetDate(out year, out month, out day);

			if (year > 0 || month > 0 || day > 0)
			{
				switch (format) {
					case DateFormat.dfDD_MM_YYYY:
						result += day > 0 ? SysUtils.NumUpdate(day, 2) + "." : "__.";
						result += month > 0 ? SysUtils.NumUpdate(month, 2) + "." : "__.";
						result += year > 0 ? year.ToString().PadLeft(4, '_') : "____";
						break;

					case DateFormat.dfYYYY_MM_DD:
						result += year > 0 ? year.ToString().PadLeft(4, '_') + "." : "____.";
						result += month > 0 ? SysUtils.NumUpdate(month, 2) + "." : "__.";
						result += day > 0 ? SysUtils.NumUpdate(day, 2) : "__";
						break;

					case DateFormat.dfYYYY:
						if (year > 0) {
							result = year.ToString().PadLeft(4, '_');
						}
						break;
				}
			}

			return result;
		}

		public static string GEDCOMEventToDateStr(GEDCOMCustomEvent evt, DateFormat format, bool sign)
		{
		    if (evt == null) return string.Empty;

			return GEDCOMCustomDateToStrEx(evt.Detail.Date.Value, format, sign);
		}

		public static string GEDCOMCustomDateToStrEx(GEDCOMCustomDate date, DateFormat format, bool sign)
		{
			string result = "";

			if (date == null)
			{
				result = "";
			}
			else
			{
				if (date is GEDCOMDateApproximated)
				{
				    GEDCOMDateApproximated appDate = date as GEDCOMDateApproximated;

                    result = GKUtils.GEDCOMDateToStr(appDate, format);
                    if (sign && appDate.Approximated != GEDCOMApproximated.daExact)
					{
						result = "~ " + result;
					}
				}
				else
				{
					if (date is GEDCOMDateRange)
					{
						GEDCOMDateRange range = date as GEDCOMDateRange;

                        if (range.After.StringValue == "" && range.Before.StringValue != "")
						{
							result = GKUtils.GEDCOMDateToStr(range.Before, format);
							if (sign)
							{
								result = "< " + result;
							}
						}
						else
						{
							if (range.After.StringValue != "" && range.Before.StringValue == "")
							{
								result = GKUtils.GEDCOMDateToStr(range.After, format);
								if (sign)
								{
									result += " >";
								}
							}
							else
							{
								if (range.After.StringValue != "" && range.Before.StringValue != "")
								{
									result = GKUtils.GEDCOMDateToStr(range.After, format) + " - " + GKUtils.GEDCOMDateToStr(range.Before, format);
								}
							}
						}
					}
					else
					{
						if (date is GEDCOMDatePeriod)
						{
							GEDCOMDatePeriod period = date as GEDCOMDatePeriod;
							if (period.DateFrom.StringValue != "" && period.DateTo.StringValue == "")
							{
								result = GKUtils.GEDCOMDateToStr(period.DateFrom, format);
								if (sign)
								{
									result += " >";
								}
							}
							else
							{
								if (period.DateFrom.StringValue == "" && period.DateTo.StringValue != "")
								{
									result = GKUtils.GEDCOMDateToStr(period.DateTo, format);
									if (sign)
									{
										result = "< " + result;
									}
								}
								else
								{
									if (period.DateFrom.StringValue != "" && period.DateTo.StringValue != "")
									{
										result = GKUtils.GEDCOMDateToStr(period.DateFrom, format) + " - " + GKUtils.GEDCOMDateToStr(period.DateTo, format);
									}
								}
							}
						}
						else
						{
							if (date is GEDCOMDate)
							{
								result = GKUtils.GEDCOMDateToStr(date as GEDCOMDate, format);
							}
						}
					}
				}
			}

			if ((date is GEDCOMDate) && (date as GEDCOMDate).YearBC) {
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

		public static string CompactDate(string date)
		{
			string result = date;
			while (result.IndexOf("__.") == 0) result = result.Remove(0, 3);
			return result;
		}

		public static string GetBirthDate(GEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
		{
			if (iRec == null) return string.Empty;

			GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMEventToDateStr(evt, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetDeathDate(GEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
		{
			if (iRec == null) return string.Empty;

			GEDCOMCustomEvent evt = iRec.FindEvent("DEAT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMEventToDateStr(evt, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetLifeStr(GEDCOMIndividualRecord iRec)
		{
			if (iRec == null) return string.Empty;

			string result = " (";

			string ds = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				ds = "?";
			}
			result += ds;

			ds = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, false);
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

            string res_str = "";

			//PedigreeFormat fmt = this.fOptions.PedigreeOptions.Format;

			switch (fmt) {
				case PedigreeFormat.pfExcess:
					{
						string ds = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
						if (ds == "")
						{
							ds = "?";
						}
						res_str += ds;
						ds = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
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
							res_str = res_str + " - " + ds;
						}
					}
					break;

				case PedigreeFormat.pfCompact:
					{
						string ds = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, true);
						string ps = GKUtils.GetBirthPlace(iRec);
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
						res_str += ds;
						ds = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, true);
						ps = GKUtils.GetDeathPlace(iRec);
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
							res_str = res_str + " " + ds;
						}
					}
					break;
			}
			
			string result;
			if (res_str == "" || res_str == " ") {
				result = "";
			} else {
				result = " (" + res_str + ")";
			}
			return result;
		}

		public static int GetIndependentYear(GEDCOMIndividualRecord iRec, string evSign)
		{
			bool dummy;
			return GetIndependentYear(iRec, evSign, out dummy);
		}

		public static int GetIndependentYear(GEDCOMIndividualRecord iRec, string evSign, out bool YearBC)
		{
			int result = -1;
            YearBC = false;
            if (iRec == null) return result;

			GEDCOMCustomEvent ev = iRec.FindEvent(evSign);
			if (ev != null)
			{
				int year;
				ushort am, ad;
				ev.Detail.Date.GetIndependentDate(out year, out am, out ad, out YearBC);
				result = year;
			}
			return result;
		}

		public static int GetIndependentMonth(GEDCOMIndividualRecord iRec, string evSign)
		{
			int result = -1;
            if (iRec == null) return result;

			GEDCOMCustomEvent ev = iRec.FindEvent(evSign);
			if (ev != null)
			{
				int ay;
				ushort month, ad;
	            bool YearBC;
				ev.Detail.Date.GetIndependentDate(out ay, out month, out ad, out YearBC);
				result = month;
			}
			return result;
		}

		public static double GetAbstractDate(GEDCOMCustomEvent evt)
		{
			double result;

			if (evt == null) {
				result = double.NaN;
			} else {
				result = GetAbstractDate(evt.Detail.Date.Value);
			}

			return result;
		}

		public static double GetAbstractDate(GEDCOMCustomDate customDate)
		{
			double result;

			if (customDate == null) {
				result = double.NaN;
			} else {
				int year;
				ushort month, day;
				bool yearBC;
				GEDCOMUtils.GetDateParts(customDate, out year, out month, out day, out yearBC);

				if (year == -1) {
					result = double.NaN; // it's empty date, as negative dates has yearBC-attribute
				} else {
					result = (double)year; // ####.0000
					result += (month / 100.0f); // 0.##000
					result += (day / 10000.0f); // 0.00##

					if (yearBC) result = -result;
				}
			}

			return result;
		}

		public static string GetEventsYearsDiff(GEDCOMCustomEvent ev1, GEDCOMCustomEvent ev2, bool currentEnd)
		{
			string result = "?";

			try
			{
				double y1 = GKUtils.GetAbstractDate(ev1);
				double y2 = GKUtils.GetAbstractDate(ev2);

				if (currentEnd && double.IsNaN(y2))
				{
					y2 = ((double)DateTime.Now.Year + (double)DateTime.Now.Month / 12.0);
				}

				if (double.IsNaN(y1) || double.IsNaN(y2))
				{
					result = "";
				}
				else
				{
					if (y1 == (double)0f || y2 == (double)0f)
					{
						result = "?";
					}
					else
					{
						long delta = SysUtils.Trunc(y2 - y1);
						result = delta.ToString();
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetEventsYearsDiff(): " + ex.Message);
			}

			return result;
		}

		public static string GetLifeExpectancy(GEDCOMIndividualRecord iRec)
		{
			string result = "";
            if (iRec == null) return result;

			try
			{
				GEDCOMCustomEvent bd;
				GEDCOMCustomEvent dd;
				iRec.GetLifeDates(out bd, out dd);

				result = GKUtils.GetEventsYearsDiff(bd, dd, false);
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetLifeExpectancy(): " + ex.Message);
			}

			return result;
		}

		public static string GetAge(GEDCOMIndividualRecord iRec, int toYear)
		{
			string result = "";
            if (iRec == null) return result;

			try
			{
				GEDCOMCustomEvent bd;
				GEDCOMCustomEvent dd;
				iRec.GetLifeDates(out bd, out dd);

				if (toYear == -1) {
					result = GKUtils.GetEventsYearsDiff(bd, dd, dd == null);
				} else {
					if (bd == null) {
						result = "";
					} else {
						ushort dummy;
						int i;
						bd.Detail.Date.GetIndependentDate(out i, out dummy, out dummy);
						result = Convert.ToString(toYear - i);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetAge(): " + ex.Message);
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
			if (date == null) {
				return string.Empty;
			}

			return GKUtils.GEDCOMCustomDateToStrEx(date, dateFormat, false);
		}

		public static string GetDaysForBirth(GEDCOMIndividualRecord iRec)
		{
            if (iRec == null) return string.Empty;

			string result = "";
			try
			{
				GEDCOMCustomEvent evt = iRec.FindEvent("DEAT");
				if (evt != null)
				{
				}
				else
				{
					evt = iRec.FindEvent("BIRT");
					if (evt != null)
					{
						GEDCOMDate dt = evt.Detail.Date.Value as GEDCOMDate;
						if (dt != null)
						{
							int bd_y;
							ushort bd_m;
							ushort bd_d;

							dt.GetDate(out bd_y, out bd_m, out bd_d);
							if (bd_m <= 0 || bd_d <= 0)
							{
							}
							else
							{
								DateTime dtx = DateTime.Now;
								ushort cur_y = (ushort)dtx.Year;
								ushort cur_m = (ushort)dtx.Month;
								ushort cur_d = (ushort)dtx.Day;
								double dt2 = (cur_y + bd_m / 12.0 + bd_d / 12.0 / 31.0);
								double dt3 = (cur_y + cur_m / 12.0 + cur_d / 12.0 / 31.0);
								if (dt2 < dt3)
								{
									bd_y = (int)(cur_y + 1u);
								}
								else
								{
									bd_y = (int)cur_y;
								}
								result = Convert.ToString(SysUtils.DaysBetween(new DateTime((int)cur_y, (int)cur_m, (int)cur_d), new DateTime(bd_y, (int)bd_m, (int)bd_d)));
							}
						}
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetDaysForBirth(): " + ex.Message);
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
            if (iRec == null) return string.Empty;

			return GKUtils.GetPlaceStr(iRec.FindEvent("RESI"), includeAddress);
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
						val += GKUtils.GetAncestorsCount(anc);

						anc = family.GetWife();
						val += GKUtils.GetAncestorsCount(anc);
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
							val += GKUtils.GetDescendantsCount(iChild);
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
						int res = GKUtils.GetDescGens_Recursive(iChild);
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
			return GKUtils.GetDescGens_Recursive(iRec) - 1;
		}

		public static int GetMarriagesCount(GEDCOMIndividualRecord iRec)
		{
			int result = ((iRec == null) ? 0 : iRec.SpouseToFamilyLinks.Count);
			return result;
		}

		public static int GetSpousesDiff(GEDCOMFamilyRecord fRec)
		{
			int result = 0;

			try
			{
				if (fRec != null) {
					GEDCOMIndividualRecord husb = fRec.GetHusband();
					GEDCOMIndividualRecord wife = fRec.GetWife();

					if (husb != null && wife != null)
					{
						GEDCOMCustomEvent evH = husb.FindEvent("BIRT");
						GEDCOMCustomEvent evW = wife.FindEvent("BIRT");

						string res = GetEventsYearsDiff(evH, evW, false);
						if (res != "" && res != "?") {
							result = Math.Abs(int.Parse(res));
						}
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetSpousesDiff(): " + ex.Message);
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
				double firstYear = double.NaN;

				GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
				if (evt != null)
				{
					double parentYear = GKUtils.GetAbstractDate(evt);

					int num = iRec.SpouseToFamilyLinks.Count;
					for (int i = 0; i < num; i++)
					{
						GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count;
						for (int j = 0; j < num2; j++)
						{
							GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
							evt = child.FindEvent("BIRT");
							if (evt != null)
							{
								double childYear = GKUtils.GetAbstractDate(evt);

								if (double.IsNaN(firstYear))
								{
									firstYear = childYear;
									iChild = child;
								}
								else
								{
									if (firstYear > childYear)
									{
										firstYear = childYear;
										iChild = child;
									}
								}
							}
						}
					}

					if (!double.IsNaN(parentYear) && !double.IsNaN(firstYear)) {
						result = (int)SysUtils.Trunc(firstYear - parentYear);
					} else {
						iChild = null;
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetFirstbornAge(): " + ex.Message);
			}
			return result;
		}

		public static int GetMarriageAge(GEDCOMIndividualRecord iRec)
		{
			int result = 0;
            if (iRec == null) return result;

			try
			{
				double firstYear = double.NaN;

				GEDCOMCustomEvent evt = iRec.FindEvent("BIRT");
				if (evt != null)
				{
					double mainYear = GKUtils.GetAbstractDate(evt);

					int num = iRec.SpouseToFamilyLinks.Count;
					for (int i = 0; i < num; i++)
					{
						GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						GEDCOMCustomEvent fEvent = family.FindEvent("MARR");
						if (fEvent != null)
						{
							double spouseYear = GKUtils.GetAbstractDate(fEvent);

							if (double.IsNaN(firstYear)) {
								firstYear = spouseYear;
							} else {
								if (firstYear > spouseYear) {
									firstYear = spouseYear;
								}
							}
						}
					}

					if (!double.IsNaN(mainYear) && !double.IsNaN(firstYear))
					{
						result = (int)SysUtils.Trunc(firstYear - mainYear);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetMarriageAge(): " + ex.Message);
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
				string regex_str = "";
				int curPos = 0;
				int len = mask.Length;
				if (curPos < len)
				{
					do
					{
						int I = mask.IndexOfAny("*?".ToCharArray(), curPos);
						if (I < curPos) break;
						if (I > curPos) {
							string part = mask.Substring(curPos, I - curPos);
							regex_str += Regex.Escape(part);
						}

						char c = mask[I];
						switch (c) {
							case '*':
								regex_str += ".*";
								break;
							case '?':
								regex_str += ".";
								break;
						}

						curPos = I + 1;
					}
					while (curPos < len);
				}

				if (curPos < len) {
					string part = mask.Substring(curPos, len - curPos);
					regex_str += Regex.Escape(part);
				}

				result = new Regex(regex_str, RegexOptions.IgnoreCase);
			}

			return result;
		}

		public static bool MatchesRegex(string str, Regex regex)
		{
			return ((regex != null) ? regex.IsMatch(str) : false);
		}

		public static bool MatchesMask(string S, string mask)
		{
			Regex regex = InitMaskRegex(mask);
			return MatchesRegex(S, regex);
		}

		#endregion

		#region Folder functions

		public static string GetTempDir()
		{
			return Environment.GetEnvironmentVariable("TEMP");
		}

		public static string GetAppPath()
		{
			Module[] mods = System.Reflection.Assembly.GetExecutingAssembly().GetModules();
			string fn = mods[0].FullyQualifiedName;
			return Path.GetDirectoryName(fn) + "\\";
		}

		#endregion

		#region UI functions

		public static void ShowMessage(string msg)
		{
			MessageBox.Show(msg, GKData.AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
		}

		public static void ShowError(string msg)
		{
			MessageBox.Show(msg, GKData.AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Hand);
		}

		public static DialogResult ShowQuestion(string msg)
		{
			return MessageBox.Show(msg, GKData.AppTitle, MessageBoxButtons.YesNo, MessageBoxIcon.Question);
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

		public static bool GetInput(string title, ref string value)
		{
            bool res = GKInputBox.QueryText(title, LangMan.LS(LSID.LSID_Value), ref value);
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
			string cause = GKUtils.GetEventCause(eventDetail);
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
						summary.Add("      " + GKUtils.HyperLink(sourceRec.XRef, nm, 0));
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
                        GKUtils.ShowLink(subj, aToList, aRec, evt, null);
                    }
                }
            } else
                if (subj is GEDCOMMultimediaRecord) {
                    int num2 = evt.Detail.MultimediaLinks.Count;
                    for (int i = 0; i < num2; i++) {
                        if (evt.Detail.MultimediaLinks[i].Value == subj) {
                            GKUtils.ShowLink(subj, aToList, aRec, evt, null);
                        }
                    }
                } else if (subj is GEDCOMSourceRecord) {
                    int num3 = evt.Detail.SourceCitations.Count;
                    for (int i = 0; i < num3; i++) {
                        if (evt.Detail.SourceCitations[i].Value == subj) {
                            GKUtils.ShowLink(subj, aToList, aRec, evt, evt.Detail.SourceCitations[i]);
                        }
                    }
                }
		}

		public static void ShowLink(GEDCOMRecord aSubject, StringList aToList, GEDCOMRecord aRec, GEDCOMTag aTag, GEDCOMPointer aExt)
		{
			string prefix;
			if (aSubject is GEDCOMSourceRecord && aExt != null) {
                GEDCOMSourceCitation cit = (aExt as GEDCOMSourceCitation);
				if (cit.Page != "") {
					prefix = cit.Page + ": ";
				} else {
					prefix = "";
				}
			} else {
				prefix = "";
			}

			string suffix;
			if (aTag != null && aTag is GEDCOMCustomEvent) {
				suffix = ", " + GKUtils.GetEventName(aTag as GEDCOMCustomEvent).ToLower();
			} else {
				suffix = "";
			}
			aToList.Add("    " + prefix + GKUtils.GenRecordLink(aRec, true) + suffix);
		}

		public static void ShowPersonExtInfo(GEDCOMTree tree, GEDCOMIndividualRecord iRec, StringList summary)
		{
		    //if (tree == null || iRec == null || summary == null) return;

        	summary.Add("");
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
        		GEDCOMRecord rec = tree[i];
        		if (rec.RecordType == GEDCOMRecordType.rtIndividual) {
        			GEDCOMIndividualRecord ir = rec as GEDCOMIndividualRecord;
        			
        			bool first = true;
        			for (int k = 0, cnt = ir.Associations.Count; k < cnt; k++) {
                        GEDCOMAssociation asso = ir.Associations[k];

                        if (asso.Individual == iRec) {
        					if (first) {
        						summary.Add(LangMan.LS(LSID.LSID_Associations) + ":");
        						first = false;
        					}
        					summary.Add("    " + GKUtils.HyperLink(ir.XRef, ir.GetNameString(true, false), 0));
        				}
        			}
        		}
        	}
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
                        if (rec is GEDCOMIndividualRecord && rec != iRec) {
                            GEDCOMIndividualRecord rel_person = rec as GEDCOMIndividualRecord;
                            string unk = rel_person.GetNameString(true, false);
                            if (st == unk) {
                                namesakes.AddObject(unk + GKUtils.GetLifeStr(rel_person), rel_person);
                            }
                        }
                    }

                    if (namesakes.Count > 0) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Namesakes) + ":");

                        int num4 = namesakes.Count;
                        for (int i = 0; i < num4; i++) {
                            GEDCOMIndividualRecord rel_person = namesakes.GetObject(i) as GEDCOMIndividualRecord;
                            summary.Add("    " + GKUtils.HyperLink(rel_person.XRef, namesakes[i], 0));
                        }
                    }
                }
                finally {
                    namesakes.Dispose();
                }
            }
            catch (Exception ex) {
                SysUtils.LogWrite("GKUtils.ShowPersonNamesakes(): " + ex.Message);
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
							GKUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is GEDCOMMultimediaRecord) {
					num = aInRecord.MultimediaLinks.Count;
					for (int i = 0; i < num; i++) {
						if (aInRecord.MultimediaLinks[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is GEDCOMSourceRecord) {
					num = aInRecord.SourceCitations.Count;
					for (int i = 0; i < num; i++) {
						if (aInRecord.SourceCitations[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
						}
					}
				}

				if (aInRecord is GEDCOMRecordWithEvents) {
					GEDCOMRecordWithEvents evsRec = aInRecord as GEDCOMRecordWithEvents;

					num = evsRec.Events.Count;
					for (int i = 0; i < num; i++) {
						GKUtils.ShowEvent(subject, aToList, evsRec, evsRec.Events[i]);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.ShowSubjectLinks(): " + ex.Message);
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

                            summary.Add("  " + GKUtils.HyperLink(mmRec.XRef, st, 0) + " (" + GKUtils.HyperLink("view_" + mmRec.XRef, "просмотр", 0) + ")");
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.RecListMediaRefresh(): " + ex.Message);
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
                SysUtils.LogWrite("GKUtils.RecListNotesRefresh(): " + ex.Message);
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

                            summary.Add("  " + GKUtils.HyperLink(sourceRec.XRef, nm, 0));
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.RecListSourcesRefresh(): " + ex.Message);
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

                        summary.Add("    " + ast.Relation + " " + GKUtils.HyperLink(ast.Individual.XRef, nm, 0));
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.RecListAssociationsRefresh(): " + ex.Message);
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
                        string st = GKUtils.GetIndividualEventName(evt);

                        string sv = "";
                        if (evt.StringValue != "")
                        {
                            sv = evt.StringValue + ", ";
                        }
                        summary.Add(st + ": " + sv + GKUtils.GetEventDesc(evt));

                        GKUtils.ShowDetailCause(evt.Detail, summary);
                        GKUtils.ShowAddressSummary(evt.Detail.Address, summary);
                        GKUtils.ShowDetailInfo(evt.Detail, summary);
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.RecListIndividualEventsRefresh(): " + ex.Message);
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
                        GEDCOMFamilyEvent evt = record.Events[i] as GEDCOMFamilyEvent;
                        string st = GKUtils.GetFamilyEventName(evt);

                        summary.Add(st + ": " + GKUtils.GetEventDesc(evt));
                        GKUtils.ShowDetailCause(evt.Detail, summary);
                        GKUtils.ShowDetailInfo(evt.Detail, summary);
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.RecListFamilyEventsRefresh(): " + ex.Message);
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
                            summary.Add("    " + GKUtils.HyperLink(grp.XRef, grp.GroupName, 0));
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.RecListGroupsRefresh(): " + ex.Message);
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
                        string st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkMale) : GKUtils.HyperLink(irec.XRef, irec.GetNameString(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Husband) + ": " + st + GKUtils.GetLifeStr(irec));

                        irec = familyRec.GetWife();
                        st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkFemale) : GKUtils.HyperLink(irec.XRef, irec.GetNameString(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Wife) + ": " + st + GKUtils.GetLifeStr(irec));

                        summary.Add("");
                        if (familyRec.Childrens.Count != 0)
                        {
                        	summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                        }

                        int num = familyRec.Childrens.Count;
                        for (int i = 0; i < num; i++)
                        {
                            irec = (familyRec.Childrens[i].Value as GEDCOMIndividualRecord);
                            summary.Add("    " + GKUtils.HyperLink(irec.XRef, irec.GetNameString(true, false), 0) + GKUtils.GetLifeStr(irec));
                        }
                        summary.Add("");

                        GKUtils.RecListFamilyEventsRefresh(familyRec, summary);
                        GKUtils.RecListNotesRefresh(familyRec, summary);
                        GKUtils.RecListMediaRefresh(familyRec, summary);
                        GKUtils.RecListSourcesRefresh(familyRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowFamilyInfo(): " + ex.Message);
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
                            GEDCOMIndividualRecord member = ptr.Value as GEDCOMIndividualRecord;
                            mbrList.AddObject(member.GetNameString(true, false), member);
                        }
                        mbrList.Sort();

                        int num2 = mbrList.Count;
                        for (int i = 0; i < num2; i++)
                        {
                            GEDCOMIndividualRecord member = mbrList.GetObject(i) as GEDCOMIndividualRecord;
                            summary.Add("    " + GKUtils.HyperLink(member.XRef, mbrList[i], i + 1));
                        }

                        GKUtils.RecListNotesRefresh(groupRec, summary);
                        GKUtils.RecListMediaRefresh(groupRec, summary);
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
                SysUtils.LogWrite("GKUtils.ShowGroupInfo(): " + ex.Message);
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
                        summary.Add("[ " + GKUtils.HyperLink("view_" + mediaRec.XRef, LangMan.LS(LSID.LSID_View), 0) + " ]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = mediaRec.Owner;
                        int num = tree.RecordsCount - 1;
                        for (int i = 0; i <= num; i++)
                        {
                            GKUtils.ShowSubjectLinks(tree[i], mediaRec, summary);
                        }

                        GKUtils.RecListNotesRefresh(mediaRec, summary);
                        GKUtils.RecListSourcesRefresh(mediaRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowMultimediaInfo(): " + ex.Message);
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
                            GKUtils.ShowSubjectLinks(tree[i], noteRec, summary);
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
                SysUtils.LogWrite("GKUtils.ShowNoteInfo(): " + ex.Message);
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
                        summary.Add(LangMan.LS(LSID.LSID_Sex) + ": " + GKUtils.SexStr(iRec.Sex));
                        try
                        {
                        	GEDCOMIndividualRecord iFather, iMother;
                        	iRec.GetParents(out iFather, out iMother);

                        	if (iFather != null || iMother != null)
                        	{
                                summary.Add("");
                                summary.Add(LangMan.LS(LSID.LSID_Parents) + ":");

                                string st;

                                st = (iFather == null) ? LangMan.LS(LSID.LSID_UnkMale) : GKUtils.HyperLink(iFather.XRef, iFather.GetNameString(true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Father) + ": " + st + GKUtils.GetLifeStr(iFather));

                                st = (iMother == null) ? LangMan.LS(LSID.LSID_UnkFemale) : GKUtils.HyperLink(iMother.XRef, iMother.GetNameString(true, false), 0);
                                summary.Add("  " + LangMan.LS(LSID.LSID_Mother) + ": " + st + GKUtils.GetLifeStr(iMother));
                        	}
                        }
                        catch (Exception ex)
                        {
                            SysUtils.LogWrite("GKUtils.ShowPersonInfo().Parents(): " + ex.Message);
                        }

                        try
                        {
                            int num = iRec.SpouseToFamilyLinks.Count;
                            for (int i = 0; i < num; i++)
                            {
                                GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
                                if (family != null)
                                {
                                    if (GKUtils.IsRecordAccess(family.Restriction, shieldState))
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
                                        string marr = GKUtils.GetMarriageDateStr(family, DateFormat.dfDD_MM_YYYY);
                                        if (marr != "")
                                        {
                                        	marr = LangMan.LS(LSID.LSID_LMarriage) + " " + marr;
                                        }
                                        else
                                        {
                                        	marr = LangMan.LS(LSID.LSID_LFamily);
                                        }
                                        GEDCOMIndividualRecord rel_person = sp.Value as GEDCOMIndividualRecord;
                                        summary.Add("");
                                        if (rel_person != null)
                                        {
                                            st = st + GKUtils.HyperLink(rel_person.XRef, rel_person.GetNameString(true, false), 0) + " (" + GKUtils.HyperLink(family.XRef, marr, 0) + ")";
                                        }
                                        else
                                        {
                                            st = st + unk + " (" + GKUtils.HyperLink(family.XRef, marr, 0) + ")";
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
                                            rel_person = family.Childrens[k].Value as GEDCOMIndividualRecord;
                                            summary.Add("    " + GKUtils.HyperLink(rel_person.XRef, rel_person.GetNameString(true, false), 0) + GKUtils.GetLifeStr(rel_person));
                                        }
                                    }
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            SysUtils.LogWrite("GKUtils.ShowPersonInfo().Families(): " + ex.Message);
                        }

                        GKUtils.RecListIndividualEventsRefresh(iRec, summary);
                        GKUtils.RecListNotesRefresh(iRec, summary);
                        GKUtils.RecListMediaRefresh(iRec, summary);
                        GKUtils.RecListSourcesRefresh(iRec, summary);
                        GKUtils.RecListAssociationsRefresh(iRec, summary);
                        GKUtils.RecListGroupsRefresh(iRec, summary);

                        GKUtils.ShowPersonNamesakes(tree, iRec, summary);
                        GKUtils.ShowPersonExtInfo(tree, iRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowPersonInfo(): " + ex.Message);
            }
        }

        public static void ShowSourceInfo(GEDCOMSourceRecord sourceRec, StringList summary)
        {
            if (summary == null) return;

            try
            {
                summary.BeginUpdate();
                StringList link_list = new StringList();
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
                                GEDCOMRepositoryRecord rep = sourceRec.RepositoryCitations[i].Value as GEDCOMRepositoryRecord;
                                summary.Add("    " + GKUtils.HyperLink(rep.XRef, rep.RepositoryName, 0));
                            }
                        }

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = sourceRec.Owner;

                        int num2 = tree.RecordsCount;
                        for (int j = 0; j < num2; j++)
                        {
                            GKUtils.ShowSubjectLinks(tree[j], sourceRec, link_list);
                        }

                        link_list.Sort();

                        int num3 = link_list.Count;
                        for (int j = 0; j < num3; j++)
                        {
                            summary.Add(link_list[j]);
                        }

                        GKUtils.RecListNotesRefresh(sourceRec, summary);
                        GKUtils.RecListMediaRefresh(sourceRec, summary);
                    }
                }
                finally
                {
                    link_list.Dispose();
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowSourceInfo(): " + ex.Message);
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

                        GKUtils.ShowAddressSummary(repositoryRec.Address, summary);

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
                                        summary.Add("    " + GKUtils.GenRecordLink(srcRec, false));
                                    }
                                }
                            }
                        }

                        GKUtils.RecListNotesRefresh(repositoryRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowRepositoryInfo(): " + ex.Message);
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
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + GKUtils.GEDCOMDateToStr(researchRec.StartDate, DateFormat.dfDD_MM_YYYY));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + GKUtils.GEDCOMDateToStr(researchRec.StopDate, DateFormat.dfDD_MM_YYYY));

                        if (researchRec.Tasks.Count > 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPTasks) + ":");

                            int num = researchRec.Tasks.Count;
                            for (int i = 0; i < num; i++)
                            {
                                GEDCOMTaskRecord taskRec = researchRec.Tasks[i].Value as GEDCOMTaskRecord;
                                summary.Add("    " + GKUtils.GenRecordLink(taskRec, false));
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
                                summary.Add("    " + GKUtils.GenRecordLink(corrRec, false));
                            }
                        }

                        if (researchRec.Groups.Count != 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPGroups) + ":");

                            int num3 = researchRec.Groups.Count;
                            for (int i = 0; i < num3; i++)
                            {
                                GEDCOMGroupRecord grp = researchRec.Groups[i].Value as GEDCOMGroupRecord;
                                summary.Add("    " + GKUtils.HyperLink(grp.XRef, grp.GroupName, 0));
                            }
                        }

                        GKUtils.RecListNotesRefresh(researchRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowResearchInfo(): " + ex.Message);
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
                        summary.Add(LangMan.LS(LSID.LSID_Goal) + ": ~ub+1~" + GKUtils.GetTaskGoalStr(taskRec) + "~bu-1~");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]));
                        summary.Add(LangMan.LS(LSID.LSID_StartDate) + ": " + GKUtils.GEDCOMDateToStr(taskRec.StartDate, DateFormat.dfDD_MM_YYYY));
                        summary.Add(LangMan.LS(LSID.LSID_StopDate) + ": " + GKUtils.GEDCOMDateToStr(taskRec.StopDate, DateFormat.dfDD_MM_YYYY));

                        GKUtils.RecListNotesRefresh(taskRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowTaskInfo(): " + ex.Message);
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
                        summary.Add(LangMan.LS(LSID.LSID_Corresponder) + ": " + GKUtils.GetCorresponderStr(tree, commRec, true));
                        summary.Add(LangMan.LS(LSID.LSID_Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]));
                        summary.Add(LangMan.LS(LSID.LSID_Date) + ": " + GKUtils.GEDCOMDateToStr(commRec.Date, DateFormat.dfDD_MM_YYYY));

                        GKUtils.RecListNotesRefresh(commRec, summary);
                        GKUtils.RecListMediaRefresh(commRec, summary);
                    }
                }
                finally
                {
                    summary.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowCommunicationInfo(): " + ex.Message);
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

                        GKUtils.GetLocationLinks(tree, locRec, ref linkList);

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

                        GKUtils.RecListNotesRefresh(locRec, summary);
                        GKUtils.RecListMediaRefresh(locRec, summary);
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
                SysUtils.LogWrite("GKUtils.ShowLocationInfo(): " + ex.Message);
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
				result += "\\";
			}
			return result;
		}

        
        #endregion

		#region Graphics primitives
		
		public static GraphicsPath CreateRoundedRectangle(int x, int y, int width, int height, int radius)
	    {
			int xw = x + width;
			int yh = y + height;
			int xwr = xw - radius;
			int yhr = yh - radius;
			int xr = x + radius;
			int yr = y + radius;
			int r2 = radius * 2;
			int xwr2 = xw - r2;
			int yhr2 = yh - r2;

			GraphicsPath p = new GraphicsPath();
			p.StartFigure();

			p.AddArc(x, y, r2, r2, 180, 90); // Top Left Corner
			p.AddLine(xr, y, xwr, y); // Top Edge
			p.AddArc(xwr2, y, r2, r2, 270, 90); // Top Right Corner
			p.AddLine(xw, yr, xw, yhr); // Right Edge
			p.AddArc(xwr2, yhr2, r2, r2, 0, 90); // Bottom Right Corner
			p.AddLine(xwr, yh, xr, yh); // Bottom Edge
			p.AddArc(x, yhr2, r2, r2, 90, 90); // Bottom Left Corner
			p.AddLine(x, yhr, x, yr); // Left Edge

			p.CloseFigure();
			return p;
		}

		public static void DrawPathWithFuzzyLine(GraphicsPath path, Graphics gfx, Color base_color, int max_opacity, int width, int opaque_width)
		{
            int num_steps = width - opaque_width + 1;       // Number of pens we will use.
			float delta = (float)max_opacity / num_steps / num_steps;   // Change in alpha between pens.
			float alpha = delta;                            // Initial alpha.

			for (int thickness = width; thickness >= opaque_width; thickness--)
			{
				Color color = Color.FromArgb((int)alpha, base_color.R, base_color.G, base_color.B);

				using (Pen pen = new Pen(color, thickness))
				{
					pen.EndCap = LineCap.Round;
					pen.StartCap = LineCap.Round;
					gfx.DrawPath(pen, path);
				}

				alpha += delta;
			}
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

				char last_sym = res[res.Length - 1];
				if (consonants.IndexOf(last_sym) >= 0) {
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

		public static GEDCOMSex GetSex(string f_name, string f_pat, bool canQuery)
		{
			const string fem_endings = "ая";
			const string male_endings = "вгдйлмнопр";

			GEDCOMSex result = GEDCOMSex.svNone;
			if (string.IsNullOrEmpty(f_name)) return result;

			char nc = f_name[f_name.Length - 1];

			if (StrContains(fem_endings, nc)) {
				if (!string.IsNullOrEmpty(f_pat)) {
					char pc = f_pat[f_pat.Length - 1];

					if (StrContains(fem_endings, pc)) {
						result = GEDCOMSex.svFemale;
					} else if (StrContains(male_endings, pc)) {
						result = GEDCOMSex.svMale;
					}
				}
			} else if (StrContains(male_endings, nc)) {
				result = GEDCOMSex.svMale;
			}

			if (result == GEDCOMSex.svNone && canQuery) {
				string fn = f_name + " " + f_pat;
				DialogResult res = GKUtils.ShowQuestion("Не определяется пол человека по имени \"" + fn + "\". Это мужской пол?");
				result = (res == DialogResult.Yes) ? GEDCOMSex.svMale : GEDCOMSex.svFemale;
			}

			return result;
		}

		#endregion
	}
}
