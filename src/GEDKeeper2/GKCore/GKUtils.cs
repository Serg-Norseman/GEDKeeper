using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Reflection;
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
		public static T GetAssemblyAttribute<T>(System.Reflection.Assembly ass) where T : Attribute
		{
			object[] attributes = ass.GetCustomAttributes(typeof(T), false);
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

		#region Aux functions

		public static GEDCOMFormat GetGEDCOMFormat(GEDCOMTree tree)
		{
			if (tree != null) 
			{
				string sour = tree.Header.Source;

				for (GEDCOMFormat gf = GEDCOMFormat.gf_Native; gf <= GEDCOMFormat.gf_Last; gf++)
				{
					if (GKData.GEDCOMFormats[(int)gf].Sign == sour)
					{
						return gf;
					}
				}
			}
			
			return GEDCOMFormat.gf_Unknown;
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

		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}

		public static bool IsRecordAccess(GEDCOMRestriction restriction, ShieldState shieldState)
		{
			bool result = false;

			switch (shieldState) {
				case ShieldState.ssMaximum:
					result = (((restriction == GEDCOMRestriction.rnConfidential || restriction == GEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case ShieldState.ssMiddle:
					result = (((restriction == GEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case ShieldState.ssNone:
					result = true;
					break;
			}

			return result;
		}

		public static string aux_GetFamilyStr(GEDCOMFamilyRecord family)
		{
			return family.aux_GetFamilyStr(LangMan.LS(LSID.LSID_UnkMale), LangMan.LS(LSID.LSID_UnkFemale));
		}

		// FIXME: переработать
		public static string ConStrings(StringList aStrings)
		{
			string result = "";

			int num = aStrings.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (result != "") result += " ";
				result += aStrings[i].Trim();
			}

			return result;
		}

		public static void GetLocationLinks(GEDCOMTree tree, GEDCOMLocationRecord locRec, ref StringList aList)
		{
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMRecord rec = tree[i];
				if (rec is GEDCOMIndividualRecord)
				{
					GEDCOMIndividualRecord i_rec = rec as GEDCOMIndividualRecord;
					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						GEDCOMCustomEvent evt = i_rec.IndividualEvents[j];
						if (evt.Detail.Place.Location.Value == locRec)
						{
							aList.Add(GKUtils.GenRecordLink(rec, true) + ", " + GKUtils.GetEventName(evt).ToLower());
						}
					}
				}
				else
				{
					if (rec is GEDCOMFamilyRecord)
					{
						GEDCOMFamilyRecord f_rec = rec as GEDCOMFamilyRecord;
						int num3 = f_rec.FamilyEvents.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							GEDCOMCustomEvent evt = f_rec.FamilyEvents[j];
							if (evt.Detail.Place.Location.Value == locRec) {
								aList.Add(GKUtils.GenRecordLink(rec, true) + ", " + GKUtils.GetEventName(evt).ToLower());
							}
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
						st = (record as GEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;
					case GEDCOMRecordType.rtFamily:
						st = GKUtils.aux_GetFamilyStr(record as GEDCOMFamilyRecord);
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
				string nm = corresponder.aux_GetNameStr(true, false);
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
            taskRec.aux_GetTaskGoal(out gt, out tempRec);

			switch (gt) {
				case GKGoalType.gtIndividual:
					result = (tempRec as GEDCOMIndividualRecord).aux_GetNameStr(true, false);
					break;
				case GKGoalType.gtFamily:
					result = GKUtils.aux_GetFamilyStr(tempRec as GEDCOMFamilyRecord);
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

			GEDCOMCustomEvent attr = iRec.GetIndividualEvent(attrName);
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

		public static string GetEventDesc(GEDCOMEventDetail eventDetail)
		{
			string dt = GKUtils.GEDCOMCustomDateToStr(eventDetail.Date, DateFormat.dfDD_MM_YYYY, false);
			string place = eventDetail.Place.StringValue;
			GEDCOMLocationRecord location = eventDetail.Place.Location.Value as GEDCOMLocationRecord;

			if (place != "" && location != null)
			{
				place = GKUtils.HyperLink(location.XRef, place, 0);
			}

			string result;

			if (dt == "" && place == "")
			{
				result = "?";
			}
			else
			{
				if (dt == "")
				{
					result = place;
				}
				else
				{
					if (place == "")
					{
						result = dt;
					}
					else
					{
						result = dt + ", " + place;
					}
				}
			}

			return result;
		}

		public static string GetEventCause(GEDCOMEventDetail eventDetail)
		{
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

		public static string GEDCOMDateToStr(GEDCOMDate aDate, DateFormat aFormat)
		{
			string result = "";
			int year;
			ushort month;
			ushort day;
			aDate.GetDate(out year, out month, out day);

			if (year > 0 || month > 0 || day > 0)
			{
				if (aFormat != DateFormat.dfDD_MM_YYYY)
				{
					if (aFormat != DateFormat.dfYYYY_MM_DD)
					{
						if (aFormat == DateFormat.dfYYYY)
						{
							if (year > 0)
							{
								result = year.ToString().PadLeft(4, '_');
							}
						}
					}
					else
					{
						if (year > 0)
						{
							result = result + year.ToString().PadLeft(4, '_') + ".";
						}
						else
						{
							result += "____.";
						}
						if (month > 0)
						{
							result = result + SysUtils.NumUpdate((int)month, 2) + ".";
						}
						else
						{
							result += "__.";
						}
						if (day > 0)
						{
							result += SysUtils.NumUpdate((int)day, 2);
						}
						else
						{
							result += "__";
						}
					}
				}
				else
				{
					if (day > 0)
					{
						result = result + SysUtils.NumUpdate((int)day, 2) + ".";
					}
					else
					{
						result += "__.";
					}
					if (month > 0)
					{
						result = result + SysUtils.NumUpdate((int)month, 2) + ".";
					}
					else
					{
						result += "__.";
					}
					if (year > 0)
					{
						result += year.ToString().PadLeft(4, '_');
					}
					else
					{
						result += "____";
					}
				}
			}
			return result;
		}

		public static string GEDCOMCustomDateToStr(GEDCOMDateValue dateValue, DateFormat format, bool sign)
		{
		    if (dateValue == null) return string.Empty;
			string result = "";

			GEDCOMCustomDate date = dateValue.Value;

			if (date == null)
			{
				result = "";
			}
			else
			{
				if (date is GEDCOMDateApproximated)
				{
					result = GKUtils.GEDCOMDateToStr(date as GEDCOMDate, format);
					if (sign && (date as GEDCOMDateApproximated).Approximated != GEDCOMApproximated.daExact)
					{
						result = "~ " + result;
					}
				}
				else
				{
					if (date is GEDCOMDateRange)
					{
						GEDCOMDateRange dt_range = date as GEDCOMDateRange;
						if (dt_range.After.StringValue == "" && dt_range.Before.StringValue != "")
						{
							result = GKUtils.GEDCOMDateToStr(dt_range.Before, format);
							if (sign)
							{
								result = "< " + result;
							}
						}
						else
						{
							if (dt_range.After.StringValue != "" && dt_range.Before.StringValue == "")
							{
								result = GKUtils.GEDCOMDateToStr(dt_range.After, format);
								if (sign)
								{
									result += " >";
								}
							}
							else
							{
								if (dt_range.After.StringValue != "" && dt_range.Before.StringValue != "")
								{
									result = GKUtils.GEDCOMDateToStr(dt_range.After, format) + "-" + GKUtils.GEDCOMDateToStr(dt_range.Before, format);
								}
							}
						}
					}
					else
					{
						if (date is GEDCOMDatePeriod)
						{
							GEDCOMDatePeriod dt_period = date as GEDCOMDatePeriod;
							if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue == "")
							{
								result = GKUtils.GEDCOMDateToStr(dt_period.DateFrom, format);
								if (sign)
								{
									result += " >";
								}
							}
							else
							{
								if (dt_period.DateFrom.StringValue == "" && dt_period.DateTo.StringValue != "")
								{
									result = GKUtils.GEDCOMDateToStr(dt_period.DateTo, format);
									if (sign)
									{
										result = "< " + result;
									}
								}
								else
								{
									if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue != "")
									{
										result = GKUtils.GEDCOMDateToStr(dt_period.DateFrom, format) + "-" + GKUtils.GEDCOMDateToStr(dt_period.DateTo, format);
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

		public static string GEDCOMEventToDateStr(GEDCOMCustomEvent aEvent, DateFormat format, bool sign)
		{
			return ((aEvent == null) ? "" : GKUtils.GEDCOMCustomDateToStr(aEvent.Detail.Date, format, sign));
		}

		public static DateTime GEDCOMDateToDate(GEDCOMDateValue date)
		{
			return ((date == null) ? new DateTime(0) : date.aux_GetDate());
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

			GEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetDeathDate(GEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
		{
			if (iRec == null) return string.Empty;

			GEDCOMCustomEvent evt = iRec.GetIndividualEvent("DEAT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
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
				GEDCOMCustomEvent ev = iRec.GetIndividualEvent("DEAT");
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
							GEDCOMCustomEvent ev = iRec.GetIndividualEvent("DEAT");
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

			GEDCOMCustomEvent ev = iRec.GetIndividualEvent(evSign);
			if (ev != null)
			{
				int year;
				ushort am, ad;
				ev.Detail.Date.aux_GetIndependentDate(out year, out am, out ad, out YearBC);
				result = year;
			}
			return result;
		}

		public static double GetAbstractDate(GEDCOMEventDetail eventDetail)
		{
			bool dummy;
			return GetAbstractDate(eventDetail, out dummy);
		}

		public static double GetAbstractDate(GEDCOMEventDetail eventDetail, out bool YearBC)
		{
			double result = 0.0;
			YearBC = false;

			if (eventDetail != null)
			{
				int y;
				ushort i;
				ushort d;
				eventDetail.Date.aux_GetIndependentDate(out y, out i, out d, out YearBC);
				if (y > 0)
				{
					result = (double)y;
					if (i > 0)
					{
						result = (result + i / 12.0);
						if (d > 0)
						{
							result = (result + d / SysUtils.DaysInAMonth((ushort)y, i) / 12.0);
						}
					}
				}
			}

			return result;
		}

		public static string GetEventsYearsDiff(GEDCOMCustomEvent ev1, GEDCOMCustomEvent ev2, bool aCurEnd)
		{
			string result = "?";

			try
			{
				bool ybc, ybc2;
				double y = ((ev1 == null) ? -1.0 : GKUtils.GetAbstractDate(ev1.Detail, out ybc));
				double y2 = ((ev2 == null) ? -1.0 : GKUtils.GetAbstractDate(ev2.Detail, out ybc2));

				if (aCurEnd && y2 <= (double)1f)
				{
					y2 = ((double)DateTime.Now.Year + (double)DateTime.Now.Month / 12.0);
				}

				if (y == (double)-1f || y2 == (double)-1f)
				{
					result = "";
				}
				else
				{
					if (y == (double)0f || y2 == (double)0f)
					{
						result = "?";
					}
					else
					{
						long delta = SysUtils.Trunc(y2 - y);
						result = delta.ToString();
					}
				}

				//if ()
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

		public static string GetAge(GEDCOMIndividualRecord iRec, int ToYear)
		{
			string result = "";
            if (iRec == null) return result;

			try
			{
				GEDCOMCustomEvent bd;
				GEDCOMCustomEvent dd;
				iRec.GetLifeDates(out bd, out dd);

				if (ToYear == -1) {
					result = GKUtils.GetEventsYearsDiff(bd, dd, dd == null);
				} else {
					if (bd == null) {
						result = "";
					} else {
						ushort dummy;
						int i;
						bd.Detail.Date.aux_GetIndependentDate(out i, out dummy, out dummy);
						result = Convert.ToString(ToYear - i);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetAge(): " + ex.Message);
			}

			return result;
		}

		public static string GetMarriageDate(GEDCOMFamilyRecord fRec, DateFormat dateFormat)
		{
			string result = "";

			if (fRec != null)
			{
				GEDCOMFamilyEvent evt = fRec.GetFamilyEvent("MARR");
				result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			}

			return result;
		}

		public static string GetDaysForBirth(GEDCOMIndividualRecord iRec)
		{
            if (iRec == null) return string.Empty;

			string result = "";
			try
			{
				GEDCOMCustomEvent evt = iRec.GetIndividualEvent("DEAT");
				if (evt != null)
				{
				}
				else
				{
					evt = iRec.GetIndividualEvent("BIRT");
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

			GEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetDeathPlace(GEDCOMIndividualRecord iRec)
		{
            if (iRec == null) return string.Empty;

			GEDCOMCustomEvent evt = iRec.GetIndividualEvent("DEAT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetResidencePlace(GEDCOMIndividualRecord iRec, bool includeAddress)
		{
            if (iRec == null) return string.Empty;

			return GKUtils.GetPlaceStr(iRec.GetIndividualEvent("RESI"), includeAddress);
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

		public static void CreateListView(Control parent, out GKListView listView)
		{
			listView = new GKListView();
			listView.HideSelection = false;
			listView.LabelEdit = false;
			listView.FullRowSelect = true;
			listView.View = View.Details;
			listView.Dock = DockStyle.Fill;
			parent.Controls.Add(listView);
		}

		public static void CreateRecordsView(Control parent, GEDCOMTree tree, GEDCOMRecordType recType, out GKRecordsView recView)
		{
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

				int num = address.PhoneNumbers.Count - 1;
				for (int i = 0; i <= num; i++) {
					summary.Add("    " + address.PhoneNumbers[i].StringValue);
				}

				int num2 = address.EmailAddresses.Count - 1;
				for (int i = 0; i <= num2; i++) {
					summary.Add("    " + address.EmailAddresses[i].StringValue);
				}

				int num3 = address.WebPages.Count - 1;
				for (int i = 0; i <= num3; i++) {
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
			if (summary != null && eventDetail.SourceCitations.Count != 0)
			{
				summary.Add("    " + LangMan.LS(LSID.LSID_RPSources) + " (" + eventDetail.SourceCitations.Count.ToString() + "):");

				int num = eventDetail.SourceCitations.Count - 1;
				for (int idx = 0; idx <= num; idx++)
				{
					GEDCOMSourceCitation cit = eventDetail.SourceCitations[idx];
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

		public static void ShowEvent(GEDCOMRecord aSubject, StringList aToList, GEDCOMRecord aRec, GEDCOMCustomEvent evt)
		{
			if (aSubject is GEDCOMNoteRecord)
			{
				int num = evt.Detail.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (evt.Detail.Notes[i].Value == aSubject)
					{
						GKUtils.ShowLink(aSubject, aToList, aRec, evt, null);
					}
				}
			}
			else
			{
				if (aSubject is GEDCOMMultimediaRecord)
				{
					int num2 = evt.Detail.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						if (evt.Detail.MultimediaLinks[i].Value == aSubject)
						{
							GKUtils.ShowLink(aSubject, aToList, aRec, evt, null);
						}
					}
				}
				else
				{
					if (aSubject is GEDCOMSourceRecord)
					{
						int num3 = evt.Detail.SourceCitations.Count - 1;
						for (int i = 0; i <= num3; i++)
						{
							if (evt.Detail.SourceCitations[i].Value == aSubject)
							{
								GKUtils.ShowLink(aSubject, aToList, aRec, evt, evt.Detail.SourceCitations[i]);
							}
						}
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
        	for (int i = 0, count = tree.RecordsCount; i < count; i++) {
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
        					summary.Add("    " + GKUtils.HyperLink(ir.XRef, ir.aux_GetNameStr(true, false), 0));
        				}
        			}
        		}
        	}
		}

		public static void ShowPersonNamesakes(GEDCOMTree tree, GEDCOMIndividualRecord iRec, StringList summary)
        {
            try
            {
                StringList namesakes = new StringList();
                try
                {
                    string st = iRec.aux_GetNameStr(true, false);

                    int num3 = tree.RecordsCount - 1;
                    for (int i = 0; i <= num3; i++)
                    {
                        GEDCOMRecord rec = tree[i];
                        if (rec is GEDCOMIndividualRecord && rec != iRec)
                        {
                            GEDCOMIndividualRecord rel_person = rec as GEDCOMIndividualRecord;
                            string unk = rel_person.aux_GetNameStr(true, false);
                            if (st == unk)
                            {
                                namesakes.AddObject(unk + GKUtils.GetLifeStr(rel_person), rel_person);
                            }
                        }
                    }

                    if (namesakes.Count > 0)
                    {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Namesakes) + ":");

                        int num4 = namesakes.Count - 1;
                        for (int i = 0; i <= num4; i++)
                        {
                            GEDCOMIndividualRecord rel_person = namesakes.GetObject(i) as GEDCOMIndividualRecord;
                            summary.Add("    " + GKUtils.HyperLink(rel_person.XRef, namesakes[i], 0));
                        }
                    }
                }
                finally
                {
                    namesakes.Dispose();
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUtils.ShowPersonNamesakes(): " + ex.Message);
            }
        }

		public static void ShowSubjectLinks(GEDCOMRecord aInRecord, GEDCOMRecord subject, StringList aToList)
		{
			try
			{
				int num;

				if (subject is GEDCOMNoteRecord) {
					num = aInRecord.Notes.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (aInRecord.Notes[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is GEDCOMMultimediaRecord) {
					num = aInRecord.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (aInRecord.MultimediaLinks[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is GEDCOMSourceRecord) {
					num = aInRecord.SourceCitations.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (aInRecord.SourceCitations[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
						}
					}
				}

				if (aInRecord is GEDCOMIndividualRecord) {
					GEDCOMIndividualRecord iRec = aInRecord as GEDCOMIndividualRecord;
					num = iRec.IndividualEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						GKUtils.ShowEvent(subject, aToList, iRec, iRec.IndividualEvents[i]);
					}
				} else if (aInRecord is GEDCOMFamilyRecord) {
					GEDCOMFamilyRecord fRec = aInRecord as GEDCOMFamilyRecord;
					num = fRec.FamilyEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						GKUtils.ShowEvent(subject, aToList, fRec, fRec.FamilyEvents[i]);
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

                    int num = record.MultimediaLinks.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMMultimediaLink mmLink = record.MultimediaLinks[idx];
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

                    int num = record.Notes.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMNotes note = record.Notes[idx];

                        int num2 = note.Notes.Count - 1;
                        for (int i = 0; i <= num2; i++)
                        {
                            string st = note.Notes[i];
                            summary.Add(st);
                        }

                        if (idx < num)
                        {
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

                    int num = record.SourceCitations.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMSourceCitation cit = record.SourceCitations[idx];
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

                    int num = record.Associations.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMAssociation ast = record.Associations[idx];
                        string nm = ((ast.Individual == null) ? "" : ast.Individual.aux_GetNameStr(true, false));

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
                if (record.IndividualEvents.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Events) + ":");

                    int num = record.IndividualEvents.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMCustomEvent evt = record.IndividualEvents[idx];
                        string st = GKUtils.GetIndividualEventName(evt);

                        string sv = "";
                        if (evt.StringValue != "")
                        {
                            sv = evt.StringValue + ", ";
                        }
                        summary.Add(st + ": " + sv + GKUtils.GetEventDesc(evt.Detail));

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
                if (record.FamilyEvents.Count != 0)
                {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.LSID_Events) + ":");

                    int num = record.FamilyEvents.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMFamilyEvent evt = record.FamilyEvents[idx];
                        string st = GKUtils.GetFamilyEventName(evt);

                        summary.Add(st + ": " + GKUtils.GetEventDesc(evt.Detail));
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

                    int num = record.Groups.Count - 1;
                    for (int idx = 0; idx <= num; idx++)
                    {
                        GEDCOMPointer ptr = record.Groups[idx];
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

                        GEDCOMIndividualRecord irec = familyRec.Husband.Value as GEDCOMIndividualRecord;
                        string st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkMale) : GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Husband) + ": " + st + GKUtils.GetLifeStr(irec));

                        irec = (familyRec.Wife.Value as GEDCOMIndividualRecord);
                        st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkFemale) : GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0));
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
                            summary.Add("    " + GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0) + GKUtils.GetLifeStr(irec));
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
                            mbrList.AddObject(member.aux_GetNameStr(true, false), member);
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
                        summary.Add("~ub+1~" + iRec.aux_GetNameStr(true, true) + "~bu-1~");
                        summary.Add(LangMan.LS(LSID.LSID_Sex) + ": " + GKUtils.SexStr(iRec.Sex));
                        try
                        {
                            if (iRec.ChildToFamilyLinks.Count != 0)
                            {
                                summary.Add("");
                                summary.Add(LangMan.LS(LSID.LSID_Parents) + ":");

                                GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;

                                GEDCOMIndividualRecord relPerson = family.Husband.Value as GEDCOMIndividualRecord;
                                string st;
                                if (relPerson != null)
                                {
                                    st = GKUtils.HyperLink(relPerson.XRef, relPerson.aux_GetNameStr(true, false), 0);
                                }
                                else
                                {
                                	st = LangMan.LS(LSID.LSID_UnkMale);
                                }
                                summary.Add("  " + LangMan.LS(LSID.LSID_Father) + ": " + st + GKUtils.GetLifeStr(relPerson));

                                relPerson = (family.Wife.Value as GEDCOMIndividualRecord);
                                if (relPerson != null)
                                {
                                    st = GKUtils.HyperLink(relPerson.XRef, relPerson.aux_GetNameStr(true, false), 0);
                                }
                                else
                                {
                                	st = LangMan.LS(LSID.LSID_UnkFemale);
                                }
                                summary.Add("  " + LangMan.LS(LSID.LSID_Mother) + ": " + st + GKUtils.GetLifeStr(relPerson));
                            }
                        }
                        catch (Exception ex)
                        {
                            SysUtils.LogWrite("GKUtils.ShowPersonInfo().Parents(): " + ex.Message);
                        }

                        try
                        {
                            int num = iRec.SpouseToFamilyLinks.Count - 1;
                            for (int idx = 0; idx <= num; idx++)
                            {
                                GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[idx].Family;
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
                                        string marr = GKUtils.GetMarriageDate(family, DateFormat.dfDD_MM_YYYY);
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
                                            st = st + GKUtils.HyperLink(rel_person.XRef, rel_person.aux_GetNameStr(true, false), 0) + " (" + GKUtils.HyperLink(family.XRef, marr, 0) + ")";
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

                                        int num2 = family.Childrens.Count - 1;
                                        for (int i = 0; i <= num2; i++)
                                        {
                                            rel_person = (family.Childrens[i].Value as GEDCOMIndividualRecord);
                                            summary.Add("    " + GKUtils.HyperLink(rel_person.XRef, rel_person.aux_GetNameStr(true, false), 0) + GKUtils.GetLifeStr(rel_person));
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

                            int num = sourceRec.RepositoryCitations.Count - 1;
                            for (int i = 0; i <= num; i++)
                            {
                                GEDCOMRepositoryRecord rep = sourceRec.RepositoryCitations[i].Value as GEDCOMRepositoryRecord;
                                summary.Add("    " + GKUtils.HyperLink(rep.XRef, rep.RepositoryName, 0));
                            }
                        }

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        GEDCOMTree tree = sourceRec.Owner;

                        int num2 = tree.RecordsCount - 1;
                        for (int j = 0; j <= num2; j++)
                        {
                            GKUtils.ShowSubjectLinks(tree[j], sourceRec, link_list);
                        }

                        link_list.Sort();

                        int num3 = link_list.Count - 1;
                        for (int j = 0; j <= num3; j++)
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

                        int num = tree.RecordsCount - 1;
                        for (int i = 0; i <= num; i++)
                        {
                            GEDCOMRecord rec = tree[i];

                            if (rec is GEDCOMSourceRecord)
                            {
                                GEDCOMSourceRecord srcRec = rec as GEDCOMSourceRecord;

                                int num2 = srcRec.RepositoryCitations.Count - 1;
                                for (int j = 0; j <= num2; j++)
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

                            int num = researchRec.Tasks.Count - 1;
                            for (int i = 0; i <= num; i++)
                            {
                                GEDCOMTaskRecord taskRec = researchRec.Tasks[i].Value as GEDCOMTaskRecord;
                                summary.Add("    " + GKUtils.GenRecordLink(taskRec, false));
                            }
                        }

                        if (researchRec.Communications.Count > 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPCommunications) + ":");

                            int num2 = researchRec.Communications.Count - 1;
                            for (int i = 0; i <= num2; i++)
                            {
                                GEDCOMCommunicationRecord corrRec = researchRec.Communications[i].Value as GEDCOMCommunicationRecord;
                                summary.Add("    " + GKUtils.GenRecordLink(corrRec, false));
                            }
                        }

                        if (researchRec.Groups.Count != 0)
                        {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.LSID_RPGroups) + ":");

                            int num3 = researchRec.Groups.Count - 1;
                            for (int i = 0; i <= num3; i++)
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

                            int num = linkList.Count - 1;
                            for (int i = 0; i <= num; i++)
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

        #region Color utils
        
		public static Color darker(Color color, float fraction)
		{
			float factor = (1.0f - fraction);

			int rgb = color.ToArgb();
			int red = (rgb >> 16) & 0xFF;
			int green = (rgb >> 8) & 0xFF;
			int blue = (rgb >> 0) & 0xFF;
			int alpha = (rgb >> 24) & 0xFF;

			red = (int) (red * factor);
			green = (int) (green * factor);
			blue = (int) (blue * factor);

			red = (red < 0) ? 0 : red;
			green = (green < 0) ? 0 : green;
			blue = (blue < 0) ? 0 : blue;

			return Color.FromArgb(red, green, blue);
		}

		public static Color lighter(Color color, float fraction)
		{
			float factor = (1.0f + fraction);
			
			int rgb = color.ToArgb();
			int red = (rgb >> 16) & 0xFF;
			int green = (rgb >> 8) & 0xFF;
			int blue = (rgb >> 0) & 0xFF;
			int alpha = (rgb >> 24) & 0xFF;

			red = (int) (red * factor);
			green = (int) (green * factor);
			blue = (int) (blue * factor);

			if (red < 0) {
				red = 0;
			} else if (red > 255) {
				red = 255;
			}
			if (green < 0) {
				green = 0;
			} else if (green > 255) {
				green = 255;
			}
			if (blue < 0) {
				blue = 0;
			} else if (blue > 255) {
				blue = 255;
			}

			//int alpha = color.getAlpha();

			return Color.FromArgb(red, green, blue);
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
	}
}
