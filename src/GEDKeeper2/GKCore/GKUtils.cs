using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKUI.Controls;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public static class GKUtils
	{
        public const int ProvedLifeLength = 130;

		public static T GetAssemblyAttribute<T>(this System.Reflection.Assembly ass) where T : Attribute
		{
			object[] attributes = ass.GetCustomAttributes(typeof(T), false);
			if (attributes == null || attributes.Length == 0)
				return null;
			return attributes.OfType<T>().SingleOrDefault();
		}

		#region Aux functions

		public static TGEDCOMFormat GetGEDCOMFormat(TGEDCOMTree tree)
		{
			if (tree != null) 
			{
				string sour = tree.Header.Source;

				for (TGEDCOMFormat gf = TGEDCOMFormat.gf_Native; gf <= TGEDCOMFormat.gf_Last; gf++)
				{
					if (GKData.GEDCOMFormats[(int)gf].Sign == sour)
					{
						return gf;
					}
				}
			}
			
			return TGEDCOMFormat.gf_Unknown;
		}

		public static string SexStr(TGEDCOMSex sex)
		{
			return LangMan.LS(GKData.SexData[(int)sex].NameId);
		}

		public static TGEDCOMSex GetSexBySign(char sexSign)
		{
			TGEDCOMSex result = TGEDCOMSex.svNone;
			
			switch (sexSign) {
				case 'F':
					result = TGEDCOMSex.svFemale;
					break;
				case 'M':
					result = TGEDCOMSex.svMale;
					break;
				case 'U':
					result = TGEDCOMSex.svUndetermined;
					break;
			}
			
			return result;
		}

		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}

		public static bool IsRecordAccess(TGEDCOMRestriction restriction, ShieldState shieldState)
		{
			bool result = false;

			switch (shieldState) {
				case ShieldState.ssMaximum:
					result = (((restriction == TGEDCOMRestriction.rnConfidential || restriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case ShieldState.ssMiddle:
					result = (((restriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case ShieldState.ssNone:
					result = true;
					break;
			}

			return result;
		}

		public static string aux_GetFamilyStr(TGEDCOMFamilyRecord family)
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

		public static void GetLocationLinks(TGEDCOMTree tree, TGEDCOMLocationRecord locRec, ref StringList aList)
		{
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = tree[i];
				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = rec as TGEDCOMIndividualRecord;
					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMCustomEvent evt = i_rec.IndividualEvents[j];
						if (evt.Detail.Place.Location.Value == locRec)
						{
							aList.Add(GKUtils.GenRecordLink(rec, true) + ", " + GKUtils.GetEventName(evt).ToLower());
						}
					}
				}
				else
				{
					if (rec is TGEDCOMFamilyRecord)
					{
						TGEDCOMFamilyRecord f_rec = rec as TGEDCOMFamilyRecord;
						int num3 = f_rec.FamilyEvents.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							TGEDCOMCustomEvent evt = f_rec.FamilyEvents[j];
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

		public static string GenRecordLink(TGEDCOMRecord record, bool signed)
		{
            string result = "";

            if (record != null) {
				string sign = "";

                if (signed) {
					TGEDCOMRecordType recordType = record.RecordType;
					if (recordType != TGEDCOMRecordType.rtIndividual) {
						if (recordType == TGEDCOMRecordType.rtFamily || (byte)recordType - (byte)TGEDCOMRecordType.rtMultimedia < (byte)TGEDCOMRecordType.rtResearch)
						{
							sign = LangMan.LS(GKData.RecordTypes[(int)record.RecordType]) + ": ";
						}
					} else {
						sign = "";
					}
				}

                string st;
				switch (record.RecordType) {
					case TGEDCOMRecordType.rtIndividual:
						st = (record as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;
					case TGEDCOMRecordType.rtFamily:
						st = GKUtils.aux_GetFamilyStr(record as TGEDCOMFamilyRecord);
						break;
					case TGEDCOMRecordType.rtMultimedia:
						st = (record as TGEDCOMMultimediaRecord).FileReferences[0].Title;
						break;
					case TGEDCOMRecordType.rtSource:
						st = (record as TGEDCOMSourceRecord).FiledByEntry;
						break;
					case TGEDCOMRecordType.rtRepository:
						st = (record as TGEDCOMRepositoryRecord).RepositoryName;
						break;
					case TGEDCOMRecordType.rtGroup:
						st = (record as TGEDCOMGroupRecord).GroupName;
						break;
					case TGEDCOMRecordType.rtResearch:
						st = (record as TGEDCOMResearchRecord).ResearchName;
						break;
					case TGEDCOMRecordType.rtTask:
						st = GKUtils.GetTaskGoalStr(record as TGEDCOMTaskRecord);
						break;
					case TGEDCOMRecordType.rtCommunication:
						st = (record as TGEDCOMCommunicationRecord).CommName;
						break;
					case TGEDCOMRecordType.rtLocation:
						st = (record as TGEDCOMLocationRecord).LocationName;
						break;
					default:
						st = record.XRef;
						break;
				}

                result = GKUtils.HyperLink(record.XRef, sign + st, 0);
			}

            return result;
		}

		public static string GetCorresponderStr(TGEDCOMTree tree, TGEDCOMCommunicationRecord commRec, bool aLink)
		{
			string result = "";
			TCommunicationDir dir = TCommunicationDir.cdFrom;
			TGEDCOMIndividualRecord corresponder = null;
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

		public static string GetTaskGoalStr(TGEDCOMTaskRecord taskRec)
		{
		    if (taskRec == null) return string.Empty;
            
            string result = "";
            
            TGoalType gt;
			TGEDCOMRecord tempRec;
            taskRec.aux_GetTaskGoal(out gt, out tempRec);

			switch (gt) {
				case TGoalType.gtIndividual:
					result = (tempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
					break;
				case TGoalType.gtFamily:
					result = GKUtils.aux_GetFamilyStr(tempRec as TGEDCOMFamilyRecord);
					break;
				case TGoalType.gtSource:
					result = (tempRec as TGEDCOMSourceRecord).FiledByEntry;
					break;
				case TGoalType.gtOther:
					result = taskRec.Goal;
					break;
			}

			if (gt != TGoalType.gtOther)
			{
				result = "[" + LangMan.LS(GKData.GoalNames[(int)gt]) + "] " + result;
			}

			return result;
		}

		#endregion

		#region Event Utils
		
		public static string GetAttributeValue(TGEDCOMIndividualRecord iRec, string attrName)
		{
			if (iRec == null) return string.Empty;

			TGEDCOMCustomEvent attr = iRec.GetIndividualEvent(attrName);
			string result = ((attr == null) ? "" : attr.StringValue);
			return result;
		}

		public static TPersonEventKind GetPersonEventKindBySign(string aSign)
		{
			TPersonEventKind res = TPersonEventKind.ekFact;

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

		public static string GetEventName(TGEDCOMCustomEvent aEvent)
		{
			string result = "";

			if (aEvent is TGEDCOMIndividualEvent || aEvent is TGEDCOMIndividualAttribute)
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
			else if (aEvent is TGEDCOMFamilyEvent)
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

		public static string GetAttributeStr(TGEDCOMIndividualAttribute iAttr)
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

		public static string GetEventDesc(TGEDCOMEventDetail eventDetail)
		{
			string dt = GKUtils.GEDCOMCustomDateToStr(eventDetail.Date, DateFormat.dfDD_MM_YYYY, false);
			string place = eventDetail.Place.StringValue;
			TGEDCOMLocationRecord location = eventDetail.Place.Location.Value as TGEDCOMLocationRecord;

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

		public static string GetEventCause(TGEDCOMEventDetail eventDetail)
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

        public static string GetIndividualEventName(TGEDCOMCustomEvent evt)
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

        public static string GetFamilyEventName(TGEDCOMFamilyEvent evt)
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

		public static string GEDCOMDateToStr(TGEDCOMDate aDate, DateFormat aFormat)
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

		public static string GEDCOMCustomDateToStr(TGEDCOMDateValue dateValue, DateFormat format, bool sign)
		{
		    if (dateValue == null) return string.Empty;
			string result = "";

			TGEDCOMCustomDate date = dateValue.Value;

			if (date == null)
			{
				result = "";
			}
			else
			{
				if (date is TGEDCOMDateApproximated)
				{
					result = GKUtils.GEDCOMDateToStr(date as TGEDCOMDate, format);
					if (sign && (date as TGEDCOMDateApproximated).Approximated != TGEDCOMApproximated.daExact)
					{
						result = "~ " + result;
					}
				}
				else
				{
					if (date is TGEDCOMDateRange)
					{
						TGEDCOMDateRange dt_range = date as TGEDCOMDateRange;
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
						if (date is TGEDCOMDatePeriod)
						{
							TGEDCOMDatePeriod dt_period = date as TGEDCOMDatePeriod;
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
							if (date is TGEDCOMDate)
							{
								result = GKUtils.GEDCOMDateToStr(date as TGEDCOMDate, format);
							}
						}
					}
				}
			}

			if ((date is TGEDCOMDate) && (date as TGEDCOMDate).YearBC) {
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

		public static string GEDCOMEventToDateStr(TGEDCOMCustomEvent aEvent, DateFormat format, bool sign)
		{
			return ((aEvent == null) ? "" : GKUtils.GEDCOMCustomDateToStr(aEvent.Detail.Date, format, sign));
		}

		public static DateTime GEDCOMDateToDate(TGEDCOMDateValue date)
		{
			return ((date == null) ? new DateTime(0) : date.aux_GetDate());
		}

		public static string CompactDate(string date)
		{
			string result = date;
			while (result.IndexOf("__.") == 0) result = result.Remove(0, 3);
			return result;
		}

		public static string GetBirthDate(TGEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
		{
			if (iRec == null) return string.Empty;

			TGEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetDeathDate(TGEDCOMIndividualRecord iRec, DateFormat dateFormat, bool compact)
		{
			if (iRec == null) return string.Empty;

			TGEDCOMCustomEvent evt = iRec.GetIndividualEvent("DEAT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetLifeStr(TGEDCOMIndividualRecord iRec)
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
				TGEDCOMCustomEvent ev = iRec.GetIndividualEvent("DEAT");
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

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign)
		{
			bool dummy;
			return GetIndependentYear(iRec, evSign, out dummy);
		}

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign, out bool YearBC)
		{
			int result = -1;
            YearBC = false;
            if (iRec == null) return result;

			TGEDCOMCustomEvent ev = iRec.GetIndividualEvent(evSign);
			if (ev != null)
			{
				int year;
				ushort am, ad;
				ev.Detail.Date.aux_GetIndependentDate(out year, out am, out ad, out YearBC);
				result = year;
			}
			return result;
		}

		public static double GetAbstractDate(TGEDCOMEventDetail eventDetail)
		{
			bool dummy;
			return GetAbstractDate(eventDetail, out dummy);
		}

		public static double GetAbstractDate(TGEDCOMEventDetail eventDetail, out bool YearBC)
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

		public static string GetEventsYearsDiff(TGEDCOMCustomEvent ev1, TGEDCOMCustomEvent ev2, bool aCurEnd)
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

		public static string GetLifeExpectancy(TGEDCOMIndividualRecord iRec)
		{
			string result = "";
            if (iRec == null) return result;

			try
			{
				TGEDCOMCustomEvent ev = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					if (evt.Name == "BIRT")
					{
						ev = evt;
					}
					else if (evt.Name == "DEAT")
					{
						ev2 = evt;
					}
				}

				result = GKUtils.GetEventsYearsDiff(ev, ev2, false);
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GKUtils.GetLifeExpectancy(): " + ex.Message);
			}

			return result;
		}

		public static string GetAge(TGEDCOMIndividualRecord iRec, int ToYear)
		{
			string result = "";
            if (iRec == null) return result;

			try
			{
				TGEDCOMCustomEvent ev1 = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					if (evt.Name == "BIRT" && ev1 == null)
					{
						ev1 = evt;
					}
					else
					{
						if (evt.Name == "DEAT" && ev2 == null)
						{
							ev2 = evt;
						}
					}
				}

				if (ToYear == -1)
				{
					result = GKUtils.GetEventsYearsDiff(ev1, ev2, ev2 == null);
				}
				else
				{
					if (ev1 == null)
					{
						result = "";
					}
					else
					{
						ushort dummy;
						int i;
						ev1.Detail.Date.aux_GetIndependentDate(out i, out dummy, out dummy);
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

		public static string GetMarriageDate(TGEDCOMFamilyRecord fRec, DateFormat dateFormat)
		{
			string result = "";

			if (fRec != null)
			{
				TGEDCOMFamilyEvent evt = fRec.aux_GetFamilyEvent("MARR");
				result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			}

			return result;
		}

		public static string GetDaysForBirth(TGEDCOMIndividualRecord iRec)
		{
            if (iRec == null) return string.Empty;

			string result = "";
			try
			{
				TGEDCOMCustomEvent evt = iRec.GetIndividualEvent("DEAT");
				if (evt != null)
				{
				}
				else
				{
					evt = iRec.GetIndividualEvent("BIRT");
					if (evt != null)
					{
						TGEDCOMDate dt = evt.Detail.Date.Value as TGEDCOMDate;
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

		public static string GetBirthPlace(TGEDCOMIndividualRecord iRec)
		{
            if (iRec == null) return string.Empty;

			TGEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetDeathPlace(TGEDCOMIndividualRecord iRec)
		{
            if (iRec == null) return string.Empty;

			TGEDCOMCustomEvent evt = iRec.GetIndividualEvent("DEAT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetResidencePlace(TGEDCOMIndividualRecord iRec, bool includeAddress)
		{
            if (iRec == null) return string.Empty;

			return GKUtils.GetPlaceStr(iRec.GetIndividualEvent("RESI"), includeAddress);
		}

		public static string GetPlaceStr(TGEDCOMCustomEvent aEvent, bool includeAddress)
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

		public static void CreateRecordsView(Control parent, TGEDCOMTree tree, TGEDCOMRecordType recType, out GKRecordsView recView)
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

        public static void ShowAddressSummary(TGEDCOMAddress address, StringList summary)
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

		public static void ShowDetailCause(TGEDCOMEventDetail eventDetail, StringList summary)
		{
			string cause = GKUtils.GetEventCause(eventDetail);
			if (summary != null && cause != "")
			{
				summary.Add("    " + cause);
			}
		}

		public static void ShowDetailInfo(TGEDCOMEventDetail eventDetail, StringList summary)
		{
			if (summary != null && eventDetail.SourceCitations.Count != 0)
			{
				summary.Add("    " + LangMan.LS(LSID.LSID_RPSources) + " (" + eventDetail.SourceCitations.Count.ToString() + "):");

				int num = eventDetail.SourceCitations.Count - 1;
				for (int idx = 0; idx <= num; idx++)
				{
					TGEDCOMSourceCitation cit = eventDetail.SourceCitations[idx];
					TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
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

		public static void ShowEvent(TGEDCOMRecord aSubject, StringList aToList, TGEDCOMRecord aRec, TGEDCOMCustomEvent evt)
		{
			if (aSubject is TGEDCOMNoteRecord)
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
				if (aSubject is TGEDCOMMultimediaRecord)
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
					if (aSubject is TGEDCOMSourceRecord)
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

		public static void ShowLink(TGEDCOMRecord aSubject, StringList aToList, TGEDCOMRecord aRec, TGEDCOMTag aTag, TGEDCOMPointer aExt)
		{
			string prefix;
			if (aSubject is TGEDCOMSourceRecord && aExt != null) {
                TGEDCOMSourceCitation cit = (aExt as TGEDCOMSourceCitation);
				if (cit.Page != "") {
					prefix = cit.Page + ": ";
				} else {
					prefix = "";
				}
			} else {
				prefix = "";
			}

			string suffix;
			if (aTag != null && aTag is TGEDCOMCustomEvent) {
				suffix = ", " + GKUtils.GetEventName(aTag as TGEDCOMCustomEvent).ToLower();
			} else {
				suffix = "";
			}
			aToList.Add("    " + prefix + GKUtils.GenRecordLink(aRec, true) + suffix);
		}

		public static void ShowPersonExtInfo(TGEDCOMTree tree, TGEDCOMIndividualRecord iRec, StringList summary)
		{
		    //if (tree == null || iRec == null || summary == null) return;

        	summary.Add("");
        	for (int i = 0, count = tree.RecordsCount; i < count; i++) {
        		TGEDCOMRecord rec = tree[i];
        		if (rec.RecordType == TGEDCOMRecordType.rtIndividual) {
        			TGEDCOMIndividualRecord ir = rec as TGEDCOMIndividualRecord;
        			
        			bool first = true;
        			for (int k = 0, cnt = ir.Associations.Count; k < cnt; k++) {
                        TGEDCOMAssociation asso = ir.Associations[k];

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

		public static void ShowPersonNamesakes(TGEDCOMTree tree, TGEDCOMIndividualRecord iRec, StringList summary)
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
                        TGEDCOMRecord rec = tree[i];
                        if (rec is TGEDCOMIndividualRecord && rec != iRec)
                        {
                            TGEDCOMIndividualRecord rel_person = rec as TGEDCOMIndividualRecord;
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
                            TGEDCOMIndividualRecord rel_person = namesakes.GetObject(i) as TGEDCOMIndividualRecord;
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

		public static void ShowSubjectLinks(TGEDCOMRecord aInRecord, TGEDCOMRecord subject, StringList aToList)
		{
			try
			{
				int num;

				if (subject is TGEDCOMNoteRecord) {
					num = aInRecord.Notes.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (aInRecord.Notes[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is TGEDCOMMultimediaRecord) {
					num = aInRecord.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (aInRecord.MultimediaLinks[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is TGEDCOMSourceRecord) {
					num = aInRecord.SourceCitations.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (aInRecord.SourceCitations[i].Value == subject) {
							GKUtils.ShowLink(subject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
						}
					}
				}

				if (aInRecord is TGEDCOMIndividualRecord) {
					TGEDCOMIndividualRecord iRec = aInRecord as TGEDCOMIndividualRecord;
					num = iRec.IndividualEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						GKUtils.ShowEvent(subject, aToList, iRec, iRec.IndividualEvents[i]);
					}
				} else if (aInRecord is TGEDCOMFamilyRecord) {
					TGEDCOMFamilyRecord fRec = aInRecord as TGEDCOMFamilyRecord;
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

        public static void RecListMediaRefresh(TGEDCOMRecord record, StringList summary)
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
                        TGEDCOMMultimediaLink mmLink = record.MultimediaLinks[idx];
                        TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;

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

        public static void RecListNotesRefresh(TGEDCOMRecord record, StringList summary)
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
                        TGEDCOMNotes note = record.Notes[idx];

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

        public static void RecListSourcesRefresh(TGEDCOMRecord record, StringList summary)
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
                        TGEDCOMSourceCitation cit = record.SourceCitations[idx];
                        TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;

                        if (sourceRec != null)
                        {
                            string nm = "\"" + sourceRec.FiledByEntry + "\"";

                            if (cit.Page != "")
                            {
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

        public static void RecListAssociationsRefresh(TGEDCOMIndividualRecord record, StringList summary)
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
                        TGEDCOMAssociation ast = record.Associations[idx];
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

        public static void RecListIndividualEventsRefresh(TGEDCOMIndividualRecord record, StringList summary)
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
                        TGEDCOMCustomEvent evt = record.IndividualEvents[idx];
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

        public static void RecListFamilyEventsRefresh(TGEDCOMFamilyRecord record, StringList summary)
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
                        TGEDCOMFamilyEvent evt = record.FamilyEvents[idx];
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

        public static void RecListGroupsRefresh(TGEDCOMIndividualRecord record, StringList summary)
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
                        TGEDCOMPointer ptr = record.Groups[idx];
                        TGEDCOMGroupRecord grp = ptr.Value as TGEDCOMGroupRecord;
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

        public static void ShowFamilyInfo(TGEDCOMFamilyRecord familyRec, StringList summary, ShieldState shieldState)
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

                        TGEDCOMIndividualRecord irec = familyRec.Husband.Value as TGEDCOMIndividualRecord;
                        string st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkMale) : GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Husband) + ": " + st + GKUtils.GetLifeStr(irec));

                        irec = (familyRec.Wife.Value as TGEDCOMIndividualRecord);
                        st = ((irec == null) ? LangMan.LS(LSID.LSID_UnkFemale) : GKUtils.HyperLink(irec.XRef, irec.aux_GetNameStr(true, false), 0));
                        summary.Add(LangMan.LS(LSID.LSID_Wife) + ": " + st + GKUtils.GetLifeStr(irec));

                        summary.Add("");
                        if (familyRec.Childrens.Count != 0)
                        {
                        	summary.Add(LangMan.LS(LSID.LSID_Childs) + ":");
                        }

                        int num = familyRec.Childrens.Count - 1;
                        for (int i = 0; i <= num; i++)
                        {
                            irec = (familyRec.Childrens[i].Value as TGEDCOMIndividualRecord);
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
        
        public static void ShowGroupInfo(TGEDCOMGroupRecord groupRec, StringList summary)
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

                        int num = groupRec.Members.Count - 1;
                        for (int i = 0; i <= num; i++)
                        {
                            TGEDCOMPointer ptr = groupRec.Members[i];
                            TGEDCOMIndividualRecord member = ptr.Value as TGEDCOMIndividualRecord;
                            mbrList.AddObject(member.aux_GetNameStr(true, false), member);
                        }
                        mbrList.Sort();

                        int num2 = mbrList.Count - 1;
                        for (int i = 0; i <= num2; i++)
                        {
                            TGEDCOMIndividualRecord member = mbrList.GetObject(i) as TGEDCOMIndividualRecord;
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

        public static void ShowMultimediaInfo(TGEDCOMMultimediaRecord mediaRec, StringList summary)
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

                        TGEDCOMTree tree = mediaRec.Owner;
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

        public static void ShowNoteInfo(TGEDCOMNoteRecord noteRec, StringList summary)
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

                        TGEDCOMTree tree = noteRec.Owner;
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

        public static void ShowPersonInfo(TGEDCOMIndividualRecord iRec, StringList summary, ShieldState shieldState)
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
                        TGEDCOMTree tree = iRec.Owner;

                        summary.Add("");
                        summary.Add("~ub+1~" + iRec.aux_GetNameStr(true, true) + "~bu-1~");
                        summary.Add(LangMan.LS(LSID.LSID_Sex) + ": " + GKUtils.SexStr(iRec.Sex));
                        try
                        {
                            if (iRec.ChildToFamilyLinks.Count != 0)
                            {
                                summary.Add("");
                                summary.Add(LangMan.LS(LSID.LSID_Parents) + ":");

                                TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;

                                TGEDCOMIndividualRecord relPerson = family.Husband.Value as TGEDCOMIndividualRecord;
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

                                relPerson = (family.Wife.Value as TGEDCOMIndividualRecord);
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
                                TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[idx].Family;
                                if (family != null)
                                {
                                    if (GKUtils.IsRecordAccess(family.Restriction, shieldState))
                                    {
                                        string st;
                                        TGEDCOMPointer sp;
                                        string unk;
                                        if (iRec.Sex == TGEDCOMSex.svMale)
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
                                        TGEDCOMIndividualRecord rel_person = sp.Value as TGEDCOMIndividualRecord;
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
                                            rel_person = (family.Childrens[i].Value as TGEDCOMIndividualRecord);
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

        public static void ShowSourceInfo(TGEDCOMSourceRecord sourceRec, StringList summary)
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
                                TGEDCOMRepositoryRecord rep = sourceRec.RepositoryCitations[i].Value as TGEDCOMRepositoryRecord;
                                summary.Add("    " + GKUtils.HyperLink(rep.XRef, rep.RepositoryName, 0));
                            }
                        }

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.LSID_Links) + ":");

                        TGEDCOMTree tree = sourceRec.Owner;

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

        public static void ShowRepositoryInfo(TGEDCOMRepositoryRecord repositoryRec, StringList summary)
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

                        TGEDCOMTree tree = repositoryRec.Owner;

                        int num = tree.RecordsCount - 1;
                        for (int i = 0; i <= num; i++)
                        {
                            TGEDCOMRecord rec = tree[i];

                            if (rec is TGEDCOMSourceRecord)
                            {
                                TGEDCOMSourceRecord srcRec = rec as TGEDCOMSourceRecord;

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

        public static void ShowResearchInfo(TGEDCOMResearchRecord researchRec, StringList summary)
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
                                TGEDCOMTaskRecord taskRec = researchRec.Tasks[i].Value as TGEDCOMTaskRecord;
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
                                TGEDCOMCommunicationRecord corrRec = researchRec.Communications[i].Value as TGEDCOMCommunicationRecord;
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
                                TGEDCOMGroupRecord grp = researchRec.Groups[i].Value as TGEDCOMGroupRecord;
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

        public static void ShowTaskInfo(TGEDCOMTaskRecord taskRec, StringList summary)
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

        public static void ShowCommunicationInfo(TGEDCOMCommunicationRecord commRec, StringList summary)
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
                        TGEDCOMTree tree = commRec.Owner;

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

        public static void ShowLocationInfo(TGEDCOMLocationRecord locRec, StringList summary)
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

                        TGEDCOMTree tree = locRec.Owner;

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
	}
}
