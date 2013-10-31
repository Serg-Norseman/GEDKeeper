using System;
using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public static class GKUtils
	{
		static GKUtils()
		{
		}

		#region Data Manipulation

		public static TGEDCOMSourceRecord aux_FindSource(TGEDCOMTree tree, string sourceName)
		{
			TGEDCOMSourceRecord result = null;
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = tree[i];

				if (rec is TGEDCOMSourceRecord && (rec as TGEDCOMSourceRecord).FiledByEntry == sourceName)
				{
					result = (rec as TGEDCOMSourceRecord);
					break;
				}
			}
			return result;
		}

		public static void aux_GetSourcesList(TGEDCOMTree tree, StringList aSources)
		{
			if (aSources != null)
			{
				aSources.Clear();
				int num = tree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = tree[i];
					if (rec is TGEDCOMSourceRecord)
					{
						aSources.AddObject((rec as TGEDCOMSourceRecord).FiledByEntry, rec);
					}
				}
			}
		}

		public static void CleanFamily(TGEDCOMFamilyRecord family)
		{
			if (family != null)
			{
				int num = family.Childrens.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMIndividualRecord child = family.Childrens[i].Value as TGEDCOMIndividualRecord;
					child.DeleteChildToFamilyLink(family);
				}

				TGEDCOMIndividualRecord spouse;

				spouse = family.Husband.Value as TGEDCOMIndividualRecord;
				family.aux_RemoveSpouse(spouse);

				spouse = (family.Wife.Value as TGEDCOMIndividualRecord);
				family.aux_RemoveSpouse(spouse);
			}
		}
		
		public static TGEDCOMCustomEvent CreateEventEx(TGEDCOMTree tree, TGEDCOMRecord aRec, string evSign, string evDate, string evPlace)
		{
			TGEDCOMCustomEvent result;

			if (aRec is TGEDCOMIndividualRecord)
			{
				TGEDCOMIndividualRecord ind_rec = aRec as TGEDCOMIndividualRecord;
				if (GKUtils.GetPersonEventKindBySign(evSign) == TPersonEventKind.ekEvent)
				{
					result = new TGEDCOMIndividualEvent(tree, ind_rec, "", "");
				}
				else
				{
					result = new TGEDCOMIndividualAttribute(tree, ind_rec, "", "");
				}
				ind_rec.AddIndividualEvent(result);
			}
			else
			{
				if (!(aRec is TGEDCOMFamilyRecord))
				{
					result = null;
					return result;
				}
				TGEDCOMFamilyRecord fam_rec = aRec as TGEDCOMFamilyRecord;
				result = new TGEDCOMFamilyEvent(tree, fam_rec, "", "");
				fam_rec.FamilyEvents.Add(result as TGEDCOMFamilyEvent);
			}
			result.Name = evSign;

			if (evDate != "") {
				result.Detail.Date.ParseString(evDate);
			}

			if (evPlace != "") {
				result.Detail.Place.StringValue = evPlace;
			}

			return result;
		}

		public static TGEDCOMIndividualRecord CreatePersonEx(TGEDCOMTree tree, string iName, string iPatronymic, string iSurname, TGEDCOMSex iSex, bool birthEvent)
		{
			TGEDCOMIndividualRecord iRec = new TGEDCOMIndividualRecord(tree, tree, "", "");
			iRec.InitNew();
			iRec.Sex = iSex;
			iRec.ChangeDate.ChangeDateTime = DateTime.Now;

			TGEDCOMPersonalName pn = new TGEDCOMPersonalName(tree, iRec, "", "");
			pn.StringValue = iName.Trim() + " " + iPatronymic.Trim() + " /" + iSurname.Trim() + "/";
			iRec.AddPersonalName(pn);

			tree.AddRecord(iRec);

			if (birthEvent) GKUtils.CreateEventEx(tree, iRec, "BIRT", "", "");

			return iRec;
		}

		#endregion

		#region Aux functions

		public static TGEDCOMFormat GetGEDCOMFormat(TGEDCOMTree tree)
		{
			string sour = tree.Header.Source;

			TGEDCOMFormat res = TGEDCOMFormat.gf_Unknown;
			for (TGEDCOMFormat gf = TGEDCOMFormat.gf_Native; gf <= TGEDCOMFormat.gf_Last; gf++)
			{
				if (GKData.GEDCOMFormats[(int)gf].Sign == sour)
				{
					res = gf;
					break;
				}
			}
			return res;
		}

		public static string SexStr(TGEDCOMSex Sex)
		{
			return LangMan.LSList[(int)GKData.SexData[(int)Sex].NameId - 1];
		}

		public static TGEDCOMSex GetSexBySign(char SexSign)
		{
			TGEDCOMSex Result = TGEDCOMSex.svNone;
			
			switch (SexSign) {
				case 'F':
					Result = TGEDCOMSex.svFemale;
					break;
				case 'M':
					Result = TGEDCOMSex.svMale;
					break;
				case 'U':
					Result = TGEDCOMSex.svUndetermined;
					break;
			}
			
			return Result;
		}

		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}

		public static bool IsRecordAccess(TGEDCOMRestriction restriction, TShieldState shieldState)
		{
			bool result = false;

			switch (shieldState) {
				case TShieldState.ssMaximum:
					result = (((restriction == TGEDCOMRestriction.rnConfidential || restriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case TShieldState.ssMiddle:
					result = (((restriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case TShieldState.ssNone:
					result = true;
					break;
			}

			return result;
		}

		public static string aux_GetFamilyStr(TGEDCOMFamilyRecord aFamily)
		{
			return aFamily.aux_GetFamilyStr(LangMan.LSList[64], LangMan.LSList[63]);
		}

		public static string SetAsName(string s)
		{
			string st = s.ToLower();
			char f = Char.ToUpper(st[0]);
			st = f + st.Substring(1);
			return st;
		}

		public static string ConStrings(StringList aStrings)
		{
			string Result = "";
			int num = aStrings.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (Result != "") Result += " ";
				Result += aStrings[i].Trim();
			}
			return Result;
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
			string Result = "~^" + XRef;
			if (Text != "")
			{
				Result = Result + ":" + Text;
			}
			Result += "~";
			return Result;
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
							sign = LangMan.LSList[(int)GKData.RecordTypes[(int)record.RecordType] - 1] + ": ";
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
			string Result = "";
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
				Result = "[" + LangMan.LSList[(int)GKData.CommunicationDirs[(int)dir] - 1] + "] " + nm;
			}
			return Result;
		}

		public static string GetTaskGoalStr(TGEDCOMTaskRecord taskRec)
		{
			TGoalType gt = TGoalType.gtOther;
			TGEDCOMRecord tempRec = null;
            taskRec.aux_GetTaskGoal(ref gt, ref tempRec);
			string Result = "";

			switch (gt) {
				case TGoalType.gtIndividual:
					Result = (tempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
					break;
				case TGoalType.gtFamily:
					Result = GKUtils.aux_GetFamilyStr(tempRec as TGEDCOMFamilyRecord);
					break;
				case TGoalType.gtSource:
					Result = (tempRec as TGEDCOMSourceRecord).FiledByEntry;
					break;
				case TGoalType.gtOther:
					Result = taskRec.Goal;
					break;
			}

			if (gt != TGoalType.gtOther)
			{
				Result = "[" + LangMan.LSList[(int)GKData.GoalNames[(int)gt] - 1] + "] " + Result;
			}
			return Result;
		}

		#endregion

		#region Event Utils
		
		public static TGEDCOMCustomEvent GetIndividualEvent(TGEDCOMIndividualRecord iRec, string evName)
		{
			return ((iRec == null) ? null : iRec.GetIndividualEvent(evName));
		}

		public static string GetAttributeValue(TGEDCOMIndividualRecord iRec, string attrName)
		{
			TGEDCOMCustomEvent attr = GKUtils.GetIndividualEvent(iRec, attrName);
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
						result = LangMan.LSList[(int)GKData.PersonEvents[ev].Name - 1];
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
						result = LangMan.LSList[(int)GKData.FamilyEvents[ev].Name - 1];
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
					st = LangMan.LSList[(int)GKData.PersonEvents[idx].Name - 1];
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
			string dt = GKUtils.GEDCOMCustomDateToStr(eventDetail.Date, TDateFormat.dfDD_MM_YYYY, false);
			string place = eventDetail.Place.StringValue;
			TGEDCOMLocationRecord location = eventDetail.Place.Location.Value as TGEDCOMLocationRecord;

			if (place != "" && location != null)
			{
				place = GKUtils.HyperLink(location.XRef, place, 0);
			}

			string Result;

			if (dt == "" && place == "")
			{
				Result = "?";
			}
			else
			{
				if (dt == "")
				{
					Result = place;
				}
				else
				{
					if (place == "")
					{
						Result = dt;
					}
					else
					{
						Result = dt + ", " + place;
					}
				}
			}

			return Result;
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

		#endregion

		#region Date functions

		public static string GEDCOMDateToStr(TGEDCOMDate aDate, TDateFormat aFormat)
		{
			string Result = "";
			int year;
			ushort month;
			ushort day;
			aDate.GetDate(out year, out month, out day);

			if (year > 0 || month > 0 || day > 0)
			{
				if (aFormat != TDateFormat.dfDD_MM_YYYY)
				{
					if (aFormat != TDateFormat.dfYYYY_MM_DD)
					{
						if (aFormat == TDateFormat.dfYYYY)
						{
							if (year > 0)
							{
								Result = year.ToString().PadLeft(4, '_');
							}
						}
					}
					else
					{
						if (year > 0)
						{
							Result = Result + year.ToString().PadLeft(4, '_') + ".";
						}
						else
						{
							Result += "____.";
						}
						if (month > 0)
						{
							Result = Result + SysUtils.NumUpdate((int)month, 2) + ".";
						}
						else
						{
							Result += "__.";
						}
						if (day > 0)
						{
							Result += SysUtils.NumUpdate((int)day, 2);
						}
						else
						{
							Result += "__";
						}
					}
				}
				else
				{
					if (day > 0)
					{
						Result = Result + SysUtils.NumUpdate((int)day, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (month > 0)
					{
						Result = Result + SysUtils.NumUpdate((int)month, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (year > 0)
					{
						Result += year.ToString().PadLeft(4, '_');
					}
					else
					{
						Result += "____";
					}
				}
			}
			return Result;
		}

		public static string StrToGEDCOMDate(string aDate, bool aException)
		{
			string Result = "";

			if (aDate.IndexOf("/") >= 0) aDate = aDate.Replace("/", ".");
			if (aDate.IndexOf("_") >= 0) aDate = aDate.Replace("_", " ");

			string[] dt_parts = aDate.Split('.');
			if (dt_parts.Length < 3)
			{
				if (aException)
				{
					throw new Exception("date failed");
				}
			}
			else
			{
				string pd = dt_parts[0].Trim();
				string pm = dt_parts[1].Trim();
				string py = dt_parts[2].Trim();

				if (pd != "") Result = Result + pd + " ";
				if (pm != "") Result = Result + TGEDCOMDate.GEDCOMMonthArray[SysUtils.ParseInt(pm, 1) - 1] + " ";
				if (py != "") Result += py;
			}
			return Result;
		}

		public static string GEDCOMCustomDateToStr(TGEDCOMDateValue dateValue, TDateFormat format, bool sign)
		{
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
					case TDateFormat.dfDD_MM_YYYY:
						result = result + " BC";
						break;
					case TDateFormat.dfYYYY_MM_DD:
						result = "BC " + result;
						break;
					case TDateFormat.dfYYYY:
						result = "BC " + result;
						break;
				}
			}

			return result;
		}

		public static string GEDCOMEventToDateStr(TGEDCOMCustomEvent aEvent, TDateFormat format, bool sign)
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

		public static string GetBirthDate(TGEDCOMIndividualRecord iRec, TDateFormat dateFormat, bool compact)
		{
			TGEDCOMCustomEvent evt = GKUtils.GetIndividualEvent(iRec, "BIRT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetDeathDate(TGEDCOMIndividualRecord iRec, TDateFormat dateFormat, bool compact)
		{
			TGEDCOMCustomEvent evt = GKUtils.GetIndividualEvent(iRec, "DEAT");
			string result = ((evt == null) ? "" : GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = GKUtils.CompactDate(result);
			return result;
		}

		public static string GetLifeStr(TGEDCOMIndividualRecord iRec)
		{
			string Result = " (";

			string ds = GKUtils.GetBirthDate(iRec, TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				ds = "?";
			}
			Result += ds;

			ds = GKUtils.GetDeathDate(iRec, TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				TGEDCOMCustomEvent ev = GKUtils.GetIndividualEvent(iRec, "DEAT");
				if (ev != null)
				{
					ds = "?";
				}
			}

			if (ds != "")
			{
				Result = Result + " - " + ds;
			}

			Result += ")";
			return Result;
		}

		// FIXME: aux_candidate
		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign)
		{
			bool dummy;
			return GetIndependentYear(iRec, evSign, out dummy);
		}

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign, out bool YearBC)
		{
			int Result = -1;
			YearBC = false;

			TGEDCOMCustomEvent ev = GKUtils.GetIndividualEvent(iRec, evSign);
			if (ev != null)
			{
				int year;
				ushort am, ad;
				ev.Detail.Date.aux_GetIndependentDate(out year, out am, out ad, out YearBC);
				Result = year;
			}
			return Result;
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
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.GetEventsYearsDiff(): " + E.Message);
			}

			return result;
		}

		public static string GetLifeExpectancy(TGEDCOMIndividualRecord iRec)
		{
			string result = "";

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
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.GetLifeExpectancy(): " + E.Message);
			}

			return result;
		}

		public static string GetAge(TGEDCOMIndividualRecord iRec, int ToYear)
		{
			string result = "";

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
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.GetAge(): " + E.Message);
			}

			return result;
		}

		public static string GetMarriageDate(TGEDCOMFamilyRecord fRec, TDateFormat dateFormat)
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
			string Result = "";
			try
			{
				TGEDCOMCustomEvent evt = GKUtils.GetIndividualEvent(iRec, "DEAT");
				if (evt != null)
				{
				}
				else
				{
					evt = GKUtils.GetIndividualEvent(iRec, "BIRT");
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
								Result = Convert.ToString(SysUtils.DaysBetween(new DateTime((int)cur_y, (int)cur_m, (int)cur_d), new DateTime(bd_y, (int)bd_m, (int)bd_d)));
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.GetDaysForBirth(): " + E.Message);
			}
			return Result;
		}

		#endregion

		#region Places functions

		public static string GetBirthPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent evt = GKUtils.GetIndividualEvent(iRec, "BIRT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetDeathPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent evt = GKUtils.GetIndividualEvent(iRec, "DEAT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetResidencePlace(TGEDCOMIndividualRecord iRec, bool includeAddress)
		{
			return GKUtils.GetPlaceStr(GKUtils.GetIndividualEvent(iRec, "RESI"), includeAddress);
		}

		public static string GetPlaceStr(TGEDCOMCustomEvent aEvent, bool includeAddress)
		{
			string Result;
			if (aEvent == null)
			{
				Result = "";
			}
			else
			{
				Result = aEvent.Detail.Place.StringValue;
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
						Result = Result + " [" + resi + "]";
					}
				}
			}
			return Result;
		}

		#endregion

		#region Match functions

		public static Regex InitMaskRegex(string Mask)
		{
			Regex result = null;

			if  (!string.IsNullOrEmpty(Mask))
			{
				string regex_str = "";
				int CurPos = 0;
				int Len = Mask.Length;
				if (CurPos < Len)
				{
					do
					{
						int I = Mask.IndexOfAny("*?".ToCharArray(), CurPos);
						if (I < CurPos) break;
						if (I > CurPos) {
							string part = Mask.Substring(CurPos, I - CurPos);
							regex_str += Regex.Escape(part);
						}

						char c = Mask[I];
						switch (c) {
							case '*':
								regex_str += ".*";
								break;
							case '?':
								regex_str += ".";
								break;
						}

						CurPos = I + 1;
					}
					while (CurPos < Len);
				}

				if (CurPos < Len) {
					string part = Mask.Substring(CurPos, Len - CurPos);
					regex_str += Regex.Escape(part);
				}

				result = new Regex(regex_str, RegexOptions.IgnoreCase);
			}

			return result;
		}

		public static bool MatchesRegex(string S, Regex regex)
		{
			return ((regex != null) ? regex.IsMatch(S) : false);
		}

		public static bool MatchesMask(string S, string Mask)
		{
			Regex regex = InitMaskRegex(Mask);
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

		public static string GetAppDataPath()
		{
			string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + "\\" + GKData.AppTitle + "\\";
			if (!Directory.Exists(path)) Directory.CreateDirectory(path);
			return path;
		}

		#endregion

		#region UI functions

		public static void ShowMessage(string Msg)
		{
			MessageBox.Show(Msg, GKData.AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
		}

		public static void ShowError(string Msg)
		{
			MessageBox.Show(Msg, GKData.AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Hand);
		}

		public static DialogResult ShowQuestion(string Msg)
		{
			return MessageBox.Show(Msg, GKData.AppTitle, MessageBoxButtons.YesNo, MessageBoxIcon.Question);
		}

		#endregion

	}
}
