using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{

	public enum TPersonColumnType : byte
	{
		pctPatriarch,
		pctName,
		pctNick,
		pctSex,
		pctBirthDate,
		pctDeathDate,
		pctBirthPlace,
		pctDeathPlace,
		pctResidence,
		pctAge,
		pctLifeExpectancy,
		pctDaysForBirth,
		pctGroups,
		pctReligion,
		pctNationality,
		pctEducation,
		pctOccupation,
		pctCaste,
		pctMili,
		pctMiliInd,
		pctMiliDis,
		pctMiliRank,
		pctChangeDate,
		pctBookmark,
		pctTitle
	}

	public sealed class TIndividualListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Patriarch), TDataType.dtString, 25, false);
			this.AddStatic(LangMan.LS(LSID.LSID_FullName), TDataType.dtString, 25, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Nickname), TDataType.dtString, 75, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Sex), TDataType.dtString, 45, false);
			this.AddStatic(LangMan.LS(LSID.LSID_BirthDate), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_DeathDate), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_BirthPlace), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_DeathPlace), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Residence), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Age), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_LifeExpectancy), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_DaysForBirth), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_RPGroups), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Religion), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Nationality), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Education), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Occupation), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Caste), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Mili), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_MiliInd), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_MiliDis), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_MiliRank), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Bookmark), TDataType.dtString, 25, true);
			this.AddStatic(LangMan.LS(LSID.LSID_NobilityTitle), TDataType.dtString, 200, true);
		}

		public TIndividualListColumns()
		{
			InitData(typeof(TPersonColumnType));
		}
	}

	public class TIndividualListFilter : TListFilter
	{
		public string AliveBeforeDate;
		public CustomFilter.TGroupMode GroupMode;
		public string GroupRef;
		public TLifeMode LifeMode;
		public string Name;
		public bool PatriarchOnly;
		public string Residence;
		public TGEDCOMSex Sex;
		public CustomFilter.TGroupMode SourceMode;
		public string SourceRef;
		public string EventVal;
		public bool ChildSelector;
		public int TimeLineYear;

		public TIndividualListFilter()
		{
			this.Clear();
		}
		
		public override void Clear()
		{
			base.Clear();

			this.GroupMode = CustomFilter.TGroupMode.gmAll;
			this.GroupRef = "";
			if (this.LifeMode != TLifeMode.lmTimeLine)
			{
				this.LifeMode = TLifeMode.lmAll;
				this.TimeLineYear = -1;
			}
			this.Name = "*";
			this.AliveBeforeDate = "";
			this.PatriarchOnly = false;
			this.Residence = "*";
			this.Sex = TGEDCOMSex.svNone;
			this.SourceMode = CustomFilter.TGroupMode.gmAll;
			this.SourceRef = "";
			this.EventVal = "*";
		}
	}

	public sealed class TIndividualListMan : TListManager
	{
		private TGEDCOMIndividualRecord FRec;
		private TGEDCOMGroupRecord filter_grp;
		private DateTime filter_abd;
		private TGEDCOMSourceRecord filter_source;
		private int FYearMin;
		private int FYearMax;
		private int age_year;

		public int YearMin
		{
			get { return this.FYearMin; }
		}

		public int YearMax
		{
			get { return this.FYearMax; }
		}

		protected override void CreateFilter()
		{
			this.FFilter = new TIndividualListFilter();
		}

		private string GetGroups()
		{
			string result = "";

			int count = this.FRec.Groups.Count;
			for (int idx = 0; idx < count; idx++)
			{
				TGEDCOMGroupRecord grp = this.FRec.Groups[idx].Value as TGEDCOMGroupRecord;
				if (grp != null)
				{
					result += grp.GroupName;
					if (idx < count - 1) result += "; ";
				}
			}

			return result;
		}

		private bool HasPlace()
		{
			bool res = false;

			TIndividualListFilter iFilter = FFilter as TIndividualListFilter;
			bool addr = GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string place = GKUtils.GetPlaceStr(this.FRec.IndividualEvents[i], addr);
				res = IsMatchesMask(place, iFilter.Residence);
				if (res) break;
			}

			return res;
		}

		private bool HasEventVal()
		{
			bool result = false;
			
			TIndividualListFilter iFilter = FFilter as TIndividualListFilter;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				result = IsMatchesMask(this.FRec.IndividualEvents[i].StringValue, iFilter.EventVal);
				if (result) break;
			}

			return result;
		}

		private bool CheckSpecificFilter(TShieldState aShieldState)
		{
			bool result = false;

			TIndividualListFilter iFilter = FFilter as TIndividualListFilter;

			TGEDCOMCustomEvent bd_ev = null;
			TGEDCOMCustomEvent dd_ev = null;

			int num = this.FRec.IndividualEvents.Count - 1;
			ushort j = 0;
			ushort d = 0;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = this.FRec.IndividualEvents[i];

				if (ev.Name == "BIRT" && bd_ev == null)
				{
					bd_ev = ev;
					int y = -1;
					ev.Detail.Date.aux_GetIndependentDate(out y, out j, out d);
					if (y > 0)
					{
						if (this.FYearMin > y) this.FYearMin = y;
						if (this.FYearMax < y) this.FYearMax = y;
					}
				}
				else if (ev.Name == "DEAT" && dd_ev == null)
				{
					dd_ev = ev;
				}
			}

			string fullname = this.FRec.aux_GetNameStr(true, false);

			if ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TShieldState.ssNone)
			    && (iFilter.Sex == TGEDCOMSex.svNone || this.FRec.Sex == iFilter.Sex)
			    && (iFilter.Name == "*" || IsMatchesMask(fullname, iFilter.Name))
			    && (this.QuickFilter == "*" || IsMatchesMask(fullname, this.QuickFilter))
				&& (iFilter.Residence == "*" || this.HasPlace())
				&& (iFilter.EventVal == "*" || this.HasEventVal())
				&& (!iFilter.PatriarchOnly || this.FRec.Patriarch))
			{
				bool isLive = (dd_ev == null);

				switch (iFilter.LifeMode) {
					case TLifeMode.lmOnlyAlive:
						if (!isLive) return result;
						break;

					case TLifeMode.lmOnlyDead:
						if (isLive) return result;
						break;

					case TLifeMode.lmAliveBefore:
						DateTime bdt = ((bd_ev == null) ? new DateTime(0) : GKUtils.GEDCOMDateToDate(bd_ev.Detail.Date));
						DateTime ddt = ((dd_ev == null) ? new DateTime(0) : GKUtils.GEDCOMDateToDate(dd_ev.Detail.Date));
						if ((bdt > this.filter_abd) || (ddt < this.filter_abd)) return result;
						break;

					case TLifeMode.lmTimeLine:
						int bdy = -1;
						if (bd_ev != null) bd_ev.Detail.Date.aux_GetIndependentDate(out bdy, out j, out d);

						int ddy = -1;
						if (dd_ev == null) dd_ev.Detail.Date.aux_GetIndependentDate(out ddy, out j, out d);

						if (this.age_year > 0 && (bdy <= 0 || bdy >= this.age_year || (ddy > 0 && (ddy <= 0 || ddy <= this.age_year))))
						{
							return result;
						}
						break;
				}

				switch (iFilter.GroupMode) {
					case CustomFilter.TGroupMode.gmAll:
						break;
					case CustomFilter.TGroupMode.gmNone:
						if (this.FRec.Groups.Count != 0) return result;
						break;
					case CustomFilter.TGroupMode.gmAny:
						if (this.FRec.Groups.Count == 0) return result;
						break;
					case CustomFilter.TGroupMode.gmSelected:
						if (this.FRec.IndexOfGroup(this.filter_grp) < 0) return result;
						break;
				}

				switch (iFilter.SourceMode) {
					case CustomFilter.TGroupMode.gmAll:
						break;
					case CustomFilter.TGroupMode.gmNone:
						if (this.FRec.SourceCitations.Count != 0) return result;
						break;
					case CustomFilter.TGroupMode.gmAny:
						if (this.FRec.SourceCitations.Count == 0) return result;
						break;
					case CustomFilter.TGroupMode.gmSelected:
						if (this.FRec.IndexOfSource(this.filter_source) < 0) return result;
						break;
				}

				if (!iFilter.ChildSelector || this.FRec.ChildToFamilyLinks.Count == 0)
				{
					result = true;
				}
			}

			return result;
		}

		public override bool CheckFilter(TShieldState aShieldState)
		{
			string fullname = this.FRec.aux_GetNameStr(true, false);
			bool res = (this.QuickFilter == "*" || IsMatchesMask(fullname, this.QuickFilter));

			res = res && base.CheckNewFilter() && this.CheckSpecificFilter(aShieldState);

			return res;
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			TPersonColumnType pct = (TPersonColumnType)col_type;

			object Result = null;

			switch (pct) {
				case TPersonColumnType.pctPatriarch:
					Result = ((this.FRec.Patriarch) ? "*" : " ");
					break;

				case TPersonColumnType.pctName:
				{
					if (col_subtype == -1) {
						Result = this.FRec.aux_GetNameStr(true, false);
					} else {
						TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
						string f, i, p;

						switch (defNameFormat) {
							case TNameFormat.nfFNP:
								Result = this.FRec.aux_GetNameStr(true, false);
								break;

							case TNameFormat.nfF_NP:
								this.FRec.aux_GetNameParts(out f, out i, out p);
								switch (col_subtype) {
									case 0:
										Result = f;
										break;
									case 1:
										Result = i + " " + p;
										break;
								}
								break;

							case TNameFormat.nfF_N_P:
								this.FRec.aux_GetNameParts(out f, out i, out p);
								switch (col_subtype) {
									case 0:
										Result = f;
										break;
									case 1:
										Result = i;
										break;
									case 2:
										Result = p;
										break;
								}
								break;
						}
					}
					
					break;
				}

				case TPersonColumnType.pctNick:
					Result = this.FRec.aux_GetNickStr();
					break;

				case TPersonColumnType.pctSex:
					Result = new string(GKUtils.SexStr(this.FRec.Sex)[0], 1);
					break;

				case TPersonColumnType.pctBirthDate:
					Result = GKUtils.GEDCOMEventToDateStr(buf_bd, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					break;

				case TPersonColumnType.pctBirthPlace:
					Result = GKUtils.GetPlaceStr(buf_bd, false);
					break;

				case TPersonColumnType.pctDeathDate:
					Result = GKUtils.GEDCOMEventToDateStr(buf_dd, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					break;

				case TPersonColumnType.pctDeathPlace:
					Result = GKUtils.GetPlaceStr(buf_dd, false);
					break;

				case TPersonColumnType.pctResidence:
					Result = buf_residence;
					break;

				case TPersonColumnType.pctAge:
					Result = GKUtils.GetAge(this.FRec, -1);
					break;

				case TPersonColumnType.pctLifeExpectancy:
					Result = GKUtils.GetLifeExpectancy(this.FRec);
					break;

				case TPersonColumnType.pctDaysForBirth:
					Result = GKUtils.GetDaysForBirth(this.FRec);
					break;

				case TPersonColumnType.pctGroups:
					Result = this.GetGroups();
					break;

				case TPersonColumnType.pctReligion:
					Result = buf_religion;
					break;

				case TPersonColumnType.pctNationality:
					Result = buf_nationality;
					break;

				case TPersonColumnType.pctEducation:
					Result = buf_education;
					break;

				case TPersonColumnType.pctOccupation:
					Result = buf_occupation;
					break;

				case TPersonColumnType.pctCaste:
					Result = buf_caste;
					break;

				case TPersonColumnType.pctMili:
					Result = buf_mili;
					break;

				case TPersonColumnType.pctMiliInd:
					Result = buf_mili_ind;
					break;

				case TPersonColumnType.pctMiliDis:
					Result = buf_mili_dis;
					break;

				case TPersonColumnType.pctMiliRank:
					Result = buf_mili_rank;
					break;

				case TPersonColumnType.pctChangeDate:
					Result = this.FRec.ChangeDate.ChangeDateTime;
					break;

				case TPersonColumnType.pctBookmark:
					Result = ((this.FRec.Bookmark) ? "*" : " ");
					break;

				case TPersonColumnType.pctTitle:
					Result = buf_title;
					break;
			}
			return Result;
		}

		public override void InitFilter()
		{
			TIndividualListFilter iFilter = (TIndividualListFilter)FFilter;
			
			if (!DateTime.TryParse(iFilter.AliveBeforeDate, out this.filter_abd)) {
				this.filter_abd = new DateTime(0);
			}

			if (iFilter.LifeMode != TLifeMode.lmTimeLine) {
				this.age_year = -1;
			} else {
				this.age_year = iFilter.TimeLineYear;
			}

			if (iFilter.GroupRef == "") {
				this.filter_grp = null;
			} else {
				this.filter_grp = this.FTree.XRefIndex_Find(iFilter.GroupRef) as TGEDCOMGroupRecord;
			}

			if (iFilter.SourceRef == "") {
				this.filter_source = null;
			} else {
				this.filter_source = this.FTree.XRefIndex_Find(iFilter.SourceRef) as TGEDCOMSourceRecord;
			}

			this.FYearMin = 10000;
			this.FYearMax = 0;
		}

		private TGEDCOMCustomEvent buf_bd = null;
		private TGEDCOMCustomEvent buf_dd = null;
		private string buf_residence = "";
		private string buf_religion = "";
		private string buf_nationality = "";
		private string buf_education = "";
		private string buf_occupation = "";
		private string buf_caste = "";
		private string buf_mili = "";
		private string buf_mili_ind = "";
		private string buf_mili_dis = "";
		private string buf_mili_rank = "";
		private string buf_title = "";

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMIndividualRecord);

			buf_bd = null;
			buf_dd = null;
			buf_residence = "";
			buf_religion = "";
			buf_nationality = "";
			buf_education = "";
			buf_occupation = "";
			buf_caste = "";
			buf_mili = "";
			buf_mili_ind = "";
			buf_mili_dis = "";
			buf_mili_rank = "";
			buf_title = "";

			GlobalOptions gOptions = GKUI.TfmGEDKeeper.Instance.Options;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = this.FRec.IndividualEvents[i];

				if (ev.Name == "BIRT" && buf_bd == null)
				{
					buf_bd = ev;
				}
				else if (ev.Name == "DEAT" && buf_dd == null)
				{
					buf_dd = ev;
				}
				else if (ev.Name == "RESI" && buf_residence == "")
				{
					buf_residence = GKUtils.GetPlaceStr(ev, gOptions.PlacesWithAddress);
				}
				else if (ev.Name == "RELI" && buf_religion == "")
				{
					buf_religion = ev.StringValue;
				}
				else if (ev.Name == "NATI" && buf_nationality == "")
				{
					buf_nationality = ev.StringValue;
				}
				else if (ev.Name == "EDUC" && buf_education == "")
				{
					buf_education = ev.StringValue;
				}
				else if (ev.Name == "OCCU" && buf_occupation == "")
				{
					buf_occupation = ev.StringValue;
				}
				else if (ev.Name == "CAST" && buf_caste == "")
				{
					buf_caste = ev.StringValue;
				}
				else if (ev.Name == "_MILI" && buf_mili == "")
				{
					buf_mili = ev.StringValue;
				}
				else if (ev.Name == "_MILI_IND" && buf_mili_ind == "")
				{
					buf_mili_ind = ev.StringValue;
				}
				else if (ev.Name == "_MILI_DIS" && buf_mili_dis == "")
				{
					buf_mili_dis = ev.StringValue;
				}
				else if (ev.Name == "_MILI_RANK" && buf_mili_rank == "")
				{
					buf_mili_rank = ev.StringValue;
				}
				else if (ev.Name == "TITL" && buf_title == "")
				{
					buf_title = ev.StringValue;
				}
			}
		}

		public override void UpdateItem(GKListItem item, bool isMain)
		{
			base.UpdateItem(item, isMain);

			GlobalOptions gOptions = GKUI.TfmGEDKeeper.Instance.Options;

			if ((FRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnparented))
			{
				item.BackColor = System.Drawing.Color.FromArgb(0xFFCACA);
			}
			else if ((FRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnmarried))
			{
				item.BackColor = System.Drawing.Color.FromArgb(0xFFFFCA);
			}
		}

		protected override void UpdateColumns(GKListView aList, bool isMain)
		{
			TIndividualListColumns columns = GKUI.TfmGEDKeeper.Instance.Options.IndividualListColumns;
			TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;

			this.ColumnsMap_Clear();
			this.AddListColumn(aList, "№", 50, false, 0, 0);

			for (int i = 0; i < columns.Count; i++)
			{
				if (columns[i].colActive)
				{
					byte bColType = columns[i].colType;
					TPersonColumnType col_type = (TPersonColumnType)bColType;

					if (col_type == TPersonColumnType.pctName) {
						bool asz = false;
						switch (defNameFormat) {
							case TNameFormat.nfF_N_P:
								this.AddListColumn(aList, LangMan.LSList[84], 150, asz, bColType, 0);
								this.AddListColumn(aList, LangMan.LSList[85], 100, asz, bColType, 1);
								this.AddListColumn(aList, LangMan.LSList[86], 150, asz, bColType, 2);
								break;

							case TNameFormat.nfF_NP:
								this.AddListColumn(aList, LangMan.LSList[84], 150, asz, bColType, 0);
								this.AddListColumn(aList, LangMan.LSList[85] + "," + LangMan.LSList[86], 150, asz, bColType, 1);
								break;

							case TNameFormat.nfFNP:
								this.AddListColumn(aList, LangMan.LSList[301], 300, asz, bColType, 0);
								break;
						}
					} else {
						this.AddListColumn(aList, LangMan.LSList[(int)GlobalOptions.PersonColumnsName[bColType].Name - 1], 
						                   GlobalOptions.PersonColumnsName[bColType].DefWidth, false, bColType, 0);
					}
				}
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TPersonColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TIndividualListColumns();
		}

		public TIndividualListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
