using System;
using System.Collections.Generic;

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
		protected override void InitDefaultColumns()
		{
			TColumnProps[] array1 = new TColumnProps[25];
			array1[0] = new TColumnProps(TPersonColumnType.pctPatriarch, true);
			array1[1] = new TColumnProps(TPersonColumnType.pctName, true);
			array1[2] = new TColumnProps(TPersonColumnType.pctNick, false);
			array1[3] = new TColumnProps(TPersonColumnType.pctSex, true);
			array1[4] = new TColumnProps(TPersonColumnType.pctBirthDate, true);
			array1[5] = new TColumnProps(TPersonColumnType.pctDeathDate, true);
			array1[6] = new TColumnProps(TPersonColumnType.pctBirthPlace, true);
			array1[7] = new TColumnProps(TPersonColumnType.pctDeathPlace, true);
			array1[8] = new TColumnProps(TPersonColumnType.pctResidence, true);
			array1[9] = new TColumnProps(TPersonColumnType.pctAge, true);
			array1[10] = new TColumnProps(TPersonColumnType.pctLifeExpectancy, true);
			array1[11] = new TColumnProps(TPersonColumnType.pctDaysForBirth, true);
			array1[12] = new TColumnProps(TPersonColumnType.pctGroups, true);
			array1[13] = new TColumnProps(TPersonColumnType.pctReligion, false);
			array1[14] = new TColumnProps(TPersonColumnType.pctNationality, false);
			array1[15] = new TColumnProps(TPersonColumnType.pctEducation, false);
			array1[16] = new TColumnProps(TPersonColumnType.pctOccupation, false);
			array1[17] = new TColumnProps(TPersonColumnType.pctCaste, false);
			array1[18] = new TColumnProps(TPersonColumnType.pctMili, false);
			array1[19] = new TColumnProps(TPersonColumnType.pctMiliInd, false);
			array1[20] = new TColumnProps(TPersonColumnType.pctMiliDis, false);
			array1[21] = new TColumnProps(TPersonColumnType.pctMiliRank, false);
			array1[22] = new TColumnProps(TPersonColumnType.pctChangeDate, true);
			array1[23] = new TColumnProps(TPersonColumnType.pctBookmark, true);
			array1[24] = new TColumnProps(TPersonColumnType.pctTitle, true);
			DefColumns = array1;
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
		public TGenEngine.TLifeMode LifeMode;
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
			if (this.LifeMode != TGenEngine.TLifeMode.lmTimeLine)
			{
				this.LifeMode = TGenEngine.TLifeMode.lmAll;
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
		private struct TColRec
		{
			public byte col_type;
			public byte col_subtype;
		}

		private TGEDCOMIndividualRecord FRec;
		private TGEDCOMGroupRecord filter_grp;
		private DateTime filter_abd;
		private TGEDCOMSourceRecord filter_source;
		private int FYearMin;
		private int FYearMax;
		private int age_year;
		private List<TColRec> FColumnsMap = new List<TColRec>();

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

			TIndividualListFilter iFilter = (TIndividualListFilter)FFilter;
			bool addr = GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string place = TGenEngine.GetPlaceStr(this.FRec.IndividualEvents[i], addr);
				res = IsMatchesMask(place, iFilter.Residence);
				if (res) break;
			}

			return res;
		}

		private bool HasEventVal()
		{
			bool result = false;
			
			TIndividualListFilter iFilter = (TIndividualListFilter)FFilter;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				result = IsMatchesMask(this.FRec.IndividualEvents[i].StringValue, iFilter.EventVal);
				if (result) break;
			}

			return result;
		}

		private bool CheckSpecificFilter(TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			
			TIndividualListFilter iFilter = (TIndividualListFilter)FFilter;
			
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
				else
				{
					if (ev.Name == "DEAT" && dd_ev == null)
					{
						dd_ev = ev;
					}
				}
			}

			string fullname = this.FRec.aux_GetNameStr(true, false);

			if ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone)
			    && (iFilter.Sex == TGEDCOMSex.svNone || this.FRec.Sex == iFilter.Sex)
			    && (iFilter.Name == "*" || IsMatchesMask(fullname, iFilter.Name))
			    && (this.QuickFilter == "*" || IsMatchesMask(fullname, this.QuickFilter))
				&& (iFilter.Residence == "*" || this.HasPlace())
				&& (iFilter.EventVal == "*" || this.HasEventVal())
				&& (!iFilter.PatriarchOnly || this.FRec.Patriarch))
			{
				bool isLive = (dd_ev == null);

				switch (iFilter.LifeMode) {
					case TGenEngine.TLifeMode.lmOnlyAlive:
						{
							if (!isLive) return Result;
							break;
						}
					case TGenEngine.TLifeMode.lmOnlyDead:
						{
							if (isLive) return Result;
							break;
						}
					case TGenEngine.TLifeMode.lmAliveBefore:
						{
							DateTime bdt = ((bd_ev == null) ? new DateTime(0) : TGenEngine.GEDCOMDateToDate(bd_ev.Detail.Date));
							DateTime ddt = ((dd_ev == null) ? new DateTime(0) : TGenEngine.GEDCOMDateToDate(dd_ev.Detail.Date));

							if ((bdt > this.filter_abd) || (ddt < this.filter_abd)) return Result;

							break;
						}
					case TGenEngine.TLifeMode.lmTimeLine:
						{
							int bdy = -1;
							if (bd_ev != null) bd_ev.Detail.Date.aux_GetIndependentDate(out bdy, out j, out d);

							int ddy = -1;
							if (dd_ev != null) dd_ev.Detail.Date.aux_GetIndependentDate(out ddy, out j, out d);

							if (this.age_year > 0 && (bdy <= 0 || bdy >= this.age_year || (ddy > 0 && (ddy <= 0 || ddy <= this.age_year))))
							{
								return Result;
							}

							break;
						}
				}

				switch (iFilter.GroupMode) {
					case CustomFilter.TGroupMode.gmAll:
						break;
					case CustomFilter.TGroupMode.gmNone:
						if (this.FRec.Groups.Count != 0) return Result;
						break;
					case CustomFilter.TGroupMode.gmAny:
						if (this.FRec.Groups.Count == 0) return Result;
						break;
					case CustomFilter.TGroupMode.gmSelected:
						if (this.FRec.IndexOfGroup(this.filter_grp) < 0) return Result;
						break;
				}

				switch (iFilter.SourceMode) {
					case CustomFilter.TGroupMode.gmAll:
						break;
					case CustomFilter.TGroupMode.gmNone:
						if (this.FRec.SourceCitations.Count != 0) return Result;
						break;
					case CustomFilter.TGroupMode.gmAny:
						if (this.FRec.SourceCitations.Count == 0) return Result;
						break;
					case CustomFilter.TGroupMode.gmSelected:
						if (this.FRec.IndexOfSource(this.filter_source) < 0) return Result;
						break;
				}

				if (!iFilter.ChildSelector || this.FRec.ChildToFamilyLinks.Count == 0)
				{
					Result = true;
				}
			}

			return Result;
		}
		
		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			string fullname = this.FRec.aux_GetNameStr(true, false);
			bool res = (this.QuickFilter == "*" || IsMatchesMask(fullname, this.QuickFilter));

			res = res && base.CheckNewFilter() && this.CheckSpecificFilter(aShieldState);

			return res;
		}

		private void SetColMap(byte aType, byte aSubType)
		{
			TColRec cr = new TColRec();
			cr.col_type = aType;
			cr.col_subtype = aSubType;
			FColumnsMap.Add(cr);
		}

		public override object GetColumnValueEx(int col_index)
		{
			TColRec colrec = this.FColumnsMap[col_index];
			//TPersonColumnType pct = (TPersonColumnType)colrec.col_type;

			return GetColumnValueDirect(colrec.col_type, (int)colrec.col_subtype);
		}

		public override object GetColumnValueDirect(int col_type, int col_subtype)
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
						TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
						string f, i, p;

						switch (defNameFormat) {
							case TGenEngine.TNameFormat.nfFNP:
								Result = this.FRec.aux_GetNameStr(true, false);
								break;

							case TGenEngine.TNameFormat.nfF_NP:
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

							case TGenEngine.TNameFormat.nfF_N_P:
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
					Result = new string(TGenEngine.SexStr(this.FRec.Sex)[0], 1);
					break;

				case TPersonColumnType.pctBirthDate:
					Result = TGenEngine.GEDCOMEventToDateStr(buf_bd, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					break;

				case TPersonColumnType.pctBirthPlace:
					Result = TGenEngine.GetPlaceStr(buf_bd, false);
					break;

				case TPersonColumnType.pctDeathDate:
					Result = TGenEngine.GEDCOMEventToDateStr(buf_dd, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					break;

				case TPersonColumnType.pctDeathPlace:
					Result = TGenEngine.GetPlaceStr(buf_dd, false);
					break;

				case TPersonColumnType.pctResidence:
					Result = buf_residence;
					break;

				case TPersonColumnType.pctAge:
					Result = TGenEngine.GetAge(this.FRec, -1);
					break;

				case TPersonColumnType.pctLifeExpectancy:
					Result = TGenEngine.GetLifeExpectancy(this.FRec);
					break;

				case TPersonColumnType.pctDaysForBirth:
					Result = TGenEngine.GetDaysForBirth(this.FRec);
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

			if (iFilter.LifeMode != TGenEngine.TLifeMode.lmTimeLine) {
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
					buf_residence = TGenEngine.GetPlaceStr(ev, gOptions.PlacesWithAddress);
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

		public override void UpdateItem(GKListItem aItem, bool isMain)
		{
			GlobalOptions gOptions = GKUI.TfmGEDKeeper.Instance.Options;

			for (int i = 1; i < FColumnsMap.Count; i++)
			{
				aItem.SubItems.Add(this.GetColumnValue(i, isMain));
			}

			if ((FRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnparented))
			{
				aItem.BackColor = System.Drawing.Color.FromArgb(0xFFCACA);
			}
			else
			{
				if ((FRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnmarried))
				{
					aItem.BackColor = System.Drawing.Color.FromArgb(0xFFFFCA);
				}
			}
		}

		public override void UpdateColumns(GKListView aList, bool isMain)
		{
			TIndividualListColumns columns = GKUI.TfmGEDKeeper.Instance.Options.IndividualListColumns;

			FColumnsMap.Clear();

			aList.AddListColumn("№", 50, false);
			this.SetColMap(0, 0);

			for (int i = 0; i < columns.Count; i++)
			{
				if (columns[i].colActive)
				{
					TPersonColumnType col_type = (TPersonColumnType)columns[i].colType;

					if (col_type == TPersonColumnType.pctName) {
						bool asz = false;
						TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
						switch (defNameFormat) {
							case TGenEngine.TNameFormat.nfF_N_P:
								{
									aList.AddListColumn(LangMan.LSList[84], 150, asz);
									this.SetColMap((byte)col_type, 0);
									aList.AddListColumn(LangMan.LSList[85], 100, asz);
									this.SetColMap((byte)col_type, 1);
									aList.AddListColumn(LangMan.LSList[86], 150, asz);
									this.SetColMap((byte)col_type, 2);
									break;
								}
							case TGenEngine.TNameFormat.nfF_NP:
								{
									aList.AddListColumn(LangMan.LSList[84], 150, asz);
									this.SetColMap((byte)col_type, 0);
									aList.AddListColumn(LangMan.LSList[85] + "," + LangMan.LSList[86], 150, asz);
									this.SetColMap((byte)col_type, 1);
									break;
								}
							case TGenEngine.TNameFormat.nfFNP:
								{
									aList.AddListColumn(LangMan.LSList[301], 300, asz);
									this.SetColMap((byte)col_type, 0);
									break;
								}
						}
					} else {
						aList.AddListColumn(LangMan.LSList[(int)GlobalOptions.PersonColumnsName[(int)col_type].Name - 1], GlobalOptions.PersonColumnsName[(int)col_type].DefWidth, false);
						this.SetColMap((byte)col_type, 0);
					}
				}
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			for (int i = 0; i < GlobalOptions.PersonColumnsName.Length; i++) {
				GlobalOptions.TColumnRec colrec = GlobalOptions.PersonColumnsName[i];

				TDataType dataType = TDataType.dtString;
				if (i == (int)TPersonColumnType.pctChangeDate) dataType = TDataType.dtDateTime;

				this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(colrec.Name), dataType, colrec.DefWidth));
			}
			/*this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctPatriarch), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctName), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctNick), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctSex), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctBirthDate), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctDeathDate), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctBirthPlace), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctDeathPlace), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctResidence), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctAge), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctLifeExpectancy), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctDaysForBirth), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctGroups), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctReligion), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctNationality), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctEducation), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctOccupation), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctCaste), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctMili), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctMiliInd), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctMiliDis), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctMiliRank), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctChangeDate), TDataType.dtDateTime)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctBookmark), TDataType.dtString)); // 
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LS(pctTitle), TDataType.dtString)); // */
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
