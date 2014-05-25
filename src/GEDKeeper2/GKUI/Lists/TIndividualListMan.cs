using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
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
			this.AddStatic(LangMan.LS(LSID.LSID_Patriarch), TDataType.dtString, 25, true);
			this.AddStatic(LangMan.LS(LSID.LSID_FullName), TDataType.dtString, 25, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Nickname), TDataType.dtString, 75, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Sex), TDataType.dtString, 45, true);
			this.AddStatic(LangMan.LS(LSID.LSID_BirthDate), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_DeathDate), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_BirthPlace), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_DeathPlace), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Residence), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Age), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_LifeExpectancy), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_DaysForBirth), TDataType.dtString, 100, false);
			this.AddStatic(LangMan.LS(LSID.LSID_RPGroups), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Religion), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Nationality), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Education), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Occupation), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Caste), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Mili), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_MiliInd), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_MiliDis), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_MiliRank), TDataType.dtString, 200, false);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Bookmark), TDataType.dtString, 25, true);
			this.AddStatic(LangMan.LS(LSID.LSID_NobilityTitle), TDataType.dtString, 200, false);
		}

		public TIndividualListColumns()
		{
			InitData(typeof(TPersonColumnType));
		}
	}

	public class TIndividualListFilter : TListFilter
	{
		public string AliveBeforeDate;
		public TGroupMode GroupMode;
		public string GroupRef;
		public TLifeMode LifeMode;
		public string Name;
		public bool PatriarchOnly;
		public string Residence;
		public TGEDCOMSex Sex;
		public TGroupMode SourceMode;
		public string SourceRef;
		public string EventVal;

		public TIndividualListFilter()
		{
			this.Clear();
		}
		
		public override void Clear()
		{
			base.Clear();

			this.GroupMode = TGroupMode.gmAll;
			this.GroupRef = "";
			if (this.LifeMode != TLifeMode.lmTimeLocked)
			{
				this.LifeMode = TLifeMode.lmAll;
			}
			this.Name = "*";
			this.AliveBeforeDate = "";
			this.PatriarchOnly = false;
			this.Residence = "*";
			this.Sex = TGEDCOMSex.svNone;
			this.SourceMode = TGroupMode.gmAll;
			this.SourceRef = "";
			this.EventVal = "*";
		}
	}

	public sealed class TIndividualListMan : TListManager
	{
		private TGEDCOMIndividualRecord fRec;
		private TGEDCOMGroupRecord filter_grp;
		private DateTime filter_abd;
		private TGEDCOMSourceRecord filter_source;

		protected override void CreateFilter()
		{
			this.fFilter = new TIndividualListFilter();
		}

		private string GetGroups()
		{
			string result = "";

			int count = this.fRec.Groups.Count;
			for (int idx = 0; idx < count; idx++)
			{
				TGEDCOMGroupRecord grp = this.fRec.Groups[idx].Value as TGEDCOMGroupRecord;
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

			TIndividualListFilter iFilter = fFilter as TIndividualListFilter;
			bool addr = TfmGEDKeeper.Instance.Options.PlacesWithAddress;
			int num = this.fRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string place = GKUtils.GetPlaceStr(this.fRec.IndividualEvents[i], addr);
				res = IsMatchesMask(place, iFilter.Residence);
				if (res) break;
			}

			return res;
		}

		private bool HasEventVal()
		{
			bool result = false;
			
			TIndividualListFilter iFilter = fFilter as TIndividualListFilter;
			int num = this.fRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				result = IsMatchesMask(this.fRec.IndividualEvents[i].StringValue, iFilter.EventVal);
				if (result) break;
			}

			return result;
		}

		private bool CheckSpecificFilter(ShieldState aShieldState)
		{
			bool result = false;

			TIndividualListFilter iFilter = fFilter as TIndividualListFilter;

			string fullname = this.fRec.aux_GetNameStr(true, false);

			if ((this.fRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == ShieldState.ssNone)
			    && (iFilter.Sex == TGEDCOMSex.svNone || this.fRec.Sex == iFilter.Sex)
			    && (iFilter.Name == "*" || IsMatchesMask(fullname, iFilter.Name))
			    && (this.QuickFilter == "*" || IsMatchesMask(fullname, this.QuickFilter))
				&& (iFilter.Residence == "*" || this.HasPlace())
				&& (iFilter.EventVal == "*" || this.HasEventVal())
				&& (!iFilter.PatriarchOnly || this.fRec.Patriarch))
			{
				bool isLive = (buf_dd == null);

				switch (iFilter.LifeMode) {
					case TLifeMode.lmOnlyAlive:
						if (!isLive) return result;
						break;

					case TLifeMode.lmOnlyDead:
						if (isLive) return result;
						break;

					case TLifeMode.lmAliveBefore:
                        DateTime bdt = ((buf_bd == null) ? new DateTime(0) : GKUtils.GEDCOMDateToDate(buf_bd.Detail.Date));
                        DateTime ddt = ((buf_dd == null) ? new DateTime(0) : GKUtils.GEDCOMDateToDate(buf_dd.Detail.Date));
						if ((bdt > this.filter_abd) || (ddt < this.filter_abd)) return result;
						break;

					case TLifeMode.lmTimeLocked:
						break;
				}

				switch (iFilter.GroupMode) {
					case TGroupMode.gmAll:
						break;
					case TGroupMode.gmNone:
						if (this.fRec.Groups.Count != 0) return result;
						break;
					case TGroupMode.gmAny:
						if (this.fRec.Groups.Count == 0) return result;
						break;
					case TGroupMode.gmSelected:
						if (this.fRec.IndexOfGroup(this.filter_grp) < 0) return result;
						break;
				}

				switch (iFilter.SourceMode) {
					case TGroupMode.gmAll:
						break;
					case TGroupMode.gmNone:
						if (this.fRec.SourceCitations.Count != 0) return result;
						break;
					case TGroupMode.gmAny:
						if (this.fRec.SourceCitations.Count == 0) return result;
						break;
					case TGroupMode.gmSelected:
						if (this.fRec.IndexOfSource(this.filter_source) < 0) return result;
						break;
				}

				result = true;
			}

			return result;
		}

		public override bool CheckFilter(ShieldState aShieldState)
		{
			string fullname = this.fRec.aux_GetNameStr(true, false);
			bool res = (this.QuickFilter == "*" || IsMatchesMask(fullname, this.QuickFilter));

			res = res && base.CheckNewFilter() && this.CheckSpecificFilter(aShieldState);

            if (this.fExternalFilter != null) {
                res = res && this.fExternalFilter(this.fRec);
            }

			return res;
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			TPersonColumnType pct = (TPersonColumnType)colType;

			object result = null;

			switch (pct) {
				case TPersonColumnType.pctPatriarch:
					result = ((this.fRec.Patriarch) ? "*" : " ");
					break;

				case TPersonColumnType.pctName:
				{
					if (colSubtype == -1) {
						result = this.fRec.aux_GetNameStr(true, false);
					} else {
						NameFormat defNameFormat = TfmGEDKeeper.Instance.Options.DefNameFormat;
						string f, i, p;

						switch (defNameFormat) {
							case NameFormat.nfFNP:
								result = this.fRec.aux_GetNameStr(true, false);
								break;

							case NameFormat.nfF_NP:
								this.fRec.aux_GetNameParts(out f, out i, out p);
								switch (colSubtype) {
									case 0:
										result = f;
										break;
									case 1:
										result = i + " " + p;
										break;
								}
								break;

							case NameFormat.nfF_N_P:
								this.fRec.aux_GetNameParts(out f, out i, out p);
								switch (colSubtype) {
									case 0:
										result = f;
										break;
									case 1:
										result = i;
										break;
									case 2:
										result = p;
										break;
								}
								break;
						}
					}
					
					break;
				}

				case TPersonColumnType.pctNick:
					result = this.fRec.aux_GetNickStr();
					break;

				case TPersonColumnType.pctSex:
					result = new string(GKUtils.SexStr(this.fRec.Sex)[0], 1);
					break;

				case TPersonColumnType.pctBirthDate:
					result = GKUtils.GEDCOMEventToDateStr(buf_bd, TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					break;

				case TPersonColumnType.pctBirthPlace:
					result = GKUtils.GetPlaceStr(buf_bd, false);
					break;

				case TPersonColumnType.pctDeathDate:
					result = GKUtils.GEDCOMEventToDateStr(buf_dd, TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					break;

				case TPersonColumnType.pctDeathPlace:
					result = GKUtils.GetPlaceStr(buf_dd, false);
					break;

				case TPersonColumnType.pctResidence:
					result = buf_residence;
					break;

				case TPersonColumnType.pctAge:
					result = GKUtils.GetAge(this.fRec, -1);
					break;

				case TPersonColumnType.pctLifeExpectancy:
					result = GKUtils.GetLifeExpectancy(this.fRec);
					break;

				case TPersonColumnType.pctDaysForBirth:
					result = GKUtils.GetDaysForBirth(this.fRec);
					break;

				case TPersonColumnType.pctGroups:
					result = this.GetGroups();
					break;

				case TPersonColumnType.pctReligion:
					result = buf_religion;
					break;

				case TPersonColumnType.pctNationality:
					result = buf_nationality;
					break;

				case TPersonColumnType.pctEducation:
					result = buf_education;
					break;

				case TPersonColumnType.pctOccupation:
					result = buf_occupation;
					break;

				case TPersonColumnType.pctCaste:
					result = buf_caste;
					break;

				case TPersonColumnType.pctMili:
					result = buf_mili;
					break;

				case TPersonColumnType.pctMiliInd:
					result = buf_mili_ind;
					break;

				case TPersonColumnType.pctMiliDis:
					result = buf_mili_dis;
					break;

				case TPersonColumnType.pctMiliRank:
					result = buf_mili_rank;
					break;

				case TPersonColumnType.pctChangeDate:
					result = this.fRec.ChangeDate.ChangeDateTime;
					break;

				case TPersonColumnType.pctBookmark:
					result = ((this.fRec.Bookmark) ? "*" : " ");
					break;

				case TPersonColumnType.pctTitle:
					result = buf_title;
					break;
			}
			return result;
		}

		public override void InitFilter()
		{
			TIndividualListFilter iFilter = (TIndividualListFilter)fFilter;
			
			if (!DateTime.TryParse(iFilter.AliveBeforeDate, out this.filter_abd)) {
				this.filter_abd = new DateTime(0);
			}

			if (iFilter.GroupRef == "") {
				this.filter_grp = null;
			} else {
				this.filter_grp = this.fTree.XRefIndex_Find(iFilter.GroupRef) as TGEDCOMGroupRecord;
			}

			if (iFilter.SourceRef == "") {
				this.filter_source = null;
			} else {
				this.filter_source = this.fTree.XRefIndex_Find(iFilter.SourceRef) as TGEDCOMSourceRecord;
			}
		}

		public TGEDCOMCustomEvent buf_bd;
		public TGEDCOMCustomEvent buf_dd;

		private string buf_residence;
		private string buf_religion;
		private string buf_nationality;
		private string buf_education;
		private string buf_occupation;
		private string buf_caste;
		private string buf_mili;
		private string buf_mili_ind;
		private string buf_mili_dis;
		private string buf_mili_rank;
		private string buf_title;

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.fRec = (aRec as TGEDCOMIndividualRecord);

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

			GlobalOptions gOptions = TfmGEDKeeper.Instance.Options;
			int num = this.fRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = this.fRec.IndividualEvents[i];

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

			GlobalOptions gOptions = TfmGEDKeeper.Instance.Options;

			if ((fRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnparented))
			{
				item.BackColor = System.Drawing.Color.FromArgb(0xFFCACA);
			}
			else if ((fRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnmarried))
			{
				item.BackColor = System.Drawing.Color.FromArgb(0xFFFFCA);
			}
		}

		public override void UpdateColumns(GKListView aList, bool isMain)
		{
			TIndividualListColumns columns = TfmGEDKeeper.Instance.Options.IndividualListColumns;
			NameFormat defNameFormat = TfmGEDKeeper.Instance.Options.DefNameFormat;

			this.ColumnsMap_Clear();
			this.AddListColumn(aList, "№", 50, false, 0, 0);

			for (int i = 0; i < columns.Count; i++)
			{
				if (columns[i].colActive) {
					byte bColType = columns[i].colType;
					TPersonColumnType colType = (TPersonColumnType)bColType;

					if (colType == TPersonColumnType.pctName) {
					    const bool asz = false;

					    switch (defNameFormat) {
							case NameFormat.nfF_N_P:
					    		this.AddListColumn(aList, LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
					    		this.AddListColumn(aList, LangMan.LS(LSID.LSID_Name), 100, asz, bColType, 1);
					    		this.AddListColumn(aList, LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 2);
								break;

							case NameFormat.nfF_NP:
								this.AddListColumn(aList, LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
								this.AddListColumn(aList, LangMan.LS(LSID.LSID_Name) + "," + LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 1);
								break;

							case NameFormat.nfFNP:
								this.AddListColumn(aList, LangMan.LS(LSID.LSID_FullName), 300, asz, bColType, 0);
								break;
						}
					} else {
						this.AddListColumn(aList, LangMan.LS(GlobalOptions.PersonColumnsName[bColType].Name),
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
