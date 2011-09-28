using System;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI.Lists
{
	public class TIndividualListMan : TListManager
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
		private TIndividualListMan.TColRec[] FColumnsMap = new TIndividualListMan.TColRec[256];

		public int YearMin
		{
			get { return this.FYearMin; }
		}

		public int YearMax
		{
			get { return this.FYearMax; }
		}

		private string GetGroups()
		{
			string result = "";

			int num = this.FRec.Groups.Count - 1;
			for (int idx = 0; idx <= num; idx++)
			{
				TGEDCOMGroupRecord grp = this.FRec.Groups[idx].Value as TGEDCOMGroupRecord;
				if (grp != null)
				{
					result += grp.GroupName;
					if (idx < this.FRec.Groups.Count - 1) result += "; ";
				}
			}

			return result;
		}

		private bool HasPlace(TPersonsFilter aFilter)
		{
			bool res = false;

			bool addr = GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string place = TGenEngine.GetPlaceStr(this.FRec.IndividualEvents[i], addr);
				res = TGenEngine.IsMatchesMask(place, aFilter.Residence);
				if (res) break;
			}

			return res;
		}

		private bool HasEventVal(TPersonsFilter aFilter)
		{
			bool result = false;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				result = TGenEngine.IsMatchesMask(this.FRec.IndividualEvents[i].StringValue, aFilter.EventVal);
				if (result) break;
			}
			return result;
		}

		private void SetColMap(byte aType, byte aSubType, ref int cols)
		{
			cols++;
			this.FColumnsMap[cols].col_type = aType;
			this.FColumnsMap[cols].col_subtype = aSubType;
		}

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
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
					TGenEngine.GetIndependentDate(ev.Detail.Date.Value, out y, out j, out d);
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

			if ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) 
			    && (aFilter.Sex == TGEDCOMSex.svNone || this.FRec.Sex == aFilter.Sex) 
			    && (aFilter.Name == "*" || TGenEngine.IsMatchesMask(TGenEngine.GetNameStr(this.FRec, true, false), aFilter.Name)) 
				&& (aFilter.Residence == "*" || this.HasPlace(aFilter)) && (aFilter.EventVal == "*" || this.HasEventVal(aFilter)) 
					&& (!aFilter.PatriarchOnly || this.FRec.Patriarch))
			{
				bool isLive = dd_ev == null;
				TGenEngine.TLifeMode lifeMode = aFilter.LifeMode;
				if (lifeMode != TGenEngine.TLifeMode.lmOnlyAlive)
				{
					if (lifeMode != TGenEngine.TLifeMode.lmOnlyDead)
					{
						if (lifeMode != TGenEngine.TLifeMode.lmAliveBefore)
						{
							if (lifeMode == TGenEngine.TLifeMode.lmTimeLine)
							{
								int bdy = -1;
								if (bd_ev != null)
								{
									TGenEngine.GetIndependentDate(bd_ev.Detail.Date.Value, out bdy, out j, out d);
								}

								int ddy = -1;
								if (dd_ev != null)
								{
									TGenEngine.GetIndependentDate(dd_ev.Detail.Date.Value, out ddy, out j, out d);
								}

								if (this.age_year > 0 && (bdy <= 0 || bdy >= this.age_year || (ddy > 0 && (ddy <= 0 || ddy <= this.age_year))))
								{
									return Result;
								}
							}
						}
						else
						{
							DateTime bdt;
							if (bd_ev == null)
							{
								bdt = new DateTime(0);
							}
							else
							{
								bdt = TGenEngine.GEDCOMDateToDate(bd_ev.Detail.Date.Value);
							}

							DateTime ddt;
							if (dd_ev == null)
							{
								ddt = new DateTime(0);
							}
							else
							{
								ddt = TGenEngine.GEDCOMDateToDate(dd_ev.Detail.Date.Value);
							}

							if (bdt.Ticks != 0 && (bdt.Ticks == 0 || !(bdt < this.filter_abd)))
							{
								return Result;
							}

							if (ddt.Ticks != 0)
							{
								if (ddt.Ticks == 0 || !(ddt > this.filter_abd))
								{
									return Result;
								}
							}
						}
					}
					else
					{
						if (isLive)
						{
							return Result;
						}
					}
				}
				else
				{
					if (!isLive)
					{
						return Result;
					}
				}

				TFilter.TGroupMode groupMode = aFilter.GroupMode;
				if (groupMode != TFilter.TGroupMode.gmNone)
				{
					if (groupMode != TFilter.TGroupMode.gmAny)
					{
						if (groupMode == TFilter.TGroupMode.gmSelected)
						{
							if (this.FRec.IndexOfGroup(this.filter_grp) < 0)
							{
								return Result;
							}
						}
					}
					else
					{
						if (this.FRec.Groups.Count == 0)
						{
							return Result;
						}
					}
				}
				else
				{
					if (this.FRec.Groups.Count != 0)
					{
						return Result;
					}
				}

				TFilter.TGroupMode sourceMode = aFilter.SourceMode;
				if (sourceMode != TFilter.TGroupMode.gmNone)
				{
					if (sourceMode != TFilter.TGroupMode.gmAny)
					{
						if (sourceMode == TFilter.TGroupMode.gmSelected)
						{
							if (this.FRec.IndexOfSource(this.filter_source) < 0)
							{
								return Result;
							}
						}
					}
					else
					{
						if (this.FRec.SourceCitations.Count == 0)
						{
							return Result;
						}
					}
				}
				else
				{
					if (this.FRec.SourceCitations.Count != 0)
					{
						return Result;
					}
				}
				if (!aFilter.ChildSelector || this.FRec.ChildToFamilyLinks.Count == 0)
				{
					Result = true;
				}
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMIndividualRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			TGlobalOptions.TPersonColumnType pct = (TGlobalOptions.TPersonColumnType)this.FColumnsMap[aColIndex].col_type;
			int sub_index = (int)this.FColumnsMap[aColIndex].col_subtype;
			string Result = "";
			switch (pct)
			{
				case TGlobalOptions.TPersonColumnType.pctPatriarch:
				{
					if (this.FRec.Patriarch)
					{
						Result = "*";
					}
					else
					{
						Result = " ";
					}
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctName:
				{
					TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
					string f = "";
					string i = "";
					string p = "";
					if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
					{
						TGenEngine.GetNameParts(this.FRec, out f, out i, out p);
					}
					TGenEngine.TNameFormat defNameFormat2 = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
					if (defNameFormat2 != TGenEngine.TNameFormat.nfFNP)
					{
						if (defNameFormat2 != TGenEngine.TNameFormat.nfF_NP)
						{
							if (defNameFormat2 == TGenEngine.TNameFormat.nfF_N_P)
							{
								if (sub_index != 0)
								{
									if (sub_index != 1)
									{
										if (sub_index == 2)
										{
											Result = p;
										}
									}
									else
									{
										Result = i;
									}
								}
								else
								{
									Result = f;
								}
							}
						}
						else
						{
							if (sub_index != 0)
							{
								if (sub_index == 1)
								{
									Result = i + " " + p;
								}
							}
							else
							{
								Result = f;
							}
						}
					}
					else
					{
						Result = TGenEngine.GetNameStr(this.FRec, true, false);
					}
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctNick:
				{
					Result = TGenEngine.GetNickStr(this.FRec);
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctSex:
				{
					Result = new string(TGenEngine.SexStr(this.FRec.Sex)[0], 1);
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctBirthDate:
				case TGlobalOptions.TPersonColumnType.pctBirthPlace:
				{
					TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(this.FRec, "BIRT");
					if (pct != TGlobalOptions.TPersonColumnType.pctBirthDate)
					{
						if (pct == TGlobalOptions.TPersonColumnType.pctBirthPlace)
						{
							Result = TGenEngine.GetPlaceStr(ev, false);
						}
					}
					else
					{
						Result = TGenEngine.GEDCOMEventToDateStr(ev, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					}
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctDeathDate:
				case TGlobalOptions.TPersonColumnType.pctDeathPlace:
				{
					TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(this.FRec, "DEAT");
					if (pct != TGlobalOptions.TPersonColumnType.pctDeathDate)
					{
						if (pct == TGlobalOptions.TPersonColumnType.pctDeathPlace)
						{
							Result = TGenEngine.GetPlaceStr(ev, false);
						}
					}
					else
					{
						Result = TGenEngine.GEDCOMEventToDateStr(ev, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					}
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctResidence:
				{
					Result = TGenEngine.GetResidencePlace(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress);
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctAge:
				{
					Result = TGenEngine.GetAge(this.FRec, -1);
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctLifeExpectancy:
				{
					Result = TGenEngine.GetLifeExpectancy(this.FRec);
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctDaysForBirth:
				{
					Result = TGenEngine.GetDaysForBirth(this.FRec);
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctGroups:
				{
					Result = this.GetGroups();
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctReligion:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "RELI");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctNationality:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "NATI");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctEducation:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "EDUC");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctOccupation:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "OCCU");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctCaste:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "CAST");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctMili:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctMiliInd:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI_IND");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctMiliDis:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI_DIS");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctMiliRank:
				{
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI_RANK");
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctChangeDate:
				{
					Result = this.FRec.ChangeDate.ToString();
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctBookmark:
				{
					if (this.FRec.Bookmark)
					{
						Result = "*";
					}
					else
					{
						Result = " ";
					}
					break;
				}
			}
			return Result;
		}

		public override void InitFilter(TPersonsFilter aFilter)
		{
			if (!DateTime.TryParse(aFilter.AliveBeforeDate, out this.filter_abd)) {
				this.filter_abd = new DateTime(0);
			}

			if (aFilter.LifeMode != TGenEngine.TLifeMode.lmTimeLine) {
				this.age_year = -1;
			} else {
				this.age_year = aFilter.TimeLineYear;
			}

			if (aFilter.GroupRef == "") {
				this.filter_grp = null;
			} else {
				this.filter_grp = (TGEDCOMGroupRecord)this.FTree.XRefIndex_Find(aFilter.GroupRef);
			}

			if (aFilter.SourceRef == "") {
				this.filter_source = null;
			} else {
				this.filter_source = (TGEDCOMSourceRecord)this.FTree.XRefIndex_Find(aFilter.SourceRef);
			}

			this.FYearMin = 10000;
			this.FYearMax = 0;
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			TGlobalOptions.TPersonColumnProps[] columns = new TGlobalOptions.TPersonColumnProps[24];
			Array.Copy(GKUI.TfmGEDKeeper.Instance.Options.ListPersonsColumns, columns, 24);
			TGEDCOMCustomEvent bd_ev = null;
			TGEDCOMCustomEvent dd_ev = null;

			string residence = "";
			string religion = "";
			string nationality = "";
			string education = "";
			string occupation = "";
			string caste = "";
			string mili = "";
			string mili_ind = "";
			string mili_dis = "";
			string mili_rank = "";

			int i;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = this.FRec.IndividualEvents[i];

				if (ev.Name == "BIRT" && bd_ev == null)
				{
					bd_ev = ev;
				}
				else if (ev.Name == "DEAT" && dd_ev == null)
				{
					dd_ev = ev;
				}
				else if (ev.Name == "RESI" && residence == "")
				{
					residence = TGenEngine.GetPlaceStr(ev, GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress);
				}
				else if (ev.Name == "RELI" && religion == "")
				{
					religion = ev.StringValue;
				}
				else if (ev.Name == "NATI" && nationality == "")
				{
					nationality = ev.StringValue;
				}
				else if (ev.Name == "EDUC" && education == "")
				{
					education = ev.StringValue;
				}
				else if (ev.Name == "OCCU" && occupation == "")
				{
					occupation = ev.StringValue;
				}
				else if (ev.Name == "CAST" && caste == "")
				{
					caste = ev.StringValue;
				}
				else if (ev.Name == "_MILI" && mili == "")
				{
					mili = ev.StringValue;
				}
				else if (ev.Name == "_MILI_IND" && mili_ind == "")
				{
					mili_ind = ev.StringValue;
				}
				else if (ev.Name == "_MILI_DIS" && mili_dis == "")
				{
					mili_dis = ev.StringValue;
				}
				else if (ev.Name == "_MILI_RANK" && mili_rank == "")
				{
					mili_rank = ev.StringValue;
				}
			}

			i = 0;
			do
			{
				TGlobalOptions.TPersonColumnType pct = columns[i].colType;
				if (columns[i].colActive)
				{
					switch (pct)
					{
						case TGlobalOptions.TPersonColumnType.pctPatriarch:
						{
							if (this.FRec.Patriarch)
							{
								aItem.SubItems.Add("*");
							}
							else
							{
								aItem.SubItems.Add(" ");
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctName:
						{
							TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
							string f = "";
							string j = "";
							string p = "";
							if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
							{
								TGenEngine.GetNameParts(this.FRec, out f, out j, out p);
							}
							TGenEngine.TNameFormat defNameFormat2 = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
							if (defNameFormat2 != TGenEngine.TNameFormat.nfFNP)
							{
								if (defNameFormat2 != TGenEngine.TNameFormat.nfF_NP)
								{
									if (defNameFormat2 == TGenEngine.TNameFormat.nfF_N_P)
									{
										aItem.SubItems.Add(f);
										aItem.SubItems.Add(j);
										aItem.SubItems.Add(p);
									}
								}
								else
								{
									aItem.SubItems.Add(f);
									aItem.SubItems.Add(j + " " + p);
								}
							}
							else
							{
								aItem.SubItems.Add(TGenEngine.GetNameStr(this.FRec, true, false));
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctNick:
						{
							aItem.SubItems.Add(TGenEngine.GetNickStr(this.FRec));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctSex:
						{
							aItem.SubItems.Add(new string(TGenEngine.SexStr(this.FRec.Sex)[0], 1));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBirthDate:
						{
							aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(bd_ev, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDeathDate:
						{
							aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(dd_ev, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBirthPlace:
						{
							aItem.SubItems.Add(TGenEngine.GetPlaceStr(bd_ev, false));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDeathPlace:
						{
							aItem.SubItems.Add(TGenEngine.GetPlaceStr(dd_ev, false));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctResidence:
						{
							aItem.SubItems.Add(residence);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctAge:
						{
							if (isMain) aItem.SubItems.Add(TGenEngine.GetAge(this.FRec, this.age_year));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctLifeExpectancy:
						{
							if (isMain) aItem.SubItems.Add(TGenEngine.GetLifeExpectancy(this.FRec));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDaysForBirth:
						{
							if (isMain) aItem.SubItems.Add(TGenEngine.GetDaysForBirth(this.FRec));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctGroups:
						{
							if (isMain) aItem.SubItems.Add(this.GetGroups());
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctReligion:
						{
							if (isMain) aItem.SubItems.Add(religion);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctNationality:
						{
							if (isMain) aItem.SubItems.Add(nationality);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctEducation:
						{
							if (isMain) aItem.SubItems.Add(education);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctOccupation:
						{
							if (isMain) aItem.SubItems.Add(occupation);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctCaste:
						{
							if (isMain) aItem.SubItems.Add(caste);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMili:
						{
							if (isMain) aItem.SubItems.Add(mili);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliInd:
						{
							if (isMain) aItem.SubItems.Add(mili_ind);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliDis:
						{
							if (isMain) aItem.SubItems.Add(mili_dis);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliRank:
						{
							if (isMain) aItem.SubItems.Add(mili_rank);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctChangeDate:
						{
							if (isMain) aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBookmark:
						{
							if (isMain)
							{
								if (this.FRec.Bookmark)
								{
									aItem.SubItems.Add("*");
								}
								else
								{
									aItem.SubItems.Add(" ");
								}
							}
							break;
						}
					}
				}
				i++;
			}
			while (i != 24);
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			TGlobalOptions.TPersonColumnProps[] columns = new TGlobalOptions.TPersonColumnProps[24];
			Array.Copy(GKUI.TfmGEDKeeper.Instance.Options.ListPersonsColumns, columns, 24);
			int cols = 0;
			aList.AddListColumn("№", 50, false);
			int i = 0;
			do
			{
				if (columns[i].colActive)
				{
					TGlobalOptions.TPersonColumnType col_type = columns[i].colType;
					if (col_type != TGlobalOptions.TPersonColumnType.pctPatriarch)
					{
						if (col_type != TGlobalOptions.TPersonColumnType.pctName)
						{
							if ((byte)col_type - (byte)TGlobalOptions.TPersonColumnType.pctNick >= (byte)TGlobalOptions.TPersonColumnType.pctDeathPlace)
							{
								if ((byte)col_type - (byte)TGlobalOptions.TPersonColumnType.pctAge < (byte)TGlobalOptions.TPersonColumnType.pctEducation)
								{
									if (isMain)
									{
										aList.AddListColumn(GKL.LSList[(int)TGlobalOptions.PersonColumnsName[(int)col_type].Name - 1], TGlobalOptions.PersonColumnsName[(int)col_type].DefWidth, false);
										this.SetColMap((byte)col_type, 0, ref cols);
									}
								}
							}
							else
							{
								aList.AddListColumn(GKL.LSList[(int)TGlobalOptions.PersonColumnsName[(int)col_type].Name - 1], TGlobalOptions.PersonColumnsName[(int)col_type].DefWidth, false);
								this.SetColMap((byte)col_type, 0, ref cols);
							}
						}
						else
						{
							bool asz = false;
							TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
							if (defNameFormat != TGenEngine.TNameFormat.nfFNP)
							{
								if (defNameFormat != TGenEngine.TNameFormat.nfF_NP)
								{
									if (defNameFormat == TGenEngine.TNameFormat.nfF_N_P)
									{
										aList.AddListColumn(GKL.LSList[84], 150, asz);
										this.SetColMap((byte)col_type, 0, ref cols);
										aList.AddListColumn(GKL.LSList[85], 100, asz);
										this.SetColMap((byte)col_type, 1, ref cols);
										aList.AddListColumn(GKL.LSList[86], 150, asz);
										this.SetColMap((byte)col_type, 2, ref cols);
									}
								}
								else
								{
									aList.AddListColumn(GKL.LSList[84], 150, asz);
									this.SetColMap((byte)col_type, 0, ref cols);
									aList.AddListColumn(GKL.LSList[85] + "," + GKL.LSList[86], 150, asz);
									this.SetColMap((byte)col_type, 1, ref cols);
								}
							}
							else
							{
								aList.AddListColumn(GKL.LSList[301], 300, asz);
								this.SetColMap((byte)col_type, 0, ref cols);
							}
						}
					}
					else
					{
						aList.AddListColumn(GKL.LSList[92], 25, false);
						this.SetColMap((byte)col_type, 0, ref cols);
					}
				}
				i++;
			}
			while (i != 24);
		}

		public TIndividualListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
