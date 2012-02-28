using System;

using GedCom551;
using GKCore;
using GKCore.Settings;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
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

		private bool HasPlace(TPersonsFilter aFilter)
		{
			bool res = false;

			bool addr = GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress;
			int num = this.FRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string place = TGenEngine.GetPlaceStr(this.FRec.IndividualEvents[i], addr);
				res = IsMatchesMask(place, aFilter.Residence);
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
				result = IsMatchesMask(this.FRec.IndividualEvents[i].StringValue, aFilter.EventVal);
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

			if ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) 
			    && (aFilter.Sex == TGEDCOMSex.svNone || this.FRec.Sex == aFilter.Sex) 
			    && (aFilter.Name == "*" || IsMatchesMask(this.FRec.aux_GetNameStr(true, false), aFilter.Name)) 
				&& (aFilter.Residence == "*" || this.HasPlace(aFilter)) && (aFilter.EventVal == "*" || this.HasEventVal(aFilter)) 
					&& (!aFilter.PatriarchOnly || this.FRec.Patriarch))
			{
				bool isLive = (dd_ev == null);

				switch (aFilter.LifeMode) {
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

				switch (aFilter.GroupMode) {
					case TFilter.TGroupMode.gmAll:
						break;
					case TFilter.TGroupMode.gmNone:
						if (this.FRec.Groups.Count != 0) return Result;
						break;
					case TFilter.TGroupMode.gmAny:
						if (this.FRec.Groups.Count == 0) return Result;
						break;
					case TFilter.TGroupMode.gmSelected:
						if (this.FRec.IndexOfGroup(this.filter_grp) < 0) return Result;
						break;
				}

				switch (aFilter.SourceMode) {
					case TFilter.TGroupMode.gmAll:
						break;
					case TFilter.TGroupMode.gmNone:
						if (this.FRec.SourceCitations.Count != 0) return Result;
						break;
					case TFilter.TGroupMode.gmAny:
						if (this.FRec.SourceCitations.Count == 0) return Result;
						break;
					case TFilter.TGroupMode.gmSelected:
						if (this.FRec.IndexOfSource(this.filter_source) < 0) return Result;
						break;
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
			TPersonColumnType pct = (TPersonColumnType)this.FColumnsMap[aColIndex].col_type;
			int sub_index = (int)this.FColumnsMap[aColIndex].col_subtype;
			string Result = "";

			switch (pct)
			{
				case TPersonColumnType.pctPatriarch:
					Result = ((this.FRec.Patriarch) ? "*" : " ");
					break;

				case TPersonColumnType.pctName:
				{
					TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
					string f = "";
					string i = "";
					string p = "";
					if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
					{
						this.FRec.aux_GetNameParts(out f, out i, out p);
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
						Result = this.FRec.aux_GetNameStr(true, false);
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
					{
						TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(this.FRec, "BIRT");
						Result = TGenEngine.GEDCOMEventToDateStr(ev, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					}
					break;

				case TPersonColumnType.pctBirthPlace:
					{
						TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(this.FRec, "BIRT");
						Result = TGenEngine.GetPlaceStr(ev, false);
					}
					break;

				case TPersonColumnType.pctDeathDate:
					{
						TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(this.FRec, "DEAT");
						Result = TGenEngine.GEDCOMEventToDateStr(ev, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false);
					}
					break;

				case TPersonColumnType.pctDeathPlace:
					{
						TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(this.FRec, "DEAT");
						Result = TGenEngine.GetPlaceStr(ev, false);
					}
					break;

				case TPersonColumnType.pctResidence:
					Result = TGenEngine.GetResidencePlace(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.PlacesWithAddress);
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
					Result = TGenEngine.GetAttributeValue(this.FRec, "RELI");
					break;

				case TPersonColumnType.pctNationality:
					Result = TGenEngine.GetAttributeValue(this.FRec, "NATI");
					break;

				case TPersonColumnType.pctEducation:
					Result = TGenEngine.GetAttributeValue(this.FRec, "EDUC");
					break;

				case TPersonColumnType.pctOccupation:
					Result = TGenEngine.GetAttributeValue(this.FRec, "OCCU");
					break;

				case TPersonColumnType.pctCaste:
					Result = TGenEngine.GetAttributeValue(this.FRec, "CAST");
					break;

				case TPersonColumnType.pctMili:
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI");
					break;

				case TPersonColumnType.pctMiliInd:
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI_IND");
					break;

				case TPersonColumnType.pctMiliDis:
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI_DIS");
					break;

				case TPersonColumnType.pctMiliRank:
					Result = TGenEngine.GetAttributeValue(this.FRec, "_MILI_RANK");
					break;

				case TPersonColumnType.pctChangeDate:
					Result = this.FRec.ChangeDate.ToString();
					break;

				case TPersonColumnType.pctBookmark:
					Result = ((this.FRec.Bookmark) ? "*" : " ");
					break;

				case TPersonColumnType.pctTitle:
					Result = TGenEngine.GetAttributeValue(this.FRec, "TITL");
					break;
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
			TGlobalOptions gOptions = GKUI.TfmGEDKeeper.Instance.Options;
			TIndividualListColumns columns = gOptions.IndividualListColumns;

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
			string title = "";

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
					residence = TGenEngine.GetPlaceStr(ev, gOptions.PlacesWithAddress);
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
				else if (ev.Name == "TITL" && title == "")
				{
					title = ev.StringValue;
				}
			}

			for (i = 0; i < columns.Count; i++)
			{
				TPersonColumnType pct = columns[i].colType;

				if (columns[i].colActive)
				{
					switch (pct)
					{
						case TPersonColumnType.pctPatriarch:
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
						case TPersonColumnType.pctName:
						{
							TGenEngine.TNameFormat defNameFormat = gOptions.DefNameFormat;
							string f = "";
							string j = "";
							string p = "";
							if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
							{
								this.FRec.aux_GetNameParts(out f, out j, out p);
							}
							TGenEngine.TNameFormat defNameFormat2 = gOptions.DefNameFormat;
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
								aItem.SubItems.Add(this.FRec.aux_GetNameStr(true, false));
							}
							break;
						}
						case TPersonColumnType.pctNick:
						{
							aItem.SubItems.Add(this.FRec.aux_GetNickStr());
							break;
						}
						case TPersonColumnType.pctSex:
						{
							aItem.SubItems.Add(new string(TGenEngine.SexStr(this.FRec.Sex)[0], 1));
							break;
						}
						case TPersonColumnType.pctBirthDate:
						{
							aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(bd_ev, gOptions.DefDateFormat, false));
							break;
						}
						case TPersonColumnType.pctDeathDate:
						{
							aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(dd_ev, gOptions.DefDateFormat, false));
							break;
						}
						case TPersonColumnType.pctBirthPlace:
						{
							aItem.SubItems.Add(TGenEngine.GetPlaceStr(bd_ev, false));
							break;
						}
						case TPersonColumnType.pctDeathPlace:
						{
							aItem.SubItems.Add(TGenEngine.GetPlaceStr(dd_ev, false));
							break;
						}
						case TPersonColumnType.pctResidence:
						{
							aItem.SubItems.Add(residence);
							break;
						}
						case TPersonColumnType.pctAge:
						{
							if (isMain) aItem.SubItems.Add(TGenEngine.GetAge(this.FRec, this.age_year));
							break;
						}
						case TPersonColumnType.pctLifeExpectancy:
						{
							if (isMain) aItem.SubItems.Add(TGenEngine.GetLifeExpectancy(this.FRec));
							break;
						}
						case TPersonColumnType.pctDaysForBirth:
						{
							if (isMain) aItem.SubItems.Add(TGenEngine.GetDaysForBirth(this.FRec));
							break;
						}
						case TPersonColumnType.pctGroups:
						{
							if (isMain) aItem.SubItems.Add(this.GetGroups());
							break;
						}
						case TPersonColumnType.pctReligion:
						{
							if (isMain) aItem.SubItems.Add(religion);
							break;
						}
						case TPersonColumnType.pctNationality:
						{
							if (isMain) aItem.SubItems.Add(nationality);
							break;
						}
						case TPersonColumnType.pctEducation:
						{
							if (isMain) aItem.SubItems.Add(education);
							break;
						}
						case TPersonColumnType.pctOccupation:
						{
							if (isMain) aItem.SubItems.Add(occupation);
							break;
						}
						case TPersonColumnType.pctCaste:
						{
							if (isMain) aItem.SubItems.Add(caste);
							break;
						}
						case TPersonColumnType.pctMili:
						{
							if (isMain) aItem.SubItems.Add(mili);
							break;
						}
						case TPersonColumnType.pctMiliInd:
						{
							if (isMain) aItem.SubItems.Add(mili_ind);
							break;
						}
						case TPersonColumnType.pctMiliDis:
						{
							if (isMain) aItem.SubItems.Add(mili_dis);
							break;
						}
						case TPersonColumnType.pctMiliRank:
						{
							if (isMain) aItem.SubItems.Add(mili_rank);
							break;
						}
						case TPersonColumnType.pctChangeDate:
						{
							if (isMain) aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
							break;
						}
						case TPersonColumnType.pctBookmark:
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
						case TPersonColumnType.pctTitle:
						{
							if (isMain) aItem.SubItems.Add(title);
							break;
						}
					}
				}
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

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			TIndividualListColumns columns = GKUI.TfmGEDKeeper.Instance.Options.IndividualListColumns;

			int cols = 0;
			aList.AddListColumn("№", 50, false);

			for (int i = 0; i < columns.Count; i++)
			{
				if (columns[i].colActive)
				{
					TPersonColumnType col_type = columns[i].colType;

					switch (col_type)
					{
						case TPersonColumnType.pctPatriarch:
							aList.AddListColumn(LangMan.LSList[92], 25, false);
							this.SetColMap((byte)col_type, 0, ref cols);
							break;

						case TPersonColumnType.pctName:
							{
								bool asz = false;
								TGenEngine.TNameFormat defNameFormat = GKUI.TfmGEDKeeper.Instance.Options.DefNameFormat;
								if (defNameFormat != TGenEngine.TNameFormat.nfFNP)
								{
									if (defNameFormat != TGenEngine.TNameFormat.nfF_NP)
									{
										if (defNameFormat == TGenEngine.TNameFormat.nfF_N_P)
										{
											aList.AddListColumn(LangMan.LSList[84], 150, asz);
											this.SetColMap((byte)col_type, 0, ref cols);
											aList.AddListColumn(LangMan.LSList[85], 100, asz);
											this.SetColMap((byte)col_type, 1, ref cols);
											aList.AddListColumn(LangMan.LSList[86], 150, asz);
											this.SetColMap((byte)col_type, 2, ref cols);
										}
									}
									else
									{
										aList.AddListColumn(LangMan.LSList[84], 150, asz);
										this.SetColMap((byte)col_type, 0, ref cols);
										aList.AddListColumn(LangMan.LSList[85] + "," + LangMan.LSList[86], 150, asz);
										this.SetColMap((byte)col_type, 1, ref cols);
									}
								}
								else
								{
									aList.AddListColumn(LangMan.LSList[301], 300, asz);
									this.SetColMap((byte)col_type, 0, ref cols);
								}
							}
							break;

						case TPersonColumnType.pctNick:
						case TPersonColumnType.pctSex:
						case TPersonColumnType.pctBirthDate:
						case TPersonColumnType.pctDeathDate:
						case TPersonColumnType.pctBirthPlace:
						case TPersonColumnType.pctDeathPlace:
						case TPersonColumnType.pctResidence:
							aList.AddListColumn(LangMan.LSList[(int)TGlobalOptions.PersonColumnsName[(int)col_type].Name - 1], TGlobalOptions.PersonColumnsName[(int)col_type].DefWidth, false);
							this.SetColMap((byte)col_type, 0, ref cols);
							break;

						case TPersonColumnType.pctAge:
						case TPersonColumnType.pctLifeExpectancy:
						case TPersonColumnType.pctDaysForBirth:
						case TPersonColumnType.pctGroups:
						case TPersonColumnType.pctReligion:
						case TPersonColumnType.pctNationality:
						case TPersonColumnType.pctEducation:
						case TPersonColumnType.pctOccupation:
						case TPersonColumnType.pctCaste:
						case TPersonColumnType.pctMili:
						case TPersonColumnType.pctMiliInd:
						case TPersonColumnType.pctMiliDis:
						case TPersonColumnType.pctMiliRank:
						case TPersonColumnType.pctChangeDate:
						case TPersonColumnType.pctBookmark:
						case TPersonColumnType.pctTitle:
							if (isMain)
							{
								aList.AddListColumn(LangMan.LSList[(int)TGlobalOptions.PersonColumnsName[(int)col_type].Name - 1], TGlobalOptions.PersonColumnsName[(int)col_type].DefWidth, false);
								this.SetColMap((byte)col_type, 0, ref cols);
							}
							break;
					}
				}
			}
		}

		public TIndividualListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
