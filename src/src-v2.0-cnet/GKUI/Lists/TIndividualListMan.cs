using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TIndividualListMan : TListManager
	{
		[StructLayout(LayoutKind.Auto)]
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
			string Result = "";
			int arg_15_0 = 0;
			int num = this.FRec.GroupsCount - 1;
			int idx = arg_15_0;
			if (num >= idx)
			{
				num++;
				do
				{
					TGEDCOMGroupRecord grp = this.FRec.GetGroup(idx).Value as TGEDCOMGroupRecord;
					if (grp != null)
					{
						Result += grp.Name;
						if (idx < this.FRec.GroupsCount - 1)
						{
							Result += "; ";
						}
					}
					idx++;
				}
				while (idx != num);
			}
			return Result;
		}

		private bool HasPlace(TPersonsFilter aFilter)
		{
			bool Result = false;
			bool addr = GKL.fmGEDKeeper.Options.PlacesWithAddress;
			int arg_21_0 = 0;
			int num = this.FRec.IndividualEventsCount - 1;
			int i = arg_21_0;
			if (num >= i)
			{
				num++;
				do
				{
					string place = TGenEngine.GetPlaceStr(this.FRec.GetIndividualEvent(i), addr);
					Result = TGenEngine.IsMatchesMask(place, aFilter.Residence);
					if (Result)
					{
						break;
					}
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		private bool HasEventVal(TPersonsFilter aFilter)
		{
			bool Result = false;
			int arg_11_0 = 0;
			int num = this.FRec.IndividualEventsCount - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				do
				{
					Result = TGenEngine.IsMatchesMask(this.FRec.GetIndividualEvent(i).StringValue, aFilter.EventVal);
					if (Result)
					{
						break;
					}
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		internal void SetColMap(byte aType, byte aSubType, ref int cols)
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

			int num = this.FRec.IndividualEventsCount - 1;
			int i = 0;
			ushort j = 0;
			ushort d = 0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMCustomEvent ev = this.FRec.GetIndividualEvent(i);
					if (ev.Name == "BIRT" && bd_ev == null)
					{
						bd_ev = ev;
						int y = -1;
						TGenEngine.GetIndependentDate(ev.Detail.Date.Value, ref y, ref j, ref d);
						if (y > 0)
						{
							if (this.FYearMin > y)
							{
								this.FYearMin = y;
							}
							if (this.FYearMax < y)
							{
								this.FYearMax = y;
							}
						}
					}
					else
					{
						if (ev.Name == "DEAT" && dd_ev == null)
						{
							dd_ev = ev;
						}
					}
					i++;
				}
				while (i != num);
			}

			if ((this.FRec.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) && (aFilter.Sex == TGEDCOMObject.TGEDCOMSex.svNone || this.FRec.Sex == aFilter.Sex) && (BDSSystem.WStrCmp(aFilter.Name, "*") == 0 || TGenEngine.IsMatchesMask(TGenEngine.GetNameStr(this.FRec, true, false), aFilter.Name)) && (BDSSystem.WStrCmp(aFilter.Residence, "*") == 0 || this.HasPlace(aFilter)) && (BDSSystem.WStrCmp(aFilter.EventVal, "*") == 0 || this.HasEventVal(aFilter)) && (!aFilter.PatriarchOnly || this.FRec.Patriarch))
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
									TGenEngine.GetIndependentDate(bd_ev.Detail.Date.Value, ref bdy, ref j, ref d);
								}

								int ddy = -1;
								if (dd_ev != null)
								{
									TGenEngine.GetIndependentDate(dd_ev.Detail.Date.Value, ref ddy, ref j, ref d);
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
								bdt = new DateTime((long)((ulong)0));
							}
							else
							{
								bdt = TGenEngine.GEDCOMDateToDate(bd_ev.Detail.Date.Value);
							}

							DateTime ddt;
							if (dd_ev == null)
							{
								ddt = new DateTime((long)((ulong)0));
							}
							else
							{
								ddt = TGenEngine.GEDCOMDateToDate(dd_ev.Detail.Date.Value);
							}

							if (bdt.Ticks != (long)((ulong)0) && (bdt.Ticks == (long)((ulong)0) || !(bdt < this.filter_abd)))
							{
								return Result;
							}

							if (ddt.Ticks != (long)((ulong)0))
							{
								if (ddt.Ticks == (long)((ulong)0) || !(ddt > this.filter_abd))
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
						if (this.FRec.GroupsCount == 0)
						{
							return Result;
						}
					}
				}
				else
				{
					if (this.FRec.GroupsCount != 0)
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
						if (this.FRec.GetSourceCitationsCount() == 0)
						{
							return Result;
						}
					}
				}
				else
				{
					if (this.FRec.GetSourceCitationsCount() != 0)
					{
						return Result;
					}
				}
				if (!aFilter.ChildSelector || this.FRec.ChildToFamilyLinksCount == 0)
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
					TGenEngine.TNameFormat defNameFormat = GKL.fmGEDKeeper.Options.DefNameFormat;
					string f = "";
					string i = "";
					string p = "";
					if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
					{
						TGenEngine.GetNameParts(this.FRec, ref f, ref i, ref p);
					}
					TGenEngine.TNameFormat defNameFormat2 = GKL.fmGEDKeeper.Options.DefNameFormat;
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
					Result = BDSSystem.WStrFromWChar(TGenEngine.SexStr(this.FRec.Sex)[0]);
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
						Result = TGenEngine.GEDCOMEventToDateStr(ev, GKL.fmGEDKeeper.Options.DefDateFormat, false);
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
						Result = TGenEngine.GEDCOMEventToDateStr(ev, GKL.fmGEDKeeper.Options.DefDateFormat, false);
					}
					break;
				}
				case TGlobalOptions.TPersonColumnType.pctResidence:
				{
					Result = TGenEngine.GetResidencePlace(this.FRec, GKL.fmGEDKeeper.Options.PlacesWithAddress);
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

		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			TGlobalOptions.TPersonColumnProps[] columns = new TGlobalOptions.TPersonColumnProps[24];
			base.GetRow(aRec, isMain, ref aRow);
			Array.Copy(GKL.fmGEDKeeper.Options.ListPersonsColumns, columns, 24);
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
			int arg_85_0 = 0;
			int num = this.FRec.IndividualEventsCount - 1;
			int i = arg_85_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMCustomEvent ev = this.FRec.GetIndividualEvent(i);
					if (BDSSystem.WStrCmp(ev.Name, "BIRT") == 0 && bd_ev == null)
					{
						bd_ev = ev;
					}
					else
					{
						if (BDSSystem.WStrCmp(ev.Name, "DEAT") == 0 && dd_ev == null)
						{
							dd_ev = ev;
						}
						else
						{
							if (BDSSystem.WStrCmp(ev.Name, "RESI") == 0 && BDSSystem.WStrCmp(residence, "") == 0)
							{
								residence = TGenEngine.GetPlaceStr(ev, GKL.fmGEDKeeper.Options.PlacesWithAddress);
							}
							else
							{
								if (BDSSystem.WStrCmp(ev.Name, "RELI") == 0 && BDSSystem.WStrCmp(religion, "") == 0)
								{
									religion = ev.StringValue;
								}
								else
								{
									if (BDSSystem.WStrCmp(ev.Name, "NATI") == 0 && BDSSystem.WStrCmp(nationality, "") == 0)
									{
										nationality = ev.StringValue;
									}
									else
									{
										if (BDSSystem.WStrCmp(ev.Name, "EDUC") == 0 && BDSSystem.WStrCmp(education, "") == 0)
										{
											education = ev.StringValue;
										}
										else
										{
											if (BDSSystem.WStrCmp(ev.Name, "OCCU") == 0 && BDSSystem.WStrCmp(occupation, "") == 0)
											{
												occupation = ev.StringValue;
											}
											else
											{
												if (BDSSystem.WStrCmp(ev.Name, "CAST") == 0 && BDSSystem.WStrCmp(caste, "") == 0)
												{
													caste = ev.StringValue;
												}
												else
												{
													if (BDSSystem.WStrCmp(ev.Name, "_MILI") == 0 && BDSSystem.WStrCmp(mili, "") == 0)
													{
														mili = ev.StringValue;
													}
													else
													{
														if (BDSSystem.WStrCmp(ev.Name, "_MILI_IND") == 0 && BDSSystem.WStrCmp(mili_ind, "") == 0)
														{
															mili_ind = ev.StringValue;
														}
														else
														{
															if (BDSSystem.WStrCmp(ev.Name, "_MILI_DIS") == 0 && BDSSystem.WStrCmp(mili_dis, "") == 0)
															{
																mili_dis = ev.StringValue;
															}
															else
															{
																if (BDSSystem.WStrCmp(ev.Name, "_MILI_RANK") == 0 && BDSSystem.WStrCmp(mili_rank, "") == 0)
																{
																	mili_rank = ev.StringValue;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
					i++;
				}
				while (i != num);
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
								aRow = aRow + "\0" + "*";
							}
							else
							{
								aRow = aRow + "\0" + " ";
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctName:
						{
							TGenEngine.TNameFormat defNameFormat = GKL.fmGEDKeeper.Options.DefNameFormat;
							string f = "";
							string j = "";
							string p = "";
							if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
							{
								TGenEngine.GetNameParts(this.FRec, ref f, ref j, ref p);
							}
							TGenEngine.TNameFormat defNameFormat2 = GKL.fmGEDKeeper.Options.DefNameFormat;
							if (defNameFormat2 != TGenEngine.TNameFormat.nfFNP)
							{
								if (defNameFormat2 != TGenEngine.TNameFormat.nfF_NP)
								{
									if (defNameFormat2 == TGenEngine.TNameFormat.nfF_N_P)
									{
										aRow = aRow + "\0" + f;
										aRow = aRow + "\0" + j;
										aRow = aRow + "\0" + p;
									}
								}
								else
								{
									aRow = aRow + "\0" + f;
									aRow = string.Concat(new string[]
									{
										aRow, 
										"\0", 
										j, 
										" ", 
										p
									});
								}
							}
							else
							{
								aRow = aRow + "\0" + TGenEngine.GetNameStr(this.FRec, true, false);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctNick:
						{
							aRow = aRow + "\0" + TGenEngine.GetNickStr(this.FRec);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctSex:
						{
							aRow = aRow + "\0" + BDSSystem.WStrFromWChar(TGenEngine.SexStr(this.FRec.Sex)[0]);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBirthDate:
						{
							aRow = aRow + "\0" + TGenEngine.GEDCOMEventToDateStr(bd_ev, GKL.fmGEDKeeper.Options.DefDateFormat, false);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDeathDate:
						{
							aRow = aRow + "\0" + TGenEngine.GEDCOMEventToDateStr(dd_ev, GKL.fmGEDKeeper.Options.DefDateFormat, false);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBirthPlace:
						{
							aRow = aRow + "\0" + TGenEngine.GetPlaceStr(bd_ev, false);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDeathPlace:
						{
							aRow = aRow + "\0" + TGenEngine.GetPlaceStr(dd_ev, false);
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctResidence:
						{
							aRow = aRow + "\0" + residence;
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctAge:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + TGenEngine.GetAge(this.FRec, this.age_year);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctLifeExpectancy:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + TGenEngine.GetLifeExpectancy(this.FRec);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDaysForBirth:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + TGenEngine.GetDaysForBirth(this.FRec);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctGroups:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + this.GetGroups();
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctReligion:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + religion;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctNationality:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + nationality;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctEducation:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + education;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctOccupation:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + occupation;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctCaste:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + caste;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMili:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + mili;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliInd:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + mili_ind;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliDis:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + mili_dis;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliRank:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + mili_rank;
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctChangeDate:
						{
							if (isMain)
							{
								aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBookmark:
						{
							if (isMain)
							{
								if (this.FRec.Bookmark)
								{
									aRow = aRow + "\0" + "*";
								}
								else
								{
									aRow = aRow + "\0" + " ";
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

		public override void InitFilter(TPersonsFilter aFilter)
		{
			if (!DateTime.TryParse(aFilter.AliveBeforeDate, out this.filter_abd)) {
				this.filter_abd = new DateTime((long)0);
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
			Array.Copy(GKL.fmGEDKeeper.Options.ListPersonsColumns, columns, 24);
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
			int arg_7C_0 = 0;
			int num = this.FRec.IndividualEventsCount - 1;
			int i = arg_7C_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMCustomEvent ev = this.FRec.GetIndividualEvent(i);
					if (BDSSystem.WStrCmp(ev.Name, "BIRT") == 0 && bd_ev == null)
					{
						bd_ev = ev;
					}
					else
					{
						if (BDSSystem.WStrCmp(ev.Name, "DEAT") == 0 && dd_ev == null)
						{
							dd_ev = ev;
						}
						else
						{
							if (BDSSystem.WStrCmp(ev.Name, "RESI") == 0 && BDSSystem.WStrCmp(residence, "") == 0)
							{
								residence = TGenEngine.GetPlaceStr(ev, GKL.fmGEDKeeper.Options.PlacesWithAddress);
							}
							else
							{
								if (BDSSystem.WStrCmp(ev.Name, "RELI") == 0 && BDSSystem.WStrCmp(religion, "") == 0)
								{
									religion = ev.StringValue;
								}
								else
								{
									if (BDSSystem.WStrCmp(ev.Name, "NATI") == 0 && BDSSystem.WStrCmp(nationality, "") == 0)
									{
										nationality = ev.StringValue;
									}
									else
									{
										if (BDSSystem.WStrCmp(ev.Name, "EDUC") == 0 && BDSSystem.WStrCmp(education, "") == 0)
										{
											education = ev.StringValue;
										}
										else
										{
											if (BDSSystem.WStrCmp(ev.Name, "OCCU") == 0 && BDSSystem.WStrCmp(occupation, "") == 0)
											{
												occupation = ev.StringValue;
											}
											else
											{
												if (BDSSystem.WStrCmp(ev.Name, "CAST") == 0 && BDSSystem.WStrCmp(caste, "") == 0)
												{
													caste = ev.StringValue;
												}
												else
												{
													if (BDSSystem.WStrCmp(ev.Name, "_MILI") == 0 && BDSSystem.WStrCmp(mili, "") == 0)
													{
														mili = ev.StringValue;
													}
													else
													{
														if (BDSSystem.WStrCmp(ev.Name, "_MILI_IND") == 0 && BDSSystem.WStrCmp(mili_ind, "") == 0)
														{
															mili_ind = ev.StringValue;
														}
														else
														{
															if (BDSSystem.WStrCmp(ev.Name, "_MILI_DIS") == 0 && BDSSystem.WStrCmp(mili_dis, "") == 0)
															{
																mili_dis = ev.StringValue;
															}
															else
															{
																if (BDSSystem.WStrCmp(ev.Name, "_MILI_RANK") == 0 && BDSSystem.WStrCmp(mili_rank, "") == 0)
																{
																	mili_rank = ev.StringValue;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
					i++;
				}
				while (i != num);
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
							TGenEngine.TNameFormat defNameFormat = GKL.fmGEDKeeper.Options.DefNameFormat;
							string f = "";
							string j = "";
							string p = "";
							if (defNameFormat >= TGenEngine.TNameFormat.nfF_NP && defNameFormat < (TGenEngine.TNameFormat)3)
							{
								TGenEngine.GetNameParts(this.FRec, ref f, ref j, ref p);
							}
							TGenEngine.TNameFormat defNameFormat2 = GKL.fmGEDKeeper.Options.DefNameFormat;
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
							aItem.SubItems.Add(BDSSystem.WStrFromWChar(TGenEngine.SexStr(this.FRec.Sex)[0]));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctBirthDate:
						{
							aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(bd_ev, GKL.fmGEDKeeper.Options.DefDateFormat, false));
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDeathDate:
						{
							aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(dd_ev, GKL.fmGEDKeeper.Options.DefDateFormat, false));
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
							if (isMain)
							{
								aItem.SubItems.Add(TGenEngine.GetAge(this.FRec, this.age_year));
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctLifeExpectancy:
						{
							if (isMain)
							{
								aItem.SubItems.Add(TGenEngine.GetLifeExpectancy(this.FRec));
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctDaysForBirth:
						{
							if (isMain)
							{
								aItem.SubItems.Add(TGenEngine.GetDaysForBirth(this.FRec));
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctGroups:
						{
							if (isMain)
							{
								aItem.SubItems.Add(this.GetGroups());
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctReligion:
						{
							if (isMain)
							{
								aItem.SubItems.Add(religion);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctNationality:
						{
							if (isMain)
							{
								aItem.SubItems.Add(nationality);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctEducation:
						{
							if (isMain)
							{
								aItem.SubItems.Add(education);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctOccupation:
						{
							if (isMain)
							{
								aItem.SubItems.Add(occupation);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctCaste:
						{
							if (isMain)
							{
								aItem.SubItems.Add(caste);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMili:
						{
							if (isMain)
							{
								aItem.SubItems.Add(mili);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliInd:
						{
							if (isMain)
							{
								aItem.SubItems.Add(mili_ind);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliDis:
						{
							if (isMain)
							{
								aItem.SubItems.Add(mili_dis);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctMiliRank:
						{
							if (isMain)
							{
								aItem.SubItems.Add(mili_rank);
							}
							break;
						}
						case TGlobalOptions.TPersonColumnType.pctChangeDate:
						{
							if (isMain)
							{
								aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
							}
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
			Array.Copy(GKL.fmGEDKeeper.Options.ListPersonsColumns, columns, 24);
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
										this.SetColMap((byte)((sbyte)col_type), 0, ref cols);
									}
								}
							}
							else
							{
								aList.AddListColumn(GKL.LSList[(int)TGlobalOptions.PersonColumnsName[(int)col_type].Name - 1], TGlobalOptions.PersonColumnsName[(int)col_type].DefWidth, false);
								this.SetColMap((byte)((sbyte)col_type), 0, ref cols);
							}
						}
						else
						{
							bool asz = false;
							TGenEngine.TNameFormat defNameFormat = GKL.fmGEDKeeper.Options.DefNameFormat;
							if (defNameFormat != TGenEngine.TNameFormat.nfFNP)
							{
								if (defNameFormat != TGenEngine.TNameFormat.nfF_NP)
								{
									if (defNameFormat == TGenEngine.TNameFormat.nfF_N_P)
									{
										aList.AddListColumn(GKL.LSList[84], 150, asz);
										this.SetColMap((byte)((sbyte)col_type), 0, ref cols);
										aList.AddListColumn(GKL.LSList[85], 100, asz);
										this.SetColMap((byte)((sbyte)col_type), 1, ref cols);
										aList.AddListColumn(GKL.LSList[86], 150, asz);
										this.SetColMap((byte)((sbyte)col_type), 2, ref cols);
									}
								}
								else
								{
									aList.AddListColumn(GKL.LSList[84], 150, asz);
									this.SetColMap((byte)((sbyte)col_type), 0, ref cols);
									aList.AddListColumn(GKL.LSList[85] + "," + GKL.LSList[86], 150, asz);
									this.SetColMap((byte)((sbyte)col_type), 1, ref cols);
								}
							}
							else
							{
								aList.AddListColumn(GKL.LSList[301], 300, asz);
								this.SetColMap((byte)((sbyte)col_type), 0, ref cols);
							}
						}
					}
					else
					{
						aList.AddListColumn(GKL.LSList[92], 25, false);
						this.SetColMap((byte)((sbyte)col_type), 0, ref cols);
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
