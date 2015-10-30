using System;
using System.Collections.Generic;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;

namespace GKCore.Stats
{
	/// <summary>
	/// 
	/// </summary>
	public class TreeStats
	{
		private readonly GEDCOMTree fTree;
		private readonly List<GEDCOMRecord> fSelectedRecords;
		
		public TreeStats(GEDCOMTree tree, List<GEDCOMRecord> selectedRecords)
		{
			this.fTree = tree;
			this.fSelectedRecords = selectedRecords;
		}

		public CommonStats GetCommonStats()
		{
			CommonStats stats = new CommonStats();
			
			int num = this.fSelectedRecords.Count;
			for (int i = 0; i < num; i++)
			{
				GEDCOMRecord rec = this.fSelectedRecords[i];
				if (rec is GEDCOMIndividualRecord)
				{
					GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
					stats.persons++;

					switch (ind.Sex) {
						case GEDCOMSex.svFemale:
							stats.persons_f++;
							if (ind.IsLive()) {
								stats.lives_f++;
								stats.lives++;
							}
							break;

						case GEDCOMSex.svMale:
							stats.persons_m++;
							if (ind.IsLive()) {
								stats.lives_m++;
								stats.lives++;
							}
							break;
					}

					string v_age = GKUtils.GetAge(ind, -1);
					stats.age.TakeVal(v_age, ind.Sex, true);

					string v_life = GKUtils.GetLifeExpectancy(ind);
					stats.life.TakeVal(v_life, ind.Sex, true);

					int ch_cnt = ind.GetTotalChildsCount();
					stats.childs.TakeVal(ch_cnt, ind.Sex, true);

					GEDCOMIndividualRecord iDummy;
					int v_fba = GKUtils.GetFirstbornAge(ind, out iDummy);
					stats.fba.TakeVal(v_fba, ind.Sex, true);

					int m_cnt = GKUtils.GetMarriagesCount(ind);
					stats.marr.TakeVal(m_cnt, ind.Sex, true);

					int v_mage = GKUtils.GetMarriageAge(ind);
					stats.mage.TakeVal(v_mage, ind.Sex, true);

					float v_ci = ind.GetCertaintyAssessment();
					stats.cIndex.TakeVal(v_ci, ind.Sex, false);
				}
			}
			
			return stats;
		}

		private static void CheckVal(List<StatsItem> valsList, string val)
		{
			if (val == "-1" || val == "" || val == "0") {
				val = "?";
			}

			int vIdx = valsList.FindIndex(delegate(StatsItem lv) { return (lv.Caption == val); });

			if (vIdx == -1) {
				valsList.Add(new StatsItem(val, 1));
			} else {
				StatsItem lv = valsList[vIdx];
				lv.Value = lv.Value + 1;
				valsList[vIdx] = lv;
			}
		}

		private static void GetSimplePersonStat(StatsMode mode, List<StatsItem> values, GEDCOMIndividualRecord iRec)
		{
			string iName = iRec.GetNameString(true, false);

			switch (mode)
			{
				case StatsMode.smAncestors:
						values.Add(new StatsItem(iName, GKUtils.GetAncestorsCount(iRec) - 1));
						break;

				case StatsMode.smDescendants:
						values.Add(new StatsItem(iName, GKUtils.GetDescendantsCount(iRec) - 1));
						break;

				case StatsMode.smDescGenerations:
						values.Add(new StatsItem(iName, GKUtils.GetDescGenerations(iRec)));
						break;

				case StatsMode.smChildsCount:
						values.Add(new StatsItem(iName, iRec.GetTotalChildsCount()));
						break;

				case StatsMode.smFirstbornAge:
						GEDCOMIndividualRecord iDummy;
						values.Add(new StatsItem(iName, GKUtils.GetFirstbornAge(iRec, out iDummy)));
						break;

				case StatsMode.smMarriages:
						values.Add(new StatsItem(iName, GKUtils.GetMarriagesCount(iRec)));
						break;

				case StatsMode.smMarriageAge:
						values.Add(new StatsItem(iName, GKUtils.GetMarriageAge(iRec)));
						break;

				case StatsMode.smFamilies:
				case StatsMode.smNames:
				case StatsMode.smPatronymics:
					{
						string V = "";
						string fam, nam, pat;
						iRec.GetNameParts(out fam, out nam, out pat);
						switch (mode) {
							case StatsMode.smFamilies:
								V = GKUtils.PrepareRusSurname(fam, iRec.Sex == GEDCOMSex.svFemale);
								break;
							case StatsMode.smNames:
								V = nam;
								break;
							case StatsMode.smPatronymics:
								V = pat;
								break;
						}
						CheckVal(values, V);
						break;
					}

                case StatsMode.smAge:
						CheckVal(values, GKUtils.GetAge(iRec, -1));
						break;

				case StatsMode.smLifeExpectancy:
						CheckVal(values, GKUtils.GetLifeExpectancy(iRec));
						break;

				case StatsMode.smBirthYears:
				case StatsMode.smBirthTenYears:
				case StatsMode.smDeathYears:
				case StatsMode.smDeathTenYears:
				case StatsMode.smBirthPlaces:
				case StatsMode.smDeathPlaces:
					{
						string V = "?";

						int num2 = iRec.Events.Count;
						for (int j = 0; j < num2; j++)
						{
							GEDCOMCustomEvent evt = iRec.Events[j];

							int year;
							ushort k, d;
							evt.Detail.Date.GetIndependentDate(out year, out k, out d);
							if (Math.Abs(year) > 3000)
							{
								GKUtils.ShowMessage(evt.Detail.Date.StringValue + "/" + iName);
							}

							if (evt.Name == "BIRT")
							{
								switch (mode) {
									case StatsMode.smBirthYears:
										V = Convert.ToString(year);
										break;
									case StatsMode.smBirthTenYears:
										V = Convert.ToString(year / 10 * 10);
										break;
									case StatsMode.smBirthPlaces:
										V = evt.Detail.Place.StringValue;
										break;
								}
							}
							else
							{
								if (evt.Name == "DEAT")
								{
									switch (mode) {
										case StatsMode.smDeathYears:
											V = Convert.ToString(year);
											break;
										case StatsMode.smDeathTenYears:
											V = Convert.ToString(year / 10 * 10);
											break;
										case StatsMode.smDeathPlaces:
											V = evt.Detail.Place.StringValue;
											break;
									}
								}
							}
						}
						CheckVal(values, V);
						break;
					}

				case StatsMode.smChildsDistribution:
						CheckVal(values, iRec.GetTotalChildsCount().ToString());
						break;

				case StatsMode.smResidences:
						CheckVal(values, GKUtils.GetResidencePlace(iRec, false));
						break;

				case StatsMode.smOccupation:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "OCCU"));
						break;

				case StatsMode.smReligious:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "RELI"));
						break;

				case StatsMode.smNational:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "NATI"));
						break;

				case StatsMode.smEducation:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "EDUC"));
						break;

				case StatsMode.smCaste:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "CAST"));
						break;

				case StatsMode.smHobby:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_HOBBY"));
						break;

				case StatsMode.smAward:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_AWARD"));
						break;

				case StatsMode.smMili:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI"));
						break;

				case StatsMode.smMiliInd:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI_IND"));
						break;

				case StatsMode.smMiliDis:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI_DIS"));
						break;

				case StatsMode.smMiliRank:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI_RANK"));
						break;

				case StatsMode.smCertaintyIndex:
						CheckVal(values, string.Format("{0:0.00}", iRec.GetCertaintyAssessment()));
						break;

				case StatsMode.smBirthByMonth:
						int month = GKUtils.GetIndependentMonth(iRec, "BIRT");
						if (month > 0) CheckVal(values, month.ToString());
						break;
			}
		}

		public void GetSpecStats(StatsMode mode, List<StatsItem> values)
		{
            if (values == null) {
                throw new ArgumentNullException("values");
            }

            if (mode < StatsMode.smDescGenerations)
			{
				GKUtils.InitExtCounts(this.fTree, -1);
			}

			try
			{
				// спецбуферы для сложных расчетов по усредненным возрастам
				Dictionary<string, List<int>> xvals = new Dictionary<string, List<int>>();

				int num = this.fTree.RecordsCount;
				for (int i = 0; i < num; i++)
				{
					GEDCOMRecord rec = this.fTree[i];

					if (rec is GEDCOMIndividualRecord && mode != StatsMode.smSpousesDiff && this.fSelectedRecords.Contains(rec))
					{
						GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
						
						if (mode != StatsMode.smAAF_1 && mode != StatsMode.smAAF_2)
						{
							GetSimplePersonStat(mode, values, iRec);
						}
						else
						{
							GEDCOMIndividualRecord iChild;
							int fba = GKUtils.GetFirstbornAge(iRec, out iChild);
							if (fba > 0) {
								string key;
								List<int> valsList = null;

								switch (mode) {
									case StatsMode.smAAF_1:
										key = SysUtils.Trunc(GKUtils.GetIndependentYear(iRec, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out valsList))
										{
											valsList = new List<int>();
											xvals.Add(key, valsList);
										}
										valsList.Add(fba);

										break;

									case StatsMode.smAAF_2:
										key = SysUtils.Trunc(GKUtils.GetIndependentYear(iChild, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out valsList))
										{
											valsList = new List<int>();
											xvals.Add(key, valsList);
										}
										valsList.Add(fba);

										break;
								}
							}
						}
					}
					else
					{
						if (rec is GEDCOMFamilyRecord && mode == StatsMode.smSpousesDiff)
						{
							GEDCOMFamilyRecord fRec = rec as GEDCOMFamilyRecord;
							values.Add(new StatsItem(GKUtils.GetFamilyString(fRec), GKUtils.GetSpousesDiff(fRec)));
						}
					}
				}
				
				if (mode == StatsMode.smAAF_1 || mode == StatsMode.smAAF_2)
				{
					foreach (KeyValuePair<string, List<int>> kvp in xvals)
					{
						List<int> vals_list = kvp.Value;

						int avg;
						if (vals_list.Count == 0) {
							avg = 0;
						} else {
							int sum = 0;
							int num2 = vals_list.Count;
							for (int i = 0; i < num2; i++) sum += vals_list[i];

							avg = (int)Math.Round((double)(sum / vals_list.Count));
						}

						values.Add(new StatsItem(kvp.Key, avg));
					}
				}
			}
			finally
			{
			}
		}
	}
}
