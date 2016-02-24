using System;
using System.Collections.Generic;
using GKCommon.GEDCOM;

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

					string vAge = GKUtils.GetAgeStr(ind, -1);
					stats.age.TakeVal(vAge, ind.Sex, true);

					string vLife = GKUtils.GetLifeExpectancyStr(ind);
					stats.life.TakeVal(vLife, ind.Sex, true);

					int chCnt = ind.GetTotalChildsCount();
					stats.childs.TakeVal(chCnt, ind.Sex, true);

					GEDCOMIndividualRecord iDummy;
					int vFba = GKUtils.GetFirstbornAge(ind, out iDummy);
					stats.fba.TakeVal(vFba, ind.Sex, true);

					int mCnt = GKUtils.GetMarriagesCount(ind);
					stats.marr.TakeVal(mCnt, ind.Sex, true);

					int vMAge = GKUtils.GetMarriageAge(ind);
					stats.mage.TakeVal(vMAge, ind.Sex, true);

					float vCI = ind.GetCertaintyAssessment();
					stats.cIndex.TakeVal(vCI, ind.Sex, false);
				}
			}
			
			return stats;
		}

		private static void CheckVal(List<StatsItem> valsList, string val, GEDCOMSex sex = GEDCOMSex.svUndetermined)
		{
			if (sex == GEDCOMSex.svUndetermined) {
				if (val == "-1" || val == "" || val == "0") {
					val = "?";
				}
			}

			int vIdx = valsList.FindIndex(delegate(StatsItem lv) { return (lv.Caption == val); });

			StatsItem lvi;
			if (vIdx == -1) {
				lvi = new StatsItem(val, sex != GEDCOMSex.svUndetermined);
				valsList.Add(lvi);
			} else {
				lvi = valsList[vIdx];
			}
			
			switch (sex)
			{
				case GEDCOMSex.svFemale:
					lvi.ValF = lvi.ValF + 1;
					break;

				case GEDCOMSex.svMale:
					lvi.ValM = lvi.ValM + 1;
					break;

				case GEDCOMSex.svUndetermined:
					lvi.Value = lvi.Value + 1;
					break;
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
						string v = "";
						string fam, nam, pat;
						iRec.GetNameParts(out fam, out nam, out pat);
						switch (mode) {
							case StatsMode.smFamilies:
								v = GKUtils.PrepareRusSurname(fam, iRec.Sex == GEDCOMSex.svFemale);
								break;
							case StatsMode.smNames:
								v = nam;
								break;
							case StatsMode.smPatronymics:
								v = pat;
								break;
						}
						CheckVal(values, v);
						break;
					}

                case StatsMode.smAge:
						CheckVal(values, GKUtils.GetAgeStr(iRec, -1));
						break;

				case StatsMode.smLifeExpectancy:
						CheckVal(values, GKUtils.GetLifeExpectancyStr(iRec));
						break;

				case StatsMode.smBirthYears:
				case StatsMode.smBirthTenYears:
				case StatsMode.smDeathYears:
				case StatsMode.smDeathTenYears:
				case StatsMode.smBirthPlaces:
				case StatsMode.smDeathPlaces:
					{
						string v = "?";

						int num2 = iRec.Events.Count;
						for (int j = 0; j < num2; j++)
						{
							GEDCOMCustomEvent evt = iRec.Events[j];
							AbsDate dtx = GEDCOMUtils.GetAbstractDate(evt);
							if (!dtx.IsValid() && (mode != StatsMode.smBirthPlaces && mode != StatsMode.smDeathPlaces)) continue;

							if (evt.Name == "BIRT")
							{
								switch (mode) {
									case StatsMode.smBirthYears:
										v = Convert.ToString(dtx.Year);
										break;
									case StatsMode.smBirthTenYears:
										v = Convert.ToString(dtx.Year / 10 * 10);
										break;
									case StatsMode.smBirthPlaces:
										v = evt.Detail.Place.StringValue;
										break;
								}
							}
							else
							{
								if (evt.Name == "DEAT")
								{
									switch (mode) {
										case StatsMode.smDeathYears:
											v = Convert.ToString(dtx.Year);
											break;
										case StatsMode.smDeathTenYears:
											v = Convert.ToString(dtx.Year / 10 * 10);
											break;
										case StatsMode.smDeathPlaces:
											v = evt.Detail.Place.StringValue;
											break;
									}
								}
							}
						}
						CheckVal(values, v);
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
						GEDCOMCustomEvent ev = iRec.FindEvent("BIRT");
						if (ev != null) {
							GEDCOMCustomDate dtx = ev.Detail.Date.Value;
							if (dtx != null) {
								int ay;
								ushort month, ad;
								bool ybc;
								dtx.GetDateParts(out ay, out month,out ad, out ybc);
								if (month > 0) CheckVal(values, month.ToString());
							}
						}
						break;

					case StatsMode.smDemography:
						{
							int lifeExp = GKUtils.GetLifeExpectancy(iRec);
							if (lifeExp > -1)
							{
								string v = Convert.ToString(lifeExp / 5 * 5);
								CheckVal(values, v, iRec.Sex);
							}
							break;
						}
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
								List<int> valsList;

								switch (mode) {
									case StatsMode.smAAF_1:
										AbsDate dtx1 = GEDCOMUtils.GetAbstractDate(iRec, "BIRT");
										if (dtx1.IsValid()) {
											key = GKUtils.Trunc(dtx1.Year / 10 * 10).ToString();

											if (!xvals.TryGetValue(key, out valsList))
											{
												valsList = new List<int>();
												xvals.Add(key, valsList);
											}
											valsList.Add(fba);
										}

										break;

									case StatsMode.smAAF_2:
										AbsDate dtx2 = GEDCOMUtils.GetAbstractDate(iChild, "BIRT");
										if (dtx2.IsValid()) {
											key = GKUtils.Trunc(dtx2.Year / 10 * 10).ToString();

											if (!xvals.TryGetValue(key, out valsList))
											{
												valsList = new List<int>();
												xvals.Add(key, valsList);
											}
											valsList.Add(fba);
										}

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

							int diff = GKUtils.GetSpousesDiff(fRec);
							if (diff != -1) {
								values.Add(new StatsItem(GKUtils.GetFamilyString(fRec), diff));
							}
						}
					}
				}
				
				if (mode == StatsMode.smAAF_1 || mode == StatsMode.smAAF_2)
				{
					foreach (KeyValuePair<string, List<int>> kvp in xvals)
					{
						List<int> valsList = kvp.Value;

						int avg;
						if (valsList.Count == 0) {
							avg = 0;
						} else {
							int sum = 0;
							int num2 = valsList.Count;
							for (int i = 0; i < num2; i++) sum += valsList[i];

							avg = (int)Math.Round((double)(sum / valsList.Count));
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
