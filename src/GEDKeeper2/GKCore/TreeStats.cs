using System;
using System.Collections.Generic;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;

namespace GKCore
{
	public class TreeStats
	{
		public struct CommonStats
		{
			public int persons;
			public int persons_m;
			public int persons_f;
			public int lives;
			public int lives_m;
			public int lives_f;
			public int age;
			public int age_m;
			public int age_f;
			public int age_cnt;
			public int age_m_cnt;
			public int age_f_cnt;
			public int life;
			public int life_m;
			public int life_f;
			public int life_cnt;
			public int life_m_cnt;
			public int life_f_cnt;
			public int childs;
			public int childs_m;
			public int childs_f;
			public int childs_cnt;
			public int childs_m_cnt;
			public int childs_f_cnt;
			public int fba;
			public int fba_m;
			public int fba_f;
			public int fba_cnt;
			public int fba_m_cnt;
			public int fba_f_cnt;
			public int marr;
			public int marr_m;
			public int marr_f;
			public int marr_cnt;
			public int marr_m_cnt;
			public int marr_f_cnt;
			public int mage;
			public int mage_m;
			public int mage_f;
			public int mage_cnt;
			public int mage_m_cnt;
			public int mage_f_cnt;

			public float ci;
			public float ci_m;
			public float ci_f;
			public int ci_cnt;
			public int ci_m_cnt;
			public int ci_f_cnt;
		}

		public class TValsItem
		{
			public string Caption;
			public int Value;

			public TValsItem(string caption, int value)
			{
				this.Caption = caption;
				this.Value = value;
			}

			public override string ToString()
			{
				return this.Caption;
			}
		}

		public enum TStatMode
		{
			smAncestors,
			smDescendants,
			smDescGenerations,
			smFamilies,
			smNames,
			smPatronymics,
			smAge,
			smLifeExpectancy,
			smBirthYears,
			smBirthTenYears,
			smDeathYears,
			smDeathTenYears,
			smChildsCount,
			smChildsDistribution,
			smBirthPlaces,
			smDeathPlaces,
			smResidences,
			smOccupation,
			smReligious,
			smNational,
			smEducation,
			smCaste,
			smFirstbornAge,
			smMarriages,
			smMarriageAge,
			smSpousesDiff,
			smHobby,
			smAward,
			smMili,
			smMiliInd,
			smMiliDis,
			smMiliRank,
			smAAF_1,
			smAAF_2,
			smCertaintyIndex,
			
			smLast = smCertaintyIndex
		}

		private readonly GEDCOMTree fTree;
		private readonly List<GEDCOMRecord> fSelectedRecords;
		
		public TreeStats(GEDCOMTree tree, List<GEDCOMRecord> selectedRecords)
		{
			this.fTree = tree;
			this.fSelectedRecords = selectedRecords;
		}

		private static void TakeVal(int val, GEDCOMSex sex,
		                     ref int com_sum, ref int com_count,
		                     ref int f_sum, ref int f_count,
		                     ref int m_sum, ref int m_count)
		{
			if (val == 0) return;

			com_sum += val;
			com_count++;
			
			switch (sex) {
				case GEDCOMSex.svFemale:
					f_sum += val;
					f_count++;
					break;
				case GEDCOMSex.svMale:
					m_sum += val;
					m_count++;
					break;
			}
		}

		private static void TakeVal(float val, GEDCOMSex sex,
		                     ref float com_sum, ref int com_count,
		                     ref float f_sum, ref int f_count,
		                     ref float m_sum, ref int m_count)
		{
			com_sum += val;
			com_count++;
			
			switch (sex) {
				case GEDCOMSex.svFemale:
					f_sum += val;
					f_count++;
					break;
				case GEDCOMSex.svMale:
					m_sum += val;
					m_count++;
					break;
			}
		}

		private static void TakeVal(string val, GEDCOMSex sex,
							 ref int com_sum, ref int com_count,
							 ref int f_sum, ref int f_count,
							 ref int m_sum, ref int m_count)
		{
			int tmp;
			if (int.TryParse(val, out tmp))
			{
				TakeVal(tmp, sex, ref com_sum, ref com_count, ref f_sum, ref f_count, ref m_sum, ref m_count);
			}
		}

		public void GetCommonStats(out CommonStats stats)
		{
			stats.persons = 0;
			stats.persons_m = 0;
			stats.persons_f = 0;
			stats.lives = 0;
			stats.lives_m = 0;
			stats.lives_f = 0;
			stats.age = 0;
			stats.age_m = 0;
			stats.age_f = 0;
			stats.age_cnt = 0;
			stats.age_m_cnt = 0;
			stats.age_f_cnt = 0;
			stats.life = 0;
			stats.life_m = 0;
			stats.life_f = 0;
			stats.life_cnt = 0;
			stats.life_m_cnt = 0;
			stats.life_f_cnt = 0;
			stats.childs = 0;
			stats.childs_m = 0;
			stats.childs_f = 0;
			stats.childs_cnt = 0;
			stats.childs_m_cnt = 0;
			stats.childs_f_cnt = 0;
			stats.fba = 0;
			stats.fba_m = 0;
			stats.fba_f = 0;
			stats.fba_cnt = 0;
			stats.fba_m_cnt = 0;
			stats.fba_f_cnt = 0;
			stats.marr = 0;
			stats.marr_m = 0;
			stats.marr_f = 0;
			stats.marr_cnt = 0;
			stats.marr_m_cnt = 0;
			stats.marr_f_cnt = 0;
			stats.mage = 0;
			stats.mage_m = 0;
			stats.mage_f = 0;
			stats.mage_cnt = 0;
			stats.mage_m_cnt = 0;
			stats.mage_f_cnt = 0;
			stats.ci = 0;
			stats.ci_m = 0;
			stats.ci_f = 0;
			stats.ci_cnt = 0;
			stats.ci_m_cnt = 0;
			stats.ci_f_cnt = 0;

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
						{
							stats.persons_f++;
							if (ind.IsLive()) {
								stats.lives_f++;
								stats.lives++;
							}
							break;
						}
						case GEDCOMSex.svMale:
						{
							stats.persons_m++;
							if (ind.IsLive()) {
								stats.lives_m++;
								stats.lives++;
							}
							break;
						}
					}

					string v_age = GKUtils.GetAge(ind, -1);
					TakeVal(v_age, ind.Sex, ref stats.age, ref stats.age_cnt, 
					        ref stats.age_f, ref stats.age_f_cnt, ref stats.age_m, ref stats.age_m_cnt);

					string v_life = GKUtils.GetLifeExpectancy(ind);
					TakeVal(v_life, ind.Sex, ref stats.life, ref stats.life_cnt,
					        ref stats.life_f, ref stats.life_f_cnt, ref stats.life_m, ref stats.life_m_cnt);

					int ch_cnt = ind.aux_GetChildsCount();
					TakeVal(ch_cnt, ind.Sex, ref stats.childs, ref stats.childs_cnt,
					        ref stats.childs_f, ref stats.childs_f_cnt, ref stats.childs_m, ref stats.childs_m_cnt);

					GEDCOMIndividualRecord iDummy;
					int v_fba = TreeStats.GetFirstbornAge(ind, out iDummy);
					TakeVal(v_fba, ind.Sex, ref stats.fba, ref stats.fba_cnt,
					        ref stats.fba_f, ref stats.fba_f_cnt, ref stats.fba_m, ref stats.fba_m_cnt);

					int m_cnt = TreeStats.GetMarriagesCount(ind);
					TakeVal(m_cnt, ind.Sex, ref stats.marr, ref stats.marr_cnt,
					        ref stats.marr_f, ref stats.marr_f_cnt, ref stats.marr_m, ref stats.marr_m_cnt);

					int v_mage = TreeStats.GetMarriageAge(ind);
					TakeVal(v_mage, ind.Sex, ref stats.mage, ref stats.mage_cnt,
					        ref stats.mage_f, ref stats.mage_f_cnt, ref stats.mage_m, ref stats.mage_m_cnt);

					float v_ci = ind.GetCertaintyAssessment();
					TakeVal(v_ci, ind.Sex, ref stats.ci, ref stats.ci_cnt,
					        ref stats.ci_f, ref stats.ci_f_cnt, ref stats.ci_m, ref stats.ci_m_cnt);
				}
			}
		}

		public struct TListVal
		{
			public string Item;
			public int Count;

			public TListVal(string item, int count)
			{
				this.Item = item;
				this.Count = count;
			}
		}

		private static void CheckVal(List<TListVal> aVals, string V)
		{
			if (V == "-1" || V == "" || V == "0") {
				V = "?";
			}

			int vIdx = aVals.FindIndex(delegate(TListVal lv) { return (lv.Item == V); });

			if (vIdx == -1) {
				aVals.Add(new TListVal(V, 1));
			} else {
				TListVal lv = aVals[vIdx];
				lv.Count = lv.Count + 1;
				aVals[vIdx] = lv;
			}
		}

		private static void GetSimplePersonStat(TStatMode mode, List<TListVal> values, GEDCOMIndividualRecord iRec)
		{
			string iName = iRec.aux_GetNameStr(true, false);

			switch (mode)
			{
				case TStatMode.smAncestors:
						values.Add(new TListVal(iName, TreeStats.GetAncestorsCount(iRec) - 1));
						break;

				case TStatMode.smDescendants:
						values.Add(new TListVal(iName, TreeStats.GetDescendantsCount(iRec) - 1));
						break;

				case TStatMode.smDescGenerations:
						values.Add(new TListVal(iName, TreeStats.GetDescGenerations(iRec)));
						break;

				case TStatMode.smChildsCount:
						values.Add(new TListVal(iName, iRec.aux_GetChildsCount()));
						break;

				case TStatMode.smFirstbornAge:
						GEDCOMIndividualRecord iDummy;
						values.Add(new TListVal(iName, TreeStats.GetFirstbornAge(iRec, out iDummy)));
						break;

				case TStatMode.smMarriages:
						values.Add(new TListVal(iName, TreeStats.GetMarriagesCount(iRec)));
						break;

				case TStatMode.smMarriageAge:
						values.Add(new TListVal(iName, TreeStats.GetMarriageAge(iRec)));
						break;

				case TStatMode.smFamilies:
				case TStatMode.smNames:
				case TStatMode.smPatronymics:
					{
						string V = "";
						string fam, nam, pat;
						iRec.GetNameParts(out fam, out nam, out pat);
						switch (mode) {
							case TStatMode.smFamilies:
								V = NamesTable.PrepareRusSurname(fam, iRec.Sex == GEDCOMSex.svFemale);
								break;
							case TStatMode.smNames:
								V = nam;
								break;
							case TStatMode.smPatronymics:
								V = pat;
								break;
						}
						CheckVal(values, V);
						break;
					}

                case TStatMode.smAge:
						CheckVal(values, GKUtils.GetAge(iRec, -1));
						break;

				case TStatMode.smLifeExpectancy:
						CheckVal(values, GKUtils.GetLifeExpectancy(iRec));
						break;

				case TStatMode.smBirthYears:
				case TStatMode.smBirthTenYears:
				case TStatMode.smDeathYears:
				case TStatMode.smDeathTenYears:
				case TStatMode.smBirthPlaces:
				case TStatMode.smDeathPlaces:
					{
						string V = "?";
						int num2 = iRec.IndividualEvents.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							GEDCOMCustomEvent evt = iRec.IndividualEvents[j];
							int year;
							ushort k, d;
							evt.Detail.Date.aux_GetIndependentDate(out year, out k, out d);
							if (Math.Abs(year) > 3000)
							{
								GKUtils.ShowMessage(evt.Detail.Date.StringValue + "/" + iName);
							}
							if (evt.Name == "BIRT")
							{
								switch (mode) {
									case TStatMode.smBirthYears:
										V = Convert.ToString(year);
										break;
									case TStatMode.smBirthTenYears:
										V = Convert.ToString(year / 10 * 10);
										break;
									case TStatMode.smBirthPlaces:
										V = evt.Detail.Place.StringValue;
										break;
								}
							}
							else
							{
								if (evt.Name == "DEAT")
								{
									switch (mode) {
										case TStatMode.smDeathYears:
											V = Convert.ToString(year);
											break;
										case TStatMode.smDeathTenYears:
											V = Convert.ToString(year / 10 * 10);
											break;
										case TStatMode.smDeathPlaces:
											V = evt.Detail.Place.StringValue;
											break;
									}
								}
							}
						}
						CheckVal(values, V);
						break;
					}

				case TStatMode.smChildsDistribution:
						CheckVal(values, iRec.aux_GetChildsCount().ToString());
						break;

				case TStatMode.smResidences:
						CheckVal(values, GKUtils.GetResidencePlace(iRec, false));
						break;

				case TStatMode.smOccupation:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "OCCU"));
						break;

				case TStatMode.smReligious:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "RELI"));
						break;

				case TStatMode.smNational:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "NATI"));
						break;

				case TStatMode.smEducation:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "EDUC"));
						break;

				case TStatMode.smCaste:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "CAST"));
						break;

				case TStatMode.smHobby:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_HOBBY"));
						break;

				case TStatMode.smAward:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_AWARD"));
						break;

				case TStatMode.smMili:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI"));
						break;

				case TStatMode.smMiliInd:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI_IND"));
						break;

				case TStatMode.smMiliDis:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI_DIS"));
						break;

				case TStatMode.smMiliRank:
						CheckVal(values, GKUtils.GetAttributeValue(iRec, "_MILI_RANK"));
						break;

				case TStatMode.smCertaintyIndex:
						CheckVal(values, string.Format("{0:0.00}", iRec.GetCertaintyAssessment()));
						break;
			}
		}

		public void GetSpecStats(TStatMode mode, List<TListVal> values)
		{
			if (mode < TStatMode.smDescGenerations)
			{
				TreeStats.InitExtCounts(this.fTree, -1);
			}

			try
			{
				// спецбуферы для сложных расчетов по усредненным возрастам
				Dictionary<string, List<int>> xvals = new Dictionary<string, List<int>>();

				int num = this.fTree.RecordsCount;
				for (int i = 0; i < num; i++)
				{
					GEDCOMRecord rec = this.fTree[i];

					if (rec is GEDCOMIndividualRecord && mode != TStatMode.smSpousesDiff && this.fSelectedRecords.Contains(rec))
					{
						GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
						
						if (mode != TStatMode.smAAF_1 && mode != TStatMode.smAAF_2)
						{
							GetSimplePersonStat(mode, values, iRec);
						}
						else
						{
							GEDCOMIndividualRecord iChild;
							int fba = TreeStats.GetFirstbornAge(iRec, out iChild);
							if (fba > 0) {
								string key;
								List<int> valsList = null;

								switch (mode) {
									case TStatMode.smAAF_1:
										key = SysUtils.Trunc(GKUtils.GetIndependentYear(iRec, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out valsList))
										{
											valsList = new List<int>();
											xvals.Add(key, valsList);
										}
										valsList.Add(fba);

										break;

									case TStatMode.smAAF_2:
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
						if (rec is GEDCOMFamilyRecord && mode == TStatMode.smSpousesDiff)
						{
							GEDCOMFamilyRecord fRec = rec as GEDCOMFamilyRecord;
							values.Add(new TListVal(GKUtils.aux_GetFamilyStr(fRec), TreeStats.GetSpousesDiff(fRec)));
						}
					}
				}
				
				if (mode == TStatMode.smAAF_1 || mode == TStatMode.smAAF_2)
				{
					foreach (KeyValuePair<string, List<int>> kvp in xvals)
					{
						List<int> vals_list = kvp.Value;
						int avg;
						if (vals_list.Count == 0)
						{
							avg = 0;
						}
						else
						{
							int sum = 0;
							for (int i = 0; i <= vals_list.Count - 1; i++) sum += vals_list[i];
							avg = (int)Math.Round((double)(sum / vals_list.Count));
						}
						values.Add(new TListVal(kvp.Key, avg));
					}
				}
			}
			finally
			{
			}
		}

		public static void InitExtCounts(GEDCOMTree tree, int value)
		{
			for (int i = 0, count = tree.RecordsCount; i < count; i++) {
				GEDCOMRecord rec = tree[i];

				if (rec is GEDCOMIndividualRecord) {
					rec.ExtData = value;
				}
			}
		}

		public static void InitExtData(GEDCOMTree tree)
		{
			for (int i = 0, count = tree.RecordsCount; i < count; i++) {
				GEDCOMRecord rec = tree[i];
				rec.ExtData = null;
			}
		}

		public static int GetAncestorsCount(GEDCOMIndividualRecord iRec)
		{
			int result = 0;

			if (iRec != null)
			{
				int val = (int)iRec.ExtData;

				if (val < 0)
				{
					val = 1;
					if (iRec.ChildToFamilyLinks.Count > 0)
					{
						GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
						GEDCOMIndividualRecord anc;

						anc = family.Husband.Value as GEDCOMIndividualRecord;
						val += TreeStats.GetAncestorsCount(anc);

						anc = (family.Wife.Value as GEDCOMIndividualRecord);
						val += TreeStats.GetAncestorsCount(anc);
					}

					iRec.ExtData = val;
				}

				result = val;
			}

			return result;
		}

		public static int GetDescendantsCount(GEDCOMIndividualRecord iRec)
		{
			int result = 0;

			if (iRec != null)
			{
				int val = (int)iRec.ExtData;
				if (val < 0)
				{
					val = 1;

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							GEDCOMIndividualRecord iChild = family.Childrens[j].Value as GEDCOMIndividualRecord;
							val += TreeStats.GetDescendantsCount(iChild);
						}
					}
					iRec.ExtData = val;
				}
				result = val;
			}

			return result;
		}

		private static int GetDescGens_Recursive(GEDCOMIndividualRecord iRec)
		{
			int result = 0;

			if (iRec != null)
			{
				int max = 0;

				int num = iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						GEDCOMIndividualRecord iChild = family.Childrens[j].Value as GEDCOMIndividualRecord;
						int res = TreeStats.GetDescGens_Recursive(iChild);
						if (max < res)
						{
							max = res;
						}
					}
				}
				result = 1 + max;
			}

			return result;
		}

		public static int GetDescGenerations(GEDCOMIndividualRecord iRec)
		{
			return TreeStats.GetDescGens_Recursive(iRec) - 1;
		}

		public static int GetMarriagesCount(GEDCOMIndividualRecord iRec)
		{
			int result = ((iRec == null) ? 0 : iRec.SpouseToFamilyLinks.Count);
			return result;
		}

		public static int GetSpousesDiff(GEDCOMFamilyRecord fRec)
		{
			int result = 0;
            if (fRec == null) return result;

			try
			{
				GEDCOMIndividualRecord h = fRec.Husband.Value as GEDCOMIndividualRecord;
				GEDCOMIndividualRecord w = fRec.Wife.Value as GEDCOMIndividualRecord;

				if (h != null && w != null)
				{
					double y = -1.0;
					double y2 = -1.0;

					GEDCOMCustomEvent evt = h.GetIndividualEvent("BIRT");
					if (evt != null) y = GKUtils.GetAbstractDate(evt.Detail);

					evt = w.GetIndividualEvent("BIRT");
					if (evt != null) y2 = GKUtils.GetAbstractDate(evt.Detail);

					if (y > 0f && y2 > 0f)
					{
						result = (int)SysUtils.Trunc(Math.Abs(y2 - y));
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeStats.GetSpousesDiff(): " + ex.Message);
			}
			return result;
		}

		public static int GetFirstbornAge(GEDCOMIndividualRecord iRec, out GEDCOMIndividualRecord iChild)
		{
			int result = 0;
			iChild = null;
            if (iRec == null) return result;

			try
			{
				double y2 = 0.0;

				GEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
				if (evt != null)
				{
					double y3 = GKUtils.GetAbstractDate(evt.Detail);

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
							evt = child.GetIndividualEvent("BIRT");
							if (evt != null)
							{
								double y2tmp = GKUtils.GetAbstractDate(evt.Detail);
								if (y2 == (double)0f)
								{
									y2 = y2tmp;
									iChild = child;
								}
								else
								{
									if (y2 > y2tmp)
									{
										y2 = y2tmp;
										iChild = child;
									}
								}
							}
						}
					}

					if (y3 > 1.0f && y2 > 1.0f)
					{
						result = (int)SysUtils.Trunc(y2 - y3);
					}
					else
					{
						iChild = null;
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeStats.GetFirstbornAge(): " + ex.Message);
			}
			return result;
		}

		public static int GetMarriageAge(GEDCOMIndividualRecord iRec)
		{
			int result = 0;
            if (iRec == null) return result;

			try
			{
				double y2 = 0.0;

				GEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
				if (evt != null)
				{
					double y3 = GKUtils.GetAbstractDate(evt.Detail);

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
						GEDCOMFamilyEvent fEvent = family.GetFamilyEvent("MARR");
						if (fEvent != null)
						{
							double y2tmp = GKUtils.GetAbstractDate(fEvent.Detail);
							if (y2 == 0.0d)
							{
								y2 = y2tmp;
							}
							else
							{
								if (y2 > y2tmp)
								{
									y2 = y2tmp;
								}
							}
						}
					}
					if (y3 > 1f && y2 > 1f)
					{
						result = (int)SysUtils.Trunc(y2 - y3);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeStats.GetMarriageAge(): " + ex.Message);
			}
			return result;
		}

	}
}
