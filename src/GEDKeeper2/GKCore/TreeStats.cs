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
			smAAF_2
		}

		private readonly GEDCOMTree fTree;
		
		public TreeStats(GEDCOMTree tree)
		{
			this.fTree = tree;
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

		public void GetCommonStats(out CommonStats aStats)
		{
			aStats.persons = 0;
			aStats.persons_m = 0;
			aStats.persons_f = 0;
			aStats.lives = 0;
			aStats.lives_m = 0;
			aStats.lives_f = 0;
			aStats.age = 0;
			aStats.age_m = 0;
			aStats.age_f = 0;
			aStats.age_cnt = 0;
			aStats.age_m_cnt = 0;
			aStats.age_f_cnt = 0;
			aStats.life = 0;
			aStats.life_m = 0;
			aStats.life_f = 0;
			aStats.life_cnt = 0;
			aStats.life_m_cnt = 0;
			aStats.life_f_cnt = 0;
			aStats.childs = 0;
			aStats.childs_m = 0;
			aStats.childs_f = 0;
			aStats.childs_cnt = 0;
			aStats.childs_m_cnt = 0;
			aStats.childs_f_cnt = 0;
			aStats.fba = 0;
			aStats.fba_m = 0;
			aStats.fba_f = 0;
			aStats.fba_cnt = 0;
			aStats.fba_m_cnt = 0;
			aStats.fba_f_cnt = 0;
			aStats.marr = 0;
			aStats.marr_m = 0;
			aStats.marr_f = 0;
			aStats.marr_cnt = 0;
			aStats.marr_m_cnt = 0;
			aStats.marr_f_cnt = 0;
			aStats.mage = 0;
			aStats.mage_m = 0;
			aStats.mage_f = 0;
			aStats.mage_cnt = 0;
			aStats.mage_m_cnt = 0;
			aStats.mage_f_cnt = 0;

			int num = this.fTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMRecord rec = this.fTree[i];
				if (rec is GEDCOMIndividualRecord)
				{
					GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
					aStats.persons++;

					switch (ind.Sex) {
						case GEDCOMSex.svFemale:
						{
							aStats.persons_f++;
							if (ind.IsLive())
							{
								aStats.lives_f++;
								aStats.lives++;
							}
							break;
						}
						case GEDCOMSex.svMale:
						{
							aStats.persons_m++;
							if (ind.IsLive())
							{
								aStats.lives_m++;
								aStats.lives++;
							}
							break;
						}
					}

					string v_age = GKUtils.GetAge(ind, -1);
					TakeVal(v_age, ind.Sex, ref aStats.age, ref aStats.age_cnt, 
					        ref aStats.age_f, ref aStats.age_f_cnt, ref aStats.age_m, ref aStats.age_m_cnt);

					string v_life = GKUtils.GetLifeExpectancy(ind);
					TakeVal(v_life, ind.Sex, ref aStats.life, ref aStats.life_cnt,
					        ref aStats.life_f, ref aStats.life_f_cnt, ref aStats.life_m, ref aStats.life_m_cnt);

					int ch_cnt = ind.aux_GetChildsCount();
					TakeVal(ch_cnt, ind.Sex, ref aStats.childs, ref aStats.childs_cnt,
					        ref aStats.childs_f, ref aStats.childs_f_cnt, ref aStats.childs_m, ref aStats.childs_m_cnt);

					GEDCOMIndividualRecord iDummy;
					int v_fba = TreeStats.GetFirstbornAge(ind, out iDummy);
					TakeVal(v_fba, ind.Sex, ref aStats.fba, ref aStats.fba_cnt,
					        ref aStats.fba_f, ref aStats.fba_f_cnt, ref aStats.fba_m, ref aStats.fba_m_cnt);

					int m_cnt = TreeStats.GetMarriagesCount(ind);
					TakeVal(m_cnt, ind.Sex, ref aStats.marr, ref aStats.marr_cnt,
					        ref aStats.marr_f, ref aStats.marr_f_cnt, ref aStats.marr_m, ref aStats.marr_m_cnt);

					int v_mage = TreeStats.GetMarriageAge(ind);
					TakeVal(v_mage, ind.Sex, ref aStats.mage, ref aStats.mage_cnt,
					        ref aStats.mage_f, ref aStats.mage_f_cnt, ref aStats.mage_m, ref aStats.mage_m_cnt);
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

		private static void GetSimplePersonStat(TStatMode aMode, List<TListVal> aVals, GEDCOMIndividualRecord iRec)
		{
			string iName = iRec.aux_GetNameStr(true, false);

			switch (aMode)
			{
				case TStatMode.smAncestors:
						aVals.Add(new TListVal(iName, TreeStats.GetAncestorsCount(iRec) - 1));
						break;

				case TStatMode.smDescendants:
						aVals.Add(new TListVal(iName, TreeStats.GetDescendantsCount(iRec) - 1));
						break;

				case TStatMode.smDescGenerations:
						aVals.Add(new TListVal(iName, TreeStats.GetDescGenerations(iRec)));
						break;

				case TStatMode.smChildsCount:
						aVals.Add(new TListVal(iName, iRec.aux_GetChildsCount()));
						break;

				case TStatMode.smFirstbornAge:
						GEDCOMIndividualRecord iDummy;
						aVals.Add(new TListVal(iName, TreeStats.GetFirstbornAge(iRec, out iDummy)));
						break;

				case TStatMode.smMarriages:
						aVals.Add(new TListVal(iName, TreeStats.GetMarriagesCount(iRec)));
						break;

				case TStatMode.smMarriageAge:
						aVals.Add(new TListVal(iName, TreeStats.GetMarriageAge(iRec)));
						break;

				case TStatMode.smFamilies:
				case TStatMode.smNames:
				case TStatMode.smPatronymics:
					{
						string V = "";
						string fam, nam, pat;
						iRec.aux_GetNameParts(out fam, out nam, out pat);
						switch (aMode) {
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
						CheckVal(aVals, V);
						break;
					}

                case TStatMode.smAge:
						CheckVal(aVals, GKUtils.GetAge(iRec, -1));
						break;

				case TStatMode.smLifeExpectancy:
						CheckVal(aVals, GKUtils.GetLifeExpectancy(iRec));
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
								switch (aMode) {
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
									switch (aMode) {
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
						CheckVal(aVals, V);
						break;
					}

				case TStatMode.smChildsDistribution:
						CheckVal(aVals, iRec.aux_GetChildsCount().ToString());
						break;

				case TStatMode.smResidences:
						CheckVal(aVals, GKUtils.GetResidencePlace(iRec, false));
						break;

				case TStatMode.smOccupation:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "OCCU"));
						break;

				case TStatMode.smReligious:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "RELI"));
						break;

				case TStatMode.smNational:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "NATI"));
						break;

				case TStatMode.smEducation:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "EDUC"));
						break;

				case TStatMode.smCaste:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "CAST"));
						break;

				case TStatMode.smHobby:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "_HOBBY"));
						break;

				case TStatMode.smAward:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "_AWARD"));
						break;

				case TStatMode.smMili:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "_MILI"));
						break;

				case TStatMode.smMiliInd:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "_MILI_IND"));
						break;

				case TStatMode.smMiliDis:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "_MILI_DIS"));
						break;

				case TStatMode.smMiliRank:
						CheckVal(aVals, GKUtils.GetAttributeValue(iRec, "_MILI_RANK"));
						break;
			}
		}

		public void GetSpecStats(TStatMode aMode, List<TListVal> aVals)
		{
			if (aMode < TStatMode.smDescGenerations)
			{
				TreeStats.InitExtCounts(this.fTree, -1);
			}

			try
			{
				// спецбуферы для сложных расчетов по усредненным возрастам
				Dictionary<string, List<int>> xvals = new Dictionary<string, List<int>>();

				int num = this.fTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = this.fTree[i];

					if (rec is GEDCOMIndividualRecord && aMode != TStatMode.smSpousesDiff)
					{
						GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
						
						if (aMode != TStatMode.smAAF_1 && aMode != TStatMode.smAAF_2)
						{
							GetSimplePersonStat(aMode, aVals, iRec);
						}
						else
						{
							GEDCOMIndividualRecord iChild;
							int fba = TreeStats.GetFirstbornAge(iRec, out iChild);
							if (fba > 0) {
								string key;
								List<int> valsList = null;

								switch (aMode) {
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
						if (rec is GEDCOMFamilyRecord && aMode == TStatMode.smSpousesDiff)
						{
							GEDCOMFamilyRecord fRec = rec as GEDCOMFamilyRecord;
							aVals.Add(new TListVal(GKUtils.aux_GetFamilyStr(fRec), TreeStats.GetSpousesDiff(fRec)));
						}
					}
				}
				
				if (aMode == TStatMode.smAAF_1 || aMode == TStatMode.smAAF_2)
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
						aVals.Add(new TListVal(kvp.Key, avg));
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
						GEDCOMFamilyEvent fEvent = family.aux_GetFamilyEvent("MARR");
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
