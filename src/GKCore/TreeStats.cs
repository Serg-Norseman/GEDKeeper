using System;
using System.Collections.Generic;

using Ext.Utils;
using GedCom551;

namespace GKCore
{
	public class TreeStats
	{
		public struct TCommonStats
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

			public TValsItem(string aCaption, int aValue)
			{
				this.Caption = aCaption;
				this.Value = aValue;
			}

			public override string ToString()
			{
				return this.Caption;
			}
		}

		public enum TStatMode : byte
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

		private TGenEngine FEngine;
		private TGEDCOMTree FTree;
		
		public TreeStats(TGenEngine engine)
		{
			this.FEngine = engine;
			this.FTree = engine.Tree;
		}

		private void TakeVal(int val, TGEDCOMSex sex,
		                     ref int com_sum, ref int com_count,
		                     ref int f_sum, ref int f_count,
		                     ref int m_sum, ref int m_count)
		{
			if (val == 0) return;

			com_sum += val;
			com_count++;
			
			switch (sex) {
				case TGEDCOMSex.svFemale:
					f_sum += val;
					f_count++;
					break;
				case TGEDCOMSex.svMale:
					m_sum += val;
					m_count++;
					break;
			}
		}

		private void TakeVal(string val, TGEDCOMSex sex,
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

		public void GetCommonStats(out TCommonStats aStats)
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

			int num = this.FTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.FTree[i];
				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
					aStats.persons++;

					switch (ind.Sex) {
						case TGEDCOMSex.svFemale:
						{
							aStats.persons_f++;
							if (ind.IsLive())
							{
								aStats.lives_f++;
								aStats.lives++;
							}
							break;
						}
						case TGEDCOMSex.svMale:
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

					string v_age = TGenEngine.GetAge(ind, -1);
					TakeVal(v_age, ind.Sex, ref aStats.age, ref aStats.age_cnt, 
					        ref aStats.age_f, ref aStats.age_f_cnt, ref aStats.age_m, ref aStats.age_m_cnt);

					string v_life = TGenEngine.GetLifeExpectancy(ind);
					TakeVal(v_life, ind.Sex, ref aStats.life, ref aStats.life_cnt,
					        ref aStats.life_f, ref aStats.life_f_cnt, ref aStats.life_m, ref aStats.life_m_cnt);

					int ch_cnt = TreeStats.GetChildsCount(ind);
					TakeVal(ch_cnt, ind.Sex, ref aStats.childs, ref aStats.childs_cnt,
					        ref aStats.childs_f, ref aStats.childs_f_cnt, ref aStats.childs_m, ref aStats.childs_m_cnt);

					TGEDCOMIndividualRecord iDummy;
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

			public TListVal(string aItem, int aCount)
			{
				this.Item = aItem;
				this.Count = aCount;
			}
		}

		private void CheckVal(List<TListVal> aVals, string V)
		{
			if (V == "-1" || V == "" || V == "0")
			{
				V = "?";
			}

			int v_idx = aVals.FindIndex(delegate(TListVal lv) { return (lv.Item == V); });

			if (v_idx == -1) {
				aVals.Add(new TListVal(V, 1));
			} else {
				TListVal lv = aVals[v_idx];
				lv.Count = lv.Count + 1;
				aVals[v_idx] = lv;
			}
		}

		private void GetSimplePersonStat(TStatMode aMode, List<TListVal> aVals, TGEDCOMIndividualRecord iRec)
		{
			string iName = iRec.aux_GetNameStr(true, false);

			switch (aMode)
			{
				case TStatMode.smAncestors:
					{
						aVals.Add(new TListVal(iName, TreeStats.GetAncestorsCount(iRec) - 1));
						break;
					}
				case TStatMode.smDescendants:
					{
						aVals.Add(new TListVal(iName, TreeStats.GetDescendantsCount(iRec) - 1));
						break;
					}
				case TStatMode.smDescGenerations:
					{
						aVals.Add(new TListVal(iName, TreeStats.GetDescGenerations(iRec)));
						break;
					}
				case TStatMode.smChildsCount:
					{
						aVals.Add(new TListVal(iName, TreeStats.GetChildsCount(iRec)));
						break;
					}
				case TStatMode.smFirstbornAge:
					{
						TGEDCOMIndividualRecord iDummy;
						aVals.Add(new TListVal(iName, TreeStats.GetFirstbornAge(iRec, out iDummy)));
						break;
					}
				case TStatMode.smMarriages:
					{
						aVals.Add(new TListVal(iName, TreeStats.GetMarriagesCount(iRec)));
						break;
					}
				case TStatMode.smMarriageAge:
					{
						aVals.Add(new TListVal(iName, TreeStats.GetMarriageAge(iRec)));
						break;
					}

				case TStatMode.smFamilies:
				case TStatMode.smNames:
				case TStatMode.smPatronymics:
					{
						string V = "";
						string fam, nam, pat;
						iRec.aux_GetNameParts(out fam, out nam, out pat);
						switch (aMode) {
							case TStatMode.smFamilies:
								V = TGenEngine.PrepareRusSurname(fam, iRec.Sex == TGEDCOMSex.svFemale);
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
					{
						CheckVal(aVals, TGenEngine.GetAge(iRec, -1));
						break;
					}
				case TStatMode.smLifeExpectancy:
					{
						CheckVal(aVals, TGenEngine.GetLifeExpectancy(iRec));
						break;
					}

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
							TGEDCOMCustomEvent evt = iRec.IndividualEvents[j];
							int year;
							ushort k, d;
							evt.Detail.Date.aux_GetIndependentDate(out year, out k, out d);
							if (Math.Abs(year) > 3000)
							{
								TGenEngine.ShowMessage(evt.Detail.Date.StringValue + "/" + iName);
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
					{
						CheckVal(aVals, TreeStats.GetChildsCount(iRec).ToString());
						break;
					}
				case TStatMode.smResidences:
					{
						CheckVal(aVals, TGenEngine.GetResidencePlace(iRec, false));
						break;
					}
				case TStatMode.smOccupation:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "OCCU"));
						break;
					}
				case TStatMode.smReligious:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "RELI"));
						break;
					}
				case TStatMode.smNational:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "NATI"));
						break;
					}
				case TStatMode.smEducation:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "EDUC"));
						break;
					}
				case TStatMode.smCaste:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "CAST"));
						break;
					}
				case TStatMode.smHobby:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_HOBBY"));
						break;
					}
				case TStatMode.smAward:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_AWARD"));
						break;
					}
				case TStatMode.smMili:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI"));
						break;
					}
				case TStatMode.smMiliInd:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI_IND"));
						break;
					}
				case TStatMode.smMiliDis:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI_DIS"));
						break;
					}
				case TStatMode.smMiliRank:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI_RANK"));
						break;
					}
			}
		}

		public void GetSpecStats(TStatMode aMode, List<TListVal> aVals)
		{
			if (aMode < TStatMode.smDescGenerations)
			{
				TreeStats.InitExtCounts(this.FTree, -1);
			}

			try
			{
				// спецбуферы для сложных расчетов по усредненным возрастам
				Dictionary<string, List<int>> xvals = new Dictionary<string, List<int>>();

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];

					if (rec is TGEDCOMIndividualRecord && aMode != TStatMode.smSpousesDiff)
					{
						TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
						
						if (aMode != TStatMode.smAAF_1 && aMode != TStatMode.smAAF_2)
						{
							GetSimplePersonStat(aMode, aVals, iRec);
						}
						else
						{
							TGEDCOMIndividualRecord iChild;
							int fba = TreeStats.GetFirstbornAge(iRec, out iChild);
							if (fba > 0) {
								string key;
								List<int> vals_list = null;

								switch (aMode) {
									case TStatMode.smAAF_1:
										key = SysUtils.Trunc(TGenEngine.GetIndependentYear(iRec, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out vals_list))
										{
											vals_list = new List<int>();
											xvals.Add(key, vals_list);
										}
										vals_list.Add(fba);

										break;

									case TStatMode.smAAF_2:
										key = SysUtils.Trunc(TGenEngine.GetIndependentYear(iChild, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out vals_list))
										{
											vals_list = new List<int>();
											xvals.Add(key, vals_list);
										}
										vals_list.Add(fba);

										break;
								}
							}
						}
					}
					else
					{
						if (rec is TGEDCOMFamilyRecord && aMode == TStatMode.smSpousesDiff)
						{
							TGEDCOMFamilyRecord fRec = rec as TGEDCOMFamilyRecord;
							aVals.Add(new TListVal(TGenEngine.aux_GetFamilyStr(fRec), TreeStats.GetSpousesDiff(fRec)));
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

		public static void InitExtCounts(TGEDCOMTree tree, int value)
		{
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = tree[i];

				if (rec is TGEDCOMIndividualRecord)
				{
					rec.ExtData = value;
				}
			}
		}

		public static int GetAncestorsCount(TGEDCOMIndividualRecord iRec)
		{
			int Result = 0;

			if (iRec != null)
			{
				int val = (int)iRec.ExtData;

				if (val < 0)
				{
					val = 1;
					if (iRec.ChildToFamilyLinks.Count > 0)
					{
						TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
						TGEDCOMIndividualRecord anc;

						anc = family.Husband.Value as TGEDCOMIndividualRecord;
						val += TreeStats.GetAncestorsCount(anc);

						anc = (family.Wife.Value as TGEDCOMIndividualRecord);
						val += TreeStats.GetAncestorsCount(anc);
					}

					iRec.ExtData = val;
				}

				Result = val;
			}

			return Result;
		}

		public static int GetDescendantsCount(TGEDCOMIndividualRecord iRec)
		{
			int Result = 0;

			if (iRec != null)
			{
				int val = (int)iRec.ExtData;
				if (val < 0)
				{
					val = 1;

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMIndividualRecord iChild = family.Childrens[j].Value as TGEDCOMIndividualRecord;
							val += TreeStats.GetDescendantsCount(iChild);
						}
					}
					iRec.ExtData = val;
				}
				Result = val;
			}

			return Result;
		}

		private static int GetDescGens_Recursive(TGEDCOMIndividualRecord iRec)
		{
			int Result = 0;

			if (iRec != null)
			{
				int max = 0;

				int num = iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMIndividualRecord iChild = family.Childrens[j].Value as TGEDCOMIndividualRecord;
						int res = TreeStats.GetDescGens_Recursive(iChild);
						if (max < res)
						{
							max = res;
						}
					}
				}
				Result = 1 + max;
			}

			return Result;
		}

		public static int GetDescGenerations(TGEDCOMIndividualRecord iRec)
		{
			return TreeStats.GetDescGens_Recursive(iRec) - 1;
		}

		public static int GetMarriagesCount(TGEDCOMIndividualRecord iRec)
		{
			int Result = ((iRec == null) ? 0 : iRec.SpouseToFamilyLinks.Count);
			return Result;
		}

		public static int GetSpousesDiff(TGEDCOMFamilyRecord fRec)
		{
			int Result = 0;
			try
			{
				TGEDCOMIndividualRecord h = fRec.Husband.Value as TGEDCOMIndividualRecord;
				TGEDCOMIndividualRecord w = fRec.Wife.Value as TGEDCOMIndividualRecord;

				if (h != null && w != null)
				{
					double y = -1.0;
					double y2 = -1.0;

					TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(h, "BIRT");
					if (evt != null) y = TGenEngine.GetAbstractDate(evt.Detail);

					evt = TGenEngine.GetIndividualEvent(w, "BIRT");
					if (evt != null) y2 = TGenEngine.GetAbstractDate(evt.Detail);

					if (y > (double)0f && y2 > (double)0f)
					{
						Result = (int)SysUtils.Trunc(Math.Abs(y2 - y));
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetSpousesDiff(): " + E.Message);
			}
			return Result;
		}

		public static int GetFirstbornAge(TGEDCOMIndividualRecord iRec, out TGEDCOMIndividualRecord iChild)
		{
			int Result = 0;
			iChild = null;
			try
			{
				double y2 = 0.0;

				TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
				if (evt != null)
				{
					double y3 = TGenEngine.GetAbstractDate(evt.Detail);

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
							evt = TGenEngine.GetIndividualEvent(child, "BIRT");
							if (evt != null)
							{
								double y2tmp = TGenEngine.GetAbstractDate(evt.Detail);
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

					if (y3 > (double)1f && y2 > (double)1f)
					{
						Result = (int)SysUtils.Trunc(y2 - y3);
					}
					else
					{
						iChild = null;
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetFirstbornAge(): " + E.Message);
			}
			return Result;
		}

		public static int GetMarriageAge(TGEDCOMIndividualRecord iRec)
		{
			int Result = 0;
			try
			{
				double y2 = 0.0;

				TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
				if (evt != null)
				{
					double y3 = TGenEngine.GetAbstractDate(evt.Detail);

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
						TGEDCOMFamilyEvent fEvent = family.aux_GetFamilyEvent("MARR");
						if (fEvent != null)
						{
							double y2tmp = TGenEngine.GetAbstractDate(fEvent.Detail);
							if (y2 == (double)0f)
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
					if (y3 > (double)1f && y2 > (double)1f)
					{
						Result = (int)SysUtils.Trunc(y2 - y3);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetMarriageAge(): " + E.Message);
			}
			return Result;
		}

		public static int GetChildsCount(TGEDCOMIndividualRecord iRec)
		{
			int Result = 0;

			if (iRec != null && iRec.SpouseToFamilyLinks.Count > 0)
			{
				int num = iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
					Result += family.Childrens.Count;
				}
			}

			return Result;
		}

	}
}
