using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public class MediaFileNotFoundException : Exception
	{
		
	}

	public enum TGEDCOMFormat : byte
	{
		gf_Unknown,
		gf_Native,
		gf_GENBOX,
		gf_ALTREE,
		gf_AGES,
		gf_PAF,
		
		gf_Last = gf_PAF
	}

	public enum TRecNotify : byte
	{
		rnDelete
	}

	public enum TGKStoreType : byte
	{
		gstReference,
		gstStorage,
		gstArchive
	}

	public class TGenEngine : IDisposable
	{
		public const string AppTitle = "GEDKeeper2";

		public struct SexStruct
		{
			public LSID NameId;
			public string Sign;

			public SexStruct(LSID aName, string aSign) {
				this.NameId = aName;
				this.Sign = aSign;
			}
		}

		public struct MarStatusStruct
		{
			public LSID Name;
			public string StatSign;
			
			public MarStatusStruct(LSID aName, string aStatSign) {
				this.Name = aName;
				this.StatSign = aStatSign;
			}
		}

		public struct S7
		{
			public LSID Name;
			public string Sign;
			public TPersonEventKind Kind;
			
			public S7(LSID aName, string aSign, TPersonEventKind aKind) {
				this.Name = aName;
				this.Sign = aSign;
				this.Kind = aKind;
			}
		}

		public struct DateKindRec
		{
			public LSID Name;
			public TDateControlsRange Dates;
			
			public DateKindRec(LSID aName, TDateControlsRange aDates) {
				this.Name = aName;
				this.Dates = aDates;
			}
		}

		public struct S21
		{
			public LSID Name;
			public string Sign;
			
			public S21(LSID aName, string aSign) {
				this.Name = aName;
				this.Sign = aSign;
			}
		}

		public struct StoreTypeRec
		{
			public LSID Name;
			public string Sign;
			
			public StoreTypeRec(LSID aName, string aSign) {
				this.Name = aName;
				this.Sign = aSign;
			}
		}

		public struct TGEDCOMAppFormat
		{
			public string Sign;
			public string Name;
			
			public TGEDCOMAppFormat(string aSign, string aName) {
				this.Sign = aSign;
				this.Name = aName;
			}
		}

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

		public enum TRecAction : byte
		{
			raAdd,
			raEdit,
			raDelete,
			raJump,
			raMoveUp,
			raMoveDown
		}

		public enum TTargetMode : byte
		{
			tmNone,
			tmParent,
			tmChild,
			tmChildToFamily
		}

		public enum TLifeMode : byte
		{
			lmAll,
			lmOnlyAlive,
			lmOnlyDead,
			lmAliveBefore,
			lmTimeLine
		}

		public enum TFamilyTarget : byte
		{
			ftNone,
			ftSpouse,
			ftChild
		}

		[Flags]
		public enum TDateControlsRange : byte
		{}

		public enum TShieldState : byte
		{
			ssMaximum,
			ssMiddle,
			ssNone
		}

		public enum TDateFormat : byte
		{
			dfDD_MM_YYYY,
			dfYYYY_MM_DD,
			dfYYYY
		}

		public enum TNameFormat : byte
		{
			nfFNP,
			nfF_NP,
			nfF_N_P
		}

		public enum TPersonEventKind : byte
		{
			ekEvent,
			ekFact
		}

		public enum TUserRef : byte
		{
			urCustom,
			urRI_StGeorgeCross,
			urUSSR_Soldier,
			urUSSR_FallInBattle,
			urUSSR_RearVeteran
		}

		public enum TChartPersonSign : byte
		{
			urRI_StGeorgeCross = 1,
			urUSSR_Soldier,
			urUSSR_FallInBattle,
			urUSSR_RearVeteran,
			
			urLast = urUSSR_RearVeteran
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

		public static readonly string[] Restrictions;
		public static readonly LSID[] RecordTypes;
		public static readonly SexStruct[] SexData;
		public static readonly MarStatusStruct[] MarriageStatus;
		public static readonly S7[] PersonEvents;
		public static readonly DateKindRec[] DateKinds;
		public static readonly LSID[] DateCalendars;
		public static readonly S21[] FamilyEvents;
		public static readonly StoreTypeRec[] GKStoreTypes;
		public static readonly LSID[] MediaTypes;
		public static readonly LSID[] PriorityNames;
		public static readonly LSID[] StatusNames;
		public static readonly LSID[] CommunicationNames;
		public static readonly LSID[] CommunicationDirs;
		public static readonly LSID[] GoalNames;
		public static readonly LSID[] CertaintyAssessments;
		public static readonly string[] UserRefs;
		public static readonly TGEDCOMAppFormat[] GEDCOMFormats;
		public static readonly LSID[] RelationKinds;
		public static readonly string[] RelationSigns;
		public static readonly string[] Numerals;
		public static readonly string[] NumKinship;
		private string FFileName;
		private TGEDCOMTree FTree;
		private bool Disposed_;



		public string FileName
		{
			get { return this.FFileName; }
			set { this.FFileName = value; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
		}

		private static int GetDescGens_Recursive(TGEDCOMIndividualRecord aPerson)
		{
			int Result = 0;
			if (aPerson != null)
			{
				int max = 0;

				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMIndividualRecord iChild = family.Childrens[j].Value as TGEDCOMIndividualRecord;
						int res = TGenEngine.GetDescGens_Recursive(iChild);
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

		public TGenEngine()
		{
			this.FTree = new TGEDCOMTree();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FTree.Dispose();
				this.Disposed_ = true;
			}
		}

		public void AddFamilySpouse(TGEDCOMFamilyRecord aFamily, TGEDCOMIndividualRecord aSpouse)
		{
			TGEDCOMSex sex = aSpouse.Sex;
			if (sex != TGEDCOMSex.svNone)
			{
				if (sex != TGEDCOMSex.svMale)
				{
					if (sex != TGEDCOMSex.svFemale)
					{
						if (sex == TGEDCOMSex.svUndetermined)
						{
							return;
						}
					}
					else
					{
						aFamily.Wife.Value = aSpouse;
					}
				}
				else
				{
					aFamily.Husband.Value = aSpouse;
				}
				TGEDCOMSpouseToFamilyLink spLink = new TGEDCOMSpouseToFamilyLink(this.FTree, aSpouse, "", "");
				spLink.Family = aFamily;
				aSpouse.SpouseToFamilyLinks.Add(spLink);
			}
		}

		public void RemoveFamilySpouse(TGEDCOMFamilyRecord aFamily, TGEDCOMIndividualRecord aSpouse)
		{
			if (aSpouse != null)
			{
				aSpouse.DeleteSpouseToFamilyLink(aFamily);
				TGEDCOMSex sex = aSpouse.Sex;
				if (sex != TGEDCOMSex.svMale)
				{
					if (sex == TGEDCOMSex.svFemale)
					{
						aFamily.Wife.Value = null;
					}
				}
				else
				{
					aFamily.Husband.Value = null;
				}
			}
		}

		public bool AddFamilyChild(TGEDCOMFamilyRecord aFamily, TGEDCOMIndividualRecord aChild)
		{
			bool Result;
			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aFamily, "", "");
				ptr.SetNamedValue("CHIL", aChild);
				aFamily.Childrens.Add(ptr);
				TGEDCOMChildToFamilyLink chLink = new TGEDCOMChildToFamilyLink(this.FTree, aChild, "", "");
				chLink.Family = aFamily;
				aChild.ChildToFamilyLinks.Add(chLink);
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.AddFamilyChild(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		public bool RemoveFamilyChild(TGEDCOMFamilyRecord aFamily, TGEDCOMIndividualRecord aChild)
		{
			bool Result;
			try
			{
				aFamily.DeleteChild(aChild);
				aChild.DeleteChildToFamilyLink(aFamily);
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.RemoveFamilyChild(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		public bool AddResearchTask(TGEDCOMResearchRecord aResearch, TGEDCOMTaskRecord aTask)
		{
			bool Result = false;
			if (aResearch != null && aTask != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aResearch, "", "");
				ptr.SetNamedValue("_TASK", aTask);
				aResearch.Tasks.Add(ptr);
				Result = true;
			}
			return Result;
		}

		public void RemoveResearchTask(TGEDCOMResearchRecord aResearch, TGEDCOMTaskRecord aTask)
		{
			aResearch.Tasks.Delete(aResearch.IndexOfTask(aTask));
		}

		public bool AddResearchGroup(TGEDCOMResearchRecord aResearch, TGEDCOMGroupRecord aGroup)
		{
			bool Result = false;
			if (aResearch != null && aGroup != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aResearch, "", "");
				ptr.SetNamedValue("_GROUP", aGroup);
				aResearch.Groups.Add(ptr);
				Result = true;
			}
			return Result;
		}

		public void RemoveResearchGroup(TGEDCOMResearchRecord aResearch, TGEDCOMGroupRecord aGroup)
		{
			aResearch.Groups.Delete(aResearch.IndexOfGroup(aGroup));
		}

		public bool AddResearchComm(TGEDCOMResearchRecord aResearch, TGEDCOMCommunicationRecord aComm)
		{
			bool Result = false;
			if (aResearch != null && aComm != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aResearch, "", "");
				ptr.SetNamedValue("_COMM", aComm);
				aResearch.Communications.Add(ptr);
				Result = true;
			}
			return Result;
		}

		public void RemoveResearchComm(TGEDCOMResearchRecord aResearch, TGEDCOMCommunicationRecord aComm)
		{
			aResearch.Communications.Delete(aResearch.IndexOfCommunication(aComm));
		}

		public bool AddGroupMember(TGEDCOMGroupRecord aGroup, TGEDCOMIndividualRecord aMember)
		{
			bool Result;
			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aGroup, "", "");
				ptr.SetNamedValue("_MEMBER", aMember);
				aGroup.Members.Add(ptr);
				ptr = new TGEDCOMPointer(this.FTree, aMember, "", "");
				ptr.SetNamedValue("_GROUP", aGroup);
				aMember.Groups.Add(ptr);
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.AddGroupMember(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		public bool RemoveGroupMember(TGEDCOMGroupRecord aGroup, TGEDCOMIndividualRecord aMember)
		{
			bool Result;
			try
			{
				aGroup.Members.Delete(aGroup.IndexOfMember(aMember));
				aMember.Groups.Delete(aMember.IndexOfGroup(aGroup));
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.RemoveGroupMember(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		public TGEDCOMAssociation AddAssociation(TGEDCOMIndividualRecord aRec, string aRel, TGEDCOMIndividualRecord aRelPerson)
		{
			TGEDCOMAssociation Result = new TGEDCOMAssociation(this.FTree, aRec, "", "");
			Result.Relation = aRel;
			Result.Individual = aRelPerson;
			aRec.Associations.Add(Result);
			return Result;
		}

		public void CleanFamily(TGEDCOMFamilyRecord aFamily)
		{
			if (aFamily != null)
			{
				int num = aFamily.Childrens.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMIndividualRecord child = aFamily.Childrens[i].Value as TGEDCOMIndividualRecord;
					child.DeleteChildToFamilyLink(aFamily);
				}

				TGEDCOMIndividualRecord spouse = aFamily.Husband.Value as TGEDCOMIndividualRecord;
				this.RemoveFamilySpouse(aFamily, spouse);

				spouse = (aFamily.Wife.Value as TGEDCOMIndividualRecord);
				this.RemoveFamilySpouse(aFamily, spouse);
			}
		}

		public TGEDCOMSubmitterRecord GetSubmitter()
		{
			TGEDCOMSubmitterRecord submitter = this.FTree.Header.Submitter.Value as TGEDCOMSubmitterRecord;
			if (submitter == null)
			{
				submitter = new TGEDCOMSubmitterRecord(this.FTree, this.FTree, "", "");
				submitter.InitNew();
				this.FTree.AddRecord(submitter);
				this.FTree.Header.SetTagStringValue("SUBM", "@" + submitter.XRef + "@");
			}
			return submitter;
		}

		public TGEDCOMSourceRecord FindSource(string aName)
		{
			TGEDCOMSourceRecord result = null;
			int num = this.FTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.FTree[i];

				if (rec is TGEDCOMSourceRecord && (rec as TGEDCOMSourceRecord).FiledByEntry == aName)
				{
					result = (rec as TGEDCOMSourceRecord);
					break;
				}
			}
			return result;
		}

		// FIXME: проверить сколько вхождений, возможно перенести в точку использования
		public void GetSourcesList(StringList aSources)
		{
			if (aSources != null)
			{
				aSources.Clear();
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMSourceRecord)
					{
						aSources.AddObject((rec as TGEDCOMSourceRecord).FiledByEntry, rec);
					}
				}
			}
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
					TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
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

					int ch_cnt = TGenEngine.GetChildsCount(ind);
					TakeVal(ch_cnt, ind.Sex, ref aStats.childs, ref aStats.childs_cnt,
					        ref aStats.childs_f, ref aStats.childs_f_cnt, ref aStats.childs_m, ref aStats.childs_m_cnt);

					TGEDCOMIndividualRecord iDummy;
					int v_fba = TGenEngine.GetFirstbornAge(ind, out iDummy);
					TakeVal(v_fba, ind.Sex, ref aStats.fba, ref aStats.fba_cnt,
					        ref aStats.fba_f, ref aStats.fba_f_cnt, ref aStats.fba_m, ref aStats.fba_m_cnt);

					int m_cnt = TGenEngine.GetMarriagesCount(ind);
					TakeVal(m_cnt, ind.Sex, ref aStats.marr, ref aStats.marr_cnt,
					        ref aStats.marr_f, ref aStats.marr_f_cnt, ref aStats.marr_m, ref aStats.marr_m_cnt);

					int v_mage = TGenEngine.GetMarriageAge(ind);
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
						aVals.Add(new TListVal(iName, TGenEngine.GetAncestorsCount(iRec) - 1));
						break;
					}
				case TStatMode.smDescendants:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetDescendantsCount(iRec) - 1));
						break;
					}
				case TStatMode.smDescGenerations:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetDescGenerations(iRec)));
						break;
					}
				case TStatMode.smChildsCount:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetChildsCount(iRec)));
						break;
					}
				case TStatMode.smFirstbornAge:
					{
						TGEDCOMIndividualRecord iDummy;
						aVals.Add(new TListVal(iName, TGenEngine.GetFirstbornAge(iRec, out iDummy)));
						break;
					}
				case TStatMode.smMarriages:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetMarriagesCount(iRec)));
						break;
					}
				case TStatMode.smMarriageAge:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetMarriageAge(iRec)));
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
								V = TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMSex.svFemale);
								break;
							case TStatMode.smNames:
								V = nam;
								break;
							case TGenEngine.TStatMode.smPatronymics:
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
						CheckVal(aVals, TGenEngine.GetChildsCount(iRec).ToString());
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
				TGenEngine.InitExtCounts(this.FTree, -1);
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
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
						
						if (aMode != TStatMode.smAAF_1 && aMode != TStatMode.smAAF_2)
						{
							GetSimplePersonStat(aMode, aVals, iRec);
						}
						else
						{
							TGEDCOMIndividualRecord iChild;
							int fba = TGenEngine.GetFirstbornAge(iRec, out iChild);
							if (fba > 0) {
								string key;
								List<int> vals_list = null;

								switch (aMode) {
									case TStatMode.smAAF_1:
										key = SysUtils.Trunc(GetIndependentYear(iRec, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out vals_list))
										{
											vals_list = new List<int>();
											xvals.Add(key, vals_list);
										}
										vals_list.Add(fba);

										break;

									case TStatMode.smAAF_2:
										key = SysUtils.Trunc(GetIndependentYear(iChild, "BIRT") / 10 * 10).ToString();

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
							TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)rec;
							aVals.Add(new TListVal(TGenEngine.aux_GetFamilyStr(fRec), TGenEngine.GetSpousesDiff(fRec)));
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

		public bool CheckPath()
		{
			string path = Path.GetDirectoryName(this.FFileName);
			bool Result = (path != null && path != "");
			if (!Result)
			{
				TGenEngine.ShowError("Для типов хранения \"архив\" и \"хранилище\" новый файл БД нужно предварительно сохранить");
			}
			return Result;
		}

		public string GetContainerName(bool arc)
		{
			string result = Path.GetFileNameWithoutExtension(this.FFileName);
			if (arc) {
				result += ".zip";
			} else {
				result += "\\";
			}
			return result;
		}

		public string GetArcFileName()
		{
			string result = Path.GetDirectoryName(this.FFileName) + "\\" + Path.GetFileNameWithoutExtension(this.FFileName) + ".zip";
			return result;
		}

		public string GetStgFolder(bool create)
		{
			string result = Path.GetDirectoryName(this.FFileName) + "\\" + Path.GetFileNameWithoutExtension(this.FFileName) + "\\";
			if (!Directory.Exists(result) && create) Directory.CreateDirectory(result);
			return result;
		}

		public TGKStoreType GetStoreType(string aFileRef, ref string aFileName)
		{
			aFileName = aFileRef;
			TGKStoreType Result;
			if (aFileRef.IndexOf(GKStoreTypes[2].Sign) == 0)
			{
				Result = TGKStoreType.gstArchive;
				aFileName = aFileName.Remove(0, 4);
			}
			else
			{
				if (aFileRef.IndexOf(GKStoreTypes[1].Sign) == 0)
				{
					Result = TGKStoreType.gstStorage;
					aFileName = aFileName.Remove(0, 4);
				}
				else
				{
					Result = TGKStoreType.gstReference;
				}
			}
			return Result;
		}

		public void MediaLoad(string aRefName, out Stream aStream, bool throwException)
		{
			aStream = null;
			string target_fn = "";
			TGKStoreType gst = this.GetStoreType(aRefName, ref target_fn);

			switch (gst) {
				case TGKStoreType.gstStorage:
				{
					target_fn = this.GetStgFolder(false) + target_fn;
					if (!File.Exists(target_fn)) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							TGenEngine.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
						}
					} else {
						aStream = new FileStream(target_fn, FileMode.Open);
					}
					break;
				}

				case TGKStoreType.gstArchive:
				{
					aStream = new MemoryStream();
					if (!File.Exists(this.GetArcFileName())) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							TGenEngine.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
						}
					} else {
						this.ArcFileLoad(target_fn, aStream);
						aStream.Seek((long)0, SeekOrigin.Begin);
					}
					break;
				}

				case TGKStoreType.gstReference:
				{
					aStream = new FileStream(target_fn, FileMode.Open);
					break;
				}
			}
		}

		public void MediaLoad(string aRefName, ref string aFileName)
		{
			try
			{
				string target_fn = "";
				TGKStoreType gst = this.GetStoreType(aRefName, ref target_fn);

				switch (gst) {
					case TGKStoreType.gstStorage:
					{
						aFileName = this.GetStgFolder(false) + target_fn;
						break;
					}

					case TGKStoreType.gstArchive:
					{
						aFileName = TGenEngine.GetTempDir() + "\\" + Path.GetFileName(target_fn);
						FileStream fs = new FileStream(aFileName, FileMode.Create);
						try
						{
							if (!File.Exists(this.GetArcFileName()))
							{
								TGenEngine.ShowError(LangMan.LSList[476]);
							}
							else
							{
								target_fn = target_fn.Replace("\\", "/");
								this.ArcFileLoad(target_fn, fs);
							}
						}
						finally
						{
							fs.Close();
							fs.Dispose();
						}
						break;
					}

					case TGKStoreType.gstReference:
					{
						aFileName = target_fn;
						break;
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.MediaLoad_fn(): " + E.Message);
				aFileName = "";
			}
		}

		public bool MediaSave(string aFileName, TGKStoreType aStoreType, ref string aRefPath)
		{
			bool result = true;

			string sfn = Path.GetFileName(aFileName);
			string spath = "";

			switch (TGEDCOMFileReference.RecognizeFormat(aFileName))
			{
				case TGEDCOMMultimediaFormat.mfNone:
				case TGEDCOMMultimediaFormat.mfOLE:
				case TGEDCOMMultimediaFormat.mfUnknown:
				{
					spath = "unknown\\";
					break;
				}
				case TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMMultimediaFormat.mfPNG:
				{
					spath = "images\\";
					break;
				}
				case TGEDCOMMultimediaFormat.mfWAV:
				{
					spath = "audio\\";
					break;
				}
				case TGEDCOMMultimediaFormat.mfTXT:
				case TGEDCOMMultimediaFormat.mfRTF:
				case TGEDCOMMultimediaFormat.mfHTM:
				{
					spath = "texts\\";
					break;
				}
				case TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMMultimediaFormat.mfMPG:
				{
					spath = "video\\";
					break;
				}
			}

			switch (aStoreType) {
				case TGKStoreType.gstReference:
					{
						aRefPath = aFileName;
						break;
					}
				case TGKStoreType.gstArchive:
					{
						sfn = spath + sfn;
						aRefPath = GKStoreTypes[(int)aStoreType].Sign + sfn;
						this.ArcFileSave(aFileName, sfn);
						break;
					}
				case TGKStoreType.gstStorage:
					{
						string target_dir = this.GetStgFolder(true) + spath;
						string target_fn = target_dir + sfn;

						aRefPath = GKStoreTypes[(int)aStoreType].Sign + spath + sfn;

						if (!Directory.Exists(target_dir)) Directory.CreateDirectory(target_dir);
						try
						{
							File.Copy(aFileName, target_fn, false);
						}
						catch (IOException)
						{
							TGenEngine.ShowError("Файл с таким именем уже есть в хранилище");
							result = false;
						}

						break;
					}
			}

			return result;
		}

		public TGEDCOMMultimediaLink SetPrimaryMultimediaRecord(TGEDCOMIndividualRecord aRec, TGEDCOMMultimediaRecord mmRec)
		{
			TGEDCOMMultimediaLink mmLink = null;
			int num = aRec.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (aRec.MultimediaLinks[i].Value == mmRec)
				{
					mmLink = aRec.MultimediaLinks[i];
					break;
				}
			}

			if (mmLink == null)
			{
				mmLink = new TGEDCOMMultimediaLink(this.FTree, aRec, "", "");
				mmLink.Value = mmRec;
				aRec.MultimediaLinks.Add(mmLink);
			}

			mmLink.IsPrimary = true;
			return mmLink;
		}

		public TGEDCOMMultimediaLink GetPrimaryMultimediaLink(TGEDCOMIndividualRecord aRec)
		{
			TGEDCOMMultimediaLink result = null;

			int num = aRec.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMMultimediaLink mmLink = aRec.MultimediaLinks[i];
				if (mmLink.IsPrimary)
				{
					result = mmLink;
					break;
				}
			}

			return result;
		}

		public Bitmap BitmapLoad(string media, int thumbWidth, int thumbHeight, bool throwException)
		{
			Bitmap result = null;
			try
			{
				Stream stm;
				this.MediaLoad(media, out stm, throwException);
				if (stm != null)
				{
					if (stm.Length != 0) {
						using (Bitmap bmp = new Bitmap(stm))
						{
							int new_width;
							int new_height;

							if (thumbWidth > 0 && thumbHeight > 0)
							{
								int maxSize_src = ((bmp.Height > bmp.Width) ? bmp.Height : bmp.Width);
								int minSize_dst = ((thumbHeight < thumbWidth) ? thumbHeight : thumbWidth);
								double ratio = (double)minSize_dst / maxSize_src;
								new_width = (int)(bmp.Width * ratio);
								new_height = (int)(bmp.Height * ratio);
							} else {
								new_width = bmp.Width;
								new_height = bmp.Height;
							}

							Bitmap new_image = new Bitmap(new_width, new_height, PixelFormat.Format24bppRgb);
							Graphics graphic = Graphics.FromImage(new_image);
							graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
							graphic.SmoothingMode = SmoothingMode.HighQuality;
							graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
							graphic.CompositingQuality = CompositingQuality.HighQuality;
							graphic.DrawImage(bmp, 0, 0, new_width, new_height);

							result = new_image;
						}
					}
					stm.Dispose();
				}
			}
			catch (MediaFileNotFoundException ex)
			{
				throw ex;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.BitmapLoad(): " + E.Message);
				result = null;
			}
			return result;
		}

		public Bitmap GetPrimaryBitmap(TGEDCOMIndividualRecord aRec, int thumbWidth, int thumbHeight, bool throwException)
		{
			Bitmap result = null;
			try
			{
				TGEDCOMMultimediaLink mmLink = this.GetPrimaryMultimediaLink(aRec);
				if (mmLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					result = BitmapLoad(mmRec.FileReferences[0].StringValue, thumbWidth, thumbHeight, throwException);
				}
			}
			catch (MediaFileNotFoundException ex)
			{
				throw ex;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetPrimaryBitmap(): " + E.Message);
				result = null;
			}
			return result;
		}

		public Stream GetPrimaryBitmapStream(TGEDCOMIndividualRecord aRec)
		{
			Stream result = null;
			try
			{
				TGEDCOMMultimediaLink mmLink = this.GetPrimaryMultimediaLink(aRec);
				if (mmLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					this.MediaLoad(mmRec.FileReferences[0].StringValue, out result, false);
				}
			}
			catch (MediaFileNotFoundException ex)
			{
				throw ex;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetPrimaryBitmapStream(): " + E.Message);
				result = null;
			}
			return result;
		}

		public void ArcFileLoad(string target_fn, Stream toStream)
		{
			// http://www.icsharpcode.net/OpenSource/SharpZipLib/ - slow, but high compression ratio
			// http://dotnetzip.codeplex.com/ - fast, but low compression ratio

			target_fn = target_fn.Replace('\\', '/');

			using (ZipStorer zip = ZipStorer.Open(GetArcFileName(), FileAccess.Read))
			{
				List<ZipStorer.ZipFileEntry> dir = zip.ReadCentralDir();
				foreach (ZipStorer.ZipFileEntry entry in dir)
				{
					if (entry.FilenameInZip.Equals(target_fn)) {
						zip.ExtractFile(entry, toStream);
						break;
					}
				}
			}
		}

		public void ArcFileSave(string aFileName, string sfn)
		{
			string arc_fn = GetArcFileName();
			ZipStorer zip = null;

			try
			{
				if (File.Exists(arc_fn)) {
					zip = ZipStorer.Open(arc_fn, FileAccess.ReadWrite);
				} else {
					zip = ZipStorer.Create(arc_fn, "");
				}
				zip.AddFile(ZipStorer.Compression.Deflate, aFileName, sfn, null);
			}
			finally
			{
				if (zip != null) zip.Dispose();
			}
		}

		// FIXME: найти все включения, переработать применение
		public void SortFamilyChilds(TGEDCOMFamilyRecord aFamily)
		{
			int num = aFamily.Childrens.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = aFamily.Childrens.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					TGEDCOMIndividualRecord iChild = aFamily.Childrens[i].Value as TGEDCOMIndividualRecord;
					TGEDCOMCustomEvent iEv = TGenEngine.GetIndividualEvent(iChild, "BIRT");

					DateTime iDate = ((iEv != null) ? TGenEngine.GEDCOMDateToDate(iEv.Detail.Date) : new DateTime(0));

					TGEDCOMIndividualRecord kChild = aFamily.Childrens[j].Value as TGEDCOMIndividualRecord;
					TGEDCOMCustomEvent kEv = TGenEngine.GetIndividualEvent(kChild, "BIRT");

					DateTime kDate = ((kEv != null) ? TGenEngine.GEDCOMDateToDate(kEv.Detail.Date) : new DateTime(0));

					if (iDate > kDate) aFamily.Childrens.Exchange(i, j);
				}
			}
		}


		public static string SexStr(TGEDCOMSex Sex)
		{
			return LangMan.LSList[(int)SexData[(int)Sex].NameId - 1];
		}


		public static TGEDCOMSex GetSexBySign([In] char SexSign)
		{
			TGEDCOMSex Result = TGEDCOMSex.svNone;
			
			switch (SexSign) {
				case 'F':
					Result = TGEDCOMSex.svFemale;
					break;
				case 'M':
					Result = TGEDCOMSex.svMale;
					break;
				case 'U':
					Result = TGEDCOMSex.svUndetermined;
					break;
			}
			
			return Result;
		}

		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}

		public static bool IsRecordAccess(TGEDCOMRestriction aRecRestriction, TShieldState aShieldState)
		{
			bool Result = false;

			switch (aShieldState) {
				case TShieldState.ssMaximum:
					Result = (((aRecRestriction == TGEDCOMRestriction.rnConfidential || aRecRestriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case TShieldState.ssMiddle:
					Result = (((aRecRestriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case TShieldState.ssNone:
					Result = true;
					break;
			}

			return Result;
		}

		public static TPersonEventKind GetPersonEventKindBySign(string aSign)
		{
			TPersonEventKind res = TPersonEventKind.ekFact;

			for (int i = 0; i < PersonEvents.Length; i++)
			{
				if (PersonEvents[i].Sign == aSign)
				{
					res = PersonEvents[i].Kind;
					break;
				}
			}

			return res;
		}

		public static int GetPersonEventIndex(string aSign)
		{
			int res = -1;

			for (int i = 0; i < PersonEvents.Length; i++)
			{
				if (PersonEvents[i].Sign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static int GetFamilyEventIndex(string aSign)
		{
			int res = -1;

			for (int i = 0; i < FamilyEvents.Length; i++)
			{
				if (FamilyEvents[i].Sign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static int GetMarriageStatusIndex(string aSign)
		{
			int res = 0;
			for (int i = 0; i < MarriageStatus.Length; i++)
			{
				if (MarriageStatus[i].StatSign == aSign)
				{
					res = i;
					break;
				}
			}
			return res;
		}

		public static string GetEventName(TGEDCOMCustomEvent aEvent)
		{
			string Result;
			if (aEvent is TGEDCOMIndividualEvent || aEvent is TGEDCOMIndividualAttribute)
			{
				int ev = TGenEngine.GetPersonEventIndex(aEvent.Name);
				if (ev == 0)
				{
					Result = aEvent.Detail.Classification;
				}
				else
				{
					if (ev > 0)
					{
						Result = LangMan.LSList[(int)PersonEvents[ev].Name - 1];
					}
					else
					{
						Result = aEvent.Name;
					}
				}
			}
			else
			{
				if (aEvent is TGEDCOMFamilyEvent)
				{
					int ev = TGenEngine.GetFamilyEventIndex(aEvent.Name);
					if (ev == 0)
					{
						Result = aEvent.Detail.Classification;
					}
					else
					{
						if (ev > 0)
						{
							Result = LangMan.LSList[(int)FamilyEvents[ev].Name - 1];
						}
						else
						{
							Result = aEvent.Name;
						}
					}
				}
				else
				{
					Result = "";
				}
			}
			return Result;
		}

		public static string aux_GetFamilyStr(TGEDCOMFamilyRecord aFamily)
		{
			string Result = "";

			TGEDCOMIndividualRecord spouse = aFamily.Husband.Value as TGEDCOMIndividualRecord;
			if (spouse == null)
			{
				Result += LangMan.LSList[64];
			}
			else
			{
				Result += spouse.aux_GetNameStr(true, false);
			}

			Result += " - ";

			spouse = (aFamily.Wife.Value as TGEDCOMIndividualRecord);
			if (spouse == null)
			{
				Result += LangMan.LSList[63];
			}
			else
			{
				Result += spouse.aux_GetNameStr(true, false);
			}

			return Result;
		}

		// FIXME: localization, old code
		public static TGEDCOMSex GetSex(string f_name, string f_pat, bool aQuery)
		{
			TGEDCOMSex Result = TGEDCOMSex.svNone;
			char c = f_name[((f_name != null) ? f_name.Length : 0) - 1];
			if (c != 'а')
			{
				if (c - 'в' < '\u0003' || c == 'й' || c - 'л' < '\u0006')
				{
					Result = TGEDCOMSex.svMale;
					goto IL_AE;
				}
				if (c != 'я')
				{
					goto IL_AE;
				}
			}
			if (((f_pat != null) ? f_pat.Length : 0) > 1)
			{
				char c2 = f_pat[((f_pat != null) ? f_pat.Length : 0) - 1];
				if (c2 == '0' || c2 == 'O')
				{
					Result = TGEDCOMSex.svFemale;
				}
				else
				{
					char c3 = f_pat[((f_pat != null) ? f_pat.Length : 0) - 1];
					if (c3 >= '2' && (c3 < '5' || c3 == '9' || (c3 >= ';' && c3 < 'A')))
					{
						Result = TGEDCOMSex.svMale;
					}
				}
			}
			IL_AE:
			if (aQuery && Result == TGEDCOMSex.svNone)
			{
				if (TGenEngine.ShowQuestion("Не определяется пол человека по имени \"" + f_name + " " + f_pat + "\". Это мужской пол?") == DialogResult.Yes)
				{
					Result = TGEDCOMSex.svMale;
				}
				else
				{
					Result = TGEDCOMSex.svFemale;
				}
			}
			return Result;
		}

		// FIXME: aux_candidate
		public static string GetXRefNum(TGEDCOMRecord aRecord)
		{
			string xref = aRecord.XRef;
			int I = 0;
			int L = xref.Length - 1;
			while (I <= L && (xref[I] < '0' || xref[I] > '9')) I++;
			xref = ((I <= L) ? xref.Substring(I) : "");
			return xref;
		}

		public static int GetId(TGEDCOMRecord aRecord)
		{
			int Result;
			try
			{
				string xref = TGenEngine.GetXRefNum(aRecord);
				Result = SysUtils.ParseInt(xref, 0);
			}
			catch (Exception)
			{
				Result = -1;
			}
			return Result;
		}

		public static string GEDCOMDateToStr(TGEDCOMDate aDate, TDateFormat aFormat)
		{
			string Result = "";
			int year;
			ushort month;
			ushort day;
			aDate.GetDate(out year, out month, out day);

			if (year > 0 || month > 0 || day > 0)
			{
				if (aFormat != TDateFormat.dfDD_MM_YYYY)
				{
					if (aFormat != TDateFormat.dfYYYY_MM_DD)
					{
						if (aFormat == TDateFormat.dfYYYY)
						{
							if (year > 0)
							{
								Result = year.ToString().PadLeft(4, '_');
							}
						}
					}
					else
					{
						if (year > 0)
						{
							Result = Result + year.ToString().PadLeft(4, '_') + ".";
						}
						else
						{
							Result += "____.";
						}
						if (month > 0)
						{
							Result = Result + TGenEngine.NumUpdate((int)month, 2) + ".";
						}
						else
						{
							Result += "__.";
						}
						if (day > 0)
						{
							Result += TGenEngine.NumUpdate((int)day, 2);
						}
						else
						{
							Result += "__";
						}
					}
				}
				else
				{
					if (day > 0)
					{
						Result = Result + TGenEngine.NumUpdate((int)day, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (month > 0)
					{
						Result = Result + TGenEngine.NumUpdate((int)month, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (year > 0)
					{
						Result += year.ToString().PadLeft(4, '_');
					}
					else
					{
						Result += "____";
					}
				}
			}
			return Result;
		}

		public static string StrToGEDCOMDate(string aDate, bool aException)
		{
			string Result = "";

			if (aDate.IndexOf("/") >= 0) aDate = aDate.Replace("/", ".");
			if (aDate.IndexOf("_") >= 0) aDate = aDate.Replace("_", " ");

			string[] dt_parts = aDate.Split('.');
			if (dt_parts.Length < 3)
			{
				if (aException)
				{
					throw new Exception("date failed");
				}
			}
			else
			{
				string pd = dt_parts[0].Trim();
				string pm = dt_parts[1].Trim();
				string py = dt_parts[2].Trim();

				if (pd != "") Result = Result + pd + " ";
				if (pm != "") Result = Result + TGEDCOMDate.GEDCOMMonthArray[SysUtils.ParseInt(pm, 1) - 1] + " ";
				if (py != "") Result += py;
			}
			return Result;
		}

		public static string GEDCOMCustomDateToStr(TGEDCOMDateValue aDateValue, TDateFormat aFormat, bool aSign)
		{
			string Result = "";

			TGEDCOMCustomDate date = aDateValue.Value;

			if (date == null)
			{
				Result = "";
			}
			else
			{
				if (date is TGEDCOMDateApproximated)
				{
					Result = TGenEngine.GEDCOMDateToStr(date as TGEDCOMDate, aFormat);
					if (aSign && (date as TGEDCOMDateApproximated).Approximated != TGEDCOMApproximated.daExact)
					{
						Result = "~ " + Result;
					}
				}
				else
				{
					if (date is TGEDCOMDateRange)
					{
						TGEDCOMDateRange dt_range = date as TGEDCOMDateRange;
						if (dt_range.After.StringValue == "" && dt_range.Before.StringValue != "")
						{
							Result = TGenEngine.GEDCOMDateToStr(dt_range.Before, aFormat);
							if (aSign)
							{
								Result = "< " + Result;
							}
						}
						else
						{
							if (dt_range.After.StringValue != "" && dt_range.Before.StringValue == "")
							{
								Result = TGenEngine.GEDCOMDateToStr(dt_range.After, aFormat);
								if (aSign)
								{
									Result += " >";
								}
							}
							else
							{
								if (dt_range.After.StringValue != "" && dt_range.Before.StringValue != "")
								{
									Result = TGenEngine.GEDCOMDateToStr(dt_range.After, aFormat) + "-" + TGenEngine.GEDCOMDateToStr(dt_range.Before, aFormat);
								}
							}
						}
					}
					else
					{
						if (date is TGEDCOMDatePeriod)
						{
							TGEDCOMDatePeriod dt_period = date as TGEDCOMDatePeriod;
							if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue == "")
							{
								Result = TGenEngine.GEDCOMDateToStr(dt_period.DateFrom, aFormat);
								if (aSign)
								{
									Result += " >";
								}
							}
							else
							{
								if (dt_period.DateFrom.StringValue == "" && dt_period.DateTo.StringValue != "")
								{
									Result = TGenEngine.GEDCOMDateToStr(dt_period.DateTo, aFormat);
									if (aSign)
									{
										Result = "< " + Result;
									}
								}
								else
								{
									if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue != "")
									{
										Result = TGenEngine.GEDCOMDateToStr(dt_period.DateFrom, aFormat) + "-" + TGenEngine.GEDCOMDateToStr(dt_period.DateTo, aFormat);
									}
								}
							}
						}
						else
						{
							if (date is TGEDCOMDate)
							{
								Result = TGenEngine.GEDCOMDateToStr(date as TGEDCOMDate, aFormat);
							}
						}
					}
				}
			}

			if ((date is TGEDCOMDate) && (date as TGEDCOMDate).YearBC) {
				switch (aFormat) {
					case TDateFormat.dfDD_MM_YYYY:
						Result = Result + " BC";
						break;
					case TDateFormat.dfYYYY_MM_DD:
						Result = "BC " + Result;
						break;
					case TDateFormat.dfYYYY:
						Result = "BC " + Result;
						break;
				}
			}

			return Result;
		}

		public static string GEDCOMEventToDateStr(TGEDCOMCustomEvent aEvent, TDateFormat aFormat, bool aSign)
		{
			return ((aEvent == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(aEvent.Detail.Date, aFormat, aSign));
		}

		public static DateTime GEDCOMDateToDate(TGEDCOMDateValue aDate)
		{
			DateTime Result;

			try
			{
				if (aDate != null)
				{
					int year;
					ushort month, day;
					aDate.aux_GetIndependentDate(out year, out month, out day);
					if (day == 0) day = 1;
					if (month == 0) month = 1;

					Result = ((year <= 0) ? new DateTime(0) : new DateTime(year, (int)month, (int)day));
				}
				else
				{
					Result = new DateTime(0);
				}
			}
			catch (Exception E)
			{
				int year = 0;
				ushort month = 0;
				ushort day = 0;
				SysUtils.LogWrite(string.Format("GEDCOMDateToDate(%d, %d, %d): ", new object[] { year, month, day }) + E.Message);
				SysUtils.LogWrite("Record (" + (aDate.ParentRecord as TGEDCOMRecord).XRef + "): invalid date");
				Result = new DateTime(0);
			}
			return Result;
		}

		public static TGEDCOMCustomEvent GetIndividualEvent(TGEDCOMIndividualRecord iRec, string evName)
		{
			return ((iRec == null) ? null : iRec.GetIndividualEvent(evName));
		}

		public static string GetAttributeValue(TGEDCOMIndividualRecord iRec, string attrName)
		{
			TGEDCOMCustomEvent attr = TGenEngine.GetIndividualEvent(iRec, attrName);
			string result = ((attr == null) ? "" : attr.StringValue);
			return result;
		}

		public static string CompactDate([In] string aDate)
		{
			string result = aDate;
			while (result.IndexOf("__.") == 0) result = result.Remove(0, 3);
			return result;
		}

		public static string GetBirthDate(TGEDCOMIndividualRecord iRec, TDateFormat aFormat, bool aCompact)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
			string result = ((evt == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, aFormat, false));
			if (aCompact) result = TGenEngine.CompactDate(result);
			return result;
		}

		public static string GetDeathDate(TGEDCOMIndividualRecord iRec, TDateFormat aFormat, bool aCompact)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "DEAT");
			string result = ((evt == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, aFormat, false));
			if (aCompact) result = TGenEngine.CompactDate(result);
			return result;
		}

		public static string GetLifeStr(TGEDCOMIndividualRecord iRec)
		{
			string Result = " (";

			string ds = TGenEngine.GetBirthDate(iRec, TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				ds = "?";
			}
			Result += ds;

			ds = TGenEngine.GetDeathDate(iRec, TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, "DEAT");
				if (ev != null)
				{
					ds = "?";
				}
			}

			if (ds != "")
			{
				Result = Result + " - " + ds;
			}

			Result += ")";
			return Result;
		}

		public static string GetBirthPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetDeathPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "DEAT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetResidencePlace(TGEDCOMIndividualRecord iRec, bool IncludeAddress)
		{
			return TGenEngine.GetPlaceStr(TGenEngine.GetIndividualEvent(iRec, "RESI"), IncludeAddress);
		}

		public static string GetPlaceStr(TGEDCOMCustomEvent aEvent, bool IncludeAddress)
		{
			string Result;
			if (aEvent == null)
			{
				Result = "";
			}
			else
			{
				Result = aEvent.Detail.Place.StringValue;
				if (IncludeAddress)
				{
					string resi = aEvent.StringValue;
					string addr = aEvent.Detail.Address.Address.Text.Trim();
					if (resi != "" && addr != "")
					{
						resi += ", ";
					}
					resi += addr;
					if (resi != "")
					{
						Result = Result + " [" + resi + "]";
					}
				}
			}
			return Result;
		}

		public static string GetAttributeStr(TGEDCOMIndividualAttribute iAttr)
		{
			int idx = TGenEngine.GetPersonEventIndex(iAttr.Name);
			string st;
			if (idx == 0)
			{
				st = iAttr.Detail.Classification;
			}
			else
			{
				if (idx > 0)
				{
					st = LangMan.LSList[(int)PersonEvents[idx].Name - 1];
				}
				else
				{
					st = iAttr.Name;
				}
			}

			string place = iAttr.Detail.Place.StringValue;
			if (place != "")
			{
				place = " [" + place + "]";
			}
			return st + ": " + iAttr.StringValue + place;
		}

		public static string GetMarriageDate(TGEDCOMFamilyRecord fRec, TDateFormat aFormat)
		{
			string result;

			if (fRec == null)
			{
				result = "";
			}
			else
			{
				TGEDCOMFamilyEvent evt = fRec.aux_GetFamilyEvent("MARR");
				result = ((evt == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, aFormat, false));
			}

			return result;
		}

		public static string GetEventDesc(TGEDCOMEventDetail evDetail)
		{
			string dt = TGenEngine.GEDCOMCustomDateToStr(evDetail.Date, TDateFormat.dfDD_MM_YYYY, false);
			string place = evDetail.Place.StringValue;
			TGEDCOMLocationRecord location = evDetail.Place.Location.Value as TGEDCOMLocationRecord;

			if (place != "" && location != null)
			{
				place = TGenEngine.HyperLink(location.XRef, place, 0);
			}

			string Result;

			if (dt == "" && place == "")
			{
				Result = "?";
			}
			else
			{
				if (dt == "")
				{
					Result = place;
				}
				else
				{
					if (place == "")
					{
						Result = dt;
					}
					else
					{
						Result = dt + ", " + place;
					}
				}
			}
			return Result;
		}

		public static string GetEventCause(TGEDCOMEventDetail evDetail)
		{
			string Result = "";

			if (evDetail.Cause != "")
			{
				Result += evDetail.Cause;
			}

			if (evDetail.Agency != "")
			{
				if (Result != "")
				{
					Result += " ";
				}
				Result = Result + "[" + evDetail.Agency + "]";
			}

			return Result;
		}

		// FIXME: aux_candidate
		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign)
		{
			bool dummy;
			return GetIndependentYear(iRec, evSign, out dummy);
		}

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign, out bool YearBC)
		{
			int Result = -1;
			YearBC = false;

			TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, evSign);
			if (ev != null)
			{
				int year;
				ushort am, ad;
				ev.Detail.Date.aux_GetIndependentDate(out year, out am, out ad, out YearBC);
				Result = year;
			}
			return Result;
		}

		public static double GetAbstractDate(TGEDCOMEventDetail aEventDetail)
		{
			bool dummy;
			return GetAbstractDate(aEventDetail, out dummy);
		}

		private static readonly ushort[][] MonthDays = new ushort[][]
		{
			new ushort[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }, 
			new ushort[] { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
		};

		public static ushort DaysInAMonth([In] ushort AYear, [In] ushort AMonth)
		{
			return MonthDays[(AMonth == 2 && DateTime.IsLeapYear((int)AYear)) ? 1 : 0][(int)AMonth - 1];
		}

		public static double GetAbstractDate(TGEDCOMEventDetail aEventDetail, out bool YearBC)
		{
			double Result = 0.0;
			YearBC = false;

			if (aEventDetail != null)
			{
				int y;
				ushort i;
				ushort d;
				aEventDetail.Date.aux_GetIndependentDate(out y, out i, out d, out YearBC);
				if (y > 0)
				{
					Result = (double)y;
					if (i > 0)
					{
						Result = (Result + i / 12.0);
						if (d > 0)
						{
							Result = (Result + d / DaysInAMonth((ushort)y, i) / 12.0);
						}
					}
				}
			}

			return Result;
		}

		public static string GetEventsYearsDiff(TGEDCOMCustomEvent ev1, TGEDCOMCustomEvent ev2, bool aCurEnd)
		{
			string Result = "?";
			try
			{
				bool ybc, ybc2;
				double y = ((ev1 == null) ? -1.0 : TGenEngine.GetAbstractDate(ev1.Detail, out ybc));
				double y2 = ((ev2 == null) ? -1.0 : TGenEngine.GetAbstractDate(ev2.Detail, out ybc2));

				if (aCurEnd && y2 <= (double)1f)
				{
					y2 = ((double)DateTime.Now.Year + (double)DateTime.Now.Month / 12.0);
				}

				if (y == (double)-1f || y2 == (double)-1f)
				{
					Result = "";
				}
				else
				{
					if (y == (double)0f || y2 == (double)0f)
					{
						Result = "?";
					}
					else
					{
						long delta = SysUtils.Trunc(y2 - y);
						Result = delta.ToString();
					}
				}

				//if ()

			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetEventsYearsDiff(): " + E.Message);
			}
			return Result;
		}

		public static string GetLifeExpectancy(TGEDCOMIndividualRecord iRec)
		{
			string Result = "";
			try
			{
				TGEDCOMCustomEvent ev = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					if (evt.Name == "BIRT")
					{
						ev = evt;
					}
					else
					{
						if (evt.Name == "DEAT")
						{
							ev2 = evt;
						}
					}
				}
				Result = TGenEngine.GetEventsYearsDiff(ev, ev2, false);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetLifeExpectancy(): " + E.Message);
			}
			return Result;
		}

		public static string GetAge(TGEDCOMIndividualRecord iRec, int ToYear)
		{
			string Result = "";
			try
			{
				TGEDCOMCustomEvent ev1 = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					if (evt.Name == "BIRT" && ev1 == null)
					{
						ev1 = evt;
					}
					else
					{
						if (evt.Name == "DEAT" && ev2 == null)
						{
							ev2 = evt;
						}
					}
				}

				if (ToYear == -1)
				{
					Result = TGenEngine.GetEventsYearsDiff(ev1, ev2, ev2 == null);
				}
				else
				{
					if (ev1 == null)
					{
						Result = "";
					}
					else
					{
						ushort dummy;
						int i;
						ev1.Detail.Date.aux_GetIndependentDate(out i, out dummy, out dummy);
						Result = Convert.ToString(ToYear - i);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetAge(): " + E.Message);
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

		private static int DaysBetween([In] DateTime ANow, [In] DateTime AThen)
		{
			TimeSpan span = ((ANow < AThen) ? AThen - ANow : ANow - AThen);
			return span.Days;
		}

		public static string GetDaysForBirth(TGEDCOMIndividualRecord iRec)
		{
			string Result = "";
			try
			{
				TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "DEAT");
				if (evt != null)
				{
				}
				else
				{
					evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
					if (evt != null)
					{
						TGEDCOMDate dt = evt.Detail.Date.Value as TGEDCOMDate;
						if (dt != null)
						{
							int bd_y;
							ushort bd_m;
							ushort bd_d;

							dt.GetDate(out bd_y, out bd_m, out bd_d);
							if (bd_m <= 0 || bd_d <= 0)
							{
							}
							else
							{
								DateTime dtx = DateTime.Now;
								ushort cur_y = (ushort)dtx.Year;
								ushort cur_m = (ushort)dtx.Month;
								ushort cur_d = (ushort)dtx.Day;
								double dt2 = (cur_y + bd_m / 12.0 + bd_d / 12.0 / 31.0);
								double dt3 = (cur_y + cur_m / 12.0 + cur_d / 12.0 / 31.0);
								if (dt2 < dt3)
								{
									bd_y = (int)(cur_y + 1u);
								}
								else
								{
									bd_y = (int)cur_y;
								}
								Result = Convert.ToString(DaysBetween(new DateTime((int)cur_y, (int)cur_m, (int)cur_d), new DateTime(bd_y, (int)bd_m, (int)bd_d)));
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetDaysForBirth(): " + E.Message);
			}
			return Result;
		}

		public static TGEDCOMCustomEvent CreateEventEx(TGEDCOMTree aTree, TGEDCOMRecord aRec, string evSign, string evDate, string evPlace)
		{
			TGEDCOMCustomEvent Result;
			if (aRec is TGEDCOMIndividualRecord)
			{
				TGEDCOMIndividualRecord ind_rec = aRec as TGEDCOMIndividualRecord;
				if (TGenEngine.GetPersonEventKindBySign(evSign) == TPersonEventKind.ekEvent)
				{
					Result = new TGEDCOMIndividualEvent(aTree, ind_rec, "", "");
				}
				else
				{
					Result = new TGEDCOMIndividualAttribute(aTree, ind_rec, "", "");
				}
				ind_rec.AddIndividualEvent(Result);
			}
			else
			{
				if (!(aRec is TGEDCOMFamilyRecord))
				{
					Result = null;
					return Result;
				}
				TGEDCOMFamilyRecord fam_rec = aRec as TGEDCOMFamilyRecord;
				Result = new TGEDCOMFamilyEvent(aTree, fam_rec, "", "");
				fam_rec.FamilyEvents.Add(Result as TGEDCOMFamilyEvent);
			}
			Result.Name = evSign;
			if (evDate != "")
			{
				Result.Detail.Date.ParseString(evDate);
			}
			if (evPlace != "")
			{
				Result.Detail.Place.StringValue = evPlace;
			}
			return Result;
		}

		public static TGEDCOMIndividualRecord CreatePersonEx(TGEDCOMTree aTree, string aName, string aPatronymic, string aFamily, TGEDCOMSex aSex, bool aBirthEvent)
		{
			TGEDCOMIndividualRecord iRec = new TGEDCOMIndividualRecord(aTree, aTree, "", "");
			iRec.InitNew();
			iRec.Sex = aSex;
			TGEDCOMPersonalName pn = new TGEDCOMPersonalName(aTree, iRec, "", "");
			pn.StringValue = aName.Trim() + " " + aPatronymic.Trim() + " /" + aFamily.Trim() + "/";
			iRec.AddPersonalName(pn);
			iRec.ChangeDate.ChangeDateTime = DateTime.Now;
			aTree.AddRecord(iRec);
			if (aBirthEvent) TGenEngine.CreateEventEx(aTree, iRec, "BIRT", "", "");
			return iRec;
		}

		public static TGEDCOMFamilyRecord CreateFamilyEx(TGEDCOMTree aTree)
		{
			TGEDCOMFamilyRecord Result = new TGEDCOMFamilyRecord(aTree, aTree, "", "");
			Result.InitNew();
			Result.ChangeDate.ChangeDateTime = DateTime.Now;
			aTree.AddRecord(Result);
			return Result;
		}

		public static void SetAddressValue(TGEDCOMAddress anAddress, string aValue)
		{
			StringList sl = new StringList(aValue);
			try
			{
				anAddress.Address = sl;
			}
			finally
			{
				sl.Free();
			}
		}

		public static TGEDCOMNoteRecord CreateNote(TGEDCOMTree aTree)
		{
			TGEDCOMNoteRecord Result = new TGEDCOMNoteRecord(aTree, aTree, "", "");
			Result.InitNew();
			Result.ChangeDate.ChangeDateTime = DateTime.Now;
			aTree.AddRecord(Result);
			return Result;
		}

		public static void BindRecordNote(TGEDCOMTree aTree, TGEDCOMRecord aRecord, TGEDCOMNoteRecord aNoteRec)
		{
			TGEDCOMNotes note = new TGEDCOMNotes(aTree, aRecord, "", "");
			note.Value = aNoteRec;
			aRecord.Notes.Add(note);
		}

		public static void AddNoteText(TGEDCOMNoteRecord aNoteRec, string aText)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = aNoteRec.Note.Text.Trim();
				strData.Add(aText);
				aNoteRec.Note = strData;
			}
			finally
			{
				strData.Free();
			}
		}

		public static TGEDCOMNoteRecord CreateNoteEx(TGEDCOMTree aTree, string aText, TGEDCOMRecord aRecord)
		{
			TGEDCOMNoteRecord result = null;

			if (aRecord != null && !string.IsNullOrEmpty(aText)) {
				result = TGenEngine.CreateNote(aTree);
				AddNoteText(result, aText);
				TGenEngine.BindRecordNote(aTree, aRecord, result);
			}

			return result;
		}

		public static TGEDCOMNoteRecord CreateNoteEx(TGEDCOMTree aTree, StringList aText, TGEDCOMRecord aRecord)
		{
			TGEDCOMNoteRecord result = null;

			if (aRecord != null && aText != null) {
				result = TGenEngine.CreateNote(aTree);
				result.Note = aText;
				TGenEngine.BindRecordNote(aTree, aRecord, result);
			}

			return result;
		}

		public static TGEDCOMSourceRecord CreateSource(TGEDCOMTree aTree)
		{
			TGEDCOMSourceRecord Result = new TGEDCOMSourceRecord(aTree, aTree, "", "");
			Result.InitNew();
			Result.ChangeDate.ChangeDateTime = DateTime.Now;
			aTree.AddRecord(Result);
			return Result;
		}

		public static TGEDCOMGroupRecord CreateGroup(TGEDCOMTree aTree)
		{
			TGEDCOMGroupRecord Result = new TGEDCOMGroupRecord(aTree, aTree, "", "");
			Result.InitNew();
			Result.ChangeDate.ChangeDateTime = DateTime.Now;
			aTree.AddRecord(Result);
			return Result;
		}

		public static void BindRecordSource(TGEDCOMTree aTree, TGEDCOMRecord aRecord, TGEDCOMSourceRecord aSrcRec, string aPage, int aQuality)
		{
			TGEDCOMSourceCitation cit = new TGEDCOMSourceCitation(aTree, aRecord, "", "");
			cit.Value = aSrcRec;
			cit.Page = aPage;
			cit.CertaintyAssessment = aQuality;
			aRecord.SourceCitations.Add(cit);
		}

		public static void BindSourceRepository(TGEDCOMTree aTree, TGEDCOMSourceRecord aSourceRecord, TGEDCOMRepositoryRecord aRepRec)
		{
			TGEDCOMRepositoryCitation cit = new TGEDCOMRepositoryCitation(aTree, aSourceRecord, "", "");
			cit.Value = aRepRec;
			aSourceRecord.RepositoryCitations.Add(cit);
		}

		public static void GetLocationLinks(TGEDCOMTree aTree, TGEDCOMLocationRecord aLocation, ref StringList aList)
		{
			int num = aTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = aTree[i];
				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMCustomEvent evt = i_rec.IndividualEvents[j];
						if (evt.Detail.Place.Location.Value == aLocation)
						{
							aList.Add(TGenEngine.GenRecordLink(aTree, rec, true) + ", " + TGenEngine.GetEventName(evt).ToLower());
						}
					}
				}
				else
				{
					if (rec is TGEDCOMFamilyRecord)
					{
						TGEDCOMFamilyRecord f_rec = (TGEDCOMFamilyRecord)rec;
						int num3 = f_rec.FamilyEvents.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							TGEDCOMCustomEvent evt = f_rec.FamilyEvents[j];
							if (evt.Detail.Place.Location.Value == aLocation)
							{
								aList.Add(TGenEngine.GenRecordLink(aTree, rec, true) + ", " + TGenEngine.GetEventName(evt).ToLower());
							}
						}
					}
				}
			}
		}

		public static string HyperLink(string XRef, string Text, int Num)
		{
			string Result = "~^" + XRef;
			if (Text != "")
			{
				Result = Result + ":" + Text;
			}
			Result += "~";
			return Result;
		}

		public static string GenRecordLink(TGEDCOMTree aTree, TGEDCOMRecord aRecord, bool aSigned)
		{
			string Result;
			if (aRecord == null)
			{
				Result = "";
			}
			else
			{
				string sign = "";
				if (aSigned)
				{
					TGEDCOMRecordType recordType = aRecord.RecordType;
					if (recordType != TGEDCOMRecordType.rtIndividual)
					{
						if (recordType == TGEDCOMRecordType.rtFamily || (byte)recordType - (byte)TGEDCOMRecordType.rtMultimedia < (byte)TGEDCOMRecordType.rtResearch)
						{
							sign = LangMan.LSList[(int)RecordTypes[(int)aRecord.RecordType] - 1] + ": ";
						}
					}
					else
					{
						sign = "";
					}
				}
				string st;
				switch (aRecord.RecordType) {
					case TGEDCOMRecordType.rtIndividual:
						st = (aRecord as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;
					case TGEDCOMRecordType.rtFamily:
						st = TGenEngine.aux_GetFamilyStr(aRecord as TGEDCOMFamilyRecord);
						break;
					case TGEDCOMRecordType.rtMultimedia:
						st = (aRecord as TGEDCOMMultimediaRecord).FileReferences[0].Title;
						break;
					case TGEDCOMRecordType.rtSource:
						st = (aRecord as TGEDCOMSourceRecord).FiledByEntry;
						break;
					case TGEDCOMRecordType.rtRepository:
						st = (aRecord as TGEDCOMRepositoryRecord).RepositoryName;
						break;
					case TGEDCOMRecordType.rtGroup:
						st = (aRecord as TGEDCOMGroupRecord).GroupName;
						break;
					case TGEDCOMRecordType.rtResearch:
						st = (aRecord as TGEDCOMResearchRecord).ResearchName;
						break;
					case TGEDCOMRecordType.rtTask:
						st = TGenEngine.GetTaskGoalStr(aTree, aRecord as TGEDCOMTaskRecord);
						break;
					case TGEDCOMRecordType.rtCommunication:
						st = (aRecord as TGEDCOMCommunicationRecord).CommName;
						break;
					case TGEDCOMRecordType.rtLocation:
						st = (aRecord as TGEDCOMLocationRecord).LocationName;
						break;
					default:
						st = aRecord.XRef;
						break;
				}
				Result = TGenEngine.HyperLink(aRecord.XRef, sign + st, 0);
			}
			return Result;
		}

		public static string GetCorresponderStr(TGEDCOMTree aTree, TGEDCOMCommunicationRecord aRec, bool aLink)
		{
			string Result = "";
			TCommunicationDir dir = TCommunicationDir.cdFrom;
			TGEDCOMIndividualRecord corresponder = null;
			aRec.GetCorresponder(ref dir, ref corresponder);
			if (corresponder != null)
			{
				string nm = corresponder.aux_GetNameStr(true, false);
				if (aLink)
				{
					nm = TGenEngine.HyperLink(corresponder.XRef, nm, 0);
				}
				Result = "[" + LangMan.LSList[(int)CommunicationDirs[(int)dir] - 1] + "] " + nm;
			}
			return Result;
		}

		// FIXME: aux_candidate
		public static void GetTaskGoal(TGEDCOMTree aTree, TGEDCOMTaskRecord aTaskRec, ref TGoalType aType, ref TGEDCOMRecord aGoalRec)
		{
			aGoalRec = aTree.XRefIndex_Find(TGEDCOMObject.CleanXRef(aTaskRec.Goal));
			if (aGoalRec is TGEDCOMIndividualRecord)
			{
				aType = TGoalType.gtIndividual;
			}
			else
			{
				if (aGoalRec is TGEDCOMFamilyRecord)
				{
					aType = TGoalType.gtFamily;
				}
				else
				{
					if (aGoalRec is TGEDCOMSourceRecord)
					{
						aType = TGoalType.gtSource;
					}
					else
					{
						aType = TGoalType.gtOther;
					}
				}
			}
		}

		public static string GetTaskGoalStr(TGEDCOMTree aTree, TGEDCOMTaskRecord aRec)
		{
			TGoalType gt = TGoalType.gtOther;
			TGEDCOMRecord tempRec = null;
			TGenEngine.GetTaskGoal(aTree, aRec, ref gt, ref tempRec);
			string Result = "";

			switch (gt) {
				case TGoalType.gtIndividual:
					Result = (tempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
					break;
				case TGoalType.gtFamily:
					Result = TGenEngine.aux_GetFamilyStr((TGEDCOMFamilyRecord)tempRec);
					break;
				case TGoalType.gtSource:
					Result = ((TGEDCOMSourceRecord)tempRec).FiledByEntry;
					break;
				case TGoalType.gtOther:
					Result = aRec.Goal;
					break;
			}

			if (gt != TGoalType.gtOther)
			{
				Result = "[" + LangMan.LSList[(int)GoalNames[(int)gt] - 1] + "] " + Result;
			}
			return Result;
		}

		public static string ClearFamily(string family)
		{
			int p = family.IndexOf(" (");
			string result = ((p >= 0) ? family.Substring(0, p) : family);
			return result;
		}

		public static string PrepareRusFamily(string f, bool aFemale)
		{
			if (f == null || f.Length <= 0 || (f[0] == '(' && f[f.Length - 1] == ')'))
			{
				f = "?";
			}
			else
			{
				if (aFemale)
				{
					f = ClearFamily(f);

					if (f.EndsWith("а")) {
						f = f.Substring(0, f.Length - 1);
					} else if (f.EndsWith("кая")) {
						f = f.Substring(0, f.Length - 3) + "кий";
					} else if (f.EndsWith("ная")) {
						f = f.Substring(0, f.Length - 3) + "ный";
					}
				}
			}

			return f;
		}

		// FIXME: aux_candidate
		public static int GetChildsCount(TGEDCOMIndividualRecord aPerson)
		{
			int Result = 0;
			if (aPerson != null && aPerson.SpouseToFamilyLinks.Count > 0)
			{
				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;
					Result += family.Childrens.Count;
				}
			}
			return Result;
		}

		public static void InitExtCounts(TGEDCOMTree aTree, int aValue)
		{
			int num = aTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = aTree[i];
				if (rec is TGEDCOMIndividualRecord)
				{
					rec.ExtData = aValue;
				}
			}
		}

		public static int GetAncestorsCount(TGEDCOMIndividualRecord aPerson)
		{
			int Result = 0;
			if (aPerson != null)
			{
				int val = (int)aPerson.ExtData;
				if (val < 0)
				{
					val = 1;
					if (aPerson.ChildToFamilyLinks.Count > 0)
					{
						TGEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;
						TGEDCOMIndividualRecord anc;
						anc = family.Husband.Value as TGEDCOMIndividualRecord;
						val += TGenEngine.GetAncestorsCount(anc);
						anc = (family.Wife.Value as TGEDCOMIndividualRecord);
						val += TGenEngine.GetAncestorsCount(anc);
					}
					aPerson.ExtData = val;
				}
				Result = val;
			}
			return Result;
		}

		public static int GetDescendantsCount(TGEDCOMIndividualRecord aPerson)
		{
			int Result = 0;
			if (aPerson != null)
			{
				int val = (int)aPerson.ExtData;
				if (val < 0)
				{
					val = 1;

					int num = aPerson.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMIndividualRecord iChild = family.Childrens[j].Value as TGEDCOMIndividualRecord;
							val += TGenEngine.GetDescendantsCount(iChild);
						}
					}
					aPerson.ExtData = val;
				}
				Result = val;
			}
			return Result;
		}

		public static int GetDescGenerations(TGEDCOMIndividualRecord aPerson)
		{
			return TGenEngine.GetDescGens_Recursive(aPerson) - 1;
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

		public static TGEDCOMFormat GetGEDCOMFormat(TGEDCOMTree aTree)
		{
			string sour = aTree.Header.Source;

			TGEDCOMFormat res = TGEDCOMFormat.gf_Unknown;
			for (TGEDCOMFormat gf = TGEDCOMFormat.gf_Native; gf <= TGEDCOMFormat.gf_Last; gf++)
			{
				if (GEDCOMFormats[(int)gf].Sign == sour)
				{
					res = gf;
					break;
				}
			}
			return res;
		}

		static TGenEngine()
		{
			NumKinship = new string[]
			{
				"-", 
				"юродный", 
				"юродная", 
				""
			};

			Numerals = new string[]
			{
				"-", 
				"дво", 
				"тро", 
				"четверо", 
				"пяти", 
				"шести", 
				"семи", 
				"восьми", 
				"девяти"
			};

			RelationSigns = new string[]
			{
				"?", 
				"P", 
				"S", 
				"C", 
				"F", 
				"M", 
				"H", 
				"W", 
				"Sn", 
				"Dg", 
				"Gf", 
				"Gm", 
				"Gs", 
				"Gd", 
				"Br", 
				"St", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-", 
				"-"
			};

			RelationKinds = new LSID[]
			{
				LSID.LSID_RK_Unk, 
				LSID.LSID_None, 
				LSID.LSID_None, 
				LSID.LSID_None, 
				LSID.LSID_RK_Father, 
				LSID.LSID_RK_Mother, 
				LSID.LSID_RK_Husband, 
				LSID.LSID_RK_Wife, 
				LSID.LSID_RK_Son, 
				LSID.LSID_RK_Daughter, 
				LSID.LSID_RK_Grandfather, 
				LSID.LSID_RK_Grandmother, 
				LSID.LSID_RK_Grandson, 
				LSID.LSID_RK_Granddaughter, 
				LSID.LSID_RK_Brother, 
				LSID.LSID_RK_Sister, 
				LSID.LSID_RK_SonInLaw, 
				LSID.LSID_RK_DaughterInLaw, 
				LSID.LSID_RK_HusbandFather, 
				LSID.LSID_RK_HusbandMother, 
				LSID.LSID_RK_WifeFather, 
				LSID.LSID_RK_WifeMother, 
				LSID.LSID_RK_Uncle, 
				LSID.LSID_RK_Aunt, 
				LSID.LSID_RK_Nephew, 
				LSID.LSID_RK_Niece, 
				LSID.LSID_RK_CousinM, 
				LSID.LSID_RK_CousinF, 
				LSID.LSID_None, 
				LSID.LSID_RK_Unk
			};

			GEDCOMFormats = new TGEDCOMAppFormat[6] {
				new TGEDCOMAppFormat("", ""),
				new TGEDCOMAppFormat("GEDKeeper", ""),
				new TGEDCOMAppFormat("GENBOX", "Genbox Family History"),
				new TGEDCOMAppFormat("ALTREE", "Agelong Tree"),
				new TGEDCOMAppFormat("AGES", "Ages!"),
				new TGEDCOMAppFormat("PAF", "Personal Ancestral File")
			};


			UserRefs = new string[5] {
				"",
				"РИ:Георгиевский кавалер",
				"СССР:ВОВ:Участник боевых действий",
				"СССР:ВОВ:Погиб в бою",
				"СССР:ВОВ:Труженик тыла"
			};


			CertaintyAssessments = new LSID[]
			{
				LSID.LSID_Cert_1, 
				LSID.LSID_Cert_2, 
				LSID.LSID_Cert_3, 
				LSID.LSID_Cert_4
			};


			GoalNames = new LSID[]
			{
				LSID.LSID_G_1, 
				LSID.LSID_G_2, 
				LSID.LSID_G_3, 
				LSID.LSID_G_4
			};


			CommunicationDirs = new LSID[]
			{
				LSID.LSID_CD_1, 
				LSID.LSID_CD_2
			};


			CommunicationNames = new LSID[]
			{
				LSID.LSID_Com_1, 
				LSID.LSID_Com_2, 
				LSID.LSID_Com_3, 
				LSID.LSID_Com_4, 
				LSID.LSID_Com_5, 
				LSID.LSID_Com_6
			};


			StatusNames = new LSID[]
			{
				LSID.LSID_RStat_1, 
				LSID.LSID_RStat_2, 
				LSID.LSID_RStat_3, 
				LSID.LSID_RStat_4, 
				LSID.LSID_RStat_5, 
				LSID.LSID_RStat_6
			};


			PriorityNames = new LSID[]
			{
				LSID.LSID_Prt_1, 
				LSID.LSID_Prt_2, 
				LSID.LSID_Prt_3, 
				LSID.LSID_Prt_4, 
				LSID.LSID_Prt_5
			};


			MediaTypes = new LSID[]
			{
				LSID.LSID_MT_01, 
				LSID.LSID_MT_02, 
				LSID.LSID_MT_03, 
				LSID.LSID_MT_04, 
				LSID.LSID_MT_05, 
				LSID.LSID_MT_06, 
				LSID.LSID_MT_07, 
				LSID.LSID_MT_08, 
				LSID.LSID_MT_09, 
				LSID.LSID_MT_10, 
				LSID.LSID_MT_11, 
				LSID.LSID_MT_12, 
				LSID.LSID_MT_13, 
				LSID.LSID_MT_14, 
				LSID.LSID_MT_15
			};


			GKStoreTypes = new StoreTypeRec[3] {
				new StoreTypeRec(LSID.LSID_STRef, ""),
				new StoreTypeRec(LSID.LSID_STStg, "stg:"),
				new StoreTypeRec(LSID.LSID_STArc, "arc:")
			};


			FamilyEvents = new S21[10] {
				new S21(LSID.LSID_Event, "EVEN"),
				new S21(LSID.LSID_FEvt_1, "ENGA"),
				new S21(LSID.LSID_FEvt_2, "MARR"),
				new S21(LSID.LSID_FEvt_3, "MARB"),
				new S21(LSID.LSID_FEvt_4, "MARC"),
				new S21(LSID.LSID_FEvt_5, "MARL"),
				new S21(LSID.LSID_FEvt_6, "MARS"),
				new S21(LSID.LSID_FEvt_7, "ANUL"),
				new S21(LSID.LSID_FEvt_8, "DIVF"),
				new S21(LSID.LSID_FEvt_9, "DIV")
			};


			DateCalendars = new LSID[]
			{
				LSID.LSID_Cal_Gregorian, 
				LSID.LSID_Cal_Julian, 
				LSID.LSID_Cal_Hebrew, 
				LSID.LSID_Cal_French, 
				LSID.LSID_Cal_Roman, 
				LSID.LSID_Unknown
			};

			
			DateKindRec[] array5 = new DateKindRec[10];
			array5[0] = new DateKindRec(LSID.LSID_DK_0,  (TDateControlsRange)2);
			array5[1] = new DateKindRec(LSID.LSID_DK_1,  (TDateControlsRange)4);
			array5[2] = new DateKindRec(LSID.LSID_DK_2,  (TDateControlsRange)2);
			array5[3] = new DateKindRec(LSID.LSID_DK_3,  (TDateControlsRange)6);
			array5[4] = new DateKindRec(LSID.LSID_DK_4,  (TDateControlsRange)2);
			array5[5] = new DateKindRec(LSID.LSID_DK_5,  (TDateControlsRange)4);
			array5[6] = new DateKindRec(LSID.LSID_DK_6,  (TDateControlsRange)6);
			array5[7] = new DateKindRec(LSID.LSID_DK_7,  (TDateControlsRange)2);
			array5[8] = new DateKindRec(LSID.LSID_DK_8,  (TDateControlsRange)2);
			array5[9] = new DateKindRec(LSID.LSID_DK_9,  (TDateControlsRange)2);
			DateKinds = array5;


			S7[] array6 = new S7[37];
			array6[ 0] = new S7(LSID.LSID_Event, "EVEN", TPersonEventKind.ekEvent);
			array6[ 1] = new S7(LSID.LSID_Birth, "BIRT", TPersonEventKind.ekEvent);
			array6[ 2] = new S7(LSID.LSID_Adoption, "ADOP", TPersonEventKind.ekEvent);
			array6[ 3] = new S7(LSID.LSID_Christening, "CHR", TPersonEventKind.ekEvent);
			array6[ 4] = new S7(LSID.LSID_Graduation, "GRAD", TPersonEventKind.ekEvent);
			array6[ 5] = new S7(LSID.LSID_Retirement, "RETI", TPersonEventKind.ekEvent);
			array6[ 6] = new S7(LSID.LSID_Naturalization, "NATU", TPersonEventKind.ekEvent);
			array6[ 7] = new S7(LSID.LSID_Emigration, "EMIG", TPersonEventKind.ekEvent);
			array6[ 8] = new S7(LSID.LSID_Immigration, "IMMI", TPersonEventKind.ekEvent);
			array6[ 9] = new S7(LSID.LSID_Census, "CENS", TPersonEventKind.ekEvent);
			array6[10] = new S7(LSID.LSID_LastWill, "WILL", TPersonEventKind.ekEvent);
			array6[11] = new S7(LSID.LSID_ProbateOfWill, "PROB", TPersonEventKind.ekEvent);
			array6[12] = new S7(LSID.LSID_Death, "DEAT", TPersonEventKind.ekEvent);
			array6[13] = new S7(LSID.LSID_Burial, "BURI", TPersonEventKind.ekEvent);
			array6[14] = new S7(LSID.LSID_Cremation, "CREM", TPersonEventKind.ekEvent);
			array6[15] = new S7(LSID.LSID_Fact, "FACT", TPersonEventKind.ekFact);
			array6[16] = new S7(LSID.LSID_Religion, "RELI", TPersonEventKind.ekFact);
			array6[17] = new S7(LSID.LSID_Nationality, "NATI", TPersonEventKind.ekFact);
			array6[18] = new S7(LSID.LSID_Residence, "RESI", TPersonEventKind.ekFact);
			array6[19] = new S7(LSID.LSID_PhysicalDesc, "DSCR", TPersonEventKind.ekFact);
			array6[20] = new S7(LSID.LSID_NationalIDNumber, "IDNO", TPersonEventKind.ekFact);
			array6[21] = new S7(LSID.LSID_SocialSecurityNumber, "SSN", TPersonEventKind.ekFact);
			array6[22] = new S7(LSID.LSID_ChildsCount, "NCHI", TPersonEventKind.ekFact);
			array6[23] = new S7(LSID.LSID_MarriagesCount, "NMR", TPersonEventKind.ekFact);
			array6[24] = new S7(LSID.LSID_Education, "EDUC", TPersonEventKind.ekFact);
			array6[25] = new S7(LSID.LSID_Occupation, "OCCU", TPersonEventKind.ekFact);
			array6[26] = new S7(LSID.LSID_Caste, "CAST", TPersonEventKind.ekFact);
			array6[27] = new S7(LSID.LSID_Property, "PROP", TPersonEventKind.ekFact);
			array6[28] = new S7(LSID.LSID_NobilityTitle, "TITL", TPersonEventKind.ekFact);
			array6[29] = new S7(LSID.LSID_Travel, "_TRAVEL", TPersonEventKind.ekFact);
			array6[30] = new S7(LSID.LSID_Hobby, "_HOBBY", TPersonEventKind.ekFact);
			array6[31] = new S7(LSID.LSID_Award, "_AWARD", TPersonEventKind.ekFact);
			array6[32] = new S7(LSID.LSID_Mili, "_MILI", TPersonEventKind.ekFact);
			array6[33] = new S7(LSID.LSID_MiliInd, "_MILI_IND", TPersonEventKind.ekFact);
			array6[34] = new S7(LSID.LSID_MiliDis, "_MILI_DIS", TPersonEventKind.ekFact);
			array6[35] = new S7(LSID.LSID_MiliRank, "_MILI_RANK", TPersonEventKind.ekFact);
			array6[36] = new S7(LSID.LSID_DNAMarkers, "_DNA", TPersonEventKind.ekFact);
			PersonEvents = array6;

			
			MarriageStatus = new MarStatusStruct[4] {
				new MarStatusStruct(LSID.LSID_Unknown, ""),
				new MarStatusStruct(LSID.LSID_MarrRegistered, "MARRIED"),
				new MarStatusStruct(LSID.LSID_MarrNotRegistered, "MARRNOTREG"),
				new MarStatusStruct(LSID.LSID_MarrDivorced, "NOTMARR")
			};


			SexData = new SexStruct[4] {
				new SexStruct(LSID.LSID_SexN, "N"),
				new SexStruct(LSID.LSID_SexM, "M"),
				new SexStruct(LSID.LSID_SexF, "F"),
				new SexStruct(LSID.LSID_SexU, "U")
			};


			RecordTypes = new LSID[]
			{
				LSID.LSID_None, 
				LSID.LSID_Person, 
				LSID.LSID_Family, 
				LSID.LSID_Note, 
				LSID.LSID_RPMultimedia, 
				LSID.LSID_Source, 
				LSID.LSID_Repository, 
				LSID.LSID_Group, 
				LSID.LSID_Research, 
				LSID.LSID_Task, 
				LSID.LSID_Communication, 
				LSID.LSID_Location, 
				LSID.LSID_None, 
				LSID.LSID_None
			};


			Restrictions = new string[]
			{
				"нет", 
				"конфиденциально", 
				"заперто", 
				"секретно"
			};
		}

		public static Regex InitMaskRegex([In] string Mask)
		{
			Regex result = null;

			if  (!string.IsNullOrEmpty(Mask))
			{
				string regex_str = "";
				int CurPos = 0;
				int Len = Mask.Length;
				if (CurPos < Len)
				{
					do
					{
						int I = Mask.IndexOfAny("*?".ToCharArray(), CurPos);
						if (I < CurPos) break;
						if (I > CurPos) {
							string part = Mask.Substring(CurPos, I - CurPos);
							regex_str += Regex.Escape(part);
						}

						char c = Mask[I];
						switch (c) {
							case '*':
								regex_str += ".*";
								break;
							case '?':
								regex_str += ".";
								break;
						}

						CurPos = I + 1;
					}
					while (CurPos < Len);
				}

				if (CurPos < Len) {
					string part = Mask.Substring(CurPos, Len - CurPos);
					regex_str += Regex.Escape(part);
				}

				result = new Regex(regex_str, RegexOptions.IgnoreCase);
			}

			return result;
		}

		public static bool MatchesRegex([In] string S, Regex regex)
		{
			return ((regex != null) ? regex.IsMatch(S) : false);
		}

		public static bool MatchesMask([In] string S, [In] string Mask)
		{
			Regex regex = InitMaskRegex(Mask);
			return MatchesRegex(S, regex);
		}

		public static string NumUpdate(int val, int up)
		{
			string result = val.ToString();
			if (result.Length < up)
			{
				StringBuilder sb = new StringBuilder(result);
				while (sb.Length < up) sb.Insert(0, '0');
				result = sb.ToString();
			}
			return result;
		}

		public static string SetAsName(string s)
		{
			string st = s.ToLower();
			char f = Char.ToUpper(st[0]);
			st = f + st.Substring(1);
			return st;
		}

		public static string ConStrings(StringList aStrings)
		{
			string Result = "";
			int num = aStrings.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (Result != "") Result += " ";
				Result += aStrings[i].Trim();
			}
			return Result;
		}

		public static string GetTempDir()
		{
			return Environment.GetEnvironmentVariable("TEMP");
		}

		public static string GetAppPath()
		{
			Module[] mods = System.Reflection.Assembly.GetExecutingAssembly().GetModules();
			string fn = mods[0].FullyQualifiedName;
			return Path.GetDirectoryName(fn) + "\\";
		}

		public static void LoadExtFile([In] string aFileName)
		{
			SysUtils.ShellExecute(0, "open", aFileName, "", "", 5);
		}

		public static string GetAppDataPath()
		{
			string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + "\\" + TGenEngine.AppTitle + "\\";
			if (!Directory.Exists(path)) Directory.CreateDirectory(path);
			return path;
		}

		public static void ShowMessage([In] string Msg)
		{
			MessageBox.Show(Msg, AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
		}

		public static void ShowError([In] string Msg)
		{
			MessageBox.Show(Msg, AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Hand);
		}

		public static DialogResult ShowQuestion([In] string Msg)
		{
			return MessageBox.Show(Msg, AppTitle, MessageBoxButtons.YesNo, MessageBoxIcon.Question);
		}

		private string[] GetFamilies(TGEDCOMIndividualRecord iRec)
		{
			string[] result = new string[1];
			string fam, nam, pat;
			iRec.aux_GetNameParts(out fam, out nam, out pat);
			bool female = (iRec.Sex == TGEDCOMSex.svFemale);

			if (female) {
				fam = fam.Trim();
				int p = fam.IndexOf('(');
				if (p >= 0) {
					string part = fam.Substring(0, p).Trim();
					result[0] = PrepareRusFamily(part, female);
					part = fam.Substring(p).Trim();
					part = part.Substring(1, part.Length-2);

					string[] parts = part.Split(',');
					for (int i = 0; i < parts.Length; i++) {
						string[] newres = new string[result.Length+1];
						result.CopyTo(newres, 0);
						result = newres;
						result[result.Length-1] = PrepareRusFamily(parts[i].Trim(), female);
					}
				} else {
					result[0] = PrepareRusFamily(fam, female);
				}
			} else {
				result[0] = fam;
			}

			return result;
		}

		public class IndividualRecordComparer: IComparer<ULIndividual>
		{
			public int Compare(ULIndividual x, ULIndividual y)
			{
				return string.Compare(x.Family, y.Family, false);
			}
		}

		public struct ULIndividual
		{
			public string Family;
			public TGEDCOMIndividualRecord iRec;
		}

		public List<ULIndividual> GetUnlinkedNamesakes()
		{
			List<ULIndividual> result = new List<ULIndividual>();

			Hashtable families = new Hashtable();
			for (int i = 0; i < FTree.RecordsCount; i++)
			{
				TGEDCOMRecord rec = FTree[i];

				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
					string[] fams = GetFamilies(iRec);
					for (int k = 0; k < fams.Length; k++)
					{
						string f = fams[k];
						if (f.Length > 1)
						{
							List<TGEDCOMIndividualRecord> ps = (List<TGEDCOMIndividualRecord>)families[f];
							if (ps == null) {
								ps = new List<TGEDCOMIndividualRecord>();
								families[f] = ps;
							}
							ps.Add(iRec);
						}
					}
				}
			}

			foreach (DictionaryEntry entry in families)
			{
				string fam = (string)entry.Key;
				List<TGEDCOMIndividualRecord> ps = (List<TGEDCOMIndividualRecord>)entry.Value;

				int i = 0;
				while (i < ps.Count)
				{
					TGEDCOMIndividualRecord iRec = ps[i];

					using (TList lst = new TList())
					{
						TreeTools.TreeWalk(iRec, TreeTools.TTreeWalkMode.twmAll, lst);
						for (int k = 0; k < lst.Count; k++)
						{
							TGEDCOMIndividualRecord item = lst[k] as TGEDCOMIndividualRecord;
							int idx = ps.IndexOf(item);
							if (item != iRec && idx >= 0 && idx > i) ps.RemoveAt(idx);
						}
					}

					i++;
				}

				if (ps.Count > 1) {
					for (i = 0; i < ps.Count; i++) {
						ULIndividual indiv;
						indiv.Family = fam;
						indiv.iRec = ps[i];
						result.Add(indiv);
					}
				}
			}

			result.Sort(new IndividualRecordComparer());

			return result;
		}


		public delegate void DuplicateFoundFunc(TGEDCOMIndividualRecord indi, List<TGEDCOMIndividualRecord> matches);

		public static List<TGEDCOMIndividualRecord> FindDuplicates(TGEDCOMIndividualRecord indi, TGEDCOMTree tree_B, float matchThreshold)
		{
			List<TGEDCOMIndividualRecord> matches = new List<TGEDCOMIndividualRecord>();

			for (int i = 0; i <= tree_B.RecordsCount - 1; i++)
			{
				if (tree_B[i] is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord matchIndi = tree_B[i] as TGEDCOMIndividualRecord;

					if (matchIndi != indi && indi.IsMatch(matchIndi, matchThreshold)) {
						matches.Add(matchIndi);
					}
				}
			}

			return matches;		
		}

		public static void FindDuplicates(TGEDCOMTree tree_A, TGEDCOMTree tree_B, float matchThreshold, DuplicateFoundFunc foundFunc)
		{		
			for (int i = 0; i <= tree_A.RecordsCount - 1; i++)
			{
				if (tree_A[i] is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord indi = tree_A[i] as TGEDCOMIndividualRecord;

					List<TGEDCOMIndividualRecord> matches = FindDuplicates(indi, tree_B, matchThreshold);
					if (matches.Count > 0) foundFunc(indi, matches);
				}
			}
		}

	}
}
