using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.IO.Compression;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore.Sys;
using GKUI;

namespace GKCore
{
	public enum TGEDCOMFormat : byte
	{
		gf_Unknown,
		gf_Native,
		gf_GENBOX,
		gf_ALTREE,
		gf_AGES,
		gf_PAF
	}

	public class TGenEngine : IDisposable
	{
		public const string AppTitle = "GEDKeeper2";

		private struct TRetCount
		{
			public ushort lngSubRows;
			public ushort lngCountLike;
		}

		public struct TSexRec
		{
			public LSID NameId;
			public string Sign;

			public TSexRec(LSID aName, string aSign) {
				this.NameId = aName;
				this.Sign = aSign;
			}
		}

		public struct S5
		{
			public LSID Name;
			public string StatSign;
			
			public S5(LSID aName, string aStatSign) {
				this.Name = aName;
				this.StatSign = aStatSign;
			}
		}

		public struct S7
		{
			public LSID Name;
			public string Sign;
			public TGenEngine.TPersonEventKind Kind;
			
			public S7(LSID aName, string aSign, TGenEngine.TPersonEventKind aKind) {
				this.Name = aName;
				this.Sign = aSign;
				this.Kind = aKind;
			}
		}

		public struct S9
		{
			public LSID Name;
			public TGenEngine.TDateControlsRange Dates;
			
			public S9(LSID aName, TGenEngine.TDateControlsRange aDates) {
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

		public struct S41
		{
			public LSID Name;
			public string Sign;
			
			public S41(LSID aName, string aSign) {
				this.Name = aName;
				this.Sign = aSign;
			}
		}

		public struct S32
		{
			public string Name;
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

		public class TPatriarchObj : IDisposable
		{
			public TGEDCOMIndividualRecord IRec;
			public int IBirthYear;
			public int IDescendantsCount;
			public int IDescGenerations;
			public List<byte> ILinks = new List<byte>();
			private bool Disposed_;

			public void Dispose()
			{
				if (!this.Disposed_)
				{
					this.Disposed_ = true;
				}
			}
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

		public struct TKinshipRec
		{
			public TEnumSet PrevRels;
			public TEnumSet CurrRels;
			public TGenEngine.TRelationKind FinRel;
			public sbyte Great;
			public sbyte Level;
		}

		public class TSyncRec
		{
			public TGEDCOMRecord MasterRecord;
			public TGEDCOMRecord UpdateRecord;
			public TGenEngine.TSyncState State;
			public string UpdateOldXRef;
			public string UpdateNewXRef;

			public void Free()
			{
				TObjectHelper.Free(this);
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

		[TSetElementType(typeof(byte)), Flags]
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

		public enum TGKStoreType : byte
		{
			gstReference,
			gstArchive,
			gstStorage
		}

		public enum TUserRef : byte
		{
			urCustom,
			urRI_StGeorgeCross,
			urUSSR_Soldier,
			urUSSR_FallInBattle,
			urUSSR_RearVeteran
		}

		//[TUInt32Subrange(1u, 4u, typeof(TGenEngine.TUserRef))]
		public enum TChartPersonSign : byte
		{
			urRI_StGeorgeCross = 1,
			urUSSR_Soldier,
			urUSSR_FallInBattle,
			urUSSR_RearVeteran
		}

		[Flags, TSetElementType(typeof(TGenEngine.TChartPersonSign))]
		public enum TChartPersonSigns : byte
		{
			urRI_StGeorgeCross = 2,
			urUSSR_Soldier = 4,
			urUSSR_FallInBattle = 8,
			urUSSR_RearVeteran = 16
		}

		public enum TTreeWalkMode : byte
		{
			twmAll,
			twmFamily,
			twmAncestors,
			twmDescendants,
			twmNone
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

		public enum TRelationKind : byte
		{
			rkNone,
			rkParent,
			rkSpouse,
			rkChild,
			rkFather,
			rkMother,
			rkHusband,
			rkWife,
			rkSon,
			rkDaughter,
			rkGrandfather,
			rkGrandmother,
			rkGrandson,
			rkGranddaughter,
			rkBrother,
			rkSister,
			rkSonInLaw,
			rkDaughterInLaw,
			rkHusbandFather,
			rkHusbandMother,
			rkWifeFather,
			rkWifeMother,
			rkUncle,
			rkAunt,
			rkNephew,
			rkNiece,
			rkCousinM,
			rkCousinF,
			rkSame,
			rkUndefined
		}

		public enum TSyncState : byte
		{
			ssUndefined,
			ssHasMaster,
			ssNoMaster
		}

		public static readonly string[] Restrictions;
		public static readonly LSID[] RecordTypes;
		public static readonly TGenEngine.TSexRec[] SexData;
		public static readonly TGenEngine.S5[] MarriageStatus;
		public static readonly TGenEngine.S7[] PersonEvents;
		public static readonly TGenEngine.S9[] DateKinds;
		public static readonly LSID[] DateCalendars;
		public static readonly TGenEngine.S21[] FamilyEvents;
		public static readonly TGenEngine.S41[] GKStoreType;
		public static readonly LSID[] MediaTypes;
		public static readonly LSID[] PriorityNames;
		public static readonly LSID[] StatusNames;
		public static readonly LSID[] CommunicationNames;
		public static readonly LSID[] CommunicationDirs;
		public static readonly LSID[] GoalNames;
		public static readonly LSID[] CertaintyAssessments;
		public static readonly TGenEngine.S32[] UserRefs;
		public static readonly TGenEngine.TGEDCOMAppFormat[] GEDCOMFormats;
		public static readonly LSID[] RelationKinds;
		public static readonly string[] RelationSigns;
		public TGenEngine.TKinshipRec[] Kinships;
		public static readonly string[] Numerals;
		public static readonly string[] NumKinship;
		private string FFileName;
		private TGEDCOMTree FTree;
		private bool Disposed_;


		public string ExtName
		{
			get { return this.GetExtName(); }
			set { this.SetExtName(value); }
		}


		public string FileName
		{
			get { return this.FFileName; }
			set { this.FFileName = value; }
		}


		public bool IsAdvanced
		{
			get { return this.GetIsAdvanced(); }
			set { this.SetIsAdvanced(value); }
		}


		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
		}

		public void RegisterKinship(TEnumSet aPrevRels, TEnumSet aCurrRels, TGenEngine.TRelationKind aFinRel, sbyte aGreat, sbyte aLevel)
		{
			TGenEngine.TKinshipRec[] kinships = this.Kinships;
			int len = (kinships != null) ? kinships.Length : 0;
			TGenEngine.TKinshipRec[] arg_1C_0 = this.Kinships;
			int num = len + 1;
			TGenEngine.TKinshipRec[] array = arg_1C_0;
			int arg_24_0;
			if ((arg_24_0 = num) < 0)
			{
				arg_24_0 = 0;
			}
			TGenEngine.TKinshipRec[] array2;
			TGenEngine.TKinshipRec[] expr_29 = array2 = new TGenEngine.TKinshipRec[arg_24_0];
			if (num > 0 && array != null)
			{
				int num2;
				if ((num2 = array.Length) > num)
				{
					num2 = num;
				}
				if (num2 > 0)
				{
					Array.Copy(array, array2, num2);
				}
			}
			this.Kinships = expr_29;
			this.Kinships[len].PrevRels = aPrevRels;
			this.Kinships[len].CurrRels = aCurrRels;
			this.Kinships[len].FinRel = aFinRel;
			this.Kinships[len].Great = aGreat;
			this.Kinships[len].Level = aLevel;
		}

		public void InitKinships()
		{
			this.Kinships = new TGenEngine.TKinshipRec[0];

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkNone
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather, 
				TGenEngine.TRelationKind.rkMother, 
				TGenEngine.TRelationKind.rkHusband, 
				TGenEngine.TRelationKind.rkWife, 
				TGenEngine.TRelationKind.rkSon, 
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkSame, 0, 0);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkHusband, 
				TGenEngine.TRelationKind.rkWife
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon, 
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkSame, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkMother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkHusband
			}), TGenEngine.TRelationKind.rkFather, 0, 0);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkWife
			}), TGenEngine.TRelationKind.rkMother, 0, 0);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkGrandfather, 
				TGenEngine.TRelationKind.rkGrandmother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon
			}), TGenEngine.TRelationKind.rkUncle, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkGrandfather, 
				TGenEngine.TRelationKind.rkGrandmother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkAunt, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkBrother, 
				TGenEngine.TRelationKind.rkSister
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon
			}), TGenEngine.TRelationKind.rkNephew, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkBrother, 
				TGenEngine.TRelationKind.rkSister
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkNiece, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkWife
			}), TGenEngine.TRelationKind.rkDaughterInLaw, 0, 0);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkDaughter
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkHusband
			}), TGenEngine.TRelationKind.rkSonInLaw, 0, 0);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkWife
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather
			}), TGenEngine.TRelationKind.rkWifeFather, 0, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkWife
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkMother
			}), TGenEngine.TRelationKind.rkWifeMother, 0, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkHusband
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather
			}), TGenEngine.TRelationKind.rkHusbandFather, 0, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkHusband
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkMother
			}), TGenEngine.TRelationKind.rkHusbandMother, 0, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather, 
				TGenEngine.TRelationKind.rkMother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather
			}), TGenEngine.TRelationKind.rkGrandfather, 0, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather, 
				TGenEngine.TRelationKind.rkMother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkMother
			}), TGenEngine.TRelationKind.rkGrandmother, 0, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather, 
				TGenEngine.TRelationKind.rkMother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon
			}), TGenEngine.TRelationKind.rkBrother, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather, 
				TGenEngine.TRelationKind.rkMother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkSister, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkGrandfather, 
				TGenEngine.TRelationKind.rkGrandmother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkFather
			}), TGenEngine.TRelationKind.rkGrandfather, 1, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkGrandfather, 
				TGenEngine.TRelationKind.rkGrandmother
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkMother
			}), TGenEngine.TRelationKind.rkGrandmother, 1, -1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon, 
				TGenEngine.TRelationKind.rkDaughter, 
				TGenEngine.TRelationKind.rkSonInLaw, 
				TGenEngine.TRelationKind.rkDaughterInLaw
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon
			}), TGenEngine.TRelationKind.rkGrandson, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon, 
				TGenEngine.TRelationKind.rkDaughter, 
				TGenEngine.TRelationKind.rkSonInLaw, 
				TGenEngine.TRelationKind.rkDaughterInLaw
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkGranddaughter, 0, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkGrandson, 
				TGenEngine.TRelationKind.rkGranddaughter
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkSon
			}), TGenEngine.TRelationKind.rkGrandson, 1, 1);

			this.RegisterKinship(TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkGrandson, 
				TGenEngine.TRelationKind.rkGranddaughter
			}), TEnumSet.Create(new Enum[]
			{
				TGenEngine.TRelationKind.rkDaughter
			}), TGenEngine.TRelationKind.rkGranddaughter, 1, 1);
		}

		public TGenEngine.TRelationKind FindKinship(TGenEngine.TRelationKind prev, TGenEngine.TRelationKind cur, out int great, out int level)
		{
			TGenEngine.TRelationKind Result = TGenEngine.TRelationKind.rkUndefined;
			great = 0;
			level = 0;
			TGenEngine.TKinshipRec[] kinships = this.Kinships;
			int num = ((kinships != null) ? kinships.Length : 0) - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.Kinships[i].PrevRels.InSet(prev) && this.Kinships[i].CurrRels.InSet(cur))
				{
					TGenEngine.TRelationKind rel = this.Kinships[i].FinRel;
					great = (int)this.Kinships[i].Great;
					level = (int)this.Kinships[i].Level;
					if (rel == TGenEngine.TRelationKind.rkSame)
					{
						rel = cur;
					}
					Result = rel;
				}
			}
			return Result;
		}

		private bool GetIsAdvanced()
		{
			return this.FTree.Header.FindTag("_ADVANCED", 0) != null;
		}

		private void SetIsAdvanced([In] bool Value)
		{
			if (Value)
			{
				if (this.FTree.Header.FindTag("_ADVANCED", 0) == null)
				{
					this.FTree.Header.AddTag("_ADVANCED", "", null);
				}
			}
			else
			{
				this.FTree.Header.DeleteTag("_ADVANCED");
			}
		}

		private string GetExtName()
		{
			TGEDCOMTag tag = this.FTree.Header.FindTag("_EXT_NAME", 0);
			string Result;
			if (tag == null)
			{
				Result = "";
			}
			else
			{
				Result = tag.StringValue;
			}
			return Result;
		}

		private void SetExtName([In] string Value)
		{
			if (Value != "")
			{
				TGEDCOMTag tag = this.FTree.Header.FindTag("_EXT_NAME", 0);
				if (tag == null)
				{
					tag = this.FTree.Header.AddTag("_EXT_NAME", "", null);
				}
				tag.StringValue = Value;
			} else {
				this.FTree.Header.DeleteTag("_EXT_NAME");
			}
		}

		private int PatriarchsCompare(object Item1, object Item2)
		{
			return (Item1 as TGenEngine.TPatriarchObj).IBirthYear - (Item2 as TGenEngine.TPatriarchObj).IBirthYear;
		}

		private static TGenEngine.TRetCount Matching(string StrA, string StrB, int lngLen)
		{
			TGenEngine.TRetCount Result;
			Result.lngSubRows = 0;
			Result.lngCountLike = 0;
			int arg_22_0 = 1;
			int num = ((StrA != null) ? StrA.Length : 0) - lngLen + 1;
			int PosStrA = arg_22_0;
			if (num >= PosStrA)
			{
				num++;
				do
				{
					string StrTempA = SysUtils.WStrCopy(StrA, PosStrA, lngLen);
					int arg_47_0 = 1;
					int num2 = ((StrB != null) ? StrB.Length : 0) - lngLen + 1;
					int PosStrB = arg_47_0;
					if (num2 >= PosStrB)
					{
						num2++;
						while (true)
						{
							string StrTempB = SysUtils.WStrCopy(StrB, PosStrB, lngLen);
							if (string.Compare(StrTempA, StrTempB, true) == 0)
							{
								break;
							}
							PosStrB++;
							if (PosStrB == num2)
							{
								goto IL_7F;
							}
						}
						Result.lngCountLike += 1;
					}
					IL_7F:
					Result.lngSubRows += 1;
					PosStrA++;
				}
				while (PosStrA != num);
			}
			return Result;
		}

		private static void CorrectIds(TGEDCOMTree aTree)
		{
			TfmProgress.ProgressInit(aTree.RecordsCount, GKL.LSList[469]);
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				int num = aTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = aTree.GetRecord(i);
					if (TGenEngine.GetId(rec) < 0)
					{
						string newXRef = aTree.XRefIndex_NewXRef(rec);
						repMap.AddXRef(rec, rec.XRef, newXRef);
						rec.XRef = newXRef;
					}
					TfmProgress.ProgressStep();
				}

				aTree.Header.ReplaceXRefs(repMap);
				TfmProgress.ProgressInit(repMap.Count, GKL.LSList[469]);

				int num2 = repMap.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
					TfmProgress.ProgressStep();
				}
			}
			finally
			{
				repMap.Free();
				TfmProgress.ProgressDone();
			}
		}

		private static void ReformNote(TGEDCOMTree aTree, TGEDCOMNotes note)
		{
			TStringList strData = new TStringList();
			try
			{
				strData.Text = note.Notes.Text;
				TGEDCOMNoteRecord noteRec = TGenEngine.CreateNoteEx(aTree, strData, null);
				note.Clear();
				note.Value = noteRec;
			}
			finally
			{
				strData.Free();
			}
		}

		private static void ReformMultimediaLink(TGEDCOMTree aTree, TGEDCOMMultimediaLink mmLink)
		{
			try
			{
				string title = mmLink.Title;
				TGEDCOMMultimediaRecord mmRec = new TGEDCOMMultimediaRecord(aTree, aTree, "", "");
				mmRec.InitNew();
				aTree.AddRecord(mmRec);

				int num = mmLink.FileReferences.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFileReference fr = mmLink.FileReferences[i];
					TGEDCOMFileReferenceWithTitle frt = new TGEDCOMFileReferenceWithTitle(aTree, mmRec, "", "");
					if (fr.MultimediaFormat != TGEDCOMMultimediaFormat.mfNone)
					{
						frt.MultimediaFormat = fr.MultimediaFormat;
					}
					if (fr.MediaType != TGEDCOMMediaType.mtNone)
					{
						frt.MediaType = fr.MediaType;
					}
					frt.LinkFile(fr.StringValue, TGEDCOMMediaType.mtUnknown, TGEDCOMMultimediaFormat.mfUnknown);
					mmRec.FileReferences.Add(frt);
				}
				mmLink.Clear();
				mmLink.Value = mmRec;
				mmLink.Title = title;
			}
			finally
			{
			}
		}

		private static void ReformSourceCitation(TGEDCOMTree aTree, TGEDCOMSourceCitation sourCit)
		{
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

		public static ushort DaysInAMonth([In] ushort AYear, [In] ushort AMonth)
		{
			return SysUtils.MonthDays[(AMonth == 2 && DateTime.IsLeapYear((int)AYear)) ? 1 : 0][(int)AMonth - 1];
		}

		public TGenEngine()
		{
			this.FTree = new TGEDCOMTree();
			this.InitKinships();
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
			TGEDCOMSourceRecord Result = null;
			int num = this.FTree.RecordsCount - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				TGEDCOMRecord rec;
				while (true)
				{
					rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMSourceRecord && (rec as TGEDCOMSourceRecord).FiledByEntry == aName)
					{
						break;
					}
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = (rec as TGEDCOMSourceRecord);
			}
			return Result;
		}

		public void GetSourcesList(TStringList aSources)
		{
			if (aSources != null)
			{
				aSources.Clear();
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
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

		public void GetCommonStats(out TGenEngine.TCommonStats aStats)
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
				TGEDCOMRecord rec = this.FTree.GetRecord(i);
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

		private void GetSimplePersonStat(TGenEngine.TStatMode aMode, List<TListVal> aVals, TGEDCOMIndividualRecord iRec)
		{
			string iName = TGenEngine.GetNameStr(iRec, true, false);

			switch (aMode)
			{
				case TGenEngine.TStatMode.smAncestors:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetAncestorsCount(iRec) - 1));
						break;
					}
				case TGenEngine.TStatMode.smDescendants:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetDescendantsCount(iRec) - 1));
						break;
					}
				case TGenEngine.TStatMode.smDescGenerations:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetDescGenerations(iRec)));
						break;
					}
				case TGenEngine.TStatMode.smChildsCount:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetChildsCount(iRec)));
						break;
					}
				case TGenEngine.TStatMode.smFirstbornAge:
					{
						TGEDCOMIndividualRecord iDummy;
						aVals.Add(new TListVal(iName, TGenEngine.GetFirstbornAge(iRec, out iDummy)));
						break;
					}
				case TGenEngine.TStatMode.smMarriages:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetMarriagesCount(iRec)));
						break;
					}
				case TGenEngine.TStatMode.smMarriageAge:
					{
						aVals.Add(new TListVal(iName, TGenEngine.GetMarriageAge(iRec)));
						break;
					}

				case TGenEngine.TStatMode.smFamilies:
				case TGenEngine.TStatMode.smNames:
				case TGenEngine.TStatMode.smPatronymics:
					{
						string V = "";
						string fam, nam, pat;
						TGenEngine.GetNameParts(iRec, out fam, out nam, out pat);
						switch (aMode) {
							case TGenEngine.TStatMode.smFamilies:
								V = TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMSex.svFemale);
								break;
							case TGenEngine.TStatMode.smNames:
								V = nam;
								break;
							case TGenEngine.TStatMode.smPatronymics:
								V = pat;
								break;
						}
						CheckVal(aVals, V);
						break;
					}
				case TGenEngine.TStatMode.smAge:
					{
						CheckVal(aVals, TGenEngine.GetAge(iRec, -1));
						break;
					}
				case TGenEngine.TStatMode.smLifeExpectancy:
					{
						CheckVal(aVals, TGenEngine.GetLifeExpectancy(iRec));
						break;
					}

				case TGenEngine.TStatMode.smBirthYears:
				case TGenEngine.TStatMode.smBirthTenYears:
				case TGenEngine.TStatMode.smDeathYears:
				case TGenEngine.TStatMode.smDeathTenYears:
				case TGenEngine.TStatMode.smBirthPlaces:
				case TGenEngine.TStatMode.smDeathPlaces:
					{
						string V = "?";
						int num2 = iRec.IndividualEvents.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMCustomEvent @event = iRec.IndividualEvents[j];
							int year;
							ushort k, d;
							TGenEngine.GetIndependentDate(@event.Detail.Date.Value, out year, out k, out d);
							if (Math.Abs(year) > 3000)
							{
								SysUtils.ShowMessage(@event.Detail.Date.StringValue + "/" + iName);
							}
							if (@event.Name == "BIRT")
							{
								switch (aMode) {
									case TGenEngine.TStatMode.smBirthYears:
										V = Convert.ToString(year);
										break;
									case TGenEngine.TStatMode.smBirthTenYears:
										V = Convert.ToString(year / 10 * 10);
										break;
									case TGenEngine.TStatMode.smBirthPlaces:
										V = @event.Detail.Place.StringValue;
										break;
								}
							}
							else
							{
								if (@event.Name == "DEAT")
								{
									switch (aMode) {
										case TGenEngine.TStatMode.smDeathYears:
											V = Convert.ToString(year);
											break;
										case TGenEngine.TStatMode.smDeathTenYears:
											V = Convert.ToString(year / 10 * 10);
											break;
										case TGenEngine.TStatMode.smDeathPlaces:
											V = @event.Detail.Place.StringValue;
											break;
									}
								}
							}
						}
						CheckVal(aVals, V);
						break;
					}

				case TGenEngine.TStatMode.smChildsDistribution:
					{
						CheckVal(aVals, TGenEngine.GetChildsCount(iRec).ToString());
						break;
					}
				case TGenEngine.TStatMode.smResidences:
					{
						CheckVal(aVals, TGenEngine.GetResidencePlace(iRec, false));
						break;
					}
				case TGenEngine.TStatMode.smOccupation:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "OCCU"));
						break;
					}
				case TGenEngine.TStatMode.smReligious:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "RELI"));
						break;
					}
				case TGenEngine.TStatMode.smNational:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "NATI"));
						break;
					}
				case TGenEngine.TStatMode.smEducation:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "EDUC"));
						break;
					}
				case TGenEngine.TStatMode.smCaste:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "CAST"));
						break;
					}
				case TGenEngine.TStatMode.smHobby:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_HOBBY"));
						break;
					}
				case TGenEngine.TStatMode.smAward:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_AWARD"));
						break;
					}
				case TGenEngine.TStatMode.smMili:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI"));
						break;
					}
				case TGenEngine.TStatMode.smMiliInd:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI_IND"));
						break;
					}
				case TGenEngine.TStatMode.smMiliDis:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI_DIS"));
						break;
					}
				case TGenEngine.TStatMode.smMiliRank:
					{
						CheckVal(aVals, TGenEngine.GetAttributeValue(iRec, "_MILI_RANK"));
						break;
					}
			}
		}

		public void GetSpecStats(TGenEngine.TStatMode aMode, List<TListVal> aVals)
		{
			if (aMode < TGenEngine.TStatMode.smDescGenerations)
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
					TGEDCOMRecord rec = this.FTree.GetRecord(i);

					if (rec is TGEDCOMIndividualRecord && aMode != TGenEngine.TStatMode.smSpousesDiff)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
						
						if (aMode != TGenEngine.TStatMode.smAAF_1 && aMode != TGenEngine.TStatMode.smAAF_2)
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
									case TGenEngine.TStatMode.smAAF_1:
										key = SysUtils.Trunc(GetIndependentYear(iRec, "BIRT") / 10 * 10).ToString();

										if (!xvals.TryGetValue(key, out vals_list))
										{
											vals_list = new List<int>();
											xvals.Add(key, vals_list);
										}
										vals_list.Add(fba);

										break;
									case TGenEngine.TStatMode.smAAF_2:
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
							aVals.Add(new TListVal(TGenEngine.GetFamilyStr(fRec), TGenEngine.GetSpousesDiff(fRec)));
						}
					}
				}
				
				if (aMode == TGenEngine.TStatMode.smAAF_1 || aMode == TGenEngine.TStatMode.smAAF_2)
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

		public void GetPatriarchsList(bool aProgress, bool aLinks, ref TObjectList aList, int aMinGens, bool aDates = true)
		{
			if (aProgress) TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[474]);

			TGenEngine.InitExtCounts(this.FTree, -1);
			try
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;

						string nf, nn, np;
						TGenEngine.GetNameParts(i_rec, out nf, out nn, out np);

						int bYear = _GetPatriarchsList_GetBirthYear(i_rec);
						int descGens = TGenEngine.GetDescGenerations(i_rec);
						bool res = i_rec.ChildToFamilyLinks.Count == 0;
						res = (res && i_rec.Sex == TGEDCOMSex.svMale);
						res = (res && /*nf != "" && nf != "?" &&*/ nn != "" && nn != "?");
						res = (res && descGens >= aMinGens);

						if (aDates) { res = (res && bYear > 0); }

						if (res)
						{
							TGenEngine.TPatriarchObj pObj = new TGenEngine.TPatriarchObj();
							pObj.IRec = i_rec;
							pObj.IBirthYear = bYear;
							pObj.IDescendantsCount = TGenEngine.GetDescendantsCount(i_rec) - 1;
							pObj.IDescGenerations = descGens;
							aList.Add(pObj);
						}
					}

					if (aProgress) TfmProgress.ProgressStep();
				}
				aList.Sort(new TListSortCompare(this.PatriarchsCompare));
			}
			finally
			{
				if (aProgress) TfmProgress.ProgressDone();
			}

			if (aLinks)
			{
				if (aProgress) TfmProgress.ProgressInit(aList.Count, GKL.LSList[475]);
				try
				{
					int num2 = aList.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						TGenEngine.TPatriarchObj patr = aList[i] as TGenEngine.TPatriarchObj;
						for (int j = i + 1; j <= num2; j++)
						{
							TGenEngine.TPatriarchObj patr2 = aList[j] as TGenEngine.TPatriarchObj;
							bool res = TGenEngine._GetPatriarchsList_SearchDesc(patr.IRec, patr2.IRec);
							if (res)
							{
								patr.ILinks.Add((byte)j);
								patr2.ILinks.Add((byte)i);
							}
						}

						if (aProgress) TfmProgress.ProgressStep();
					}
				}
				finally
				{
					if (aProgress) TfmProgress.ProgressDone();
				}
			}
		}

		public string GetPatriarchLinks(TObjectList lst, TGenEngine.TPatriarchObj pObj)
		{
			string Result = "";

			int num = pObj.ILinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				byte ix = pObj.ILinks[i];
				if (Result != "") Result += ", ";
				Result += TGenEngine.GetNameStr((lst[ix] as TGenEngine.TPatriarchObj).IRec, true, false);
			}
			return Result;
		}

		public string GetXDBFolder()
		{
			string md_path = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments/*ApplicationData*/);
			string Result = Path.Combine(md_path, AppTitle + "\\xap.db");
			if (!Directory.Exists(Result)) SysUtils.CreateDir(Result);
			return Result;
		}

		public bool CheckPath()
		{
			string path = SysUtils.ExtractFilePath(this.FFileName);
			bool Result = (path != "");
			if (!Result)
			{
				SysUtils.ShowError("Для типов хранения \"архив\" и \"хранилище\" новый файл БД нужно предварительно сохранить");
			}
			return Result;
		}

		public string GetArcFileName()
		{
			return SysUtils.ExtractFilePath(this.FFileName) + this.GetSpecExtName() + ".zip";
		}

		public string GetStoreFolder()
		{
			string Result = SysUtils.ExtractFilePath(this.FFileName) + this.GetSpecExtName() + "\\";
			if (!Directory.Exists(Result))
			{
				SysUtils.CreateDir(Result);
			}
			return Result;
		}

		public string GetSpecExtName()
		{
			string ext = this.ExtName;
			string result;
			if (ext == "")
			{
				result = Path.GetFileNameWithoutExtension(this.FFileName);
			}
			else
			{
				result = ext;
			}
			return result;
		}

		public TGenEngine.TGKStoreType GetStoreType(string aFileRef, ref string aFileName)
		{
			aFileName = aFileRef;
			TGenEngine.TGKStoreType Result;
			if (SysUtils.Pos(TGenEngine.GKStoreType[1].Sign, aFileRef) > 0)
			{
				Result = TGenEngine.TGKStoreType.gstArchive;
				aFileName = aFileName.Remove(0, 4);
			}
			else
			{
				if (SysUtils.Pos(TGenEngine.GKStoreType[2].Sign, aFileRef) > 0)
				{
					Result = TGenEngine.TGKStoreType.gstStorage;
					aFileName = aFileName.Remove(0, 4);
				}
				else
				{
					Result = TGenEngine.TGKStoreType.gstReference;
				}
			}
			return Result;
		}

		public void MediaLoad(string aRefName, out Stream aStream)
		{
			aStream = null;
			string target_fn = "";
			TGenEngine.TGKStoreType gst = this.GetStoreType(aRefName, ref target_fn);

			switch (gst) {
				case TGenEngine.TGKStoreType.gstStorage:
				{
					target_fn = this.GetStoreFolder() + target_fn;
					aStream = new FileStream(target_fn, FileMode.Open);
					break;
				}

				case TGenEngine.TGKStoreType.gstArchive:
				{
					aStream = new MemoryStream();
					if (!File.Exists(this.GetArcFileName())) {
						SysUtils.ShowError(GKL.LSList[476]);
					} else {
						this.ArcFileLoad(target_fn, aStream);
						aStream.Seek((long)0, SeekOrigin.Begin);
					}
					break;
				}

				case TGenEngine.TGKStoreType.gstReference:
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
				TGenEngine.TGKStoreType gst = this.GetStoreType(aRefName, ref target_fn);

				switch (gst) {
					case TGenEngine.TGKStoreType.gstStorage:
					{
						aFileName = this.GetStoreFolder() + target_fn;
						break;
					}
					case TGenEngine.TGKStoreType.gstArchive:
					{
						aFileName = SysUtils.GetTempDir() + "\\" + Path.GetFileName(target_fn);
						FileStream fs = new FileStream(aFileName, FileMode.Create);
						try
						{
							if (!File.Exists(this.GetArcFileName()))
							{
								SysUtils.ShowError(GKL.LSList[476]);
							}
							else
							{
								target_fn = SysUtils.StringReplace(target_fn, "\\", "/", TReplaceFlags.rfReplaceAll);
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
					case TGenEngine.TGKStoreType.gstReference:
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

		public void MediaSave(string aFileName, TGenEngine.TGKStoreType aStoreType, ref string aRefPath)
		{
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
			if (aStoreType != TGenEngine.TGKStoreType.gstReference)
			{
				if (aStoreType != TGenEngine.TGKStoreType.gstArchive)
				{
					if (aStoreType == TGenEngine.TGKStoreType.gstStorage)
					{
						sfn = spath + sfn;
						aRefPath = TGenEngine.GKStoreType[(int)aStoreType].Sign + sfn;
						string target_fn = this.GetStoreFolder() + sfn;
						File.Move(aFileName, target_fn);
					}
				}
				else
				{
					sfn = spath + sfn;
					aRefPath = TGenEngine.GKStoreType[(int)aStoreType].Sign + sfn;
					this.ArcFileSave(aFileName, sfn);
				}
			}
			else
			{
				aRefPath = aFileName;
			}
		}

		public TGEDCOMMultimediaLink SetPrimaryMultimediaRecord(TGEDCOMIndividualRecord aRec, TGEDCOMMultimediaRecord mmRec)
		{
			TGEDCOMMultimediaLink mmLink = null;
			int num = aRec.MultimediaLinks.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				while (!object.Equals(aRec.MultimediaLinks[i].Value, mmRec))
				{
					i++;
					if (i == num)
					{
						goto IL_3B;
					}
				}
				mmLink = aRec.MultimediaLinks[i];
			}
			IL_3B:
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

		public Bitmap GetPrimaryBitmap(TGEDCOMIndividualRecord aRec)
		{
			Bitmap result = null;
			try
			{
				TGEDCOMMultimediaLink mmLink = this.GetPrimaryMultimediaLink(aRec);
				if (mmLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					string target_fn = "";
					this.MediaLoad(mmRec.FileReferences[0].StringValue, ref target_fn);
					if (File.Exists(target_fn))
					{
						using (var fs = new System.IO.FileStream(target_fn, System.IO.FileMode.Open))
						{
    						result = new Bitmap(fs);
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetPrimaryBitmap(): " + E.Message);
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

		public void SortFamilyChilds(TGEDCOMFamilyRecord aFamily)
		{
			int num = aFamily.Childrens.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					int num2 = aFamily.Childrens.Count - 1;
					int j = i + 1;
					if (num2 >= j)
					{
						num2++;
						do
						{
							TGEDCOMIndividualRecord iChild = aFamily.Childrens[i].Value as TGEDCOMIndividualRecord;
							TGEDCOMCustomEvent iEv = TGenEngine.GetIndividualEvent(iChild, "BIRT");
							DateTime iDate;
							if (iEv != null)
							{
								iDate = TGenEngine.GEDCOMDateToDate(iEv.Detail.Date.Value);
							}
							else
							{
								iDate = new DateTime(0);
							}

							TGEDCOMIndividualRecord kChild = aFamily.Childrens[j].Value as TGEDCOMIndividualRecord;
							TGEDCOMCustomEvent kEv = TGenEngine.GetIndividualEvent(kChild, "BIRT");
							DateTime kDate;
							if (kEv != null)
							{
								kDate = TGenEngine.GEDCOMDateToDate(kEv.Detail.Date.Value);
							}
							else
							{
								kDate = new DateTime(0);
							}

							if (iDate > kDate) aFamily.Childrens.Exchange(i, j);

							j++;
						}
						while (j != num2);
					}
					i++;
				}
				while (i != num);
			}
		}


		public static string SexStr(TGEDCOMSex Sex)
		{
			return GKL.LSList[(int)TGenEngine.SexData[(int)Sex].NameId - 1];
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


		public static bool IsRecordAccess(TGEDCOMRestriction aRecRestriction, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aShieldState != TGenEngine.TShieldState.ssMaximum)
			{
				if (aShieldState != TGenEngine.TShieldState.ssMiddle)
				{
					if (aShieldState == TGenEngine.TShieldState.ssNone)
					{
						Result = true;
					}
				}
				else
				{
					Result = (((aRecRestriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
				}
			}
			else
			{
				Result = (((aRecRestriction == TGEDCOMRestriction.rnConfidential || aRecRestriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
			}
			return Result;
		}

		public static TGenEngine.TPersonEventKind GetPersonEventKindBySign(string aSign)
		{
			TGenEngine.TPersonEventKind Result = TGenEngine.TPersonEventKind.ekFact;
			int i = 0;
			while (TGenEngine.PersonEvents[i].Sign != aSign)
			{
				i++;
				if (i == 36)
				{
					return Result;
				}
			}
			Result = TGenEngine.PersonEvents[i].Kind;
			return Result;
		}

		public static int GetPersonEventIndex(string aSign)
		{
			int Result = -1;
			int i = 0;
			while (TGenEngine.PersonEvents[i].Sign != aSign)
			{
				i++;
				if (i == 36)
				{
					return Result;
				}
			}
			Result = i;
			return Result;
		}

		public static int GetFamilyEventIndex(string aSign)
		{
			int Result = -1;
			int i = 0;
			while (TGenEngine.FamilyEvents[i].Sign != aSign)
			{
				i++;
				if (i == 10)
				{
					return Result;
				}
			}
			Result = i;
			return Result;
		}

		public static int GetMarriageStatusIndex(string aSign)
		{
			int Result = 0;
			int i = 0;
			while (TGenEngine.MarriageStatus[i].StatSign != aSign)
			{
				i++;
				if (i == 4)
				{
					return Result;
				}
			}
			Result = i;
			return Result;
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
						Result = GKL.LSList[(int)TGenEngine.PersonEvents[ev].Name - 1];
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
							Result = GKL.LSList[(int)TGenEngine.FamilyEvents[ev].Name - 1];
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

		public static void GetNameParts(TGEDCOMIndividualRecord iRec, out string aFamily, out string aName, out string aPatronymic)
		{
			if (iRec != null && iRec.PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = iRec.PersonalNames[0];
				aFamily = np.Surname;
				if (SysUtils.GetTokensCount(np.FirstPart, ' ') > 1)
				{
					aName = SysUtils.GetToken(np.FirstPart, ' ', 1);
					aPatronymic = SysUtils.GetToken(np.FirstPart, ' ', 2);
				} else {
					aName = np.FirstPart;
					aPatronymic = "";
				}
			} else {
				aFamily = "";
				aName = "";
				aPatronymic = "";
			}
		}

		public static string GetNameStr(TGEDCOMIndividualRecord iRec, bool aByFamily, bool aPieces)
		{
			string Result;
			if (iRec != null && iRec.PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = iRec.PersonalNames[0];
				if (aByFamily)
				{
					Result = np.Surname + " " + np.FirstPart;
				}
				else
				{
					Result = np.FirstPart + " " + np.Surname;
				}
				if (aPieces)
				{
					string nick = np.Pieces.Nickname;
					if (nick != "") Result = Result + " [" + nick + "]";
				}
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public static string GetNickStr(TGEDCOMIndividualRecord iRec)
		{
			string Result;
			if (iRec != null)
			{
				TGEDCOMPersonalName np = iRec.PersonalNames[0];
				Result = np.Pieces.Nickname;
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public static string GetFamilyStr(TGEDCOMFamilyRecord aFamily)
		{
			string Result = "";

			TGEDCOMIndividualRecord spouse = aFamily.Husband.Value as TGEDCOMIndividualRecord;
			if (spouse == null)
			{
				Result += GKL.LSList[64];
			}
			else
			{
				Result += TGenEngine.GetNameStr(spouse, true, false);
			}

			Result += " - ";

			spouse = (aFamily.Wife.Value as TGEDCOMIndividualRecord);
			if (spouse == null)
			{
				Result += GKL.LSList[63];
			}
			else
			{
				Result += TGenEngine.GetNameStr(spouse, true, false);
			}

			return Result;
		}

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
				if (SysUtils.ShowQuestion(string.Concat(new string[]
				{
					"Не определяется пол человека по имени \"", 
					f_name, 
					" ", 
					f_pat, 
					"\". Это мужской пол?"
				})) == DialogResult.Yes)
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

		public static string GetXRefNum(TGEDCOMRecord aRecord)
		{
			string xref = aRecord.XRef;
			while (true)
			{
				char c = xref[0];
				if (c >= '0' && c < ':')
				{
					break;
				}
				xref = xref.Remove(0, 1);
			}
			return xref;
		}

		public static int GetId(TGEDCOMRecord aRecord)
		{
			int Result;
			try
			{
				string xref = TGenEngine.GetXRefNum(aRecord);
				Result = SysUtils.StrToIntDef(xref, 0);
			}
			catch (Exception)
			{
				Result = -1;
			}
			return Result;
		}

		public static string GEDCOMDateToStr(TGEDCOMDate aDate, TGenEngine.TDateFormat aFormat)
		{
			string Result = "";
			int year;
			ushort month;
			ushort day;
			aDate.GetDate(out year, out month, out day);

			if (year > 0 || month > 0 || day > 0)
			{
				if (aFormat != TGenEngine.TDateFormat.dfDD_MM_YYYY)
				{
					if (aFormat != TGenEngine.TDateFormat.dfYYYY_MM_DD)
					{
						if (aFormat == TGenEngine.TDateFormat.dfYYYY)
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
							Result = Result + SysUtils.NumUpdate((int)month, 2) + ".";
						}
						else
						{
							Result += "__.";
						}
						if (day > 0)
						{
							Result += SysUtils.NumUpdate((int)day, 2);
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
						Result = Result + SysUtils.NumUpdate((int)day, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (month > 0)
					{
						Result = Result + SysUtils.NumUpdate((int)month, 2) + ".";
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
			if (SysUtils.Pos("/", aDate) > 0)
			{
				aDate = SysUtils.StringReplace(aDate, "/", ".", TReplaceFlags.rfReplaceAll);
			}
			if (SysUtils.Pos("_", aDate) > 0)
			{
				aDate = SysUtils.StringReplace(aDate, "_", " ", TReplaceFlags.rfReplaceAll);
			}

			int cnt = SysUtils.GetTokensCount(aDate, '.');
			if (cnt < 3)
			{
				if (aException)
				{
					throw new Exception("date failed");
				}
			}
			else
			{
				string pd = SysUtils.GetToken(aDate, '.', 1).Trim();
				string pm = SysUtils.GetToken(aDate, '.', 2).Trim();
				string py = SysUtils.GetToken(aDate, '.', 3).Trim();

				if (pd != "")
				{
					Result = Result + pd + " ";
				}
				if (pm != "")
				{
					Result = Result + TGEDCOMDate.GEDCOMMonthArray[SysUtils.StrToInt(pm) - 1] + " ";
				}
				if (py != "")
				{
					Result += py;
				}
			}
			return Result;
		}

		public static string GEDCOMCustomDateToStr(TGEDCOMCustomDate aDate, TGenEngine.TDateFormat aFormat, bool aSign)
		{
			string Result = "";
			if (aDate == null)
			{
				Result = "";
			}
			else
			{
				if (aDate is TGEDCOMDateApproximated)
				{
					Result = TGenEngine.GEDCOMDateToStr(aDate as TGEDCOMDate, aFormat);
					if (aSign && (aDate as TGEDCOMDateApproximated).Approximated != TGEDCOMApproximated.daExact)
					{
						Result = "~ " + Result;
					}
				}
				else
				{
					if (aDate is TGEDCOMDateRange)
					{
						TGEDCOMDateRange dt_range = aDate as TGEDCOMDateRange;
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
						if (aDate is TGEDCOMDatePeriod)
						{
							TGEDCOMDatePeriod dt_period = aDate as TGEDCOMDatePeriod;
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
							if (aDate is TGEDCOMDate)
							{
								Result = TGenEngine.GEDCOMDateToStr(aDate as TGEDCOMDate, aFormat);
							}
						}
					}
				}
			}
			return Result;
		}

		public static string GEDCOMEventToDateStr(TGEDCOMCustomEvent aEvent, TGenEngine.TDateFormat aFormat, bool aSign)
		{
			string Result;
			if (aEvent == null)
			{
				Result = "";
			}
			else
			{
				Result = TGenEngine.GEDCOMCustomDateToStr(aEvent.Detail.Date.Value, aFormat, aSign);
			}
			return Result;
		}

		public static DateTime GEDCOMDateToDate(TGEDCOMCustomDate aDate)
		{
			DateTime Result;
			try
			{
				int year;
				ushort month;
				ushort day;
				TGenEngine.GetIndependentDate(aDate, out year, out month, out day);
				if (day == 0)
				{
					day = 1;
				}
				if (month == 0)
				{
					month = 1;
				}
				if (year <= 0)
				{
					Result = new DateTime(0);
				}
				else
				{
					Result = new DateTime(year, (int)month, (int)day);
				}
			}
			catch (Exception E)
			{
				int year = 0;
				ushort month = 0;
				ushort day = 0;
				SysUtils.LogWrite(string.Format("GEDCOMDateToDate(%d, %d, %d): ", new object[]
				{
					year, 
					month, 
					day
				}) + E.Message);
				SysUtils.LogWrite("Record (" + (aDate.ParentRecord as TGEDCOMRecord).XRef + "): invalid date");
				Result = new DateTime(0);
			}
			return Result;
		}

		public static TGEDCOMCustomEvent GetIndividualEvent(TGEDCOMIndividualRecord iRec, string evName)
		{
			TGEDCOMCustomEvent Result;
			if (iRec == null)
			{
				Result = null;
			}
			else
			{
				Result = iRec.GetIndividualEvent(evName);
			}
			return Result;
		}

		public static TGEDCOMFamilyEvent GetFamilyEvent(TGEDCOMFamilyRecord fRec, string evName)
		{
			TGEDCOMFamilyEvent Result = null;
			if (fRec != null)
			{
				int num = fRec.FamilyEvents.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					TGEDCOMFamilyEvent @event;
					while (true)
					{
						@event = fRec.FamilyEvents[i];
						if (@event.Name == evName)
						{
							break;
						}
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = @event;
				}
			}
			return Result;
		}

		public static string CompactDate([In] string aDate)
		{
			string Result = aDate;
			while (SysUtils.Pos("__.", Result) == 1)
			{
				Result = Result.Remove(0, 3);
			}
			return Result;
		}

		public static string GetBirthDate(TGEDCOMIndividualRecord iRec, TGenEngine.TDateFormat aFormat, bool aCompact)
		{
			TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(iRec, "BIRT");
			string Result;
			if (@event == null)
			{
				Result = "";
			}
			else
			{
				Result = TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, aFormat, false);
			}
			if (aCompact)
			{
				Result = TGenEngine.CompactDate(Result);
			}
			return Result;
		}

		public static string GetDeathDate(TGEDCOMIndividualRecord iRec, TGenEngine.TDateFormat aFormat, bool aCompact)
		{
			TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(iRec, "DEAT");
			string Result;
			if (@event == null)
			{
				Result = "";
			}
			else
			{
				Result = TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, aFormat, false);
			}
			if (aCompact)
			{
				Result = TGenEngine.CompactDate(Result);
			}
			return Result;
		}

		public static string GetLifeStr(TGEDCOMIndividualRecord iRec)
		{
			string Result = " (";

			string ds = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				ds = "?";
			}
			Result += ds;

			ds = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
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
			TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(iRec, "BIRT");
			string Result;
			if (@event == null)
			{
				Result = "";
			}
			else
			{
				Result = @event.Detail.Place.StringValue;
			}
			return Result;
		}

		public static string GetDeathPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(iRec, "DEAT");
			string Result;
			if (@event == null)
			{
				Result = "";
			}
			else
			{
				Result = @event.Detail.Place.StringValue;
			}
			return Result;
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

		public static string GetAttributeValue(TGEDCOMIndividualRecord iRec, string attrName)
		{
			TGEDCOMCustomEvent attr = TGenEngine.GetIndividualEvent(iRec, attrName);
			string Result;
			if (attr == null)
			{
				Result = "";
			}
			else
			{
				Result = attr.StringValue;
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
					st = GKL.LSList[(int)TGenEngine.PersonEvents[idx].Name - 1];
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

		public static string GetMarriageDate(TGEDCOMFamilyRecord fRec, TGenEngine.TDateFormat aFormat)
		{
			TGEDCOMFamilyEvent @event = TGenEngine.GetFamilyEvent(fRec, "MARR");
			string Result;
			if (@event == null)
			{
				Result = "";
			}
			else
			{
				Result = TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, aFormat, false);
			}
			return Result;
		}

		public static string GetEventDesc(TGEDCOMEventDetail evDetail)
		{
			string dt = TGenEngine.GEDCOMCustomDateToStr(evDetail.Date.Value, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
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

		public static void GetLifeDates(TGEDCOMIndividualRecord iRec, ref TGEDCOMCustomEvent aBirthEvent, ref TGEDCOMCustomEvent aDeathEvent)
		{
			aBirthEvent = null;
			aDeathEvent = null;

			int num = iRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = iRec.IndividualEvents[i];
				if (ev.Name == "BIRT")
				{
					aBirthEvent = ev;
				}
				else
				{
					if (ev.Name == "DEAT")
					{
						aDeathEvent = ev;
					}
				}
			}
		}

		public static void GetIndependentDate(TGEDCOMCustomDate aDate, out int AYear, out ushort AMonth, out ushort ADay)
		{
			AYear = -1;
			AMonth = 0;
			ADay = 0;

			if (aDate is TGEDCOMDateApproximated)
			{
				(aDate as TGEDCOMDate).GetDate(out AYear, out AMonth, out ADay);
			}
			else
			{
				if (aDate is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dt_range = aDate as TGEDCOMDateRange;
					if (dt_range.After.StringValue == "" && dt_range.Before.StringValue != "")
					{
						dt_range.Before.GetDate(out AYear, out AMonth, out ADay);
					}
					else
					{
						if (dt_range.After.StringValue != "" && dt_range.Before.StringValue == "")
						{
							dt_range.After.GetDate(out AYear, out AMonth, out ADay);
						}
						else
						{
							if (dt_range.After.StringValue != "" && dt_range.Before.StringValue != "")
							{
								dt_range.After.GetDate(out AYear, out AMonth, out ADay);
							}
						}
					}
				}
				else
				{
					if (aDate is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dt_period = aDate as TGEDCOMDatePeriod;
						if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue == "")
						{
							dt_period.DateFrom.GetDate(out AYear, out AMonth, out ADay);
						}
						else
						{
							if (dt_period.DateFrom.StringValue == "" && dt_period.DateTo.StringValue != "")
							{
								dt_period.DateTo.GetDate(out AYear, out AMonth, out ADay);
							}
							else
							{
								if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue != "")
								{
									dt_period.DateFrom.GetDate(out AYear, out AMonth, out ADay);
								}
							}
						}
					}
					else
					{
						if (aDate is TGEDCOMDate)
						{
							(aDate as TGEDCOMDate).GetDate(out AYear, out AMonth, out ADay);
						}
					}
				}
			}
		}

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign)
		{
			TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, evSign);
			int Result = -1;
			if (ev != null)
			{
				int year;
				ushort am, ad;
				TGenEngine.GetIndependentDate(ev.Detail.Date.Value, out year, out am, out ad);
				Result = year;
			}
			return Result;
		}

		public static double GetAbstractDate(TGEDCOMEventDetail aEventDetail)
		{
			double Result = 0.0;
			int y;
			ushort i;
			ushort d;
			TGenEngine.GetIndependentDate(aEventDetail.Date.Value, out y, out i, out d);
			if (y > 0)
			{
				Result = (double)y;
				if (i > 0)
				{
					Result = (Result + i / 12.0);
					if (d > 0)
					{
						Result = (Result + d / TGenEngine.DaysInAMonth((ushort)y, i) / 12.0);
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
				double y;
				if (ev1 == null)
				{
					y = -1.0;
				}
				else
				{
					y = TGenEngine.GetAbstractDate(ev1.Detail);
				}
				double y2;
				if (ev2 == null)
				{
					y2 = -1.0;
				}
				else
				{
					y2 = TGenEngine.GetAbstractDate(ev2.Detail);
				}
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
						Result = SysUtils.Trunc(y2 - y).ToString();
					}
				}
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
					TGEDCOMCustomEvent @event = iRec.IndividualEvents[i];
					if (@event.Name == "BIRT")
					{
						ev = @event;
					}
					else
					{
						if (@event.Name == "DEAT")
						{
							ev2 = @event;
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
				TGEDCOMCustomEvent ev = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent @event = iRec.IndividualEvents[i];
					if (@event.Name == "BIRT")
					{
						ev = @event;
					}
					else
					{
						if (@event.Name == "DEAT")
						{
							ev2 = @event;
						}
					}
				}

				if (ToYear == -1)
				{
					Result = TGenEngine.GetEventsYearsDiff(ev, ev2, ev2 == null);
				}
				else
				{
					if (ev == null)
					{
						Result = "";
					}
					else
					{
						ushort dummy;
						int i;
						TGenEngine.GetIndependentDate(ev.Detail.Date.Value, out i, out dummy, out dummy);
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
				TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(iRec, "BIRT");
				if (@event == null)
				{
				}
				else
				{
					double y3 = TGenEngine.GetAbstractDate(@event.Detail);

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

						int num2 = family.Childrens.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
							@event = TGenEngine.GetIndividualEvent(child, "BIRT");
							if (@event != null)
							{
								double y2tmp = TGenEngine.GetAbstractDate(@event.Detail);
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
				TGEDCOMCustomEvent iEvent = TGenEngine.GetIndividualEvent(iRec, "BIRT");
				if (iEvent == null)
				{
				}
				else
				{
					double y3 = TGenEngine.GetAbstractDate(iEvent.Detail);

					int num = iRec.SpouseToFamilyLinks.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
						TGEDCOMFamilyEvent fEvent = TGenEngine.GetFamilyEvent(family, "MARR");
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

		public static string GetDaysForBirth(TGEDCOMIndividualRecord iRec)
		{
			string Result = "";
			try
			{
				TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(iRec, "DEAT");
				if (@event != null)
				{
				}
				else
				{
					@event = TGenEngine.GetIndividualEvent(iRec, "BIRT");
					if (@event != null)
					{
						TGEDCOMDate dt = @event.Detail.Date.Value as TGEDCOMDate;
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
								Result = Convert.ToString(SysUtils.DaysBetween(new DateTime((int)cur_y, (int)cur_m, (int)cur_d), new DateTime(bd_y, (int)bd_m, (int)bd_d)));
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
				if (TGenEngine.GetPersonEventKindBySign(evSign) == TGenEngine.TPersonEventKind.ekEvent)
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
			TGEDCOMIndividualRecord Result = new TGEDCOMIndividualRecord(aTree, aTree, "", "");
			Result.InitNew();
			Result.Sex = aSex;
			TGEDCOMPersonalName pn = new TGEDCOMPersonalName(aTree, Result, "", "");
			pn.StringValue = string.Concat(new string[]
			{
				aName.Trim(), " ", aPatronymic.Trim(), " /", aFamily.Trim(), "/"
			});
			Result.AddPersonalName(pn);
			Result.ChangeDate.ChangeDateTime = DateTime.Now;
			aTree.AddRecord(Result);
			if (aBirthEvent)
			{
				TGenEngine.CreateEventEx(aTree, Result, "BIRT", "", "");
			}
			return Result;
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
			TStringList sl = new TStringList();
			try
			{
				sl.Text = aValue;
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
			TStringList strData = new TStringList();
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

		public static TGEDCOMNoteRecord CreateNoteEx(TGEDCOMTree aTree, TStrings aText, TGEDCOMRecord aRecord)
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

		public static void GetLocationLinks(TGEDCOMTree aTree, TGEDCOMLocationRecord aLocation, ref TStringList aList)
		{
			int num = aTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = aTree.GetRecord(i);
				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;

					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMCustomEvent @event = i_rec.IndividualEvents[j];
						if (object.Equals(@event.Detail.Place.Location.Value, aLocation))
						{
							aList.Add(TGenEngine.GenRecordLink(aTree, rec, true) + ", " + TGenEngine.GetEventName(@event).ToLower());
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
							TGEDCOMCustomEvent @event = f_rec.FamilyEvents[j];
							if (object.Equals(@event.Detail.Place.Location.Value, aLocation))
							{
								aList.Add(TGenEngine.GenRecordLink(aTree, rec, true) + ", " + TGenEngine.GetEventName(@event).ToLower());
							}
						}
					}
				}
			}
		}

		public static bool IsMatchesMask([In] string aName, [In] string aMask)
		{
			bool Result = false;
			if (aName != "" && aMask != "")
			{
				string strx = aName.ToLower();
				string[] sts = aMask.ToLower().Split(new char[] { '|' });

				int num = ((sts != null) ? sts.Length : 0) - 1;
				for (int i = 0; i <= num; i++)
				{
					Result = (Result || SysUtils.MatchesMask(strx, sts[i]));
				}
			}
			return Result;
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
							sign = GKL.LSList[(int)TGenEngine.RecordTypes[(int)aRecord.RecordType] - 1] + ": ";
						}
					}
					else
					{
						sign = "";
					}
				}
				string st;
				switch (aRecord.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						st = TGenEngine.GetNameStr(aRecord as TGEDCOMIndividualRecord, true, false);
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						st = TGenEngine.GetFamilyStr(aRecord as TGEDCOMFamilyRecord);
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						st = (aRecord as TGEDCOMMultimediaRecord).FileReferences[0].Title;
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtSource:
					{
						st = (aRecord as TGEDCOMSourceRecord).FiledByEntry;
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						st = (aRecord as TGEDCOMRepositoryRecord).RepositoryName;
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						st = (aRecord as TGEDCOMGroupRecord).GroupName;
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						st = (aRecord as TGEDCOMResearchRecord).ResearchName;
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtTask:
					{
						st = TGenEngine.GetTaskGoalStr(aTree, aRecord as TGEDCOMTaskRecord);
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						st = (aRecord as TGEDCOMCommunicationRecord).CommName;
						goto IL_13D;
					}
					case TGEDCOMRecordType.rtLocation:
					{
						st = (aRecord as TGEDCOMLocationRecord).LocationName;
						goto IL_13D;
					}
				}
				st = aRecord.XRef;
				IL_13D:
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
				string nm = TGenEngine.GetNameStr(corresponder, true, false);
				if (aLink)
				{
					nm = TGenEngine.HyperLink(corresponder.XRef, nm, 0);
				}
				Result = "[" + GKL.LSList[(int)TGenEngine.CommunicationDirs[(int)dir] - 1] + "] " + nm;
			}
			return Result;
		}

		public static void GetTaskGoal(TGEDCOMTree aTree, TGEDCOMTaskRecord aRec, ref TGoalType aType, ref TGEDCOMRecord aGoalRec)
		{
			aGoalRec = aTree.XRefIndex_Find(TGEDCOMObject.CleanXRef(aRec.Goal));
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
			if (gt != TGoalType.gtIndividual)
			{
				if (gt != TGoalType.gtFamily)
				{
					if (gt != TGoalType.gtSource)
					{
						if (gt == TGoalType.gtOther)
						{
							Result = aRec.Goal;
						}
					}
					else
					{
						Result = ((TGEDCOMSourceRecord)tempRec).FiledByEntry;
					}
				}
				else
				{
					Result = TGenEngine.GetFamilyStr((TGEDCOMFamilyRecord)tempRec);
				}
			}
			else
			{
				Result = TGenEngine.GetNameStr((TGEDCOMIndividualRecord)tempRec, true, false);
			}
			if (gt != TGoalType.gtOther)
			{
				Result = "[" + GKL.LSList[(int)TGenEngine.GoalNames[(int)gt] - 1] + "] " + Result;
			}
			return Result;
		}

		public static int IndistinctMatching(int MaxMatching, string strInputMatching, string strInputStandart)
		{
			int Result = 0;
			if (MaxMatching != 0 && (strInputMatching != null && strInputMatching.Length != 0) && (strInputStandart != null && strInputStandart.Length != 0))
			{
				TGenEngine.TRetCount gret;
				gret.lngCountLike = 0;
				gret.lngSubRows = 0;
				int lngCurLen = 1;
				if (MaxMatching >= lngCurLen)
				{
					int num = MaxMatching + 1;
					do
					{
						TGenEngine.TRetCount tret = TGenEngine.Matching(strInputMatching, strInputStandart, lngCurLen);
						gret.lngCountLike = (ushort)(gret.lngCountLike + tret.lngCountLike);
						gret.lngSubRows = (ushort)(gret.lngSubRows + tret.lngSubRows);
						tret = TGenEngine.Matching(strInputStandart, strInputMatching, lngCurLen);
						gret.lngCountLike = (ushort)(gret.lngCountLike + tret.lngCountLike);
						gret.lngSubRows = (ushort)(gret.lngSubRows + tret.lngSubRows);
						lngCurLen++;
					}
					while (lngCurLen != num);
				}
				if (gret.lngSubRows != 0)
				{
					Result = (int)SysUtils.Trunc(gret.lngCountLike / gret.lngSubRows * 100.0);
				}
			}
			return Result;
		}

		public static string ClearFamily(string aFamily)
		{
			int p = SysUtils.Pos(" (", aFamily);
			string Result;
			if (p > 0)
			{
				Result = SysUtils.WStrCopy(aFamily, 1, p - 1);
			}
			else
			{
				Result = aFamily;
			}
			return Result;
		}

		public static string PrepareRusFamily(string f, bool aFemale)
		{
			int p = SysUtils.Pos(" (", f);
			if (p > 0)
			{
				f = SysUtils.WStrCopy(f, 1, p - 1);
			}
			if (((f != null) ? f.Length : 0) > 0)
			{
				if (aFemale)
				{
					if (f[((f != null) ? f.Length : 0) - 1] == 'а')
					{
						f = SysUtils.WStrCopy(f, 1, ((f != null) ? f.Length : 0) - 1);
					}
					else
					{
						if (SysUtils.WStrCopy(f, ((f != null) ? f.Length : 0) - 2, 3) == "кая")
						{
							f = SysUtils.WStrCopy(f, 1, ((f != null) ? f.Length : 0) - 3) + "кий";
						}
						else
						{
							if (SysUtils.WStrCopy(f, ((f != null) ? f.Length : 0) - 2, 3) == "ная")
							{
								f = SysUtils.WStrCopy(f, 1, ((f != null) ? f.Length : 0) - 3) + "ный";
							}
						}
					}
				}
				if (f[0] == '(' && f[((f != null) ? f.Length : 0) - 1] == ')')
				{
					f = "?";
				}
			}
			else
			{
				f = "?";
			}
			return f;
		}

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
				TGEDCOMRecord rec = aTree.GetRecord(i);
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
						TGEDCOMIndividualRecord anc = family.Husband.Value as TGEDCOMIndividualRecord;
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

		public static int GetMarriagesCount(TGEDCOMIndividualRecord aPerson)
		{
			int Result;
			if (aPerson == null)
			{
				Result = 0;
			}
			else
			{
				Result = aPerson.SpouseToFamilyLinks.Count;
			}
			return Result;
		}

		public static int GetSpousesDiff(TGEDCOMFamilyRecord fRec)
		{
			int Result = 0;
			try
			{
				double y = -1.0;
				double y2 = -1.0;
				TGEDCOMIndividualRecord h = fRec.Husband.Value as TGEDCOMIndividualRecord;
				TGEDCOMIndividualRecord w = fRec.Wife.Value as TGEDCOMIndividualRecord;
				if (h == null || w == null)
				{
				}
				else
				{
					TGEDCOMCustomEvent @event = TGenEngine.GetIndividualEvent(h, "BIRT");
					if (@event != null)
					{
						y = TGenEngine.GetAbstractDate(@event.Detail);
					}
					@event = TGenEngine.GetIndividualEvent(w, "BIRT");
					if (@event != null)
					{
						y2 = TGenEngine.GetAbstractDate(@event.Detail);
					}
					if (y > (double)0f && y2 > (double)0f)
					{
						Result = (int)SysUtils.Trunc((Math.Abs((y2 - y))));
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetSpousesDiff(): " + E.Message);
			}
			return Result;
		}

		public static void CheckRecord(TGEDCOMTree aTree, TGEDCOMRecord aRec, TGEDCOMFormat aFormat)
		{
			if (aRec.UID == null || aRec.UID == "")
			{
				aRec.NewUID();
			}

			if (aFormat != TGEDCOMFormat.gf_Native)
			{
				int num = aRec.MultimediaLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMMultimediaLink mmLink = aRec.MultimediaLinks[i];
					if (!mmLink.IsPointer) TGenEngine.ReformMultimediaLink(aTree, mmLink);
				}

				num = aRec.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMNotes note = aRec.Notes[i];
					if (!note.IsPointer) TGenEngine.ReformNote(aTree, note);
				}

				num = aRec.SourceCitations.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMSourceCitation sourCit = aRec.SourceCitations[i];
					if (!sourCit.IsPointer) TGenEngine.ReformSourceCitation(aTree, sourCit);
				}
			}

			TGEDCOMRecordType recordType = aRec.RecordType;
			if (recordType != TGEDCOMRecordType.rtIndividual)
			{
				if (recordType != TGEDCOMRecordType.rtFamily)
				{
					if (recordType == TGEDCOMRecordType.rtGroup)
					{
						TGenEngine._CheckRecord_CheckGroup((TGEDCOMGroupRecord)aRec);
					}
				}
				else
				{
					_CheckRecord_CheckFamily(aTree, aFormat, (TGEDCOMFamilyRecord)aRec);
				}
			}
			else
			{
				_CheckRecord_CheckPerson(aTree, aFormat, (TGEDCOMIndividualRecord)aRec);
			}
		}

		public static TGEDCOMFormat GetGEDCOMFormat(TGEDCOMTree aTree)
		{
			string sour = aTree.Header.Source;
			TGEDCOMFormat gf = TGEDCOMFormat.gf_Native;
			TGEDCOMFormat Result;
			while (TGenEngine.GEDCOMFormats[(int)gf].Sign != sour)
			{
				gf++;
				if (gf == (TGEDCOMFormat)6)
				{
					Result = TGEDCOMFormat.gf_Unknown;
					return Result;
				}
			}
			Result = gf;
			return Result;
		}

		public static bool CheckGEDCOMFormat(TGEDCOMTree aTree)
		{
			TfmProgress.ProgressInit(aTree.RecordsCount, GKL.LSList[470]);
			bool Result;
			try
			{
				TGEDCOMFormat format = TGenEngine.GetGEDCOMFormat(aTree);
				bool idCheck = true;
				int i = 0;
				while (i < aTree.RecordsCount)
				{
					TGEDCOMRecord rec = aTree.GetRecord(i);
					TGenEngine.CheckRecord(aTree, rec, format);
					if (format != TGEDCOMFormat.gf_Native && idCheck && TGenEngine.GetId(rec) < 0)
					{
						idCheck = false;
					}
					i++;
					TfmProgress.ProgressStep();
				}
				if (!idCheck && SysUtils.ShowQuestion(GKL.LSList[471]) == DialogResult.Yes)
				{
					TGenEngine.CorrectIds(aTree);
				}
				Result = true;
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
			return Result;
		}

		public static void TreeWalk(TGEDCOMIndividualRecord iRec, TGenEngine.TTreeWalkMode aMode, TList aList)
		{
			if (iRec != null && aList.IndexOf(iRec) < 0)
			{
				aList.Add(iRec);
				if (aMode != TGenEngine.TTreeWalkMode.twmNone)
				{
					if ((aMode == TGenEngine.TTreeWalkMode.twmAll || aMode == TGenEngine.TTreeWalkMode.twmAncestors) && iRec.ChildToFamilyLinks.Count > 0)
					{
						TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
						TGEDCOMIndividualRecord rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
						TGenEngine.TreeWalk(rel_person, aMode, aList);
						rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
						TGenEngine.TreeWalk(rel_person, aMode, aList);
					}
					if (aMode < TGenEngine.TTreeWalkMode.twmAncestors || aMode == TGenEngine.TTreeWalkMode.twmDescendants)
					{
						int num = iRec.SpouseToFamilyLinks.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
							TGEDCOMPointer sp;
							if (iRec.Sex == TGEDCOMSex.svMale)
							{
								sp = family.Wife;
							}
							else
							{
								sp = family.Husband;
							}
							TGenEngine.TTreeWalkMode int_mode;
							if (aMode == TGenEngine.TTreeWalkMode.twmAll)
							{
								int_mode = TGenEngine.TTreeWalkMode.twmAll;
							}
							else
							{
								int_mode = TGenEngine.TTreeWalkMode.twmNone;
							}
							TGEDCOMIndividualRecord rel_person = sp.Value as TGEDCOMIndividualRecord;
							TGenEngine.TreeWalk(rel_person, int_mode, aList);
							if (aMode != TGenEngine.TTreeWalkMode.twmAll)
							{
								if (aMode != TGenEngine.TTreeWalkMode.twmFamily)
								{
									if (aMode == TGenEngine.TTreeWalkMode.twmDescendants)
									{
										int_mode = TGenEngine.TTreeWalkMode.twmDescendants;
									}
								}
								else
								{
									int_mode = TGenEngine.TTreeWalkMode.twmNone;
								}
							}
							else
							{
								int_mode = TGenEngine.TTreeWalkMode.twmAll;
							}

							int num2 = family.Childrens.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								rel_person = (family.Childrens[j].Value as TGEDCOMIndividualRecord);
								TGenEngine.TreeWalk(rel_person, int_mode, aList);
							}
						}
					}
				}
			}
		}

		public static void TreeMerge(TGEDCOMTree aMainTree, string aFileName, TextBox aLog)
		{
			if (aLog != null)
			{
				aLog.Clear();
				aLog.AppendText(string.Format(GKL.LSList[472], new object[]
				{
					aMainTree.RecordsCount.ToString()
				}) + "\r\n");
			}
			TGEDCOMTree extTree = new TGEDCOMTree();
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				extTree.LoadFromFile(aFileName);
				extTree.Header.Clear();
				while (extTree.RecordsCount > 0)
				{
					TGEDCOMRecord rec = extTree.Extract(0);
					string newXRef = aMainTree.XRefIndex_NewXRef(rec);
					repMap.AddXRef(rec, rec.XRef, newXRef);
					rec.XRef = newXRef;
					rec.ResetOwner(aMainTree);
					aMainTree.AddRecord(rec);
				}

				int num = repMap.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
				}

				if (aLog != null)
				{
					aLog.AppendText(string.Format(GKL.LSList[472], new object[]
					{
						Convert.ToString(aMainTree.RecordsCount)
					}) + "\r\n");
				}
			}
			finally
			{
				repMap.Free();
				extTree.Dispose();
			}
		}

		public static void TreeSync(TGEDCOMTree aMainTree, string aFileName, TextBox aLog)
		{
			aLog.Clear();
			TGEDCOMTree extTree = new TGEDCOMTree();
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			TObjectList sync_list = new TObjectList(true);
			try
			{
				extTree.LoadFromFile(aFileName);
				extTree.Header.Clear();
				TGenEngine.CheckGEDCOMFormat(extTree);

				int num = extTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = extTree.GetRecord(i);
					sync_list.Add(new TGenEngine.TSyncRec
					{
						MasterRecord = null, 
						UpdateRecord = rec, 
						State = TGenEngine.TSyncState.ssUndefined, 
						UpdateOldXRef = "", 
						UpdateNewXRef = ""
					});
				}

				int num2 = sync_list.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TGenEngine.TSyncRec sync_rec = sync_list[i] as TGenEngine.TSyncRec;
					TGEDCOMRecord rec = aMainTree.FindUID(sync_rec.UpdateRecord.UID);
					if (rec != null)
					{
						sync_rec.MasterRecord = rec;
						sync_rec.State = TGenEngine.TSyncState.ssHasMaster;
					}
					else
					{
						sync_rec.State = TGenEngine.TSyncState.ssNoMaster;
						rec = extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
						string newXRef = aMainTree.XRefIndex_NewXRef(rec);
						repMap.AddXRef(rec, rec.XRef, newXRef);
						rec.XRef = newXRef;
						rec.ResetOwner(aMainTree);
						aMainTree.AddRecord(rec);
					}
				}

				int num3 = repMap.Count - 1;
				for (int i = 0; i <= num3; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
				}

				int num4 = extTree.RecordsCount - 1;
				for (int i = 0; i <= num4; i++)
				{
					TGEDCOMRecord rec = extTree.GetRecord(i);
					rec.ReplaceXRefs(repMap);
				}

				int num5 = sync_list.Count - 1;
				for (int i = 0; i <= num5; i++)
				{
					TGenEngine.TSyncRec sync_rec = sync_list[i] as TGenEngine.TSyncRec;
					if (sync_rec.State == TGenEngine.TSyncState.ssHasMaster)
					{
						TGEDCOMRecord rec = extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
						rec.XRef = aMainTree.XRefIndex_NewXRef(rec);
						rec.ResetOwner(aMainTree);
						aMainTree.AddRecord(rec);
						string backUID = sync_rec.MasterRecord.UID;
						sync_rec.UpdateRecord.MoveTo(sync_rec.MasterRecord, true);
						sync_rec.MasterRecord.UID = backUID;
						aMainTree.DeleteRecord(rec);
					}
				}
				aLog.AppendText(GKL.LSList[473] + "\r\n");
			}
			finally
			{
				sync_list.Free();
				repMap.Free();
				extTree.Dispose();
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		static TGenEngine()
		{
			TGenEngine.NumKinship = new string[]
			{
				"-", 
				"юродный", 
				"юродная", 
				""
			};

			TGenEngine.Numerals = new string[]
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

			TGenEngine.RelationSigns = new string[]
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

			TGenEngine.RelationKinds = new LSID[]
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

			TGenEngine.TGEDCOMAppFormat[] afs = new TGenEngine.TGEDCOMAppFormat[6];
			afs[0] = new TGEDCOMAppFormat("", "");
			afs[1] = new TGEDCOMAppFormat("GEDKeeper", "");
			afs[2] = new TGEDCOMAppFormat("GENBOX", "Genbox Family History");
			afs[3] = new TGEDCOMAppFormat("ALTREE", "Agelong Tree");
			afs[4] = new TGEDCOMAppFormat("AGES", "Ages!");
			afs[5] = new TGEDCOMAppFormat("PAF", "Personal Ancestral File");
			TGenEngine.GEDCOMFormats = afs;

			TGenEngine.S32[] array2 = new TGenEngine.S32[5];
			array2[0].Name = "";
			array2[1].Name = "РИ:Георгиевский кавалер";
			array2[2].Name = "СССР:ВОВ:Участник боевых действий";
			array2[3].Name = "СССР:ВОВ:Погиб в бою";
			array2[4].Name = "СССР:ВОВ:Труженик тыла";
			TGenEngine.UserRefs = array2;

			TGenEngine.CertaintyAssessments = new LSID[]
			{
				LSID.LSID_Cert_1, 
				LSID.LSID_Cert_2, 
				LSID.LSID_Cert_3, 
				LSID.LSID_Cert_4
			};

			TGenEngine.GoalNames = new LSID[]
			{
				LSID.LSID_G_1, 
				LSID.LSID_G_2, 
				LSID.LSID_G_3, 
				LSID.LSID_G_4
			};

			TGenEngine.CommunicationDirs = new LSID[]
			{
				LSID.LSID_CD_1, 
				LSID.LSID_CD_2
			};

			TGenEngine.CommunicationNames = new LSID[]
			{
				LSID.LSID_Com_1, 
				LSID.LSID_Com_2, 
				LSID.LSID_Com_3, 
				LSID.LSID_Com_4, 
				LSID.LSID_Com_5, 
				LSID.LSID_Com_6
			};

			TGenEngine.StatusNames = new LSID[]
			{
				LSID.LSID_RStat_1, 
				LSID.LSID_RStat_2, 
				LSID.LSID_RStat_3, 
				LSID.LSID_RStat_4, 
				LSID.LSID_RStat_5, 
				LSID.LSID_RStat_6
			};

			TGenEngine.PriorityNames = new LSID[]
			{
				LSID.LSID_Prt_1, 
				LSID.LSID_Prt_2, 
				LSID.LSID_Prt_3, 
				LSID.LSID_Prt_4, 
				LSID.LSID_Prt_5
			};

			TGenEngine.MediaTypes = new LSID[]
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


			TGenEngine.S41[] array3 = new TGenEngine.S41[3];
			array3[0] = new S41(LSID.LSID_STRef, "");
			array3[1] = new S41(LSID.LSID_STArc, "arc:");
			array3[2] = new S41(LSID.LSID_STStg, "stg:");
			TGenEngine.GKStoreType = array3;


			TGenEngine.S21[] array4 = new TGenEngine.S21[10];
			array4[0] = new S21(LSID.LSID_Event, "EVEN");
			array4[1] = new S21(LSID.LSID_FEvt_1, "ENGA");
			array4[2] = new S21(LSID.LSID_FEvt_2, "MARR");
			array4[3] = new S21(LSID.LSID_FEvt_3, "MARB");
			array4[4] = new S21(LSID.LSID_FEvt_4, "MARC");
			array4[5] = new S21(LSID.LSID_FEvt_5, "MARL");
			array4[6] = new S21(LSID.LSID_FEvt_6, "MARS");
			array4[7] = new S21(LSID.LSID_FEvt_7, "ANUL");
			array4[8] = new S21(LSID.LSID_FEvt_8, "DIVF");
			array4[9] = new S21(LSID.LSID_FEvt_9, "DIV");
			TGenEngine.FamilyEvents = array4;


			TGenEngine.DateCalendars = new LSID[]
			{
				LSID.LSID_Cal_Gregorian, 
				LSID.LSID_Cal_Julian, 
				LSID.LSID_Cal_Hebrew, 
				LSID.LSID_Cal_French, 
				LSID.LSID_Cal_Roman, 
				LSID.LSID_Unknown
			};

			
			TGenEngine.S9[] array5 = new TGenEngine.S9[10];
			array5[0] = new S9(LSID.LSID_DK_0,  (TGenEngine.TDateControlsRange)2);
			array5[1] = new S9(LSID.LSID_DK_1,  (TGenEngine.TDateControlsRange)4);
			array5[2] = new S9(LSID.LSID_DK_2,  (TGenEngine.TDateControlsRange)2);
			array5[3] = new S9(LSID.LSID_DK_3,  (TGenEngine.TDateControlsRange)6);
			array5[4] = new S9(LSID.LSID_DK_4,  (TGenEngine.TDateControlsRange)2);
			array5[5] = new S9(LSID.LSID_DK_5,  (TGenEngine.TDateControlsRange)4);
			array5[6] = new S9(LSID.LSID_DK_6,  (TGenEngine.TDateControlsRange)6);
			array5[7] = new S9(LSID.LSID_DK_7,  (TGenEngine.TDateControlsRange)2);
			array5[8] = new S9(LSID.LSID_DK_8,  (TGenEngine.TDateControlsRange)2);
			array5[9] = new S9(LSID.LSID_DK_9,  (TGenEngine.TDateControlsRange)2);
			TGenEngine.DateKinds = array5;


			TGenEngine.S7[] array6 = new TGenEngine.S7[37];
			array6[ 0] = new S7(LSID.LSID_Event, "EVEN", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 1] = new S7(LSID.LSID_Birth, "BIRT", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 2] = new S7(LSID.LSID_Adoption, "ADOP", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 3] = new S7(LSID.LSID_Christening, "CHR", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 4] = new S7(LSID.LSID_Graduation, "GRAD", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 5] = new S7(LSID.LSID_Retirement, "RETI", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 6] = new S7(LSID.LSID_Naturalization, "NATU", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 7] = new S7(LSID.LSID_Emigration, "EMIG", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 8] = new S7(LSID.LSID_Immigration, "IMMI", TGenEngine.TPersonEventKind.ekEvent);
			array6[ 9] = new S7(LSID.LSID_Census, "CENS", TGenEngine.TPersonEventKind.ekEvent);
			array6[10] = new S7(LSID.LSID_LastWill, "WILL", TGenEngine.TPersonEventKind.ekEvent);
			array6[11] = new S7(LSID.LSID_ProbateOfWill, "PROB", TGenEngine.TPersonEventKind.ekEvent);
			array6[12] = new S7(LSID.LSID_Death, "DEAT", TGenEngine.TPersonEventKind.ekEvent);
			array6[13] = new S7(LSID.LSID_Burial, "BURI", TGenEngine.TPersonEventKind.ekEvent);
			array6[14] = new S7(LSID.LSID_Cremation, "CREM", TGenEngine.TPersonEventKind.ekEvent);
			array6[15] = new S7(LSID.LSID_Fact, "FACT", TGenEngine.TPersonEventKind.ekFact);
			array6[16] = new S7(LSID.LSID_Religion, "RELI", TGenEngine.TPersonEventKind.ekFact);
			array6[17] = new S7(LSID.LSID_Nationality, "NATI", TGenEngine.TPersonEventKind.ekFact);
			array6[18] = new S7(LSID.LSID_Residence, "RESI", TGenEngine.TPersonEventKind.ekFact);
			array6[19] = new S7(LSID.LSID_PhysicalDesc, "DSCR", TGenEngine.TPersonEventKind.ekFact);
			array6[20] = new S7(LSID.LSID_NationalIDNumber, "IDNO", TGenEngine.TPersonEventKind.ekFact);
			array6[21] = new S7(LSID.LSID_SocialSecurityNumber, "SSN", TGenEngine.TPersonEventKind.ekFact);
			array6[22] = new S7(LSID.LSID_ChildsCount, "NCHI", TGenEngine.TPersonEventKind.ekFact);
			array6[23] = new S7(LSID.LSID_MarriagesCount, "NMR", TGenEngine.TPersonEventKind.ekFact);
			array6[24] = new S7(LSID.LSID_Education, "EDUC", TGenEngine.TPersonEventKind.ekFact);
			array6[25] = new S7(LSID.LSID_Occupation, "OCCU", TGenEngine.TPersonEventKind.ekFact);
			array6[26] = new S7(LSID.LSID_Caste, "CAST", TGenEngine.TPersonEventKind.ekFact);
			array6[27] = new S7(LSID.LSID_Property, "PROP", TGenEngine.TPersonEventKind.ekFact);
			array6[28] = new S7(LSID.LSID_NobilityTitle, "TITL", TGenEngine.TPersonEventKind.ekFact);
			array6[29] = new S7(LSID.LSID_Travel, "_TRAVEL", TGenEngine.TPersonEventKind.ekFact);
			array6[30] = new S7(LSID.LSID_Hobby, "_HOBBY", TGenEngine.TPersonEventKind.ekFact);
			array6[31] = new S7(LSID.LSID_Award, "_AWARD", TGenEngine.TPersonEventKind.ekFact);
			array6[32] = new S7(LSID.LSID_Mili, "_MILI", TGenEngine.TPersonEventKind.ekFact);
			array6[33] = new S7(LSID.LSID_MiliInd, "_MILI_IND", TGenEngine.TPersonEventKind.ekFact);
			array6[34] = new S7(LSID.LSID_MiliDis, "_MILI_DIS", TGenEngine.TPersonEventKind.ekFact);
			array6[35] = new S7(LSID.LSID_MiliRank, "_MILI_RANK", TGenEngine.TPersonEventKind.ekFact);
			array6[36] = new S7(LSID.LSID_DNAMarkers, "_DNA", TGenEngine.TPersonEventKind.ekFact);
			TGenEngine.PersonEvents = array6;

			
			TGenEngine.S5[] array7 = new TGenEngine.S5[4];
			array7[0] = new S5(LSID.LSID_Unknown, "");
			array7[1] = new S5(LSID.LSID_MarrRegistered, "MARRIED");
			array7[2] = new S5(LSID.LSID_MarrNotRegistered, "MARRNOTREG");
			array7[3] = new S5(LSID.LSID_MarrDivorced, "NOTMARR");
			TGenEngine.MarriageStatus = array7;


			TGenEngine.TSexRec[] array8 = new TGenEngine.TSexRec[4];
			array8[0] = new TSexRec(LSID.LSID_SexN, "N");
			array8[1] = new TSexRec(LSID.LSID_SexM, "M");
			array8[2] = new TSexRec(LSID.LSID_SexF, "F");
			array8[3] = new TSexRec(LSID.LSID_SexU, "U");
			TGenEngine.SexData = array8;

			TGenEngine.RecordTypes = new LSID[]
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

			TGenEngine.Restrictions = new string[]
			{
				"нет", 
				"конфиденциально", 
				"заперто", 
				"секретно"
			};
		}

		private static bool _GetPatriarchsList_SearchAnc(TGEDCOMIndividualRecord descendantRec, TGEDCOMIndividualRecord searchRec)
		{
			bool Result = false;
			if (descendantRec != null)
			{
				Result = object.Equals(descendantRec, searchRec);
				if (!Result && descendantRec.ChildToFamilyLinks.Count > 0)
				{
					TGEDCOMFamilyRecord family = descendantRec.ChildToFamilyLinks[0].Family;
					TGEDCOMIndividualRecord ancestor = family.Husband.Value as TGEDCOMIndividualRecord;
					if (ancestor != null)
					{
						Result = TGenEngine._GetPatriarchsList_SearchAnc(ancestor, searchRec);
						if (Result)
						{
							return Result;
						}
					}
					ancestor = (family.Wife.Value as TGEDCOMIndividualRecord);
					if (ancestor != null)
					{
						Result = TGenEngine._GetPatriarchsList_SearchAnc(ancestor, searchRec);
					}
				}
			}
			return Result;
		}

		private static bool _GetPatriarchsList_SearchDesc(TGEDCOMIndividualRecord ancestorRec, TGEDCOMIndividualRecord searchRec)
		{
			bool Result = false;

			int num = ancestorRec.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMFamilyRecord family = ancestorRec.SpouseToFamilyLinks[i].Family;
				TGEDCOMPointer sp;
				if (ancestorRec.Sex == TGEDCOMSex.svMale)
				{
					sp = family.Wife;
				}
				else
				{
					sp = family.Husband;
				}
				if (sp != null)
				{
					TGEDCOMIndividualRecord spouse = sp.Value as TGEDCOMIndividualRecord;
					Result = TGenEngine._GetPatriarchsList_SearchAnc(spouse, searchRec);
					if (Result)
					{
						break;
					}
				}

				int num2 = family.Childrens.Count - 1;
				for (int j = 0; j <= num2; j++)
				{
					TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
					Result = TGenEngine._GetPatriarchsList_SearchDesc(child, searchRec);
					if (Result)
					{
						return Result;
					}
				}
			}
			return Result;
		}

		private static int _GetPatriarchsList_GetBirthYear(TGEDCOMIndividualRecord iRec)
		{
			int Result = -1;
			if (iRec != null)
			{
				int year = TGenEngine.GetIndependentYear(iRec, "BIRT");
				if (year > 0)
				{
					Result = year;
				}
				else
				{
					int num = iRec.SpouseToFamilyLinks.Count - 1;
					int i = 0;
					if (num >= i)
					{
						num++;
						while (true)
						{
							TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

							int num2 = family.Childrens.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
								year = TGenEngine._GetPatriarchsList_GetBirthYear(child);
								if (year > 0)
								{
									goto Block_5;
								}
							}
							i++;
							if (i == num)
							{
								return Result;
							}
						}
						Block_5:
						Result = year - 20;
					}
				}
			}
			return Result;
		}

		private static void _CheckRecord_PrepareTag([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (!mmLink.IsPointer) ReformMultimediaLink(aTree, mmLink);
			}

			num = tag.Notes.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMNotes note = tag.Notes[i];
				if (!note.IsPointer) ReformNote(aTree, note);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (!sourCit.IsPointer) ReformSourceCitation(aTree, sourCit);
			}
		}

		private static void _CheckRecord_RepairTag([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = num; i >= 0; i--) {
				TGEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (mmLink.IsPointer && mmLink.Value == null) tag.MultimediaLinks.Delete(i);
			}

			num = tag.Notes.Count - 1;
			for (int i = num; i >= 0; i--) {
				TGEDCOMNotes note = tag.Notes[i];
				if (note.IsPointer && note.Value == null) tag.Notes.Delete(i);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = num; i >= 0; i--) {
				TGEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (sourCit.IsPointer && sourCit.Value == null) tag.SourceCitations.Delete(i);
			}
		}

		private static void _CheckRecord_PreparePtr([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMPointerWithNotes ptr)
		{
			int num = ptr.Notes.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMNotes note = ptr.Notes[i];
				if (!note.IsPointer) ReformNote(aTree, note);
			}
		}

		private static void _CheckRecord_CheckEventPlace(TGEDCOMCustomEvent aEvent)
		{
			TGEDCOMPlace place = aEvent.Detail.Place;
			if (place.Location.XRef != "" && place.Location.Value == null)
			{
				place.Location.XRef = "";
			}
			if (place.StringValue != "")
			{
				TGEDCOMLocationRecord loc = place.Location.Value as TGEDCOMLocationRecord;
				if (loc != null && place.StringValue != loc.LocationName)
				{
					place.StringValue = loc.LocationName;
				}
			}
		}

		private static void _CheckRecord_AddUserRef([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec, string uRef)
		{
			TGEDCOMUserReference @ref = new TGEDCOMUserReference(aTree, iRec, "", "");
			@ref.StringValue = uRef;
			iRec.UserReferences.Add(@ref);
		}

		private static void _CheckRecord_CheckAttrCompatible([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec, TGEDCOMCustomEvent aEvent)
		{
			if (aEvent.Name == "_MILI")
			{
				string cause = aEvent.Detail.Classification.ToLower();
				if (SysUtils.Pos("б/д", cause) > 0)
				{
					if (SysUtils.Pos("+", cause) > 0)
					{
						_CheckRecord_AddUserRef(aTree, aFormat, iRec, TGenEngine.UserRefs[3].Name);
					}
					else
					{
						_CheckRecord_AddUserRef(aTree, aFormat, iRec, TGenEngine.UserRefs[2].Name);
					}
					aEvent.Detail.Classification = "";
				}
				else
				{
					if (SysUtils.Pos("т/т", cause) > 0)
					{
						_CheckRecord_AddUserRef(aTree, aFormat, iRec, TGenEngine.UserRefs[4].Name);
						aEvent.Detail.Classification = "";
					}
				}
			}
		}

		private static void _CheckRecord_CheckURefCompatible(TGEDCOMIndividualRecord iRec, TGEDCOMUserReference aUserRef)
		{
		}

		private static void _CheckRecord_CheckPerson([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord ind)
		{
			int i;
			if (aFormat == TGEDCOMFormat.gf_Native)
			{
				int num = ind.IndividualEvents.Count - 1;
				for (i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = ind.IndividualEvents[i];
					TGenEngine._CheckRecord_CheckEventPlace(evt);
					_CheckRecord_CheckAttrCompatible(aTree, aFormat, ind, evt);
					_CheckRecord_RepairTag(aTree, aFormat, evt.Detail);
				}

				int num2 = ind.UserReferences.Count - 1;
				for (i = 0; i <= num2; i++)
				{
					TGenEngine._CheckRecord_CheckURefCompatible(ind, ind.UserReferences[i]);
				}
			}
			else
			{
				int num3 = ind.IndividualEvents.Count - 1;
				for (i = 0; i <= num3; i++)
				{
					_CheckRecord_PrepareTag(aTree, aFormat, ind.IndividualEvents[i].Detail);
				}

				int num4 = ind.ChildToFamilyLinks.Count - 1;
				for (i = 0; i <= num4; i++)
				{
					_CheckRecord_PreparePtr(aTree, aFormat, ind.ChildToFamilyLinks[i]);
				}

				int num5 = ind.SpouseToFamilyLinks.Count - 1;
				for (i = 0; i <= num5; i++)
				{
					_CheckRecord_PreparePtr(aTree, aFormat, ind.SpouseToFamilyLinks[i]);
				}

				int num6 = ind.Associations.Count - 1;
				for (i = 0; i <= num6; i++)
				{
					_CheckRecord_PreparePtr(aTree, aFormat, ind.Associations[i]);
				}
			}

			for (i = ind.ChildToFamilyLinks.Count - 1; i >= 0; i--)
			{
				if (ind.ChildToFamilyLinks[i].Family == null)
					ind.ChildToFamilyLinks.Delete(i);
			}

			for (i = ind.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
			{
				if (ind.SpouseToFamilyLinks[i].Family == null)
					ind.SpouseToFamilyLinks.Delete(i);
			}
		}

		private static void _CheckRecord_CheckFamily([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMFamilyRecord fam)
		{
			int i;
			if (aFormat == TGEDCOMFormat.gf_Native)
			{
				int num = fam.FamilyEvents.Count - 1;
				for (i = 0; i <= num; i++)
				{
					TGenEngine._CheckRecord_CheckEventPlace(fam.FamilyEvents[i]);
				}
			}
			else
			{
				int num2 = fam.FamilyEvents.Count - 1;
				for (i = 0; i <= num2; i++)
				{
					_CheckRecord_PrepareTag(aTree, aFormat, fam.FamilyEvents[i].Detail);
				}
			}

			for (i = fam.Childrens.Count - 1; i >= 0; i--)
			{
				if (fam.Childrens[i].Value == null)
					fam.Childrens.Delete(i);
			}

			fam.SortChilds();
		}

		private static void _CheckRecord_CheckGroup(TGEDCOMGroupRecord group)
		{
			for (int i = group.Members.Count - 1; i >= 0; i--)
			{
				TGEDCOMPointer ptr = group.Members[i];
				TGEDCOMIndividualRecord irec = ptr.Value as TGEDCOMIndividualRecord;
				if (irec == null)
				{
					group.Members.Delete(i);
				}
				else
				{
					if (irec.IndexOfGroup(group) < 0)
					{
						group.Members.Delete(i);
					}
				}
			}
		}
	}
}
