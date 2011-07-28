using GedCom551;
using GKCore;
using GKSys;
using GKUI;
using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKCore
{
	[TSetElementType(typeof(byte))]
	public class C92 : TUniqueTypeModifier
	{
	}

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
		[StructLayout(LayoutKind.Sequential, Pack = 1)]
		internal struct TRetCount
		{
			public ushort lngSubRows;
			public ushort lngCountLike;
		}

		[StructLayout(LayoutKind.Auto)]
		public struct TSexRec
		{
			public LSID NameId;
			public string Sign;
			
			public TSexRec(LSID aName, string aSign) {
				//TSexRec res;
				this.NameId = aName;
				this.Sign = aSign;
				//return res;
			}
		}

		[StructLayout(LayoutKind.Auto)]
		public struct S5
		{
			public LSID Name;
			public string StatSign;
			
			public S5(LSID aName, string aStatSign) {
				//S5 res;
				this.Name = aName;
				this.StatSign = aStatSign;
				//return res;
			}
		}

		[StructLayout(LayoutKind.Auto)]
		public struct S7
		{
			public LSID Name;
			public string Sign;
			public TGenEngine.TPersonEventKind Kind;
			
			public S7(LSID aName, string aSign, TGenEngine.TPersonEventKind aKind) {
				//S7 res;
				this.Name = aName;
				this.Sign = aSign;
				this.Kind = aKind;
				//return res;
			}
		}

		[StructLayout(LayoutKind.Auto)]
		public struct S9
		{
			public LSID Name;
			public TGenEngine.TDateControlsRange Dates;
			
			public S9(LSID aName, TGenEngine.TDateControlsRange aDates) {
				//S9 res;
				this.Name = aName;
				this.Dates = aDates;
				//return res;
			}
		}

		[StructLayout(LayoutKind.Auto)]
		public struct S21
		{
			public LSID Name;
			public string Sign;
			
			public S21(LSID aName, string aSign) {
				//S21 res;
				this.Name = aName;
				this.Sign = aSign;
				//return res;
			}
		}

		[StructLayout(LayoutKind.Auto)]
		public struct S41
		{
			public LSID Name;
			public string Sign;
			
			public S41(LSID aName, string aSign) {
				//S41 res;
				this.Name = aName;
				this.Sign = aSign;
				//return res;
			}
		}

		[StructLayout(LayoutKind.Auto)]
		public struct S32
		{
			public string Name;
		}

		[StructLayout(LayoutKind.Auto)]
		public struct TGEDCOMAppFormat
		{
			public string Sign;
			public string Name;
			
			public TGEDCOMAppFormat(string aSign, string aName) {
				//TGEDCOMAppFormat af;
				this.Sign = aSign;
				this.Name = aName;
				//return af;
			}
		}

		[StructLayout(LayoutKind.Auto)]
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

		public class TPatriarchObj
		{
			public TGEDCOMIndividualRecord IRec;
			public int IBirthYear;
			public int IDescendantsCount;
			public int IDescGenerations;
			public byte[] ILinks;

			public void Free()
			{
				TObjectHelper.Free(this);
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

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		[StructLayout(LayoutKind.Auto)]
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
			tmAncestor,
			tmDescendant,
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
		[Serializable]
		public enum TDateControlsRange : byte
		{

		}

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

		[TUInt32Subrange(1u, 4u, typeof(TGenEngine.TUserRef))]
		public enum TChartPersonSign : byte
		{
			urRI_StGeorgeCross = 1,
			urUSSR_Soldier,
			urUSSR_FallInBattle,
			urUSSR_RearVeteran
		}

		[Flags, TSetElementType(typeof(TGenEngine.TChartPersonSign))]
		[Serializable]
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
			smMiliRank
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
		internal string FFileName;
		internal TGEDCOMTree FTree;
		protected internal bool Disposed_;

		[Browsable(false)]
		public string ExtName
		{
			get { return this.GetExtName(); }
			set { this.SetExtName(value); }
		}

		[Browsable(false)]
		public string FileName
		{
			get { return this.FFileName; }
			set { this.FFileName = value; }
		}

		[Browsable(false)]
		public bool IsAdvanced
		{
			get { return this.GetIsAdvanced(); }
			set { this.SetIsAdvanced(value); }
		}

		[Browsable(false)]
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
			TGenEngine.TKinshipRec[] kinships = this.Kinships;
			TGenEngine.TKinshipRec[] array;
			TGenEngine.TKinshipRec[] expr_0E = array = new TGenEngine.TKinshipRec[0];
			if (kinships != null)
			{
				int num;
				if ((num = kinships.Length) > 0)
				{
					num = 0;
				}
				if (num > 0)
				{
					Array.Copy(kinships, array, num);
				}
			}
			this.Kinships = expr_0E;
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

		public TGenEngine.TRelationKind FindKinship(TGenEngine.TRelationKind prev, TGenEngine.TRelationKind cur, ref int great, ref int level)
		{
			TGenEngine.TRelationKind Result = TGenEngine.TRelationKind.rkUndefined;
			great = 0;
			level = 0;
			TGenEngine.TKinshipRec[] kinships = this.Kinships;
			int num = ((kinships != null) ? kinships.Length : 0) - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
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
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		internal bool GetIsAdvanced()
		{
			return this.FTree.Header.FindTag("_ADVANCED", 0) != null;
		}

		internal void SetIsAdvanced([In] bool Value)
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

		internal string GetExtName()
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

		internal void SetExtName([In] string Value)
		{
			if (BDSSystem.WStrCmp(Value, "") != 0)
			{
				TGEDCOMTag tag = this.FTree.Header.FindTag("_EXT_NAME", 0);
				if (tag == null)
				{
					tag = this.FTree.Header.AddTag("_EXT_NAME", "", null);
				}
				tag.StringValue = Value;
			}
			else
			{
				this.FTree.Header.DeleteTag("_EXT_NAME");
			}
		}

		internal int PatriarchsCompare(object Item1, object Item2)
		{
			return (Item1 as TGenEngine.TPatriarchObj).IBirthYear - (Item2 as TGenEngine.TPatriarchObj).IBirthYear;
		}

		internal static TGenEngine.TRetCount Matching(string StrA, string StrB, int lngLen)
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
					string StrTempA = BDSSystem.WStrCopy(StrA, PosStrA, lngLen);
					int arg_47_0 = 1;
					int num2 = ((StrB != null) ? StrB.Length : 0) - lngLen + 1;
					int PosStrB = arg_47_0;
					if (num2 >= PosStrB)
					{
						num2++;
						while (true)
						{
							string StrTempB = BDSSystem.WStrCopy(StrB, PosStrB, lngLen);
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

		internal static void CorrectIds(TGEDCOMTree aTree)
		{
			TfmProgress.ProgressInit(aTree.RecordsCount, GKL.LSList[469]);
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				int arg_26_0 = 0;
				int num = aTree.RecordsCount - 1;
				int i = arg_26_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = aTree.GetRecord(i);
						if (TGenEngine.GetId(rec) < 0)
						{
							string newXRef = aTree.XRefIndex_NewXRef(rec);
							repMap.AddXRef(rec, rec.XRef, newXRef);
							rec.XRef = newXRef;
						}
						TfmProgress.ProgressStep();
						i++;
					}
					while (i != num);
				}
				aTree.Header.ReplaceXRefs(repMap);
				TfmProgress.ProgressInit(repMap.Count, GKL.LSList[469]);
				int arg_9B_0 = 0;
				int num2 = repMap.Count - 1;
				i = arg_9B_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TGEDCOMRecord rec = repMap.GetRecord(i).Rec;
						rec.ReplaceXRefs(repMap);
						TfmProgress.ProgressStep();
						i++;
					}
					while (i != num2);
				}
			}
			finally
			{
				repMap.Free();
				TfmProgress.ProgressDone();
			}
		}

		internal static void ReformNote(TGEDCOMTree aTree, TGEDCOMNotes note)
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

		internal static void ReformMultimediaLink(TGEDCOMTree aTree, TGEDCOMMultimediaLink mmLink)
		{
			try
			{
				string title = mmLink.Title;
				TGEDCOMMultimediaRecord mmRec = new TGEDCOMMultimediaRecord(aTree, aTree, "", "");
				mmRec.InitNew();
				aTree.AddRecord(mmRec);
				int arg_32_0 = 0;
				int num = mmLink.GetFileReferencesCount() - 1;
				int i = arg_32_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMFileReference fr = mmLink.GetFileReference(i);
						TGEDCOMFileReferenceWithTitle frt = new TGEDCOMFileReferenceWithTitle(aTree, mmRec, "", "");
						if (fr.MultimediaFormat != TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfNone)
						{
							frt.MultimediaFormat = fr.MultimediaFormat;
						}
						if (fr.MediaType != TGEDCOMFileReference.TGEDCOMMediaType.mtNone)
						{
							frt.MediaType = fr.MediaType;
						}
						frt.LinkFile(fr.StringValue, TGEDCOMFileReference.TGEDCOMMediaType.mtUnknown, TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfUnknown);
						mmRec.AddFileReference(frt);
						i++;
					}
					while (i != num);
				}
				mmLink.Clear();
				mmLink.Value = mmRec;
				mmLink.Title = title;
			}
			finally
			{
			}
		}

		internal static void ReformSourceCitation(TGEDCOMTree aTree, TGEDCOMSourceCitation sourCit)
		{
		}

		internal static int GetDescGens_Recursive(TGEDCOMIndividualRecord aPerson)
		{
			int Result = 0;
			if (aPerson != null)
			{
				int max = 0;
				int arg_14_0 = 0;
				int num = aPerson.SpouseToFamilyLinksCount - 1;
				int i = arg_14_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.GetSpouseToFamilyLink(i).Family;
						int arg_3C_0 = 0;
						int num2 = family.ChildrenCount - 1;
						int j = arg_3C_0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								TGEDCOMIndividualRecord iChild = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
								int res = TGenEngine.GetDescGens_Recursive(iChild);
								if (max < res)
								{
									max = res;
								}
								j++;
							}
							while (j != num2);
						}
						i++;
					}
					while (i != num);
				}
				Result = 1 + max;
			}
			return Result;
		}

		internal static ushort DaysInAMonth([In] ushort AYear, [In] ushort AMonth)
		{
			return BDSSystem.MonthDays[(AMonth == 2 && DateTime.IsLeapYear((int)AYear)) ? 1 : 0][(int)AMonth - 1];
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
			TGEDCOMObject.TGEDCOMSex sex = aSpouse.Sex;
			if (sex != TGEDCOMObject.TGEDCOMSex.svNone)
			{
				if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (sex != TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						if (sex == TGEDCOMObject.TGEDCOMSex.svUndetermined)
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
				aSpouse.AddSpouseToFamilyLink(spLink);
			}
		}
		public void RemoveFamilySpouse(TGEDCOMFamilyRecord aFamily, TGEDCOMIndividualRecord aSpouse)
		{
			if (aSpouse != null)
			{
				aSpouse.DeleteSpouseToFamilyLink(aFamily);
				TGEDCOMObject.TGEDCOMSex sex = aSpouse.Sex;
				if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
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
				aFamily.AddChild(ptr);
				TGEDCOMChildToFamilyLink chLink = new TGEDCOMChildToFamilyLink(this.FTree, aChild, "", "");
				chLink.Family = aFamily;
				aChild.AddChildToFamilyLink(chLink);
				Result = true;
			}
			catch (Exception E)
			{
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
				aResearch.AddTask(ptr);
				Result = true;
			}
			return Result;
		}

		public void RemoveResearchTask(TGEDCOMResearchRecord aResearch, TGEDCOMTaskRecord aTask)
		{
			aResearch.DeleteTask(aResearch.IndexOfTask(aTask));
		}

		public bool AddResearchGroup(TGEDCOMResearchRecord aResearch, TGEDCOMGroupRecord aGroup)
		{
			bool Result = false;
			if (aResearch != null && aGroup != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aResearch, "", "");
				ptr.SetNamedValue("_GROUP", aGroup);
				aResearch.AddGroup(ptr);
				Result = true;
			}
			return Result;
		}

		public void RemoveResearchGroup(TGEDCOMResearchRecord aResearch, TGEDCOMGroupRecord aGroup)
		{
			aResearch.DeleteGroup(aResearch.IndexOfGroup(aGroup));
		}

		public bool AddResearchComm(TGEDCOMResearchRecord aResearch, TGEDCOMCommunicationRecord aComm)
		{
			bool Result = false;
			if (aResearch != null && aComm != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aResearch, "", "");
				ptr.SetNamedValue("_COMM", aComm);
				aResearch.AddCommunication(ptr);
				Result = true;
			}
			return Result;
		}

		public void RemoveResearchComm(TGEDCOMResearchRecord aResearch, TGEDCOMCommunicationRecord aComm)
		{
			aResearch.DeleteCommunication(aResearch.IndexOfCommunication(aComm));
		}

		public bool AddGroupMember(TGEDCOMGroupRecord aGroup, TGEDCOMIndividualRecord aMember)
		{
			bool Result;
			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, aGroup, "", "");
				ptr.SetNamedValue("_MEMBER", aMember);
				aGroup.AddMember(ptr);
				ptr = new TGEDCOMPointer(this.FTree, aMember, "", "");
				ptr.SetNamedValue("_GROUP", aGroup);
				aMember.AddGroup(ptr);
				Result = true;
			}
			catch (Exception E)
			{
				Result = false;
			}
			return Result;
		}

		public bool RemoveGroupMember(TGEDCOMGroupRecord aGroup, TGEDCOMIndividualRecord aMember)
		{
			bool Result;
			try
			{
				aGroup.DeleteMember(aGroup.IndexOfMember(aMember));
				aMember.DeleteGroup(aMember.IndexOfGroup(aGroup));
				Result = true;
			}
			catch (Exception E)
			{
				Result = false;
			}
			return Result;
		}

		public TGEDCOMAssociation AddAssociation(TGEDCOMIndividualRecord aRec, string aRel, TGEDCOMIndividualRecord aRelPerson)
		{
			TGEDCOMAssociation Result = new TGEDCOMAssociation(this.FTree, aRec, "", "");
			Result.Relation = aRel;
			Result.Individual = aRelPerson;
			aRec.AddAssociation(Result);
			return Result;
		}

		public void CleanFamily(TGEDCOMFamilyRecord aFamily)
		{
			if (aFamily != null)
			{
				int arg_0D_0 = 0;
				int num = aFamily.ChildrenCount - 1;
				int i = arg_0D_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMIndividualRecord child = aFamily.GetChildren(i).Value as TGEDCOMIndividualRecord;
						child.DeleteChildToFamilyLink(aFamily);
						i++;
					}
					while (i != num);
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
					if (rec is TGEDCOMSourceRecord && BDSSystem.WStrCmp((rec as TGEDCOMSourceRecord).FiledByEntry, aName) == 0)
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
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMSourceRecord)
						{
							aSources.AddObject((rec as TGEDCOMSourceRecord).FiledByEntry, rec);
						}
						i++;
					}
					while (i != num);
				}
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
			int arg_135_0 = 0;
			int num = this.FTree.RecordsCount - 1;
			int i = arg_135_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						aStats.persons++;
						if (ind.IsLive())
						{
							aStats.lives++;
						}
						string v_age = TGenEngine.GetAge(ind, -1);
						if (BDSSystem.WStrCmp(v_age, "") != 0 && BDSSystem.WStrCmp(v_age, "?") != 0)
						{
							aStats.age += TGKSys.StrToInt(v_age);
							aStats.age_cnt++;
						}
						string v_life = TGenEngine.GetLifeExpectancy(ind);
						if (BDSSystem.WStrCmp(v_life, "") != 0 && BDSSystem.WStrCmp(v_life, "?") != 0)
						{
							aStats.life += TGKSys.StrToInt(v_life);
							aStats.life_cnt++;
						}
						int ch_cnt = TGenEngine.GetChildsCount(ind);
						if (ch_cnt != 0)
						{
							aStats.childs += ch_cnt;
							aStats.childs_cnt++;
						}
						string v_fba = TGenEngine.GetFirstbornAge(ind).ToString();
						if (BDSSystem.WStrCmp(v_fba, "") != 0 && BDSSystem.WStrCmp(v_fba, "?") != 0 && BDSSystem.WStrCmp(v_fba, "0") != 0)
						{
							aStats.fba += TGKSys.StrToInt(v_fba);
							aStats.fba_cnt++;
						}
						int m_cnt = TGenEngine.GetMarriagesCount(ind);
						if (m_cnt != 0)
						{
							aStats.marr += m_cnt;
							aStats.marr_cnt++;
						}
						string v_mage = TGenEngine.GetMarriageAge(ind).ToString();
						if (BDSSystem.WStrCmp(v_mage, "") != 0 && BDSSystem.WStrCmp(v_mage, "?") != 0 && BDSSystem.WStrCmp(v_mage, "0") != 0)
						{
							aStats.mage += TGKSys.StrToInt(v_mage);
							aStats.mage_cnt++;
						}
						TGEDCOMObject.TGEDCOMSex sex = ind.Sex;
						if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
						{
							if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
							{
								aStats.persons_f++;
								if (ind.IsLive())
								{
									aStats.lives_f++;
								}
								if (BDSSystem.WStrCmp(v_age, "") != 0 && BDSSystem.WStrCmp(v_age, "?") != 0)
								{
									aStats.age_f += TGKSys.StrToInt(v_age);
									aStats.age_f_cnt++;
								}
								if (BDSSystem.WStrCmp(v_life, "") != 0 && BDSSystem.WStrCmp(v_life, "?") != 0)
								{
									aStats.life_f += TGKSys.StrToInt(v_life);
									aStats.life_f_cnt++;
								}
								if (ch_cnt != 0)
								{
									aStats.childs_f += ch_cnt;
									aStats.childs_f_cnt++;
								}
								if (BDSSystem.WStrCmp(v_fba, "") != 0 && BDSSystem.WStrCmp(v_fba, "?") != 0 && BDSSystem.WStrCmp(v_fba, "0") != 0)
								{
									aStats.fba_f += TGKSys.StrToInt(v_fba);
									aStats.fba_f_cnt++;
								}
								if (m_cnt != 0)
								{
									aStats.marr_f += m_cnt;
									aStats.marr_f_cnt++;
								}
								if (BDSSystem.WStrCmp(v_mage, "") != 0 && BDSSystem.WStrCmp(v_mage, "?") != 0 && BDSSystem.WStrCmp(v_mage, "0") != 0)
								{
									aStats.mage_f += TGKSys.StrToInt(v_mage);
									aStats.mage_f_cnt++;
								}
							}
						}
						else
						{
							aStats.persons_m++;
							if (ind.IsLive())
							{
								aStats.lives_m++;
							}
							if (BDSSystem.WStrCmp(v_age, "") != 0 && BDSSystem.WStrCmp(v_age, "?") != 0)
							{
								aStats.age_m += TGKSys.StrToInt(v_age);
								aStats.age_m_cnt++;
							}
							if (BDSSystem.WStrCmp(v_life, "") != 0 && BDSSystem.WStrCmp(v_life, "?") != 0)
							{
								aStats.life_m += TGKSys.StrToInt(v_life);
								aStats.life_m_cnt++;
							}
							if (ch_cnt != 0)
							{
								aStats.childs_m += ch_cnt;
								aStats.childs_m_cnt++;
							}
							if (BDSSystem.WStrCmp(v_fba, "") != 0 && BDSSystem.WStrCmp(v_fba, "?") != 0 && BDSSystem.WStrCmp(v_fba, "0") != 0)
							{
								aStats.fba_m += TGKSys.StrToInt(v_fba);
								aStats.fba_m_cnt++;
							}
							if (m_cnt != 0)
							{
								aStats.marr_m += m_cnt;
								aStats.marr_m_cnt++;
							}
							if (BDSSystem.WStrCmp(v_mage, "") != 0 && BDSSystem.WStrCmp(v_mage, "?") != 0 && BDSSystem.WStrCmp(v_mage, "0") != 0)
							{
								aStats.mage_m += TGKSys.StrToInt(v_mage);
								aStats.mage_m_cnt++;
							}
						}
					}
					i++;
				}
				while (i != num);
			}
		}

		public void GetSpecStats(TGenEngine.TStatMode aMode, Hashtable aVals)
		{
			if (aMode < TGenEngine.TStatMode.smDescGenerations)
			{
				TGenEngine.InitExtCounts(this.FTree, -1);
			}

			try
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						if (this.FTree.GetRecord(i) is TGEDCOMIndividualRecord && aMode != TGenEngine.TStatMode.smSpousesDiff)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FTree.GetRecord(i);
							string iName = TGenEngine.GetNameStr(iRec, true, false);
							switch (aMode)
							{
								case TGenEngine.TStatMode.smAncestors:
								{
									aVals.Add(iName, TGenEngine.GetAncestorsCount(iRec) - 1);
									goto IL_661;
								}
								case TGenEngine.TStatMode.smDescendants:
								{
									aVals.Add(iName, TGenEngine.GetDescendantsCount(iRec) - 1);
									goto IL_661;
								}
								case TGenEngine.TStatMode.smDescGenerations:
								{
									aVals.Add(iName, TGenEngine.GetDescGenerations(iRec));
									goto IL_661;
								}
								case TGenEngine.TStatMode.smChildsCount:
								{
									aVals.Add(iName, TGenEngine.GetChildsCount(iRec));
									goto IL_661;
								}
								case TGenEngine.TStatMode.smFirstbornAge:
								{
									aVals.Add(iName, TGenEngine.GetFirstbornAge(iRec));
									goto IL_661;
								}
								case TGenEngine.TStatMode.smMarriages:
								{
									aVals.Add(iName, TGenEngine.GetMarriagesCount(iRec));
									goto IL_661;
								}
								case TGenEngine.TStatMode.smMarriageAge:
								{
									aVals.Add(iName, TGenEngine.GetMarriageAge(iRec));
									goto IL_661;
								}
							}

							string V = "";
							switch (aMode)
							{
								case TGenEngine.TStatMode.smFamilies:
								case TGenEngine.TStatMode.smNames:
								case TGenEngine.TStatMode.smPatronymics:
								{
									string fam = "";
									string nam = "";
									string pat = "";
									TGenEngine.GetNameParts(iRec, ref fam, ref nam, ref pat);
									if (aMode != TGenEngine.TStatMode.smFamilies)
									{
										if (aMode != TGenEngine.TStatMode.smNames)
										{
											if (aMode == TGenEngine.TStatMode.smPatronymics)
											{
												V = pat;
											}
										}
										else
										{
											V = nam;
										}
									}
									else
									{
										V = TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMObject.TGEDCOMSex.svFemale);
									}
									break;
								}
								case TGenEngine.TStatMode.smAge:
								{
									V = TGenEngine.GetAge(iRec, -1);
									break;
								}
								case TGenEngine.TStatMode.smLifeExpectancy:
								{
									V = TGenEngine.GetLifeExpectancy(iRec);
									break;
								}
								case TGenEngine.TStatMode.smBirthYears:
								case TGenEngine.TStatMode.smBirthTenYears:
								case TGenEngine.TStatMode.smDeathYears:
								case TGenEngine.TStatMode.smDeathTenYears:
								case TGenEngine.TStatMode.smBirthPlaces:
								case TGenEngine.TStatMode.smDeathPlaces:
								{
									V = "?";
									int num2 = iRec.IndividualEventsCount - 1;
									int j = 0;
									if (num2 >= j)
									{
										num2++;
										do
										{
											TGEDCOMCustomEvent @event = iRec.GetIndividualEvent(j);
											int year = 0;
											ushort k = 0;
											ushort d = 0;
											TGenEngine.GetIndependentDate(@event.Detail.Date.Value, ref year, ref k, ref d);
											if (Math.Abs(year) > 3000)
											{
												TGKSys.ShowMessage(@event.Detail.Date.StringValue + "/" + iName);
											}
											if (@event.Name == "BIRT")
											{
												if (aMode == TGenEngine.TStatMode.smBirthYears)
												{
													V = Convert.ToString(year);
												}
												else
												{
													if (aMode == TGenEngine.TStatMode.smBirthTenYears)
													{
														V = Convert.ToString(year / 10 * 10);
													}
													else
													{
														if (aMode == TGenEngine.TStatMode.smBirthPlaces)
														{
															V = @event.Detail.Place.StringValue;
														}
													}
												}
											}
											else
											{
												if (@event.Name == "DEAT")
												{
													if (aMode == TGenEngine.TStatMode.smDeathYears)
													{
														V = Convert.ToString(year);
													}
													else
													{
														if (aMode == TGenEngine.TStatMode.smDeathTenYears)
														{
															V = Convert.ToString(year / 10 * 10);
														}
														else
														{
															if (aMode == TGenEngine.TStatMode.smDeathPlaces)
															{
																V = @event.Detail.Place.StringValue;
															}
														}
													}
												}
											}
											j++;
										}
										while (j != num2);
									}
									break;
								}
								case TGenEngine.TStatMode.smChildsDistribution:
								{
									V = TGenEngine.GetChildsCount(iRec).ToString();
									break;
								}
								case TGenEngine.TStatMode.smResidences:
								{
									V = TGenEngine.GetResidencePlace(iRec, false);
									break;
								}
								case TGenEngine.TStatMode.smOccupation:
								{
									V = TGenEngine.GetAttributeValue(iRec, "OCCU");
									break;
								}
								case TGenEngine.TStatMode.smReligious:
								{
									V = TGenEngine.GetAttributeValue(iRec, "RELI");
									break;
								}
								case TGenEngine.TStatMode.smNational:
								{
									V = TGenEngine.GetAttributeValue(iRec, "NATI");
									break;
								}
								case TGenEngine.TStatMode.smEducation:
								{
									V = TGenEngine.GetAttributeValue(iRec, "EDUC");
									break;
								}
								case TGenEngine.TStatMode.smCaste:
								{
									V = TGenEngine.GetAttributeValue(iRec, "CAST");
									break;
								}
								case TGenEngine.TStatMode.smHobby:
								{
									V = TGenEngine.GetAttributeValue(iRec, "_HOBBY");
									break;
								}
								case TGenEngine.TStatMode.smAward:
								{
									V = TGenEngine.GetAttributeValue(iRec, "_AWARD");
									break;
								}
								case TGenEngine.TStatMode.smMili:
								{
									V = TGenEngine.GetAttributeValue(iRec, "_MILI");
									break;
								}
								case TGenEngine.TStatMode.smMiliInd:
								{
									V = TGenEngine.GetAttributeValue(iRec, "_MILI_IND");
									break;
								}
								case TGenEngine.TStatMode.smMiliDis:
								{
									V = TGenEngine.GetAttributeValue(iRec, "_MILI_DIS");
									break;
								}
								case TGenEngine.TStatMode.smMiliRank:
								{
									V = TGenEngine.GetAttributeValue(iRec, "_MILI_RANK");
									break;
								}
							}
							if (V == "-1" || V == "" || V == "0")
							{
								V = "?";
							}
							if (!aVals.ContainsKey(V))
							{
								aVals.Add(V, 1);
							}
							else
							{
								aVals[V] = int.Parse(aVals[V].ToString()) + 1;
							}
						}
						else
						{
							if (this.FTree.GetRecord(i) is TGEDCOMFamilyRecord && aMode == TStatMode.smSpousesDiff)
							{
								TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)this.FTree.GetRecord(i);
								aVals.Add(TGenEngine.GetFamilyStr(fRec), TGenEngine.GetSpousesDiff(fRec));
							}
						}
						IL_661:
						i++;
					}
					while (i != num);
				}
			}
			finally
			{
			}
		}

		public void GetPatriarchsList(bool aProgress, bool aLinks, ref TObjectList aList, int aMinGens)
		{
			if (aProgress)
			{
				TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[474]);
			}

			TGenEngine.InitExtCounts(this.FTree, -1);

			try
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
							string nf = "";
							string nn = "";
							string np = "";
							TGenEngine.GetNameParts(i_rec, ref nf, ref nn, ref np);
							int bYear = TGenEngine._GetPatriarchsList_GetBirthYear(this, i_rec);
							int descGens = TGenEngine.GetDescGenerations(i_rec);
							bool res = i_rec.ChildToFamilyLinksCount == 0;
							res = (res && i_rec.Sex == TGEDCOMObject.TGEDCOMSex.svMale);
							res = (res && BDSSystem.WStrCmp(nf, "") != 0 && BDSSystem.WStrCmp(nf, "?") != 0 && BDSSystem.WStrCmp(nn, "") != 0 && BDSSystem.WStrCmp(nn, "?") != 0);
							res = (res && descGens >= aMinGens);
							res = (res && bYear > 0);
							if (res)
							{
								TGenEngine.TPatriarchObj pObj = new TGenEngine.TPatriarchObj();
								pObj.IRec = i_rec;
								pObj.IBirthYear = bYear;
								pObj.IDescendantsCount = TGenEngine.GetDescendantsCount(i_rec) - 1;
								pObj.IDescGenerations = descGens;
								TGenEngine.TPatriarchObj arg_17C_0 = pObj;
								//alert!!!
								/*byte[] expr_171 = BDSSystem.SetNew(32);
								RuntimeHelpers.InitializeArray(expr_171, ldtoken(GKL.Set_val_220));
								arg_17C_0.ILinks = expr_171;*/
								aList.Add(pObj);
							}
						}
						if (aProgress)
						{
							TfmProgress.ProgressStep();
						}
						i++;
					}
					while (i != num);
				}
				aList.Sort(new TListSortCompare(this.PatriarchsCompare));
			}
			finally
			{
				if (aProgress)
				{
					TfmProgress.ProgressDone();
				}
			}
			if (aLinks)
			{
				if (aProgress)
				{
					TfmProgress.ProgressInit(aList.Count, GKL.LSList[475]);
				}
				try
				{
					int num2 = aList.Count - 1;
					int i = 0;
					if (num2 >= i)
					{
						num2++;
						do
						{
							TGenEngine.TPatriarchObj patr = aList[i] as TGenEngine.TPatriarchObj;
							int arg_21B_0 = i + 1;
							int num3 = aList.Count - 1;
							int j = arg_21B_0;
							if (num3 >= j)
							{
								num3++;
								do
								{
									TGenEngine.TPatriarchObj patr2 = aList[j] as TGenEngine.TPatriarchObj;
									bool res = TGenEngine._GetPatriarchsList_SearchDesc(patr.IRec, patr2.IRec);
									if (res)
									{
										BDSSystem.SetUnion(ref patr.ILinks, BDSSystem.SetExpand(BDSSystem.SetElem(j, 32), 32, 0, 32, 0), 32);
										BDSSystem.SetUnion(ref patr2.ILinks, BDSSystem.SetExpand(BDSSystem.SetElem(i, 32), 32, 0, 32, 0), 32);
									}
									j++;
								}
								while (j != num3);
							}
							if (aProgress)
							{
								TfmProgress.ProgressStep();
							}
							i++;
						}
						while (i != num2);
					}
				}
				finally
				{
					if (aProgress)
					{
						TfmProgress.ProgressDone();
					}
				}
			}
		}

		public string GetPatriarchLinks(TObjectList lst, TGenEngine.TPatriarchObj pObj)
		{
			string Result = "";
			int arg_10_0 = 0;
			int num = lst.Count - 1;
			int i = arg_10_0;
			if (num >= i)
			{
				num++;
				do
				{
					if (BDSSystem.SetTest(pObj.ILinks, i, 32))
					{
						if (Result != "") Result += ", ";
						Result += TGenEngine.GetNameStr((lst[i] as TGenEngine.TPatriarchObj).IRec, true, false);
					}
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		public bool CheckPath()
		{
			string path = VCLUtils.ExtractFilePath(this.FFileName);
			bool Result = BDSSystem.WStrCmp(path, "") > 0;
			if (!Result)
			{
				TGKSys.ShowError("Для типов хранения \"архив\" и \"хранилище\" новый файл БД нужно предварительно сохранить");
			}
			return Result;
		}

		public string GetArcFileName()
		{
			return VCLUtils.ExtractFilePath(this.FFileName) + this.GetSpecExtName() + ".zip";
		}

		public string GetStoreFolder()
		{
			string Result = VCLUtils.ExtractFilePath(this.FFileName) + this.GetSpecExtName() + "\\";
			if (!Directory.Exists(Result))
			{
				VCLUtils.CreateDir(Result);
			}
			return Result;
		}

		public string GetSpecExtName()
		{
			string ext = this.ExtName;
			string Result;
			if (ext == "")
			{
				Result = Path.GetFileName(this.FFileName);
				int p = BDSSystem.Pos(".ged", Result);
				if (p > 0)
				{
					Result = BDSSystem.WStrCopy(Result, 1, p - 1);
				}
			}
			else
			{
				Result = ext;
			}
			return Result;
		}

		public TGenEngine.TGKStoreType GetStoreType(string aFileRef, ref string aFileName)
		{
			aFileName = aFileRef;
			TGenEngine.TGKStoreType Result;
			if (BDSSystem.Pos(TGenEngine.GKStoreType[1].Sign, aFileRef) > 0)
			{
				Result = TGenEngine.TGKStoreType.gstArchive;
				aFileName = aFileName.Remove(0, 4);
			}
			else
			{
				if (BDSSystem.Pos(TGenEngine.GKStoreType[2].Sign, aFileRef) > 0)
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

		public void MediaLoad(string aRefName, ref TStream aStream)
		{
			string target_fn = "";
			TGenEngine.TGKStoreType gst = this.GetStoreType(aRefName, ref target_fn);
			if (gst != TGenEngine.TGKStoreType.gstReference)
			{
				if (gst != TGenEngine.TGKStoreType.gstArchive)
				{
					if (gst == TGenEngine.TGKStoreType.gstStorage)
					{
						target_fn = this.GetStoreFolder() + target_fn;
						aStream = new TFileStream(target_fn, 0);
					}
				}
				else
				{
					aStream = new TMemoryStream();
					if (!File.Exists(this.GetArcFileName()))
					{
						TGKSys.ShowError(GKL.LSList[476]);
					}
					else
					{
						this.ArcFileLoad(target_fn, aStream);
						aStream.Seek((long)((ulong)0), TSeekOrigin.soBeginning);
					}
				}
			}
			else
			{
				aStream = new TFileStream(target_fn, 0);
			}
		}

		public void MediaLoad(string aRefName, ref string aFileName)
		{
			string target_fn = "";
			TGenEngine.TGKStoreType gst = this.GetStoreType(aRefName, ref target_fn);
			if (gst != TGenEngine.TGKStoreType.gstReference)
			{
				if (gst != TGenEngine.TGKStoreType.gstArchive)
				{
					if (gst == TGenEngine.TGKStoreType.gstStorage)
					{
						aFileName = this.GetStoreFolder() + target_fn;
					}
				}
				else
				{
					aFileName = TGKSys.GetTempDir() + Path.GetFileName(target_fn);
					TFileStream fs = new TFileStream(aFileName, 65535);
					try
					{
						if (!File.Exists(this.GetArcFileName()))
						{
							TGKSys.ShowError(GKL.LSList[476]);
						}
						else
						{
							target_fn = VCLUtils.StringReplace(target_fn, "\\", "/", TReplaceFlags.rfReplaceAll);
							this.ArcFileLoad(target_fn, fs);
						}
					}
					finally
					{
						fs.Dispose();
					}
				}
			}
			else
			{
				aFileName = target_fn;
			}
		}

		public void MediaSave(string aFileName, TGenEngine.TGKStoreType aStoreType, ref string aRefPath)
		{
			string sfn = Path.GetFileName(aFileName);
			string spath = "";

			switch (TGEDCOMFileReference.RecognizeFormat(aFileName))
			{
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfNone:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfOLE:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfUnknown:
				{
					spath = "unknown\\";
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPNG:
				{
					spath = "images\\";
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfWAV:
				{
					spath = "audio\\";
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTXT:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfRTF:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfHTM:
				{
					spath = "texts\\";
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfMPG:
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
			int num = aRec.GetMultimediaLinksCount() - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				while (!object.Equals(aRec.GetMultimediaLink(i).Value, mmRec))
				{
					i++;
					if (i == num)
					{
						goto IL_3B;
					}
				}
				mmLink = aRec.GetMultimediaLink(i);
			}
			IL_3B:
			if (mmLink == null)
			{
				mmLink = new TGEDCOMMultimediaLink(this.FTree, aRec, "", "");
				mmLink.Value = mmRec;
				aRec.AddMultimediaLink(mmLink);
			}
			mmLink.IsPrimary = true;
			return mmLink;
		}

		public TGEDCOMMultimediaLink GetPrimaryMultimediaLink(TGEDCOMIndividualRecord aRec)
		{
			int num = aRec.GetMultimediaLinksCount() - 1;
			int i = 0;
			TGEDCOMMultimediaLink Result;
			if (num >= i)
			{
				num++;
				TGEDCOMMultimediaLink mmLink;
				while (true)
				{
					mmLink = aRec.GetMultimediaLink(i);
					if (mmLink.IsPrimary)
					{
						break;
					}
					i++;
					if (i == num)
					{
						goto IL_2F;
					}
				}
				Result = mmLink;
				return Result;
			}
			IL_2F:
			Result = null;
			return Result;
		}

		public Bitmap GetPrimaryBitmap(TGEDCOMIndividualRecord aRec)
		{
			Bitmap Result = null;
			TGEDCOMMultimediaLink mmLink = this.GetPrimaryMultimediaLink(aRec);
			if (mmLink != null)
			{
				TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
				string target_fn = "";
				this.MediaLoad(mmRec.GetFileReference(0).StringValue, ref target_fn);
				if (File.Exists(target_fn))
				{
					Result = new Bitmap(target_fn);
				}
			}
			return Result;
		}

		public void ArcFileLoad(string target_fn, TStream toStream)
		{
		}

		public void ArcFileSave(string aFileName, string sfn)
		{
		}

		public void SortFamilyChilds(TGEDCOMFamilyRecord aFamily)
		{
			int num = aFamily.ChildrenCount - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					int arg_29_0 = i + 1;
					int num2 = aFamily.ChildrenCount - 1;
					int j = arg_29_0;
					if (num2 >= j)
					{
						num2++;
						do
						{
							TGEDCOMIndividualRecord iChild = aFamily.GetChildren(i).Value as TGEDCOMIndividualRecord;
							TGEDCOMCustomEvent iEv = TGenEngine.GetIndividualEvent(iChild, "BIRT");
							DateTime iDate;
							if (iEv != null)
							{
								iDate = TGenEngine.GEDCOMDateToDate(iEv.Detail.Date.Value);
							}
							else
							{
								iDate = new DateTime((long)((ulong)0));
							}
							TGEDCOMIndividualRecord kChild = aFamily.GetChildren(j).Value as TGEDCOMIndividualRecord;
							TGEDCOMCustomEvent kEv = TGenEngine.GetIndividualEvent(kChild, "BIRT");
							DateTime kDate;
							if (kEv != null)
							{
								kDate = TGenEngine.GEDCOMDateToDate(kEv.Detail.Date.Value);
							}
							else
							{
								kDate = new DateTime((long)((ulong)0));
							}
							if (iDate > kDate)
							{
								aFamily.ChildrenExchange(i, j);
							}
							j++;
						}
						while (j != num2);
					}
					i++;
				}
				while (i != num);
			}
		}


		public static string SexStr(TGEDCOMObject.TGEDCOMSex Sex)
		{
			return GKL.LSList[(int)TGenEngine.SexData[(int)Sex].NameId - 1];
		}


		public static TGEDCOMObject.TGEDCOMSex GetSexBySign([In] char SexSign)
		{
			TGEDCOMObject.TGEDCOMSex Result = TGEDCOMObject.TGEDCOMSex.svNone;
			
			switch (SexSign) {
				case 'F':
					Result = TGEDCOMObject.TGEDCOMSex.svFemale;
					break;
				case 'M':
					Result = TGEDCOMObject.TGEDCOMSex.svMale;
					break;
				case 'U':
					Result = TGEDCOMObject.TGEDCOMSex.svUndetermined;
					break;
			}
			
			return Result;
		}


		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}


		public static bool IsRecordAccess(TGEDCOMObject.TGEDCOMRestriction aRecRestriction, TGenEngine.TShieldState aShieldState)
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
					Result = (((aRecRestriction == TGEDCOMObject.TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
				}
			}
			else
			{
				Result = (((aRecRestriction == TGEDCOMObject.TGEDCOMRestriction.rnConfidential || aRecRestriction == TGEDCOMObject.TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
			}
			return Result;
		}

		public static TGenEngine.TPersonEventKind GetPersonEventKindBySign(string aSign)
		{
			TGenEngine.TPersonEventKind Result = TGenEngine.TPersonEventKind.ekFact;
			int i = 0;
			while (BDSSystem.WStrCmp(TGenEngine.PersonEvents[i].Sign, aSign) != 0)
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
			while (BDSSystem.WStrCmp(TGenEngine.PersonEvents[i].Sign, aSign) != 0)
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
			while (BDSSystem.WStrCmp(TGenEngine.FamilyEvents[i].Sign, aSign) != 0)
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
			while (BDSSystem.WStrCmp(TGenEngine.MarriageStatus[i].StatSign, aSign) != 0)
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

		public static void GetNameParts(TGEDCOMIndividualRecord iRec, ref string aFamily, ref string aName, ref string aPatronymic)
		{
			if (iRec != null && iRec.PersonalNamesCount > 0)
			{
				TGEDCOMPersonalName np = iRec.GetPersonalName(0);
				aFamily = np.Surname;
				if (TGKSys.GetTokensCount(np.FirstPart, ' ') > 1)
				{
					aName = TGKSys.GetToken(np.FirstPart, ' ', 1);
					aPatronymic = TGKSys.GetToken(np.FirstPart, ' ', 2);
				}
				else
				{
					aName = np.FirstPart;
					aPatronymic = "";
				}
			}
			else
			{
				aFamily = "";
				aName = "";
				aPatronymic = "";
			}
		}

		public static string GetNameStr(TGEDCOMIndividualRecord iRec, bool aByFamily, bool aPieces)
		{
			string Result;
			if (iRec != null && iRec.PersonalNamesCount > 0)
			{
				TGEDCOMPersonalName np = iRec.GetPersonalName(0);
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
					if (BDSSystem.WStrCmp(nick, "") != 0)
					{
						Result = Result + " [" + nick + "]";
					}
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
				TGEDCOMPersonalName np = iRec.GetPersonalName(0);
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

		public static TGEDCOMObject.TGEDCOMSex GetSex(string f_name, string f_pat, bool aQuery)
		{
			TGEDCOMObject.TGEDCOMSex Result = TGEDCOMObject.TGEDCOMSex.svNone;
			char c = f_name[((f_name != null) ? f_name.Length : 0) - 1];
			if (c != 'а')
			{
				if (c - 'в' < '\u0003' || c == 'й' || c - 'л' < '\u0006')
				{
					Result = TGEDCOMObject.TGEDCOMSex.svMale;
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
					Result = TGEDCOMObject.TGEDCOMSex.svFemale;
				}
				else
				{
					char c3 = f_pat[((f_pat != null) ? f_pat.Length : 0) - 1];
					if (c3 >= '2' && (c3 < '5' || c3 == '9' || (c3 >= ';' && c3 < 'A')))
					{
						Result = TGEDCOMObject.TGEDCOMSex.svMale;
					}
				}
			}
			IL_AE:
			if (aQuery && Result == TGEDCOMObject.TGEDCOMSex.svNone)
			{
				if (TGKSys.ShowQuestion(string.Concat(new string[]
				{
					"Не определяется пол человека по имени \"", 
					f_name, 
					" ", 
					f_pat, 
					"\". Это мужской пол?"
				})) == DialogResult.Yes)
				{
					Result = TGEDCOMObject.TGEDCOMSex.svMale;
				}
				else
				{
					Result = TGEDCOMObject.TGEDCOMSex.svFemale;
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
				Result = VCLUtils.StrToIntDef(xref, 0);
			}
			catch (Exception E)
			{
				Result = -1;
			}
			return Result;
		}

		public static string GEDCOMDateToStr(TGEDCOMDate aDate, TGenEngine.TDateFormat aFormat)
		{
			string Result = "";
			int year = 0;
			ushort month = 0;
			ushort day = 0;
			aDate.GetDate(ref year, ref month, ref day);
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
								Result = year.ToString();
							}
						}
					}
					else
					{
						if (year > 0)
						{
							Result = Result + year.ToString() + ".";
						}
						else
						{
							Result += "____.";
						}
						if (month > 0)
						{
							Result = Result + TGKSys.NumUpdate((int)month, 2) + ".";
						}
						else
						{
							Result += "__.";
						}
						if (day > 0)
						{
							Result += TGKSys.NumUpdate((int)day, 2);
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
						Result = Result + TGKSys.NumUpdate((int)day, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (month > 0)
					{
						Result = Result + TGKSys.NumUpdate((int)month, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (year > 0)
					{
						Result += year.ToString();
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
			if (BDSSystem.Pos("/", aDate) > 0)
			{
				aDate = VCLUtils.StringReplace(aDate, "/", ".", TReplaceFlags.rfReplaceAll);
			}
			int cnt = TGKSys.GetTokensCount(aDate, '.');
			if (cnt < 3)
			{
				if (aException)
				{
					throw new Exception("date failed");
				}
			}
			else
			{
				string pd = TGKSys.GetToken(aDate, '.', 1).Trim();
				string pm = TGKSys.GetToken(aDate, '.', 2).Trim();
				string py = TGKSys.GetToken(aDate, '.', 3).Trim();
				if (BDSSystem.WStrCmp(pd, "") != 0)
				{
					Result = Result + pd + " ";
				}
				if (BDSSystem.WStrCmp(pm, "") != 0)
				{
					Result = Result + TGEDCOMDate.GEDCOMMonthArray[TGKSys.StrToInt(pm) - 1] + " ";
				}
				if (BDSSystem.WStrCmp(py, "") != 0)
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
					if (aSign && (aDate as TGEDCOMDateApproximated).Approximated != TGEDCOMDateApproximated.TGEDCOMApproximated.daExact)
					{
						Result = "~ " + Result;
					}
				}
				else
				{
					if (aDate is TGEDCOMDateRange)
					{
						TGEDCOMDateRange dt_range = aDate as TGEDCOMDateRange;
						if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") == 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") != 0)
						{
							Result = TGenEngine.GEDCOMDateToStr(dt_range.Before, aFormat);
							if (aSign)
							{
								Result = "< " + Result;
							}
						}
						else
						{
							if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") == 0)
							{
								Result = TGenEngine.GEDCOMDateToStr(dt_range.After, aFormat);
								if (aSign)
								{
									Result += " >";
								}
							}
							else
							{
								if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") != 0)
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
							if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") == 0)
							{
								Result = TGenEngine.GEDCOMDateToStr(dt_period.DateFrom, aFormat);
								if (aSign)
								{
									Result += " >";
								}
							}
							else
							{
								if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") == 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") != 0)
								{
									Result = TGenEngine.GEDCOMDateToStr(dt_period.DateTo, aFormat);
									if (aSign)
									{
										Result = "< " + Result;
									}
								}
								else
								{
									if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") != 0)
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
				int year = 0;
				ushort month = 0;
				ushort day = 0;
				TGenEngine.GetIndependentDate(aDate, ref year, ref month, ref day);
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
					Result = new DateTime((long)((ulong)0));
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
				TGKSys.LogWrite(string.Format("GEDCOMDateToDate(%d, %d, %d): ", new object[]
				{
					year, 
					month, 
					day
				}) + E.Message);
				TGKSys.LogWrite("Record (" + (aDate.ParentRecord as TGEDCOMRecord).XRef + "): invalid date");
				Result = new DateTime((long)((ulong)0));
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
				int arg_0F_0 = 0;
				int num = fRec.GetFamilyEventCount() - 1;
				int i = arg_0F_0;
				if (num >= i)
				{
					num++;
					TGEDCOMFamilyEvent @event;
					while (true)
					{
						@event = fRec.GetFamilyEvent(i);
						if (BDSSystem.WStrCmp(@event.Name, evName) == 0)
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
			while (BDSSystem.Pos("__.", Result) == 1)
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
			if (BDSSystem.WStrCmp(ds, "") == 0)
			{
				ds = "?";
			}
			Result += ds;
			ds = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
			if (BDSSystem.WStrCmp(ds, "") == 0)
			{
				TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, "DEAT");
				if (ev != null)
				{
					ds = "?";
				}
			}
			if (BDSSystem.WStrCmp(ds, "") != 0)
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
					if (BDSSystem.WStrCmp(resi, "") != 0 && BDSSystem.WStrCmp(addr, "") != 0)
					{
						resi += ", ";
					}
					resi += addr;
					if (BDSSystem.WStrCmp(resi, "") != 0)
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
			if (BDSSystem.WStrCmp(place, "") != 0)
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
			if (BDSSystem.WStrCmp(place, "") != 0 && location != null)
			{
				place = TGenEngine.HyperLink(location.XRef, place, 0);
			}
			string Result;
			if (BDSSystem.WStrCmp(dt, "") == 0 && BDSSystem.WStrCmp(place, "") == 0)
			{
				Result = "?";
			}
			else
			{
				if (BDSSystem.WStrCmp(dt, "") == 0)
				{
					Result = place;
				}
				else
				{
					if (BDSSystem.WStrCmp(place, "") == 0)
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
			if (BDSSystem.WStrCmp(evDetail.Cause, "") != 0)
			{
				Result += evDetail.Cause;
			}
			if (BDSSystem.WStrCmp(evDetail.Agency, "") != 0)
			{
				if (BDSSystem.WStrCmp(Result, "") != 0)
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
			int arg_10_0 = 0;
			int num = iRec.IndividualEventsCount - 1;
			int i = arg_10_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMCustomEvent ev = iRec.GetIndividualEvent(i);
					if (BDSSystem.WStrCmp(ev.Name, "BIRT") == 0)
					{
						aBirthEvent = ev;
					}
					else
					{
						if (BDSSystem.WStrCmp(ev.Name, "DEAT") == 0)
						{
							aDeathEvent = ev;
						}
					}
					i++;
				}
				while (i != num);
			}
		}

		public static void GetIndependentDate(TGEDCOMCustomDate aDate, ref int AYear, ref ushort AMonth, ref ushort ADay)
		{
			if (aDate is TGEDCOMDateApproximated)
			{
				(aDate as TGEDCOMDate).GetDate(ref AYear, ref AMonth, ref ADay);
			}
			else
			{
				if (aDate is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dt_range = aDate as TGEDCOMDateRange;
					if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") == 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") != 0)
					{
						dt_range.Before.GetDate(ref AYear, ref AMonth, ref ADay);
					}
					else
					{
						if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") == 0)
						{
							dt_range.After.GetDate(ref AYear, ref AMonth, ref ADay);
						}
						else
						{
							if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") != 0)
							{
								dt_range.After.GetDate(ref AYear, ref AMonth, ref ADay);
							}
						}
					}
				}
				else
				{
					if (aDate is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dt_period = aDate as TGEDCOMDatePeriod;
						if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") == 0)
						{
							dt_period.DateFrom.GetDate(ref AYear, ref AMonth, ref ADay);
						}
						else
						{
							if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") == 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") != 0)
							{
								dt_period.DateTo.GetDate(ref AYear, ref AMonth, ref ADay);
							}
							else
							{
								if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") != 0)
								{
									dt_period.DateFrom.GetDate(ref AYear, ref AMonth, ref ADay);
								}
							}
						}
					}
					else
					{
						if (aDate is TGEDCOMDate)
						{
							(aDate as TGEDCOMDate).GetDate(ref AYear, ref AMonth, ref ADay);
						}
					}
				}
			}
		}

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign)
		{
			TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, evSign);
			int Result = -1;
			if (ev == null)
			{
				Result = -1;
			}
			else
			{
				int year = 0;
				ushort am = 0;
				ushort ad = 0;
				TGenEngine.GetIndependentDate(ev.Detail.Date.Value, ref year, ref am, ref ad);
				Result = year;
			}
			return Result;
		}

		public static double GetAbstractDate(TGEDCOMEventDetail aEventDetail)
		{
			double Result = 0.0;
			int y = 0;
			ushort i = 0;
			ushort d = 0;
			TGenEngine.GetIndependentDate(aEventDetail.Date.Value, ref y, ref i, ref d);
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
						Result = BDSSystem.Trunc(y2 - y).ToString();
					}
				}
			}
			catch (Exception E)
			{
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
				int arg_0F_0 = 0;
				int num = iRec.IndividualEventsCount - 1;
				int i = arg_0F_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMCustomEvent @event = iRec.GetIndividualEvent(i);
						if (BDSSystem.WStrCmp(@event.Name, "BIRT") == 0)
						{
							ev = @event;
						}
						else
						{
							if (BDSSystem.WStrCmp(@event.Name, "DEAT") == 0)
							{
								ev2 = @event;
							}
						}
						i++;
					}
					while (i != num);
				}
				Result = TGenEngine.GetEventsYearsDiff(ev, ev2, false);
			}
			catch (Exception E)
			{
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
				int arg_0E_0 = 0;
				int num = iRec.IndividualEventsCount - 1;
				int i = arg_0E_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMCustomEvent @event = iRec.GetIndividualEvent(i);
						if (BDSSystem.WStrCmp(@event.Name, "BIRT") == 0)
						{
							ev = @event;
						}
						else
						{
							if (BDSSystem.WStrCmp(@event.Name, "DEAT") == 0)
							{
								ev2 = @event;
							}
						}
						i++;
					}
					while (i != num);
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
						ushort dummy = 0;
						TGenEngine.GetIndependentDate(ev.Detail.Date.Value, ref i, ref dummy, ref dummy);
						Result = Convert.ToString(ToYear - i);
					}
				}
			}
			catch (Exception E)
			{
			}
			return Result;
		}

		public static int GetFirstbornAge(TGEDCOMIndividualRecord iRec)
		{
			int Result = 0;
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
					int arg_42_0 = 0;
					int num = iRec.SpouseToFamilyLinksCount - 1;
					int i = arg_42_0;
					if (num >= i)
					{
						num++;
						do
						{
							TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(i).Family;
							int arg_6D_0 = 0;
							int num2 = family.ChildrenCount - 1;
							int j = arg_6D_0;
							if (num2 >= j)
							{
								num2++;
								do
								{
									TGEDCOMIndividualRecord child = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
									@event = TGenEngine.GetIndividualEvent(child, "BIRT");
									if (@event != null)
									{
										double y2tmp = TGenEngine.GetAbstractDate(@event.Detail);
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
									j++;
								}
								while (j != num2);
							}
							i++;
						}
						while (i != num);
					}
					if (y3 > (double)1f && y2 > (double)1f)
					{
						Result = (int)BDSSystem.Trunc(y2 - y3);
					}
				}
			}
			catch (Exception E)
			{
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
					int arg_44_0 = 0;
					int num = iRec.SpouseToFamilyLinksCount - 1;
					int i = arg_44_0;
					if (num >= i)
					{
						num++;
						do
						{
							TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(i).Family;
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
							i++;
						}
						while (i != num);
					}
					if (y3 > (double)1f && y2 > (double)1f)
					{
						Result = (int)BDSSystem.Trunc(y2 - y3);
					}
				}
			}
			catch (Exception E)
			{
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
							int bd_y = -1;
							ushort bd_m = 0;
							ushort bd_d = 0;
							dt.GetDate(ref bd_y, ref bd_m, ref bd_d);
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
									bd_y = (int)((uint)cur_y + 1u);
								}
								else
								{
									bd_y = (int)cur_y;
								}
								Result = Convert.ToString(TGKSys.DaysBetween(new DateTime((int)cur_y, (int)cur_m, (int)cur_d), new DateTime(bd_y, (int)bd_m, (int)bd_d)));
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				string xr = iRec.XRef;
				object obj = xr;
				int num = TGKSys.Hole(ref obj);
				xr = (obj as string);
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
				fam_rec.AddFamilyEvent(Result as TGEDCOMFamilyEvent);
			}
			Result.Name = evSign;
			if (BDSSystem.WStrCmp(evDate, "") != 0)
			{
				Result.Detail.Date.ParseString(evDate);
			}
			if (BDSSystem.WStrCmp(evPlace, "") != 0)
			{
				Result.Detail.Place.StringValue = evPlace;
			}
			return Result;
		}

		public static TGEDCOMIndividualRecord CreatePersonEx(TGEDCOMTree aTree, string aName, string aPatronymic, string aFamily, TGEDCOMObject.TGEDCOMSex aSex, bool aBirthEvent)
		{
			TGEDCOMIndividualRecord Result = new TGEDCOMIndividualRecord(aTree, aTree, "", "");
			Result.InitNew();
			Result.Sex = aSex;
			TGEDCOMPersonalName pn = new TGEDCOMPersonalName(aTree, Result, "", "");
			pn.StringValue = string.Concat(new string[]
			{
				aName.Trim(), 
				" ", 
				aPatronymic.Trim(), 
				" /", 
				aFamily.Trim(), 
				"/"
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
			aRecord.AddNotes(note);
		}

		public static void AddNoteText(TGEDCOMNoteRecord aNoteRec, string aText)
		{
			TStringList strData = new TStringList();
			try
			{
				strData.Text = aNoteRec.Notes.Text.Trim();
				strData.Add(aText);
				aNoteRec.Notes = strData;
			}
			finally
			{
				strData.Free();
			}
		}

		public static TGEDCOMNoteRecord CreateNoteEx(TGEDCOMTree aTree, TStrings aText, TGEDCOMRecord aRecord)
		{
			TGEDCOMNoteRecord Result = TGenEngine.CreateNote(aTree);
			if (aText != null)
			{
				Result.Notes = aText;
			}
			if (aRecord != null)
			{
				TGenEngine.BindRecordNote(aTree, aRecord, Result);
			}
			return Result;
		}

		public static TGEDCOMSourceRecord CreateSource(TGEDCOMTree aTree)
		{
			TGEDCOMSourceRecord Result = new TGEDCOMSourceRecord(aTree, aTree, "", "");
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
			aRecord.AddSourceCitation(cit);
		}


		public static void BindSourceRepository(TGEDCOMTree aTree, TGEDCOMSourceRecord aSourceRecord, TGEDCOMRepositoryRecord aRepRec)
		{
			TGEDCOMRepositoryCitation cit = new TGEDCOMRepositoryCitation(aTree, aSourceRecord, "", "");
			cit.Value = aRepRec;
			aSourceRecord.AddRepositoryCitation(cit);
		}


		public static void GetLocationLinks(TGEDCOMTree aTree, TGEDCOMLocationRecord aLocation, ref TStringList aList)
		{
			int arg_0B_0 = 0;
			int num = aTree.RecordsCount - 1;
			int i = arg_0B_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMRecord rec = aTree.GetRecord(i);
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
						int arg_40_0 = 0;
						int num2 = i_rec.IndividualEventsCount - 1;
						int j = arg_40_0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								TGEDCOMCustomEvent @event = i_rec.GetIndividualEvent(j);
								if (object.Equals(@event.Detail.Place.Location.Value, aLocation))
								{
									aList.Add(TGenEngine.GenRecordLink(aTree, rec, true) + ", " + TGenEngine.GetEventName(@event).ToLower());
								}
								j++;
							}
							while (j != num2);
						}
					}
					else
					{
						if (rec is TGEDCOMFamilyRecord)
						{
							TGEDCOMFamilyRecord f_rec = (TGEDCOMFamilyRecord)rec;
							int arg_BF_0 = 0;
							int num3 = f_rec.GetFamilyEventCount() - 1;
							int j = arg_BF_0;
							if (num3 >= j)
							{
								num3++;
								do
								{
									TGEDCOMCustomEvent @event = f_rec.GetFamilyEvent(j);
									if (object.Equals(@event.Detail.Place.Location.Value, aLocation))
									{
										aList.Add(TGenEngine.GenRecordLink(aTree, rec, true) + ", " + TGenEngine.GetEventName(@event).ToLower());
									}
									j++;
								}
								while (j != num3);
							}
						}
					}
					i++;
				}
				while (i != num);
			}
		}

		public static bool IsMatchesMask([In] string aName, [In] string aMask)
		{
			bool Result = false;
			if (BDSSystem.WStrCmp(aName, "") != 0 && BDSSystem.WStrCmp(aMask, "") != 0)
			{
				string strx = aName.ToLower();
				string[] sts = aMask.ToLower().Split(new char[]
				{
					'|'
				});
				int arg_4F_0 = 0;
				int num = ((sts != null) ? sts.Length : 0) - 1;
				int i = arg_4F_0;
				if (num >= i)
				{
					num++;
					do
					{
						Result = (Result || VCLUtils.MatchesMask(strx, sts[i]));
						i++;
					}
					while (i != num);
				}
			}
			return Result;
		}

		public static string HyperLink(string XRef, string Text, int Num)
		{
			string Result = "~^" + XRef;
			if (BDSSystem.WStrCmp(Text, "") != 0)
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
					TGEDCOMRecord.TGEDCOMRecordType recordType = aRecord.RecordType;
					if (recordType != TGEDCOMRecord.TGEDCOMRecordType.rtIndividual)
					{
						if (recordType == TGEDCOMRecord.TGEDCOMRecordType.rtFamily || (byte)recordType - (byte)TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia < (byte)TGEDCOMRecord.TGEDCOMRecordType.rtResearch)
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
					case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
					{
						st = TGenEngine.GetNameStr(aRecord as TGEDCOMIndividualRecord, true, false);
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
					{
						st = TGenEngine.GetFamilyStr(aRecord as TGEDCOMFamilyRecord);
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
					{
						st = (aRecord as TGEDCOMMultimediaRecord).GetFileReference(0).Title;
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
					{
						st = (aRecord as TGEDCOMSourceRecord).FiledByEntry;
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
					{
						st = (aRecord as TGEDCOMRepositoryRecord).RepositoryName;
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
					{
						st = (aRecord as TGEDCOMGroupRecord).Name;
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtResearch:
					{
						st = (aRecord as TGEDCOMResearchRecord).Name;
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
					{
						st = TGenEngine.GetTaskGoalStr(aTree, aRecord as TGEDCOMTaskRecord);
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
					{
						st = (aRecord as TGEDCOMCommunicationRecord).Name;
						goto IL_13D;
					}
					case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
					{
						st = (aRecord as TGEDCOMLocationRecord).Name;
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
			TGEDCOMCommunicationRecord.TCommunicationDir dir = TGEDCOMCommunicationRecord.TCommunicationDir.cdFrom;
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
						gret.lngCountLike = (ushort)((uint)gret.lngCountLike + (uint)tret.lngCountLike);
						gret.lngSubRows = (ushort)((uint)gret.lngSubRows + (uint)tret.lngSubRows);
						tret = TGenEngine.Matching(strInputStandart, strInputMatching, lngCurLen);
						gret.lngCountLike = (ushort)((uint)gret.lngCountLike + (uint)tret.lngCountLike);
						gret.lngSubRows = (ushort)((uint)gret.lngSubRows + (uint)tret.lngSubRows);
						lngCurLen++;
					}
					while (lngCurLen != num);
				}
				if (gret.lngSubRows != 0)
				{
					Result = (int)BDSSystem.Trunc(gret.lngCountLike / gret.lngSubRows * 100.0);
				}
			}
			return Result;
		}

		public static string ClearFamily(string aFamily)
		{
			int p = BDSSystem.Pos(" (", aFamily);
			string Result;
			if (p > 0)
			{
				Result = BDSSystem.WStrCopy(aFamily, 1, p - 1);
			}
			else
			{
				Result = aFamily;
			}
			return Result;
		}

		public static string PrepareRusFamily(string f, bool aFemale)
		{
			int p = BDSSystem.Pos(" (", f);
			if (p > 0)
			{
				f = BDSSystem.WStrCopy(f, 1, p - 1);
			}
			if (((f != null) ? f.Length : 0) > 0)
			{
				if (aFemale)
				{
					if (f[((f != null) ? f.Length : 0) - 1] == 'а')
					{
						f = BDSSystem.WStrCopy(f, 1, ((f != null) ? f.Length : 0) - 1);
					}
					else
					{
						if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(f, ((f != null) ? f.Length : 0) - 2, 3), "кая") == 0)
						{
							f = BDSSystem.WStrCopy(f, 1, ((f != null) ? f.Length : 0) - 3) + "кий";
						}
						else
						{
							if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(f, ((f != null) ? f.Length : 0) - 2, 3), "ная") == 0)
							{
								f = BDSSystem.WStrCopy(f, 1, ((f != null) ? f.Length : 0) - 3) + "ный";
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
			if (aPerson != null && aPerson.SpouseToFamilyLinksCount > 0)
			{
				int arg_18_0 = 0;
				int num = aPerson.SpouseToFamilyLinksCount - 1;
				int i = arg_18_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.GetSpouseToFamilyLink(i).Family;
						Result += family.ChildrenCount;
						i++;
					}
					while (i != num);
				}
			}
			return Result;
		}

		public static void InitExtCounts(TGEDCOMTree aTree, int aValue)
		{
			int arg_0A_0 = 0;
			int num = aTree.RecordsCount - 1;
			int i = arg_0A_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMRecord rec = aTree.GetRecord(i);
					if (rec is TGEDCOMIndividualRecord)
					{
						rec.ExtData = aValue;
					}
					i++;
				}
				while (i != num);
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
					if (aPerson.ChildToFamilyLinksCount > 0)
					{
						TGEDCOMFamilyRecord family = aPerson.GetChildToFamilyLink(0).Family;
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
					int arg_2A_0 = 0;
					int num = aPerson.SpouseToFamilyLinksCount - 1;
					int i = arg_2A_0;
					if (num >= i)
					{
						num++;
						do
						{
							TGEDCOMFamilyRecord family = aPerson.GetSpouseToFamilyLink(i).Family;
							int arg_50_0 = 0;
							int num2 = family.ChildrenCount - 1;
							int j = arg_50_0;
							if (num2 >= j)
							{
								num2++;
								do
								{
									TGEDCOMIndividualRecord iChild = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
									val += TGenEngine.GetDescendantsCount(iChild);
									j++;
								}
								while (j != num2);
							}
							i++;
						}
						while (i != num);
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
				Result = aPerson.SpouseToFamilyLinksCount;
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
						Result = (int)BDSSystem.Trunc((Math.Abs((y2 - y))));
					}
				}
			}
			catch (Exception E)
			{
			}
			return Result;
		}

		public static void CheckRecord(TGEDCOMTree aTree, TGEDCOMRecord aRec, TGEDCOMFormat aFormat)
		{
			if (BDSSystem.WStrCmp(aRec.UID, "") == 0)
			{
				aRec.NewUID();
			}
			if (aFormat != TGEDCOMFormat.gf_Native)
			{
				int arg_40_0 = 0;
				int num = aRec.GetMultimediaLinksCount() - 1;
				int i = arg_40_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMMultimediaLink mmLink = aRec.GetMultimediaLink(i);
						if (!mmLink.IsPointer)
						{
							TGenEngine.ReformMultimediaLink(aTree, mmLink);
						}
						i++;
					}
					while (i != num);
				}
				int arg_7E_0 = 0;
				int num2 = aRec.GetNotesCount() - 1;
				i = arg_7E_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TGEDCOMNotes note = aRec.GetNote(i);
						if (!note.IsPointer)
						{
							TGenEngine.ReformNote(aTree, note);
						}
						i++;
					}
					while (i != num2);
				}
				int arg_BC_0 = 0;
				int num3 = aRec.GetSourceCitationsCount() - 1;
				i = arg_BC_0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						TGEDCOMSourceCitation sourCit = aRec.GetSourceCitation(i);
						if (!sourCit.IsPointer)
						{
							TGenEngine.ReformSourceCitation(aTree, sourCit);
						}
						i++;
					}
					while (i != num3);
				}
			}
			TGEDCOMRecord.TGEDCOMRecordType recordType = aRec.RecordType;
			if (recordType != TGEDCOMRecord.TGEDCOMRecordType.rtIndividual)
			{
				if (recordType != TGEDCOMRecord.TGEDCOMRecordType.rtFamily)
				{
					if (recordType == TGEDCOMRecord.TGEDCOMRecordType.rtGroup)
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
			while (BDSSystem.WStrCmp(TGenEngine.GEDCOMFormats[(int)gf].Sign, sour) != 0)
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
				if (!idCheck && TGKSys.ShowQuestion(GKL.LSList[471]) == DialogResult.Yes)
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
					if ((aMode == TGenEngine.TTreeWalkMode.twmAll || aMode == TGenEngine.TTreeWalkMode.twmAncestors) && iRec.ChildToFamilyLinksCount > 0)
					{
						TGEDCOMFamilyRecord family = iRec.GetChildToFamilyLink(0).Family;
						TGEDCOMIndividualRecord rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
						TGenEngine.TreeWalk(rel_person, aMode, aList);
						rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
						TGenEngine.TreeWalk(rel_person, aMode, aList);
					}
					if (aMode < TGenEngine.TTreeWalkMode.twmAncestors || aMode == TGenEngine.TTreeWalkMode.twmDescendants)
					{
						int arg_8A_0 = 0;
						int num = iRec.SpouseToFamilyLinksCount - 1;
						int i = arg_8A_0;
						if (num >= i)
						{
							num++;
							do
							{
								TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(i).Family;
								TGEDCOMPointer sp;
								if (iRec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
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
								int arg_10A_0 = 0;
								int num2 = family.ChildrenCount - 1;
								int j = arg_10A_0;
								if (num2 >= j)
								{
									num2++;
									do
									{
										rel_person = (family.GetChildren(j).Value as TGEDCOMIndividualRecord);
										TGenEngine.TreeWalk(rel_person, int_mode, aList);
										j++;
									}
									while (j != num2);
								}
								i++;
							}
							while (i != num);
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
				int arg_B0_0 = 0;
				int num = repMap.Count - 1;
				int i = arg_B0_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = repMap.GetRecord(i).Rec;
						rec.ReplaceXRefs(repMap);
						i++;
					}
					while (i != num);
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
				int arg_40_0 = 0;
				int num = extTree.RecordsCount - 1;
				int i = arg_40_0;
				if (num >= i)
				{
					num++;
					do
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
						i++;
					}
					while (i != num);
				}
				int arg_A3_0 = 0;
				int num2 = sync_list.Count - 1;
				i = arg_A3_0;
				if (num2 >= i)
				{
					num2++;
					do
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
						i++;
					}
					while (i != num2);
				}
				int arg_147_0 = 0;
				int num3 = repMap.Count - 1;
				i = arg_147_0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						TGEDCOMRecord rec = repMap.GetRecord(i).Rec;
						rec.ReplaceXRefs(repMap);
						i++;
					}
					while (i != num3);
				}
				int arg_181_0 = 0;
				int num4 = extTree.RecordsCount - 1;
				i = arg_181_0;
				if (num4 >= i)
				{
					num4++;
					do
					{
						TGEDCOMRecord rec = extTree.GetRecord(i);
						rec.ReplaceXRefs(repMap);
						i++;
					}
					while (i != num4);
				}
				int arg_1B2_0 = 0;
				int num5 = sync_list.Count - 1;
				i = arg_1B2_0;
				if (num5 >= i)
				{
					num5++;
					do
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
						i++;
					}
					while (i != num5);
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


			TGenEngine.S7[] array6 = new TGenEngine.S7[36];
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
				if (!Result && descendantRec.ChildToFamilyLinksCount > 0)
				{
					TGEDCOMFamilyRecord family = descendantRec.GetChildToFamilyLink(0).Family;
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
			int arg_0D_0 = 0;
			int num = ancestorRec.SpouseToFamilyLinksCount - 1;
			int i = arg_0D_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMFamilyRecord family = ancestorRec.GetSpouseToFamilyLink(i).Family;
					TGEDCOMPointer sp;
					if (ancestorRec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
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
					int arg_73_0 = 0;
					int num2 = family.ChildrenCount - 1;
					int j = arg_73_0;
					if (num2 >= j)
					{
						num2++;
						do
						{
							TGEDCOMIndividualRecord child = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
							Result = TGenEngine._GetPatriarchsList_SearchDesc(child, searchRec);
							if (Result)
							{
								return Result;
							}
							j++;
						}
						while (j != num2);
					}
					i++;
				}
				while (i != num);
			}
			return Result;
		}
		private static int _GetPatriarchsList_GetBirthYear([In] TGenEngine Self, TGEDCOMIndividualRecord iRec)
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
					int arg_34_0 = 0;
					int num = iRec.SpouseToFamilyLinksCount - 1;
					int i = arg_34_0;
					if (num >= i)
					{
						num++;
						while (true)
						{
							TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(i).Family;
							int arg_5C_0 = 0;
							int num2 = family.ChildrenCount - 1;
							int j = arg_5C_0;
							if (num2 >= j)
							{
								num2++;
								do
								{
									TGEDCOMIndividualRecord child = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
									year = TGenEngine._GetPatriarchsList_GetBirthYear(Self, child);
									if (year > 0)
									{
										goto Block_5;
									}
									j++;
								}
								while (j != num2);
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
			int arg_0B_0 = 0;
			int num = tag.GetMultimediaLinksCount() - 1;
			int i = arg_0B_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMMultimediaLink mmLink = tag.GetMultimediaLink(i);
					if (!mmLink.IsPointer)
					{
						ReformMultimediaLink(aTree, mmLink);
					}
					i++;
				}
				while (i != num);
			}
			int arg_48_0 = 0;
			int num2 = tag.GetNotesCount() - 1;
			i = arg_48_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					TGEDCOMNotes note = tag.GetNote(i);
					if (!note.IsPointer)
					{
						ReformNote(aTree, note);
					}
					i++;
				}
				while (i != num2);
			}
			int arg_85_0 = 0;
			int num3 = tag.GetSourceCitationsCount() - 1;
			i = arg_85_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TGEDCOMSourceCitation sourCit = tag.GetSourceCitation(i);
					if (!sourCit.IsPointer)
					{
						ReformSourceCitation(aTree, sourCit);
					}
					i++;
				}
				while (i != num3);
			}
		}
		private static void _CheckRecord_PreparePtr([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMPointerWithNotes ptr)
		{
			int arg_0A_0 = 0;
			int num = ptr.GetNotesCount() - 1;
			int i = arg_0A_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMNotes note = ptr.GetNote(i);
					if (!note.IsPointer)
					{
						ReformNote(aTree, note);
					}
					i++;
				}
				while (i != num);
			}
		}
		private static void _CheckRecord_CheckEventPlace(TGEDCOMCustomEvent aEvent)
		{
			TGEDCOMPlace place = aEvent.Detail.Place;
			if (BDSSystem.WStrCmp(place.Location.XRef, "") != 0 && place.Location.Value == null)
			{
				place.Location.XRef = "";
			}
			if (BDSSystem.WStrCmp(place.StringValue, "") != 0)
			{
				TGEDCOMLocationRecord loc = place.Location.Value as TGEDCOMLocationRecord;
				if (loc != null && BDSSystem.WStrCmp(place.StringValue, loc.Name) != 0)
				{
					place.StringValue = loc.Name;
				}
			}
		}

		private static void _CheckRecord_AddUserRef([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec, string uRef)
		{
			TGEDCOMUserReference @ref = new TGEDCOMUserReference(aTree, iRec, "", "");
			@ref.StringValue = uRef;
			iRec.AddUserReference(@ref);
		}

		private static void _CheckRecord_CheckAttrCompatible([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec, TGEDCOMCustomEvent aEvent)
		{
			if (BDSSystem.WStrCmp(aEvent.Name, "_MILI") == 0)
			{
				string cause = aEvent.Detail.Classification.ToLower();
				if (BDSSystem.Pos("б/д", cause) > 0)
				{
					if (BDSSystem.Pos("+", cause) > 0)
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
					if (BDSSystem.Pos("т/т", cause) > 0)
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
				int arg_13_0 = 0;
				int num = ind.IndividualEventsCount - 1;
				i = arg_13_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMCustomEvent evt = ind.GetIndividualEvent(i);
						TGenEngine._CheckRecord_CheckEventPlace(evt);
						_CheckRecord_CheckAttrCompatible(aTree, aFormat, ind, evt);
						i++;
					}
					while (i != num);
				}
				int arg_44_0 = 0;
				int num2 = ind.GetUserReferencesCount() - 1;
				i = arg_44_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TGenEngine._CheckRecord_CheckURefCompatible(ind, ind.GetUserReference(i));
						i++;
					}
					while (i != num2);
				}
			}
			else
			{
				int arg_75_0 = 0;
				int num3 = ind.IndividualEventsCount - 1;
				i = arg_75_0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						_CheckRecord_PrepareTag(aTree, aFormat, ind.GetIndividualEvent(i).Detail);
						i++;
					}
					while (i != num3);
				}
				int arg_A8_0 = 0;
				int num4 = ind.ChildToFamilyLinksCount - 1;
				i = arg_A8_0;
				if (num4 >= i)
				{
					num4++;
					do
					{
						_CheckRecord_PreparePtr(aTree, aFormat, ind.GetChildToFamilyLink(i));
						i++;
					}
					while (i != num4);
				}
				int arg_D6_0 = 0;
				int num5 = ind.SpouseToFamilyLinksCount - 1;
				i = arg_D6_0;
				if (num5 >= i)
				{
					num5++;
					do
					{
						_CheckRecord_PreparePtr(aTree, aFormat, ind.GetSpouseToFamilyLink(i));
						i++;
					}
					while (i != num5);
				}
				int arg_104_0 = 0;
				int num6 = ind.AssociationsCount - 1;
				i = arg_104_0;
				if (num6 >= i)
				{
					num6++;
					do
					{
						_CheckRecord_PreparePtr(aTree, aFormat, ind.GetAssociation(i));
						i++;
					}
					while (i != num6);
				}
			}
			i = ind.ChildToFamilyLinksCount - 1;
			if (i >= 0)
			{
				do
				{
					if (ind.GetChildToFamilyLink(i).Family == null)
					{
						ind.DeleteChildToFamilyLink(i);
					}
					i--;
				}
				while (i != -1);
			}
			i = ind.SpouseToFamilyLinksCount - 1;
			if (i >= 0)
			{
				do
				{
					if (ind.GetSpouseToFamilyLink(i).Family == null)
					{
						ind.DeleteSpouseToFamilyLink(i);
					}
					i--;
				}
				while (i != -1);
			}
		}

		private static void _CheckRecord_CheckFamily([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMFamilyRecord fam)
		{
			int i;
			if (aFormat == TGEDCOMFormat.gf_Native)
			{
				int arg_13_0 = 0;
				int num = fam.GetFamilyEventCount() - 1;
				i = arg_13_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGenEngine._CheckRecord_CheckEventPlace(fam.GetFamilyEvent(i));
						i++;
					}
					while (i != num);
				}
			}
			else
			{
				int arg_3C_0 = 0;
				int num2 = fam.GetFamilyEventCount() - 1;
				i = arg_3C_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						_CheckRecord_PrepareTag(aTree, aFormat, fam.GetFamilyEvent(i).Detail);
						i++;
					}
					while (i != num2);
				}
			}
			i = fam.ChildrenCount - 1;
			if (i >= 0)
			{
				do
				{
					if (fam.GetChildren(i).Value == null)
					{
						fam.DeleteChild(i);
					}
					i--;
				}
				while (i != -1);
			}
			fam.SortChilds();
		}
		private static void _CheckRecord_CheckGroup(TGEDCOMGroupRecord group)
		{
			int i = group.GetMembersCount() - 1;
			if (i >= 0)
			{
				do
				{
					TGEDCOMPointer ptr = group.GetMember(i);
					TGEDCOMIndividualRecord irec = ptr.Value as TGEDCOMIndividualRecord;
					if (irec == null)
					{
						group.DeleteMember(i);
					}
					else
					{
						if (irec.IndexOfGroup(group) < 0)
						{
							group.DeleteMember(i);
						}
					}
					i--;
				}
				while (i != -1);
			}
		}
	}
}
