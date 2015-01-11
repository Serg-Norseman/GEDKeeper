using System;
using System.Collections.Generic;

using ExtUtils;
using GedCom551;

/// <summary>
/// 
/// </summary>

namespace GKCore
{
	[Serializable]
	public class MediaFileNotFoundException : Exception
	{
		
	}

	public enum MediaStoreType
	{
		mstReference,
		mstStorage,
		mstArchive
	}

	public enum RecordAction
	{
		raAdd,
		raEdit,
		raDelete,
		raJump,
		raMoveUp,
		raMoveDown
	}

	public enum TargetMode
	{
		tmNone,
		tmParent,
		tmChild,
		tmChildToFamily,
		tmWife
	}

	public enum FamilyTarget
	{
		ftNone,
		ftSpouse,
		ftChild
	}

	public enum ShieldState
	{
		ssMaximum,
		ssMiddle,
		ssNone
	}

	public enum DateFormat
	{
		dfDD_MM_YYYY,
		dfYYYY_MM_DD,
		dfYYYY
	}

	public enum NameFormat
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

	public enum TLifeMode
	{
		lmAll,
		lmOnlyAlive,
		lmOnlyDead,
		lmAliveBefore,
		lmTimeLocked
	}

	public enum TGroupMode
	{
		gmAll,
		gmNone,
		gmAny,
		gmSelected
	}

	public sealed class TPatriarchObj : BaseObject
	{
		public TGEDCOMIndividualRecord IRec;
		public int IBirthYear;
		public int IDescendantsCount;
		public int IDescGenerations;
		public List<TPatriarchObj> ILinks = new List<TPatriarchObj>();
		public bool HasLinks;
	}

	public enum NodeType
	{
		ntDefault,
		ntPatriarch,
		ntIntersection,
	}

	public sealed class PatriarchsGraphNode
	{
		public string FamilyXRef;
		public NodeType Type;

		public PatriarchsGraphNode(string label, NodeType type)
		{
			this.FamilyXRef = label;
			this.Type = type;
		}
	}

	public enum TRelationKind : byte
	{
		rkNone,		/* 0 */
		rkParent,	/* 1 */
		rkSpouse,	/* 2 */
		rkChild,	/* 3 */
		rkFather,	/* 4 */
		rkMother,	/* 5 */
		rkHusband,	/* 6 */
		rkWife,		/* 7 */
		rkSon,		/* 8 */
		rkDaughter,	/* 9 */
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

}
