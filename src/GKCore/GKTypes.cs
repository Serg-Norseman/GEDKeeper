using System;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	[Serializable]
	public class MediaFileNotFoundException : Exception
	{
		
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
		tmChildToFamily,
		tmWife
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

}
