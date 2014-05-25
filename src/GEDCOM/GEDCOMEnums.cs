using System;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public enum TGEDCOMFormat
	{
		gf_Unknown,
		gf_Native,
		gf_GENBOX,
		gf_ALTREE,
		gf_AGES,
		gf_PAF,
		
		gf_Last = gf_PAF
	}

	public enum TGEDCOMRecordType
	{
		rtNone,
		rtIndividual,
		rtFamily,
		rtNote,
		rtMultimedia,
		rtSource,
		rtRepository,
		rtGroup,
		rtResearch,
		rtTask,
		rtCommunication,
		rtLocation,
		rtSubmission,
		rtSubmitter,

		rtLast
	}

	public enum TGEDCOMState
	{
		osLoading,
		osReady
	}

	public enum TGEDCOMRestriction
	{
		rnNone,
		rnConfidential,
		rnLocked,
		rnPrivacy,

		rnLast = rnPrivacy
	}

	public enum TGEDCOMCharacterSet
	{
		csASCII,
		csANSEL,
		csUNICODE,
		csUTF8
	}

	public enum TGEDCOMNameType
	{
		ntNone,
		ntAka,
		ntBirth,
		ntImmigrant,
		ntMaiden,
		ntMarried
	}

	public enum TGEDCOMNamePieceType
	{
		nptPrefix,
		nptGiven,
		nptNickname,
		nptSurnamePrefix,
		nptSurname,
		nptSuffix
	}

	public enum TGEDCOMSex
	{
		svNone,
		svMale,
		svFemale,
		svUndetermined,

		svLast = svUndetermined
	}

	public enum TGEDCOMPedigreeLinkageType
	{
		plNone,
		plAdopted,
		plBirth,
		plFoster,
		plSealing
	}

	public enum TGEDCOMChildLinkageStatus
	{
		clNone,
		clChallenged,
		clDisproven,
		clProven
	}

	public enum TGEDCOMChildSealingDateStatus
	{
		cdsNone,
		cdsBIC,
		cdsExcluded,
		cdsPre1970,
		cdsStillborn,
		cdsSubmitted,
		cdsUncleared
	}

	public enum TGEDCOMBaptismDateStatus
	{
		bdsNone,
		bdsChild,
		bdsCompleted,
		bdsExcluded,
		bdsPre1970,
		bdsStillborn,
		bdsSubmitted,
		bdsUncleared
	}

	public enum TGEDCOMEndowmentDateStatus
	{
		edsNone,
		edsChild,
		edsCompleted,
		edsExcluded,
		edsInfant,
		edsPre1970,
		edsStillborn,
		edsSubmitted,
		edsUncleared
	}

	public enum TGEDCOMCalendar
	{
		dcGregorian,
		dcJulian,
		dcHebrew,
		dcFrench,
		dcRoman,
		dcUnknown,

		dcLast = dcUnknown
	}

	public enum TGEDCOMDateFormat
	{
		dfGEDCOMStd,
		dfSystem
	}

	public enum TGEDCOMApproximated
	{
		daExact,
		daAbout,
		daCalculated,
		daEstimated
	}

	public enum TGEDCOMRange
	{
		drAfter,
		drBefore,
		drBetween,
		drAnd
	}

	public enum TGEDCOMSpouseSealingDateStatus
	{
		sdsNone,
		sdsCanceled,
		sdsCompleted,
		sdsExcluded,
		sdsDNS,
		sdsDNSCAN,
		sdsPre1970,
		sdsSubmitted,
		sdsUncleared
	}

	public enum TGEDCOMOrdinanceProcessFlag
	{
		opNone,
		opYes,
		opNo
	}

	public enum TGEDCOMMultimediaFormat
	{
		mfNone,

		mfBMP,
		mfGIF,
		mfJPG,
		mfPCX,
		mfTIF,
		mfTGA,
		mfPNG,
		mfRAW, 

		mfTXT,
		mfRTF,
		mfHTM,

		mfWAV,
		mfMP3, 

		mfAVI,
		mfMPG,
		mfWMA, 

		mfOLE,
		mfUnknown
	}

	public enum TGEDCOMMediaType
	{
		mtNone,
		mtAudio,
		mtBook,
		mtCard,
		mtElectronic,
		mtFiche,
		mtFilm,
		mtMagazine,
		mtManuscript,
		mtMap,
		mtNewspaper,
		mtPhoto,
		mtTombstone,
		mtVideo,
		mtUnknown,

		mtLast = mtUnknown
	}

	public enum TCommunicationType
	{
		ctCall,
		ctEMail,
		ctFax,
		ctLetter,
		ctTape,
		ctVisit,
		
		ctLast = ctVisit
	}

	public enum TCommunicationDir
	{
		cdFrom,
		cdTo
	}

	public enum TGoalType
	{
		gtIndividual,
		gtFamily,
		gtSource,
		gtOther
	}

	public enum TResearchPriority
	{
		rpNone,
		rpLow,
		rpNormal,
		rpHigh,
		rpTop
	}

	public enum TResearchStatus
	{
		rsDefined,
		rsInProgress,
		rsOnHold,
		rsProblems,
		rsCompleted,
		rsWithdrawn
	}



//					this.WriteLine(1, "_HCOL", person.Biology.HairColor.ToString(), false);
//					this.WriteLine(1, "_LECL", person.Biology.LeftEyeColor.ToString(), false);
//					this.WriteLine(1, "_RECL", person.Biology.RightEyeColor.ToString(), false);
//					this.WriteLine(1, "_BGRO", person.Biology.BloodGroup.ToString(), false);
//					this.WriteLine(1, "_YDNA", person.Biology.HaplogroupY.ToString(), false);
//					this.WriteLine(1, "_MDNA", person.Biology.HaplogroupM.ToString(), false);

	[Serializable]
	public enum BloodGroup
	{
		APositive,
		ANegative,
		BPositive,
		BNegative,
		ABPositive,
		ABNegative,
		OPositive,
		ONegative,
		Unknown
	}

	[Serializable]
	public enum EyeColor
	{
		Amber,
		Blue,
		Brown,
		Grey,
		Green,
		Hazel,
		Red,
		Albino,
		Unknown
	}

	[Serializable]
	public enum HairColor
	{
		Auburn,
		Brown,
		Black,
		Blond,
		Grey,
		Red,
		White,
		Unknown
	}


	public struct MatchParams
	{
		public bool RusNames;

		public float NamesIndistinctThreshold;

		public bool DatesCheck;
		public int YearsInaccuracy;
	}

}