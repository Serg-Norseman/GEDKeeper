using System;

namespace GedCom551
{
	public enum TGEDCOMRecordType : byte
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

	public enum TGEDCOMState : byte
	{
		osLoading,
		osReady
	}

	public enum TGEDCOMRestriction : byte
	{
		rnNone,
		rnConfidential,
		rnLocked,
		rnPrivacy
	}

	public enum TGEDCOMCharacterSet : byte
	{
		csASCII,
		csANSEL,
		csUNICODE,
		csUTF8
	}

	public enum TGEDCOMNameType : byte
	{
		ntNone,
		ntAka,
		ntBirth,
		ntImmigrant,
		ntMaiden,
		ntMarried
	}

	public enum TGEDCOMNamePieceType : byte
	{
		nptPrefix,
		nptGiven,
		nptNickname,
		nptSurnamePrefix,
		nptSurname,
		nptSuffix
	}

	public enum TGEDCOMSex : byte
	{
		svNone,
		svMale,
		svFemale,
		svUndetermined
	}

	public enum TGEDCOMSubList : byte
	{
		stNotes,
		stSource,
		stMultimedia
	}

	public enum TGEDCOMPedigreeLinkageType : byte
	{
		plNone,
		plAdopted,
		plBirth,
		plFoster,
		plSealing
	}

	public enum TGEDCOMChildLinkageStatus : byte
	{
		clNone,
		clChallenged,
		clDisproven,
		clProven
	}

	public enum TGEDCOMChildSealingDateStatus : byte
	{
		cdsNone,
		cdsBIC,
		cdsExcluded,
		cdsPre1970,
		cdsStillborn,
		cdsSubmitted,
		cdsUncleared
	}

	public enum TGEDCOMBaptismDateStatus : byte
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

	public enum TGEDCOMEndowmentDateStatus : byte
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

	public enum TGEDCOMCalendar : byte
	{
		dcGregorian,
		dcJulian,
		dcHebrew,
		dcFrench,
		dcRoman,
		dcUnknown
	}

	public enum TGEDCOMDateFormat : byte
	{
		dfGEDCOMStd,
		dfSystem
	}

	public enum TGEDCOMApproximated : byte
	{
		daExact,
		daAbout,
		daCalculated,
		daEstimated
	}

	public enum TGEDCOMRange : byte
	{
		drAfter,
		drBefore,
		drBetween,
		drAnd
	}

	public enum TGEDCOMSpouseSealingDateStatus : byte
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

	public enum TGEDCOMOrdinanceProcessFlag : byte {
		opNone,
		opYes,
		opNo
	}

	public enum TGEDCOMMultimediaFormat : byte
	{
		mfNone,
		mfBMP,
		mfGIF,
		mfJPG,
		mfOLE,
		mfPCX,
		mfTIF,
		mfWAV,
		mfTXT,
		mfRTF,
		mfAVI,
		mfTGA,
		mfPNG,
		mfMPG,
		mfHTM,
		mfUnknown
	}

	public enum TGEDCOMMediaType : byte
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
		mtUnknown
	}

	public enum TCommunicationType : byte
	{
		ctCall,
		ctEMail,
		ctFax,
		ctLetter,
		ctTape,
		ctVisit
	}

	public enum TCommunicationDir : byte
	{
		cdFrom,
		cdTo
	}

	public enum TGoalType : byte
	{
		gtIndividual,
		gtFamily,
		gtSource,
		gtOther
	}

	public enum TResearchPriority : byte
	{
		rpNone,
		rpLow,
		rpNormal,
		rpHigh,
		rpTop
	}

	public enum TResearchStatus : byte
	{
		rsDefined,
		rsInProgress,
		rsOnHold,
		rsProblems,
		rsCompleted,
		rsWithdrawn
	}
}