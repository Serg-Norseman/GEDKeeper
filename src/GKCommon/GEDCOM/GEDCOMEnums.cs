namespace GKCommon.GEDCOM
{
    public enum GEDCOMRecordType
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

    public enum GEDCOMFormat
    {
        gf_Unknown,
        gf_Native,
        gf_GENBOX,
        gf_ALTREE,
        gf_AGES,
        gf_PAF,
		
        gf_Last = gf_PAF
    }

    public enum GEDCOMCharacterSet
    {
        csASCII,
        csANSEL,
        csUNICODE,
        csUTF8
    }

    public enum GEDCOMState
    {
        osLoading,
        osReady
    }

    public enum GEDCOMRestriction
    {
        rnNone,
        rnLocked,
        rnConfidential,
        rnPrivacy,

        rnLast = rnPrivacy
    }

    public enum GEDCOMSex
    {
        svNone,
        svMale,
        svFemale,
        svUndetermined,

        svLast = svUndetermined
    }

    public enum GEDCOMNameType
    {
        ntNone,
        ntAka,
        ntBirth,
        ntImmigrant,
        ntMaiden,
        ntMarried
    }

    /*public enum GEDCOMNamePieceType
    {
        nptPrefix,
        nptGiven,
        nptNickname,
        nptSurnamePrefix,
        nptSurname,
        nptSuffix
    }*/

    public enum GEDCOMCalendar
    {
        dcGregorian,
        dcJulian,
        dcHebrew,
        dcFrench,
        dcRoman,
        dcUnknown,

        dcLast = dcUnknown
    }

    public enum GEDCOMDateFormat
    {
        dfGEDCOMStd,
        dfSystem
    }

    public enum GEDCOMApproximated
    {
        daExact,
        daAbout,
        daCalculated,
        daEstimated
    }

    public enum GEDCOMRange
    {
        drAfter,
        drBefore,
        drBetween,
        drAnd
    }

    public enum GEDCOMBaptismDateStatus
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

    public enum GEDCOMChildLinkageStatus
    {
        clNone,
        clChallenged,
        clDisproven,
        clProven
    }

    public enum GEDCOMChildSealingDateStatus
    {
        cdsNone,
        cdsBIC,
        cdsExcluded,
        cdsPre1970,
        cdsStillborn,
        cdsSubmitted,
        cdsUncleared
    }

    public enum GEDCOMEndowmentDateStatus
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

    public enum GEDCOMOrdinanceProcessFlag
    {
        opNone,
        opYes,
        opNo
    }

    public enum GEDCOMPedigreeLinkageType
    {
        plNone,
        plAdopted,
        plBirth,
        plFoster,
        plSealing
    }

    public enum GEDCOMSpouseSealingDateStatus
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

    public enum GEDCOMMediaType
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

    public enum GEDCOMMultimediaFormat
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

    public enum GKCommunicationDir
    {
        cdFrom,
        cdTo
    }

    public enum GKCommunicationType
    {
        ctCall,
        ctEMail,
        ctFax,
        ctLetter,
        ctTape,
        ctVisit,
		
        ctLast = ctVisit
    }

    public enum GKGoalType
    {
        gtIndividual,
        gtFamily,
        gtSource,
        gtOther
    }

    public enum GKResearchPriority
    {
        rpNone,
        rpLow,
        rpNormal,
        rpHigh,
        rpTop
    }

    public enum GKResearchStatus
    {
        rsDefined,
        rsInProgress,
        rsOnHold,
        rsProblems,
        rsCompleted,
        rsWithdrawn
    }
}
