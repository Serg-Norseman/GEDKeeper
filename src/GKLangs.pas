unit GKLangs;

interface

uses
  Windows, SysUtils;

type
  TLangID = Windows.LANGID;

  ILocalization = interface
    ['{F7295BF7-EE01-4DD7-8C44-3BDFEAA9E318}']
    procedure SetLang();
  end;

  LSID = Cardinal;

const
  // Language String IDs
  LSID_None                     =   0;

  LSID_First                    =   1;

  LSID_MIFile                   =   1;
  LSID_MIEdit                   =   2;
  LSID_MIPedigree               =   3;
  LSID_MIService                =   4;
  LSID_MIWindow                 =   5;
  LSID_MIHelp                   =   6;

  LSID_MIFileNew                =   7;
  LSID_MIFileLoad               =   8;
  LSID_MIMRUFiles               =   9;
  LSID_MIFileSave               =  10;
  LSID_MIFileClose              =  11;
  LSID_MIFileProperties         =  12;
  LSID_MIExport                 =  13;
  LSID_MIExportToWeb            =  14;
  LSID_MIExportToExcelApp       =  15;
  LSID_MIExportToExcelFile      =  16;
  LSID_MIExit                   =  17;

  LSID_MIUndo                   =  18;
  LSID_MIRedo                   =  19;
  LSID_MIRecordAdd              =  20;
  LSID_MIRecordEdit             =  21;
  LSID_MIRecordDelete           =  22;
  LSID_MIStreamInput            =  23;

  LSID_MITreeAncestors          =  24;
  LSID_MITreeDescendants        =  25;
  LSID_MITreeBoth               =  26;
  LSID_MIPedigree_dAboville     =  27;
  LSID_MIPedigree_Konovalov     =  28;
  LSID_MIMap                    =  29;
  LSID_MIStats                  =  30;

  LSID_MICalc                   =  31;
  LSID_MINamesBook              =  32;
  LSID_MICalendar               =  33;
  LSID_MITimeLine               =  34;
  LSID_MIOrganizer              =  35;
  LSID_MIScripts                =  36;
  LSID_MIDBImport               =  37;
  LSID_MITreeTools              =  38;
  LSID_MIFilter                 =  39;
  LSID_MIOptions                =  40;

  LSID_MIWinCascade             =  41;
  LSID_MIWinHTile               =  42;
  LSID_MIWinVTile               =  43;
  LSID_MIWinMinimize            =  44;
  LSID_MIWinArrange             =  45;

  LSID_MIGenResources           =  46;
  LSID_MIKinshipTerms           =  47;
  LSID_MIFAQ                    =  48;
  LSID_MIContext                =  49;
  LSID_MIAbout                  =  50;

  LSID_SBRecords                =  51;
  LSID_SBFiltered               =  52;

  LSID_RPIndividuals            =  53;
  LSID_RPFamilies               =  54;
  LSID_RPNotes                  =  55;
  LSID_RPMultimedia             =  56;
  LSID_RPSources                =  57;
  LSID_RPRepositories           =  58;
  LSID_RPGroups                 =  59;
  LSID_RPResearches             =  60;
  LSID_RPTasks                  =  61;
  LSID_RPCommunications         =  62;
  LSID_RPLocations              =  63;

  LSID_UnkFemale                =  64;
  LSID_UnkMale                  =  65;

  LSID_SexN                     =  66;
  LSID_SexM                     =  67;
  LSID_SexF                     =  68;
  LSID_SexU                     =  69;

  LSID_FileSaveQuery            =  70;
  LSID_ParentsQuery             =  71;

  LSID_PersonDeleteQuery        =  72;
  LSID_FamilyDeleteQuery        =  73;
  LSID_NoteDeleteQuery          =  74;
  LSID_SourceDeleteQuery        =  75;
  LSID_MediaDeleteQuery         =  76;
  LSID_RepositoryDeleteQuery    =  77;
  LSID_GroupDeleteQuery         =  78;
  LSID_ResearchDeleteQuery      =  79;
  LSID_TaskDeleteQuery          =  80;
  LSID_CommunicationDeleteQuery =  81;
  LSID_LocationDeleteQuery      =  82;

  LSID_Address                  =  83;
  LSID_Events                   =  84;
  LSID_Surname                  =  85;
  LSID_Name                     =  86;
  LSID_Patronymic               =  87;
  LSID_Sex                      =  88;
  LSID_Nickname                 =  89;
  LSID_SurnamePrefix            =  90;
  LSID_NamePrefix               =  91;
  LSID_NameSuffix               =  92;
  LSID_Patriarch                =  93;
  LSID_Bookmark                 =  94;

  LSID_Association              =  95;
  LSID_Relation                 =  96;
  LSID_Person                   =  97;

  LSID_DlgAccept                =  98;
  LSID_DlgCancel                =  99;
  LSID_DlgClose                 = 100;
  LSID_DlgSelect                = 101;
  LSID_DlgAppend                = 102;

  LSID_WinPersonNew             = 103;
  LSID_WinPersonEdit            = 104;
  LSID_WinCheckSex              = 105;
  LSID_WinRecordSelect          = 106;
  LSID_WinSourceCitEdit         = 107;
  LSID_WinUserRefEdit           = 108;

  LSID_Note                     = 109;
  LSID_Source                   = 110;
  LSID_Page                     = 111;
  LSID_Certainty                = 112;
  LSID_Reference                = 113;
  LSID_Type                     = 114;

  LSID_Family                   = 115;
  LSID_Husband                  = 116;
  LSID_Wife                     = 117;
  LSID_Status                   = 118;
  LSID_Childs                   = 119;

  LSID_DetachHusbandQuery       = 120;
  LSID_DetachWifeQuery          = 121;
  LSID_DetachChildQuery         = 122;

  LSID_BirthDate                = 123;
  LSID_DeathDate                = 124;
  LSID_Restriction              = 125;

  LSID_Title                    = 126;
  LSID_Members                  = 127;

  LSID_WinGroupEdit             = 128;
  LSID_DetachMemberQuery        = 129;

  LSID_TimeScale                = 130;
  LSID_CurrentYear              = 131;

  LSID_Telephone                = 132;
  LSID_Mail                     = 133;
  LSID_WebSite                  = 134;

  LSID_Repository               = 135;
  LSID_Progress                 = 136;

  LSID_TimePassed               = 137;
  LSID_TimeRemain               = 138;
  LSID_TimeTotal                = 139;

  LSID_Date                     = 140;
  LSID_Text                     = 141;
  LSID_ShortTitle               = 142;
  LSID_Author                   = 143;
  LSID_Publication              = 144;
  LSID_Common                   = 145;

  LSID_DetachRepositoryQuery    = 146;

  LSID_StoreType                = 147;
  LSID_File                     = 148;
  LSID_View                     = 149;

  LSID_AdvancedWarning          = 150;

  LSID_Father                   = 151;
  LSID_Mother                   = 152;
  LSID_Parents                  = 153;
  LSID_Spouses                  = 154;
  LSID_Associations             = 155;
  LSID_UserRefs                 = 156;

  LSID_Cal_Gregorian            = 157;
  LSID_Cal_Julian               = 158;
  LSID_Cal_Hebrew               = 159;
  LSID_Cal_Islamic              = 160;
  LSID_Cal_Persian              = 161;
  LSID_Cal_Indian               = 162;
  LSID_Cal_Bahai                = 163;
  LSID_Cal_French               = 164;
  LSID_Cal_Roman                = 165;

  LSID_Unknown                  = 166;

  LSID_CopyResultToClipboard    = 167;

  LSID_Advanced                 = 168;
  LSID_AdvancedSupport          = 169;
  LSID_ExtName                  = 170;

  LSID_Location                 = 171;
  LSID_Latitude                 = 172;
  LSID_Longitude                = 173;

  LSID_Show                     = 174;
  LSID_SearchCoords             = 175;
  LSID_Search                   = 176;
  LSID_SelectCoords             = 177;
  LSID_SelectName               = 178;

  LSID_Priority                 = 179;
  LSID_Percent                  = 180;
  LSID_StartDate                = 181;
  LSID_StopDate                 = 182;

  LSID_Goal                     = 183;
  LSID_Theme                    = 184;
  LSID_Corresponder             = 185;
  LSID_Group                    = 186;

  LSID_DetachTaskQuery          = 187;
  LSID_DetachCommunicationQuery = 188;
  LSID_DetachGroupQuery         = 189;

  LSID_WinResearchEdit          = 190;
  LSID_WinTaskEdit              = 191;
  LSID_WinCommunicationEdit     = 192;

  LSID_Error                    = 193;
  LSID_LuaStartFailed           = 194;
  LSID_DateFormatInvalid        = 195;

  LSID_AdCountry                = 196;
  LSID_AdState                  = 197;
  LSID_AdCity                   = 198;
  LSID_AdPostalCode             = 199;

  LSID_Telephones               = 200;
  LSID_EMails                   = 201;
  LSID_WebSites                 = 202;

  LSID_Value                    = 203;
  LSID_Event                    = 204;
  LSID_Place                    = 205;
  LSID_Cause                    = 206;
  LSID_Agency                   = 207;

  LSID_PatFemale                = 208;
  LSID_PatMale                  = 209;

  LSID_NotSelectedPerson        = 210;
  LSID_IsNotDefinedSex          = 211;
  LSID_IsNotFamilies            = 212;

  LSID_AncestorsNumberIsInvalid = 213;
  LSID_DescendantsNumberIsInvalid = 214;

  LSID_GenerationsVisible       = 215;
  LSID_Unlimited                = 216;
  LSID_Spouse                   = 217;
  LSID_MarriageDate             = 218;

  LSID_DetachFatherQuery        = 219;
  LSID_DetachMotherQuery        = 220;
  LSID_DetachSpouseQuery        = 221;
  LSID_DetachParentsQuery       = 222;

  LSID_TM_Both                  = 223;
  LSID_TM_Ancestors             = 224;
  LSID_TM_Descendants           = 225;
  LSID_TM_TraceRoot             = 226;

  LSID_DoEdit                   = 227;
  LSID_FamilyAdd                = 228;
  LSID_SpouseAdd                = 229;
  LSID_SonAdd                   = 230;
  LSID_DaughterAdd              = 231;
  LSID_DoDelete                 = 232;
  LSID_RebuildTree              = 233;
  LSID_RebuildKinships          = 234;

  LSID_Links                    = 235;
  LSID_PlaceAndAttribute        = 236;
  LSID_LMarriage                = 237;
  LSID_LFamily                  = 238;
  LSID_Namesakes                = 239;

  LSID_RemoveEventQuery         = 240;
  LSID_DetachNoteQuery          = 241;
  LSID_DetachMultimediaQuery    = 242;
  LSID_DetachSourceQuery        = 243;
  LSID_RemoveAssociationQuery   = 244;
  LSID_RemoveUserRefQuery       = 245;

  LSID_LoadGedComFailed         = 246;
  LSID_CheckGedComFailed        = 247;

  LSID_BirthDays                = 248;
  LSID_DaysRemained             = 249;

  LSID_Interface                = 250;
  LSID_Trees                    = 251;
  LSID_Pedigrees                = 252;

  LSID_SaveCoding               = 253;
  LSID_WorkMode                 = 254;
  LSID_Simple                   = 255;
  LSID_Expert                   = 256;
  LSID_Internet                 = 257;
  LSID_ProxyUse                 = 258;
  LSID_ProxyServer              = 259;
  LSID_ProxyPort                = 260;
  LSID_ProxyLogin               = 261;
  LSID_ProxyPassword            = 262;
  LSID_Tips                     = 263;
  LSID_StartupTips              = 264;
  LSID_Language                 = 265;

  LSID_ListsAll                 = 266;
  LSID_ListPersons              = 267;
  LSID_NamesFormat              = 268;
  LSID_NF1                      = 269;
  LSID_NF2                      = 270;
  LSID_NF3                      = 271;
  LSID_DateFormat               = 272;
  LSID_PlacesWithAddress        = 273;
  LSID_HighlightUnparented      = 274;
  LSID_HighlightUnmarried       = 275;
  LSID_DefList                  = 276;

  LSID_ViewTree                 = 277;

  LSID_DiffLines                = 278;
  LSID_OnlyYears                = 279;
  LSID_Kinship                  = 280;
  LSID_SignsVisible             = 281;
  LSID_TreeDecorative           = 282;
  LSID_PortraitsVisible         = 283;
  LSID_ChildlessExclude         = 284;

  LSID_Decor                    = 285;
  LSID_Man                      = 286;
  LSID_Woman                    = 287;
  LSID_UnkSex                   = 288;
  LSID_UnHusband                = 289;
  LSID_UnWife                   = 290;
  LSID_Font                     = 291;

  LSID_PedigreeGen              = 292;
  LSID_IncludeAttributes        = 293;
  LSID_IncludeNotes             = 294;
  LSID_IncludeSources           = 295;
  LSID_PedigreeFormat           = 296;
  LSID_PF1                      = 297;
  LSID_PF2                      = 298;

  LSID_RecordGoto               = 299;
  LSID_RecordMoveUp             = 300;
  LSID_RecordMoveDown           = 301;

  LSID_FullName                 = 302;

  LSID_BirthPlace               = 303;
  LSID_DeathPlace               = 304;
  LSID_Residence                = 305;
  LSID_Age                      = 306;
  LSID_LifeExpectancy           = 307;
  LSID_DaysForBirth             = 308;
  LSID_Religion                 = 309;
  LSID_Nationality              = 310;
  LSID_Education                = 311;
  LSID_Occupation               = 312;
  LSID_Caste                    = 313;
  LSID_Mili                     = 314;
  LSID_MiliInd                  = 315;
  LSID_MiliDis                  = 316;
  LSID_MiliRank                 = 317;
  LSID_Changed                  = 318;

  LSID_MarrRegistered           = 319;
  LSID_MarrNotRegistered        = 320;
  LSID_MarrDivorced             = 321;

  LSID_Birth                    = 322;
  LSID_Adoption                 = 323;
  LSID_Christening              = 324;
  LSID_Graduation               = 325;
  LSID_Retirement               = 326;
  LSID_Naturalization           = 327;
  LSID_Emigration               = 328;
  LSID_Immigration              = 329;
  LSID_Census                   = 330;
  LSID_LastWill                 = 331;
  LSID_ProbateOfWill            = 332;
  LSID_Death                    = 333;
  LSID_Burial                   = 334;
  LSID_Cremation                = 335;

  LSID_Fact                     = 336;
  LSID_PhysicalDesc             = 337;
  LSID_NationalIDNumber         = 338;
  LSID_SocialSecurityNumber     = 339;
  LSID_ChildsCount              = 340;
  LSID_MarriagesCount           = 341;
  LSID_Property                 = 342;
  LSID_NobilityTitle            = 343;
  LSID_Travel                   = 344;
  LSID_Hobby                    = 345;
  LSID_Award                    = 346;

  LSID_RK_Unk                   = 347;
  LSID_RK_Father                = 348;
  LSID_RK_Mother                = 349;
  LSID_RK_Husband               = 350;
  LSID_RK_Wife                  = 351;
  LSID_RK_Son                   = 352;
  LSID_RK_Daughter              = 353;
  LSID_RK_Grandfather           = 354;
  LSID_RK_Grandmother           = 355;
  LSID_RK_Grandson              = 356;
  LSID_RK_Granddaughter         = 357;
  LSID_RK_Brother               = 358;
  LSID_RK_Sister                = 359;
  LSID_RK_SonInLaw              = 360;
  LSID_RK_DaughterInLaw         = 361;
  LSID_RK_HusbandFather         = 362;
  LSID_RK_HusbandMother         = 363;
  LSID_RK_WifeFather            = 364;
  LSID_RK_WifeMother            = 365;
  LSID_RK_Uncle                 = 366;
  LSID_RK_Aunt                  = 367;
  LSID_RK_Nephew                = 368;
  LSID_RK_Niece                 = 369;
  LSID_RK_CousinM               = 370;
  LSID_RK_CousinF               = 371;
  LSID_RK_01                    = 372;
  LSID_RK_02                    = 373;
  LSID_RK_03                    = 374;
  LSID_RK_04                    = 375;
  LSID_RK_05                    = 376;
  LSID_RK_06                    = 377;
  LSID_RK_07                    = 378;
  LSID_RK_08                    = 379;
  LSID_RK_09                    = 380;

  LSID_TooMuchWidth             = 381;
  LSID_Backward                 = 382;
  LSID_Forward                  = 383;
  LSID_Prev                     = 384;
  LSID_Next                     = 385;
  LSID_YouKnowWhat              = 386;

  LSID_LoadingLocations         = 387;
  LSID_NotSelected              = 388;

  LSID_MapSelection             = 389;
  LSID_MapSelOnAll              = 390;
  LSID_MSBirthPlaces            = 391;
  LSID_MSDeathPlaces            = 392;
  LSID_MSResiPlace              = 393;
  LSID_MapSelOnSelected         = 394;
  LSID_SaveImage                = 395;

  LSID_FormatUnsupported        = 396;
  LSID_DataLoadError            = 397;
  LSID_ParseError_LineSeq       = 398;
  LSID_PersonParsed             = 399;
  LSID_Generation               = 400;
  LSID_ParseError_AncNotFound   = 401;
  LSID_ParseError_DateInvalid   = 402;

  LSID_DK_0                     = 403;
  LSID_DK_1                     = 404;
  LSID_DK_2                     = 405;
  LSID_DK_3                     = 406;
  LSID_DK_4                     = 407;
  LSID_DK_5                     = 408;
  LSID_DK_6                     = 409;
  LSID_DK_7                     = 410;
  LSID_DK_8                     = 411;
  LSID_DK_9                     = 412;

  LSID_FEvt_1                   = 413;
  LSID_FEvt_2                   = 414;
  LSID_FEvt_3                   = 415;
  LSID_FEvt_4                   = 416;
  LSID_FEvt_5                   = 417;
  LSID_FEvt_6                   = 418;
  LSID_FEvt_7                   = 419;
  LSID_FEvt_8                   = 420;
  LSID_FEvt_9                   = 421;

  LSID_STRef                    = 422;
  LSID_STArc                    = 423;
  LSID_STStg                    = 424;

  LSID_MT_01                    = 425;
  LSID_MT_02                    = 426;
  LSID_MT_03                    = 427;
  LSID_MT_04                    = 428;
  LSID_MT_05                    = 429;
  LSID_MT_06                    = 430;
  LSID_MT_07                    = 431;
  LSID_MT_08                    = 432;
  LSID_MT_09                    = 433;
  LSID_MT_10                    = 434;
  LSID_MT_11                    = 435;
  LSID_MT_12                    = 436;
  LSID_MT_13                    = 437;
  LSID_MT_14                    = 438;
  LSID_MT_15                    = 439;

  LSID_Prt_1                    = 440;
  LSID_Prt_2                    = 441;
  LSID_Prt_3                    = 442;
  LSID_Prt_4                    = 443;
  LSID_Prt_5                    = 444;

  LSID_RStat_1                  = 445;
  LSID_RStat_2                  = 446;
  LSID_RStat_3                  = 447;
  LSID_RStat_4                  = 448;
  LSID_RStat_5                  = 449;
  LSID_RStat_6                  = 450;

  LSID_Com_1                    = 451;
  LSID_Com_2                    = 452;
  LSID_Com_3                    = 453;
  LSID_Com_4                    = 454;
  LSID_Com_5                    = 455;
  LSID_Com_6                    = 456;

  LSID_CD_1                     = 457;
  LSID_CD_2                     = 458;

  LSID_G_1                      = 459;
  LSID_G_2                      = 460;
  LSID_G_3                      = 461;
  LSID_G_4                      = 462;

  LSID_Cert_1                   = 463;
  LSID_Cert_2                   = 464;
  LSID_Cert_3                   = 465;
  LSID_Cert_4                   = 466;

  LSID_Research                 = 467;
  LSID_Task                     = 468;
  LSID_Communication            = 469;

  LSID_IDsCorrect               = 470;
  LSID_FormatCheck              = 471;
  LSID_IDsCorrectNeed           = 472;
  LSID_MainBaseSize             = 473;
  LSID_SyncFin                  = 474;
  LSID_PatSearch                = 475;
  LSID_LinksSearch              = 476;
  LSID_ArcNotFound              = 477;

  LSID_GenDB                    = 478;
  LSID_GenIndex                 = 479;

  LSID_SurnamesIndex            = 480;
  LSID_NamesIndex               = 481;
  LSID_BirthIndex               = 482;
  LSID_DeathIndex               = 483;
  LSID_CommonStats              = 484;
  LSID_ExpPedigree              = 485;

  LSID_InputSimple              = 486;
  LSID_InputSource              = 487;
  LSID_SourceKind               = 488;
  LSID_SK_Rev                   = 489;
  LSID_SK_Met                   = 490;
  LSID_Year                     = 491;
  LSID_Settlement               = 492;
  LSID_EventDate                = 493;
  LSID_EventType                = 494;
  LSID_Join                     = 495;
  LSID_Comment                  = 496;

  LSID_BranchCut                = 497;
  LSID_Not                      = 498;
  LSID_BCut_Years               = 499;
  LSID_BCut_Persons             = 500;

  LSID_SrcAll                   = 501;
  LSID_SrcNot                   = 502;
  LSID_SrcAny                   = 503;

  LSID_PLPerson                 = 504;
  LSID_PLGodparent              = 505;
  LSID_Child                    = 506;

  LSID_NameInvalid              = 507;
  LSID_BasePersonInvalid        = 508;
  LSID_SourceYearInvalid        = 509;
  LSID_ValueInvalid             = 510;

  LSID_Operation                = 511;
  LSID_ToolOp_1                 = 512;
  LSID_ToolOp_2                 = 513;
  LSID_ToolOp_3                 = 514;
  LSID_ToolOp_4                 = 515;
  LSID_ToolOp_5                 = 516;
  LSID_ToolOp_6                 = 517;
  LSID_ToolOp_7                 = 518;
  LSID_ToolOp_8                 = 519;
  LSID_ToolOp_9                 = 520;

  LSID_SearchMatches            = 521;
  LSID_CheckFamiliesConnection  = 522;

  LSID_All                      = 523;
  LSID_OnlyAlive                = 524;
  LSID_OnlyDied                 = 525;
  LSID_AliveBefore              = 526;
  LSID_OnlyMans                 = 527;
  LSID_OnlyWomans               = 528;

  LSID_NameMask                 = 529;
  LSID_PlaceMask                = 530;
  LSID_EventMask                = 531;
  LSID_OnlyPatriarchs           = 532;

  LSID_DateInvalid              = 533;

  LSID_People                   = 534;
  LSID_Years                    = 535;
  LSID_Decennial                = 536;
  LSID_HowBirthes               = 537;
  LSID_HowDeads                 = 538;

  LSID_Living                   = 539;
  LSID_Deads                    = 540;
  LSID_AvgAge                   = 541;
  LSID_AvgLife                  = 542;
  LSID_AvgChilds                = 543;
  LSID_AvgBorn                  = 544;
  LSID_AvgMarriagesCount        = 545;
  LSID_AvgMarriagesAge          = 546;

  LSID_AncestorsCount           = 547;
  LSID_DescendantsCount         = 548;
  LSID_GenerationsCount         = 549;
  LSID_BirthYears               = 550;
  LSID_BirthYearsDec            = 551;
  LSID_DeathYears               = 552;
  LSID_DeathYearsDec            = 553;
  LSID_DistrChilds              = 554;
  LSID_AgeFirstborn             = 555;
  LSID_MarriagesAge             = 556;
  LSID_DiffSpouses              = 557;

  LSID_SelAll                   = 558;
  LSID_SelFamily                = 559;
  LSID_SelAncestors             = 560;
  LSID_SelDescendants           = 561;

  LSID_RecMerge                 = 562;
  LSID_RM_Search                = 563;
  LSID_RM_Skip                  = 564;
  LSID_RM_Records               = 565;
  LSID_RM_SearchPersons         = 566;
  LSID_RM_DirectMatching        = 567;
  LSID_RM_IndistinctMatching    = 568;
  LSID_RM_OnlyNP                = 569;
  LSID_RM_BirthYear             = 570;
  LSID_RM_NameAccuracy          = 571;
  LSID_RM_YearInaccuracy        = 572;

  LSID_Repair                   = 573;
  LSID_MinGenerations           = 574;
  LSID_SetPatFlag               = 575;
  LSID_InsertIntoBook           = 576;

  LSID_SimilarSurnames          = 577;
  LSID_SimilarNames             = 578;

  LSID_RecsDeleted              = 579;
  LSID_Record                   = 580;
  LSID_Problem                  = 581;
  LSID_Solve                    = 582;
  LSID_PersonLonglived          = 583;
  LSID_PersonSexless            = 584;
  LSID_LiveYearsInvalid         = 585;
  LSID_StrangeSpouse            = 586;
  LSID_StrangeParent            = 587;
  LSID_Descendants              = 588;
  LSID_Generations              = 589;
  LSID_LinksCount               = 590;
  LSID_PlacesPrepare            = 591;
  LSID_PlaceAlreadyInBook       = 592;

  LSID_Last                     = 592;

const
  LSDefName = 'Русский';
  LSDefCode = 1049;
  LSDefList: array [LSID_First..LSID_Last] of string = (
    {   1 } 'Файл',
    {   2 } 'Правка',
    {   3 } 'Родословная',
    {   4 } 'Сервис',
    {   5 } '&Окна',
    {   6 } 'Справка',

    {   7 } 'Новый',
    {   8 } 'Открыть...',
    {   9 } 'Открыть последний',
    {  10 } 'Сохранить...',
    {  11 } 'Закрыть',
    {  12 } 'Свойства файла...',
    {  13 } 'Экспорт',
    {  14 } 'Экспорт в Web...',
    {  15 } 'Экспорт в Excel...',
    {  16 } 'Экспорт в Excel-файл...',
    {  17 } 'Выход',

    {  18 } 'Отменить',
    {  19 } 'Вернуть',
    {  20 } 'Добавить запись',
    {  21 } 'Изменить запись',
    {  22 } 'Удалить запись',
    {  23 } 'Поточный ввод',

    {  24 } 'Древо предков',
    {  25 } 'Древо потомков',
    {  26 } 'Древо полное',
    {  27 } 'Роспись по дАбовиллю',
    {  28 } 'Роспись по Коновалову',
    {  29 } 'Карты',
    {  30 } 'Статистика',

    {  31 } 'Калькулятор',
    {  32 } 'Справочник имен',
    {  33 } 'Календарь',
    {  34 } 'Линия времени',
    {  35 } 'Органайзер',
    {  36 } 'Скрипты...',
    {  37 } 'Импорт баз данных...',
    {  38 } 'Инструменты...',
    {  39 } 'Фильтр',
    {  40 } 'Настройки',

    {  41 } '&Каскад',
    {  42 } '&Горизонтальная мозаика',
    {  43 } '&Вертикальная мозаика',
    {  44 } '&Свернуть все',
    {  45 } '&Разместить все',

    {  46 } 'Ресурсы в Интернете...',
    {  47 } 'Терминология родства...',
    {  48 } 'Часто задаваемые вопросы...',
    {  49 } 'Содержание',
    {  50 } 'О программе',

    {  51 } 'Записей',
    {  52 } 'фильтрованных',

    {  53 } 'Персоны',
    {  54 } 'Семьи',
    {  55 } 'Заметки',
    {  56 } 'Мультимедиа',
    {  57 } 'Источники',
    {  58 } 'Архивы',
    {  59 } 'Группы',
    {  60 } 'Исследования',
    {  61 } 'Задачи',
    {  62 } 'Коммуникации',
    {  63 } 'Места',

    {  64 } 'неизвестная',
    {  65 } 'неизвестный',

    {  66 } '?',
    {  67 } 'Мужской',
    {  68 } 'Женский',
    {  69 } 'Неопределенный',

    {  70 } 'Файл изменен. Сохранить?',
    {  71 } 'У заданного родителя найдена семья "%s". \nВключить в неё ребенка?',

    {  72 } 'Удалить персональную запись "%s"?',
    {  73 } 'Удалить семью "%s"?',
    {  74 } 'Удалить заметку?',
    {  75 } 'Удалить источник "%s"?',
    {  76 } 'Удалить мультимедиа "%s"?',
    {  77 } 'Удалить архив "%s"?',
    {  78 } 'Удалить группу "%s"?',
    {  79 } 'Удалить исследование "%s"?',
    {  80 } 'Удалить задачу "%s"?',
    {  81 } 'Удалить коммуникацию "%s"?',
    {  82 } 'Удалить место "%s"?',

    {  83 } 'Адрес',
    {  84 } 'Факты',
    {  85 } 'Фамилия',
    {  86 } 'Имя',
    {  87 } 'Отчество',
    {  88 } 'Пол',
    {  89 } 'Прозвище',
    {  90 } 'Префикс фамилии',
    {  91 } 'Префикс имени',
    {  92 } 'Суффикс имени',
    {  93 } 'Патриарх',
    {  94 } 'Закладка',
    {  95 } 'Ассоциация',
    {  96 } 'Отношение',
    {  97 } 'Персона',

    {  98 } 'Принять',
    {  99 } 'Отменить',
    { 100 } 'Закрыть',
    { 101 } 'Выбрать',
    { 102 } 'Добавить',

    { 103 } 'Новая персональная запись',
    { 104 } 'Редактирование персональной записи',
    { 105 } 'Проверка пола',
    { 106 } 'Выбор записи',
    { 107 } 'Цитата источника',
    { 108 } 'Пользовательская сноска',

    { 109 } 'Заметка',
    { 110 } 'Источник',
    { 111 } 'Лист/Страница',
    { 112 } 'Достоверность',
    { 113 } 'Сноска/ссылка',
    { 114 } 'Тип',
    { 115 } 'Семья',
    { 116 } 'Муж',
    { 117 } 'Жена',
    { 118 } 'Статус',
    { 119 } 'Дети',

    { 120 } 'Удалить ссылку на мужа?',
    { 121 } 'Удалить ссылку на жену?',
    { 122 } 'Удалить ссылку на ребенка?',

    { 123 } 'Дата рождения',
    { 124 } 'Дата смерти',
    { 125 } 'Ограничение безопасности',

    { 126 } 'Название',
    { 127 } 'Участники',

    { 128 } 'Редактирование группы',
    { 129 } 'Удалить ссылку на участника группы?',

    { 130 } 'Шкала времени',
    { 131 } 'Текущий год',

    { 132 } 'Телефон',
    { 133 } 'Эл. почта',
    { 134 } 'Сайт',

    { 135 } 'Архив',
    { 136 } 'Прогресс',
    { 137 } 'Времени прошло',
    { 138 } 'Времени осталось',
    { 139 } 'Времени всего',

    { 140 } 'Дата',
    { 141 } 'Текст',
    { 142 } 'Краткое название',
    { 143 } 'Автор',
    { 144 } 'Опубликовано',
    { 145 } 'Общее',

    { 146 } 'Удалить ссылку на архив?',

    { 147 } 'Способ хранения',
    { 148 } 'Файл', {need delete}
    { 149 } 'Просмотр',

    { 150 } 'Для выбранного типа хранения не включен режим расширения',

    { 151 } 'Отец',
    { 152 } 'Мать',
    { 153 } 'Родители',
    { 154 } 'Супруги',
    { 155 } 'Ассоциации',
    { 156 } 'Сноски/Пометки',

    { 157 } 'Григорианский',
    { 158 } 'Юлианский',
    { 159 } 'Еврейский',
    { 160 } 'Исламский (Хиджры)',
    { 161 } 'Иранский',
    { 162 } 'Индийский',
    { 163 } 'Бахаи',
    { 164 } 'Французский',
    { 165 } 'Римский',

    { 166 } 'Неизвестно',

    { 167 } 'Поместить результат в буфер обмена',

    { 168 } 'Расширение проекта',
    { 169 } 'Поддержка расширения (архив, хранилище файлов)',
    { 170 } 'Название архива и папки хранилища',

    { 171 } 'Местоположение',
    { 172 } 'Широта',
    { 173 } 'Долгота',

    { 174 } 'Показать',
    { 175 } 'Поиск координат',
    { 176 } 'Поиск',
    { 177 } 'Выбрать коорд.',
    { 178 } 'Выбрать название',

    { 179 } 'Приоритет',
    { 180 } 'Процент',
    { 181 } 'Запущено',
    { 182 } 'Завершено',

    { 183 } 'Цель',
    { 184 } 'Тема',
    { 185 } 'Корреспондент',
    { 186 } 'Группа',

    { 187 } 'Удалить ссылку на задачу?',
    { 188 } 'Удалить ссылку на корреспонденцию?',
    { 189 } 'Удалить ссылку на группу?',

    { 190 } 'Редактирование исследования',
    { 191 } 'Редактирование задачи',
    { 192 } 'Редактирование коммуникации',

    { 193 } 'Ошибка',
    { 194 } 'Ошибка запуска Lua!',
    { 195 } 'Некорректный формат даты',

    { 196 } 'Страна',
    { 197 } 'Штат/Область',
    { 198 } 'Город',
    { 199 } 'Почтовый код',

    { 200 } 'Телефоны',
    { 201 } 'Эл. почта',
    { 202 } 'Веб-страницы',

    { 203 } 'Значение',
    { 204 } 'Событие',
    { 205 } 'Место',
    { 206 } 'Причина',
    { 207 } 'Засвидетельствовавший орган',

    { 208 } 'Женское',
    { 209 } 'Мужское',

    { 210 } 'Не выбрана персональная запись',
    { 211 } 'У данной персоны не задан пол.',
    { 212 } 'У данной персоны нет семей.',

    { 213 } 'Расчетное количество предков %s больше допустимых пределов.',
    { 214 } 'Расчетное количество потомков %s больше допустимых пределов.',

    { 215 } 'Показывать поколений:',
    { 216 } 'неограниченно',
    { 217 } 'Cупруг(а)',
    { 218 } 'Дата брака',

    { 219 } 'Удалить ссылку на отца?',
    { 220 } 'Удалить ссылку на мать?',
    { 221 } 'Удалить ссылку на супруга?',
    { 222 } 'Удалить персону из семьи родителей?',

    { 223 } 'Всё',
    { 224 } 'Только предки',
    { 225 } 'Только потомки',
    { 226 } 'Автосмена центра',

    { 227 } 'Редактировать',
    { 228 } 'Добавить семью',
    { 229 } 'Добавить супруга(у)',
    { 230 } 'Добавить сына',
    { 231 } 'Добавить дочь',
    { 232 } 'Удалить',
    { 233 } 'Перестроить древо',
    { 234 } 'Перестроить отношения',

    { 235 } 'Ссылки',
    { 236 } 'Место/Атрибут',
    { 237 } 'брак',
    { 238 } 'семья',
    { 239 } 'Тёзки',

    { 240 } 'Удалить факт?',
    { 241 } 'Удалить ссылку на заметку?',
    { 242 } 'Удалить ссылку на мультимедиа?',
    { 243 } 'Удалить ссылку на источник?',
    { 244 } 'Удалить ассоциацию?',
    { 245 } 'Удалить пользовательскую сноску?',

    { 246 } 'Ошибка загрузки файла',
    { 247 } 'Ошибка проверки формата',

    { 248 } 'Дни рождения',
    { 249 } 'До дня рождения "%s" осталось %s дня(-ей)',

    { 250 } 'Интерфейс',
    { 251 } 'Родословные древа',
    { 252 } 'Росписи',

    { 253 } 'Кодировка сохранения файлов',
    { 254 } 'Режим работы',
    { 255 } 'Простой',
    { 256 } 'Продвинутый',
    { 257 } 'Загрузка из Интернета',
    { 258 } 'Использовать прокси-сервер',
    { 259 } 'Сервер',
    { 260 } 'Порт',
    { 261 } 'Логин',
    { 262 } 'Пароль',
    { 263 } 'Подсказки',
    { 264 } 'Показывать при старте',
    { 265 } 'Язык',

    { 266 } 'Все списки',
    { 267 } 'Список персон',
    { 268 } 'Формат имен в списках',
    { 269 } 'Фамилия_Имя_Отчество',
    { 270 } 'Фамилия; Имя_Отчество',
    { 271 } 'Фамилия; Имя; Отчество',
    { 272 } 'Формат даты в списках',
    { 273 } 'Включать адрес в строки мест',
    { 274 } 'Подсвечивать персоны без родителей',
    { 275 } 'Подсвечивать персоны без семьи',
    { 276 } 'Значения по умолчанию',

    { 277 } 'Отображение персон в древе',
    { 278 } 'Разные строки (имя и отчество)',
    { 279 } 'Только годы',
    { 280 } 'Степень родства',
    { 281 } 'Дополнительные символы',
    { 282 } 'Декоративное оформление',
    { 283 } 'Отображать портреты',
    { 284 } 'Исключить умерших в детстве',

    { 285 } 'Оформление',
    { 286 } 'Мужчина',
    { 287 } 'Женщина',
    { 288 } 'Неизвестный пол',
    { 289 } 'Разведенный супруг',
    { 290 } 'Разведенная супруга',
    { 291 } 'Шрифт',

    { 292 } 'Генерация росписей',
    { 293 } 'Включая атрибуты персон',
    { 294 } 'Включая заметки',
    { 295 } 'Включая источники',
    { 296 } 'Формат',
    { 297 } 'Избыточный',
    { 298 } 'Традиционный',

    { 299 } 'Перейти на запись',
    { 300 } 'Поместить выше',
    { 301 } 'Поместить ниже',

    { 302 } 'Полное имя',
    { 303 } 'Место рождения',
    { 304 } 'Место смерти',
    { 305 } 'Местожительство',
    { 306 } 'Возраст',
    { 307 } 'Продолжительность жизни',
    { 308 } 'Дней до ДР',
    { 309 } 'Вероисповедание',
    { 310 } 'Национальность',
    { 311 } 'Образование',
    { 312 } 'Профессия',
    { 313 } 'Социальное положение',
    { 314 } 'Военная служба',
    { 315 } 'Призван в ВС',
    { 316 } 'Уволен из ВС',
    { 317 } 'Звание в ВС',
    { 318 } 'Изменено',

    { 319 } 'Брак зарегистрирован',
    { 320 } 'Брак не зарегистрирован',
    { 321 } 'Разведены',

    { 322 } 'Рождение',
    { 323 } 'Усыновление',
    { 324 } 'Крещение',
    { 325 } 'Получение ученой степени',
    { 326 } 'Уход на пенсию',
    { 327 } 'Натурализация',
    { 328 } 'Эмиграция',
    { 329 } 'Иммиграция',
    { 330 } 'Перепись',
    { 331 } 'Завещание',
    { 332 } 'Утверждение завещания',
    { 333 } 'Смерть',
    { 334 } 'Похороны',
    { 335 } 'Кремация',

    { 336 } 'Факт',
    { 337 } 'Физическое описание',
    { 338 } 'Идентификационный номер',
    { 339 } 'Код социального страхования',
    { 340 } 'Количество детей',
    { 341 } 'Количество браков',
    { 342 } 'Собственность',
    { 343 } 'Титул',
    { 344 } 'Путешествие',
    { 345 } 'Хобби',
    { 346 } 'Награда',

    { 347 } '?',
    { 348 } 'отец',
    { 349 } 'мать',
    { 350 } 'муж',
    { 351 } 'жена',
    { 352 } 'сын',
    { 353 } 'дочь',
    { 354 } 'дед',
    { 355 } 'бабушка',
    { 356 } 'внук',
    { 357 } 'внучка',
    { 358 } 'брат',
    { 359 } 'сестра',
    { 360 } 'зять',
    { 361 } 'невестка',
    { 362 } 'свекор',
    { 363 } 'свекровь',
    { 364 } 'тесть',
    { 365 } 'теща',
    { 366 } 'дядя',
    { 367 } 'тетя',
    { 368 } 'племянник',
    { 369 } 'племянница',
    { 370 } 'кузен',
    { 371 } 'кузина',

    { 372 } '<reserved>',
    { 373 } '<reserved>',
    { 374 } '<reserved>',
    { 375 } '<reserved>',
    { 376 } '<reserved>',
    { 377 } '<reserved>',
    { 378 } '<reserved>',
    { 379 } '<reserved>',
    { 380 } '<reserved>',

    { 381 } 'Ширина изображения более 65 тыс. точек. Сохранить невозможно',
    { 382 } 'Назад',
    { 383 } 'Вперед',
    { 384 } '<reserved>',
    { 385 } 'Далее',
    { 386 } 'Вы знаете что...',

    { 387 } 'Загрузка и поиск мест',
    { 388 } '( не выбран )',

    { 389 } 'Выборка',
    { 390 } 'По всем людям',
    { 391 } 'Места рождения',
    { 392 } 'Места смерти',
    { 393 } 'Места проживания',
    { 394 } 'Только по выбранному',
    { 395 } 'Сохранить снимок...',

    { 396 } 'Формат не поддерживается',
    { 397 } 'Ошибка загрузки данных.',
    { 398 } 'Ошибка разбора: номера записей содержат пропуск.',
    { 399 } 'Распознана персональная запись',
    { 400 } 'Поколение',
    { 401 } 'Ошибка разбора: в списке не обнаружен предок с номером',
    { 402 } 'Ошибка разбора: дата',

    { 403 } 'Точно',
    { 404 } 'Ранее',
    { 405 } 'Позднее',
    { 406 } 'Между',
    { 407 } 'Период до',
    { 408 } 'Период после',
    { 409 } 'Период между',
    { 410 } 'Около',
    { 411 } 'По расчету',
    { 412 } 'По оценке',

    { 413 } 'Помолвка',
    { 414 } 'Бракосочетание',
    { 415 } 'Публичное объявление о бракосочетании',
    { 416 } 'Заключение брачного контракта',
    { 417 } 'Получение разрешения на брак',
    { 418 } 'Заключение брачного соглашения',
    { 419 } 'Аннулирование брака',
    { 420 } 'Подача заявления о разводе',
    { 421 } 'Развод',

    { 422 } 'Ссылка на файл',
    { 423 } 'Размещение в архиве',
    { 424 } 'Размещение в хранилище',

    { 425 } '-',
    { 426 } 'Звукозапись',
    { 427 } 'Книга',
    { 428 } 'Карточка',
    { 429 } 'Электронный',
    { 430 } 'Микрофиша',
    { 431 } 'Фильм',
    { 432 } 'Журнал',
    { 433 } 'Рукопись',
    { 434 } 'Карта',
    { 435 } 'Газета',
    { 436 } 'Фотография',
    { 437 } 'Надгробие',
    { 438 } 'Видео',
    { 439 } '- Другой -',

    { 440 } 'Не задан',
    { 441 } 'Низкий',
    { 442 } 'Нормальный',
    { 443 } 'Высокий',
    { 444 } 'Срочный',

    { 445 } 'Определено',
    { 446 } 'Выполняется',
    { 447 } 'Задержано',
    { 448 } 'Осложнения',
    { 449 } 'Завершено',
    { 450 } 'Отозвано',

    { 451 } 'Звонок',
    { 452 } 'Эл.письмо',
    { 453 } 'Факс',
    { 454 } 'Письмо',
    { 455 } 'Кассета',
    { 456 } 'Визит',

    { 457 } 'от',
    { 458 } 'к',

    { 459 } 'персона',
    { 460 } 'семья',
    { 461 } 'источник',
    { 462 } 'иная',

    { 463 } 'Ненадежное подтверждение или предполагаемые данные',
    { 464 } 'Сомнительная надежность подтверждения',
    { 465 } 'Косвенные доказательства',
    { 466 } 'Прямые и первичные доказательства',

    { 467 } 'Исследование',
    { 468 } 'Задача',
    { 469 } 'Корреспонденция',

    { 470 } 'Коррекция идентификаторов',
    { 471 } 'Проверка формата',
    { 472 } 'Требуется коррекция идентификаторов записей, продолжить?',
    { 473 } 'Количество объектов в основной базе: %s',
    { 474 } 'Синхронизация завершена.',
    { 475 } 'Поиск патриархов',
    { 476 } 'Поиск взаимосвязей',
    { 477 } 'Архив не найден, данные не загружены',

    { 478 } 'Генеалогическая база данных',
    { 479 } 'Индекс',
    { 480 } 'Индекс фамилий',
    { 481 } 'Индекс имен',
    { 482 } 'Индекс годов рождения',
    { 483 } 'Индекс годов смерти',
    { 484 } 'Общая статистика',
    { 485 } 'Родословная роспись',

    { 486 } 'Простой ввод',
    { 487 } 'Источник (метрики/ревизии)',
    { 488 } 'Тип источника',
    { 489 } 'Ревизская сказка',
    { 490 } 'Метрическая книга',
    { 491 } 'Год',
    { 492 } 'Населенный пункт',
    { 493 } 'Дата события',
    { 494 } 'Тип события',
    { 495 } 'Связь',
    { 496 } 'Примечание',

    { 497 } 'Отсечение ветвей',
    { 498 } 'нет',
    { 499 } 'по границе лет',
    { 500 } 'по заданным лицам',

    { 501 } '- всё -',
    { 502 } '- нет -',
    { 503 } '- любые -',

    { 504 } 'Лицо',
    { 505 } 'Крестный',
    { 506 } 'Ребенок',

    { 507 } 'Количество компонентов имени меньше трех.',
    { 508 } 'Базовая персона ("Лицо") не определена первой',
    { 509 } 'Год источника задан неверно',
    { 510 } 'Значение неверно',

    { 511 } 'Операция',
    { 512 } 'Сравнить базы данных',
    { 513 } 'Объединить базы данных',
    { 514 } 'Разделить базу данных',
    { 515 } 'Объединить дубликаты записей',
    { 516 } 'Импорт росписей из внешних форматов',
    { 517 } 'Проверка связности семей',
    { 518 } 'Проверка базы данных',
    { 519 } 'Поиск патриархов',
    { 520 } 'Управление местами',

    { 521 } 'Поиск совпадений...',
    { 522 } 'Проверка связности семей',

    { 523 } 'все',
    { 524 } 'только живые',
    { 525 } 'только умершие',
    { 526 } 'В живых до',

    { 527 } 'только мужчины',
    { 528 } 'только женщины',

    { 529 } 'Маска имени',
    { 530 } 'Маска местожительства',
    { 531 } 'Маска фактов',
    { 532 } 'Только главы семей',

    { 533 } 'Дата неверна',

    { 534 } 'Люди',
    { 535 } 'Годы',
    { 536 } 'Десятилетия',
    { 537 } 'Родилось',
    { 538 } 'Умерло',

    { 539 } 'Живущие',
    { 540 } 'Умершие',
    { 541 } 'Средний возраст',
    { 542 } 'Средняя продолжительность жизни',
    { 543 } 'Среднее число детей',
    { 544 } 'Средний возраст рождения первенца',
    { 545 } 'Среднее количество браков',
    { 546 } 'Средний возраст заключения брака',

    { 547 } 'Количество предков',
    { 548 } 'Количество потомков',
    { 549 } 'Количество поколений потомков',
    { 550 } 'Годы рождения',
    { 551 } 'Годы рождения (десятилетиями)',
    { 552 } 'Годы смерти',
    { 553 } 'Годы смерти (десятилетиями)',
    { 554 } 'Распределение количества детей',
    { 555 } 'Возраст рождения первенца',
    { 556 } 'Возраст вступления в брак',
    { 557 } 'Разница возрастов супругов',

    { 558 } 'Выбрать все связи',
    { 559 } 'Выбрать семью',
    { 560 } 'Выбрать предков',
    { 561 } 'Выбрать потомков',

    { 562 } 'Объединение',
    { 563 } 'Автопоиск',
    { 564 } 'Пропустить',
    { 565 } 'Записи',
    { 566 } 'Поиск персон',
    { 567 } 'Прямое сравнение',
    { 568 } 'Нечеткое сравнение',
    { 569 } 'Только по имени/отчеству (только женщины)',
    { 570 } 'Учитывать год рождения',
    { 571 } 'Точность имени, %',
    { 572 } 'Погрешность лет',

    { 573 } 'Исправить',
    { 574 } 'Поколений потомков не менее',
    { 575 } 'Установить признак',
    { 576 } 'Внести в справочник',

    { 577 } 'Схожие фамилии:',
    { 578 } 'Схожие имена:',

    { 579 } 'Выбранные персональные записи удалены',
    { 580 } 'Запись',
    { 581 } 'Проблема',
    { 582 } 'Решение',
    { 583 } 'Возможно умерший (возраст %s)',
    { 584 } 'Не задан пол',
    { 585 } 'Год рождения больше года смерти',
    { 586 } 'Первый брак в возрасте %s лет?',
    { 587 } 'Первый ребенок родился в возрасте %s лет?',
    { 588 } 'Потомков',
    { 589 } 'Поколений',
    { 590 } 'Количество ссылок',
    { 591 } 'Обработка мест',
    { 592 } 'Место уже есть в справочнике'
  );

var
  LSList: array [LSID_First..LSID_Last] of string;

implementation

uses
  GKUtils;

procedure SaveSample();
var
  lf: TextFile;
  i: Integer;
begin
  AssignFile(lf, GetAppPath() + 'langs\russian.sample'); Rewrite(lf);
  Writeln(lf, ';'+IntToStr(LSDefCode)+','+AnsiToUTF8(LSDefName));
  for i := LSID_First to LSID_Last do Writeln(lf, AnsiToUTF8(LSDefList[i]));
  CloseFile(lf);
end;

initialization
  //SaveSample();

end.
