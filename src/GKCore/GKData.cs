using System;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public static class GKData
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

		public struct PersonEventStruct
		{
			public LSID Name;
			public string Sign;
			public TPersonEventKind Kind;
			
			public PersonEventStruct(LSID aName, string aSign, TPersonEventKind aKind) {
				this.Name = aName;
				this.Sign = aSign;
				this.Kind = aKind;
			}
		}

		public struct DateKindStruct
		{
			public LSID Name;
			public TDateControlsRange Dates;
			
			public DateKindStruct(LSID aName, TDateControlsRange aDates) {
				this.Name = aName;
				this.Dates = aDates;
			}
		}

		public struct FamilyEventStruct
		{
			public LSID Name;
			public string Sign;
			
			public FamilyEventStruct(LSID aName, string aSign) {
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

		public struct TStatsTitleStruct
		{
			public LSID Title;
			public LSID Cap;

			public TStatsTitleStruct(LSID title, LSID cap) {
				this.Title = title;
				this.Cap = cap;
			}
		}


		public static string[] Restrictions;
		public static LSID[] RecordTypes;
		public static SexStruct[] SexData;
		public static MarStatusStruct[] MarriageStatus;
		public static PersonEventStruct[] PersonEvents;
		public static DateKindStruct[] DateKinds;
		public static LSID[] DateCalendars;
		public static FamilyEventStruct[] FamilyEvents;
		public static StoreTypeRec[] GKStoreTypes;
		public static LSID[] MediaTypes;
		public static LSID[] PriorityNames;
		public static LSID[] StatusNames;
		public static LSID[] CommunicationNames;
		public static LSID[] CommunicationDirs;
		public static LSID[] GoalNames;
		public static LSID[] CertaintyAssessments;
		public static string[] UserRefs;
		public static TGEDCOMAppFormat[] GEDCOMFormats;
		public static LSID[] RelationKinds;
		public static string[] RelationSigns;
		public static string[] Numerals;
		public static string[] NumKinship;
		public static TStatsTitleStruct[] StatsTitles;

		static GKData()
		{
			DataSetup();
		}

		public static void DataSetup()
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


			FamilyEvents = new FamilyEventStruct[10] {
				new FamilyEventStruct(LSID.LSID_Event, "EVEN"),
				new FamilyEventStruct(LSID.LSID_FEvt_1, "ENGA"),
				new FamilyEventStruct(LSID.LSID_FEvt_2, "MARR"),
				new FamilyEventStruct(LSID.LSID_FEvt_3, "MARB"),
				new FamilyEventStruct(LSID.LSID_FEvt_4, "MARC"),
				new FamilyEventStruct(LSID.LSID_FEvt_5, "MARL"),
				new FamilyEventStruct(LSID.LSID_FEvt_6, "MARS"),
				new FamilyEventStruct(LSID.LSID_FEvt_7, "ANUL"),
				new FamilyEventStruct(LSID.LSID_FEvt_8, "DIVF"),
				new FamilyEventStruct(LSID.LSID_FEvt_9, "DIV")
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

			
			DateKindStruct[] array5 = new DateKindStruct[10];
			array5[0] = new DateKindStruct(LSID.LSID_DK_0,  (TDateControlsRange)2);
			array5[1] = new DateKindStruct(LSID.LSID_DK_1,  (TDateControlsRange)4);
			array5[2] = new DateKindStruct(LSID.LSID_DK_2,  (TDateControlsRange)2);
			array5[3] = new DateKindStruct(LSID.LSID_DK_3,  (TDateControlsRange)6);
			array5[4] = new DateKindStruct(LSID.LSID_DK_4,  (TDateControlsRange)2);
			array5[5] = new DateKindStruct(LSID.LSID_DK_5,  (TDateControlsRange)4);
			array5[6] = new DateKindStruct(LSID.LSID_DK_6,  (TDateControlsRange)6);
			array5[7] = new DateKindStruct(LSID.LSID_DK_7,  (TDateControlsRange)2);
			array5[8] = new DateKindStruct(LSID.LSID_DK_8,  (TDateControlsRange)2);
			array5[9] = new DateKindStruct(LSID.LSID_DK_9,  (TDateControlsRange)2);
			DateKinds = array5;


			PersonEventStruct[] array6 = new PersonEventStruct[37];
			array6[ 0] = new PersonEventStruct(LSID.LSID_Event, "EVEN", TPersonEventKind.ekEvent);
			array6[ 1] = new PersonEventStruct(LSID.LSID_Birth, "BIRT", TPersonEventKind.ekEvent);
			array6[ 2] = new PersonEventStruct(LSID.LSID_Adoption, "ADOP", TPersonEventKind.ekEvent);
			array6[ 3] = new PersonEventStruct(LSID.LSID_Christening, "CHR", TPersonEventKind.ekEvent);
			array6[ 4] = new PersonEventStruct(LSID.LSID_Graduation, "GRAD", TPersonEventKind.ekEvent);
			array6[ 5] = new PersonEventStruct(LSID.LSID_Retirement, "RETI", TPersonEventKind.ekEvent);
			array6[ 6] = new PersonEventStruct(LSID.LSID_Naturalization, "NATU", TPersonEventKind.ekEvent);
			array6[ 7] = new PersonEventStruct(LSID.LSID_Emigration, "EMIG", TPersonEventKind.ekEvent);
			array6[ 8] = new PersonEventStruct(LSID.LSID_Immigration, "IMMI", TPersonEventKind.ekEvent);
			array6[ 9] = new PersonEventStruct(LSID.LSID_Census, "CENS", TPersonEventKind.ekEvent);
			array6[10] = new PersonEventStruct(LSID.LSID_LastWill, "WILL", TPersonEventKind.ekEvent);
			array6[11] = new PersonEventStruct(LSID.LSID_ProbateOfWill, "PROB", TPersonEventKind.ekEvent);
			array6[12] = new PersonEventStruct(LSID.LSID_Death, "DEAT", TPersonEventKind.ekEvent);
			array6[13] = new PersonEventStruct(LSID.LSID_Burial, "BURI", TPersonEventKind.ekEvent);
			array6[14] = new PersonEventStruct(LSID.LSID_Cremation, "CREM", TPersonEventKind.ekEvent);
			array6[15] = new PersonEventStruct(LSID.LSID_Fact, "FACT", TPersonEventKind.ekFact);
			array6[16] = new PersonEventStruct(LSID.LSID_Religion, "RELI", TPersonEventKind.ekFact);
			array6[17] = new PersonEventStruct(LSID.LSID_Nationality, "NATI", TPersonEventKind.ekFact);
			array6[18] = new PersonEventStruct(LSID.LSID_Residence, "RESI", TPersonEventKind.ekFact);
			array6[19] = new PersonEventStruct(LSID.LSID_PhysicalDesc, "DSCR", TPersonEventKind.ekFact);
			array6[20] = new PersonEventStruct(LSID.LSID_NationalIDNumber, "IDNO", TPersonEventKind.ekFact);
			array6[21] = new PersonEventStruct(LSID.LSID_SocialSecurityNumber, "SSN", TPersonEventKind.ekFact);
			array6[22] = new PersonEventStruct(LSID.LSID_ChildsCount, "NCHI", TPersonEventKind.ekFact);
			array6[23] = new PersonEventStruct(LSID.LSID_MarriagesCount, "NMR", TPersonEventKind.ekFact);
			array6[24] = new PersonEventStruct(LSID.LSID_Education, "EDUC", TPersonEventKind.ekFact);
			array6[25] = new PersonEventStruct(LSID.LSID_Occupation, "OCCU", TPersonEventKind.ekFact);
			array6[26] = new PersonEventStruct(LSID.LSID_Caste, "CAST", TPersonEventKind.ekFact);
			array6[27] = new PersonEventStruct(LSID.LSID_Property, "PROP", TPersonEventKind.ekFact);
			array6[28] = new PersonEventStruct(LSID.LSID_NobilityTitle, "TITL", TPersonEventKind.ekFact);
			array6[29] = new PersonEventStruct(LSID.LSID_Travel, "_TRAVEL", TPersonEventKind.ekFact);
			array6[30] = new PersonEventStruct(LSID.LSID_Hobby, "_HOBBY", TPersonEventKind.ekFact);
			array6[31] = new PersonEventStruct(LSID.LSID_Award, "_AWARD", TPersonEventKind.ekFact);
			array6[32] = new PersonEventStruct(LSID.LSID_Mili, "_MILI", TPersonEventKind.ekFact);
			array6[33] = new PersonEventStruct(LSID.LSID_MiliInd, "_MILI_IND", TPersonEventKind.ekFact);
			array6[34] = new PersonEventStruct(LSID.LSID_MiliDis, "_MILI_DIS", TPersonEventKind.ekFact);
			array6[35] = new PersonEventStruct(LSID.LSID_MiliRank, "_MILI_RANK", TPersonEventKind.ekFact);
			array6[36] = new PersonEventStruct(LSID.LSID_DNAMarkers, "_DNA", TPersonEventKind.ekFact);
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


			StatsTitles = new TStatsTitleStruct[34];
			StatsTitles[0] = new TStatsTitleStruct(LSID.LSID_AncestorsCount, LSID.LSID_Name);
			StatsTitles[1] = new TStatsTitleStruct(LSID.LSID_DescendantsCount, LSID.LSID_Name);
			StatsTitles[2] = new TStatsTitleStruct(LSID.LSID_GenerationsCount, LSID.LSID_Name);
			StatsTitles[3] = new TStatsTitleStruct(LSID.LSID_Surname, LSID.LSID_Surname);
			StatsTitles[4] = new TStatsTitleStruct(LSID.LSID_Name, LSID.LSID_Name);
			StatsTitles[5] = new TStatsTitleStruct(LSID.LSID_Patronymic, LSID.LSID_Patronymic);
			StatsTitles[6] = new TStatsTitleStruct(LSID.LSID_Age, LSID.LSID_Age);
			StatsTitles[7] = new TStatsTitleStruct(LSID.LSID_LifeExpectancy, LSID.LSID_Age);
			StatsTitles[8] = new TStatsTitleStruct(LSID.LSID_BirthYears, LSID.LSID_BirthYears);
			StatsTitles[9] = new TStatsTitleStruct(LSID.LSID_BirthYearsDec, LSID.LSID_BirthYears);
			StatsTitles[10] = new TStatsTitleStruct(LSID.LSID_DeathYears, LSID.LSID_DeathYears);
			StatsTitles[11] = new TStatsTitleStruct(LSID.LSID_DeathYearsDec, LSID.LSID_DeathYears);
			StatsTitles[12] = new TStatsTitleStruct(LSID.LSID_ChildsCount, LSID.LSID_Name);
			StatsTitles[13] = new TStatsTitleStruct(LSID.LSID_DistrChilds, LSID.LSID_ChildsCount);
			StatsTitles[14] = new TStatsTitleStruct(LSID.LSID_BirthPlace, LSID.LSID_BirthPlace);
			StatsTitles[15] = new TStatsTitleStruct(LSID.LSID_DeathPlace, LSID.LSID_DeathPlace);
			StatsTitles[16] = new TStatsTitleStruct(LSID.LSID_Residence, LSID.LSID_Residence);
			StatsTitles[17] = new TStatsTitleStruct(LSID.LSID_Occupation, LSID.LSID_Occupation);
			StatsTitles[18] = new TStatsTitleStruct(LSID.LSID_Religion, LSID.LSID_Religion);
			StatsTitles[19] = new TStatsTitleStruct(LSID.LSID_Nationality, LSID.LSID_Nationality);
			StatsTitles[20] = new TStatsTitleStruct(LSID.LSID_Education, LSID.LSID_Education);
			StatsTitles[21] = new TStatsTitleStruct(LSID.LSID_Caste, LSID.LSID_Caste);
			StatsTitles[22] = new TStatsTitleStruct(LSID.LSID_AgeFirstborn, LSID.LSID_Name);
			StatsTitles[23] = new TStatsTitleStruct(LSID.LSID_MarriagesCount, LSID.LSID_Name);
			StatsTitles[24] = new TStatsTitleStruct(LSID.LSID_MarriagesAge, LSID.LSID_Name);
			StatsTitles[25] = new TStatsTitleStruct(LSID.LSID_DiffSpouses, LSID.LSID_Family);
			StatsTitles[26] = new TStatsTitleStruct(LSID.LSID_Hobby, LSID.LSID_Hobby);
			StatsTitles[27] = new TStatsTitleStruct(LSID.LSID_Award, LSID.LSID_Award);
			StatsTitles[28] = new TStatsTitleStruct(LSID.LSID_Mili, LSID.LSID_Mili);
			StatsTitles[29] = new TStatsTitleStruct(LSID.LSID_MiliInd, LSID.LSID_MiliInd);
			StatsTitles[30] = new TStatsTitleStruct(LSID.LSID_MiliDis, LSID.LSID_MiliDis);
			StatsTitles[31] = new TStatsTitleStruct(LSID.LSID_MiliRank, LSID.LSID_MiliRank);

			StatsTitles[32] = new TStatsTitleStruct(LSID.LSID_AAF_1, LSID.LSID_AAF_1);
			StatsTitles[33] = new TStatsTitleStruct(LSID.LSID_AAF_2, LSID.LSID_AAF_2);
		}
	}
}
