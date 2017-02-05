/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Drawing;
using GKCore.Types;

namespace GKCore
{
    public struct PluginInfo
    {
        public string Title;
        public string Description;
        public string Copyright;
        public string Version;
    }

    /// <summary>
    /// 
    /// </summary>
    public static class GKData
    {
        public const string APP_TITLE = "GEDKeeper2";
        public const string APP_MAIL = "gedkeeper@yandex.ru";

        public const int APP_FORMAT_DEFVER = 39; // don't change it!
        public const int APP_FORMAT_CURVER = 39;

        public const string GEDCOM_EXT = "ged";
        public const string GEDCOM_SEC_EXT = "geds";
        public const string LUA_EXT = "lua";

        public const int NoteNameMaxLength = 64;

        public const string GAPI_KEY = "AIzaSyCebJC5BpniJtRaJCSEl3tXdFy3KhbV5hk";

        #if __MonoCS__
        public static readonly Color HighlightUnparentedColor = Color.FromArgb(unchecked((int)0xFFFFCACA));
        public static readonly Color HighlightUnmarriedColor = Color.FromArgb(unchecked((int)0xFFFFFFA1));
        #else
        public static readonly Color HighlightUnparentedColor = Color.FromArgb(0xFFCACA);
        public static readonly Color HighlightUnmarriedColor = Color.FromArgb(0xFFFFA1);
        #endif

        public struct SexStruct
        {
            public LSID NameId;
            public string Sign;

            public SexStruct(LSID name, string sign) {
                this.NameId = name;
                this.Sign = sign;
            }
        }

        public struct MarStatusStruct
        {
            public LSID Name;
            public string StatSign;

            public MarStatusStruct(LSID name, string sign) {
                this.Name = name;
                this.StatSign = sign;
            }
        }

        public struct PersonEventStruct
        {
            public LSID Name;
            public string Sign;
            public PersonEventKind Kind;

            public PersonEventStruct(LSID name, string sign, PersonEventKind kind) {
                this.Name = name;
                this.Sign = sign;
                this.Kind = kind;
            }
        }

        public struct DateKindStruct
        {
            public LSID Name;
            public byte Dates;

            public DateKindStruct(LSID name, byte dates) {
                this.Name = name;
                this.Dates = dates;
            }
        }

        public struct FamilyEventStruct
        {
            public LSID Name;
            public string Sign;

            public FamilyEventStruct(LSID name, string sign) {
                this.Name = name;
                this.Sign = sign;
            }
        }

        public struct CalendarStruct
        {
            public LSID Name;
            public string Sign;
            public bool HasSupport;

            public CalendarStruct(LSID name, string sign, bool hasSupport) {
                this.Name = name;
                this.Sign = sign;
                this.HasSupport = hasSupport;
            }
        }

        public struct StoreTypeRec
        {
            public LSID Name;
            public string Sign;

            public StoreTypeRec(LSID name, string sign) {
                this.Name = name;
                this.Sign = sign;
            }
        }

        public struct StatsTitleStruct
        {
            public LSID Title;
            public LSID Cap;

            public StatsTitleStruct(LSID title, LSID cap) {
                this.Title = title;
                this.Cap = cap;
            }
        }


        public static readonly LSID[] Restrictions;
        public static readonly LSID[] RecordTypes;
        public static readonly SexStruct[] SexData;
        public static readonly MarStatusStruct[] MarriageStatus;
        public static readonly PersonEventStruct[] PersonEvents;
        public static readonly DateKindStruct[] DateKinds;
        public static readonly CalendarStruct[] DateCalendars;
        public static readonly FamilyEventStruct[] FamilyEvents;
        public static readonly StoreTypeRec[] GKStoreTypes;
        public static readonly LSID[] MediaTypes;
        public static readonly LSID[] PriorityNames;
        public static readonly LSID[] StatusNames;
        public static readonly LSID[] CommunicationNames;
        public static readonly LSID[] CommunicationDirs;
        public static readonly LSID[] GoalNames;
        public static readonly LSID[] CertaintyAssessments;
        public static readonly string[] SpecialUserRefs;
        public static readonly LSID[] RelationKinds;
        public static readonly string[] RelationSigns;
        public static readonly string[] Numerals;
        public static readonly string[] NumKinship;
        public static readonly LSID GreatPrefix;
        public static readonly StatsTitleStruct[] StatsTitles;
        public static readonly LSID[] CheckSolveNames;
        public static readonly LSID[] NameTypes;

        public static string[] CondSigns;
        public static string BloodGroups;

        static GKData()
        {
            GreatPrefix = LSID.LSID_RK_GreatPrefix;

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
                LSID.LSID_RK_BrotherInLaw_H,
                LSID.LSID_RK_SisterInLaw_H,
                LSID.LSID_RK_BrotherInLaw_W,
                LSID.LSID_RK_SisterInLaw_W,
                LSID.LSID_None,
                LSID.LSID_RK_Unk
            };


            SpecialUserRefs = new string[] {
                "",
                "РИ:Георгиевский кавалер",
                "СССР:ВОВ:Участник боевых действий",
                "СССР:ВОВ:Погиб в бою",
                "СССР:ВОВ:Труженик тыла",
                "СССР:Репрессирован",
                "Вероисповедание:Ислам",
                "Вероисповедание:Католицизм",
                "Вероисповедание:Православие",
                "Вероисповедание:Старообрядчество"
            };

            // /* 723 */ LSID_RI_GeorgeKnight,
            // /* 724 */ LSID_USSR_WWII_Combatant,
            // /* 725 */ LSID_USSR_WWII_KilledInBattle,
            // /* 726 */ LSID_USSR_WWII_HomeFrontWorker,
            // /* 727 */ LSID_USSR_Repressed,
            // /* 728 */ LSID_Religion_Islam,
            // /* 729 */ LSID_Religion_Catholicism,
            // /* 730 */ LSID_Religion_Orthodoxy,
            // /* 731 */ LSID_Religion_TheOldBelievers,


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
                LSID.LSID_DefaultValue, // TODO: may be LSID.LSID_MT_15 (Unknown)?
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
                LSID.LSID_MT_14
                //LSID.LSID_MT_15 <Unknown removed to first position>
            };


            GKStoreTypes = new StoreTypeRec[] {
                new StoreTypeRec(LSID.LSID_STRef, ""),
                new StoreTypeRec(LSID.LSID_STStg, "stg:"),
                new StoreTypeRec(LSID.LSID_STArc, "arc:")
            };


            FamilyEvents = new FamilyEventStruct[] {
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


            // FIXME: use calendars for StrToGEDCOMDate() and class EventEditDlg
            DateCalendars = new CalendarStruct[]
            {
                new CalendarStruct(LSID.LSID_Cal_Gregorian, " [G]", true),
                new CalendarStruct(LSID.LSID_Cal_Julian, " [J]", true),
                new CalendarStruct(LSID.LSID_Cal_Hebrew, " [H]", false), // !
                new CalendarStruct(LSID.LSID_Cal_French, " [FR]", false),
                new CalendarStruct(LSID.LSID_Cal_Roman, " [R]", false),
                new CalendarStruct(LSID.LSID_Cal_Islamic, " [I]", false), // !
                new CalendarStruct(LSID.LSID_Unknown, "", false)
            };


            DateKindStruct[] array5 = new DateKindStruct[10];
            array5[0] = new DateKindStruct(LSID.LSID_DK_0, 1); // 1
            array5[1] = new DateKindStruct(LSID.LSID_DK_1, 2); // 2
            array5[2] = new DateKindStruct(LSID.LSID_DK_2, 1); // 1
            array5[3] = new DateKindStruct(LSID.LSID_DK_3, 3); // 1,2
            array5[4] = new DateKindStruct(LSID.LSID_DK_4, 1); // 1
            array5[5] = new DateKindStruct(LSID.LSID_DK_5, 2); // 2
            array5[6] = new DateKindStruct(LSID.LSID_DK_6, 3); // 1,2
            array5[7] = new DateKindStruct(LSID.LSID_DK_7, 1); // 1
            array5[8] = new DateKindStruct(LSID.LSID_DK_8, 1); // 1
            array5[9] = new DateKindStruct(LSID.LSID_DK_9, 1); // 1
            DateKinds = array5;


            PersonEventStruct[] array6 = new PersonEventStruct[41];
            array6[ 0] = new PersonEventStruct(LSID.LSID_Event, "EVEN", PersonEventKind.ekEvent);
            array6[ 1] = new PersonEventStruct(LSID.LSID_Birth, "BIRT", PersonEventKind.ekEvent);
            array6[ 2] = new PersonEventStruct(LSID.LSID_Adoption, "ADOP", PersonEventKind.ekEvent);
            array6[ 3] = new PersonEventStruct(LSID.LSID_Christening, "CHR", PersonEventKind.ekEvent);
            array6[ 4] = new PersonEventStruct(LSID.LSID_Graduation, "GRAD", PersonEventKind.ekEvent);
            array6[ 5] = new PersonEventStruct(LSID.LSID_Retirement, "RETI", PersonEventKind.ekEvent);
            array6[ 6] = new PersonEventStruct(LSID.LSID_Naturalization, "NATU", PersonEventKind.ekEvent);
            array6[ 7] = new PersonEventStruct(LSID.LSID_Emigration, "EMIG", PersonEventKind.ekEvent);
            array6[ 8] = new PersonEventStruct(LSID.LSID_Immigration, "IMMI", PersonEventKind.ekEvent);
            array6[ 9] = new PersonEventStruct(LSID.LSID_Census, "CENS", PersonEventKind.ekEvent);
            array6[10] = new PersonEventStruct(LSID.LSID_LastWill, "WILL", PersonEventKind.ekEvent);
            array6[11] = new PersonEventStruct(LSID.LSID_ProbateOfWill, "PROB", PersonEventKind.ekEvent);
            array6[12] = new PersonEventStruct(LSID.LSID_Death, "DEAT", PersonEventKind.ekEvent);
            array6[13] = new PersonEventStruct(LSID.LSID_Burial, "BURI", PersonEventKind.ekEvent);
            array6[14] = new PersonEventStruct(LSID.LSID_Cremation, "CREM", PersonEventKind.ekEvent);

            array6[15] = new PersonEventStruct(LSID.LSID_Fact, "FACT", PersonEventKind.ekFact);
            array6[16] = new PersonEventStruct(LSID.LSID_Religion, "RELI", PersonEventKind.ekFact);
            array6[17] = new PersonEventStruct(LSID.LSID_Nationality, "NATI", PersonEventKind.ekFact);
            array6[18] = new PersonEventStruct(LSID.LSID_Residence, "RESI", PersonEventKind.ekFact);
            array6[19] = new PersonEventStruct(LSID.LSID_PhysicalDesc, "DSCR", PersonEventKind.ekFact);
            array6[20] = new PersonEventStruct(LSID.LSID_NationalIDNumber, "IDNO", PersonEventKind.ekFact);
            array6[21] = new PersonEventStruct(LSID.LSID_SocialSecurityNumber, "SSN", PersonEventKind.ekFact);
            array6[22] = new PersonEventStruct(LSID.LSID_ChildsCount, "NCHI", PersonEventKind.ekFact);
            array6[23] = new PersonEventStruct(LSID.LSID_MarriagesCount, "NMR", PersonEventKind.ekFact);
            array6[24] = new PersonEventStruct(LSID.LSID_Education, "EDUC", PersonEventKind.ekFact);
            array6[25] = new PersonEventStruct(LSID.LSID_Occupation, "OCCU", PersonEventKind.ekFact);
            array6[26] = new PersonEventStruct(LSID.LSID_Caste, "CAST", PersonEventKind.ekFact);
            array6[27] = new PersonEventStruct(LSID.LSID_Property, "PROP", PersonEventKind.ekFact);
            array6[28] = new PersonEventStruct(LSID.LSID_NobilityTitle, "TITL", PersonEventKind.ekFact);
            array6[29] = new PersonEventStruct(LSID.LSID_Travel, "_TRAVEL", PersonEventKind.ekFact);
            array6[30] = new PersonEventStruct(LSID.LSID_Hobby, "_HOBBY", PersonEventKind.ekFact);
            array6[31] = new PersonEventStruct(LSID.LSID_Award, "_AWARD", PersonEventKind.ekFact);
            array6[32] = new PersonEventStruct(LSID.LSID_Mili, "_MILI", PersonEventKind.ekFact);
            array6[33] = new PersonEventStruct(LSID.LSID_MiliInd, "_MILI_IND", PersonEventKind.ekFact);
            array6[34] = new PersonEventStruct(LSID.LSID_MiliDis, "_MILI_DIS", PersonEventKind.ekFact);
            array6[35] = new PersonEventStruct(LSID.LSID_MiliRank, "_MILI_RANK", PersonEventKind.ekFact);

            array6[36] = new PersonEventStruct(LSID.LSID_BloodGroup, "_BGRO", PersonEventKind.ekFact); // {MyFamilyTree}
            array6[37] = new PersonEventStruct(LSID.LSID_HairColor, "_HAIR", PersonEventKind.ekFact); // {BKW6, PAF}
            array6[38] = new PersonEventStruct(LSID.LSID_EyesColor, "_EYES", PersonEventKind.ekFact); // {Gen}

            array6[39] = new PersonEventStruct(LSID.LSID_MDNAHaplogroup, "_MDNA", PersonEventKind.ekFact); // {MyFamilyTree}
            array6[40] = new PersonEventStruct(LSID.LSID_YDNAHaplogroup, "_YDNA", PersonEventKind.ekFact); // {MyFamilyTree}
            PersonEvents = array6;


            MarriageStatus = new MarStatusStruct[] {
                new MarStatusStruct(LSID.LSID_Unknown, ""),
                new MarStatusStruct(LSID.LSID_MarrRegistered, "MARRIED"),
                new MarStatusStruct(LSID.LSID_MarrNotRegistered, "MARRNOTREG"),
                new MarStatusStruct(LSID.LSID_MarrDivorced, "NOTMARR")
            };


            SexData = new SexStruct[] {
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
                LSID.LSID_Submission,
                LSID.LSID_Submitter
            };


            Restrictions = new LSID[]
            {
                LSID.LSID_RestrictNone,
                LSID.LSID_RestrictLocked,
                LSID.LSID_RestrictConfidential,
                LSID.LSID_RestrictPrivacy
            };


            StatsTitles = new StatsTitleStruct[37];
            StatsTitles[0] = new StatsTitleStruct(LSID.LSID_AncestorsCount, LSID.LSID_Name);
            StatsTitles[1] = new StatsTitleStruct(LSID.LSID_DescendantsCount, LSID.LSID_Name);
            StatsTitles[2] = new StatsTitleStruct(LSID.LSID_GenerationsCount, LSID.LSID_Name);
            StatsTitles[3] = new StatsTitleStruct(LSID.LSID_Surname, LSID.LSID_Surname);
            StatsTitles[4] = new StatsTitleStruct(LSID.LSID_Name, LSID.LSID_Name);
            StatsTitles[5] = new StatsTitleStruct(LSID.LSID_Patronymic, LSID.LSID_Patronymic);
            StatsTitles[6] = new StatsTitleStruct(LSID.LSID_Age, LSID.LSID_Age);
            StatsTitles[7] = new StatsTitleStruct(LSID.LSID_LifeExpectancy, LSID.LSID_Age);
            StatsTitles[8] = new StatsTitleStruct(LSID.LSID_BirthYears, LSID.LSID_BirthYears);
            StatsTitles[9] = new StatsTitleStruct(LSID.LSID_BirthYearsDec, LSID.LSID_BirthYears);
            StatsTitles[10] = new StatsTitleStruct(LSID.LSID_DeathYears, LSID.LSID_DeathYears);
            StatsTitles[11] = new StatsTitleStruct(LSID.LSID_DeathYearsDec, LSID.LSID_DeathYears);
            StatsTitles[12] = new StatsTitleStruct(LSID.LSID_ChildsCount, LSID.LSID_Name);
            StatsTitles[13] = new StatsTitleStruct(LSID.LSID_DistrChilds, LSID.LSID_ChildsCount);
            StatsTitles[14] = new StatsTitleStruct(LSID.LSID_BirthPlace, LSID.LSID_BirthPlace);
            StatsTitles[15] = new StatsTitleStruct(LSID.LSID_DeathPlace, LSID.LSID_DeathPlace);
            StatsTitles[16] = new StatsTitleStruct(LSID.LSID_Residence, LSID.LSID_Residence);
            StatsTitles[17] = new StatsTitleStruct(LSID.LSID_Occupation, LSID.LSID_Occupation);
            StatsTitles[18] = new StatsTitleStruct(LSID.LSID_Religion, LSID.LSID_Religion);
            StatsTitles[19] = new StatsTitleStruct(LSID.LSID_Nationality, LSID.LSID_Nationality);
            StatsTitles[20] = new StatsTitleStruct(LSID.LSID_Education, LSID.LSID_Education);
            StatsTitles[21] = new StatsTitleStruct(LSID.LSID_Caste, LSID.LSID_Caste);
            StatsTitles[22] = new StatsTitleStruct(LSID.LSID_AgeFirstborn, LSID.LSID_Name);
            StatsTitles[23] = new StatsTitleStruct(LSID.LSID_MarriagesCount, LSID.LSID_Name);
            StatsTitles[24] = new StatsTitleStruct(LSID.LSID_MarriagesAge, LSID.LSID_Name);
            StatsTitles[25] = new StatsTitleStruct(LSID.LSID_DiffSpouses, LSID.LSID_Family);
            StatsTitles[26] = new StatsTitleStruct(LSID.LSID_Hobby, LSID.LSID_Hobby);
            StatsTitles[27] = new StatsTitleStruct(LSID.LSID_Award, LSID.LSID_Award);
            StatsTitles[28] = new StatsTitleStruct(LSID.LSID_Mili, LSID.LSID_Mili);
            StatsTitles[29] = new StatsTitleStruct(LSID.LSID_MiliInd, LSID.LSID_MiliInd);
            StatsTitles[30] = new StatsTitleStruct(LSID.LSID_MiliDis, LSID.LSID_MiliDis);
            StatsTitles[31] = new StatsTitleStruct(LSID.LSID_MiliRank, LSID.LSID_MiliRank);
            StatsTitles[32] = new StatsTitleStruct(LSID.LSID_AAF_1, LSID.LSID_AAF_1);
            StatsTitles[33] = new StatsTitleStruct(LSID.LSID_AAF_2, LSID.LSID_AAF_2);
            StatsTitles[34] = new StatsTitleStruct(LSID.LSID_CertaintyIndex, LSID.LSID_CertaintyIndex);
            StatsTitles[35] = new StatsTitleStruct(LSID.LSID_BirthByMonth, LSID.LSID_BirthByMonth);
            StatsTitles[36] = new StatsTitleStruct(LSID.LSID_Demography, LSID.LSID_Demography);

            CheckSolveNames = new LSID[4];
            CheckSolveNames[0] = LSID.LSID_RM_Skip;
            CheckSolveNames[1] = LSID.LSID_SetIsDead;
            CheckSolveNames[2] = LSID.LSID_DefineSex;
            CheckSolveNames[3] = LSID.LSID_DoDelete;

            CondSigns = new string[]
            {
                "!=", "<", "<=", "==", "=>", ">", "contains", "not contains"
            };

            NameTypes = new LSID[]
            {
                LSID.LSID_DefaultValue,
                LSID.LSID_NTAka,
                LSID.LSID_NTBirth,
                LSID.LSID_NTImmigrant,
                LSID.LSID_NTMaiden,
                LSID.LSID_NTMarried
            };

            //BloodGroups = "|A+|A-|B+|B-|AB+|AB-|O+|O-";
            BloodGroups = "|(I) O+|(I) O-|(II) A+|(II) A-|(III) B+|(III) B-|(IV) AB+|(IV) AB-";
            //BloodGroups = new string[] { "", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-" };
        }
    }
}
