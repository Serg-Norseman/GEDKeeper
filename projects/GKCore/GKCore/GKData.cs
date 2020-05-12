﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GKCore
{
    /// <summary>
    /// Global container for various constants.
    /// </summary>
    public static class GKData
    {
        // Until the next major version, we cannot change this line,
        // because it including defines the path to the profile of the program.
        public const string APP_TITLE = "GEDKeeper2";
        public const string APP_TITLE_NEW = "GEDKeeper";

        public const string APP_MAIL = "gedkeeper@yandex.ru";

        public const int APP_FORMAT_DEFVER = 39; // don't change it!
        public const int APP_FORMAT_CURVER = 41;

        public const string GEDCOM_EXT = "ged";
        public const string GEDCOM_SEC_EXT = "geds";
        public const string LUA_EXT = "lua";

        public const int NOTE_NAME_MAX_LENGTH = 64;

        public const string GAPI_KEY = "AIzaSyCebJC5BpniJtRaJCSEl3tXdFy3KhbV5hk";

        public static readonly int HighlightUnparentedColor = 0xFFCACA;
        public static readonly int HighlightUnmarriedColor = 0xFFFFA1;

        #if __MonoCS__
        public const string DEF_FONT = "Noto Sans";
        #else
        public const string DEF_FONT = "Tahoma";
        #endif

        public sealed class SexStruct
        {
            public LSID NameId;
            public string Sign;

            public SexStruct(LSID name, string sign) {
                NameId = name;
                Sign = sign;
            }
        }

        public sealed class MarStatusStruct
        {
            public LSID Name;
            public string StatSign;

            public MarStatusStruct(LSID name, string sign) {
                Name = name;
                StatSign = sign;
            }
        }

        public sealed class EventStruct
        {
            public LSID Name;
            public string Sign;
            public PersonEventKind Kind;

            public EventStruct(LSID name, string sign, PersonEventKind kind) {
                Name = name;
                Sign = sign;
                Kind = kind;
            }
        }

        public sealed class DateKindStruct
        {
            public LSID Name;
            public byte Dates;

            public DateKindStruct(LSID name, byte dates) {
                Name = name;
                Dates = dates;
            }
        }

        public sealed class CalendarStruct
        {
            public LSID Name;
            public string Sign;
            public bool HasSupport;

            public CalendarStruct(LSID name, string sign, bool hasSupport) {
                Name = name;
                Sign = sign;
                HasSupport = hasSupport;
            }
        }

        public sealed class StoreTypeRec
        {
            public LSID Name;
            public string Sign;

            public StoreTypeRec(LSID name, string sign) {
                Name = name;
                Sign = sign;
            }
        }

        public sealed class StatsTitleStruct
        {
            public LSID Title;
            public LSID Cap;

            public StatsTitleStruct(LSID title, LSID cap) {
                Title = title;
                Cap = cap;
            }
        }

        public sealed class SpecialUserRef
        {
            public LSID Title;
            public string ResName;

            public SpecialUserRef(LSID title, string resName) {
                Title = title;
                ResName = resName;
            }
        }

        public static readonly LSID[] Restrictions;
        public static readonly LSID[] RecordTypes;
        public static readonly SexStruct[] SexData;
        public static readonly MarStatusStruct[] MarriageStatus;
        public static readonly EventStruct[] PersonEvents;
        public static readonly DateKindStruct[] DateKinds;
        public static readonly CalendarStruct[] DateCalendars;
        public static readonly EventStruct[] FamilyEvents;
        public static readonly StoreTypeRec[] GKStoreTypes;
        public static readonly LSID[] MediaTypes;
        public static readonly LSID[] PriorityNames;
        public static readonly LSID[] StatusNames;
        public static readonly LSID[] CommunicationNames;
        public static readonly LSID[] CommunicationDirs;
        public static readonly LSID[] GoalNames;
        public static readonly LSID[] CertaintyAssessments;
        public static readonly SpecialUserRef[] SpecialUserRefs;
        public static readonly LSID[] RelationKinds;
        public static readonly string[] RelationSigns;
        public static readonly string[] Numerals;
        public static readonly string[] NumKinship;
        public static readonly LSID GreatPrefix;
        public static readonly StatsTitleStruct[] StatsTitles;
        public static readonly LSID[] CheckSolveNames;
        public static readonly LSID[] NameTypes;
        public static readonly LSID[] ParentTypes;
        public static readonly string[] CondSigns;
        public static readonly string BloodGroups;


        /// <summary>
        /// Bounds checks data for correctness.
        /// </summary>
        public const int PROVED_LIFE_LENGTH = 122; // max. duration of a person's life (proved)

        // TODO: implement checks and options!
        public const int MIN_PARENT_AGE = 10; // min. age of parent at child's birth
        public const int MAX_MOTHER_AGE = 55; // max. age of mother at child's birth
        public const int MAX_FATHER_AGE = 85; // max. age of father at child's birth
        public const int MIN_MARRIAGE_AGE = 15; // min. age for marriage
        public const int MAX_SPOUSES_DIFF = 90; // max. age difference between spouses
        public const int MAX_BRT_SST_DIFF = 40; // max. age difference between brothers/sisters


        static GKData()
        {
            GreatPrefix = LSID.LSID_RK_GreatPrefix;

            // TODO: need to find a way of localization
            NumKinship = new string[]
            {
                "-",
                "юродный",
                "юродная",
                ""
            };

            // TODO: need to find a way of localization
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


            SpecialUserRefs = new SpecialUserRef[]
            {
                new SpecialUserRef(LSID.LSID_RI_GeorgeKnight, ""),
                new SpecialUserRef(LSID.LSID_USSR_WWII_Combatant, ""),
                new SpecialUserRef(LSID.LSID_USSR_WWII_KilledInBattle, ""),
                new SpecialUserRef(LSID.LSID_USSR_WWII_WorkerInRear, ""),
                new SpecialUserRef(LSID.LSID_USSR_Repressed, ""),
                new SpecialUserRef(LSID.LSID_Religion_Islam, ""),
                new SpecialUserRef(LSID.LSID_Religion_Catholicism, ""),
                new SpecialUserRef(LSID.LSID_Religion_Orthodoxy, ""),
                new SpecialUserRef(LSID.LSID_Religion_TheOldBelievers, "")
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


            GKStoreTypes = new StoreTypeRec[]
            {
                new StoreTypeRec(LSID.LSID_STRef, ""),
                new StoreTypeRec(LSID.LSID_STStg, "stg:"),
                new StoreTypeRec(LSID.LSID_STArc, "arc:"),
                new StoreTypeRec(LSID.LSID_STRel, "rel:")
            };


            FamilyEvents = new EventStruct[] {
                new EventStruct(LSID.LSID_Event, GEDCOMTagName.EVEN, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_1, GEDCOMTagName.ENGA, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_2, GEDCOMTagName.MARR, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_3, GEDCOMTagName.MARB, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_4, GEDCOMTagName.MARC, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_5, GEDCOMTagName.MARL, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_6, GEDCOMTagName.MARS, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_7, GEDCOMTagName.ANUL, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_8, GEDCOMTagName.DIVF, PersonEventKind.ekEvent),
                new EventStruct(LSID.LSID_FEvt_9, GEDCOMTagName.DIV, PersonEventKind.ekEvent),
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


            EventStruct[] array6 = new EventStruct[41];
            array6[ 0] = new EventStruct(LSID.LSID_Event, GEDCOMTagName.EVEN, PersonEventKind.ekEvent);
            array6[ 1] = new EventStruct(LSID.LSID_Birth, GEDCOMTagName.BIRT, PersonEventKind.ekEvent);
            array6[ 2] = new EventStruct(LSID.LSID_Adoption, GEDCOMTagName.ADOP, PersonEventKind.ekEvent);
            array6[ 3] = new EventStruct(LSID.LSID_Christening, GEDCOMTagName.CHR, PersonEventKind.ekEvent);
            array6[ 4] = new EventStruct(LSID.LSID_Graduation, GEDCOMTagName.GRAD, PersonEventKind.ekEvent);
            array6[ 5] = new EventStruct(LSID.LSID_Retirement, GEDCOMTagName.RETI, PersonEventKind.ekEvent);
            array6[ 6] = new EventStruct(LSID.LSID_Naturalization, GEDCOMTagName.NATU, PersonEventKind.ekEvent);
            array6[ 7] = new EventStruct(LSID.LSID_Emigration, GEDCOMTagName.EMIG, PersonEventKind.ekEvent);
            array6[ 8] = new EventStruct(LSID.LSID_Immigration, GEDCOMTagName.IMMI, PersonEventKind.ekEvent);
            array6[ 9] = new EventStruct(LSID.LSID_Census, GEDCOMTagName.CENS, PersonEventKind.ekEvent);
            array6[10] = new EventStruct(LSID.LSID_LastWill, GEDCOMTagName.WILL, PersonEventKind.ekEvent);
            array6[11] = new EventStruct(LSID.LSID_ProbateOfWill, GEDCOMTagName.PROB, PersonEventKind.ekEvent);
            array6[12] = new EventStruct(LSID.LSID_Death, GEDCOMTagName.DEAT, PersonEventKind.ekEvent);
            array6[13] = new EventStruct(LSID.LSID_Burial, GEDCOMTagName.BURI, PersonEventKind.ekEvent);
            array6[14] = new EventStruct(LSID.LSID_Cremation, GEDCOMTagName.CREM, PersonEventKind.ekEvent);

            array6[15] = new EventStruct(LSID.LSID_Fact, GEDCOMTagName.FACT, PersonEventKind.ekFact);
            array6[16] = new EventStruct(LSID.LSID_Religion, GEDCOMTagName.RELI, PersonEventKind.ekFact);
            array6[17] = new EventStruct(LSID.LSID_Nationality, GEDCOMTagName.NATI, PersonEventKind.ekFact);
            array6[18] = new EventStruct(LSID.LSID_Residence, GEDCOMTagName.RESI, PersonEventKind.ekFact);
            array6[19] = new EventStruct(LSID.LSID_PhysicalDesc, GEDCOMTagName.DSCR, PersonEventKind.ekFact);
            array6[20] = new EventStruct(LSID.LSID_NationalIDNumber, GEDCOMTagName.IDNO, PersonEventKind.ekFact);
            array6[21] = new EventStruct(LSID.LSID_SocialSecurityNumber, GEDCOMTagName.SSN, PersonEventKind.ekFact);
            array6[22] = new EventStruct(LSID.LSID_ChildsCount, GEDCOMTagName.NCHI, PersonEventKind.ekFact);
            array6[23] = new EventStruct(LSID.LSID_MarriagesCount, GEDCOMTagName.NMR, PersonEventKind.ekFact);
            array6[24] = new EventStruct(LSID.LSID_Education, GEDCOMTagName.EDUC, PersonEventKind.ekFact);
            array6[25] = new EventStruct(LSID.LSID_Occupation, GEDCOMTagName.OCCU, PersonEventKind.ekFact);
            array6[26] = new EventStruct(LSID.LSID_Caste, GEDCOMTagName.CAST, PersonEventKind.ekFact);
            array6[27] = new EventStruct(LSID.LSID_Property, GEDCOMTagName.PROP, PersonEventKind.ekFact);
            array6[28] = new EventStruct(LSID.LSID_NobilityTitle, GEDCOMTagName.TITL, PersonEventKind.ekFact);

            array6[29] = new EventStruct(LSID.LSID_Travel, GEDCOMTagName._TRAVEL, PersonEventKind.ekFact);
            array6[30] = new EventStruct(LSID.LSID_Hobby, GEDCOMTagName._HOBBY, PersonEventKind.ekFact);
            array6[31] = new EventStruct(LSID.LSID_Award, GEDCOMTagName._AWARD, PersonEventKind.ekFact);
            array6[32] = new EventStruct(LSID.LSID_Mili, GEDCOMTagName._MILI, PersonEventKind.ekFact);
            array6[33] = new EventStruct(LSID.LSID_MiliInd, GEDCOMTagName._MILI_IND, PersonEventKind.ekFact);
            array6[34] = new EventStruct(LSID.LSID_MiliDis, GEDCOMTagName._MILI_DIS, PersonEventKind.ekFact);
            array6[35] = new EventStruct(LSID.LSID_MiliRank, GEDCOMTagName._MILI_RANK, PersonEventKind.ekFact);

            array6[36] = new EventStruct(LSID.LSID_BloodGroup, GEDCOMTagName._BGRO, PersonEventKind.ekFact);
            array6[37] = new EventStruct(LSID.LSID_HairColor, GEDCOMTagName._HAIR, PersonEventKind.ekFact);
            array6[38] = new EventStruct(LSID.LSID_EyesColor, GEDCOMTagName._EYES, PersonEventKind.ekFact);
            array6[39] = new EventStruct(LSID.LSID_MDNAHaplogroup, GEDCOMTagName._MDNA, PersonEventKind.ekFact);
            array6[40] = new EventStruct(LSID.LSID_YDNAHaplogroup, GEDCOMTagName._YDNA, PersonEventKind.ekFact);
            PersonEvents = array6;


            MarriageStatus = new MarStatusStruct[]
            {
                new MarStatusStruct(LSID.LSID_Unknown, ""),
                new MarStatusStruct(LSID.LSID_MarrRegistered, "MARRIED"),
                new MarStatusStruct(LSID.LSID_MarrNotRegistered, "MARRNOTREG"),
                new MarStatusStruct(LSID.LSID_MarrDivorced, "NOTMARR")
            };


            SexData = new SexStruct[]
            {
                new SexStruct(LSID.LSID_SexU, "U"),
                new SexStruct(LSID.LSID_SexM, "M"),
                new SexStruct(LSID.LSID_SexF, "F"),
                new SexStruct(LSID.LSID_SexX, "X")
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

            CheckSolveNames = new LSID[]
            {
                LSID.LSID_RM_Skip,
                LSID.LSID_SetIsDead,
                LSID.LSID_DefineSex,
                LSID.LSID_DoDelete,
                LSID.LSID_DoEdit
            };

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

            ParentTypes = new LSID[] {
                LSID.LSID_DefaultValue,
                LSID.LSID_PLT_Adopted,
                LSID.LSID_PLT_Birth,
                LSID.LSID_PLT_Foster,
            };

            BloodGroups = "|(I) O+|(I) O-|(II) A+|(II) A-|(III) B+|(III) B-|(IV) AB+|(IV) AB-";
            //BloodGroups = new string[] { "", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-" };
        }
    }
}
