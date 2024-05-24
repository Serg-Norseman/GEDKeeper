/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
        public const string APP_TITLE = "GEDKeeper";

        public const string APP_COPYRIGHT = "Copyright © 2009-2024 by Sergey V. Zhdanovskih";

        public const string APP_VERSION_2X = "2.30.0.0";
        public const string APP_VERSION_3X = "3.6.0.0";

#if !GK3
        public const string UpdateURL = "https://sourceforge.net/projects/gedkeeper/files/gk_version.xml";
#else
        public const string UpdateURL = "https://sourceforge.net/projects/gedkeeper/files/gk3_version.xml";
#endif


        public const string APP_MAIL = "gedkeeper@yandex.ru";
        public const string APP_SITE = "https://gedkeeper.net/";

        public const string APP_FORUM_EN = "https://groups.google.com/g/gedkeeper-en";
        public const string APP_CHANNEL_EN = "https://t.me/gedkeeper_en";

        public const string APP_FORUM_RU = "https://groups.google.com/g/gedkeeper-ru";
        public const string APP_CHANNEL_RU = "https://t.me/gedkeeper_ru";

        public const int APP_FORMAT_DEFVER = 39; // don't change it!

        /*
         * 44 - v2.19.0
         * 45 - v2.27.0 / v3.3.0
         * 46 - v2.29.0 / v3.5.0
         */
        public const int APP_FORMAT_CURVER = 46;

        public const string GEDCOM_EXT = "ged";
        public const string GEDCOM_SEC_EXT = "geds";
        public const string LUA_EXT = "lua";

        public const int NOTE_NAME_MAX_LENGTH = 64;
        public const int NOTE_NAME_DBL_MAX_LENGTH = 128;

        public const string GAPI_KEY = "AIzaSyCyPx_u1PhHaN2d3ld4J8hsgASF5lOdpGY";
        public const string YAPI_KEY = "a5653896-1335-477f-aac7-10a2ba9e52c5";

        public static int HighlightUnparentedColor = 0xFFCACA;
        public static int HighlightUnmarriedColor = 0xFFFFA1;
        public static int HighlightInaccessibleFiles = 0xFFCACA;
        public static int HighlightReadabilityRows = 0xEFEFEF;

        public const string INFO_HTTP_PREFIX = "http";
        public const string INFO_HREF_VIEW = "view_";
        public const string INFO_HREF_FILTER_INDI = "filter_indi_by_";

        #if OS_LINUX || OS_FREEBSD
        public const string DEF_FONT = "Noto Sans";
        #else
        public const string DEF_FONT = "Tahoma";
        #endif

        public const string CHECK_MARK = " ✔ ";

        public sealed class SexStruct
        {
            public LSID NameId;
            public string Sign;
            public string SymImage;
            public string DefPortraitImage;

            public SexStruct(LSID name, string sign, string symImage, string defPortraitImage)
            {
                NameId = name;
                Sign = sign;
                SymImage = symImage;
                DefPortraitImage = defPortraitImage;
            }
        }

        public sealed class MarStatusStruct
        {
            public LSID Name;
            public string StatSign;

            public MarStatusStruct(LSID name, string sign)
            {
                Name = name;
                StatSign = sign;
            }
        }

        public sealed class EventStruct
        {
            public LSID Name;
            public string Sign;
            public PersonEventKind Kind;
            public bool AcceptableEmpty;

            public EventStruct(LSID name, string sign, PersonEventKind kind, bool acceptableEmpty = false)
            {
                Name = name;
                Sign = sign;
                Kind = kind;
                AcceptableEmpty = acceptableEmpty;
            }
        }

        public sealed class DateKindStruct
        {
            public LSID Name;
            public byte Dates;

            public DateKindStruct(LSID name, byte dates)
            {
                Name = name;
                Dates = dates;
            }
        }

        public sealed class CalendarStruct
        {
            public LSID Name;
            public string Sign;
            public bool HasSupport;

            public CalendarStruct(LSID name, string sign, bool hasSupport)
            {
                Name = name;
                Sign = sign;
                HasSupport = hasSupport;
            }
        }

        public sealed class StoreTypeRec
        {
            public LSID Name;
            public string Sign;

            public StoreTypeRec(LSID name, string sign)
            {
                Name = name;
                Sign = sign;
            }
        }

        public sealed class StatsTitleStruct
        {
            public LSID Title;
            public LSID Cap;

            public StatsTitleStruct(LSID title, LSID cap)
            {
                Title = title;
                Cap = cap;
            }
        }

        public sealed class SpecialUserRef
        {
            public LSID Title;
            public string ResName;

            public SpecialUserRef(LSID title, string resName)
            {
                Title = title;
                ResName = resName;
            }
        }

        public sealed class RecordTypeStruct
        {
            public LSID Name;
            public string Sign;

            public RecordTypeStruct(LSID name, string sign)
            {
                Name = name;
                Sign = sign;
            }
        }

        public sealed class FARPropertyStruct
        {
            public LSID Name;
            public bool Enabled;

            public FARPropertyStruct(LSID name, bool enabled)
            {
                Name = name;
                Enabled = enabled;
            }
        }

        public static readonly LSID[] Restrictions;
        public static readonly RecordTypeStruct[] RecordTypes;
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
        public static readonly StatsTitleStruct[] StatsTitles;
        public static readonly LSID[] CheckSolveNames;
        public static readonly LSID[] NameTypes;
        public static readonly LSID[] ParentTypes;
        public static readonly string[] CondSigns;
        public static readonly string BloodGroups;
        public static readonly FARPropertyStruct[] FARPropertyTypes;
        public static readonly LSID URTreeNoteType;
        public static readonly LSID[] ChartWindowsShowModes;


        /// <summary>
        /// Bounds checks data for correctness.
        /// </summary>
        public const int PROVED_LIFE_LENGTH = 122; // max. duration of a person's life (proved)
        public const int MIN_PARENT_AGE = 10; // min. age of parent at child's birth
        public const int MAX_MOTHER_AGE = 55; // max. age of mother at child's birth
        public const int MAX_FATHER_AGE = 85; // max. age of father at child's birth
        public const int MIN_MARRIAGE_AGE = 13; // min. age for marriage
        public const int MAX_SPOUSES_DIFF = 90; // max. age difference between spouses
        public const int MAX_SIBLINGS_DIFF = 40; // max. age difference between brothers/sisters


        static GKData()
        {
            SpecialUserRefs = new SpecialUserRef[] {
                new SpecialUserRef(LSID.RI_Recruit, "tg_recruit.png"),
                new SpecialUserRef(LSID.RI_GeorgeKnight, "tg_george_cross.gif"),
                new SpecialUserRef(LSID.USSR_WWII_Combatant, "tg_soldier.gif"),
                new SpecialUserRef(LSID.USSR_WWII_KilledInBattle, "tg_soldier_fall.gif"),
                new SpecialUserRef(LSID.USSR_WWII_WorkerInRear, "tg_veteran_rear.gif"),
                new SpecialUserRef(LSID.USSR_Repressed, "tg_barbed_wire.gif"),
                new SpecialUserRef(LSID.USSR_CPSUMember, "tg_cpsu.png"),
                new SpecialUserRef(LSID.Religion_Islam, "tg_islam_sym.gif"),
                new SpecialUserRef(LSID.Religion_Catholicism, "tg_latin_cross.gif"),
                new SpecialUserRef(LSID.Religion_Orthodoxy, "tg_orthodox_cross.gif"),
                new SpecialUserRef(LSID.Religion_TheOldBelievers, "tg_oldritual_cross.gif"),
            };

            CertaintyAssessments = new LSID[] {
                LSID.Cert_1,
                LSID.Cert_2,
                LSID.Cert_3,
                LSID.Cert_4
            };

            GoalNames = new LSID[] {
                LSID.G_1,
                LSID.G_2,
                LSID.G_3,
                LSID.G_4
            };

            CommunicationDirs = new LSID[] {
                LSID.CD_1,
                LSID.CD_2
            };

            CommunicationNames = new LSID[] {
                LSID.Com_1,
                LSID.Com_2,
                LSID.Com_3,
                LSID.Com_4,
                LSID.Com_5,
                LSID.Com_6
            };

            StatusNames = new LSID[] {
                LSID.RStat_1,
                LSID.RStat_2,
                LSID.RStat_3,
                LSID.RStat_4,
                LSID.RStat_5,
                LSID.RStat_6
            };

            PriorityNames = new LSID[] {
                LSID.Prt_1,
                LSID.Prt_2,
                LSID.Prt_3,
                LSID.Prt_4,
                LSID.Prt_5
            };

            MediaTypes = new LSID[] {
                LSID.DefaultValue, // TODO: may be LSID.MT_15 (Unknown)?
                LSID.MT_02,
                LSID.MT_03,
                LSID.MT_04,
                LSID.MT_05,
                LSID.MT_06,
                LSID.MT_07,
                LSID.MT_08,
                LSID.MT_09,
                LSID.MT_10,
                LSID.MT_11,
                LSID.MT_12,
                LSID.MT_13,
                LSID.MT_14
                //LSID.MT_15 <Unknown removed to first position>
            };

            GKStoreTypes = new StoreTypeRec[] {
                new StoreTypeRec(LSID.STRef, ""),
                new StoreTypeRec(LSID.STStg, "stg:"),
                new StoreTypeRec(LSID.STArc, "arc:"),
                new StoreTypeRec(LSID.STRel, "rel:"),
                new StoreTypeRec(LSID.STWeb, "http")
            };

            // гр|юл|евр|фр|рим|исл(хид?)|?
            // н.ст.|ст.ст.|евр|фр|рим|исл(хид?)|?
            // G|J|H|FR|R|I|?
            // FIXME: use calendars for StrToGEDCOMDate() and class EventEditDlg
            DateCalendars = new CalendarStruct[] {
                new CalendarStruct(LSID.Cal_Gregorian, " [G]", true),
                new CalendarStruct(LSID.Cal_Julian, " [J]", true),
                new CalendarStruct(LSID.Cal_Hebrew, " [H]", false), // !
                new CalendarStruct(LSID.Cal_French, " [FR]", false),
                new CalendarStruct(LSID.Cal_Roman, " [R]", false),
                new CalendarStruct(LSID.Cal_Islamic, " [I]", false), // !
                new CalendarStruct(LSID.Unknown, "", false)
            };

            DateKinds = new DateKindStruct[] {
                new DateKindStruct(LSID.DK_0, 1), // Exact; 1
                new DateKindStruct(LSID.DK_1, 2), // Before; 2
                new DateKindStruct(LSID.DK_2, 1), // After; 1
                new DateKindStruct(LSID.DK_3, 3), // Between; 1,2
                new DateKindStruct(LSID.DK_4, 2), // Period before (to); 2
                new DateKindStruct(LSID.DK_5, 1), // Period from (from); 1
                new DateKindStruct(LSID.DK_6, 3), // Period between; 1,2
                new DateKindStruct(LSID.DK_7, 1), // About; 1
                new DateKindStruct(LSID.DK_8, 1), // Calculated; 1
                new DateKindStruct(LSID.DK_9, 1)  // Estimated; 1
            };

            PersonEvents = new EventStruct[] {
                new EventStruct(LSID.Event, GEDCOMTagName.EVEN, PersonEventKind.ekEvent),
                new EventStruct(LSID.Birth, GEDCOMTagName.BIRT, PersonEventKind.ekEvent),
                new EventStruct(LSID.Adoption, GEDCOMTagName.ADOP, PersonEventKind.ekEvent),
                new EventStruct(LSID.Christening, GEDCOMTagName.CHR, PersonEventKind.ekEvent),
                new EventStruct(LSID.Graduation, GEDCOMTagName.GRAD, PersonEventKind.ekEvent),
                new EventStruct(LSID.Retirement, GEDCOMTagName.RETI, PersonEventKind.ekEvent),
                new EventStruct(LSID.Naturalization, GEDCOMTagName.NATU, PersonEventKind.ekEvent),
                new EventStruct(LSID.Emigration, GEDCOMTagName.EMIG, PersonEventKind.ekEvent),
                new EventStruct(LSID.Immigration, GEDCOMTagName.IMMI, PersonEventKind.ekEvent),
                new EventStruct(LSID.Census, GEDCOMTagName.CENS, PersonEventKind.ekEvent),
                new EventStruct(LSID.LastWill, GEDCOMTagName.WILL, PersonEventKind.ekEvent),
                new EventStruct(LSID.ProbateOfWill, GEDCOMTagName.PROB, PersonEventKind.ekEvent),
                new EventStruct(LSID.Death, GEDCOMTagName.DEAT, PersonEventKind.ekEvent),
                new EventStruct(LSID.Burial, GEDCOMTagName.BURI, PersonEventKind.ekEvent),
                new EventStruct(LSID.Cremation, GEDCOMTagName.CREM, PersonEventKind.ekEvent),

                new EventStruct(LSID.Fact, GEDCOMTagName.FACT, PersonEventKind.ekFact),
                new EventStruct(LSID.Religion, GEDCOMTagName.RELI, PersonEventKind.ekFact),
                new EventStruct(LSID.Nationality, GEDCOMTagName.NATI, PersonEventKind.ekFact),
                new EventStruct(LSID.Residence, GEDCOMTagName.RESI, PersonEventKind.ekFact, true),
                new EventStruct(LSID.PhysicalDesc, GEDCOMTagName.DSCR, PersonEventKind.ekFact),
                new EventStruct(LSID.NationalIDNumber, GEDCOMTagName.IDNO, PersonEventKind.ekFact),
                new EventStruct(LSID.SocialSecurityNumber, GEDCOMTagName.SSN, PersonEventKind.ekFact),
                new EventStruct(LSID.ChildsCount, GEDCOMTagName.NCHI, PersonEventKind.ekFact),
                new EventStruct(LSID.MarriagesCount, GEDCOMTagName.NMR, PersonEventKind.ekFact),
                new EventStruct(LSID.Education, GEDCOMTagName.EDUC, PersonEventKind.ekFact),
                new EventStruct(LSID.Occupation, GEDCOMTagName.OCCU, PersonEventKind.ekFact),
                new EventStruct(LSID.Caste, GEDCOMTagName.CAST, PersonEventKind.ekFact),
                new EventStruct(LSID.PersonalProperty, GEDCOMTagName.PROP, PersonEventKind.ekFact),
                new EventStruct(LSID.NobilityTitle, GEDCOMTagName.TITL, PersonEventKind.ekFact),

                new EventStruct(LSID.Travel, GEDCOMTagName._TRAVEL, PersonEventKind.ekFact),
                new EventStruct(LSID.Hobby, GEDCOMTagName._HOBBY, PersonEventKind.ekFact),
                new EventStruct(LSID.Award, GEDCOMTagName._AWARD, PersonEventKind.ekFact),
                new EventStruct(LSID.Mili, GEDCOMTagName._MILI, PersonEventKind.ekFact),
                new EventStruct(LSID.MiliInd, GEDCOMTagName._MILI_IND, PersonEventKind.ekFact),
                new EventStruct(LSID.MiliDis, GEDCOMTagName._MILI_DIS, PersonEventKind.ekFact),
                new EventStruct(LSID.MiliRank, GEDCOMTagName._MILI_RANK, PersonEventKind.ekFact),

                new EventStruct(LSID.BloodGroup, GEDCOMTagName._BGRO, PersonEventKind.ekFact),
                new EventStruct(LSID.HairColor, GEDCOMTagName._HAIR, PersonEventKind.ekFact),
                new EventStruct(LSID.EyesColor, GEDCOMTagName._EYES, PersonEventKind.ekFact),
                new EventStruct(LSID.MDNAHaplogroup, GEDCOMTagName._MDNA, PersonEventKind.ekFact),
                new EventStruct(LSID.YDNAHaplogroup, GEDCOMTagName._YDNA, PersonEventKind.ekFact),

                new EventStruct(LSID.Baptism, GEDCOMTagName.BAPM, PersonEventKind.ekEvent),
                new EventStruct(LSID.BarMitzvah, GEDCOMTagName.BARM, PersonEventKind.ekEvent),
                new EventStruct(LSID.BatMitzvah, GEDCOMTagName.BASM, PersonEventKind.ekEvent),
                new EventStruct(LSID.Blessing, GEDCOMTagName.BLES, PersonEventKind.ekEvent),
                new EventStruct(LSID.AdultChristening, GEDCOMTagName.CHRA, PersonEventKind.ekEvent),
                new EventStruct(LSID.Confirmation, GEDCOMTagName.CONF, PersonEventKind.ekEvent),
                new EventStruct(LSID.FirstCommunion, GEDCOMTagName.FCOM, PersonEventKind.ekEvent),
                new EventStruct(LSID.Ordination, GEDCOMTagName.ORDN, PersonEventKind.ekEvent),
            };

            FamilyEvents = new EventStruct[] {
                new EventStruct(LSID.Event, GEDCOMTagName.EVEN, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_1, GEDCOMTagName.ENGA, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_2, GEDCOMTagName.MARR, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_3, GEDCOMTagName.MARB, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_4, GEDCOMTagName.MARC, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_5, GEDCOMTagName.MARL, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_6, GEDCOMTagName.MARS, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_7, GEDCOMTagName.ANUL, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_8, GEDCOMTagName.DIVF, PersonEventKind.ekEvent),
                new EventStruct(LSID.FEvt_9, GEDCOMTagName.DIV, PersonEventKind.ekEvent),
            };

            MarriageStatus = new MarStatusStruct[] {
                new MarStatusStruct(LSID.Unknown, ""),
                new MarStatusStruct(LSID.MarrRegistered, "MARRIED"),
                new MarStatusStruct(LSID.MarrNotRegistered, "MARRNOTREG"),
                new MarStatusStruct(LSID.MarrDivorced, "NOTMARR")
            };

            SexData = new SexStruct[] {
                new SexStruct(LSID.SexU, "U", "", ""),
                new SexStruct(LSID.SexM, "M", "Resources.sym_male.png", "Resources.pi_male_140.png"),
                new SexStruct(LSID.SexF, "F", "Resources.sym_female.png", "Resources.pi_female_140.png"),
                new SexStruct(LSID.SexX, "X", "", "")
            };

            RecordTypes = new RecordTypeStruct[] {
                new RecordTypeStruct(LSID.None, ""),
                new RecordTypeStruct(LSID.Person, "Individual"),
                new RecordTypeStruct(LSID.Family, "Family"),
                new RecordTypeStruct(LSID.Note, "Note"),
                new RecordTypeStruct(LSID.RPMultimedia, "Multimedia"),
                new RecordTypeStruct(LSID.Source, "Source"),
                new RecordTypeStruct(LSID.Repository, "Repository"),
                new RecordTypeStruct(LSID.Group, "Group"),
                new RecordTypeStruct(LSID.Research, "Research"),
                new RecordTypeStruct(LSID.Task, "Task"),
                new RecordTypeStruct(LSID.Communication, "Communication"),
                new RecordTypeStruct(LSID.Location, "Location"),
                new RecordTypeStruct(LSID.Submission, "Submission"),
                new RecordTypeStruct(LSID.Submitter, "Submitter")
            };

            Restrictions = new LSID[] {
                LSID.RestrictNone,
                LSID.RestrictLocked,
                LSID.RestrictConfidential,
                LSID.RestrictPrivacy
            };

            StatsTitles = new StatsTitleStruct[] {
                new StatsTitleStruct(LSID.AncestorsCount, LSID.Name),
                new StatsTitleStruct(LSID.DescendantsCount, LSID.Name),
                new StatsTitleStruct(LSID.GenerationsCount, LSID.Name),
                new StatsTitleStruct(LSID.Surname, LSID.Surname),
                new StatsTitleStruct(LSID.Name, LSID.Name),
                new StatsTitleStruct(LSID.Patronymic, LSID.Patronymic),
                new StatsTitleStruct(LSID.Age, LSID.Age),
                new StatsTitleStruct(LSID.LifeExpectancy, LSID.Age),
                new StatsTitleStruct(LSID.BirthYears, LSID.BirthYears),
                new StatsTitleStruct(LSID.BirthYearsDec, LSID.BirthYears),
                new StatsTitleStruct(LSID.DeathYears, LSID.DeathYears),
                new StatsTitleStruct(LSID.DeathYearsDec, LSID.DeathYears),
                new StatsTitleStruct(LSID.ChildsCount, LSID.Name),
                new StatsTitleStruct(LSID.DistrChilds, LSID.ChildsCount),
                new StatsTitleStruct(LSID.BirthPlace, LSID.BirthPlace),
                new StatsTitleStruct(LSID.DeathPlace, LSID.DeathPlace),
                new StatsTitleStruct(LSID.Residence, LSID.Residence),
                new StatsTitleStruct(LSID.Occupation, LSID.Occupation),
                new StatsTitleStruct(LSID.Religion, LSID.Religion),
                new StatsTitleStruct(LSID.Nationality, LSID.Nationality),
                new StatsTitleStruct(LSID.Education, LSID.Education),
                new StatsTitleStruct(LSID.Caste, LSID.Caste),
                new StatsTitleStruct(LSID.AgeFirstborn, LSID.Name),
                new StatsTitleStruct(LSID.MarriagesCount, LSID.Name),
                new StatsTitleStruct(LSID.MarriagesAge, LSID.Name),
                new StatsTitleStruct(LSID.DiffSpouses, LSID.Family),
                new StatsTitleStruct(LSID.Hobby, LSID.Hobby),
                new StatsTitleStruct(LSID.Award, LSID.Award),
                new StatsTitleStruct(LSID.Mili, LSID.Mili),
                new StatsTitleStruct(LSID.MiliInd, LSID.MiliInd),
                new StatsTitleStruct(LSID.MiliDis, LSID.MiliDis),
                new StatsTitleStruct(LSID.MiliRank, LSID.MiliRank),
                new StatsTitleStruct(LSID.AAF_1, LSID.AAF_1),
                new StatsTitleStruct(LSID.AAF_2, LSID.AAF_2),
                new StatsTitleStruct(LSID.CertaintyIndex, LSID.CertaintyIndex),
                new StatsTitleStruct(LSID.BirthByMonth, LSID.BirthByMonth),
                new StatsTitleStruct(LSID.Demography, LSID.Demography),
            };

            CheckSolveNames = new LSID[] {
                LSID.RM_Skip,
                LSID.SetIsDead,
                LSID.DefineSex,
                LSID.DoDelete,
                LSID.DoEdit,
                LSID.Repair
            };

            CondSigns = new string[] {
                "!=", "<", "<=", "==", "=>", ">", "contains", "not contains"
            };

            NameTypes = new LSID[] {
                LSID.DefaultValue,
                LSID.NTAka,
                LSID.NTBirth,
                LSID.NTImmigrant,
                LSID.NTMaiden,
                LSID.NTMarried
            };

            ParentTypes = new LSID[] {
                LSID.DefaultValue,
                LSID.PLT_Adopted,
                LSID.PLT_Birth,
                LSID.PLT_Foster,
            };

            BloodGroups = "|(I) O+|(I) O-|(II) A+|(II) A-|(III) B+|(III) B-|(IV) AB+|(IV) AB-";

            FARPropertyTypes = new FARPropertyStruct[] {
                new FARPropertyStruct(LSID.Name, true),
                new FARPropertyStruct(LSID.Place, false),
                new FARPropertyStruct(LSID.Address, false),
                new FARPropertyStruct(LSID.Fact, true),
                new FARPropertyStruct(LSID.Event, false),
                new FARPropertyStruct(LSID.Association, true),
            };

            URTreeNoteType = LSID.URTreeNoteType;

            ChartWindowsShowModes = new LSID[] {
                LSID.CWSM_Default,
                LSID.CWSM_Maximize,
                LSID.CWSM_LeftHalf,
                LSID.CWSM_RightHalf,
            };
        }
    }
}
