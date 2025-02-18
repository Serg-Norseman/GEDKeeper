/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

        public const string APP_COPYRIGHT = "Copyright © 2009-2025 by Sergey V. Zhdanovskih";

        public const string APP_VERSION_2X = "2.32.0.0";
        public const string APP_VERSION_3X = "3.8.0.0";

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
        public const string INFO_HREF_EXPAND_ASSO = "expand_asso_";
        public const string INFO_HREF_LOC_SUB = "map_loc_sub_";
        public const string INFO_HREF_LOC_INDI = "map_loc_indi_";

#if OS_LINUX || OS_FREEBSD
        public const string DEF_FONT = "Noto Sans";
#else
        public const string DEF_FONT = "Tahoma";
#endif

        public const string CHECK_MARK = " ✔ ";
        public const string CROSS_MARK = " ❌ ";

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
        public static readonly PredefinedEvent[] PredefinedEvents;
        public static readonly DateKindStruct[] DateKinds;
        public static readonly CalendarStruct[] DateCalendars;
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
        public static readonly LSID[] TextEffects;


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

            PredefinedEvents = new PredefinedEvent[] {
                new PredefinedEvent(LSID.Event, GEDCOMTagName.EVEN, EventKind.ekEvent, EventTarget.etAny),

                new PredefinedEvent(LSID.Birth, GEDCOMTagName.BIRT, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Adoption, GEDCOMTagName.ADOP, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Christening, GEDCOMTagName.CHR, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Graduation, GEDCOMTagName.GRAD, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Retirement, GEDCOMTagName.RETI, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Naturalization, GEDCOMTagName.NATU, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Emigration, GEDCOMTagName.EMIG, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Immigration, GEDCOMTagName.IMMI, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Census, GEDCOMTagName.CENS, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.LastWill, GEDCOMTagName.WILL, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.ProbateOfWill, GEDCOMTagName.PROB, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Death, GEDCOMTagName.DEAT, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Burial, GEDCOMTagName.BURI, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Cremation, GEDCOMTagName.CREM, EventKind.ekEvent, EventTarget.etIndividual),

                new PredefinedEvent(LSID.Fact, GEDCOMTagName.FACT, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Religion, GEDCOMTagName.RELI, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Nationality, GEDCOMTagName.NATI, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Residence, GEDCOMTagName.RESI, EventKind.ekFact, EventTarget.etIndividual, true),
                new PredefinedEvent(LSID.PhysicalDesc, GEDCOMTagName.DSCR, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.NationalIDNumber, GEDCOMTagName.IDNO, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.SocialSecurityNumber, GEDCOMTagName.SSN, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.ChildsCount, GEDCOMTagName.NCHI, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.MarriagesCount, GEDCOMTagName.NMR, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Education, GEDCOMTagName.EDUC, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Occupation, GEDCOMTagName.OCCU, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Caste, GEDCOMTagName.CAST, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.PersonalProperty, GEDCOMTagName.PROP, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.NobilityTitle, GEDCOMTagName.TITL, EventKind.ekFact, EventTarget.etIndividual),

                new PredefinedEvent(LSID.Travel, GEDCOMTagName._TRAVEL, EventKind.ekFact, EventTarget.etIndividual, true),
                new PredefinedEvent(LSID.Hobby, GEDCOMTagName._HOBBY, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Award, GEDCOMTagName._AWARD, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Mili, GEDCOMTagName._MILI, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.MiliInd, GEDCOMTagName._MILI_IND, EventKind.ekFact, EventTarget.etIndividual, true),
                new PredefinedEvent(LSID.MiliDis, GEDCOMTagName._MILI_DIS, EventKind.ekFact, EventTarget.etIndividual, true),
                new PredefinedEvent(LSID.MiliRank, GEDCOMTagName._MILI_RANK, EventKind.ekFact, EventTarget.etIndividual),

                new PredefinedEvent(LSID.BloodGroup, GEDCOMTagName._BGRO, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.HairColor, GEDCOMTagName._HAIR, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.EyesColor, GEDCOMTagName._EYES, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.MDNAHaplogroup, GEDCOMTagName._MDNA, EventKind.ekFact, EventTarget.etIndividual),
                new PredefinedEvent(LSID.YDNAHaplogroup, GEDCOMTagName._YDNA, EventKind.ekFact, EventTarget.etIndividual),

                new PredefinedEvent(LSID.Baptism, GEDCOMTagName.BAPM, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.BarMitzvah, GEDCOMTagName.BARM, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.BatMitzvah, GEDCOMTagName.BASM, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Blessing, GEDCOMTagName.BLES, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.AdultChristening, GEDCOMTagName.CHRA, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Confirmation, GEDCOMTagName.CONF, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.FirstCommunion, GEDCOMTagName.FCOM, EventKind.ekEvent, EventTarget.etIndividual),
                new PredefinedEvent(LSID.Ordination, GEDCOMTagName.ORDN, EventKind.ekEvent, EventTarget.etIndividual),

                new PredefinedEvent(LSID.FEvt_1, GEDCOMTagName.ENGA, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_2, GEDCOMTagName.MARR, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_3, GEDCOMTagName.MARB, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_4, GEDCOMTagName.MARC, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_5, GEDCOMTagName.MARL, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_6, GEDCOMTagName.MARS, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_7, GEDCOMTagName.ANUL, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_8, GEDCOMTagName.DIVF, EventKind.ekEvent, EventTarget.etFamily),
                new PredefinedEvent(LSID.FEvt_9, GEDCOMTagName.DIV, EventKind.ekEvent, EventTarget.etFamily),
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
                new StatsTitleStruct(LSID.AncestorsCount, LSID.GeneralName),
                new StatsTitleStruct(LSID.DescendantsCount, LSID.GeneralName),
                new StatsTitleStruct(LSID.GenerationsCount, LSID.GeneralName),
                new StatsTitleStruct(LSID.Surname, LSID.Surname),
                new StatsTitleStruct(LSID.GivenName, LSID.GivenName),
                new StatsTitleStruct(LSID.Patronymic, LSID.Patronymic),
                new StatsTitleStruct(LSID.Age, LSID.Age),
                new StatsTitleStruct(LSID.LifeExpectancy, LSID.Age),
                new StatsTitleStruct(LSID.BirthYears, LSID.BirthYears),
                new StatsTitleStruct(LSID.BirthYearsDec, LSID.BirthYears),
                new StatsTitleStruct(LSID.DeathYears, LSID.DeathYears),
                new StatsTitleStruct(LSID.DeathYearsDec, LSID.DeathYears),
                new StatsTitleStruct(LSID.ChildsCount, LSID.GeneralName),
                new StatsTitleStruct(LSID.DistrChilds, LSID.ChildsCount),
                new StatsTitleStruct(LSID.BirthPlace, LSID.BirthPlace),
                new StatsTitleStruct(LSID.DeathPlace, LSID.DeathPlace),
                new StatsTitleStruct(LSID.Residence, LSID.Residence),
                new StatsTitleStruct(LSID.Occupation, LSID.Occupation),
                new StatsTitleStruct(LSID.Religion, LSID.Religion),
                new StatsTitleStruct(LSID.Nationality, LSID.Nationality),
                new StatsTitleStruct(LSID.Education, LSID.Education),
                new StatsTitleStruct(LSID.Caste, LSID.Caste),
                new StatsTitleStruct(LSID.AgeFirstborn, LSID.GeneralName),
                new StatsTitleStruct(LSID.MarriagesCount, LSID.GeneralName),
                new StatsTitleStruct(LSID.MarriagesAge, LSID.GeneralName),
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
                new StatsTitleStruct(LSID.ParentsAge, LSID.ParentsAge),
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
                "!=", "<", "<=", "==", "=>", ">", "contains", "not contains", "contains mask", "not contains mask"
            };

            NameTypes = new LSID[] {
                LSID.DefaultValue,
                LSID.Adoption, // FIXME: user defined name types (gh#550)
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
                new FARPropertyStruct(LSID.GeneralName, true),
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

            TextEffects = new LSID[] {
                LSID.TE_Simple,
                LSID.TE_Sunken,
                LSID.TE_Raised,
                LSID.TE_Glow,
            };
        }
    }
}
