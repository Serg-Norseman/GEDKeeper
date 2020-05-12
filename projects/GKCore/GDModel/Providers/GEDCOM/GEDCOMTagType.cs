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

using System;

namespace GDModel.Providers.GEDCOM
{
    public enum GEDCOMTagType
    {
        Unknown,

        // Record's tags (don't change the order of the items, because its used to cast)
        INDI, // Individual [std]
        FAM, // Family [std]
        NOTE, // Note [std]
        OBJE, // Multimedia [std]
        SOUR, // Source [std]
        REPO, // Repository [std]
        _GROUP, // Group [GK]
        _RESEARCH, // Research [GK]
        _TASK, // Task [GK]
        _COMM, // Communication [GK]
        _LOC, // Location [GEDCOM 5.5EL]
        SUBN, // Submission [std]
        SUBM, // Submitter [std]

        // Other tags
        ABBR,
        ADDR,
        ADOP,
        ADR1,
        ADR2,
        ADR3,
        AFN, // AncestralFileNumber
        AGNC,
        ALIA,
        ANCE,
        ANCI,
        ANUL,
        ASSO,
        AUTH,
        BAPL,
        BAPM,
        BARM,
        BASM,
        BIRT,
        BLES,
        BURI,
        CAST,
        CAUS,
        CENS,
        CHAN,
        CHAR,
        CHIL,
        CHR,
        CHRA,
        CITY,
        CONC,
        CONF,
        CONL,
        CONT,
        COPR,
        CORP,
        CREM,
        CTRY,
        DATA,
        DATE,
        DEAT,
        DESC,
        DESI,
        DEST,
        DIV,
        DIVF,
        DSCR,
        EDUC,
        ENDL,
        EVEN,
        EMAIL,
        EMIG,
        ENGA,
        FACT,
        FAMC,
        FAMF,
        FAMS,
        FAX,
        FCOM,
        FILE,
        FONE,
        FORM,
        FROM,
        GEDC,
        GIVN,
        GRAD,
        HEAD,
        HUSB,
        IDNO,
        IMMI,
        INT,
        LANG,
        LATI,
        LONG,
        MAP,
        MARB,
        MARC,
        MARL,
        MARR,
        MARS,
        MEDI,
        NAME,
        NATI,
        NATU,
        NCHI,
        NICK,
        NMR,
        NPFX,
        NSFX,
        OCCU,
        ORDI,
        ORDN,
        PAGE,
        PEDI,
        PHON,
        PLAC,
        POST,
        PROB,
        PROP,
        PUBL,
        QUAY,
        REFN,
        RELA,
        RELI,
        RESI,
        RESN, // Restriction
        RETI,
        RFN, // PermanentRecordFileNumber
        RIN, // AutomatedRecordID
        ROMN,
        SEX,
        SLGC,
        SLGS,
        SPFX,
        SSN,
        STAE,
        STAT,
        SURN,
        TEMP,
        TEXT,
        TIME,
        TITL,
        TO,
        TRLR,
        TYPE,
        VERS,
        WIFE,
        WILL,
        WWW,

        // non-standard extended tags
        _AWARD,
        _BGRO, // [MyFamilyTree]
        _BOOKMARK, // [GK]
        _CENN, // Census Name [BKW6]
        _ELEC, // Election [???]
        _EXCM, // Excommunication [???]
        _EYES, // [Gen]
        _FOLDER, // [GK]
        _GOAL, // [GK]
        _GRP, // Group record [Genney]
        _HAIR, // [BKW6, PAF]
        _HOBBY,
        _LANG, // [GK], outdated, replaced by LANG
        _MARN, // Married Surname [BKW6]
        _MDCL, // Medical condition [???]
        _MDNA, // [MyFamilyTree]
        _MEMBER, // [GK]
        _MILI, // [GK]
        _MILI_DIS, // [GK]
        _MILI_IND, // [GK]
        _MILI_RANK, // [GK]
        _OBIT, // Obituary [???]
        _PATN, // Patronymic Name
        _PATRIARCH, // [GK]
        _PERCENT, // [GK]
        _PLAC, // Place/Location record [Family Historian]
        _PLC, // Place/Location record [Genney]
        _POSITION,
        _PRIM, // [PhpGedView, AQ3, PAF5, FO7]
        _PRIM_CUTOUT, // [FTB]
        _PRIORITY, // [GK]
        _RELN, // Religious Name [BKW6]
        _REV, // [GK]
        _STARTDATE, // [GK]
        _STAT,
        _STATUS, // [GK]
        _STOPDATE, // [GK]
        _TRAVEL,
        _UID, // 
        _YDNA, // [MyFamilyTree]
    }

    public static class GEDCOMTagName
    {
        public const string ABBR = "ABBR";
        public const string ADDR = "ADDR";
        public const string ADOP = "ADOP";
        public const string ADR1 = "ADR1";
        public const string ADR2 = "ADR2";
        public const string ADR3 = "ADR3";
        public const string AFN = "AFN"; // AncestralFileNumber
        public const string AGNC = "AGNC";
        public const string ALIA = "ALIA";
        public const string ANCE = "ANCE";
        public const string ANCI = "ANCI";
        public const string ANUL = "ANUL";
        public const string ASSO = "ASSO";
        public const string AUTH = "AUTH";
        public const string BAPL = "BAPL";
        public const string BAPM = "BAPM";
        public const string BARM = "BARM";
        public const string BASM = "BASM";
        public const string BIRT = "BIRT";
        public const string BLES = "BLES";
        public const string BURI = "BURI";
        public const string CAST = "CAST";
        public const string CAUS = "CAUS";
        public const string CENS = "CENS";
        public const string CHAN = "CHAN";
        public const string CHAR = "CHAR";
        public const string CHIL = "CHIL";
        public const string CHR = "CHR";
        public const string CHRA = "CHRA";
        public const string CITY = "CITY";
        public const string CONC = "CONC";
        public const string CONF = "CONF";
        public const string CONL = "CONL";
        public const string CONT = "CONT";
        public const string COPR = "COPR";
        public const string CORP = "CORP";
        public const string CREM = "CREM";
        public const string CTRY = "CTRY";
        public const string DATA = "DATA";
        public const string DATE = "DATE";
        public const string DEAT = "DEAT";
        public const string DESC = "DESC";
        public const string DESI = "DESI";
        public const string DEST = "DEST";
        public const string DIV = "DIV";
        public const string DIVF = "DIVF";
        public const string DSCR = "DSCR";
        public const string EDUC = "EDUC";
        public const string ENDL = "ENDL";
        public const string EVEN = "EVEN";
        public const string EMAIL = "EMAIL";
        public const string EMIG = "EMIG";
        public const string ENGA = "ENGA";
        public const string FACT = "FACT";
        public const string FAM = "FAM";
        public const string FAMC = "FAMC";
        public const string FAMF = "FAMF";
        public const string FAMS = "FAMS";
        public const string FAX = "FAX";
        public const string FCOM = "FCOM";
        public const string FILE = "FILE";
        public const string FONE = "FONE";
        public const string FORM = "FORM";
        public const string FROM = "FROM";
        public const string GEDC = "GEDC";
        public const string GIVN = "GIVN";
        public const string GRAD = "GRAD";
        public const string HEAD = "HEAD";
        public const string HUSB = "HUSB";
        public const string IDNO = "IDNO";
        public const string IMMI = "IMMI";
        public const string INDI = "INDI"; // IndividualRecord
        public const string INT = "INT";
        public const string LANG = "LANG";
        public const string LATI = "LATI";
        public const string LONG = "LONG";
        public const string MAP = "MAP";
        public const string MARB = "MARB";
        public const string MARC = "MARC";
        public const string MARL = "MARL";
        public const string MARR = "MARR";
        public const string MARS = "MARS";
        public const string MEDI = "MEDI";
        public const string NAME = "NAME";
        public const string NATI = "NATI";
        public const string NATU = "NATU";
        public const string NCHI = "NCHI";
        public const string NICK = "NICK";
        public const string NMR = "NMR";
        public const string NOTE = "NOTE";
        public const string NPFX = "NPFX";
        public const string NSFX = "NSFX";
        public const string OBJE = "OBJE";
        public const string OCCU = "OCCU";
        public const string ORDI = "ORDI";
        public const string ORDN = "ORDN";
        public const string PAGE = "PAGE";
        public const string PEDI = "PEDI";
        public const string PHON = "PHON";
        public const string PLAC = "PLAC";
        public const string POST = "POST";
        public const string PROB = "PROB";
        public const string PROP = "PROP";
        public const string PUBL = "PUBL";
        public const string QUAY = "QUAY";
        public const string REFN = "REFN";
        public const string RELA = "RELA";
        public const string RELI = "RELI";
        public const string REPO = "REPO";
        public const string RESI = "RESI";
        public const string RESN = "RESN"; // Restriction
        public const string RETI = "RETI";
        public const string RFN = "RFN"; // PermanentRecordFileNumber
        public const string RIN = "RIN"; // AutomatedRecordID
        public const string ROMN = "ROMN";
        public const string SEX = "SEX";
        public const string SLGC = "SLGC";
        public const string SLGS = "SLGS";
        public const string SOUR = "SOUR";
        public const string SPFX = "SPFX";
        public const string SSN = "SSN";
        public const string STAE = "STAE";
        public const string STAT = "STAT";
        public const string SUBM = "SUBM";
        public const string SUBN = "SUBN";
        public const string SURN = "SURN";
        public const string TEMP = "TEMP";
        public const string TEXT = "TEXT";
        public const string TIME = "TIME";
        public const string TITL = "TITL";
        public const string TO = "TO";
        public const string TRLR = "TRLR";
        public const string TYPE = "TYPE";
        public const string VERS = "VERS";
        public const string WIFE = "WIFE";
        public const string WILL = "WILL";
        public const string WWW = "WWW";

        // non-standard extended tags (other applications)
        public const string _AWARD = "_AWARD";
        public const string _BGRO = "_BGRO"; // [MyFamilyTree]
        public const string _CENN = "_CENN"; // Census Name [BKW6]
        public const string _ELEC = "_ELEC"; // Election [???]
        public const string _EXCM = "_EXCM"; // Excommunication [???]
        public const string _EYES = "_EYES"; // [Gen]
        public const string _GRP = "_GRP"; // Group record [Genney]
        public const string _HAIR = "_HAIR"; // [BKW6, PAF]
        public const string _HOBBY = "_HOBBY";
        public const string _LOC = "_LOC"; // [GEDCOM 5.5EL]
        public const string _MARN = "_MARN"; // Married Surname [BKW6]
        public const string _MDCL = "_MDCL"; // Medical condition [???]
        public const string _MDNA = "_MDNA"; // [MyFamilyTree]
        public const string _OBIT = "_OBIT"; // Obituary [???]
        public const string _PATN = "_PATN"; // Patronymic Name
        public const string _PLAC = "_PLAC"; // Place/Location record [Family Historian]
        public const string _PLC = "_PLC"; // Place/Location record [Genney]
        public const string _POSITION = "_POSITION";
        public const string _PRIM = "_PRIM"; // [PhpGedView, AQ3, PAF5, FO7]
        public const string _PRIM_CUTOUT = "_PRIM_CUTOUT"; // [FTB]
        public const string _RELN = "_RELN"; // Religious Name [BKW6]
        public const string _STAT = "_STAT";
        public const string _TRAVEL = "_TRAVEL";
        public const string _UID = "_UID"; // 
        public const string _YDNA = "_YDNA"; // [MyFamilyTree]

        // non-standard extended tags (GEDKeeper)
        public const string _BOOKMARK = "_BOOKMARK";
        public const string _COMM = "_COMM";
        public const string _FOLDER = "_FOLDER";
        public const string _GOAL = "_GOAL";
        public const string _GROUP = "_GROUP";
        public const string _LANG = "_LANG"; // [GK], outdated, replaced by LANG
        public const string _MEMBER = "_MEMBER";
        public const string _MILI = "_MILI";
        public const string _MILI_DIS = "_MILI_DIS";
        public const string _MILI_IND = "_MILI_IND";
        public const string _MILI_RANK = "_MILI_RANK";
        public const string _PATRIARCH = "_PATRIARCH";
        public const string _PERCENT = "_PERCENT";
        public const string _PRIORITY = "_PRIORITY";
        public const string _RESEARCH = "_RESEARCH";
        public const string _REV = "_REV";
        public const string _STARTDATE = "_STARTDATE";
        public const string _STOPDATE = "_STOPDATE";
        public const string _STATUS = "_STATUS";
        public const string _TASK = "_TASK";
    }
}
