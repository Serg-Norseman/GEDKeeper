/*
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
using System.Collections.Generic;
using GDModel;
using GDModel.Providers;

namespace GDModel.Providers.GEDCOM
{
    public static class GEDCOMTagType
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
        public const string MARS = "MARS";
        public const string MARR = "MARR";
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
        public const string _EYES = "_EYES"; // [Gen]
        public const string _HAIR = "_HAIR"; // [BKW6, PAF]
        public const string _HOBBY = "_HOBBY";
        public const string _LOC = "_LOC"; // [GEDCOM 5.5EL]
        public const string _MARN = "_MARN"; // Married Surname [BKW6]
        public const string _PATN = "_PATN"; // Patronymic Name
        public const string _PLAC = "_PLAC"; // Place/Location record [Family Historian]
        public const string _POSITION = "_POSITION";
        public const string _PRIM = "_PRIM"; // [PhpGedView, AQ3, PAF5, FO7]
        public const string _PRIM_CUTOUT = "_PRIM_CUTOUT"; // [FTB]
        public const string _RELN = "_RELN"; // Religious Name [BKW6]
        public const string _STAT = "_STAT";
        public const string _TRAVEL = "_TRAVEL";
        public const string _UID = "_UID"; // 

        public const string _MDNA = "_MDNA"; // [MyFamilyTree]
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
        public const string _STARTDATE = "_STARTDATE";
        public const string _STOPDATE = "_STOPDATE";
        public const string _STATUS = "_STATUS";
        public const string _TASK = "_TASK";
    }


    public delegate GDMTag TagConstructor(GDMObject owner, string tagName, string tagValue);


    public sealed class TagInfo
    {
        public readonly TagConstructor Constructor;
        public readonly AddTagHandler AddHandler;
        public SaveTagHandler SaveHandler;
        public readonly bool SkipEmpty;
        public readonly bool GKExtend;

        public TagInfo(TagConstructor constructor, AddTagHandler addHandler)
        {
            Constructor = constructor;
            AddHandler = addHandler;
            SkipEmpty = false;
            GKExtend = false;
        }

        public TagInfo(TagConstructor constructor, AddTagHandler addHandler, SaveTagHandler saveHandler, bool skipEmpty, bool extend)
        {
            Constructor = constructor;
            AddHandler = addHandler;
            SaveHandler = saveHandler;
            SkipEmpty = skipEmpty;
            GKExtend = extend;
        }
    }


    public sealed class GEDCOMFactory
    {
        private static GEDCOMFactory fInstance;
        private readonly Dictionary<string, TagInfo> fTags;

        public static GEDCOMFactory GetInstance()
        {
            if (fInstance == null) fInstance = new GEDCOMFactory();
            return fInstance;
        }

        public GEDCOMFactory()
        {
            fTags = new Dictionary<string, TagInfo>();
        }

        public void RegisterTag(string tagName, TagConstructor constructor, AddTagHandler addHandler = null,
                                SaveTagHandler saveHandler = null, bool skipEmpty = false, bool extend = false)
        {
            TagInfo tagInfo;
            if (!fTags.TryGetValue(tagName, out tagInfo)) {
                tagInfo = new TagInfo(constructor, addHandler, saveHandler, skipEmpty, extend);
                fTags.Add(tagName, tagInfo);
            } else {
                //tagInfo.Constructor = constructor;
                //tagInfo.AddHandler = addHandler;
            }
        }

        public GDMTag CreateTag(GDMObject owner, string tagName, string tagValue)
        {
            TagInfo tagInfo;
            if (fTags.TryGetValue(tagName, out tagInfo)) {
                TagConstructor ctor = tagInfo.Constructor;
                return (ctor == null) ? new GDMTag(owner, tagName, tagValue) : ctor(owner, tagName, tagValue);
            }
            return null;
        }

        public TagInfo GetTagInfo(string tagName)
        {
            TagInfo result;
            fTags.TryGetValue(tagName, out result);
            return result;
        }

        public AddTagHandler GetAddHandler(string tagName)
        {
            TagInfo tagInfo;
            if (fTags.TryGetValue(tagName, out tagInfo)) {
                return tagInfo.AddHandler;
            }
            return GEDCOMProvider.AddBaseTag;
        }
    }
}
