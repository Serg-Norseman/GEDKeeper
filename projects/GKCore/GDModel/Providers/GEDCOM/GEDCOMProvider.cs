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
using System.Globalization;
using System.IO;
using System.Text;
using BSLib;
using GDModel;
using GDModel.Providers;
using GKCore;

namespace GDModel.Providers.GEDCOM
{
    public enum GEDCOMFormat
    {
        gf_Unknown,
        gf_Native,
        gf_Genealogy_RusOld,

        gf_Ahnenblatt,
        gf_AncestQuest,
        gf_AGES,
        gf_ALTREE,
        gf_EasyTree,
        gf_FamilyHistorian,
        gf_FamilyTreeMaker,
        gf_FTB,
        gf_GeneWeb,
        gf_Geni,
        gf_Genney,
        gf_GenoPro,
        gf_Gramps,
        gf_GENBOX,
        gf_GENJ,
        gf_Heredis,
        gf_Legacy,
        gf_Lifelines,
        gf_PAF,
        gf_Reunion,
        gf_RootsMagic,

        gf_Last = gf_RootsMagic
    }
    
    public delegate GDMTag TagConstructor(GDMObject owner, int tagId, string tagValue);

    public delegate StackTuple AddTagHandler(GDMObject owner, int tagLevel, int tagId, string tagValue);

    public delegate bool SaveTagHandler(StreamWriter stream, int level, GDMTag tag);
    /// <summary>
    /// 
    /// </summary>
    public class GEDCOMProvider : FileProvider
    {
        public const char GEDCOM_DELIMITER = ' ';
        public const char GEDCOM_YEAR_MODIFIER_SEPARATOR = '/';
        public const char GEDCOM_NAME_SEPARATOR = '/';
        public const string GEDCOM_YEAR_BC = "B.C.";
        public const char GEDCOM_POINTER_DELIMITER = '@';
        public const string GEDCOM_NEWLINE = "\r\n";
        public const int MAX_LINE_LENGTH = 248;

        // deprecated
        //public const byte GEDCOMMaxPhoneNumbers = 3;
        //public const byte GEDCOMMaxEmailAddresses = 3;
        //public const byte GEDCOMMaxFaxNumbers = 3;
        //public const byte GEDCOMMaxWebPages = 3;
        //public const byte GEDCOMMaxLanguages = 3;

        public static readonly GEDCOMAppFormat[] GEDCOMFormats;
        static GEDCOMProvider()
        {
            GEDCOMFormats = new GEDCOMAppFormat[] {
                new GEDCOMAppFormat(GEDCOMFormat.gf_Unknown, "", "", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Native, "GEDKeeper", "", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Genealogy_RusOld, "├σφσαδεπΦ", "Genealogy (Rus, old)", 1251), // signature in CP437

                new GEDCOMAppFormat(GEDCOMFormat.gf_AGES, "AGES", "Ages!", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_ALTREE, "ALTREE", "Agelong Tree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Ahnenblatt, "AHN", "Ahnenblatt", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_AncestQuest, "AncestQuest", "Ancestral Quest", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_EasyTree, "EasyTree", "EasyTree", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyHistorian, "FAMILY_HISTORIAN", "Family Historian", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyTreeMaker, "FTM", "Family Tree Maker", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FamilyTreeMaker, "FTW", "Family Tree Maker", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_FTB, "MYHERITAGE", "MyHeritage Family Tree Builder", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GENBOX, "GENBOX", "Genbox Family History", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GENJ, "GENJ", "GENJ", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Geni, "Geni.com", "Geni", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GeneWeb, "GeneWeb", "GeneWeb", 1252),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Genney, "Genney", "Genney", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_GenoPro, "GenoPro", "GenoPro", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Gramps, "Gramps", "Gramps", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Heredis, "HEREDIS 12 PC", "Heredis", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Legacy, "Legacy", "Legacy", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Lifelines, "Lifelines", "Lifelines", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_PAF, "PAF", "Personal Ancestral File", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_Reunion, "Reunion", "Reunion", -1),
                new GEDCOMAppFormat(GEDCOMFormat.gf_RootsMagic, "RootsMagic", "RootsMagic", -1),
            };


            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ABBR, GEDCOMTagName.ABBR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADDR, GEDCOMTagName.ADDR, GDMAddress.Create, StackTuple.AddAddressTag, WriteAddress, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADOP, GEDCOMTagName.ADOP, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR1, GEDCOMTagName.ADR1);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR2, GEDCOMTagName.ADR2);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ADR3, GEDCOMTagName.ADR3);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AFN, GEDCOMTagName.AFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AGNC, GEDCOMTagName.AGNC, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ALIA, GEDCOMTagName.ALIA, null, StackTuple.AddBaseTag, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANCE, GEDCOMTagName.ANCE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANCI, GEDCOMTagName.ANCI, GDMPointer.Create, StackTuple.AddBaseTag);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ANUL, GEDCOMTagName.ANUL, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ASSO, GEDCOMTagName.ASSO, null, StackTuple.AddAssociationTag, WriteAssociation, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.AUTH, GEDCOMTagName.AUTH, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BAPL, GEDCOMTagName.BAPL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BAPM, GEDCOMTagName.BAPM, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BARM, GEDCOMTagName.BARM, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BASM, GEDCOMTagName.BASM, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BIRT, GEDCOMTagName.BIRT, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BLES, GEDCOMTagName.BLES, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.BURI, GEDCOMTagName.BURI, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CAST, GEDCOMTagName.CAST, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CAUS, GEDCOMTagName.CAUS, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CENS, GEDCOMTagName.CENS, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHAN, GEDCOMTagName.CHAN, null, StackTuple.AddChangeDateTag, WriteChangeDate, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHAR, GEDCOMTagName.CHAR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHIL, GEDCOMTagName.CHIL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHR, GEDCOMTagName.CHR, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CHRA, GEDCOMTagName.CHRA, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CITY, GEDCOMTagName.CITY, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONC, GEDCOMTagName.CONC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONF, GEDCOMTagName.CONF, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONL, GEDCOMTagName.CONL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CONT, GEDCOMTagName.CONT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.COPR, GEDCOMTagName.COPR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CORP, GEDCOMTagName.CORP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CREM, GEDCOMTagName.CREM, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.CTRY, GEDCOMTagName.CTRY, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DATA, GEDCOMTagName.DATA, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DATE, GEDCOMTagName.DATE, GDMDateValue.Create, StackTuple.AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DEAT, GEDCOMTagName.DEAT, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DESC, GEDCOMTagName.DESC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DESI, GEDCOMTagName.DESI, GDMPointer.Create, StackTuple.AddBaseTag);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DEST, GEDCOMTagName.DEST);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DIV, GEDCOMTagName.DIV, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DIVF, GEDCOMTagName.DIVF, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.DSCR, GEDCOMTagName.DSCR, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EDUC, GEDCOMTagName.EDUC, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ENDL, GEDCOMTagName.ENDL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EVEN, GEDCOMTagName.EVEN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EMAIL, GEDCOMTagName.EMAIL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.EMIG, GEDCOMTagName.EMIG, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ENGA, GEDCOMTagName.ENGA, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FACT, GEDCOMTagName.FACT, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAM, GEDCOMTagName.FAM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMC, GEDCOMTagName.FAMC, GDMChildToFamilyLink.Create, StackTuple.AddChildToFamilyLinkTag, WriteChildToFamilyLink);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMF, GEDCOMTagName.FAMF);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAMS, GEDCOMTagName.FAMS, null, StackTuple.AddPointerWithNotesTag);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FAX, GEDCOMTagName.FAX);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FCOM, GEDCOMTagName.FCOM, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FILE, GEDCOMTagName.FILE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FONE, GEDCOMTagName.FONE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FORM, GEDCOMTagName.FORM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.FROM, GEDCOMTagName.FROM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GEDC, GEDCOMTagName.GEDC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GIVN, GEDCOMTagName.GIVN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.GRAD, GEDCOMTagName.GRAD, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.HEAD, GEDCOMTagName.HEAD);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.HUSB, GEDCOMTagName.HUSB, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.IDNO, GEDCOMTagName.IDNO, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.IMMI, GEDCOMTagName.IMMI, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.INDI, GEDCOMTagName.INDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.INT, GEDCOMTagName.INT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LANG, GEDCOMTagName.LANG, GDMLanguage.Create, StackTuple.AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LATI, GEDCOMTagName.LATI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.LONG, GEDCOMTagName.LONG);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MAP, GEDCOMTagName.MAP, GDMMap.Create, StackTuple.AddMapTag, WriteMap, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARB, GEDCOMTagName.MARB, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARC, GEDCOMTagName.MARC, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARL, GEDCOMTagName.MARL, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARS, GEDCOMTagName.MARS, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MARR, GEDCOMTagName.MARR, GDMFamilyEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.MEDI, GEDCOMTagName.MEDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NAME, GEDCOMTagName.NAME, null, StackTuple.AddPersonalNameTag, WritePersonalName);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NATI, GEDCOMTagName.NATI, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NATU, GEDCOMTagName.NATU, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NCHI, GEDCOMTagName.NCHI, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NICK, GEDCOMTagName.NICK, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NMR, GEDCOMTagName.NMR, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NOTE, GEDCOMTagName.NOTE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NPFX, GEDCOMTagName.NPFX, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.NSFX, GEDCOMTagName.NSFX, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.OBJE, GEDCOMTagName.OBJE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.OCCU, GEDCOMTagName.OCCU, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ORDI, GEDCOMTagName.ORDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ORDN, GEDCOMTagName.ORDN, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PAGE, GEDCOMTagName.PAGE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PEDI, GEDCOMTagName.PEDI);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PHON, GEDCOMTagName.PHON, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PLAC, GEDCOMTagName.PLAC, GDMPlace.Create, StackTuple.AddPlaceTag, WritePlace, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.POST, GEDCOMTagName.POST, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PROB, GEDCOMTagName.PROB, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PROP, GEDCOMTagName.PROP, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.PUBL, GEDCOMTagName.PUBL, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.QUAY, GEDCOMTagName.QUAY, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.REFN, GEDCOMTagName.REFN, null, StackTuple.AddUserReferenceTag, WriteUserReference);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RELA, GEDCOMTagName.RELA);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RELI, GEDCOMTagName.RELI, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.REPO, GEDCOMTagName.REPO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RESI, GEDCOMTagName.RESI, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RESN, GEDCOMTagName.RESN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RETI, GEDCOMTagName.RETI, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RFN, GEDCOMTagName.RFN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.RIN, GEDCOMTagName.RIN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.ROMN, GEDCOMTagName.ROMN);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SEX, GEDCOMTagName.SEX);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SLGC, GEDCOMTagName.SLGC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SLGS, GEDCOMTagName.SLGS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SOUR, GEDCOMTagName.SOUR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SPFX, GEDCOMTagName.SPFX, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SSN, GEDCOMTagName.SSN, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.STAE, GEDCOMTagName.STAE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.STAT, GEDCOMTagName.STAT, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SUBM, GEDCOMTagName.SUBM, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SUBN, GEDCOMTagName.SUBN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.SURN, GEDCOMTagName.SURN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TEMP, GEDCOMTagName.TEMP, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TEXT, GEDCOMTagName.TEXT, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TIME, GEDCOMTagName.TIME, GDMTime.Create, StackTuple.AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TITL, GEDCOMTagName.TITL, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TO, GEDCOMTagName.TO);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TRLR, GEDCOMTagName.TRLR);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.TYPE, GEDCOMTagName.TYPE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.VERS, GEDCOMTagName.VERS, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WIFE, GEDCOMTagName.WIFE, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WILL, GEDCOMTagName.WILL, GDMIndividualEvent.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType.WWW, GEDCOMTagName.WWW, null, null, null, true);

            // non-standard extended tags (other applications)
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._AWARD, GEDCOMTagName._AWARD, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._BGRO, GEDCOMTagName._BGRO, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._CENN, GEDCOMTagName._CENN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._ELEC, GEDCOMTagName._ELEC, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._EXCM, GEDCOMTagName._EXCM, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._EYES, GEDCOMTagName._EYES, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GRP, GEDCOMTagName._GRP, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._HAIR, GEDCOMTagName._HAIR, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._HOBBY, GEDCOMTagName._HOBBY, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._LOC, GEDCOMTagName._LOC, GDMPointer.Create, StackTuple.AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MARN, GEDCOMTagName._MARN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MDCL, GEDCOMTagName._MDCL, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MDNA, GEDCOMTagName._MDNA, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._OBIT, GEDCOMTagName._OBIT, GDMIndividualAttribute.Create, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PATN, GEDCOMTagName._PATN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PLAC, GEDCOMTagName._PLAC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PLC, GEDCOMTagName._PLC);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._POSITION, GEDCOMTagName._POSITION, GDMCutoutPosition.Create, StackTuple.AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIM, GEDCOMTagName._PRIM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIM_CUTOUT, GEDCOMTagName._PRIM_CUTOUT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._RELN, GEDCOMTagName._RELN, null, null, null, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STAT, GEDCOMTagName._STAT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._TRAVEL, GEDCOMTagName._TRAVEL, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._UID, GEDCOMTagName._UID);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._YDNA, GEDCOMTagName._YDNA, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent, true);

            // non-standard extended tags (GEDKeeper)
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._BOOKMARK, GEDCOMTagName._BOOKMARK);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._COMM, GEDCOMTagName._COMM);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._FOLDER, GEDCOMTagName._FOLDER);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GOAL, GEDCOMTagName._GOAL);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._GROUP, GEDCOMTagName._GROUP);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._LANG, GEDCOMTagName._LANG, GDMLanguage.Create, StackTuple.AddBaseTag, WriteBaseTag, true);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MEMBER, GEDCOMTagName._MEMBER);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI, GEDCOMTagName._MILI, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_DIS, GEDCOMTagName._MILI_DIS, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_IND, GEDCOMTagName._MILI_IND, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._MILI_RANK, GEDCOMTagName._MILI_RANK, GDMIndividualAttribute.Create, StackTuple.AddCustomEventTag, WriteCustomEvent);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PATRIARCH, GEDCOMTagName._PATRIARCH);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PERCENT, GEDCOMTagName._PERCENT);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._PRIORITY, GEDCOMTagName._PRIORITY);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._RESEARCH, GEDCOMTagName._RESEARCH);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._REV, GEDCOMTagName._REV);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STARTDATE, GEDCOMTagName._STARTDATE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STOPDATE, GEDCOMTagName._STOPDATE);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._STATUS, GEDCOMTagName._STATUS);
            GEDCOMTagsTable.RegisterTag(GEDCOMTagType._TASK, GEDCOMTagName._TASK);
        }

        public GEDCOMProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return LangMan.LS(LSID.LSID_GEDCOMFilter);
        }

        #region Encoding routines

        private enum EncodingState { esUnchecked, esUnchanged, esChanged }

        private const int DEF_CODEPAGE = 437;
        private Encoding fDefaultEncoding;
        private Encoding fSourceEncoding;
        private EncodingState fEncodingState;

        private void SetEncoding(Encoding encoding)
        {
            fSourceEncoding = encoding;
            fEncodingState = (fDefaultEncoding.Equals(fSourceEncoding)) ? EncodingState.esUnchanged : EncodingState.esChanged;
        }

        private void DefineEncoding(StreamReader reader, GEDCOMFormat format, string streamCharset)
        {
            GEDCOMCharacterSet charSet = fTree.Header.CharacterSet.Value;
            switch (charSet)
            {
                case GEDCOMCharacterSet.csUTF8:
                    if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                        SetEncoding(Encoding.UTF8); // file without BOM
                    } else {
                        fEncodingState = EncodingState.esUnchanged;
                    }
                    break;

                case GEDCOMCharacterSet.csUNICODE:
                    if (format == GEDCOMFormat.gf_Geni) {
                        SetEncoding(Encoding.UTF8);
                    } else if (format == GEDCOMFormat.gf_GENJ) {
                        SetEncoding(Encoding.UTF8);
                    } else {
                        if (!SysUtils.IsUnicodeEncoding(reader.CurrentEncoding)) {
                            SetEncoding(Encoding.Unicode); // file without BOM
                        } else {
                            fEncodingState = EncodingState.esUnchanged;
                        }
                    }
                    break;

                case GEDCOMCharacterSet.csANSEL:
                    if (format == GEDCOMFormat.gf_ALTREE) {
                        // Agelong Tree 4.0 with ANSEL is actually characteristic 
                        // for the Russian-language data export
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else if (format == GEDCOMFormat.gf_Geni) {
                        SetEncoding(Encoding.UTF8);
                    } else {
                        SetEncoding(new AnselEncoding());
                    }
                    break;

                case GEDCOMCharacterSet.csASCII:
                    if (format == GEDCOMFormat.gf_Native) {
                        // GEDKeeper native format (old) and ASCII charset
                        SetEncoding(Encoding.GetEncoding(1251));
                    } else {
                        var fmtProps = GetGEDCOMFormatProps(format);
                        if (fmtProps.PredefCharset > -1) {
                            SetEncoding(Encoding.GetEncoding(fmtProps.PredefCharset));
                        } else {
                            string cpVers = fTree.Header.CharacterSet.Version;
                            if (!string.IsNullOrEmpty(cpVers)) {
                                int sourceCodepage = ConvertHelper.ParseInt(cpVers, DEF_CODEPAGE);
                                SetEncoding(Encoding.GetEncoding(sourceCodepage));
                            } else {
                                if (fTree.Header.Language == GDMLanguageID.Russian) {
                                    SetEncoding(Encoding.GetEncoding(1251));
                                } else {
                                    if (streamCharset == null) {
                                        SetEncoding(Encoding.GetEncoding(DEF_CODEPAGE));
                                    } else {
                                        SetEncoding(Encoding.GetEncoding(streamCharset));
                                    }
                                }
                            }
                        }
                    }
                    break;
            }
        }

        protected override Encoding GetDefaultEncoding()
        {
            return Encoding.GetEncoding(DEF_CODEPAGE);
        }

        protected override string DetectCharset(Stream inputStream, bool charsetDetection)
        {
            string streamCharset = null;
            if (charsetDetection) {
                var charsetRes = GKUtils.DetectCharset(inputStream);
                if (charsetRes.Confidence >= 0.7f) {
                    streamCharset = charsetRes.Charset;
                }
            }
            return streamCharset;
        }

        #endregion

        #region Buffered read without excessive allocating memory

        private const int SB_SIZE = 32 * 1024;
        private const int LB_SIZE = 1024;

        private char[] fStreamBuffer, fLineBuffer;
        private int fStmBufLen, fStmBufPos, fLineBufPos;

        private void InitBuffers()
        {
            fStreamBuffer = new char[SB_SIZE];
            fLineBuffer = new char[LB_SIZE];
            fStmBufLen = 0;
            fStmBufPos = 0;
            fLineBufPos = 0;
        }

        private int ReadLine(StreamReader reader)
        {
            while (true) {
                if (fStmBufPos >= fStmBufLen) {
                    fStmBufLen = reader.Read(fStreamBuffer, 0, SB_SIZE);
                    if (fStmBufLen <= 0 && fLineBufPos <= 0) {
                        return -1; // eof, no more lines and no line's buffer
                    }
                    fStmBufPos = 0;
                }

                // here '\r' - it's replace for \0, to reduce checks
                char ch = (fStmBufPos >= fStmBufLen) ? '\r' : fStreamBuffer[fStmBufPos];
                fStmBufPos += 1;

                if (ch == '\r' || ch == '\n') {
                    int linePos = fLineBufPos;
                    fLineBufPos = 0;
                    if (linePos > 0) {
                        if (fEncodingState == EncodingState.esChanged) {
                            byte[] src = fDefaultEncoding.GetBytes(fLineBuffer, 0, linePos);
                            linePos = fSourceEncoding.GetChars(src, 0, src.Length, fLineBuffer, 0);
                        }

                        return linePos;
                    }
                } else {
                    fLineBuffer[fLineBufPos] = ch;
                    fLineBufPos += 1;
                }
            }
        }

        #endregion

        #region Loading functions

        protected override void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                fDefaultEncoding = GetDefaultEncoding();
                fSourceEncoding = fDefaultEncoding;
                fEncodingState = EncodingState.esUnchecked;

                long fileSize = fileStream.Length;
                int progress = 0;
                var invariantText = GEDCOMUtils.InvariantTextInfo;

                InitBuffers();
                var strTok = new GEDCOMParser(false);
                GDMTag curRecord = null;
                GDMTag curTag = null;
                var stack = new Stack<StackTuple>(9);

                int lineNum = 0;
                int lineLen;
                while ((lineLen = ReadLine(reader)) != -1) {
                    lineNum++;

                    int tagLevel;
                    string tagXRef, tagName, tagValue;
                    int tagId;

                        strTok.Reset(fLineBuffer, 0, lineLen);
                        int lineRes = GEDCOMUtils.ParseTag(strTok, out tagLevel, out tagXRef, out tagName, out tagValue);

                        // empty line
                        if (lineRes == -2) continue;

                        // line with text but not in standard tag format
                        if (lineRes == -1) {
                            if (fTree.Format == GEDCOMFormat.gf_FTB) {
                                FixFTBLine(curRecord, curTag, lineNum, tagValue);
                                continue;
                            } else {
                                throw new GDMInvalidFormatException(lineNum);
                            }
                        }

                        tagName = invariantText.ToUpper(tagName);
                        tagId = GEDCOMTagsTable.Lookup(tagName);

                    if (tagLevel == 0) {
                        if (curRecord == fTree.Header && fEncodingState == EncodingState.esUnchecked) {
                            // beginning recognition of the first is not header record
                            // to check for additional versions of the code page
                            var format = GetGEDCOMFormat(fTree);
                            fTree.Format = format;
                            DefineEncoding(reader, format, streamCharset);
                        }

                        StackTuple stackTuple = StackTuple.AddTreeTag(fTree, tagLevel, tagId, tagValue);
                        if (stackTuple != null) {
                            stack.Clear();
                            stack.Push(stackTuple);

                            curRecord = stackTuple.Tag;
                            if (!string.IsNullOrEmpty(tagXRef)) {
                                ((GDMRecord)curRecord).XRef = tagXRef;
                            }
                            curTag = null;
                        } else {
                            // only TRLR
                            break;
                        }
                    } else {
                        if (curRecord != null) {
                            curTag = ProcessTag(stack, tagLevel, tagId, tagValue);
                        }
                    }

                    if (progressHandler != null)
                    {
                        int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);
                        if (progress != newProgress)
                        {
                            progress = newProgress;
                            progressHandler(fTree, progress);
                        }
                    }
                }

                stack.Clear();

                if (lineNum == 0) {
                    throw new GEDCOMEmptyFileException();
                }
            } finally {
                fTree.State = GDMTreeState.osReady;
            }
        }

        internal static GDMTag ProcessTag(Stack<StackTuple> stack, int tagLevel, int tagId, string tagValue)
        {
            GDMTag curTag = null;

            GDMTag parentTag = null;
            AddTagHandler addTagHandler = null;
            while (stack.Count > 0) {
                var tuple = stack.Peek();
                if (tagLevel > tuple.Level) {
                    parentTag = tuple.Tag;
                    addTagHandler = tuple.AddHandler;
                    break;
                }
                stack.Pop();
            }

            if (parentTag != null) {
                StackTuple tuple = null;

                if (addTagHandler != null) {
                    tuple = addTagHandler(parentTag, tagLevel, tagId, tagValue);
                } else {
                    tuple = StackTuple.AddBaseTag(parentTag, tagLevel, tagId, tagValue);
                }

                if (tuple != null) {
                    stack.Push(tuple);
                    curTag = tuple.Tag;
                }
            }

            return curTag;
        }
        #endregion

        #region Saving functions

        public void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            using (FileStream fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write)) {
                SaveToStreamExt(fileStream, charSet);
            }
        }

        public void SaveToStreamExt(Stream outputStream, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!

            StreamWriter writer = new StreamWriter(outputStream, GEDCOMUtils.GetEncodingByCharacterSet(charSet));
            IList<GDMRecord> records = fTree.GetRecords().GetList();
            SaveToStream(writer, records);
            writer.Flush();
        }

        public void SaveToStream(StreamWriter writer, IList<GDMRecord> list)
        {
            // write header
            WriteHeader(writer, 0, fTree.Header);

            if (list != null) {
                int num = list.Count;
                for (int i = 0; i < num; i++) {
                    GDMRecord record = list[i];
                    WriteRecordEx(writer, record);
                }
            }

            // write footer
            WriteTagLine(writer, 0, GEDCOMTagName.TRLR, string.Empty);
        }

        public static void WriteRecordEx(StreamWriter writer, GDMRecord record)
        {
            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    WriteIndividualRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtFamily:
                    WriteFamilyRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtNote:
                    WriteNoteRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtMultimedia:
                    WriteMultimediaRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtSource:
                    WriteSourceRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtRepository:
                    WriteRepositoryRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtGroup:
                    WriteGroupRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtResearch:
                    WriteResearchRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtTask:
                    WriteTaskRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtCommunication:
                    WriteCommunicationRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtLocation:
                    WriteLocationRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtSubmission:
                    WriteSubmissionRecord(writer, 0, record);
                    break;

                case GDMRecordType.rtSubmitter:
                    WriteSubmitterRecord(writer, 0, record);
                    break;

                default:
                    WriteRecord(writer, 0, record);
                    break;
            }
        }

        #endregion

        #region Unified read/write functions

        private static void WriteIndividualRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMIndividualRecord indiRec = (GDMIndividualRecord)tag;

            WriteRecordWithEvents(stream, level, indiRec);

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.SEX, GEDCOMUtils.GetSexStr(indiRec.Sex), true);

            WriteList(stream, level, indiRec.PersonalNames, WritePersonalName);
            WriteList(stream, level, indiRec.ChildToFamilyLinks, WriteChildToFamilyLink);
            WriteList(stream, level, indiRec.SpouseToFamilyLinks, WriteTagEx);
            WriteList(stream, level, indiRec.Events, WriteCustomEvent);
            WriteList(stream, level, indiRec.Associations, WriteAssociation);
            WriteList(stream, level, indiRec.Aliases, WriteTagEx);
            WriteList(stream, level, indiRec.Groups, WriteTagEx);
        }

        private static void WriteFamilyRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFamilyRecord famRec = (GDMFamilyRecord)tag;

            WriteRecordWithEvents(stream, level, famRec);

            level += 1;
            WriteBaseTag(stream, level, famRec.Husband);
            WriteBaseTag(stream, level, famRec.Wife);
            WriteTagLine(stream, level, GEDCOMTagName._STAT, GEDCOMUtils.GetMarriageStatusStr(famRec.Status), true);

            WriteList(stream, level, famRec.Children, WriteTagEx);
            WriteList(stream, level, famRec.Events, WriteCustomEvent);
        }

        private static void WriteRecordWithEvents(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRecordWithEvents recWE = (GDMRecordWithEvents)tag;

            WriteRecord(stream, level, recWE);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.RESN, GEDCOMUtils.GetRestrictionStr(recWE.Restriction), true);
        }
        private static void WriteGroupRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)tag;

            WriteRecord(stream, level, groupRec);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, groupRec.GroupName, true);
            WriteList(stream, level, groupRec.Members, WriteTagEx);
        }
        private static void WriteMultimediaRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)tag;

            WriteRecord(stream, level, mmRec);
            WriteList(stream, ++level, mmRec.FileReferences, WriteFileReferenceWithTitle);
        }


        
        private static void WriteSourceRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceRecord sourRec = (GDMSourceRecord)tag;

            WriteRecord(stream, level, sourRec);

            level += 1;
            WriteText(stream, level, sourRec.Title);
            WriteText(stream, level, sourRec.Publication);
            WriteTagLine(stream, level, GEDCOMTagName.ABBR, sourRec.ShortTitle, true);
            WriteList(stream, level, sourRec.RepositoryCitations, WriteTagEx);

            WriteSourceData(stream, level, sourRec.Data);
            WriteText(stream, level, sourRec.Originator);
            WriteText(stream, level, sourRec.Text);
        }

        private static void WriteResearchRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMResearchRecord resRec = (GDMResearchRecord)tag;

            WriteRecord(stream, level, resRec);

            level += 1;

            WriteBaseTag(stream, level, resRec.StartDate);
            WriteBaseTag(stream, level, resRec.StopDate);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, resRec.ResearchName, true);
            WriteTagLine(stream, level, GEDCOMTagName._PRIORITY, GEDCOMUtils.GetPriorityStr(resRec.Priority), true);
            WriteTagLine(stream, level, GEDCOMTagName._STATUS, GEDCOMUtils.GetStatusStr(resRec.Status), true);
            WriteTagLine(stream, level, GEDCOMTagName._PERCENT, GEDCOMUtils.GetIntStr(resRec.Percent), true);

            WriteList(stream, level, resRec.Tasks, WriteTagEx);
            WriteList(stream, level, resRec.Communications, WriteTagEx);
            WriteList(stream, level, resRec.Groups, WriteTagEx);
        }

        private static void WriteNoteRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMNoteRecord noteRec = (GDMNoteRecord)tag;

            WriteRecordValue(stream, level, noteRec);
            level += 1;
            WriteText(stream, level, noteRec, true);
            WriteSubTags(stream, level, tag);
            if (!DebugWrite) {
                WriteTagLine(stream, level, GEDCOMTagName._UID, noteRec.UID, true);
                WriteChangeDate(stream, level, noteRec.ChangeDate);
            }
            WriteList(stream, level, noteRec.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, noteRec.UserReferences, WriteUserReference);
        }        
        private static void WriteRepositoryRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRepositoryRecord repoRec = (GDMRepositoryRecord)tag;

            WriteRecord(stream, level, repoRec);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.NAME, repoRec.RepositoryName, true);
            WriteAddress(stream, level, repoRec.Address);
        }
        private static void WriteTaskRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMTaskRecord taskRec = (GDMTaskRecord)tag;

            WriteRecord(stream, level, taskRec);

            level += 1;
            WriteBaseTag(stream, level, taskRec.StartDate);
            WriteBaseTag(stream, level, taskRec.StopDate);
            WriteTagLine(stream, level, GEDCOMTagName._PRIORITY, GEDCOMUtils.GetPriorityStr(taskRec.Priority), true);
            WriteTagLine(stream, level, GEDCOMTagName._GOAL, taskRec.Goal, true);
        }
        private static void WriteCommunicationRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMCommunicationRecord commRec = (GDMCommunicationRecord)tag;

            WriteRecord(stream, level, commRec);

            level += 1;
            WriteBaseTag(stream, level, commRec.Date);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, commRec.CommName, true);
            WriteTagLine(stream, level, GEDCOMTagName.TYPE, GEDCOMUtils.GetCommunicationTypeStr(commRec.CommunicationType), true);
            WriteBaseTag(stream, level, commRec.Corresponder);
        }
        private static void WriteSubmissionRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSubmissionRecord submnRec = (GDMSubmissionRecord)tag;

            WriteRecord(stream, level, submnRec);

            level += 1;
            WriteBaseTag(stream, level, submnRec.Submitter);
            WriteTagLine(stream, level, GEDCOMTagName.FAMF, submnRec.FamilyFileName, true);
            WriteTagLine(stream, level, GEDCOMTagName.TEMP, submnRec.TempleCode, true);
            WriteTagLine(stream, level, GEDCOMTagName.ANCE, GEDCOMUtils.GetIntStr(submnRec.GenerationsOfAncestors), true);
            WriteTagLine(stream, level, GEDCOMTagName.DESC, GEDCOMUtils.GetIntStr(submnRec.GenerationsOfDescendants), true);
            WriteTagLine(stream, level, GEDCOMTagName.ORDI, GEDCOMUtils.GetOrdinanceProcessFlagStr(submnRec.OrdinanceProcessFlag), true);
        }
        private static void WriteSubmitterRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)tag;

            WriteRecord(stream, level, submrRec);

            level += 1;
            WritePersonalName(stream, level, submrRec.Name);
            WriteList(stream, level, submrRec.Languages, WriteTagEx);
            WriteAddress(stream, level, submrRec.Address);
            WriteTagLine(stream, level, GEDCOMTagName.RFN, submrRec.RegisteredReference, true);
        }
        private static void WriteRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMRecord record = (GDMRecord)tag;

            WriteRecordValue(stream, level, record);

            level += 1;
            if (!DebugWrite) {
                WriteTagLine(stream, level, GEDCOMTagName._UID, record.UID, true);
                WriteChangeDate(stream, level, record.ChangeDate);
            }
            WriteSubTags(stream, level, tag);

            WriteTagLine(stream, level, GEDCOMTagName.RIN, record.AutomatedRecordID, true);
            WriteList(stream, level, record.Notes, WriteNote);
            WriteList(stream, level, record.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, record.MultimediaLinks, WriteMultimediaLink);
            WriteList(stream, level, record.UserReferences, WriteUserReference);
        }
        private static bool WriteHeader(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeader header = (GDMHeader)tag;

            if (!WriteBaseTag(stream, level, header)) return false;

            level += 1;
            WriteHeaderSource(stream, level, header.Source);
            WriteTagLine(stream, level, GEDCOMTagName.DEST, header.ReceivingSystemName, true);
            WriteHeaderCharSet(stream, level, header.CharacterSet);
            WriteTagLine(stream, level, GEDCOMTagName.LANG, GEDCOMUtils.GetLanguageStr(header.Language), true);
            WriteHeaderGEDCOM(stream, level, header.GEDCOM);
            WriteHeaderFile(stream, level, header.File);
            WriteDateTime(stream, level, header.TransmissionDateTime);
            WriteTagLine(stream, level, GEDCOMTagName.COPR, header.Copyright, true);
            WritePlace(stream, level, header.Place);
            WriteBaseTag(stream, level, header.Submitter);
            WriteBaseTag(stream, level, header.Submission);
            WriteText(stream, level, header.Note);
            return true;
        }
        private static bool WriteHeaderSource(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderSource headerSource = (GDMHeaderSource)tag;

            if (!WriteBaseTag(stream, level, headerSource)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.VERS, headerSource.Version, true);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, headerSource.ProductName, true);
            return true;
        }

        private static bool WriteHeaderGEDCOM(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderGEDCOM headerGEDCOM = (GDMHeaderGEDCOM)tag;

            if (!WriteBaseTag(stream, level, headerGEDCOM)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.VERS, headerGEDCOM.Version, true);
            WriteTagLine(stream, level, GEDCOMTagName.FORM, headerGEDCOM.Form, true);
            return true;
        }




        private static bool WriteHeaderCharSet(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderCharSet headerCharSet = (GDMHeaderCharSet)tag;

            if (!WriteBaseTag(stream, level, headerCharSet)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.VERS, headerCharSet.Version, true);
            return true;
        }

        private static bool WriteHeaderFile(StreamWriter stream, int level, GDMTag tag)
        {
            GDMHeaderFile headerFile = (GDMHeaderFile)tag;

            if (!WriteBaseTag(stream, level, headerFile)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName._REV, GEDCOMUtils.GetIntStr(headerFile.Revision), true);
            return true;
        }
        private static bool WriteChangeDate(StreamWriter stream, int level, GDMTag tag)
        {
            GDMChangeDate changeDate = (GDMChangeDate)tag;

            if (!WriteBaseTag(stream, level, changeDate)) return false;

            level += 1;
            WriteDateTime(stream, level, changeDate.ChangeDateTime);
            return true;
        }

        private static void WriteDateTime(StreamWriter stream, int level, DateTime dtx)
        {
            if (!dtx.Equals(GDMChangeDate.ZeroDateTime)) {
                WriteTagLine(stream, level, GEDCOMTagName.DATE, GEDCOMUtils.GetDateStr(dtx), true);
                WriteTagLine(stream, ++level, GEDCOMTagName.TIME, GEDCOMUtils.GetTimeStr(dtx.TimeOfDay), true);
            }
        }      

        private static bool WriteCustomEvent(StreamWriter stream, int level, GDMTag tag)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)tag;

            if (!WriteBaseTag(stream, level, custEvent)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.TYPE, custEvent.Classification, true);
            WriteBaseTag(stream, level, custEvent.Date);
            WritePlace(stream, level, custEvent.Place);
            WriteAddress(stream, level, custEvent.Address);
            WriteTagLine(stream, level, GEDCOMTagName.CAUS, custEvent.Cause, true);
            WriteTagLine(stream, level, GEDCOMTagName.AGNC, custEvent.Agency, true);
            WriteTagLine(stream, level, GEDCOMTagName.RELI, custEvent.ReligiousAffilation, true);
            WriteTagLine(stream, level, GEDCOMTagName.RESN, GEDCOMUtils.GetRestrictionStr(custEvent.Restriction), true);

            WriteList(stream, level, custEvent.Notes, WriteNote);
            WriteList(stream, level, custEvent.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, custEvent.MultimediaLinks, WriteMultimediaLink);
            return true;
        }

        private static bool WriteSourceData(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceData sourData = (GDMSourceData)tag;

            if (!WriteTagWithLists(stream, level, tag)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.AGNC, sourData.Agency, true);
            WriteList(stream, level, sourData.Events, WriteSourceDataEvent);
            return true;
        }
        private static bool WriteSourceDataEvent(StreamWriter stream, int level, GDMTag tag)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)tag;

            if (!WriteBaseTag(stream, level, dataEvent)) return false;

            level += 1;
            WriteBaseTag(stream, level, dataEvent.Date);
            WritePlace(stream, level, dataEvent.Place);
            return true;
        }

        private static bool WriteTagWithLists(StreamWriter stream, int level, GDMTag tag)
        {
            GDMTagWithLists tagWL = (GDMTagWithLists)tag;

            if (!WriteBaseTag(stream, level, tagWL)) return false;

            level += 1;
            WriteList(stream, level, tagWL.Notes, WriteNote);
            WriteList(stream, level, tagWL.SourceCitations, WriteSourceCitation);
            WriteList(stream, level, tagWL.MultimediaLinks, WriteMultimediaLink);
            return true;
        }

        private static bool WriteText(StreamWriter stream, int level, IGDMTextObject textTag, bool skipTag = false)
        {
            if (textTag.IsEmpty()) return false;

            var strings = textTag.Lines;
            int strCount = strings.Count;
            for (int i = 0; i < strCount; i++) {
                string str = strings[i];

                int len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                string sub = str.Substring(0, len);
                str = str.Remove(0, len);

                if (i == 0 && !skipTag) {
                    WriteTagLine(stream, level, ((GDMTag)textTag).GetTagName(), sub);
                    level += 1;
                } else {
                    WriteTagLine(stream, level, GEDCOMTagName.CONT, sub);
                }

                while (str.Length > 0) {
                    len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);

                    WriteTagLine(stream, level, GEDCOMTagName.CONC, str.Substring(0, len));

                    str = str.Remove(0, len);
                }
            }

            return true;
        }

        private static void WriteSubTags(StreamWriter stream, int level, GDMTag tag)
        {
            var subTags = tag.SubTags;

            int subtagsCount = subTags.Count;
            if (subtagsCount > 0) {
                for (int i = 0; i < subtagsCount; i++) {
                    GDMTag subtag = subTags[i];
                    var tagType = subtag.GetTagType();

                    if (tagType == GEDCOMTagType.CONC || tagType == GEDCOMTagType.CONT) {
                        WriteBaseTag(stream, level, subtag);
                    }
                }

                for (int i = 0; i < subtagsCount; i++) {
                    GDMTag subtag = subTags[i];
                    var tagType = subtag.GetTagType();

                    if (tagType != GEDCOMTagType.CONT && tagType != GEDCOMTagType.CONC) {
                        WriteTagEx(stream, level, subtag);
                    }
                }
            }
        }

        private static void WriteRecordValue(StreamWriter stream, int level, GDMRecord record)
        {
            string str = level.ToString();

            if (!string.IsNullOrEmpty(record.XRef)) {
                str = str + " @" + record.XRef + "@";
            }

            str = str + " " + record.GetTagName();

            string strValue = record.StringValue;
            if (!string.IsNullOrEmpty(strValue)) {
                str = str + " " + strValue;
            }

            stream.Write(str + GEDCOMProvider.GEDCOM_NEWLINE);
        }

        private static void WriteTagValue(StreamWriter stream, int level, GDMTag tag)
        {
            WriteTagLine(stream, level, tag.GetTagName(), tag.StringValue);
        }

        public static bool WriteTagEx(StreamWriter stream, int level, GDMTag tag)
        {
            bool result;
            GEDCOMTagProps tagInfo = GEDCOMTagsTable.GetTagProps(tag.Id);
            if (tagInfo == null) {
                result = WriteBaseTag(stream, level, tag);
            } else {
                SaveTagHandler saveHandler = tagInfo.SaveHandler;
                if (saveHandler == null) {
                    result = WriteBaseTag(stream, level, tag);
                } else {
                    result = saveHandler(stream, level, tag);
                }
            }
            return result;
        }

        private static bool WriteBaseTag(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            WriteTagValue(stream, level, tag);
            WriteSubTags(stream, ++level, tag);
            return true;
        }

        private static void WriteList<T>(StreamWriter stream, int level, GDMList<T> list, SaveTagHandler tagHandler) where T : GDMTag
        {
            IList<T> internalList = list.GetList();
            if (internalList == null) return;

            int num = internalList.Count;
            for (int i = 0; i < num; i++) {
                var item = internalList[i];
                if (item != null) {
                    tagHandler(stream, level, item);
                }
            }
        }

        // debug field
        public static bool DebugWrite = false;

        private static void WriteTagLine(StreamWriter stream, int level, string tagName, string tagValue, bool skipEmpty = false)
        {
            bool isEmpty = string.IsNullOrEmpty(tagValue);
            if (string.IsNullOrEmpty(tagName) || (isEmpty && skipEmpty)) return;

            string str = level + " " + tagName;
            if (!string.IsNullOrEmpty(tagValue)) {
                str = str + " " + tagValue;
            }
            stream.Write(str + GEDCOM_NEWLINE);
        }

        private static void WriteLocationRecord(StreamWriter stream, int level, GDMTag tag)
        {
            GDMLocationRecord locRec = (GDMLocationRecord)tag;

            WriteRecord(stream, level, locRec);

            level += 1;
            WriteMap(stream, level, locRec.Map);
            WriteTagLine(stream, level, GEDCOMTagName.NAME, locRec.LocationName, true);
        }
        private static bool WritePlace(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPlace place = (GDMPlace)tag;

            if (!WriteTagWithLists(stream, level, tag)) return false;

            level += 1;
            WriteBaseTag(stream, level, place.Location);
            WriteMap(stream, level, place.Map);
            WriteTagLine(stream, level, GEDCOMTagName.FORM, place.Form, true);
            return true;
        }
        private static bool WriteMap(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMap map = (GDMMap)tag;
            if (map.IsEmpty() && GEDCOMProvider.SkipEmptyTag(map.Id)) return false;

            WriteTagValue(stream, level, tag);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.LATI, GEDCOMUtils.GetFloatStr(map.Lati), true);
            WriteTagLine(stream, level, GEDCOMTagName.LONG, GEDCOMUtils.GetFloatStr(map.Long), true);
            return true;
        }

        private static bool WriteUserReference(StreamWriter stream, int level, GDMTag tag)
        {
            GDMUserReference userRef = (GDMUserReference)tag;

            if (!WriteBaseTag(stream, level, userRef)) return false;

            GEDCOMProvider.WriteTagLine(stream, ++level, GEDCOMTagName.TYPE, userRef.ReferenceType, true);
            return true;
        }


        

        // Format: FORM\TYPE
        private static bool WriteFileReferenceWithTitle(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFileReferenceWithTitle fileRef = (GDMFileReferenceWithTitle)tag;

            if (!WriteBaseTag(stream, level, fileRef)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.FORM, GEDCOMUtils.GetMultimediaFormatStr(fileRef.MultimediaFormat), true);
            GEDCOMProvider.WriteTagLine(stream, (level+1), GEDCOMTagName.TYPE, GEDCOMUtils.GetMediaTypeStr(fileRef.MediaType), true);
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.TITL, fileRef.Title, true);
            return true;
        }

        // Format: FORM\MEDI
        private static bool WriteFileReference(StreamWriter stream, int level, GDMTag tag)
        {
            GDMFileReference fileRef = (GDMFileReference)tag;

            if (!WriteBaseTag(stream, level, fileRef)) return false;

            level += 1;
            GEDCOMProvider.WriteTagLine(stream, level, GEDCOMTagName.FORM, GEDCOMUtils.GetMultimediaFormatStr(fileRef.MultimediaFormat), true);
            GEDCOMProvider.WriteTagLine(stream, ++level, GEDCOMTagName.MEDI, GEDCOMUtils.GetMediaTypeStr(fileRef.MediaType), true);
            return true;
        }
        public static bool WriteMultimediaLink(StreamWriter stream, int level, GDMTag tag)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)tag;

            if (!WriteBaseTag(stream, level, mmLink)) return false;

            level += 1;
            WriteList(stream, level, mmLink.FileReferences, WriteFileReference);
            WriteTagLine(stream, level, GEDCOMTagName.TITL, mmLink.Title, true);
            if (mmLink.IsPrimary) WriteTagLine(stream, level, GEDCOMTagName._PRIM, GEDCOMUtils.GetBoolStr(mmLink.IsPrimary), true);
            if (mmLink.IsPrimaryCutout) WriteTagLine(stream, level, GEDCOMTagName._PRIM_CUTOUT, GEDCOMUtils.GetBoolStr(mmLink.IsPrimaryCutout), true);
            WriteBaseTag(stream, level, mmLink.CutoutPosition);
            return true;
        }

        private static bool WriteNote(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            GDMNotes note = (GDMNotes)tag;
            if (note.IsPointer) {
                WriteTagValue(stream, level, note);
            } else {
                WriteText(stream, level, note);
            }
            WriteSubTags(stream, ++level, note);

            return true;
        }
        

        public static bool WriteSourceCitation(StreamWriter stream, int level, GDMTag tag)
        {
            if (tag.IsEmpty() && GEDCOMProvider.SkipEmptyTag(tag.Id)) return false;

            GDMSourceCitation sourCit = (GDMSourceCitation)tag;
            if (sourCit.IsPointer) {
                WriteTagValue(stream, level, sourCit);
            } else {
                WriteText(stream, level, sourCit);
            }
            level += 1;
            WriteSubTags(stream, level, sourCit);

            WriteTagLine(stream, level, GEDCOMTagName.PAGE, sourCit.Page, true);
            WriteTagLine(stream, level, GEDCOMTagName.QUAY, GEDCOMUtils.GetIntStr(sourCit.CertaintyAssessment), true);
            return true;
        }



        private static bool WriteAssociation(StreamWriter stream, int level, GDMTag tag)
        {
            GDMAssociation asso = (GDMAssociation)tag;

            if (!WriteBaseTag(stream, level, asso)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.RELA, asso.Relation, true);
            WriteList(stream, level, asso.SourceCitations, WriteSourceCitation);
            return true;
        }

        private static bool WriteAddress(StreamWriter stream, int level, GDMTag tag)
        {
            GDMAddress addr = (GDMAddress)tag;
            if (addr.IsEmpty() && GEDCOMProvider.SkipEmptyTag(addr.Id)) return false;

            WriteTagValue(stream, level, addr);

            int lev = level + 1;

            var strings = addr.Lines;
            int strCount = strings.Count;
            for (int i = 1; i < strCount; i++) {
                WriteTagLine(stream, lev, GEDCOMTagName.CONT, strings[i]);
            }

            WriteTagLine(stream, lev, GEDCOMTagName.ADR1, addr.AddressLine1, true);
            WriteTagLine(stream, lev, GEDCOMTagName.ADR2, addr.AddressLine2, true);
            WriteTagLine(stream, lev, GEDCOMTagName.ADR3, addr.AddressLine3, true);
            WriteTagLine(stream, lev, GEDCOMTagName.CITY, addr.AddressCity, true);
            WriteTagLine(stream, lev, GEDCOMTagName.STAE, addr.AddressState, true);
            WriteTagLine(stream, lev, GEDCOMTagName.CTRY, addr.AddressCountry, true);
            WriteTagLine(stream, lev, GEDCOMTagName.POST, addr.AddressPostalCode, true);

            WriteList(stream, level, addr.PhoneNumbers, WriteTagEx);
            WriteList(stream, level, addr.EmailAddresses, WriteTagEx);
            WriteList(stream, level, addr.FaxNumbers, WriteTagEx);
            WriteList(stream, level, addr.WebPages, WriteTagEx);
            return true;
        }

        private static bool WriteChildToFamilyLink(StreamWriter stream, int level, GDMTag tag)
        {
            GDMChildToFamilyLink cfl = (GDMChildToFamilyLink)tag;

            if (!WriteBaseTag(stream, level, cfl)) return false;

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagName.STAT, GEDCOMUtils.GetChildLinkageStatusStr(cfl.ChildLinkageStatus), true);
            WriteTagLine(stream, level, GEDCOMTagName.PEDI, GEDCOMUtils.GetPedigreeLinkageTypeStr(cfl.PedigreeLinkageType), true);
            return true;
        }

        

        public static bool WritePersonalName(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPersonalName persName = (GDMPersonalName)tag;

            if (!WriteBaseTag(stream, level, persName)) return false;

            int lev = level + 1;
            WriteTagLine(stream, lev, GEDCOMTagName.LANG, GEDCOMUtils.GetLanguageStr(persName.Language), true);
            WriteTagLine(stream, lev, GEDCOMTagName.TYPE, GEDCOMUtils.GetNameTypeStr(persName.NameType), true);
            WritePersonalNamePieces(stream, level, persName.Pieces); // same level
            return true;
        }

        private static void WritePersonalNamePieces(StreamWriter stream, int level, GDMTag tag)
        {
            GDMPersonalNamePieces persNamePieces = (GDMPersonalNamePieces)tag;

            int lev = level + 1;
            WriteSubTags(stream, lev, persNamePieces);
            WriteList(stream, level, persNamePieces.Notes, WriteNote);
            WriteList(stream, level, persNamePieces.SourceCitations, WriteSourceCitation);

            WriteTagLine(stream, lev, GEDCOMTagName.SURN, persNamePieces.Surname, true);
            WriteTagLine(stream, lev, GEDCOMTagName.GIVN, persNamePieces.Given, true);
            WriteTagLine(stream, lev, GEDCOMTagName._PATN, persNamePieces.PatronymicName, true);
            WriteTagLine(stream, lev, GEDCOMTagName.NPFX, persNamePieces.Prefix, true);
            WriteTagLine(stream, lev, GEDCOMTagName.NICK, persNamePieces.Nickname, true);
            WriteTagLine(stream, lev, GEDCOMTagName.SPFX, persNamePieces.SurnamePrefix, true);
            WriteTagLine(stream, lev, GEDCOMTagName.NSFX, persNamePieces.Suffix, true);
            WriteTagLine(stream, lev, GEDCOMTagName._MARN, persNamePieces.MarriedName, true);
            WriteTagLine(stream, lev, GEDCOMTagName._RELN, persNamePieces.ReligiousName, true);
            WriteTagLine(stream, lev, GEDCOMTagName._CENN, persNamePieces.CensusName, true);
        }

        #endregion

        #region Format variations

        /// <summary>
        /// Fix of errors that are in the dates of FamilyTreeBuilder.
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        public static string FixFTB(string str)
        {
            string result = str;
            string su = result.Substring(0, 3).ToUpperInvariant();

            if (su == GDMCustomDate.GEDCOMDateRangeArray[0] ||
                su == GDMCustomDate.GEDCOMDateRangeArray[1] ||
                su == GDMCustomDate.GEDCOMDateApproximatedArray[1] ||
                su == GDMCustomDate.GEDCOMDateApproximatedArray[2] ||
                su == GDMCustomDate.GEDCOMDateApproximatedArray[3])
            {
                result = result.Remove(0, 4);
            }
            return result;
        }

        /// <summary>
        /// Fix of line errors that are in the files of FamilyTreeBuilder.
        /// </summary>
        private static void FixFTBLine(GDMTag curRecord, GDMTag curTag, int lineNum, string str)
        {
            try {
                if (curTag != null) {
                    var tagType = curTag.GetTagType();

                    if (curTag is IGDMTextObject) {
                        str = " " + str;
                        StackTuple.AddTextTag(curTag, 0, (int)GEDCOMTagType.CONC, str);
                    } else if (tagType == GEDCOMTagType.CONT || tagType == GEDCOMTagType.CONC) {
                        curTag.StringValue += str;
                    } else {
                        StackTuple.AddBaseTag(curTag, 0, (int)GEDCOMTagType.NOTE, str);
                    }
                } else if (curRecord != null) {
                    if (curRecord is IGDMTextObject) {
                        str = " " + str;
                        StackTuple.AddTextTag(curRecord, 0, (int)GEDCOMTagType.CONC, str);
                    } else {
                        StackTuple.AddRecordTag(curRecord, 0, (int)GEDCOMTagType.NOTE, str);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("GEDCOMProvider.FixFTBLine(): Line " + lineNum.ToString() + " failed correct: " + ex.Message);
            }
        }

        public static GEDCOMFormat GetGEDCOMFormat(GDMTree tree)
        {
            if (tree != null) {
                string sour = tree.Header.Source.StringValue.Trim();

                int num = GEDCOMProvider.GEDCOMFormats.Length;
                for (int i = 1; i < num; i++) {
                    var appFmt = GEDCOMProvider.GEDCOMFormats[i];
                    if (string.Equals(appFmt.Sign, sour, StringComparison.Ordinal)) {
                        return appFmt.Format;
                    }
                }
            }

            return GEDCOMFormat.gf_Unknown;
        }

        public static GEDCOMAppFormat GetGEDCOMFormatProps(GEDCOMFormat format)
        {
            int num = GEDCOMProvider.GEDCOMFormats.Length;
            for (int i = 1; i < num; i++) {
                var appFmt = GEDCOMProvider.GEDCOMFormats[i];
                if (appFmt.Format == format) {
                    return appFmt;
                }
            }

            return GEDCOMProvider.GEDCOMFormats[0];
        }

        #endregion

        #region Tag properties
        public static GDMTag CreateTag(GDMObject owner, int tagId, string tagValue)
        {
            GEDCOMTagProps tagInfo = GEDCOMTagsTable.GetTagProps(tagId);
            if (tagInfo != null) {
                TagConstructor ctor = tagInfo.Constructor;
                return (ctor == null) ? new GDMTag(owner, tagId, tagValue) : ctor(owner, tagId, tagValue);
            }
            return null;
        }

        public static bool SkipEmptyTag(int tagId)
        {
            GEDCOMTagProps props = GEDCOMTagsTable.GetTagProps(tagId);
            return (props != null && props.SkipEmpty);
        }

        #endregion
    }
}
