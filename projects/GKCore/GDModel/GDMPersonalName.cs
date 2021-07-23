/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMNameType
    {
        ntNone,
        ntAka,
        ntBirth,
        ntImmigrant,
        ntMaiden,
        ntMarried
    }

    /**
     * Page 55 
     * NAME_PERSONAL := [ <NAME_TEXT> /<NAME_TEXT>/ <NAME_TEXT> ]
     * can be interpreted as:
     * NAME_PERSONAL := [ <name_prefix> <given_name> /<surname_prefix> <surname>/ <name_suffix> ]
     * 
     * name_prefix (+), given_name (+), surname_prefix (+), surname (+), name_suffix (+), nickname (separate)
     * 
     * William Lee (given name only or surname not known)
     * /Parry/ (surname only)
     * William Lee /Parry/
     * William Lee /Mac Parry/ (both parts (Mac and Parry) are surname parts
     * William /Lee/ Parry (surname imbedded in the name string)
     * William Lee /Pa.../
     * 
     * [ Lt. Cmndr. Joseph /Allen/ jr. ] - in this example "Lt. Cmndr." is considered as the name prefix portion.
     * [ Lt. Cmndr. Joseph /Allen/ jr. ] - in this example "jr." is considered as the name suffix portion.
     * [ /de la Cruz/ ] - in this example "de la" - surname prefix or article used in a family name.
     */

    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMPersonalName : GDMTag, IGDMStructWithNotes, IGDMStructWithSourceCitations
    {
        private string fFirstPart; // GIVN | GIVN + _PATN | NPFX + GIVN
        private string fSurname; // SURN | SPFX + SURN

        private GDMLanguageID fLanguage;
        private GDMNameType fNameType;
        private string fNameSuffix; // NSFX
        private string fNickname; // NICK
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;


        public string FullName
        {
            get {
                return GEDCOMUtils.GetFullName(fFirstPart, fSurname, fNameSuffix);
            }
        }

        public string FirstPart
        {
            get { return fFirstPart; }
            set { fFirstPart = GEDCOMUtils.Trim(value); }
        }

        public string Surname
        {
            get { return fSurname; }
            set { fSurname = GEDCOMUtils.Trim(value); }
        }

        public GDMLanguageID Language
        {
            get { return fLanguage; }
            set { fLanguage = value; }
        }

        public GDMNameType NameType
        {
            get { return fNameType; }
            set { fNameType = value; }
        }

        public string NameSuffix
        {
            get { return fNameSuffix; }
            set { fNameSuffix = GEDCOMUtils.Trim(value); }
        }

        public string Nickname
        {
            get { return fNickname; }
            set { fNickname = GEDCOMUtils.Trim(value); }
        }

        public bool HasNotes
        {
            get { return fNotes != null && fNotes.Count != 0; }
        }

        public GDMList<GDMNotes> Notes
        {
            get {
                if (fNotes == null) {
                    fNotes = new GDMList<GDMNotes>();
                }

                return fNotes;
            }
        }

        public bool HasSourceCitations
        {
            get { return fSourceCitations != null && fSourceCitations.Count != 0; }
        }


        public GDMList<GDMSourceCitation> SourceCitations
        {
            get {
                if (fSourceCitations == null) {
                    fSourceCitations = new GDMList<GDMSourceCitation>();
                }

                return fSourceCitations;
            }
        }


        #region Pieces

        private string fPieces_Prefix; // NPFX
        private string fPieces_Given; // GIVN
        private string fPieces_SurnamePrefix; // SPFX
        private string fPieces_Surname; // SURN

        private string fPieces_PatronymicName;
        private string fPieces_MarriedName;
        private string fPieces_ReligiousName;
        private string fPieces_CensusName;


        public string Pieces_Prefix
        {
            get { return fPieces_Prefix; }
            set { fPieces_Prefix = value; }
        }

        public string Pieces_Given
        {
            get { return fPieces_Given; }
            set { fPieces_Given = value; }
        }

        public string Pieces_SurnamePrefix
        {
            get { return fPieces_SurnamePrefix; }
            set { fPieces_SurnamePrefix = value; }
        }

        public string Pieces_Surname
        {
            get { return fPieces_Surname; }
            set { fPieces_Surname = value; }
        }

        public string Pieces_PatronymicName
        {
            get { return fPieces_PatronymicName; }
            set { fPieces_PatronymicName = value; }
        }

        public string Pieces_MarriedName
        {
            get { return fPieces_MarriedName; }
            set { fPieces_MarriedName = value; }
        }

        public string Pieces_ReligiousName
        {
            get { return fPieces_ReligiousName; }
            set { fPieces_ReligiousName = value; }
        }

        public string Pieces_CensusName
        {
            get { return fPieces_CensusName; }
            set { fPieces_CensusName = value; }
        }

        #endregion


        public GDMPersonalName()
        {
            SetName(GEDCOMTagType.NAME);

            fFirstPart = string.Empty;
            fSurname = string.Empty;
            fNameSuffix = string.Empty;

            fPieces_Prefix = string.Empty;
            fPieces_Given = string.Empty;
            fPieces_SurnamePrefix = string.Empty;
            fPieces_Surname = string.Empty;

            fNickname = string.Empty;

            fPieces_PatronymicName = string.Empty;
            fPieces_MarriedName = string.Empty;
            fPieces_ReligiousName = string.Empty;
            fPieces_CensusName = string.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fNotes != null) fNotes.Dispose();
                if (fSourceCitations != null) fSourceCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fNotes != null) fNotes.TrimExcess();
            if (fSourceCitations != null) fSourceCitations.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMPersonalName otherName = (source as GDMPersonalName);
            if (otherName == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherName);

            fFirstPart = otherName.fFirstPart;
            fSurname = otherName.fSurname;
            fNameSuffix = otherName.fNameSuffix;

            fLanguage = otherName.fLanguage;
            fNameType = otherName.fNameType;

            fPieces_Prefix = otherName.fPieces_Prefix;
            fPieces_Given = otherName.fPieces_Given;
            fPieces_SurnamePrefix = otherName.fPieces_SurnamePrefix;
            fPieces_Surname = otherName.fPieces_Surname;

            fNickname = otherName.fNickname;

            fPieces_PatronymicName = otherName.fPieces_PatronymicName;
            fPieces_MarriedName = otherName.fPieces_MarriedName;
            fPieces_ReligiousName = otherName.fPieces_ReligiousName;
            fPieces_CensusName = otherName.fPieces_CensusName;

            if (otherName.fNotes != null) AssignList(otherName.fNotes, Notes);
            if (otherName.fSourceCitations != null) AssignList(otherName.fSourceCitations, SourceCitations);
        }

        public override void Clear()
        {
            base.Clear();

            fFirstPart = string.Empty;
            fSurname = string.Empty;
            fNameSuffix = string.Empty;

            fLanguage = GDMLanguageID.Unknown;
            fNameType = GDMNameType.ntNone;

            fPieces_Prefix = string.Empty;
            fPieces_Given = string.Empty;
            fPieces_SurnamePrefix = string.Empty;
            fPieces_Surname = string.Empty;

            fNickname = string.Empty;

            fPieces_PatronymicName = string.Empty;
            fPieces_MarriedName = string.Empty;
            fPieces_ReligiousName = string.Empty;
            fPieces_CensusName = string.Empty;

            if (fNotes != null) fNotes.Clear();
            if (fSourceCitations != null) fSourceCitations.Clear();
        }

        private bool IsPiecesEmpty()
        {
            return 
                string.IsNullOrEmpty(fPieces_Prefix) && string.IsNullOrEmpty(fPieces_Given) &&
                string.IsNullOrEmpty(fPieces_SurnamePrefix) && string.IsNullOrEmpty(fPieces_Surname) &&
                string.IsNullOrEmpty(fNameSuffix) && string.IsNullOrEmpty(fNickname) &&
                string.IsNullOrEmpty(fPieces_PatronymicName) && string.IsNullOrEmpty(fPieces_MarriedName) &&
                string.IsNullOrEmpty(fPieces_ReligiousName) && string.IsNullOrEmpty(fPieces_CensusName);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty()
                && string.IsNullOrEmpty(fFirstPart) && string.IsNullOrEmpty(fSurname)
                && IsPiecesEmpty() && (fLanguage == GDMLanguageID.Unknown) && (fNameType == GDMNameType.ntNone)
                && (fNotes == null || fNotes.Count == 0)
                && (fSourceCitations == null || fSourceCitations.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (fNotes != null) fNotes.ReplaceXRefs(map);
            if (fSourceCitations != null) fSourceCitations.ReplaceXRefs(map);
        }

        protected override string GetStringValue()
        {
            return GEDCOMUtils.GetNameTagValue(this);
        }

        public override string ParseString(string strValue)
        {
            return GEDCOMUtils.ParseName(strValue, this);
        }

        public void SetNameParts(string firstPart, string surname, string lastPart)
        {
            fFirstPart = GEDCOMUtils.Trim(firstPart);
            fSurname = GEDCOMUtils.Trim(surname);
            fNameSuffix = GEDCOMUtils.Trim(lastPart);
        }

        private static bool IsUnknown(string str)
        {
            return string.Equals(str, "?") || (string.Compare(str, "unknown", true) == 0);
        }

        public float IsMatch(GDMPersonalName otherName, bool onlyFirstPart)
        {
            if (otherName == null)
                return 0.0f;

            int parts = 0;
            float matches = 0;
            bool surnameMatched = false;

            if (!(string.IsNullOrEmpty(otherName.FirstPart) && string.IsNullOrEmpty(fFirstPart))) {
                parts++;
                if (otherName.FirstPart == fFirstPart)
                    matches++;
            }

            if (!onlyFirstPart) {
                if (!(string.IsNullOrEmpty(otherName.Surname) && string.IsNullOrEmpty(fSurname))) {
                    if (IsUnknown(otherName.Surname) && IsUnknown(fSurname)) {
                        // not really matched, surname isn't known,
                        // don't count as part being checked, and don't penalize
                        surnameMatched = true;
                    } else {
                        parts++;
                        if (otherName.Surname == fSurname) {
                            matches++;
                            surnameMatched = true;
                        }
                    }
                } else {
                    // pretend the surname matches
                    surnameMatched = true;
                }
            } else {
                // pretend the surname matches
                surnameMatched = true;
            }

            if (!string.IsNullOrEmpty(otherName.Nickname) && !string.IsNullOrEmpty(Nickname)) {
                parts++;
                if (otherName.Nickname == Nickname)
                    matches++;
            }

            float match = (parts == 0) ? 0.0f : (matches / parts) * 100.0f;

            // heavily penalise the surname not matching
            // for this to work correctly better matching needs to be
            // performed, not just string comparison
            if (!onlyFirstPart && !surnameMatched) {
                match *= 0.25f;
            }

            return match;
        }
    }
}
