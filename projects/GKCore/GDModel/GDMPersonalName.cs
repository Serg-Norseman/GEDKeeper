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

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMNameType
    {
        ntNone,
        ntAdoption, // FIXME: user defined name types (gh#550)
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
        private string fGiven; // GIVN
        private GDMLanguageID fLanguage;
        private string fNamePrefix; // NPFX
        private string fNameSuffix; // NSFX
        private GDMNameType fNameType;
        private string fNickname; // NICK
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;
        private string fSurnamePrefix; // SPFX
        private string fSurname; // SURN

        private string fPatronymicName;
        private string fMarriedName;
        private string fReligiousName;
        private string fCensusName;


        public string FullName
        {
            get {
                return GEDCOMUtils.GetFullName(FirstPart, fSurname, fNameSuffix);
            }
        }

        internal string FirstPart
        {
            get {
                string result = fGiven;
                if (!string.IsNullOrEmpty(fPatronymicName)) {
                    if (!string.IsNullOrEmpty(result)) {
                        result += " ";
                    }
                    result += fPatronymicName;
                }
                return result;
            }
        }

        public string Given
        {
            get { return fGiven; }
            set { fGiven = GEDCOMUtils.Trim(value); }
        }

        public GDMLanguageID Language
        {
            get { return fLanguage; }
            set { fLanguage = value; }
        }

        public string NamePrefix
        {
            get { return fNamePrefix; }
            set { fNamePrefix = GEDCOMUtils.Trim(value); }
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

        public string SurnamePrefix
        {
            get { return fSurnamePrefix; }
            set { fSurnamePrefix = GEDCOMUtils.Trim(value); }
        }

        public string Surname
        {
            get { return fSurname; }
            set { fSurname = GEDCOMUtils.Trim(value); }
        }


        public string PatronymicName
        {
            get { return fPatronymicName; }
            set { fPatronymicName = GEDCOMUtils.Trim(value); }
        }

        public string MarriedName
        {
            get { return fMarriedName; }
            set { fMarriedName = GEDCOMUtils.Trim(value); }
        }

        public string ReligiousName
        {
            get { return fReligiousName; }
            set { fReligiousName = GEDCOMUtils.Trim(value); }
        }

        public string CensusName
        {
            get { return fCensusName; }
            set { fCensusName = GEDCOMUtils.Trim(value); }
        }


        public GDMPersonalName()
        {
            SetName(GEDCOMTagType.NAME);

            fNamePrefix = string.Empty;
            fGiven = string.Empty;
            fSurnamePrefix = string.Empty;
            fSurname = string.Empty;
            fNameSuffix = string.Empty;
            fNickname = string.Empty;

            fPatronymicName = string.Empty;
            fMarriedName = string.Empty;
            fReligiousName = string.Empty;
            fCensusName = string.Empty;
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

            fNamePrefix = otherName.fNamePrefix;
            fGiven = otherName.fGiven;
            fSurnamePrefix = otherName.fSurnamePrefix;
            fSurname = otherName.fSurname;
            fNameSuffix = otherName.fNameSuffix;
            fNickname = otherName.fNickname;

            fPatronymicName = otherName.fPatronymicName;
            fMarriedName = otherName.fMarriedName;
            fReligiousName = otherName.fReligiousName;
            fCensusName = otherName.fCensusName;

            fLanguage = otherName.fLanguage;
            fNameType = otherName.fNameType;

            if (otherName.fNotes != null) AssignList(otherName.fNotes, Notes);
            if (otherName.fSourceCitations != null) AssignList(otherName.fSourceCitations, SourceCitations);
        }

        public override void Clear()
        {
            base.Clear();

            fNamePrefix = string.Empty;
            fGiven = string.Empty;
            fSurnamePrefix = string.Empty;
            fSurname = string.Empty;
            fNameSuffix = string.Empty;
            fNickname = string.Empty;

            fPatronymicName = string.Empty;
            fMarriedName = string.Empty;
            fReligiousName = string.Empty;
            fCensusName = string.Empty;

            fLanguage = GDMLanguageID.Unknown;
            fNameType = GDMNameType.ntNone;

            if (fNotes != null) fNotes.Clear();
            if (fSourceCitations != null) fSourceCitations.Clear();
        }

        private bool IsPiecesEmpty()
        {
            return 
                string.IsNullOrEmpty(fNamePrefix) && string.IsNullOrEmpty(fGiven) &&
                string.IsNullOrEmpty(fSurnamePrefix) && string.IsNullOrEmpty(fSurname) &&
                string.IsNullOrEmpty(fNameSuffix) && string.IsNullOrEmpty(fNickname) &&
                string.IsNullOrEmpty(fPatronymicName) && string.IsNullOrEmpty(fMarriedName) &&
                string.IsNullOrEmpty(fReligiousName) && string.IsNullOrEmpty(fCensusName);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && IsPiecesEmpty() &&
                (fLanguage == GDMLanguageID.Unknown) && (fNameType == GDMNameType.ntNone)
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

            if (!(string.IsNullOrEmpty(otherName.FirstPart) && string.IsNullOrEmpty(FirstPart))) {
                parts++;
                if (otherName.FirstPart == FirstPart)
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

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fGiven);
            hashCode.Add(fLanguage);
            hashCode.Add(fNamePrefix);
            hashCode.Add(fNameSuffix);
            hashCode.Add(fNameType);
            hashCode.Add(fNickname);
            ProcessHashes(ref hashCode, fNotes);
            ProcessHashes(ref hashCode, fSourceCitations);
            hashCode.Add(fSurnamePrefix);
            hashCode.Add(fSurname);
            hashCode.Add(fPatronymicName);
            hashCode.Add(fMarriedName);
            hashCode.Add(fReligiousName);
            hashCode.Add(fCensusName);
        }
    }
}
