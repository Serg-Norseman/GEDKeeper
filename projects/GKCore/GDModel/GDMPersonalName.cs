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


    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMPersonalName : GDMTag, IGDMStructWithNotes, IGDMStructWithSourceCitations
    {
        private string fFirstPart;
        private string fSurname;
        private string fLastPart;

        private GDMLanguageID fLanguage;
        private GDMNameType fNameType;
        private GDMPersonalNamePieces fPieces;
        private GDMList<GDMNotes> fNotes;
        private GDMList<GDMSourceCitation> fSourceCitations;


        public string FullName
        {
            get {
                return GEDCOMUtils.GetFullName(fFirstPart, fSurname, fLastPart);
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

        public string LastPart
        {
            get { return fLastPart; }
            set { fLastPart = GEDCOMUtils.Trim(value); }
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

        public GDMPersonalNamePieces Pieces
        {
            get { return fPieces; }
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


        public GDMPersonalName()
        {
            SetName(GEDCOMTagType.NAME);

            fFirstPart = string.Empty;
            fSurname = string.Empty;
            fLastPart = string.Empty;

            fPieces = new GDMPersonalNamePieces();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fPieces.Dispose();
                if (fNotes != null) fNotes.Dispose();
                if (fSourceCitations != null) fSourceCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fPieces.TrimExcess();
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
            fLastPart = otherName.fLastPart;

            fLanguage = otherName.fLanguage;
            fNameType = otherName.fNameType;

            fPieces.Assign(otherName.Pieces);
            if (otherName.fNotes != null) AssignList(otherName.fNotes, Notes);
            if (otherName.fSourceCitations != null) AssignList(otherName.fSourceCitations, SourceCitations);
        }

        public override void Clear()
        {
            base.Clear();

            fFirstPart = string.Empty;
            fSurname = string.Empty;
            fLastPart = string.Empty;

            fLanguage = GDMLanguageID.Unknown;
            fNameType = GDMNameType.ntNone;

            fPieces.Clear();
            if (fNotes != null) fNotes.Clear();
            if (fSourceCitations != null) fSourceCitations.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty()
                && string.IsNullOrEmpty(fFirstPart) && string.IsNullOrEmpty(fSurname) && string.IsNullOrEmpty(fLastPart)
                && fPieces.IsEmpty() && (fLanguage == GDMLanguageID.Unknown) && (fNameType == GDMNameType.ntNone)
                && (fNotes == null || fNotes.Count == 0)
                && (fSourceCitations == null || fSourceCitations.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fPieces.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
            if (fSourceCitations != null) fSourceCitations.ReplaceXRefs(map);
        }

        protected override string GetStringValue()
        {
            return GEDCOMUtils.GetNameTagValue(fFirstPart, fSurname, fLastPart);
        }

        public override string ParseString(string strValue)
        {
            return GEDCOMUtils.ParseName(strValue, this);
        }

        public void SetNameParts(string firstPart, string surname, string lastPart)
        {
            fFirstPart = GEDCOMUtils.Trim(firstPart);
            fSurname = GEDCOMUtils.Trim(surname);
            fLastPart = GEDCOMUtils.Trim(lastPart);
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

            if (!string.IsNullOrEmpty(otherName.Pieces.Nickname) && !string.IsNullOrEmpty(fPieces.Nickname)) {
                parts++;
                if (otherName.Pieces.Nickname == fPieces.Nickname)
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
