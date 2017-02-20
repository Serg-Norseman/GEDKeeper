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

using System;
using System.IO;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMPersonalName : GEDCOMTag
    {
        private string fFirstPart;
        private string fSurname;
        private string fLastPart;
        private GEDCOMPersonalNamePieces fPieces;

        public string FullName
        {
            get {
                string result = GEDCOMUtils.TrimLeft(fFirstPart);
                if (!string.IsNullOrEmpty(fSurname)) {
                    result += " " + fSurname;

                    if (!string.IsNullOrEmpty(fLastPart)) {
                        result += GEDCOMUtils.TrimRight(" " + fLastPart);
                    }
                }

                return result;
            }
        }

        public string FirstPart
        {
            get { return fFirstPart; }
            set { fFirstPart = value; }
        }

        public string Surname
        {
            get { return fSurname; }
            set { fSurname = value; }
        }

        public string LastPart
        {
            get { return fLastPart; }
            set { fLastPart = value; }
        }

        public GEDCOMPersonalNamePieces Pieces
        {
            get { return fPieces; }
        }

        public GEDCOMNameType NameType
        {
            get { return GEDCOMUtils.GetNameTypeVal(GetTagStringValue("TYPE")); }
            set { SetTagStringValue("TYPE", GEDCOMUtils.GetNameTypeStr(value)); }
        }

        protected override string GetStringValue()
        {
            // see "THE GEDCOM STANDARD Release 5.5.1", p.54 ("NAME_PERSONAL")

            string result = GEDCOMUtils.TrimLeft(fFirstPart);
            if (!string.IsNullOrEmpty(fSurname)) {
                result += " /" + fSurname + "/";

                if (!string.IsNullOrEmpty(fLastPart)) {
                    result += GEDCOMUtils.TrimRight(" " + fLastPart);
                }
            }
            return result;
        }

        public override string ParseString(string strValue)
        {
            fFirstPart = "";
            fSurname = "";
            fLastPart = "";

            string sv = strValue;
            if (string.IsNullOrEmpty(sv)) return string.Empty;

            int p = sv.IndexOf('/');
            if (p < 0) {
                fFirstPart = sv;
                return string.Empty;
            }

            fFirstPart = sv.Substring(0, p);
            fFirstPart = GEDCOMUtils.TrimRight(fFirstPart);

            int p2 = ((p < 0) ? -1 : sv.IndexOf('/', p + 1));
            if (p2 < 0) return string.Empty;

            p++;
            fSurname = sv.Substring(p, p2 - p);

            if (p2 >= sv.Length - 1) return string.Empty;

            fLastPart = GEDCOMUtils.TrimLeft(sv.Substring(p2 + 1));
            return string.Empty;
        }

        public void SetNameParts(string firstPart, string surname, string lastPart)
        {
            fFirstPart = firstPart.Trim();
            fSurname = surname.Trim();
            fLastPart = lastPart.Trim();
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("NAME");

            fPieces = new GEDCOMPersonalNamePieces(owner, this, "", "");
            fPieces.SetLevel(Level);

            fFirstPart = "";
            fSurname = "";
            fLastPart = "";
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fPieces.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "TYPE" || tagName == "FONE" || tagName == "ROMN")
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }
            else
            {
                result = fPieces.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMPersonalName otherName = (source as GEDCOMPersonalName);
            if (otherName == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherName);

            fFirstPart = otherName.fFirstPart;
            fSurname = otherName.fSurname;
            fLastPart = otherName.fLastPart;

            fPieces.Assign(otherName.Pieces);
        }

        public override void Clear()
        {
            base.Clear();

            fFirstPart = "";
            fSurname = "";
            fLastPart = "";

            if (fPieces != null) fPieces.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty()
                && string.IsNullOrEmpty(fFirstPart) && string.IsNullOrEmpty(fSurname) && string.IsNullOrEmpty(fLastPart)
                && fPieces.IsEmpty();
        }

        public override void Pack()
        {
            base.Pack();
            fPieces.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fPieces.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            fPieces.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            fPieces.SaveToStream(stream);
        }

        public float IsMatch(GEDCOMPersonalName otherName, bool onlyFirstPart)
        {
            if (otherName == null) return 0.0f;

            int parts = 0;
            float matches = 0;
            bool surnameMatched = false;

            if (!(string.IsNullOrEmpty(otherName.FirstPart) && string.IsNullOrEmpty(fFirstPart)))
            {
                parts++;
                if (otherName.FirstPart == fFirstPart) matches++;
            }

            if (!(string.IsNullOrEmpty(otherName.Surname) && string.IsNullOrEmpty(fSurname)))
            {
                if ((otherName.Surname == "?" && fSurname == "?") ||
                    ((string.Compare(otherName.Surname, "unknown", true) == 0) &&
                     (string.Compare(fSurname, "unknown", true) == 0)))
                {
                    // not really matched, surname isn't known,
                    // don't count as part being checked, and don't penalize
                    surnameMatched = true;
                }
                else
                {
                    parts++;
                    if (otherName.Surname == fSurname) {
                        matches++;
                        surnameMatched = true;
                    }
                }
            }
            else
            {
                // pretend the surname matches
                surnameMatched = true;
            }

            if (!(string.IsNullOrEmpty(otherName.Pieces.Nickname) && string.IsNullOrEmpty(fPieces.Nickname)))
            {
                parts++;
                if (otherName.Pieces.Nickname == fPieces.Nickname) matches++;
            }

            float match = (parts == 0) ? 0.0f : (matches / parts) * 100.0f;

            // heavily penalise the surname not matching
            // for this to work correctly better matching needs to be
            // performed, not just string comparison
            if (!surnameMatched) match *= 0.25f;

            return match;
        }

        public GEDCOMPersonalName(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMPersonalName(owner, parent, tagName, tagValue);
        }
    }
}
