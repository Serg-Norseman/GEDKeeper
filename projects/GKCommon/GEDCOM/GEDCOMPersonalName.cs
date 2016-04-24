/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.IO;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMPersonalName : GEDCOMTag
    {
        private GEDCOMPersonalNamePieces fPieces;

        public string FullName
        {
            get { return this.GetFullName(); }
        }

        public string FirstPart
        {
            get { return this.GetFirstPart(); }
        }

        public string Surname
        {
            get { return this.GetSurname(); }
            set { this.SetSurname(value); }
        }

        public string LastPart
        {
            get { return this.GetLastPart(); }
        }

        public GEDCOMPersonalNamePieces Pieces
        {
            get { return this.fPieces; }
        }

        public GEDCOMNameType NameType
        {
            get { return GEDCOMUtils.GetNameTypeVal(base.GetTagStringValue("TYPE")); }
            set { base.SetTagStringValue("TYPE", GEDCOMUtils.GetNameTypeStr(value)); }
        }

        public void GetNameParts(out string firstPart, out string surname/*, out string ALastPart*/)
        {
            string sv = base.StringValue;

            if (string.IsNullOrEmpty(sv)) {
                firstPart = "";
                surname = "";
            } else {
                int p = sv.IndexOf('/');

                if (p < 0) {
                    firstPart = "";
                } else {
                    firstPart = sv.Substring(0, p);
                    firstPart = GEDCOMUtils.TrimRight(firstPart);
                }

                int p2 = ((p < 0) ? -1 : sv.IndexOf('/', p + 1));

                if (p < 0 || p2 < 0) {
                    surname = "";
                } else {
                    p++;
                    surname = sv.Substring(p, p2 - p);
                }
            }

            //ALastPart = GetLastPart();
        }

        public void GetRusNameParts(out string surname, out string name, out string patronymic)
        {
            string firstPart /*, dummy*/;
            this.GetNameParts(out firstPart, out surname /*, out dummy*/);

            string[] parts = firstPart.Split(' ');
            if (parts.Length > 1)
            {
                name = parts[0];
                patronymic = parts[1];
            } else {
                name = firstPart;
                patronymic = "";
            }
        }

        public void SetNameParts(string firstPart, string surname, string lastPart)
        {
            base.StringValue = GEDCOMUtils.TrimLeft(firstPart + " ") + "/" + surname + "/" + GEDCOMUtils.TrimRight(" " + lastPart);
        }

        private string GetFirstPart()
        {
            string result;

            string sv = base.StringValue;
            if (string.IsNullOrEmpty(sv)) {
                result = "";
            } else {
                int p = sv.IndexOf('/');
                if (p < 0) {
                    result = "";
                } else {
                    result = sv.Substring(0, p);
                    result = GEDCOMUtils.TrimRight(result);
                }
            }

            return result;
        }

        private string GetSurname()
        {
            string result;

            string sv = base.StringValue;
            if (string.IsNullOrEmpty(sv)) {
                result = "";
            } else {
                int p = sv.IndexOf('/');
                int p2 = ((p < 0) ? -1 : sv.IndexOf('/', p + 1));

                if (p < 0 || p2 < 0) {
                    result = "";
                } else {
                    p++;
                    result = sv.Substring(p, p2 - p);
                }
            }

            return result;
        }

        private string GetLastPart()
        {
            string result = "";

            string sv = base.StringValue;
            int p = sv.IndexOf('/');
            if (p >= 0)
            {
                p = sv.IndexOf('/', p + 1);
                if (p >= 0)
                {
                    result = GEDCOMUtils.TrimLeft(sv.Substring(p + 1));
                }
            }

            return result;
        }

        private string GetFullName()
        {
            string result = base.StringValue;
            while (result.IndexOf('/') >= 0)
            {
                result = result.Remove(result.IndexOf('/'), 1);
            }
            return result;
        }

        private void SetSurname(string value)
        {
            base.StringValue = string.Concat(new string[]
                                             {
                                                 GEDCOMUtils.TrimLeft(this.FirstPart + " "), "/", value, "/", GEDCOMUtils.TrimRight(" " + this.LastPart)
                                             });
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.SetName("NAME");

            this.fPieces = new GEDCOMPersonalNamePieces(owner, this, "", "");
            this.fPieces.SetLevel(base.Level);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fPieces.Dispose();
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
                result = this.fPieces.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Assign(GEDCOMTag source)
        {
            base.Assign(source);

            if (source is GEDCOMPersonalName)
            {
                this.fPieces.Assign((source as GEDCOMPersonalName).Pieces);
            }
        }

        public override void Clear()
        {
            base.Clear();
            if (this.fPieces != null) this.fPieces.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fPieces.IsEmpty();
        }

        public override void Pack()
        {
            base.Pack();
            this.fPieces.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            this.fPieces.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            this.fPieces.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            this.fPieces.SaveToStream(stream);
        }

        public float IsMatch(GEDCOMPersonalName name)
        {
            if (name == null) return 0.0f;

            float match = 0.0f;
            
            int parts = 0;
            float matches = 0;
            bool surnameMatched = false;

            if (!(string.IsNullOrEmpty(name.FirstPart) && string.IsNullOrEmpty(FirstPart)))
            {
                parts++;
                if (name.FirstPart == FirstPart) matches++;
            }

            if (!(string.IsNullOrEmpty(name.Surname) && string.IsNullOrEmpty(Surname)))
            {
                if ((name.Surname == "?" && Surname == "?") ||
                    ((string.Compare(name.Surname, "unknown", true) == 0) &&
                     (string.Compare(Surname, "unknown", true) == 0)))
                {
                    // not really matched, surname isn't known,
                    // don't count as part being checked, and don't penalize
                    surnameMatched = true;
                }
                else
                {
                    parts++;
                    if (name.Surname == Surname) {
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

            if (!(string.IsNullOrEmpty(name.Pieces.Nickname) && string.IsNullOrEmpty(Pieces.Nickname)))
            {
                parts++;
                if (name.Pieces.Nickname == Pieces.Nickname) matches++;
            }

            match = (matches / parts) * 100.0F;

            // heavily penalise the surname not matching
            // for this to work correctly better matching needs to be
            // performed, not just string comparison
            if (!surnameMatched) match *= 0.25F;

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
