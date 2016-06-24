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


namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMNotes : GEDCOMPointer
    {
        public bool IsPointer
        {
            get { return (!string.IsNullOrEmpty(base.XRef)); }
        }

        public StringList Notes
        {
            get { return this.GetNotes(); }
            set { this.SetNotes(value); }
        }

        private StringList GetNotes()
        {
            StringList notes;

            if (!this.IsPointer)
            {
                notes = base.GetTagStrings(this);
            }
            else
            {
                GEDCOMRecord notesRecord = base.Value;
                if (notesRecord is GEDCOMNoteRecord) {
                    notes = ((notesRecord as GEDCOMNoteRecord).Note);
                } else {
                    notes = new StringList();
                }
            }

            return notes;
        }

        private void SetNotes(StringList value)
        {
            this.Clear();
            base.SetTagStrings(this, value);
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.SetName("NOTE");
        }

        protected override string GetStringValue()
        {
            string result = this.IsPointer ? base.GetStringValue() : this.fStringValue;
            return result;
        }

        public override bool IsEmpty()
        {
            bool result;
            if (this.IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (this.fStringValue == "" && base.Count == 0);
            }
            return result;
        }

        public override string ParseString(string strValue)
        {
            this.fStringValue = "";
            base.XRef = "";
            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = base.ParseString(result);
                if (!this.IsPointer)
                {
                    this.fStringValue = result;
                    result = "";
                }
            }
            return result;
        }

        public GEDCOMNotes(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMNotes(owner, parent, tagName, tagValue);
        }
    }
}
