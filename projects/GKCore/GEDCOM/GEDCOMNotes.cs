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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMNotes : GEDCOMPointer
    {
        public bool IsPointer
        {
            get { return (!string.IsNullOrEmpty(XRef)); }
        }

        public StringList Notes
        {
            get { return GetNotes(); }
            set { SetNotes(value); }
        }

        private StringList GetNotes()
        {
            StringList notes;

            if (!IsPointer)
            {
                notes = GetTagStrings(this);
            }
            else
            {
                GEDCOMRecord notesRecord = Value;
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
            Clear();
            SetTagStrings(this, value);
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("NOTE");
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : fStringValue;
            return result;
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (fStringValue == "" && Count == 0);
            }
            return result;
        }

        public override string ParseString(string strValue)
        {
            fStringValue = "";
            XRef = "";
            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = base.ParseString(result);
                if (!IsPointer)
                {
                    fStringValue = result;
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
