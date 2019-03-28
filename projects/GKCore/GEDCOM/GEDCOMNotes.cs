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

using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMNotes : GEDCOMPointer
    {
        public StringList Notes
        {
            get {
                StringList notes;
                if (!IsPointer) {
                    notes = GetTagStrings(this);
                } else {
                    GEDCOMNoteRecord notesRecord = Value as GEDCOMNoteRecord;
                    notes = (notesRecord != null) ? notesRecord.Note : new StringList();
                }
                return notes;
            }
            set {
                Clear();
                SetTagStrings(this, value);
            }
        }


        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMNotes(owner, tagName, tagValue);
        }

        public GEDCOMNotes(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public GEDCOMNotes(GEDCOMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.NOTE);
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (string.IsNullOrEmpty(fStringValue) && Count == 0);
            }
            return result;
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : fStringValue;
            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fStringValue = result;
                result = string.Empty;
            } else {
                fStringValue = string.Empty;
            }
            return result;
        }
    }
}
