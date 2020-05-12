﻿/*
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
using BSLib;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMTextTag : GDMTag, IGDMTextObject
    {
        private StringList fLines;

        public StringList Lines
        {
            get { return fLines; }
        }


        public GDMTextTag(GDMObject owner) : base(owner)
        {
            fLines = new StringList();
        }

        public GDMTextTag(GDMObject owner, int tagId) : this(owner)
        {
            SetName(tagId);
        }

        public override void Assign(GDMTag source)
        {
            GDMTextTag sourceObj = (source as GDMTextTag);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fLines.Assign(sourceObj.fLines);
        }

        public override void Clear()
        {
            base.Clear();
            fLines.Clear();
        }

        public override bool IsEmpty()
        {
            return fLines.IsEmpty();
        }

        protected override string GetStringValue()
        {
            return fLines.Text;
        }

        public override string ParseString(string strValue)
        {
            fLines.Clear();
            fLines.Add(strValue);
            return string.Empty;
        }
    }


    public sealed class GDMNotes : GDMPointer, IGDMTextObject
    {
        private StringList fLines;

        public StringList Lines
        {
            get {
                StringList lines;
                if (!IsPointer) {
                    lines = fLines;
                } else {
                    GDMNoteRecord notesRecord = Value as GDMNoteRecord;
                    lines = (notesRecord != null) ? notesRecord.Lines : new StringList();
                }
                return lines;
            }
        }


        public GDMNotes(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.NOTE);

            fLines = new StringList();
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (fLines.IsEmpty() && SubTags.Count == 0);
            }
            return result;
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : ((fLines.Count > 0) ? fLines[0] : string.Empty);
            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fLines.Clear();
                if (!string.IsNullOrEmpty(result)) {
                    fLines.Add(result);
                }
                result = string.Empty;
            } else {
                fStringValue = string.Empty;
            }
            return result;
        }
    }
}
