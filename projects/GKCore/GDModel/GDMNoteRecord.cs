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
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMNoteRecord : GDMRecord, IGDMTextObject
    {
        private readonly GDMLines fLines;

        public GDMLines Lines
        {
            get { return fLines; }
        }


        public GDMNoteRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.NOTE);

            fLines = new GDMLines();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fLines.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMNoteRecord sourceObj = (source as GDMNoteRecord);
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
            return base.IsEmpty() && fLines.IsEmpty();
        }

        protected override string GetStringValue()
        {
            return string.Empty;
        }

        public override string ParseString(string strValue)
        {
            fLines.Clear();
            if (!string.IsNullOrEmpty(strValue)) {
                fLines.Add(strValue);
            }
            return string.Empty;
        }

        /// <summary>
        /// The MoveTo() merges records and their references, but does not change the text in the target.
        /// </summary>
        /// <param name="targetRecord"></param>
        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMNoteRecord targetNote = (targetRecord as GDMNoteRecord);
            if (targetNote == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            string targetText = targetNote.Lines.Text;
            base.MoveTo(targetRecord);
            targetNote.Lines.Text = targetText;
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMNoteRecord note = tag as GDMNoteRecord;
            if (note == null) return 0.0f;

            float match = 0.0f;

            if (string.Compare(fLines.Text, note.Lines.Text, true) == 0) {
                match = 100.0f;
            }

            return match;
        }

        public void SetNotesArray(params string[] value)
        {
            fLines.Clear();
            fLines.AddRange(value);
        }

        public void AddNoteText(string text)
        {
            fLines.Add(text);
        }

        public void SetNoteText(string text)
        {
            if (text == null)
                throw new ArgumentNullException("text");

            fLines.Text = text;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fLines);
        }
    }
}
