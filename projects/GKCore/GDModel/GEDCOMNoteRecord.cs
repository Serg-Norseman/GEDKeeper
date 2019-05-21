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

using System;
using BSLib;
using GKCore.Types;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMNoteRecord : GEDCOMRecord
    {
        public StringList Note
        {
            get { return GetTagStrings(this); }
            set { SetTagStrings(this, value); }
        }

        public GEDCOMNoteRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtNote);
            SetName(GEDCOMTagType.NOTE);
        }

        /// <summary>
        /// The MoveTo() merges records and their references, but does not change the text in the target.
        /// </summary>
        /// <param name="targetRecord"></param>
        /// <param name="clearDest"></param>
        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMNoteRecord targetNote = (targetRecord as GEDCOMNoteRecord);
            if (targetNote == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            using (StringList cont = new StringList()) {
                cont.Text = targetNote.Note.Text;
                base.MoveTo(targetRecord, clearDest);
                targetNote.Note = cont;
            }
        }

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMNoteRecord note = tag as GEDCOMNoteRecord;
            if (note == null) return 0.0f;

            float match = 0.0f;

            if (string.Compare(Note.Text, note.Note.Text, true) == 0) {
                match = 100.0f;
            }

            return match;
        }

        public void SetNotesArray(params string[] value)
        {
            SetTagStrings(this, value);
        }

        public void AddNoteText(string text)
        {
            using (StringList strData = new StringList()) {
                strData.Text = Note.Text.Trim();
                strData.Add(text);
                Note = strData;
            }
        }

        public void SetNoteText(string text)
        {
            if (text == null)
                throw new ArgumentNullException("text");

            using (StringList strData = new StringList(text)) {
                Note = strData;
            }
        }
    }
}
