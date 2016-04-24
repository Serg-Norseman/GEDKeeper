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

using System;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMNoteRecord : GEDCOMRecord
    {
        public StringList Note
        {
            get { return base.GetTagStrings(this); }
            set { base.SetTagStrings(this, value); }
        }

        public void SetNotesArray(params string[] value)
        {
            base.SetTagStrings(this, value);
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtNote);
            base.SetName("NOTE");
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMNoteRecord targetNote = (targetRecord as GEDCOMNoteRecord);
            if (targetNote == null)
            {
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");
            }

            StringList cont = new StringList();
            try
            {
                cont.Text = targetNote.Note.Text;
                base.MoveTo(targetRecord, clearDest);
                targetNote.Note = cont;
            }
            finally
            {
                cont.Dispose();
            }
        }

        public GEDCOMNoteRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMNoteRecord(owner, parent, tagName, tagValue);
        }

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMNoteRecord note = tag as GEDCOMNoteRecord;
            if (note == null) return 0.0f;
            
            float match = 0.0f;

            if (string.Compare(this.Note.Text, note.Note.Text, true) == 0) {
                match = 100.0f;
            }

            return match;
        }

        #region Auxiliary

        public void AddNoteText(string text)
        {
            StringList strData = new StringList();
            try
            {
                strData.Text = this.Note.Text.Trim();
                strData.Add(text);
                this.Note = strData;
            }
            finally
            {
                strData.Dispose();
            }
        }

        #endregion
    }
}
