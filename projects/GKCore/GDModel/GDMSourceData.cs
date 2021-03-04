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

using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMSourceData : GDMTag, IGDMStructWithNotes
    {
        private string fAgency;
        private GDMList<GDMSourceEvent> fEvents;
        private GDMList<GDMNotes> fNotes;


        public string Agency
        {
            get { return fAgency; }
            set { fAgency = value; }
        }

        public GDMList<GDMSourceEvent> Events
        {
            get { return fEvents; }
        }

        public GDMList<GDMNotes> Notes
        {
            get { return fNotes; }
        }


        public GDMSourceData(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.DATA);

            fAgency = string.Empty;
            fEvents = new GDMList<GDMSourceEvent>(this);
            fNotes = new GDMList<GDMNotes>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fEvents.Dispose();
                fNotes.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            base.Clear();
            fAgency = string.Empty;
            fEvents.Clear();
            fNotes.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fEvents.Count == 0) && string.IsNullOrEmpty(fAgency) && (fNotes.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fEvents.ReplaceXRefs(map);
            fNotes.ReplaceXRefs(map);
        }

        public GDMNotes AddNote(GDMNoteRecord noteRec)
        {
            GDMNotes note = null;
            
            if (noteRec != null) {
                note = new GDMNotes(this);
                note.Value = noteRec;
                fNotes.Add(note);
            }
            
            return note;
        }
    }
}
