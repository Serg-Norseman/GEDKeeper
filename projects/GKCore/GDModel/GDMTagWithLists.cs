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

namespace GDModel
{
    public class GDMTagWithLists : GDMTag, IGEDCOMStructWithLists
    {
        protected GDMList<GDMNotes> fNotes;
        protected GDMList<GDMSourceCitation> fSourceCitations;
        protected GDMList<GDMMultimediaLink> fMultimediaLinks;


        public GDMList<GDMNotes> Notes
        {
            get { return fNotes; }
        }

        public GDMList<GDMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }

        public GDMList<GDMMultimediaLink> MultimediaLinks
        {
            get { return fMultimediaLinks; }
        }


        protected GDMTagWithLists(GDMObject owner) : base(owner)
        {
            fNotes = new GDMList<GDMNotes>(this);
            fSourceCitations = new GDMList<GDMSourceCitation>(this);
            fMultimediaLinks = new GDMList<GDMMultimediaLink>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fNotes.Dispose();
                fSourceCitations.Dispose();
                fMultimediaLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fNotes.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
            fMultimediaLinks.ReplaceXRefs(map);
        }

        public override void Clear()
        {
            base.Clear();

            fNotes.Clear();
            fSourceCitations.Clear();
            fMultimediaLinks.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fNotes.Count == 0 && fSourceCitations.Count == 0 && fMultimediaLinks.Count == 0;
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

        public GDMSourceCitation AddSource(GDMSourceRecord sourceRec, string page, int quality)
        {
            GDMSourceCitation cit = null;
            
            if (sourceRec != null) {
                cit = new GDMSourceCitation(this);
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                fSourceCitations.Add(cit);
            }
            
            return cit;
        }

        public GDMMultimediaLink AddMultimedia(GDMMultimediaRecord mediaRec)
        {
            GDMMultimediaLink result = null;

            if (mediaRec != null) {
                result = new GDMMultimediaLink(this);
                result.Value = mediaRec;
                fMultimediaLinks.Add(result);
            }

            return result;
        }
    }
}
