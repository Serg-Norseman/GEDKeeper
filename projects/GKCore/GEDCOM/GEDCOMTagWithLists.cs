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

using System.IO;

namespace GKCommon.GEDCOM
{
    public class GEDCOMTagWithLists : GEDCOMTag, IGEDCOMStructWithLists
    {
        protected GEDCOMList<GEDCOMNotes> fNotes;
        protected GEDCOMList<GEDCOMSourceCitation> fSourceCitations;
        protected GEDCOMList<GEDCOMMultimediaLink> fMultimediaLinks;

        public GEDCOMList<GEDCOMNotes> Notes
        {
            get { return fNotes; }
        }

        public GEDCOMList<GEDCOMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }

        public GEDCOMList<GEDCOMMultimediaLink> MultimediaLinks
        {
            get { return fMultimediaLinks; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);

            fNotes = new GEDCOMList<GEDCOMNotes>(this);
            fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
            fMultimediaLinks = new GEDCOMList<GEDCOMMultimediaLink>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fNotes.Dispose();
                fSourceCitations.Dispose();
                fMultimediaLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Pack()
        {
            base.Pack();

            fNotes.Pack();
            fSourceCitations.Pack();
            fMultimediaLinks.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fNotes.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
            fMultimediaLinks.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);

            fNotes.ResetOwner(newOwner);
            fSourceCitations.ResetOwner(newOwner);
            fMultimediaLinks.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            fNotes.SaveToStream(stream);
            fSourceCitations.SaveToStream(stream);
            fMultimediaLinks.SaveToStream(stream);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.NOTE) {
                result = fNotes.Add(new GEDCOMNotes(Owner, this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.SOUR) {
                result = fSourceCitations.Add(new GEDCOMSourceCitation(Owner, this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.OBJE) {
                result = fMultimediaLinks.Add(new GEDCOMMultimediaLink(Owner, this, tagName, tagValue));
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
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

        protected GEDCOMTagWithLists(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        #region Auxiliary

        public GEDCOMNotes AddNote(GEDCOMNoteRecord noteRec)
        {
            GEDCOMNotes note = null;
            
            if (noteRec != null)
            {
                note = new GEDCOMNotes(Owner, this, "", "");
                note.Value = noteRec;
                Notes.Add(note);
            }
            
            return note;
        }

        public GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality)
        {
            GEDCOMSourceCitation cit = null;
            
            if (sourceRec != null)
            {
                cit = new GEDCOMSourceCitation(Owner, this, "", "");
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                SourceCitations.Add(cit);
            }
            
            return cit;
        }

        public GEDCOMMultimediaLink AddMultimedia(GEDCOMMultimediaRecord mediaRec)
        {
            GEDCOMMultimediaLink result = null;

            if (mediaRec != null)
            {
                result = new GEDCOMMultimediaLink(Owner, this, "", "");
                result.Value = mediaRec;
                MultimediaLinks.Add(result);
            }

            return result;
        }

        #endregion
    }
}
