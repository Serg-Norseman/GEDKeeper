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

namespace GDModel
{
    /// <summary>
    /// 
    /// </summary>
    public static class GDMExtensions
    {
        public static GDMNotes AddNote(this IGDMStructWithNotes _struct, GDMNoteRecord noteRec)
        {
            GDMNotes note = null;

            if (noteRec != null) {
                note = new GDMNotes((GDMObject)_struct);
                note.Value = noteRec;
                _struct.Notes.Add(note);
            }

            return note;
        }

        public static GDMSourceCitation AddSource(this IGDMStructWithSourceCitations _struct, GDMSourceRecord sourceRec, string page, int quality)
        {
            GDMSourceCitation cit = null;

            if (sourceRec != null) {
                cit = new GDMSourceCitation((GDMObject)_struct);
                cit.Value = sourceRec;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                _struct.SourceCitations.Add(cit);
            }

            return cit;
        }

        public static GDMMultimediaLink AddMultimedia(this IGDMStructWithMultimediaLinks _struct, GDMMultimediaRecord mediaRec)
        {
            GDMMultimediaLink result = null;

            if (mediaRec != null) {
                result = new GDMMultimediaLink((GDMObject)_struct);
                result.Value = mediaRec;
                _struct.MultimediaLinks.Add(result);
            }

            return result;
        }

        public static void AddUserRef(this IGDMStructWithUserReferences _struct, string reference)
        {
            GDMUserReference uRef = new GDMUserReference((GDMObject)_struct);
            uRef.StringValue = reference;
            _struct.UserReferences.Add(uRef);
        }
    }
}
