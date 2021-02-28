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

using System.Collections.Generic;

namespace GDModel
{
    public interface IGDMObject
    {
    }


    public interface IGDMTag : IGDMObject
    {
        bool IsEmpty();
    }


    public interface IGDMLines
    {
        string this[int index] { get; set; }
        int Count { get; }
        string Text { get; set; }

        void Clear();
        bool IsEmpty();
    }


    public interface IGDMTextObject : IGDMTag
    {
        GDMLines Lines { get; }
    }


    public interface IGEDCOMListEnumerator<T> : IEnumerator<T>
    {
    }


    public interface IGEDCOMTreeEnumerator
    {
        bool MoveNext(out GDMRecord current);
        void Reset();
    }


    public interface IGEDCOMStructWithLists : IGDMObject
    {
        GDMList<GDMNotes> Notes { get; }
        GDMList<GDMSourceCitation> SourceCitations { get; }
        GDMList<GDMMultimediaLink> MultimediaLinks { get; }

        GDMNotes AddNote(GDMNoteRecord noteRec);
        GDMSourceCitation AddSource(GDMSourceRecord sourceRec, string page, int quality);
        GDMMultimediaLink AddMultimedia(GDMMultimediaRecord mediaRec);
    }


    public interface IGEDCOMRecordWithEvents : IGDMObject
    {
        GDMList<GDMCustomEvent> Events { get; }
        
        GDMCustomEvent AddEvent(GDMCustomEvent evt);
        GDMCustomEvent FindEvent(string eventName);
    }
}
