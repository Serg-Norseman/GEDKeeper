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

using System.Collections;

namespace GKCommon.GEDCOM
{
    public interface IGEDCOMListEnumerator : IEnumerator
    {
        GEDCOMObject Owner
        {
            get;
        }
    }

    public interface IGEDCOMTreeEnumerator
    {
        bool MoveNext(out GEDCOMRecord current);
        void Reset();
    }

    public interface IGEDCOMStructWithLists
    {
        GEDCOMList<GEDCOMNotes> Notes { get; }
        GEDCOMList<GEDCOMSourceCitation> SourceCitations { get; }
        GEDCOMList<GEDCOMMultimediaLink> MultimediaLinks { get; }

        GEDCOMNotes AddNote(GEDCOMNoteRecord noteRec);
        GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality);
        GEDCOMMultimediaLink AddMultimedia(GEDCOMMultimediaRecord mediaRec);
    }
    
    public interface IGEDCOMRecordWithEvents
    {
        GEDCOMList<GEDCOMCustomEvent> Events { get; }
        
        GEDCOMCustomEvent AddEvent(GEDCOMCustomEvent evt);
        GEDCOMCustomEvent FindEvent(string eventName);
    }
}
