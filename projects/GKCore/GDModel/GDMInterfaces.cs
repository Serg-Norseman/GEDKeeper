/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public interface IGDMObject
    {
    }


    public interface IGDMList<T> : IDisposable, IEnumerable<T>
        where T : class, IGDMObject
    {
        int Count { get; }

        T this[int index] { get; }


        T Add(T item);

        void Clear();

        void Remove(T item);

        void RemoveAt(int index);

        int IndexOf(T item);
    }


    public interface IGDMTextObject
    {
        GDMLines Lines { get; }

        bool IsEmpty();
    }


    public interface IGDMTreeEnumerator
    {
        bool MoveNext(out GDMRecord current);
        void Reset();
    }


    public interface IGDMTreeEnumerator<T> : IGDMTreeEnumerator where T : GDMRecord
    {
        bool MoveNext(out T current);
    }


    public interface IGDMStructWithAddress
    {
        bool HasAddress { get; }
        GDMAddress Address { get; }
    }


    public interface IGDMStructWithPlace
    {
        bool HasPlace { get; }
        GDMPlace Place { get; }
    }


    public interface IGDMStructWithRestriction
    {
        GDMRestriction Restriction { get; set; }
    }


    public interface IGDMStructWithNotes
    {
        bool HasNotes { get; }
        GDMList<GDMNotes> Notes { get; }
    }


    public interface IGDMStructWithSourceCitations
    {
        bool HasSourceCitations { get; }
        GDMList<GDMSourceCitation> SourceCitations { get; }
    }


    public interface IGDMStructWithMultimediaLinks
    {
        bool HasMultimediaLinks { get; }
        GDMList<GDMMultimediaLink> MultimediaLinks { get; }
    }


    public interface IGDMStructWithUserReferences
    {
        bool HasUserReferences { get; }
        GDMList<GDMUserReference> UserReferences { get; }
    }


    public interface IGDMStructWithLists : IGDMStructWithNotes, IGDMStructWithSourceCitations, IGDMStructWithMultimediaLinks
    {
    }


    public interface IGDMStructWithDate
    {
        GDMDateValue Date { get; }
    }


    public interface IGDMEvent : IGDMStructWithLists, IGDMStructWithAddress, IGDMStructWithPlace, IGDMStructWithRestriction, IGDMStructWithDate
    {
        string Agency { get; set; }
        string Cause { get; set; }
        string Classification { get; set; }
        string ReligiousAffilation { get; set; }
    }


    public interface IGDMPointerHost
    {
        string XRef { get; }
    }


    public interface IGDMRecord : IGDMPointerHost, IGDMStructWithLists, IGDMStructWithUserReferences
    {
        string AutomatedRecordID { get; }
        GDMChangeDate ChangeDate { get; }
        GDMRecordType RecordType { get; }
        GDMTree Tree { get; }
        string UID { get; }
    }


    public interface IGDMRecordWithEvents : IGDMRecord, IGDMStructWithRestriction
    {
        bool HasEvents { get; }
        GDMList<GDMCustomEvent> Events { get; }
        
        GDMCustomEvent AddEvent(GDMCustomEvent evt);
        GDMCustomEvent FindEvent(string eventName);
        GDMCustomEvent FindEvent(GEDCOMTagType eventType);
    }
}
