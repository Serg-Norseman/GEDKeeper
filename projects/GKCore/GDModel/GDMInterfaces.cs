/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
