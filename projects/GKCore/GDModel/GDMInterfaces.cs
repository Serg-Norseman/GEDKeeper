/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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


    public interface IGDMTag : IGDMObject
    {
        int Id { get; }
        string StringValue { get; set; }
        GDMList<GDMTag> SubTags { get; }

        bool IsEmpty();

        int GetHashCode();
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


    public interface IGDMLines
    {
        string this[int index] { get; set; }
        int Count { get; }
        string Text { get; set; }

        void Clear();
        bool IsEmpty();
        void TrimExcess();
    }


    public interface IGDMTextObject : IGDMTag
    {
        GDMLines Lines { get; }
    }


    public interface IGDMListEnumerator<out T> : IEnumerator<T>
    {
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


    public interface IGDMStructWithAddress : IGDMObject
    {
        bool HasAddress { get; }
        GDMAddress Address { get; }
    }


    public interface IGDMStructWithPlace : IGDMObject
    {
        bool HasPlace { get; }
        GDMPlace Place { get; }
    }


    public interface IGDMStructWithNotes : IGDMObject
    {
        bool HasNotes { get; }
        GDMList<GDMNotes> Notes { get; }
    }


    public interface IGDMStructWithSourceCitations : IGDMObject
    {
        bool HasSourceCitations { get; }
        GDMList<GDMSourceCitation> SourceCitations { get; }
    }


    public interface IGDMStructWithMultimediaLinks : IGDMObject
    {
        bool HasMultimediaLinks { get; }
        GDMList<GDMMultimediaLink> MultimediaLinks { get; }
    }


    public interface IGDMStructWithUserReferences : IGDMObject
    {
        bool HasUserReferences { get; }
        GDMList<GDMUserReference> UserReferences { get; }
    }


    public interface IGDMStructWithLists : IGDMStructWithNotes, IGDMStructWithSourceCitations, IGDMStructWithMultimediaLinks
    {
    }


    public interface IGDMEvent : IGDMStructWithLists, IGDMStructWithAddress, IGDMStructWithPlace
    {
        string Agency { get; set; }
        string Cause { get; set; }
        string Classification { get; set; }
        GDMDateValue Date { get; }
        string ReligiousAffilation { get; set; }
        GDMRestriction Restriction { get; set; }
    }


    public interface IGDMPointerHost
    {
        string XRef { get; }
    }


    public interface IGDMUIDHolder
    {
        string UID { get; }
    }


    public interface IGDMRecord : IGDMPointerHost, IGDMUIDHolder, IGDMStructWithLists, IGDMStructWithUserReferences
    {
        string AutomatedRecordID { get; }
        GDMChangeDate ChangeDate { get; }
        GDMRecordType RecordType { get; }
        GDMTree Tree { get; }
    }


    public interface IGDMRecordWithEvents : IGDMRecord
    {
        bool HasEvents { get; }
        GDMList<GDMCustomEvent> Events { get; }

        GDMRestriction Restriction { get; set; }
        
        GDMCustomEvent AddEvent(GDMCustomEvent evt);
        GDMCustomEvent FindEvent(string eventName);
        GDMCustomEvent FindEvent(GEDCOMTagType eventType);
    }


    public interface IGDMIndividualRecord : IGDMRecordWithEvents
    {
        bool HasAssociations { get; }
        GDMList<GDMAssociation> Associations { get; }

        bool Bookmark { get; set; }
        GDMList<GDMChildToFamilyLink> ChildToFamilyLinks { get; }

        bool HasGroups { get; }
        GDMList<GDMPointer> Groups { get; }

        bool Patriarch { get; set; }
        GDMList<GDMPersonalName> PersonalNames { get; }
        GDMSex Sex { get; set; }
        GDMList<GDMSpouseToFamilyLink> SpouseToFamilyLinks { get; }
    }


    public interface IGDMFamilyRecord : IGDMRecordWithEvents
    {
        GDMList<GDMChildLink> Children { get; }
        GDMIndividualLink Husband { get; }
        GDMIndividualLink Wife { get; }
        GDMMarriageStatus Status { get; set; }
    }
}
