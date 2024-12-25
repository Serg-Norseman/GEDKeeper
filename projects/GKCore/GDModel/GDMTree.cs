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
using GKCore;
using GKCore.Calendar;
using GKCore.Interfaces;

namespace GDModel
{
    public enum GDMTreeState
    {
        osLoading,
        osReady
    }


    /// <summary>
    /// The main container for the Genealogy Data Model (GDM).
    /// </summary>
    public sealed class GDMTree : GDMObject
    {
        #region Tree Enumerator

        private struct TreeEnumerator : IGDMTreeEnumerator
        {
            private readonly IList<GDMRecord> fTreeRecords;
            private readonly GDMRecordType fRecType;
            private readonly int fEndIndex;
            private int fIndex;

            public TreeEnumerator(GDMTree tree, GDMRecordType recType)
            {
                fTreeRecords = tree.fRecords.GetList();
                fIndex = -1;
                fEndIndex = ((fTreeRecords == null) ? 0 : fTreeRecords.Count) - 1;
                fRecType = recType;
            }

            public bool MoveNext(out GDMRecord current)
            {
                if (fRecType == GDMRecordType.rtNone) {
                    if (fIndex < fEndIndex) {
                        fIndex++;
                        current = fTreeRecords[fIndex];
                        return true;
                    }
                } else {
                    while (fIndex < fEndIndex) {
                        fIndex++;
                        GDMRecord rec = fTreeRecords[fIndex];
                        if (rec.RecordType == fRecType) {
                            current = rec;
                            return true;
                        }
                    }
                }

                fIndex = fEndIndex + 1;
                current = null;
                return false;
            }

            public void Reset()
            {
                fIndex = -1;
            }
        }

        private struct TreeEnumerator<T> : IGDMTreeEnumerator<T> where T : GDMRecord
        {
            private readonly IList<GDMRecord> fTreeRecords;
            private readonly int fEndIndex;
            private int fIndex;

            public TreeEnumerator(GDMTree tree)
            {
                fTreeRecords = tree.fRecords.GetList();
                fIndex = -1;
                fEndIndex = ((fTreeRecords == null) ? 0 : fTreeRecords.Count) - 1;
            }

            public bool MoveNext(out T current)
            {
                while (fIndex < fEndIndex) {
                    fIndex++;
                    T rec = fTreeRecords[fIndex] as T;
                    if (rec != null) {
                        current = rec;
                        return true;
                    }
                }

                fIndex = fEndIndex + 1;
                current = null;
                return false;
            }

            public bool MoveNext(out GDMRecord current)
            {
                while (fIndex < fEndIndex) {
                    fIndex++;
                    var rec = fTreeRecords[fIndex];
                    if (rec != null) {
                        current = rec;
                        return true;
                    }
                }

                fIndex = fEndIndex + 1;
                current = null;
                return false;
            }

            public void Reset()
            {
                fIndex = -1;
            }
        }

        #endregion


        private readonly GDMHeader fHeader;
        private readonly GDMList<GDMRecord> fRecords;
        private readonly Dictionary<string, GDMRecord> fXRefIndex;

        private GEDCOMFormat fFormat;
        private int[] fLastIDs;
        private IProgressController fProgressCallback;
        private GDMTreeState fState;
        private int fUpdateCount;


        public GEDCOMFormat Format
        {
            get { return fFormat; }
            internal set { fFormat = value; }
        }

        public GDMHeader Header
        {
            get { return fHeader; }
        }

        public bool IsEmpty
        {
            get { return (fRecords.Count == 0); }
        }

        public int RecordsCount
        {
            get { return fRecords.Count; }
        }

        public GDMRecord this[int index]
        {
            get { return fRecords[index]; }
        }

        public GDMTreeState State
        {
            get { return fState; }
            set { fState = value; }
        }

        public event EventHandler OnChange;

        public event EventHandler OnChanging;

        public IProgressController ProgressCallback
        {
            get { return fProgressCallback; }
            set { fProgressCallback = value; }
        }


        public GDMTree()
        {
            fXRefIndex = new Dictionary<string, GDMRecord>();
            fRecords = new GDMList<GDMRecord>();
            fHeader = new GDMHeader();

            ResetLastIDs();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fHeader.Dispose();
                fRecords.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Internal

        internal GDMList<GDMRecord> GetRecords()
        {
            return fRecords;
        }

        internal void TrimExcess()
        {
            fHeader.TrimExcess();
            fRecords.TrimExcess();
        }

        #endregion

        #region XRef Search

        private void XRefIndex_AddRecord(GDMRecord record)
        {
            if (record == null || string.IsNullOrEmpty(record.XRef)) return;

            bool exists = fXRefIndex.ContainsKey(record.XRef);
            if (!exists) fXRefIndex.Add(record.XRef, record);
        }

        private void XRefIndex_DeleteRecord(GDMRecord record)
        {
            bool exists = fXRefIndex.ContainsKey(record.XRef);
            if (exists) fXRefIndex.Remove(record.XRef);
        }

        public GDMRecord XRefIndex_Find(string xref)
        {
            if (string.IsNullOrEmpty(xref)) return null;

            GDMRecord record;
            return fXRefIndex.TryGetValue(xref, out record) ? record : null;
        }

        private void ResetLastIDs()
        {
            fLastIDs = new int[(int)GDMRecordType.rtLast + 1];
        }

        private string XRefIndex_NewXRef(GDMRecord record)
        {
            var invNFI = GEDCOMUtils.InvariantNumberFormatInfo;
            string sign = GEDCOMUtils.GetSignByRecord(record);
            string xref;

            int recType = (int)record.RecordType;
            int lastId = fLastIDs[recType];

            do {
                lastId++;
                xref = sign + lastId.ToString(invNFI);
            } while (fXRefIndex.ContainsKey(xref));

            fLastIDs[recType] = lastId;
            return xref;
        }

        public void SetXRef(string oldXRef, GDMRecord record, bool removeOldXRef)
        {
            if (removeOldXRef && !string.IsNullOrEmpty(oldXRef)) {
                // remove can verify existing of key
                fXRefIndex.Remove(oldXRef);
            }

            XRefIndex_AddRecord(record);
        }

        #endregion

        #region Main functionality

        public List<T> GetRecords<T>() where T : GDMRecord
        {
            List<T> result = new List<T>();

            for (int i = 0; i < fRecords.Count; i++) {
                T rec = fRecords[i] as T;
                if (rec != null) {
                    result.Add(rec);
                }
            }

            return result;
        }

        public List<GDMRecord> GetRecords(GDMRecordType recType)
        {
            var result = new List<GDMRecord>();

            for (int i = 0; i < fRecords.Count; i++) {
                var rec = fRecords[i];
                if (recType == GDMRecordType.rtNone || rec.RecordType == recType) {
                    result.Add(rec);
                }
            }

            return result;
        }

        public IGDMTreeEnumerator GetEnumerator(GDMRecordType recType)
        {
            return new TreeEnumerator(this, recType);
        }

        public IGDMTreeEnumerator<T> GetEnumerator<T>() where T : GDMRecord
        {
            return new TreeEnumerator<T>(this);
        }

        public void Clear()
        {
            fHeader.Clear();
            fRecords.Clear();
            fXRefIndex.Clear();
            ResetLastIDs();
        }

        public GDMRecord AddRecord(GDMRecord record)
        {
            fRecords.Add(record);
            XRefIndex_AddRecord(record);
            return record;
        }

        public void Delete(int index)
        {
            XRefIndex_DeleteRecord(fRecords[index]);
            fRecords.RemoveAt(index);
        }

        public void DeleteRecord(GDMRecord record)
        {
            XRefIndex_DeleteRecord(record);
            fRecords.Remove(record);
        }

        public GDMRecord Extract(int index)
        {
            XRefIndex_DeleteRecord(fRecords[index]);
            return fRecords.Extract(index);
        }

        public int IndexOf(GDMRecord record)
        {
            return fRecords.IndexOf(record);
        }

        public GDMRecord FindUID(string uid)
        {
            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                if (rec.UID == uid) {
                    return rec;
                }
            }

            return null;
        }

        public string NewXRef(GDMRecord gdmRec, bool removeOldXRef = false)
        {
            string newXRef = XRefIndex_NewXRef(gdmRec);
            gdmRec.SetXRef(this, newXRef, removeOldXRef);
            return gdmRec.XRef;
        }

        public T GetPtrValue<T>(GDMPointer ptr) where T : GDMRecord
        {
            return (ptr == null || !ptr.IsPointer) ? default : XRefIndex_Find(ptr.XRef) as T;
        }

        public void SetPtrValue(GDMPointer ptr, GDMRecord record)
        {
            if (ptr == null) return;

            ptr.XRef = string.Empty;
            if (record == null) return;

            string xrf = record.XRef;
            if (string.IsNullOrEmpty(xrf)) {
                xrf = NewXRef(record);
            }
            ptr.XRef = xrf;
        }

        public T FindXRef<T>(string xref) where T : GDMRecord
        {
            return XRefIndex_Find(xref) as T;
        }

        #endregion

        public int[] GetRecordStats()
        {
            int[] stats = new int[((int)GDMRecordType.rtLast)];

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                int index = (int)rec.RecordType;
                stats[index] += 1;
            }

            return stats;
        }

        public GDMSubmitterRecord GetSubmitter()
        {
            GDMSubmitterRecord submitter = GetPtrValue<GDMSubmitterRecord>(fHeader.Submitter);
            if (submitter == null) {
                submitter = new GDMSubmitterRecord(this);
                NewXRef(submitter);
                AddRecord(submitter);
                SetPtrValue(fHeader.Submitter, submitter);
            }
            return submitter;
        }

        public GDMIndividualRecord CreateIndividual()
        {
            GDMIndividualRecord result = new GDMIndividualRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMFamilyRecord CreateFamily()
        {
            GDMFamilyRecord result = new GDMFamilyRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMNoteRecord CreateNote()
        {
            GDMNoteRecord result = new GDMNoteRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMSourceRecord CreateSource()
        {
            GDMSourceRecord result = new GDMSourceRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMRepositoryRecord CreateRepository()
        {
            GDMRepositoryRecord result = new GDMRepositoryRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMResearchRecord CreateResearch()
        {
            GDMResearchRecord result = new GDMResearchRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMCommunicationRecord CreateCommunication()
        {
            GDMCommunicationRecord result = new GDMCommunicationRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMTaskRecord CreateTask()
        {
            GDMTaskRecord result = new GDMTaskRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMMultimediaRecord CreateMultimedia()
        {
            GDMMultimediaRecord result = new GDMMultimediaRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMLocationRecord CreateLocation()
        {
            GDMLocationRecord result = new GDMLocationRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMGroupRecord CreateGroup()
        {
            GDMGroupRecord result = new GDMGroupRecord(this);
            NewXRef(result);
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public bool DeleteIndividualRecord(GDMIndividualRecord iRec)
        {
            if (iRec == null) return false;

            iRec.Clear();
            DeleteRecord(iRec);
            return true;
        }

        public bool DeleteFamilyRecord(GDMFamilyRecord famRec)
        {
            if (famRec == null) return false;

            famRec.Clear();
            DeleteRecord(famRec);
            return true;
        }

        public bool DeleteGroupRecord(GDMGroupRecord groupRec)
        {
            if (groupRec == null) return false;

            for (int i = groupRec.Members.Count - 1; i >= 0; i--) {
                GDMIndividualRecord member = GetPtrValue<GDMIndividualRecord>(groupRec.Members[i]);
                groupRec.RemoveMember(member);
            }

            DeleteRecord(groupRec);
            return true;
        }

        public bool DeleteMediaRecord(GDMMultimediaRecord mRec)
        {
            if (mRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                if (!rec.HasMultimediaLinks) continue;

                for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--) {
                    if (rec.MultimediaLinks[j].XRef == mRec.XRef) {
                        rec.MultimediaLinks.RemoveAt(j);
                    }
                }
            }

            DeleteRecord(mRec);
            return true;
        }

        public bool DeleteNoteRecord(GDMNoteRecord nRec)
        {
            if (nRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                for (int j = rec.Notes.Count - 1; j >= 0; j--) {
                    if (rec.Notes[j].XRef == nRec.XRef)
                        rec.Notes.RemoveAt(j);
                }
            }

            DeleteRecord(nRec);
            return true;
        }

        public bool DeleteRepositoryRecord(GDMRepositoryRecord repRec)
        {
            if (repRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                if (rec.RecordType == GDMRecordType.rtSource) {
                    GDMSourceRecord srcRec = (GDMSourceRecord)rec;
                    for (int j = srcRec.RepositoryCitations.Count - 1; j >= 0; j--) {
                        if (srcRec.RepositoryCitations[j].XRef == repRec.XRef) {
                            srcRec.RepositoryCitations.RemoveAt(j);
                        }
                    }
                }
            }

            DeleteRecord(repRec);
            return true;
        }

        public bool DeleteResearchRecord(GDMResearchRecord resRec)
        {
            if (resRec == null) return false;

            DeleteRecord(resRec);
            return true;
        }

        public bool DeleteSourceRecord(GDMSourceRecord srcRec)
        {
            if (srcRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                if (!rec.HasSourceCitations) continue;

                for (int j = rec.SourceCitations.Count - 1; j >= 0; j--) {
                    if (rec.SourceCitations[j].XRef == srcRec.XRef) {
                        rec.SourceCitations.RemoveAt(j);
                    }
                }
            }

            DeleteRecord(srcRec);
            return true;
        }

        public bool DeleteTaskRecord(GDMTaskRecord taskRec)
        {
            if (taskRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                if (rec.RecordType == GDMRecordType.rtResearch) {
                    GDMResearchRecord resRec = (GDMResearchRecord)rec;
                    for (int j = resRec.Tasks.Count - 1; j >= 0; j--) {
                        if (resRec.Tasks[j].XRef == taskRec.XRef) {
                            resRec.Tasks.RemoveAt(j);
                        }
                    }
                }
            }

            DeleteRecord(taskRec);
            return true;
        }

        public bool DeleteCommunicationRecord(GDMCommunicationRecord commRec)
        {
            if (commRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fRecords[i];
                if (rec.RecordType == GDMRecordType.rtResearch) {
                    GDMResearchRecord resRec = (GDMResearchRecord)rec;
                    for (int j = resRec.Communications.Count - 1; j >= 0; j--) {
                        if (resRec.Communications[j].XRef == commRec.XRef) {
                            resRec.Communications.RemoveAt(j);
                        }
                    }
                }
            }

            DeleteRecord(commRec);
            return true;
        }

        public bool DeleteLocationRecord(GDMLocationRecord locRec)
        {
            if (locRec == null) return false;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                var rec = fRecords[i];

                switch (rec.RecordType) {
                    case GDMRecordType.rtIndividual:
                    case GDMRecordType.rtFamily: {
                            var evsRec = rec as GDMRecordWithEvents;
                            if (evsRec == null || !evsRec.HasEvents) continue;

                            for (int j = evsRec.Events.Count - 1; j >= 0; j--) {
                                var evt = evsRec.Events[j];
                                if (!evt.HasPlace) continue;

                                GDMPointer evLocation = evt.Place.Location;
                                if (evLocation.XRef == locRec.XRef) {
                                    evLocation.XRef = string.Empty;
                                }
                            }
                        }
                        break;

                    case GDMRecordType.rtLocation: {
                            var lRec = rec as GDMLocationRecord;
                            if (lRec == null) continue;

                            for (int j = lRec.TopLevels.Count - 1; j >= 0; j--) {
                                var topLev = lRec.TopLevels[j];
                                if (topLev.XRef == locRec.XRef) {
                                    lRec.TopLevels.RemoveAt(j);
                                }
                            }
                        }
                        break;
                }
            }

            DeleteRecord(locRec);
            return true;
        }

        public void RenameLocationRecord(GDMLocationRecord locRec)
        {
            if (locRec == null) return;

            int num = fRecords.Count;
            for (int i = 0; i < num; i++) {
                var evsRec = fRecords[i] as GDMRecordWithEvents;
                if (evsRec == null || !evsRec.HasEvents) continue;

                for (int j = evsRec.Events.Count - 1; j >= 0; j--) {
                    var evt = evsRec.Events[j];
                    if (!evt.HasPlace) continue;

                    GDMPlace evPlace = evt.Place;
                    if (evPlace.Location.XRef == locRec.XRef) {
                        evPlace.StringValue = GKUtils.GetLocationNameExt(locRec, evt.Date.Value);
                    }
                }
            }
        }

        private int ChildrenEventsCompare(GDMPointer cp1, GDMPointer cp2)
        {
            var child1 = GetPtrValue<GDMIndividualRecord>(cp1);
            var child2 = GetPtrValue<GDMIndividualRecord>(cp2);

            UDN udn1 = child1.GetUDN(GEDCOMTagType.BIRT);
            UDN udn2 = child2.GetUDN(GEDCOMTagType.BIRT);

            return udn1.CompareTo(udn2);
        }

        public void SortChildren(GDMFamilyRecord famRec)
        {
            if (famRec != null) {
                famRec.Children.Sort(ChildrenEventsCompare);
            }
        }

        private int SpousesEventsCompare(GDMPointer cp1, GDMPointer cp2)
        {
            var spouse1 = GetPtrValue<GDMFamilyRecord>(cp1);
            var spouse2 = GetPtrValue<GDMFamilyRecord>(cp2);

            UDN udn1 = spouse1.GetUDN(GEDCOMTagType.MARR);
            UDN udn2 = spouse2.GetUDN(GEDCOMTagType.MARR);

            return udn1.CompareTo(udn2);
        }

        public void SortSpouses(GDMIndividualRecord indiRec)
        {
            if (indiRec != null) {
                indiRec.SpouseToFamilyLinks.Sort(SpousesEventsCompare);
            }
        }

        public GDMIndividualRecord GetSpouseBy(GDMFamilyRecord family, GDMIndividualRecord spouse)
        {
            GDMIndividualRecord husb = GetPtrValue<GDMIndividualRecord>(family.Husband);
            GDMIndividualRecord wife = GetPtrValue<GDMIndividualRecord>(family.Wife);

            return (spouse == husb) ? wife : husb;
        }

        #region Updating

        public bool IsUpdated()
        {
            return (fUpdateCount != 0);
        }

        public void BeginUpdate()
        {
            if (fUpdateCount == 0) {
                SetUpdateState(true);
            }
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;
            if (fUpdateCount == 0) {
                SetUpdateState(false);
            }
        }

        private void SetUpdateState(bool updating)
        {
            if (updating) {
                Changing();
            } else {
                Changed();
            }
        }

        private void Changed()
        {
            if (fUpdateCount == 0) {
                var eventHandler = OnChange;
                if (eventHandler != null)
                    eventHandler(this, new EventArgs());
            }
        }

        private void Changing()
        {
            if (fUpdateCount == 0) {
                var eventHandler = OnChanging;
                if (eventHandler != null)
                    eventHandler(this, new EventArgs());
            }
        }

        #endregion

        public int GetTotalChildrenCount(GDMIndividualRecord individualRec)
        {
            int result = 0;

            int num = individualRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                var family = GetPtrValue<GDMFamilyRecord>(individualRec.SpouseToFamilyLinks[i]);
                result += family.Children.Count;
            }

            return result;
        }

        public GDMFamilyRecord FindChildFamily(GDMIndividualRecord individualRec, GDMIndividualRecord childRec)
        {
            int num = individualRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                var family = GetPtrValue<GDMFamilyRecord>(individualRec.SpouseToFamilyLinks[i]);
                if (family.IndexOfChild(childRec) >= 0) {
                    return family;
                }
            }
            return null;
        }

        public GDMLines GetNoteLines(GDMNotes notes)
        {
            GDMLines lines;
            if (!notes.IsPointer) {
                lines = notes.Lines;
            } else {
                var notesRecord = GetPtrValue<GDMNoteRecord>(notes);
                lines = (notesRecord != null) ? notesRecord.Lines : new GDMLines();
            }

            return lines;
        }
    }
}
