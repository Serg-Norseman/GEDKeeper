﻿/*
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

using System;
using System.Collections.Generic;
using System.Globalization;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public delegate void ProgressEventHandler(object sender, int progress);


    public enum GDMTreeState
    {
        osLoading,
        osReady
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMTree : GDMObject
    {
        #region Tree Enumerator

        private struct TreeEnumerator : IGEDCOMTreeEnumerator
        {
            private readonly GDMTree fTree;
            private readonly GDMRecordType fRecType;
            private readonly int fEndIndex;
            private int fIndex;

            public TreeEnumerator(GDMTree tree, GDMRecordType recType)
            {
                fTree = tree;
                fIndex = -1;
                fEndIndex = tree.RecordsCount - 1;
                fRecType = recType;
            }

            public bool MoveNext(out GDMRecord current)
            {
                if (fRecType == GDMRecordType.rtNone) {
                    if (fIndex < fEndIndex) {
                        fIndex++;
                        current = fTree[fIndex];
                        return true;
                    }
                } else {
                    while (fIndex < fEndIndex) {
                        fIndex++;
                        GDMRecord rec = fTree[fIndex];
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

        #endregion


        private readonly GDMHeader fHeader;
        private readonly GDMList<GDMRecord> fRecords;
        private readonly Dictionary<string, GDMRecord> fXRefIndex;

        private GEDCOMFormat fFormat;
        private int[] fLastIDs;
        private EventHandler fOnChange;
        private EventHandler fOnChanging;
        private ProgressEventHandler fOnProgressEvent;
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

        public event EventHandler OnChange
        {
            add { fOnChange = value; }
            remove { if (fOnChange == value) fOnChange = null; }
        }

        public event EventHandler OnChanging
        {
            add { fOnChanging = value; }
            remove { if (fOnChanging == value) fOnChanging = null; }
        }

        public ProgressEventHandler OnProgress
        {
            get { return fOnProgressEvent; }
            set { fOnProgressEvent = value; }
        }


        public GDMTree()
        {
            fXRefIndex = new Dictionary<string, GDMRecord>();
            fRecords = new GDMList<GDMRecord>(this);
            fHeader = new GDMHeader(this);

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
            if (fXRefIndex.TryGetValue(xref, out record)) {
                return record;
            } else {
                return null;
            }
        }

        private void ResetLastIDs()
        {
            fLastIDs = new int[(int)GDMRecordType.rtLast + 1];
        }

        public string XRefIndex_NewXRef(GDMRecord record)
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

        public void SetXRef(string oldXRef, GDMRecord record)
        {
            if (!string.IsNullOrEmpty(oldXRef)) {
                bool exists = fXRefIndex.ContainsKey(oldXRef);
                if (exists) fXRefIndex.Remove(oldXRef);
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

        public IGEDCOMTreeEnumerator GetEnumerator(GDMRecordType recType)
        {
            return new TreeEnumerator(this, recType);
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
            fRecords.DeleteAt(index);
        }

        public void DeleteRecord(GDMRecord record)
        {
            XRefIndex_DeleteRecord(record);
            fRecords.Delete(record);
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
            GDMSubmitterRecord submitter = fHeader.Submitter.Value as GDMSubmitterRecord;
            if (submitter == null) {
                submitter = new GDMSubmitterRecord(this);
                submitter.InitNew();
                AddRecord(submitter);
                fHeader.Submitter.Value = submitter;
            }
            return submitter;
        }

        public GDMIndividualRecord CreateIndividual()
        {
            GDMIndividualRecord result = new GDMIndividualRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMFamilyRecord CreateFamily()
        {
            GDMFamilyRecord result = new GDMFamilyRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMNoteRecord CreateNote()
        {
            GDMNoteRecord result = new GDMNoteRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMSourceRecord CreateSource()
        {
            GDMSourceRecord result = new GDMSourceRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMRepositoryRecord CreateRepository()
        {
            GDMRepositoryRecord result = new GDMRepositoryRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMResearchRecord CreateResearch()
        {
            GDMResearchRecord result = new GDMResearchRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMCommunicationRecord CreateCommunication()
        {
            GDMCommunicationRecord result = new GDMCommunicationRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMTaskRecord CreateTask()
        {
            GDMTaskRecord result = new GDMTaskRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMMultimediaRecord CreateMultimedia()
        {
            GDMMultimediaRecord result = new GDMMultimediaRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMLocationRecord CreateLocation()
        {
            GDMLocationRecord result = new GDMLocationRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        public GDMGroupRecord CreateGroup()
        {
            GDMGroupRecord result = new GDMGroupRecord(this);
            result.InitNew();
            result.ChangeDate.ChangeDateTime = DateTime.Now;

            AddRecord(result);
            return result;
        }

        //

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
                GDMIndividualRecord member = groupRec.Members[i].Individual;
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
                for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--) {
                    if (rec.MultimediaLinks[j].Value == mRec) {
                        rec.MultimediaLinks.DeleteAt(j);
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
                    if (rec.Notes[j].Value == nRec)
                        rec.Notes.DeleteAt(j);
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
                        if (srcRec.RepositoryCitations[j].Value == repRec) {
                            srcRec.RepositoryCitations.DeleteAt(j);
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
                for (int j = rec.SourceCitations.Count - 1; j >= 0; j--) {
                    if (rec.SourceCitations[j].Value == srcRec) {
                        rec.SourceCitations.DeleteAt(j);
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
                        if (resRec.Tasks[j].Value == taskRec) {
                            resRec.Tasks.DeleteAt(j);
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
                        if (resRec.Communications[j].Value == commRec) {
                            resRec.Communications.DeleteAt(j);
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
                var evsRec = fRecords[i] as GDMRecordWithEvents;
                if (evsRec != null) {
                    for (int j = evsRec.Events.Count - 1; j >= 0; j--) {
                        GDMPointer evLocation = evsRec.Events[j].Place.Location;

                        if (evLocation.Value == locRec) {
                            evLocation.Value = null;
                        }
                    }
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
                if (evsRec != null) {
                    for (int j = evsRec.Events.Count - 1; j >= 0; j--) {
                        GDMPlace evPlace = evsRec.Events[j].Place;

                        if (evPlace.Location.Value == locRec) {
                            evPlace.StringValue = locRec.LocationName;
                        }
                    }
                }
            }
        }

        #region Utilities

        #endregion

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
            if (fUpdateCount == 0 && fOnChange != null) {
                fOnChange(this, new EventArgs());
            }
        }

        private void Changing()
        {
            if (fUpdateCount == 0 && fOnChanging != null) {
                fOnChanging(this, new EventArgs());
            }
        }

        #endregion
    }
}
