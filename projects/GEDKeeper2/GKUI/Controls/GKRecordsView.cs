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

using System;
using System.Collections.Generic;
using System.Windows.Forms;

using Externals;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Lists;
using GKCore.Types;

namespace GKUI.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GKRecordsView : GKListView
    {
        #region Private fields

        private class ValItem
        {
            public readonly GEDCOMRecord Record;
            public object ColumnValue;

            public ValItem(GEDCOMRecord record)
            {
                this.Record = record;
                this.ColumnValue = null;
            }
        }

        private List<ValItem> fContentList;
        private int fFilteredCount;
        private bool fIsMainList;
        private ListManager fListMan;
        private GEDCOMRecordType fRecordType;
        private int fTotalCount;
        private GEDCOMTree fTree;

        private int fXSortColumn;
        private int fXSortFactor;
        private SortOrder fXSortOrder;

        private GKListItem[] fCache;
        private int fCacheFirstItem;

        #endregion

        #region Public properties

        public int FilteredCount
        {
            get { return this.fFilteredCount; }
        }

        public bool IsMainList
        {
            get { return this.fIsMainList; }
            set { this.fIsMainList = value; }
        }

        public ListManager ListMan
        {
            get { return this.fListMan; }
        }

        public GEDCOMRecordType RecordType
        {
            get { return this.fRecordType; }
            set { this.SetRecordType(value); }
        }

        public int TotalCount
        {
            get { return this.fTotalCount; }
        }

        public GEDCOMTree Tree
        {
            get { return this.fTree; }
            set { this.fTree = value; }
        }

        #endregion

        public GKRecordsView() : base()
        {
            this.fContentList = new List<ValItem>();
            this.fListMan = null;
            this.fRecordType = GEDCOMRecordType.rtNone;
            this.fXSortColumn = 0;
            this.fXSortOrder = SortOrder.Ascending;

            base.UnsetSorter();
            base.ColumnClick += this.List_ColumnClick;
            base.RetrieveVirtualItem += this.List_RetrieveVirtualItem;
            base.CacheVirtualItems += this.List_CacheVirtualItems;
            base.VirtualMode = true;
            //base.ColumnWidthChanged += this.List_ColumnWidthChanged;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (this.fListMan != null)
                {
                    this.fListMan.Dispose();
                    this.fListMan = null;
                }

                this.fContentList = null;
            }
            base.Dispose(disposing);
        }

        public override void BeginUpdates()
        {
            base.ColumnWidthChanged -= this.List_ColumnWidthChanged;
            base.BeginUpdates();
        }

        public override void EndUpdates()
        {
            try {
                base.EndUpdates();
                base.ColumnWidthChanged += this.List_ColumnWidthChanged;
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.EndUpdates(): " + ex.Message);
            }
        }

        private void List_ColumnWidthChanged(object sender, ColumnWidthChangedEventArgs e)
        {
            switch (this.fRecordType) {
                case GEDCOMRecordType.rtIndividual:
                    if (this.fListMan != null) {
                        this.fListMan.WidthChanged(e.ColumnIndex, this.Columns[e.ColumnIndex].Width);
                    }
                    break;
            }
        }

        private void List_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            GEDCOMRecord rec = this.GetSelectedRecord();

            if (e.Column == fXSortColumn) {
                fXSortOrder = (fXSortOrder == SortOrder.Ascending ? SortOrder.Descending : SortOrder.Ascending);
            } else {
                fXSortColumn = e.Column;
                fXSortOrder = SortOrder.Ascending;
            }

            this.SortContents();
            this.SelectItemByRec(rec);
            base.Invalidate();
        }

        private void SortContents()
        {
            try {
                int num = this.fContentList.Count;
                for (int i = 0; i < num; i++) {
                    ValItem valItem = this.fContentList[i];
                    GEDCOMRecord rec = valItem.Record;

                    if (this.fXSortColumn == 0) {
                        valItem.ColumnValue = rec.GetId();
                    } else {
                        this.fListMan.Fetch(rec);
                        valItem.ColumnValue = this.fListMan.GetColumnInternalValue(this.fXSortColumn);
                    }
                }

                this.fXSortFactor = (this.fXSortOrder == SortOrder.Ascending ? 1 : -1);
                ListTimSort<ValItem>.Sort(this.fContentList, CompareItems);

                // clear cache
                this.fCache = null;
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.SortContents(): " + ex.Message);
            }
        }

        private int CompareItems(ValItem item1, ValItem item2)
        {
            int compRes;
            object cv1 = item1.ColumnValue;
            object cv2 = item2.ColumnValue;

            if (cv1 != null && cv2 != null)
            {
                compRes = ((IComparable)cv1).CompareTo(cv2);
            }
            else if (cv1 != null && cv2 == null)
            {
                compRes = -1;
            }
            else if (cv1 == null && cv2 != null)
            {
                compRes = 1;
            }
            else {
                compRes = 0;
            }

            return compRes * this.fXSortFactor;
        }

        private GKListItem GetListItem(int itemIndex)
        {
            GKListItem newItem;

            if (itemIndex < 0 || itemIndex >= this.fContentList.Count) {
                newItem = null;
            } else {
                GEDCOMRecord rec = this.fContentList[itemIndex].Record;

                newItem = new GKListItem(rec.GetXRefNum(), rec);

                this.fListMan.Fetch(rec);
                this.fListMan.UpdateItem(newItem, this.fIsMainList);
            }

            return newItem;
        }

        private void List_CacheVirtualItems(object sender, CacheVirtualItemsEventArgs e)
        {
            // Only recreate the cache if we need to.
            if (fCache != null && e.StartIndex >= fCacheFirstItem && e.EndIndex <= fCacheFirstItem + fCache.Length) return;

            fCacheFirstItem = e.StartIndex;
            int length = e.EndIndex - e.StartIndex + 1;

            fCache = new GKListItem[length];
            for (int i = 0; i < fCache.Length; i++)
            {
                fCache[i] = GetListItem(fCacheFirstItem + i);
            }
        }

        private void List_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            // If we have the item cached, return it. Otherwise, recreate it.
            if (fCache != null && e.ItemIndex >= fCacheFirstItem && e.ItemIndex < fCacheFirstItem + fCache.Length) {
                e.Item = fCache[e.ItemIndex - fCacheFirstItem];
            } else {
                e.Item = GetListItem(e.ItemIndex);
            }
        }

        private void SetRecordType(GEDCOMRecordType value)
        {
            this.fRecordType = value;

            if (this.fListMan != null) {
                this.fListMan.Dispose();
                this.fListMan = null;
            }

            switch (this.fRecordType) {
                case GEDCOMRecordType.rtIndividual:
                    this.fListMan = new IndividualListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtFamily:
                    this.fListMan = new FamilyListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtNote:
                    this.fListMan = new NoteListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    this.fListMan = new MultimediaListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtSource:
                    this.fListMan = new SourceListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtRepository:
                    this.fListMan = new RepositoryListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtGroup:
                    this.fListMan = new GroupListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtResearch:
                    this.fListMan = new ResearchListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtTask:
                    this.fListMan = new TaskListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    this.fListMan = new CommunicationListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtLocation:
                    this.fListMan = new LocationListMan(this.fTree);
                    break;

                case GEDCOMRecordType.rtSubmission:
                    this.fListMan = null;
                    break;

                case GEDCOMRecordType.rtSubmitter:
                    this.fListMan = null;
                    break;
            }
        }

        public void UpdateTitles()
        {
            try {
                if (this.fListMan == null) return;

                this.BeginUpdates();
                try
                {
                    this.Columns.Clear();
                    this.fListMan.UpdateColumns(this, this.fIsMainList);
                }
                finally
                {
                    this.EndUpdates();
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.UpdateTitles(): " + ex.Message);
            }
        }

        public void UpdateContents(ShieldState shieldState, bool titles, int autosizeColumn)
        {
            if (this.fListMan == null) return;

            try
            {
                GEDCOMRecord tempRec = this.GetSelectedRecord();

                this.fTotalCount = 0;
                this.fFilteredCount = 0;

                if (titles) {
                    this.UpdateTitles();
                }

                this.BeginUpdates();
                try
                {
                    //this.SelectedIndices.Clear();
                    //this.SelectedItems.Clear();

                    this.fListMan.InitFilter();

                    int contentSize = this.fTree.RecordsCount;

                    this.fContentList.Clear();
                    this.fContentList.Capacity = contentSize;

                    for (int i = 0; i < contentSize; i++) {
                        GEDCOMRecord rec = this.fTree[i];

                        if (rec.RecordType == this.fRecordType) {
                            this.fTotalCount++;

                            this.fListMan.Fetch(rec);
                            if (this.fListMan.CheckFilter(shieldState)) {
                                this.fContentList.Add(new ValItem(rec));
                            }
                        }
                    }

                    this.SortContents();

                    this.fFilteredCount = this.fContentList.Count;
                    this.VirtualListSize = this.fContentList.Count;

                    #if __MonoCS__
                    if (this.fContentList.Count != 0)
                    {
                        this.TopItem = this.Items[0];
                    }
                    #endif

                    base.ResizeColumn(autosizeColumn);
                }
                finally
                {
                    this.EndUpdates();
                }

                if (tempRec != null) this.SelectItemByRec(tempRec);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKRecordsView.UpdateContents(): " + ex.Message);
            }
        }

        public List<GEDCOMRecord> GetContentList()
        {
            int size = this.fContentList.Count;
            List<GEDCOMRecord> result = new List<GEDCOMRecord>(size);

            for (int i = 0; i < size; i++) {
                ValItem vi = this.fContentList[i];
                result.Add(vi.Record);
            }

            return result;
        }

        public int IndexOfRecord(GEDCOMRecord record)
        {
            int result = -1;

            int num = this.fContentList.Count;
            for (int i = 0; i < num; i++) {
                ValItem vi = this.fContentList[i];
                if (vi.Record == record) {
                    result = i;
                    break;
                }
            }

            return result;
        }

        public void DeleteRecord(GEDCOMRecord record)
        {
            // crash protection: when you delete records from the diagrams, 
            // between the actual deleting a record and updating the list 
            // may take a few requests to update the list's items which does not already exist
            int idx = this.IndexOfRecord(record);
            if (idx >= 0) {
                this.fContentList.RemoveAt(idx);
                this.fFilteredCount = this.fContentList.Count;
                base.VirtualListSize = this.fContentList.Count;
            }
        }

        public void SelectItemByRec(GEDCOMRecord record)
        {
            try {
                int idx = this.IndexOfRecord(record);
                if (idx >= 0) {
                    ListViewItem item = this.Items[idx];

                    this.SelectedIndices.Clear();
                    item.Selected = true;

                    // platform: in Mono it doesn't work
                    //item.EnsureVisible();
                    this.EnsureVisible(idx);
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.SelectItemByRec(): " + ex.Message);
            }
        }

        public GEDCOMRecord GetSelectedRecord()
        {
            try {
                GEDCOMRecord result = null;

                if (!this.VirtualMode) {
                    GKListItem item = base.SelectedItem();
                    if (item != null) result = (item.Data as GEDCOMRecord);
                } else {
                    if (base.SelectedIndices.Count > 0) {
                        int index = base.SelectedIndices[0];
                        if (index >= 0 && index < fContentList.Count) {
                            result = this.fContentList[index].Record;
                        }
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.GetSelectedRecord(): " + ex.Message);
                return null;
            }
        }
    }
}
