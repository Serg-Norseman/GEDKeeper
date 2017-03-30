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

using System;
using System.Collections.Generic;
using System.Windows.Forms;

using Externals;
using GKCommon;
using GKCommon.Controls;
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
                Record = record;
                ColumnValue = null;
            }
        }

        private List<ValItem> fContentList;
        private int fFilteredCount;
        private bool fIsMainList;
        private ListManager fListMan;
        private GEDCOMRecordType fRecordType;
        private int fTotalCount;
        private GEDCOMTree fTree;
        private int fXSortFactor;

        private GKListItem[] fCache;
        private int fCacheFirstItem;

        #endregion

        #region Public properties

        public int FilteredCount
        {
            get { return fFilteredCount; }
        }

        public bool IsMainList
        {
            get { return fIsMainList; }
            set { fIsMainList = value; }
        }

        public ListManager ListMan
        {
            get { return fListMan; }
        }

        public GEDCOMRecordType RecordType
        {
            get { return fRecordType; }
            set { SetRecordType(value); }
        }

        public int TotalCount
        {
            get { return fTotalCount; }
        }

        public GEDCOMTree Tree
        {
            get { return fTree; }
            set { fTree = value; }
        }

        #endregion

        public GKRecordsView() : base()
        {
            fContentList = new List<ValItem>();
            fListMan = null;
            fRecordType = GEDCOMRecordType.rtNone;
            fSortColumn = 0;
            fSortOrder = SortOrder.Ascending;
            VirtualMode = true;
            //ColumnWidthChanged += this.List_ColumnWidthChanged;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fListMan != null)
                {
                    fListMan.Dispose();
                    fListMan = null;
                }

                fContentList = null;
            }
            base.Dispose(disposing);
        }

        private void SetRecordType(GEDCOMRecordType value)
        {
            fRecordType = value;

            if (fListMan != null) {
                fListMan.Dispose();
                fListMan = null;
            }

            switch (fRecordType) {
                case GEDCOMRecordType.rtIndividual:
                    fListMan = new IndividualListMan(fTree);
                    break;

                case GEDCOMRecordType.rtFamily:
                    fListMan = new FamilyListMan(fTree);
                    break;

                case GEDCOMRecordType.rtNote:
                    fListMan = new NoteListMan(fTree);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    fListMan = new MultimediaListMan(fTree);
                    break;

                case GEDCOMRecordType.rtSource:
                    fListMan = new SourceListMan(fTree);
                    break;

                case GEDCOMRecordType.rtRepository:
                    fListMan = new RepositoryListMan(fTree);
                    break;

                case GEDCOMRecordType.rtGroup:
                    fListMan = new GroupListMan(fTree);
                    break;

                case GEDCOMRecordType.rtResearch:
                    fListMan = new ResearchListMan(fTree);
                    break;

                case GEDCOMRecordType.rtTask:
                    fListMan = new TaskListMan(fTree);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    fListMan = new CommunicationListMan(fTree);
                    break;

                case GEDCOMRecordType.rtLocation:
                    fListMan = new LocationListMan(fTree);
                    break;

                case GEDCOMRecordType.rtSubmission:
                    fListMan = null;
                    break;

                case GEDCOMRecordType.rtSubmitter:
                    fListMan = null;
                    break;
            }
        }

        private void SortContents()
        {
            try {
                int num = fContentList.Count;
                for (int i = 0; i < num; i++) {
                    ValItem valItem = fContentList[i];
                    GEDCOMRecord rec = valItem.Record;

                    if (fSortColumn == 0) {
                        valItem.ColumnValue = rec.GetId();
                    } else {
                        fListMan.Fetch(rec);
                        valItem.ColumnValue = fListMan.GetColumnInternalValue(fSortColumn);
                    }
                }

                fXSortFactor = (fSortOrder == SortOrder.Ascending ? 1 : -1);
                ListTimSort<ValItem>.Sort(fContentList, CompareItems);

                // clear cache
                fCache = null;
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

            return compRes * fXSortFactor;
        }

        private GKListItem GetListItem(int itemIndex)
        {
            GKListItem newItem;

            if (itemIndex < 0 || itemIndex >= fContentList.Count) {
                newItem = null;
            } else {
                GEDCOMRecord rec = fContentList[itemIndex].Record;

                newItem = new GKListItem(rec.GetXRefNum(), rec);

                fListMan.Fetch(rec);
                fListMan.UpdateItem(newItem, fIsMainList);
            }

            return newItem;
        }

        #region Protected methods

        protected override void OnColumnWidthChanged(ColumnWidthChangedEventArgs e)
        {
            if (fListMan == null || fUpdateCount > 0) return;

            try
            {
                switch (fRecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        fListMan.ChangeColumnWidth(e.ColumnIndex, Columns[e.ColumnIndex].Width);
                        break;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKRecordsView.OnColumnWidthChanged(): " + ex.Message);
            }

            base.OnColumnWidthChanged(e);
        }

        protected override void InternalColumnClick(ColumnClickEventArgs e)
        {
            GEDCOMRecord rec = GetSelectedRecord();

            SortOrder prevOrder = GetColumnSortOrder(e.Column);
            fSortOrder = (prevOrder == SortOrder.Ascending) ? SortOrder.Descending : SortOrder.Ascending;
            fSortColumn = e.Column;

            SortContents();
            SelectItemByRec(rec);
        }

        protected override void OnCacheVirtualItems(CacheVirtualItemsEventArgs e)
        {
            // Only recreate the cache if we need to.
            if (fCache != null && e.StartIndex >= fCacheFirstItem && e.EndIndex <= fCacheFirstItem + fCache.Length) return;

            fCacheFirstItem = e.StartIndex;
            int length = e.EndIndex - e.StartIndex + 1;

            fCache = new GKListItem[length];
            for (int i = 0; i < length; i++)
            {
                fCache[i] = GetListItem(fCacheFirstItem + i);
            }
        }

        protected override void OnRetrieveVirtualItem(RetrieveVirtualItemEventArgs e)
        {
            // If we have the item cached, return it. Otherwise, recreate it.
            if (fCache != null && e.ItemIndex >= fCacheFirstItem && e.ItemIndex < fCacheFirstItem + fCache.Length) {
                e.Item = fCache[e.ItemIndex - fCacheFirstItem];
            } else {
                e.Item = GetListItem(e.ItemIndex);
            }
        }

        #endregion

        #region Public methods

        public void UpdateTitles()
        {
            try {
                if (fListMan == null) return;

                BeginUpdate();
                try
                {
                    Columns.Clear();
                    fListMan.UpdateColumns(this, fIsMainList);
                }
                finally
                {
                    EndUpdate();
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.UpdateTitles(): " + ex.Message);
            }
        }

        public void UpdateContents(ShieldState shieldState, bool titles, int autosizeColumn)
        {
            if (fListMan == null) return;

            try
            {
                GEDCOMRecord tempRec = GetSelectedRecord();

                fTotalCount = 0;
                fFilteredCount = 0;

                if (titles) {
                    UpdateTitles();
                }

                BeginUpdate();
                try
                {
                    //SelectedIndices.Clear();
                    //SelectedItems.Clear();

                    fListMan.InitFilter();

                    int contentSize = fTree.RecordsCount;

                    fContentList.Clear();
                    fContentList.Capacity = contentSize;

                    for (int i = 0; i < contentSize; i++) {
                        GEDCOMRecord rec = fTree[i];

                        if (rec.RecordType == fRecordType) {
                            fTotalCount++;

                            fListMan.Fetch(rec);
                            if (fListMan.CheckFilter(shieldState)) {
                                fContentList.Add(new ValItem(rec));
                            }
                        }
                    }

                    SortContents();

                    fFilteredCount = fContentList.Count;
                    VirtualListSize = fContentList.Count;

                    #if __MonoCS__
                    if (this.fContentList.Count != 0)
                    {
                        this.TopItem = this.Items[0];
                    }
                    #endif

                    ResizeColumn(autosizeColumn);
                }
                finally
                {
                    EndUpdate();
                }

                if (tempRec != null) SelectItemByRec(tempRec);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKRecordsView.UpdateContents(): " + ex.Message);
            }
        }

        public List<GEDCOMRecord> GetContentList()
        {
            int size = fContentList.Count;
            var result = new List<GEDCOMRecord>(size);

            for (int i = 0; i < size; i++) {
                result.Add(fContentList[i].Record);
            }

            return result;
        }

        public int IndexOfRecord(GEDCOMRecord record)
        {
            int result = -1;

            int num = fContentList.Count;
            for (int i = 0; i < num; i++) {
                if (fContentList[i].Record == record) {
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
            int idx = IndexOfRecord(record);
            if (idx >= 0) {
                fContentList.RemoveAt(idx);
                fFilteredCount = fContentList.Count;
                VirtualListSize = fContentList.Count;
            }
        }

        public void SelectItemByRec(GEDCOMRecord record)
        {
            try {
                int idx = IndexOfRecord(record);
                if (idx >= 0) {
                    ListViewItem item = Items[idx];

                    SelectedIndices.Clear();
                    item.Selected = true;

                    // platform: in Mono it doesn't work
                    //item.EnsureVisible();
                    EnsureVisible(idx);
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.SelectItemByRec(): " + ex.Message);
            }
        }

        public GEDCOMRecord GetSelectedRecord()
        {
            try {
                GEDCOMRecord result = null;

                if (!VirtualMode) {
                    GKListItem item = GetSelectedItem();
                    if (item != null) result = (item.Data as GEDCOMRecord);
                } else {
                    if (SelectedIndices.Count > 0) {
                        int index = SelectedIndices[0];
                        if (index >= 0 && index < fContentList.Count) {
                            result = fContentList[index].Record;
                        }
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.LogWrite("GKRecordsView.GetSelectedRecord(): " + ex.Message);
                return null;
            }
        }

        #endregion
    }
}
