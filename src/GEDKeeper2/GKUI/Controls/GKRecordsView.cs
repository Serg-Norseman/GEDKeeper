using System;
using System.Collections.Generic;
using System.Windows.Forms;

using ExtUtils;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Types;
using GKUI.Lists;

namespace GKUI.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GKRecordsView : GKListView
	{
		#region Private fields

		private List<GEDCOMRecord> fContentList;

		private int fFilteredCount;
		private bool fIsMainList;
		private ListManager fListMan;
		private GEDCOMRecordType fRecordType;
		private int fTotalCount;
		private GEDCOMTree fTree;

		private int fXSortColumn;
		private SortOrder fXSortOrder;
		private int fXSortFactor;

		private GKListItem[] fCache;
		private int fCacheFirstItem;

		#endregion

		#region Public properties

		public List<GEDCOMRecord> ContentList
		{
			get { return this.fContentList; }
		}

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
			this.fContentList = new List<GEDCOMRecord>();
			this.fListMan = null;
			this.fRecordType = GEDCOMRecordType.rtNone;
            this.fXSortColumn = 0;
            this.fXSortOrder = SortOrder.Ascending;

			base.UnsetSorter();
			base.ColumnClick += this.List_ColumnClick;
			base.RetrieveVirtualItem += this.List_RetrieveVirtualItem;
			base.CacheVirtualItems += this.List_CacheVirtualItems;
			base.VirtualMode = true;
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
				//this.fContentList.Dispose();
			    this.fContentList = null;
			}
			base.Dispose(disposing);
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

			SortContents();

			this.SelectItemByRec(rec);
			base.Invalidate();
		}

		private class ValItem
		{
			public readonly object ColumnValue;
			public readonly GEDCOMRecord Record;

            public ValItem(object columnValue, GEDCOMRecord record)
            {
                this.ColumnValue = columnValue;
                this.Record = record;
            }
		}

		private void SortContents()
		{
			List<ValItem> buffer = new List<ValItem>();

			int count = fContentList.Count;
			for (int i = 0; i < count; i++) {
				GEDCOMRecord rec = fContentList[i];

			    object columnValue;
				if (fXSortColumn == 0) {
					columnValue = rec.aux_GetId();
				} else {
					fListMan.Fetch(rec);
					columnValue = fListMan.GetColumnValue(fXSortColumn);
				}

                buffer.Add(new ValItem(columnValue, rec));
			}

			fXSortFactor = (fXSortOrder == SortOrder.Ascending ? 1 : -1);
			ListTimSort<ValItem>.Sort(buffer, XCompare);

			for (int i = 0; i < count; i++) fContentList[i] = buffer[i].Record;

			this.ClearCache();
		}

		private int XCompare(ValItem item1, ValItem item2)
		{
			int compRes = ((IComparable)item1.ColumnValue).CompareTo(item2.ColumnValue);

			if (item1.ColumnValue is string && item2.ColumnValue is string) {
				string str1 = (string)item1.ColumnValue;
				string str2 = (string)item2.ColumnValue;

				if (str1 != "" && str2 == "") compRes = -1;
				if (str1 == "" && str2 != "") compRes = 1;
			} else {
				if (item1.ColumnValue != null && item2.ColumnValue == null) compRes = -1;
				if (item1.ColumnValue == null && item2.ColumnValue != null) compRes = 1;
			}

			return compRes * fXSortFactor;
		}

		private GKListItem GetListItem(int itemIndex)
		{
			GKListItem newItem;

			if (itemIndex < 0 || itemIndex >= this.fContentList.Count) {
				newItem = null;
			} else {
				GEDCOMRecord rec = this.fContentList[itemIndex];

				newItem = new GKListItem(rec.aux_GetXRefNum(), rec);

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

		private void ClearCache()
		{
			fCache = null;
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
            if (this.fListMan == null) return;

			this.BeginUpdate();
			try
			{
				this.Columns.Clear();
				this.fListMan.UpdateColumns(this, this.fIsMainList);
			}
			finally
			{
				this.EndUpdate();
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

				base.BeginUpdate();
				try
				{
					this.fListMan.InitFilter();

					this.fContentList.Clear();
					int num = this.fTree.RecordsCount;
					for (int i = 0; i < num; i++) {
						GEDCOMRecord rec = this.fTree[i];

						if (rec.RecordType == this.fRecordType) {
							this.fTotalCount++;
							this.fListMan.Fetch(rec);
							if (this.fListMan.CheckFilter(shieldState))
							{
								this.fContentList.Add(rec);
							}
						}
					}

					this.fFilteredCount = this.fContentList.Count;
					VirtualListSize = this.fContentList.Count;

					this.SortContents();
					base.ResizeColumn(autosizeColumn);
				}
				finally
				{
					base.EndUpdate();
				}

				if (tempRec != null) this.SelectItemByRec(tempRec);
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("GKRecordsView.UpdateContents(): " + ex.Message);
			}
		}

		public void DeleteRecord(GEDCOMRecord record)
		{
			// защита от сбоев: при удалении в режиме диаграмм, между фактическим удалением записи и 
			// обновлением списка успевает пройти несколько запросов на обновление пунктов списка
			// которые могут быть уже удалены
		    int recIndex = this.fContentList.IndexOf(record);
			if (recIndex >= 0) {
                this.fContentList.RemoveAt(recIndex);
                this.fFilteredCount = this.fContentList.Count;
				base.VirtualListSize = this.fContentList.Count;
			}
		}

		public GEDCOMRecord GetSelectedRecord()
		{
			GEDCOMRecord result = null;

			if (!this.VirtualMode) {
				GKListItem item = base.SelectedItem();
				if (item != null) result = (item.Data as GEDCOMRecord);
			} else {
				if (base.SelectedIndices.Count > 0) {
					int index = base.SelectedIndices[0];
					if (index >= 0 && index < fContentList.Count) {
						result = fContentList[index];
					}					
				}
			}

			return result;
		}

		public void SelectItemByRec(GEDCOMRecord aRec)
		{
			int idx = this.fContentList.IndexOf(aRec);
			if (idx >= 0) {
				ListViewItem item = this.Items[idx];
				this.SelectedIndices.Clear();
				item.Selected = true;
				item.EnsureVisible();
			}
		}
	}
}
