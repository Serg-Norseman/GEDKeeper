using System;
using System.Collections.Generic;
using System.Windows.Forms;

using ExtUtils;
using ExtUtils.TimSort;
using GedCom551;
using GKCore;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public sealed class GKRecordsView : GKListView
	{
		#region Private fields

		private readonly ExtList<TGEDCOMRecord> fContentList;

		private int FFilteredCount;
		private bool FIsMainList;
		private TListManager FListMan;
		private TGEDCOMRecordType FRecordType;
		private int FTotalCount;
		private TGEDCOMTree FTree;

		private int FXSortColumn;
		private SortOrder FXSortOrder;
		private int FXSortFactor;

		private GKListItem[] FCache;
		private int FCacheFirstItem;

		#endregion

		#region Public properties

		public ExtList<TGEDCOMRecord> ContentList
		{
			get { return this.fContentList; }
		}

		public int FilteredCount
		{
			get { return this.FFilteredCount; }
		}

		public bool IsMainList
		{
			get { return this.FIsMainList; }
			set { this.FIsMainList = value; }
		}

		public TListManager ListMan
		{
			get { return this.FListMan; }
		}

		public TGEDCOMRecordType RecordType
		{
			get { return this.FRecordType; }
			set { this.SetRecordType(value); }
		}

		public int TotalCount
		{
			get { return this.FTotalCount; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
		}

		#endregion

		public GKRecordsView() : base()
		{
			this.fContentList = new ExtList<TGEDCOMRecord>();
			this.FListMan = null;
			this.FRecordType = TGEDCOMRecordType.rtNone;
            this.FXSortColumn = 0;
            this.FXSortOrder = SortOrder.Ascending;

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
				if (this.FListMan != null)
				{
					this.FListMan.Dispose();
					this.FListMan = null;
				}
				this.fContentList.Dispose();
			}
			base.Dispose(disposing);
		}

		private void List_ColumnClick(object sender, ColumnClickEventArgs e)
		{
			TGEDCOMRecord rec = this.GetSelectedRecord();

			if (e.Column == FXSortColumn) {
				FXSortOrder = (FXSortOrder == SortOrder.Ascending ? SortOrder.Descending : SortOrder.Ascending);
			} else {
				FXSortColumn = e.Column;
				FXSortOrder = SortOrder.Ascending;
			}

			SortContents();

			this.SelectItemByRec(rec);
			base.Invalidate();
		}

		private class ValItem
		{
			public object ColumnValue;
			public TGEDCOMRecord Record;
		}

		private void SortContents()
		{
			List<ValItem> buffer = new List<ValItem>();

			int count = fContentList.Count;
			for (int i = 0; i < count; i++) {
				TGEDCOMRecord rec = fContentList[i] as TGEDCOMRecord;

				ValItem vi = new ValItem();
				vi.Record = rec;

				if (FXSortColumn == 0) {
					vi.ColumnValue = rec.aux_GetId();
				} else {
					FListMan.Fetch(rec);
					vi.ColumnValue = FListMan.GetColumnValue(FXSortColumn);
				}

				buffer.Add(vi);
			}

			FXSortFactor = (FXSortOrder == SortOrder.Ascending ? 1 : -1);
			ListTimSort<ValItem>.Sort(buffer, xCompare);

			for (int i = 0; i < count; i++) fContentList[i] = buffer[i].Record;

			this.ClearCache();
		}

		private int xCompare(ValItem item1, ValItem item2)
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

			return compRes * FXSortFactor;
		}

		private GKListItem GetListItem(int itemIndex)
		{
			GKListItem newItem;

			if (itemIndex < 0 || itemIndex >= this.fContentList.Count) {
				newItem = null;
			} else {
				TGEDCOMRecord rec = this.fContentList[itemIndex] as TGEDCOMRecord;

				newItem = new GKListItem(rec.aux_GetXRefNum());
				newItem.Data = rec;

				this.FListMan.Fetch(rec);
				this.FListMan.UpdateItem(newItem, this.FIsMainList);
			}

			return newItem;
		}

		private void List_CacheVirtualItems(object sender, CacheVirtualItemsEventArgs e)
		{
			// Only recreate the cache if we need to.
			if (FCache != null && e.StartIndex >= FCacheFirstItem && e.EndIndex <= FCacheFirstItem + FCache.Length) return;

			FCacheFirstItem = e.StartIndex;
			int length = e.EndIndex - e.StartIndex + 1;

			FCache = new GKListItem[length];
			for (int i = 0; i < FCache.Length; i++)
			{
				FCache[i] = GetListItem(FCacheFirstItem + i);
			}
		}

		private void List_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
		{
			// If we have the item cached, return it. Otherwise, recreate it.
			if (FCache != null && e.ItemIndex >= FCacheFirstItem && e.ItemIndex < FCacheFirstItem + FCache.Length) {
				e.Item = FCache[e.ItemIndex - FCacheFirstItem];
			} else {
				e.Item = GetListItem(e.ItemIndex);
			}
		}

		private void ClearCache()
		{
			FCache = null;
		}

		private void SetRecordType(TGEDCOMRecordType value)
		{
			this.FRecordType = value;

			if (this.FListMan != null) {
				this.FListMan.Dispose();
				this.FListMan = null;
			}

			switch (this.FRecordType) {
				case TGEDCOMRecordType.rtIndividual:
					this.FListMan = new TIndividualListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtFamily:
					this.FListMan = new TFamilyListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtNote:
					this.FListMan = new TNoteListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtMultimedia:
					this.FListMan = new TMultimediaListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtSource:
					this.FListMan = new TSourceListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtRepository:
					this.FListMan = new TRepositoryListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtGroup:
					this.FListMan = new TGroupListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtResearch:
					this.FListMan = new TResearchListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtTask:
					this.FListMan = new TTaskListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtCommunication:
					this.FListMan = new TCommunicationListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtLocation:
					this.FListMan = new TLocationListMan(this.FTree);
					break;

				case TGEDCOMRecordType.rtSubmission:
					this.FListMan = null;
					break;

				case TGEDCOMRecordType.rtSubmitter:
					this.FListMan = null;
					break;
			}
		}

		public void UpdateTitles()
		{
            if (this.FListMan == null) return;

			this.BeginUpdate();
			try
			{
				this.Columns.Clear();
				this.FListMan.UpdateColumns(this, this.FIsMainList);
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public void UpdateContents(ShieldState shieldState, bool titles, int autosizeColumn)
		{
			if (this.FListMan == null) return;
			
			try
			{
				TGEDCOMRecord tempRec = this.GetSelectedRecord();

				this.FTotalCount = 0;
				this.FFilteredCount = 0;

				if (titles) {
					this.UpdateTitles();
				}

				base.BeginUpdate();
				try
				{
					this.FListMan.InitFilter();

					this.fContentList.Clear();
					int num = this.FTree.RecordsCount;
					for (int i = 0; i < num; i++) {
						TGEDCOMRecord rec = this.FTree[i];

						if (rec.RecordType == this.FRecordType) {
							this.FTotalCount++;
							this.FListMan.Fetch(rec);
							if (this.FListMan.CheckFilter(shieldState))
							{
								this.fContentList.Add(rec);
							}
						}
					}

					this.FFilteredCount = this.fContentList.Count;
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

		public void DeleteRecord(TGEDCOMRecord aRec)
		{
			// защита от сбоев: при удалении в режиме диаграмм, между фактическим удалением записи и 
			// обновлением списка успевает пройти несколько запросов на обновление пунктов списка
			// которые могут быть уже удалены
			int res = this.fContentList.Remove(aRec);
			if (res >= 0) {
				this.FFilteredCount = this.fContentList.Count;
				base.VirtualListSize = this.fContentList.Count;
			}
		}

		public TGEDCOMRecord GetSelectedRecord()
		{
			TGEDCOMRecord result = null;

			if (!this.VirtualMode) {
				GKListItem item = base.SelectedItem();
				if (item != null) result = (item.Data as TGEDCOMRecord);
			} else {
				if (base.SelectedIndices.Count > 0) {
					int index = base.SelectedIndices[0];
					if (index >= 0 && index < fContentList.Count) {
						result = fContentList[index] as TGEDCOMRecord;
					}					
				}
			}

			return result;
		}

		public void SelectItemByRec(TGEDCOMRecord aRec)
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
