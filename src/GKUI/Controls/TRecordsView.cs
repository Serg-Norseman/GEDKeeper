using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using Ext.TimSort;
using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public sealed class TRecordsView : GKListView, IDisposable
	{
		private TList FContentList;
		private int FFilteredCount;
		private bool FIsMainList;
		private TListManager FListMan;
		private TGEDCOMRecordType FRecordType;
		private int FTotalCount;
		private TGEDCOMTree FTree;

		private int FXSortColumn = 0;
		private SortOrder FXSortOrder = SortOrder.Ascending;
		private int FXSortFactor;

		private GKListItem[] FCache;
		private int FCacheFirstItem;

		public TList ContentList
		{
			get { return this.FContentList; }
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

		public TRecordsView() : base()
		{
			this.FContentList = new TList();
			this.FListMan = null;
			this.FRecordType = TGEDCOMRecordType.rtNone;

			base.UnsetSorter();
			base.ColumnClick += new ColumnClickEventHandler(this.List_ColumnClick);

			base.RetrieveVirtualItem += new RetrieveVirtualItemEventHandler(this.List_RetrieveVirtualItem);
			base.CacheVirtualItems += new CacheVirtualItemsEventHandler(this.List_CacheVirtualItems);
			base.VirtualMode = true;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				if (this.FListMan != null)
				{
					this.FListMan.Dispose();
					this.FListMan = null;
				}
				this.FContentList.Dispose();
			}
			base.Dispose(Disposing);
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
			public object Record;
		}

		private void SortContents()
		{
			List<ValItem> buffer = new List<ValItem>();

			int count = FContentList.Count;
			for (int i = 0; i < count; i++) {
				TGEDCOMRecord rec = FContentList[i] as TGEDCOMRecord;

				ValItem vi = new ValItem();
				vi.Record = rec;

				if (FXSortColumn == 0) {
					vi.ColumnValue = TGenEngine.GetId(rec);
				} else {
					FListMan.Fetch(rec);
					vi.ColumnValue = FListMan.GetColumnValueEx(FXSortColumn);
				}

				buffer.Add(vi);
			}

			FXSortFactor = (FXSortOrder == SortOrder.Ascending ? 1 : -1);
			ListTimSort<ValItem>.Sort(buffer, xCompare);

			for (int i = 0; i < count; i++) FContentList[i] = buffer[i].Record;

			this.ClearCache();
		}

		private int xCompare(ValItem Item1, ValItem Item2)
		{
			int comp_res = ((IComparable)Item1.ColumnValue).CompareTo(Item2.ColumnValue);

			if (Item1.ColumnValue is string && Item2.ColumnValue is string) {
				string Str1 = (string)Item1.ColumnValue;
				string Str2 = (string)Item2.ColumnValue;

				if (Str1 != "" && Str2 == "") comp_res = -1;
				if (Str1 == "" && Str2 != "") comp_res = 1;
			} else {
				if (Item1.ColumnValue != null && Item2.ColumnValue == null) comp_res = -1;
				if (Item1.ColumnValue == null && Item2.ColumnValue != null) comp_res = 1;
			}

			return comp_res * FXSortFactor;
		}

		private GKListItem GetListItem(int ItemIndex)
		{
			GKListItem newItem;

			if (ItemIndex < 0 || ItemIndex >= this.FContentList.Count) {
				newItem = null;
			} else {
				TGEDCOMRecord rec = this.FContentList[ItemIndex] as TGEDCOMRecord;

				newItem = new GKListItem(TGenEngine.GetId(rec).ToString());
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

		private void SetRecordType([In] TGEDCOMRecordType Value)
		{
			this.FRecordType = Value;

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

		public void UpdateContents(TGenEngine.TShieldState aShieldState, bool aTitles, int aAutoSizeColumn)
		{
			try
			{
				this.FTotalCount = 0;
				this.FFilteredCount = 0;

				if (aTitles && this.FListMan != null)
				{
					this.FListMan.UpdateTitles(this, this.FIsMainList);
				}

				base.BeginUpdate();
				try
				{
					this.FListMan.InitFilter();

					this.FContentList.Clear();
					int num = this.FTree.RecordsCount - 1;
					for (int i = 0; i <= num; i++) {
						TGEDCOMRecord rec = this.FTree[i];

						if (rec.RecordType == this.FRecordType) {
							this.FTotalCount++;
							this.FListMan.Fetch(rec);
							if (this.FListMan.CheckFilter(aShieldState))
							{
								this.FContentList.Add(rec);
							}
						}
					}

					this.FFilteredCount = this.FContentList.Count;
					VirtualListSize = this.FContentList.Count;

					this.SortContents();
					base.ResizeColumn(aAutoSizeColumn);
				}
				finally
				{
					base.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("UpdateContents(): " + E.Message);
			}
		}

		public void UpdateTitles()
		{
			this.FListMan.UpdateTitles(this, this.FIsMainList);
		}

		public void DeleteRecord(TGEDCOMRecord aRec)
		{
			// защита от сбоев: при удалении в режиме диаграмм, между фактическим удалением записи и 
			// обновлением списка успевает пройти несколько запросов на обновление пунктов списка
			// которые могут быть уже удалены
			int res = this.FContentList.Remove(aRec);
			if (res >= 0)
			{
				this.FFilteredCount = this.FContentList.Count;
				VirtualListSize = this.FContentList.Count;
			}
		}

		public TGEDCOMRecord GetSelectedRecord()
		{
			TGEDCOMRecord Result = null;

			if (!this.VirtualMode) {
				GKListItem item = base.SelectedItem();
				if (item != null) Result = (item.Data as TGEDCOMRecord);
			} else {
				if (base.SelectedIndices.Count > 0) {
					int index = base.SelectedIndices[0];
					if (index >= 0 && index < FContentList.Count) {
						Result = FContentList[index] as TGEDCOMRecord;
					}					
				}
			}

			return Result;
		}

		public void SelectItemByRec(TGEDCOMRecord aRec)
		{
			int idx = FContentList.IndexOf(aRec);
			if (idx >= 0) {
				ListViewItem item = this.Items[idx];
				this.SelectedIndices.Clear();
				item.Selected = true;
				item.EnsureVisible();
			}
		}
	}
}
