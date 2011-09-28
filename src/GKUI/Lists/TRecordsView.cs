using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI.Lists
{
	public class TRecordsView : TGKListView, IDisposable
	{
		/*private class TRow
		{
			public int Index;
			public string Value;
			public int Age;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}*/

		private TList FContentList;
		private int FFilteredCount;
		private bool FIsMainList;
		private TListManager FListMan;
		private TGEDCOMRecordType FRecordType;
		private int FTotalCount;
		private TGEDCOMTree FTree;

		private int FXSortColumn = 0;
		private SortOrder FXSortOrder = SortOrder.Ascending;

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

		public TRecordsView(Control AOwner) : base(null)
		{
			this.FContentList = new TList();
			this.FListMan = null;
			this.FRecordType = TGEDCOMRecordType.rtNone;

			base.UnsetSorter();
			base.ColumnClick += new ColumnClickEventHandler(this.List_ColumnClick);

			base.RetrieveVirtualItem += new RetrieveVirtualItemEventHandler(this.List_RetrieveVirtualItem);
			base.VirtualMode = true;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				if (this.FListMan != null)
				{
					object fListMan = this.FListMan;
					SysUtils.FreeAndNil(ref fListMan);
					this.FListMan = (fListMan as TListManager);
				}
				this.FContentList.Free();
			}
			base.Dispose(Disposing);
		}

		private void List_ColumnClick(object sender, ColumnClickEventArgs e)
		{
			TGEDCOMRecord rec = this.GetSelectedRecord();

			if (e.Column == FXSortColumn) {
				if (FXSortOrder == SortOrder.Ascending) {
					FXSortOrder = SortOrder.Descending;
				} else {
					FXSortOrder = SortOrder.Ascending;
				}
			} else {
				FXSortColumn = e.Column;
				FXSortOrder = SortOrder.Ascending;
			}

			SortContents();

			this.SelectItemByRec(rec);
			base.Invalidate();
		}

		private void SortContents()
		{
			SysUtils.QuickSort(this.FContentList, new SysUtils.TSortCompareFunc(this.xCompare));
		}

		private int xCompare(object Item1, object Item2)
		{
			string val1, val2;
			int f;

			if (FXSortColumn == 0) {
				val1 = TGenEngine.GetXRefNum((TGEDCOMRecord)Item1);
				val2 = TGenEngine.GetXRefNum((TGEDCOMRecord)Item2);
			} else {
				FListMan.Fetch((Item1 as TGEDCOMRecord));
				val1 = FListMan.GetColumnValue(FXSortColumn, FIsMainList);
				FListMan.Fetch((Item2 as TGEDCOMRecord));
				val2 = FListMan.GetColumnValue(FXSortColumn, FIsMainList);
			}

			if (FXSortOrder == SortOrder.Ascending) { f = 1; } else { f = -1; }
			int Result = SysUtils.agCompare(val1, val2) * f;

			return Result;
		}

		private void SetRecordType([In] TGEDCOMRecordType Value)
		{
			this.FRecordType = Value;

			if (this.FListMan != null) {
				object fListMan = this.FListMan;
				SysUtils.FreeAndNil(ref fListMan);
				this.FListMan = (fListMan as TListManager);
			}

			switch (this.FRecordType) {
				case TGEDCOMRecordType.rtIndividual:
				{
					this.FListMan = new TIndividualListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtFamily:
				{
					this.FListMan = new TFamilyListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtNote:
				{
					this.FListMan = new TNoteListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtMultimedia:
				{
					this.FListMan = new TMultimediaListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtSource:
				{
					this.FListMan = new TSourceListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtRepository:
				{
					this.FListMan = new TRepositoryListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtGroup:
				{
					this.FListMan = new TGroupListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtResearch:
				{
					this.FListMan = new TResearchListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtTask:
				{
					this.FListMan = new TTaskListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtCommunication:
				{
					this.FListMan = new TCommunicationListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtLocation:
				{
					this.FListMan = new TLocationListMan(this.FTree);
					break;
				}
				case TGEDCOMRecordType.rtSubmission:
				{
					this.FListMan = null;
					break;
				}
				case TGEDCOMRecordType.rtSubmitter:
				{
					this.FListMan = null;
					break;
				}
			}
		}

		private void List_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
		{
			TGEDCOMRecord rec = this.FContentList[e.ItemIndex] as TGEDCOMRecord;
			e.Item = new TExtListItem(TGenEngine.GetId(rec).ToString());
			((TExtListItem)e.Item).Data = rec;

			this.FListMan.Fetch(rec);
			this.FListMan.UpdateItem((TExtListItem)e.Item, this.FIsMainList);

			if (rec is TGEDCOMIndividualRecord) {
				//&& not((cdsFocused in State) && (cdsSelected in State))
				TGEDCOMIndividualRecord i_rec = (rec as TGEDCOMIndividualRecord);

				if ((i_rec.ChildToFamilyLinks.Count == 0) && (GKUI.TfmGEDKeeper.Instance.Options.ListPersons_HighlightUnparented))
				{
					e.Item.BackColor = System.Drawing.Color.FromArgb(0xFFCACA);
				}
				else
				{
					if ((i_rec.SpouseToFamilyLinks.Count == 0) && (GKUI.TfmGEDKeeper.Instance.Options.ListPersons_HighlightUnmarried))
					{
						e.Item.BackColor = System.Drawing.Color.FromArgb(0xFFFFCA);
					}
				}
			}
		}

		public void UpdateContents(TGenEngine.TShieldState aShieldState, bool aTitles, TPersonsFilter aFilter, int aAutoSizeColumn)
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
					this.FListMan.InitFilter(aFilter);

					this.FContentList.Clear();
					int num = this.FTree.RecordsCount - 1;
					for (int i = 0; i <= num; i++) {
						TGEDCOMRecord rec = this.FTree.GetRecord(i);

						if (rec.RecordType == this.FRecordType) {
							this.FTotalCount++;
							this.FListMan.Fetch(rec);
							if (this.FListMan.CheckFilter(aFilter, aShieldState))
							{
								this.FContentList.Add(rec);
							}
						}
					}

					this.FFilteredCount = this.FContentList.Count;
					VirtualListSize = this.FContentList.Count;

					SortContents();

					/*base.Items.Clear();
					for (int i = 0; i <= this.FContentList.Count - 1; i++) {
						TGEDCOMRecord rec = this.FContentList[i] as TGEDCOMRecord;
						TExtListItem item = base.AddItem(TGenEngine.GetId(rec).ToString(), rec);
						this.FListMan.Fetch(rec);
						this.FListMan.UpdateItem(item, this.FIsMainList);
					}

					TGKSys.QuickSort(this.FContentList, new TGKSys.TSortCompareFunc(this.xCompare));
					*/

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
			this.FContentList.Remove(aRec);
		}

		public TGEDCOMRecord GetSelectedRecord()
		{
			TGEDCOMRecord Result = null;

			if (!this.VirtualMode) {
				TExtListItem item = base.SelectedItem();
				if (item != null) Result = (item.Data as TGEDCOMRecord);
			} else {
				if (base.SelectedIndices.Count > 0) {
					int index = base.SelectedIndices[0];
					if (index >= 0 && index < FContentList.Count) {
						Result = (TGEDCOMRecord)FContentList[index];
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

			/*for (int idx = 0; idx <= base.Items.Count - 1; idx++) {
				TExtListItem item = base.Items[idx] as TExtListItem;
				if (object.Equals(item.Data, aRec))
				{
					base.SelectedItems.Clear();
					item.Selected = true;
					item.EnsureVisible();
				}
			}*/
		}

	}
}
