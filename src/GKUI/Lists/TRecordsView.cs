using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Settings;
using GKSys;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public sealed class TRecordsView : TGKListView, IDisposable
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

		private void SortContents()
		{
			this.FContentList.Sort(new TListSortCompare(this.xCompare));
		}

		private int xCompare(object Item1, object Item2)
		{
			string val1, val2;
			TGEDCOMRecord rec1, rec2;

			rec1 = (TGEDCOMRecord)Item1;
			rec2 = (TGEDCOMRecord)Item2;

			if (FXSortColumn == 0) {
				val1 = TGenEngine.GetXRefNum(rec1);
				val2 = TGenEngine.GetXRefNum(rec2);
			} else {
				FListMan.Fetch(rec1);
				val1 = FListMan.GetColumnValue(FXSortColumn, FIsMainList);
				FListMan.Fetch(rec2);
				val2 = FListMan.GetColumnValue(FXSortColumn, FIsMainList);
			}

			int f = (FXSortOrder == SortOrder.Ascending ? 1 : -1);
			return SysUtils.agCompare(val1, val2) * f;
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

		private void List_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
		{
			if (e.ItemIndex < 0 || e.ItemIndex >= this.FContentList.Count)
			{
				e.Item = null;
				return;
			}

			TGEDCOMRecord rec = this.FContentList[e.ItemIndex] as TGEDCOMRecord;

			TExtListItem newItem = new TExtListItem(TGenEngine.GetId(rec).ToString());
			newItem.Data = rec;

			this.FListMan.Fetch(rec);
			this.FListMan.UpdateItem(newItem, this.FIsMainList);

			if (rec is TGEDCOMIndividualRecord) {
				//&& not((cdsFocused in State) && (cdsSelected in State))
				TGEDCOMIndividualRecord i_rec = (rec as TGEDCOMIndividualRecord);

				TGlobalOptions gOptions = GKUI.TfmGEDKeeper.Instance.Options;

				if ((i_rec.ChildToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnparented))
				{
					newItem.BackColor = System.Drawing.Color.FromArgb(0xFFCACA);
				}
				else
				{
					if ((i_rec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListPersons_HighlightUnmarried))
					{
						newItem.BackColor = System.Drawing.Color.FromArgb(0xFFFFCA);
					}
				}
			}

			e.Item = newItem;
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
		}
	}
}
