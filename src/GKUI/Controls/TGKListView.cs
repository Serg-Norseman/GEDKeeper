using System;
using System.Collections;
using System.Windows.Forms;

using GKCore.Sys;

namespace GKUI.Controls
{
	public class TGKListView : ListView
	{
		private class TListViewColumnSorter : IComparer
		{
			private int FSortColumn;
			private SortOrder FSortOrder;
			private CaseInsensitiveComparer FObjCompare;

			public int SortColumn
			{
				get { return this.FSortColumn; }
				set { this.FSortColumn = value; }
			}

			public SortOrder Order
			{
				get { return this.FSortOrder; }
				set { this.FSortOrder = value; }
			}

			public TListViewColumnSorter()
			{
				this.FSortColumn = 0;
				this.FSortOrder = SortOrder.None;
				this.FObjCompare = new CaseInsensitiveComparer();
			}

			int IComparer.Compare(object x, object y)
			{
				ListViewItem item = x as ListViewItem;
				ListViewItem item2 = y as ListViewItem;
				int comp_res = SysUtils.agCompare(item.SubItems[this.FSortColumn].Text, item2.SubItems[this.FSortColumn].Text);
				int Result;
				if (this.FSortOrder == SortOrder.Ascending) {
					Result = comp_res;
				} else {
					if (this.FSortOrder == SortOrder.Descending) {
						Result = -comp_res;
					} else {
						Result = 0;
					}
				}
				return Result;
			}
		}

		private TListViewColumnSorter lvwColumnSorter;

		public int SortColumn
		{
			get { return this.lvwColumnSorter.SortColumn; }
			set { this.lvwColumnSorter.SortColumn = value; }
		}

		public TGKListView(Control AOwner)
		{
			this.lvwColumnSorter = new TGKListView.TListViewColumnSorter();
			base.ListViewItemSorter = this.lvwColumnSorter;
			base.ColumnClick += new ColumnClickEventHandler(this.lvColumnClick);
			//this.DoubleBuffered = true;
		}

		public void UnsetSorter()
		{
			base.ColumnClick -= new ColumnClickEventHandler(this.lvColumnClick);
		}

		private void lvColumnClick(object sender, ColumnClickEventArgs e)
		{
			if (e.Column == this.lvwColumnSorter.SortColumn)
			{
				if (this.lvwColumnSorter.Order == SortOrder.Ascending) {
					this.lvwColumnSorter.Order = SortOrder.Descending;
				} else {
					this.lvwColumnSorter.Order = SortOrder.Ascending;
				}
			} else {
				this.lvwColumnSorter.SortColumn = e.Column;
				this.lvwColumnSorter.Order = SortOrder.Ascending;
			}
			base.Sort();
		}

		public void AddListColumn(string aCaption, int aWidth, bool aAutoSize)
		{
			if (aAutoSize) {
				aWidth = -1;
			}
			ColumnHeader col = base.Columns.Add(aCaption, aWidth, HorizontalAlignment.Left);
		}

		public TExtListItem AddItem(string title, object data)
		{
			TExtListItem Result = new TExtListItem(title);
			Result.Data = data;
			base.Items.Add(Result);
			return Result;
		}

		public TExtListItem SelectedItem()
		{
			TExtListItem Result;

			if (base.SelectedItems.Count <= 0) {
				Result = null;
			} else {
				Result = (base.SelectedItems[0] as TExtListItem);
			}

			return Result;
		}

		public void ResizeColumn(int aColumnIndex)
		{
			if (aColumnIndex >= 0) {
				this.AutoResizeColumn(aColumnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

				if (this.Columns[aColumnIndex].Width < 20) {
					this.AutoResizeColumn(aColumnIndex, ColumnHeaderAutoResizeStyle.HeaderSize);
				}
			}
		}
	}
}
