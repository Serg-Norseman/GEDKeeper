using System;
using System.Collections;
using System.Windows.Forms;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class GKListView : ListView
	{
		private class TListViewColumnSorter : IComparer
		{
			private int FSortColumn;
			private SortOrder FSortOrder;

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
			}

			int IComparer.Compare(object x, object y)
			{
				int Result = 0;

				int sort_column = this.FSortColumn;

				if (this.FSortOrder != SortOrder.None && sort_column >= 0 ) {
					ListViewItem item = x as ListViewItem;
					ListViewItem item2 = y as ListViewItem;

					if (sort_column < item.SubItems.Count && sort_column < item2.SubItems.Count)
					{					
						int comp_res = GKListView.agCompare(item.SubItems[sort_column].Text, item2.SubItems[sort_column].Text);

						if (this.FSortOrder == SortOrder.Ascending) {
							Result = comp_res;
						} else if (this.FSortOrder == SortOrder.Descending) {
							Result = -comp_res;
						}
					}
				}

				return Result;
			}
		}

		private TListViewColumnSorter lvwColumnSorter;
		private SortOrder old_SortOrder = SortOrder.None;

		public int SortColumn
		{
			get { return this.lvwColumnSorter.SortColumn; }
			set { this.lvwColumnSorter.SortColumn = value; }
		}

		public GKListView()
		{
			this.lvwColumnSorter = new GKListView.TListViewColumnSorter();
			base.ListViewItemSorter = this.lvwColumnSorter;
			base.ColumnClick += new ColumnClickEventHandler(this.lvColumnClick);
			//this.DoubleBuffered = true;
		}

		public void UnsetSorter()
		{
			base.ColumnClick -= new ColumnClickEventHandler(this.lvColumnClick);
		}

		public void SwitchSorter()
		{
			if (old_SortOrder == SortOrder.None) {
				old_SortOrder = this.lvwColumnSorter.Order;
				this.lvwColumnSorter.Order = SortOrder.None;
			} else {
				this.lvwColumnSorter.Order = old_SortOrder;
				old_SortOrder = SortOrder.None;
				base.Sort();
			}
		}

		/*public override void BeginUpdate()
		{
			base.BeginUpdate();
			this.SwitchSorter();
		}

		public override void EndUpdate()
		{
			this.SwitchSorter();
			base.EndUpdate();
		}*/

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

		public void AddListColumn(string caption, int width, bool autoSize)
		{
			if (autoSize) width = -1;
			base.Columns.Add(caption, width, HorizontalAlignment.Left);
		}

		public GKListItem AddItem(string title, object data)
		{
			GKListItem result = new GKListItem(title);
			result.Data = data;
			base.Items.Add(result);
			return result;
		}

		public GKListItem SelectedItem()
		{
			GKListItem Result;

			if (base.SelectedItems.Count <= 0) {
				Result = null;
			} else {
				Result = (base.SelectedItems[0] as GKListItem);
			}

			return Result;
		}

		public void ResizeColumn(int columnIndex)
		{
			if (columnIndex >= 0) {
				this.AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

				if (this.Columns[columnIndex].Width < 20) {
					this.AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.HeaderSize);
				}
			}
		}

		protected static int agCompare(string Str1, string Str2)
		{
			double Val, Val2;
			bool v = double.TryParse(Str1, out Val);
			bool v2 = double.TryParse(Str2, out Val2);

			int Result;
			if (v && v2)
			{
				if (Val < Val2) {
					Result = -1;
				} else if (Val > Val2) {
					Result = 1;
				} else {
					Result = 0;
				}
			}
			else
			{
				Result = string.Compare(Str1, Str2, false);
				if (Str1 != "" && Str2 == "")
				{
					Result = -1;
				}
				if (Str1 == "" && Str2 != "")
				{
					Result = 1;
				}
			}
			return Result;
		}

		public void SelectItem(ListViewItem item)
		{
			if (item != null) {
				this.SelectedIndices.Clear();
				item.Selected = true;
				item.EnsureVisible();
			}
		}

		public void SelectItem(int index)
		{
			if (index >= 0 && index < this.Items.Count) {
				ListViewItem item = this.Items[index];
				this.SelectItem(item);
			}
		}

		public void SelectItem(object data)
		{
			GKListItem item;
			for (int i = 0; i < this.Items.Count; i++) {
				item = this.Items[i] as GKListItem;
				if (item.Data == data) {
					this.SelectItem(item);
					return;
				}
			}
		}
	}
}
