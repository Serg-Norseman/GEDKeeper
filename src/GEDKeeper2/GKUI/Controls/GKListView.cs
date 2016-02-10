using System;
using System.Collections;
using System.Windows.Forms;

using GKCommon;

namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
	public class GKListView : ListView
	{
		private class LVColumnSorter : IComparer
		{
			private int fSortColumn;
			private SortOrder fSortOrder;

			public int SortColumn
			{
				get { return this.fSortColumn; }
				set { this.fSortColumn = value; }
			}

			public SortOrder Order
			{
				get { return this.fSortOrder; }
				set { this.fSortOrder = value; }
			}

			public LVColumnSorter()
			{
				this.fSortColumn = 0;
				this.fSortOrder = SortOrder.None;
			}

			public int Compare(object x, object y)
			{
				int result = 0;

				int sortColumn = this.fSortColumn;
				if (this.fSortOrder != SortOrder.None && sortColumn >= 0) {
					ListViewItem item1 = x as ListViewItem;
					ListViewItem item2 = y as ListViewItem;

					int compRes = 0;
					if (item1 is GKListItem && item2 is GKListItem) {
						GKListItem eitem1 = x as GKListItem;
						GKListItem eitem2 = y as GKListItem;

						if (sortColumn == 0) {
							compRes = eitem1.CompareTo(eitem2);
						} else {
							GKListSubItem sub1 = item1.SubItems[sortColumn] as GKListSubItem;
							GKListSubItem sub2 = item2.SubItems[sortColumn] as GKListSubItem;

							compRes = sub1.CompareTo(sub2);
						}
					} else {
						if (sortColumn < item1.SubItems.Count && sortColumn < item2.SubItems.Count)
						{
							compRes = agCompare(item1.SubItems[sortColumn].Text, item2.SubItems[sortColumn].Text);
						}
					}

					if (this.fSortOrder == SortOrder.Ascending) {
						result = compRes;
					} else if (this.fSortOrder == SortOrder.Descending) {
						result = -compRes;
					}
				}

				return result;
			}
			
			#region Private methods

			private static int agCompare(string str1, string str2)
			{
				double val1, val2;
				bool v1 = double.TryParse(str1, out val1);
				bool v2 = double.TryParse(str2, out val2);

				int result;
				if (v1 && v2)
				{
					if (val1 < val2) {
						result = -1;
					} else if (val1 > val2) {
						result = +1;
					} else {
						result = 0;
					}
				}
				else
				{
					result = string.Compare(str1, str2, false);
					if (str1 != "" && str2 == "") {
						result = -1;
					} else if (str1 == "" && str2 != "") {
						result = +1;
					}
				}
				return result;
			}

			#endregion
		}

		private readonly LVColumnSorter fColumnSorter;
		private SortOrder fOldSortOrder;

		public int SortColumn
		{
			get { return this.fColumnSorter.SortColumn; }
			set { this.fColumnSorter.SortColumn = value; }
		}

		public GKListView()
		{
			base.SetStyle(ControlStyles.OptimizedDoubleBuffer, true);

			this.fOldSortOrder = SortOrder.None;
			this.fColumnSorter = new LVColumnSorter();

			base.ListViewItemSorter = this.fColumnSorter;
			base.ColumnClick += this.lvColumnClick;
		}

		public void UnsetSorter()
		{
			base.ColumnClick -= this.lvColumnClick;
		}

		public void SwitchSorter()
		{
			if (fOldSortOrder == SortOrder.None) {
				fOldSortOrder = this.fColumnSorter.Order;
				this.fColumnSorter.Order = SortOrder.None;
			} else {
				this.fColumnSorter.Order = fOldSortOrder;
				fOldSortOrder = SortOrder.None;
				base.Sort();
			}
		}

		public virtual void BeginUpdates()
		{
			base.BeginUpdate();
			//this.SwitchSorter();
		}

		public virtual void EndUpdates()
		{
			//this.SwitchSorter();
			base.EndUpdate();
		}

		private void lvColumnClick(object sender, ColumnClickEventArgs e)
		{
			if (e.Column == this.fColumnSorter.SortColumn)
			{
				if (this.fColumnSorter.Order == SortOrder.Ascending) {
					this.fColumnSorter.Order = SortOrder.Descending;
				} else {
					this.fColumnSorter.Order = SortOrder.Ascending;
				}
			} else {
				this.fColumnSorter.SortColumn = e.Column;
				this.fColumnSorter.Order = SortOrder.Ascending;
			}

			base.Sort();
		}

		public void AddListColumn(string caption, int width, bool autoSize)
		{
			if (autoSize) width = -1;
			base.Columns.Add(caption, width, HorizontalAlignment.Left);
		}

		public GKListItem AddItem(object itemValue, object data)
		{
			GKListItem result = new GKListItem(itemValue, data);
			base.Items.Add(result);
			return result;
		}

		public GKListItem SelectedItem()
		{
			GKListItem result;

			if (base.SelectedItems.Count <= 0) {
				result = null;
			} else {
				result = (base.SelectedItems[0] as GKListItem);
			}

			return result;
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
			int num = this.Items.Count;
			for (int i = 0; i < num; i++) {
                GKListItem item = (GKListItem)this.Items[i];

                if (item.Data == data) {
					this.SelectItem(item);
					return;
				}
			}
		}
	}
}
