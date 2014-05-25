using System.Collections;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class GKListView : ListView
	{
		private class TListViewColumnSorter : IComparer
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

			public TListViewColumnSorter()
			{
				this.fSortColumn = 0;
				this.fSortOrder = SortOrder.None;
			}

			int IComparer.Compare(object x, object y)
			{
				int result = 0;

				int sortColumn = this.fSortColumn;

				if (this.fSortOrder != SortOrder.None && sortColumn >= 0) {
					ListViewItem item = x as ListViewItem;
					ListViewItem item2 = y as ListViewItem;

					if (sortColumn < item.SubItems.Count && sortColumn < item2.SubItems.Count)
					{					
						int compRes = agCompare(item.SubItems[sortColumn].Text, item2.SubItems[sortColumn].Text);

						if (this.fSortOrder == SortOrder.Ascending) {
							result = compRes;
						} else if (this.fSortOrder == SortOrder.Descending) {
							result = -compRes;
						}
					}
				}

				return result;
			}
		}

		private readonly TListViewColumnSorter lvwColumnSorter;
		private SortOrder old_SortOrder = SortOrder.None;

		public int SortColumn
		{
			get { return this.lvwColumnSorter.SortColumn; }
			set { this.lvwColumnSorter.SortColumn = value; }
		}

		public GKListView()
		{
			this.lvwColumnSorter = new TListViewColumnSorter();
			base.ListViewItemSorter = this.lvwColumnSorter;
			base.ColumnClick += this.lvColumnClick;
			//this.DoubleBuffered = true;
		}

		public void UnsetSorter()
		{
			base.ColumnClick -= this.lvColumnClick;
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

		protected static int agCompare(string str1, string str2)
		{
			double Val, Val2;
			bool v = double.TryParse(str1, out Val);
			bool v2 = double.TryParse(str2, out Val2);

			int result;
			if (v && v2)
			{
				if (Val < Val2) {
					result = -1;
				} else if (Val > Val2) {
					result = 1;
				} else {
					result = 0;
				}
			}
			else
			{
				result = string.Compare(str1, str2, false);
				if (str1 != "" && str2 == "")
				{
					result = -1;
				}
				if (str1 == "" && str2 != "")
				{
					result = 1;
				}
			}
			return result;
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
			for (int i = 0; i < this.Items.Count; i++) {
                GKListItem item = this.Items[i] as GKListItem;
				if (item.Data == data) {
					this.SelectItem(item);
					return;
				}
			}
		}
	}
}
