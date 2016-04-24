/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

                if (this.fSortOrder != SortOrder.None && this.fSortColumn >= 0)
                {
                    ListViewItem item1 = (ListViewItem)x;
                    ListViewItem item2 = (ListViewItem)y;

                    if (item1 is GKListItem && item2 is GKListItem) {
                        if (this.fSortColumn == 0) {
                            GKListItem eitem1 = ((GKListItem)x);
                            GKListItem eitem2 = ((GKListItem)y);

                            result = eitem1.CompareTo(eitem2);
                        } else {
                            if (this.fSortColumn < item1.SubItems.Count && this.fSortColumn < item2.SubItems.Count)
                            {
                                GKListSubItem sub1 = (GKListSubItem)item1.SubItems[this.fSortColumn];
                                GKListSubItem sub2 = (GKListSubItem)item2.SubItems[this.fSortColumn];

                                result = sub1.CompareTo(sub2);
                            }
                        }
                    } else {
                        if (this.fSortColumn < item1.SubItems.Count && this.fSortColumn < item2.SubItems.Count)
                        {
                            result = agCompare(item1.SubItems[this.fSortColumn].Text, item2.SubItems[this.fSortColumn].Text);
                        }
                    }

                    if (this.fSortOrder == SortOrder.Descending)
                    {
                        result = -result;
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
            base.SetStyle(ControlStyles.AllPaintingInWmPaint, true);

            // Enable the OnNotifyMessage event so we get a chance to filter out
            // Windows messages before they get to the form's WndProc
            base.SetStyle(ControlStyles.EnableNotifyMessage, true);

            base.DoubleBuffered = true;

            this.fOldSortOrder = SortOrder.None;
            this.fColumnSorter = new LVColumnSorter();

            base.ListViewItemSorter = this.fColumnSorter;
            base.ColumnClick += this.LVColumnClick;
        }

        protected override void OnNotifyMessage(Message m)
        {
            ////Filter out the WM_ERASEBKGND message
            //if(m.Msg != 0x14)
            //{
            //    base.OnNotifyMessage(m);
            //}
        }

        public void UnsetSorter()
        {
            base.ColumnClick -= this.LVColumnClick;
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

        private void LVColumnClick(object sender, ColumnClickEventArgs e)
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
            try {
                if (columnIndex >= 0 && this.Items.Count > 0)
                {
                    this.AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

                    if (this.Columns[columnIndex].Width < 20)
                    {
                        this.AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.HeaderSize);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKListView.ResizeColumn(): " + ex.Message);
            }
        }

        public void SelectItem(ListViewItem item)
        {
            if (item == null) return;

            this.SelectedIndices.Clear();
            item.Selected = true;
            item.EnsureVisible();
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
