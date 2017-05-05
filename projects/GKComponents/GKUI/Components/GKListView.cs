/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

//#define DEFAULT_HEADER

using System;
using System.Collections;
using System.Drawing;
using System.Drawing.Text;
using System.Windows.Forms;
using System.Windows.Forms.VisualStyles;

using GKCommon;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    [Serializable]
    public class GKListItem : ListViewItem, IListItem
    {
        protected object fValue;

        public object Data;

        public GKListItem(object itemValue, object data)
        {
            fValue = itemValue;
            Text = ToString();
            Data = data;
        }

        public override string ToString()
        {
            return (fValue == null) ? string.Empty : fValue.ToString();
        }

        public int CompareTo(object obj)
        {
            GKListItem otherItem = obj as GKListItem;
            if (otherItem == null) {
                return -1;
            }

            IComparable cv1 = fValue as IComparable;
            IComparable cv2 = otherItem.fValue as IComparable;

            int compRes;
            if (cv1 != null && cv2 != null)
            {
                compRes = cv1.CompareTo(cv2);
            }
            else if (cv1 != null)
            {
                compRes = -1;
            }
            else if (cv2 != null)
            {
                compRes = 1;
            }
            else {
                compRes = 0;
            }
            return compRes;
        }

        public void AddSubItem(object itemValue)
        {
            GKListSubItem subItem = new GKListSubItem(itemValue);
            SubItems.Add(subItem);
        }

        /*public void SetBackColor(IColor color)
        {
            BackColor = color as Color;
        }*/
    }


    public class GKListSubItem : ListViewItem.ListViewSubItem, IComparable
    {
        protected object fValue;

        public GKListSubItem(object itemValue)
        {
            fValue = itemValue;
            Text = ToString();
        }

        public override string ToString()
        {
            return (fValue == null) ? string.Empty : fValue.ToString();
        }

        public int CompareTo(object obj)
        {
            GKListSubItem otherItem = obj as GKListSubItem;
            if (otherItem == null) {
                return -1;
            }

            IComparable cv1 = fValue as IComparable;
            IComparable cv2 = otherItem.fValue as IComparable;

            int compRes;
            if (cv1 != null && cv2 != null)
            {
                compRes = cv1.CompareTo(cv2);
            }
            else if (cv1 != null)
            {
                compRes = -1;
            }
            else if (cv2 != null)
            {
                compRes = 1;
            }
            else {
                compRes = 0;
            }
            return compRes;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class GKListView : ListView, IListView
    {
        private class LVColumnSorter : IComparer
        {
            private readonly GKListView fOwner;

            public LVColumnSorter(GKListView owner)
            {
                fOwner = owner;
            }

            public int Compare(object x, object y)
            {
                int result = 0;

                int sortColumn = fOwner.fSortColumn;
                SortOrder sortOrder = fOwner.fSortOrder;

                if (sortOrder != SortOrder.None && sortColumn >= 0)
                {
                    ListViewItem item1 = (ListViewItem)x;
                    ListViewItem item2 = (ListViewItem)y;

                    if (item1 is IComparable && item2 is IComparable) {
                        if (sortColumn == 0) {
                            IComparable eitem1 = (IComparable)x;
                            IComparable eitem2 = (IComparable)y;

                            result = eitem1.CompareTo(eitem2);
                        } else {
                            if (sortColumn < item1.SubItems.Count && sortColumn < item2.SubItems.Count)
                            {
                                IComparable sub1 = (IComparable)item1.SubItems[sortColumn];
                                IComparable sub2 = (IComparable)item2.SubItems[sortColumn];

                                result = sub1.CompareTo(sub2);
                            }
                        }
                    } else {
                        if (sortColumn < item1.SubItems.Count && sortColumn < item2.SubItems.Count)
                        {
                            result = agCompare(item1.SubItems[sortColumn].Text, item2.SubItems[sortColumn].Text);
                        }
                    }

                    if (sortOrder == SortOrder.Descending)
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

        protected int fSortColumn;
        protected SortOrder fSortOrder;
        protected int fUpdateCount;


        public int SortColumn
        {
            get { return fSortColumn; }
            set { fSortColumn = value; }
        }

        public SortOrder Order
        {
            get { return fSortOrder; }
            set { fSortOrder = value; }
        }


        public GKListView()
        {
            SetStyle(ControlStyles.DoubleBuffer, true);
            SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
            SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            // Enable the OnNotifyMessage event so we get a chance to filter out
            // Windows messages before they get to the form's WndProc
            SetStyle(ControlStyles.EnableNotifyMessage, true);

            OwnerDraw = true;
            HideSelection = false;
            LabelEdit = false;
            FullRowSelect = true;
            View = View.Details;

            fSortColumn = 0;
            fSortOrder = SortOrder.None;
            fColumnSorter = new LVColumnSorter(this);

            ListViewItemSorter = fColumnSorter;
        }

        public new void BeginUpdate()
        {
            if (fUpdateCount == 0)
            {
                #if !__MonoCS__
                ListViewItemSorter = null;
                #endif
                base.BeginUpdate();
            }

            fUpdateCount++;
        }

        public new void EndUpdate()
        {
            fUpdateCount--;

            if (fUpdateCount == 0)
            {
                base.EndUpdate();
                #if !__MonoCS__
                ListViewItemSorter = fColumnSorter;
                #endif
            }
        }

        protected SortOrder GetColumnSortOrder(int columnIndex)
        {
            return (fSortColumn == columnIndex) ? fSortOrder : SortOrder.None;
        }

        protected virtual void InternalColumnClick(ColumnClickEventArgs e)
        {
            SortOrder prevOrder = GetColumnSortOrder(e.Column);
            fSortOrder = (prevOrder == SortOrder.Ascending) ? SortOrder.Descending : SortOrder.Ascending;
            fSortColumn = e.Column;

            Sort();
        }

        protected override void OnColumnClick(ColumnClickEventArgs e)
        {
            InternalColumnClick(e);

            // we use Refresh() because only Invalidate() isn't update header's area
            Refresh();

            base.OnColumnClick(e);
        }

        protected override void OnDrawColumnHeader(DrawListViewColumnHeaderEventArgs e)
        {
            #if DEFAULT_HEADER

            e.DrawDefault = true;

            #else

            using (var sf = new StringFormat())
            {
                Graphics gfx = e.Graphics;
                Rectangle rt = e.Bounds;

                #if !__MonoCS__
                VisualStyleElement element = VisualStyleElement.Header.Item.Normal;
                if ((e.State & ListViewItemStates.Hot) == ListViewItemStates.Hot)
                    element = VisualStyleElement.Header.Item.Hot;
                if ((e.State & ListViewItemStates.Selected) == ListViewItemStates.Selected)
                    element = VisualStyleElement.Header.Item.Pressed;

                var visualStyleRenderer = new VisualStyleRenderer(element);
                visualStyleRenderer.DrawBackground(gfx, rt);
                #else
                e.DrawBackground();
                #endif

                switch (e.Header.TextAlign)
                {
                    case HorizontalAlignment.Left:
                        sf.Alignment = StringAlignment.Near;
                        break;

                    case HorizontalAlignment.Right:
                        sf.Alignment = StringAlignment.Far;
                        break;

                    case HorizontalAlignment.Center:
                        sf.Alignment = StringAlignment.Center;
                        break;
                }

                sf.LineAlignment = StringAlignment.Center;
                sf.Trimming = StringTrimming.EllipsisCharacter;
                sf.FormatFlags = StringFormatFlags.NoWrap;

                int w = TextRenderer.MeasureText(" ", Font).Width;
                rt.Inflate(-(w / 5), 0);

                gfx.DrawString(e.Header.Text, Font, Brushes.Black, rt, sf);

                string arrow = "";
                switch (GetColumnSortOrder(e.ColumnIndex)) {
                    case SortOrder.Ascending:
                        arrow = "▲";
                        break;
                    case SortOrder.Descending:
                        arrow = "▼";
                        break;
                }

                if (arrow != "") {
                    using (var fnt = new Font(Font.FontFamily, Font.SizeInPoints * 0.6f, FontStyle.Regular)) {
                        float aw = gfx.MeasureString(arrow, fnt).Width;
                        float x = rt.Left + (rt.Width - aw) / 2.0f;
                        gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
                        gfx.DrawString(arrow, fnt, Brushes.Black, x, rt.Top);
                    }
                }
            }

            #endif

            base.OnDrawColumnHeader(e);
        }

        protected override void OnDrawItem(DrawListViewItemEventArgs e)
        {
            e.DrawDefault = true;
            base.OnDrawItem(e);
        }

        protected override void OnDrawSubItem(DrawListViewSubItemEventArgs e)
        {
            e.DrawDefault = true;
            base.OnDrawSubItem(e);
        }

        #region Public methods

        public void AddColumn(string caption, int width, bool autoSize)
        {
            if (autoSize) width = -1;
            Columns.Add(caption, width, HorizontalAlignment.Left);
        }

        public void ResizeColumn(int columnIndex)
        {
            try {
                if (columnIndex >= 0 && Items.Count > 0)
                {
                    AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

                    if (Columns[columnIndex].Width < 20)
                    {
                        AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.HeaderSize);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("ExtListView.ResizeColumn(): " + ex.Message);
            }
        }

        public GKListItem AddItem(object itemValue, object data)
        {
            var result = new GKListItem(itemValue, data);
            Items.Add(result);
            return result;
        }

        public GKListItem AddItem(object itemValue, object data, GKListSubItem[] subitemsValues)
        {
            var result = AddItem(itemValue, data);
            result.SubItems.AddRange(subitemsValues);
            return result;
        }

        public GKListItem GetSelectedItem()
        {
            GKListItem result;

            if (SelectedItems.Count <= 0) {
                result = null;
            } else {
                result = (SelectedItems[0] as GKListItem);
            }

            return result;
        }

        public void SelectItem(ListViewItem item)
        {
            if (item == null) return;

            SelectedIndices.Clear();
            item.Selected = true;
            item.EnsureVisible();
        }

        public void SelectItem(int index)
        {
            if (index >= 0 && index < Items.Count) {
                ListViewItem item = Items[index];
                SelectItem(item);
            }
        }

        public void SelectItem(object data)
        {
            int num = Items.Count;
            for (int i = 0; i < num; i++) {
                var item = (GKListItem)Items[i];

                if (item.Data == data) {
                    SelectItem(item);
                    return;
                }
            }
        }

        #endregion
    }
}
