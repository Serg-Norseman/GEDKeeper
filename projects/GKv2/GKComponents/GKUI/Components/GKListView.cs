/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKUI.Platform.Handlers;
using BSDListItem = GKCore.Design.Controls.IListItem;
using BSDSortOrder = GKCore.Design.BSDTypes.SortOrder;
using IListSource = GKCore.Interfaces.IListSource;

namespace GKUI.Components
{
    /// <summary>
    ///
    /// </summary>
    [Serializable]
    public class GKListItem : ListViewItem, BSDListItem
    {
        public GKListItem(object itemValue, object tag)
        {
            Text = (itemValue == null) ? string.Empty : itemValue.ToString();
            Tag = tag;
        }
    }


    /// <summary>
    ///
    /// </summary>
    public class GKListView : ListView, IListView
    {
        private readonly ListViewAppearance fAppearance;
        private GKListItem[] fCache;
        private int fCacheFirstItem;
        private IListSource fListMan;
        private int fSortColumn;
        private BSDSortOrder fSortOrder;


        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public ListViewAppearance Appearance
        {
            get { return fAppearance; }
        }

        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public IListSource ListMan
        {
            get {
                return fListMan;
            }
            set {
                if (fListMan != value) {
                    fListMan = value;

                    if (fListMan != null) {
                        VirtualMode = true;
                        fSortColumn = 0;
                        fSortOrder = BSDSortOrder.Ascending;
                    } else {
                        VirtualMode = false;
                    }
                }
            }
        }

        public int SelectedIndex
        {
            get {
                return (VirtualMode && SelectedIndices.Count > 0) ? SelectedIndices[0] : Items.IndexOf(GetSelectedItem());
            }
            set {
                SelectItem(value);
            }
        }

        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public int SortColumn
        {
            get { return fSortColumn; }
            set { fSortColumn = value; }
        }

        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public BSDSortOrder SortOrder
        {
            get { return fSortOrder; }
            set { fSortOrder = value; }
        }


        public GKListView()
        {
            fAppearance = new ListViewAppearance(this);

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
            fSortOrder = BSDSortOrder.None;
            fListMan = null;
        }

        public void Activate()
        {
            Select();
        }

        protected BSDSortOrder GetColumnSortOrder(int columnIndex)
        {
            return (fSortColumn == columnIndex) ? fSortOrder : BSDSortOrder.None;
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            int prevColumn = fSortColumn;
            if (prevColumn == sortColumn && checkOrder) {
                var prevOrder = GetColumnSortOrder(sortColumn);
                fSortOrder = (prevOrder == BSDSortOrder.Ascending) ? BSDSortOrder.Descending : BSDSortOrder.Ascending;
            } else {
                fSortOrder = BSDSortOrder.Ascending;
            }

            fSortColumn = sortColumn;
            SortContents(true);
        }

        private void SortContents(bool restoreSelected)
        {
            if (fListMan == null || fSortOrder == BSDSortOrder.None) return;

            object rec = (restoreSelected) ? GetSelectedData() : null;

            fListMan.SortContents(fSortColumn, fSortOrder == BSDSortOrder.Ascending);
            ResetCache();

            if (rec != null) SelectItem(rec);
        }

        public void SortModelColumn(int columnId)
        {
            if (fListMan == null || AppHost.TEST_MODE) return;

            int sortColumn = fListMan.GetColumnIndex(columnId);
            if (sortColumn != -1) {
                SetSortColumn(sortColumn, false);
            }
        }

        protected override void OnColumnClick(ColumnClickEventArgs e)
        {
            SetSortColumn(e.Column);

            // we use Refresh() because only Invalidate() isn't update header's area
            Refresh();

            base.OnColumnClick(e);
        }

        protected override void OnDrawColumnHeader(DrawListViewColumnHeaderEventArgs e)
        {
            e.DrawDefault = false;

            using (var sf = new StringFormat()) {
                Graphics gfx = e.Graphics;
                Rectangle rt = e.Bounds;

                if ((e.State & ListViewItemStates.Selected) == ListViewItemStates.Selected) {
                    UIHelper.DrawHeaderBackground(gfx, fAppearance.HeaderPressed, rt);
                } else {
                    UIHelper.DrawHeaderBackground(gfx, fAppearance.Header, rt);
                }

                switch (e.Header.TextAlign) {
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

                using (var brush = new SolidBrush(fAppearance.HeaderText)) {
                    gfx.DrawString(e.Header.Text, Font, brush, rt, sf);
                }

                switch (GetColumnSortOrder(e.ColumnIndex)) {
                    case BSDSortOrder.Ascending:
                        UIHelper.DrawSortArrow(gfx, this.Font, rt, "▲");
                        break;
                    case BSDSortOrder.Descending:
                        UIHelper.DrawSortArrow(gfx, this.Font, rt, "▼");
                        break;
                }
            }

            base.OnDrawColumnHeader(e);
        }

        protected override void OnDrawItem(DrawListViewItemEventArgs e)
        {
            e.DrawDefault = true;
            // Hack: to show checkbox for unchecked items too
            if (!e.Item.Checked) {
                e.Item.Checked = true;
                e.Item.Checked = false;
            }
            base.OnDrawItem(e);
        }

        protected override void OnDrawSubItem(DrawListViewSubItemEventArgs e)
        {
            e.DrawDefault = true;
            base.OnDrawSubItem(e);
        }

        protected override void OnItemCheck(ItemCheckEventArgs e)
        {
            // Attention: is not called in VirtualMode
            base.OnItemCheck(e);
        }

        protected override void OnMouseClick(MouseEventArgs e)
        {
            // Hack: for VirtualMode
            if (base.CheckBoxes) {
                ListViewItem lvi = GetItemAt(e.X, e.Y);
                if (lvi != null) {
                    if (e.X < (lvi.Bounds.Left + 16)) {
                        lvi.Checked = !lvi.Checked;
                        fListMan.SetColumnValue(lvi.Index, 0, lvi.Checked);
                        Invalidate(lvi.Bounds);
                    }
                }
            }
            base.OnMouseClick(e);
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            // Hack: for VirtualMode
            if (base.CheckBoxes) {
                ListViewItem lvi = GetItemAt(e.X, e.Y);
                if (lvi != null)
                    Invalidate(lvi.Bounds);
            }
            base.OnMouseDoubleClick(e);
        }

        private GKListItem GetVirtualItem(int itemIndex)
        {
            object rowData = fListMan.GetContentItem(itemIndex);
            if (rowData == null) return null;

            object[] columnValues = fListMan.GetItemData(rowData);
            if (columnValues == null) return null;

            bool isChecked = false;
            var colValZ = columnValues[0];
            if (this.CheckBoxes && colValZ is bool boolVal) {
                isChecked = boolVal;
                colValZ = string.Empty;
            }

            var item = new GKListItem(colValZ, rowData);
            item.Checked = isChecked;

            for (int i = 1, num = columnValues.Length; i < num; i++) {
                var val = columnValues[i];
                var text = (val == null) ? string.Empty : val.ToString();
                item.SubItems.Add(text);
            }

            var backColor = fListMan.GetBackgroundColor(itemIndex, rowData);
            if (backColor != null && backColor is ColorHandler colorHandler) {
                item.BackColor = colorHandler.Handle;
            }

            return item;
        }

        protected override void OnCacheVirtualItems(CacheVirtualItemsEventArgs e)
        {
            // Only recreate the cache if we need to.
            if (fCache != null && e.StartIndex >= fCacheFirstItem && e.EndIndex <= fCacheFirstItem + fCache.Length) return;

            fCacheFirstItem = e.StartIndex;
            int length = e.EndIndex - e.StartIndex + 1;

            if (fCache == null || fCache.Length != length) {
                fCache = new GKListItem[length];
            }

            for (int i = 0; i < length; i++) {
                fCache[i] = GetVirtualItem(fCacheFirstItem + i);
            }
        }

        protected override void OnRetrieveVirtualItem(RetrieveVirtualItemEventArgs e)
        {
            // If we have the item cached, return it. Otherwise, recreate it.
            if (fCache != null && e.ItemIndex >= fCacheFirstItem && e.ItemIndex < fCacheFirstItem + fCache.Length) {
                e.Item = fCache[e.ItemIndex - fCacheFirstItem];
            } else {
                e.Item = GetVirtualItem(e.ItemIndex);
            }
        }

        public void ResetCache()
        {
            fCache = null;
        }

        protected override void OnColumnWidthChanged(ColumnWidthChangedEventArgs e)
        {
            if (fListMan != null) {
                fListMan.ChangeColumnWidth(e.ColumnIndex, Columns[e.ColumnIndex].Width);
                Invalidate();
            }

            base.OnColumnWidthChanged(e);
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            if (fListMan == null) return;

            try {
                object tempRec = GetSelectedData();
                base.BeginUpdate();
                try {
                    if (columnsChanged || Columns.Count == 0 || fListMan.ColumnsMap.Count == 0) {
                        fListMan.UpdateColumns(this);
                    }

                    fListMan.UpdateContents();
                    SortContents(false);
                    VirtualListSize = fListMan.FilteredCount;

                    ResizeColumns();
                } finally {
                    base.EndUpdate();
                    if (tempRec != null) SelectItem(tempRec);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.UpdateContents()", ex);
            }
        }

        public void DeleteRecord(object data)
        {
            // crash protection: when you delete records from the diagrams,
            // between the actual deleting a record and updating the list
            // may take a few requests to update the list's items which does not already exist
            if (fListMan != null && fListMan.DeleteItem(data)) {
                VirtualListSize = fListMan.FilteredCount;
            }
        }

        public new void Clear()
        {
            // identical clearing of columns and items
            base.Clear();
        }

        public void ClearColumns()
        {
            Columns.Clear();
        }

        public void AddColumn(string caption, int width, bool autoSize = false, BSDTypes.HorizontalAlignment textAlign = BSDTypes.HorizontalAlignment.Left)
        {
            if (autoSize) width = -1;
            Columns.Add(caption, width, (HorizontalAlignment)textAlign);
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            if (autoSize) width = -1;
            Columns.Add(caption, width, HorizontalAlignment.Center);
        }

        public void SetColumnCaption(int index, string caption)
        {
            Columns[index].Text = caption;
        }

        public void ResizeColumn(int columnIndex)
        {
            try {
                if (columnIndex >= 0 && Items.Count > 0) {
                    AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

                    if (Columns[columnIndex].Width < 20) {
                        AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.HeaderSize);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.ResizeColumn()", ex);
            }
        }

        public void ResizeColumns()
        {
            if (fListMan == null) return;

            for (int i = 0; i < Columns.Count; i++) {
                if (fListMan.IsColumnAutosize(i)) {
                    ResizeColumn(i);
                }
            }
        }

        public IList<object> GetSelectedItems()
        {
            try {
                var result = new List<object>();

                if (!VirtualMode) {
                    int num = SelectedItems.Count;
                    for (int i = 0; i < num; i++) {
                        var lvItem = SelectedItems[i] as GKListItem;
                        result.Add(lvItem.Tag);
                    }
                } else {
                    int num = SelectedIndices.Count;
                    for (int i = 0; i < num; i++) {
                        int index = SelectedIndices[i];
                        result.Add(fListMan.GetContentItem(index));
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.GetSelectedItems()", ex);
                return null;
            }
        }

        public GKListItem GetSelectedItem()
        {
            GKListItem result = (SelectedItems.Count <= 0) ? null : SelectedItems[0] as GKListItem;
            return result;
        }

        public object GetSelectedData()
        {
            try {
                object result = null;

                if (!VirtualMode) {
                    GKListItem item = GetSelectedItem();
                    if (item != null) result = item.Tag;
                } else {
                    if (SelectedIndices.Count > 0) {
                        int index = SelectedIndices[0];
                        result = fListMan.GetContentItem(index);
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.GetSelectedData()", ex);
                return null;
            }
        }

        private void SelectItem(int index, GKListItem item)
        {
            if (item == null) return;

            SelectedIndices.Clear();
            item.Selected = true;
            EnsureVisible(index);
        }

        public void SelectItem(int index)
        {
            if (!VirtualMode) {
                if (index == -1) {
                    index = Items.Count - 1;
                }

                if (index >= 0 && index < Items.Count) {
                    var item = (GKListItem)Items[index];
                    SelectItem(index, item);
                }
            } else {
                SelectedIndices.Clear();
                SelectedIndices.Add(index);
            }
        }

        public void SelectItem(object rowData)
        {
            if (fListMan == null || rowData == null) return;

            try {
                // "virtual" mode
                int idx = fListMan.IndexOfItem(rowData);
                if (idx >= 0) {
                    var item = (GKListItem)Items[idx];
                    SelectItem(idx, item);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.SelectItem()", ex);
            }
        }
    }


    public class ListViewAppearance
    {
        private readonly ListView fOwner;
        private Color fBackColor;
        private Color fHeader;
        private Color fHeaderPressed;
        private Color fHeaderText;
        private Color fInterlaced;
        private Color fItemSelected;

        public Color BackColor
        {
            get {
                return fBackColor;
            }
            set {
                if (fBackColor != value) {
                    fBackColor = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color Header
        {
            get {
                return fHeader;
            }
            set {
                if (fHeader != value) {
                    fHeader = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color HeaderPressed
        {
            get {
                return fHeaderPressed;
            }
            set {
                if (fHeaderPressed != value) {
                    fHeaderPressed = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color HeaderText
        {
            get {
                return fHeaderText;
            }
            set {
                if (fHeaderText != value) {
                    fHeaderText = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color Interlaced
        {
            get {
                return fInterlaced;
            }
            set {
                if (fInterlaced != value) {
                    fInterlaced = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color ItemSelected
        {
            get {
                return fItemSelected;
            }
            set {
                if (fItemSelected != value) {
                    fItemSelected = value;
                    fOwner.Invalidate();
                }
            }
        }

        public ListViewAppearance(ListView owner)
        {
            fOwner = owner;
            Reset(false);
        }

        public void Reset(bool invalidate = true)
        {
            fBackColor = SystemColors.Control;
            fHeader = SystemColors.Control;
            fHeaderPressed = Color.FromArgb(188, 220, 244); // BCDCF4;
            fHeaderText = SystemColors.ControlText;
            fInterlaced = SystemColors.ControlDark;
            fItemSelected = SystemColors.Highlight;

            if (invalidate)
                fOwner.Invalidate();
        }
    }
}
