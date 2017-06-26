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
using System.Collections.ObjectModel;
using Eto.Forms;

using GKCommon;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /*
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

        public void SetBackColor(IColor color)
        {
            var colorHandler = color as ColorHandler;
            if (colorHandler != null) {
                BackColor = colorHandler.Handle;
            }
        }
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
    }*/

    public class GKListItem : GridItem, GKCore.Interfaces.IListItem
    {
        private object fData;

        public object Data
        {
            get { return fData; }
            set { fData = value; }
        }

        public GKListItem(params object[] values) : base(values)
        {
        }

        public void AddSubItem(object itemValue)
        {
            
        }

        public void SetBackColor(IColor color)
        {
            
        }

        public int CompareTo(object obj)
        {
            return 0;
        }
    }

    public enum SortOrder
    {
        None, Ascending, Descending
    }

    /// <summary>
    /// 
    /// </summary>
    public class GKListView : GridView, IListView
    {
        /*private class LVColumnSorter : IComparer
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

        private readonly LVColumnSorter fColumnSorter;*/

        protected int fSortColumn;
        protected SortOrder fSortOrder;
        protected int fUpdateCount;

        // Virtual fields
        private GKListItem[] fCache;
        private int fCacheFirstItem;
        private IListManager fListMan;


        private ObservableCollection<GridItem> fItems;


        public int SortColumn
        {
            get { return fSortColumn; }
            set { fSortColumn = value; }
        }

        public SortOrder Sorting
        {
            get; set;
        }

        public bool CheckBoxes
        {
            get; set;
        }

        public int SelectedIndex
        {
            get; set;
        }

        public bool FullRowSelect
        {
            get; set;
        }

        public SortOrder Order
        {
            get { return fSortOrder; }
            set { fSortOrder = value; }
        }

        public IListManager ListMan
        {
            get {
                return fListMan;
            }
            set {
                if (fListMan != value) {
                    if (fListMan != null) fListMan.Dispose();

                    fListMan = value;

                    if (fListMan != null) {
                        //VirtualMode = true;
                        fSortColumn = 0;
                        fSortOrder = SortOrder.Ascending;
                    } else {
                        //VirtualMode = false;
                    }

                    //VirtualMode = (fListMan != null);
                }
            }
        }


        public GKListView()
        {
            fItems = new ObservableCollection<GridItem>();
            DataStore = fItems;

            //SetStyle(ControlStyles.DoubleBuffer, true);
            //SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
            //SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            // Enable the OnNotifyMessage event so we get a chance to filter out
            // Windows messages before they get to the form's WndProc
            //SetStyle(ControlStyles.EnableNotifyMessage, true);

            //OwnerDraw = true;
            //HideSelection = false;
            //LabelEdit = false;
            FullRowSelect = true;
            //View = View.Details;

            fSortColumn = 0;
            fSortOrder = SortOrder.None;
            //fColumnSorter = new LVColumnSorter(this);

            //ListViewItemSorter = fColumnSorter;

            fListMan = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fListMan != null) {
                    fListMan.Dispose();
                    fListMan = null;
                }
            }
            base.Dispose(disposing);
        }

        public new void BeginUpdate()
        {
            if (fUpdateCount == 0)
            {
                #if !__MonoCS__
                //ListViewItemSorter = null;
                #endif
                //base.BeginUpdate();
            }

            fUpdateCount++;
        }

        public new void EndUpdate()
        {
            fUpdateCount--;

            if (fUpdateCount == 0)
            {
                //base.EndUpdate();
                #if !__MonoCS__
                //ListViewItemSorter = fColumnSorter;
                #endif
            }
        }

        protected SortOrder GetColumnSortOrder(int columnIndex)
        {
            return (fSortColumn == columnIndex) ? fSortOrder : SortOrder.None;
        }

        /*protected override void OnColumnClick(ColumnClickEventArgs e)
        {
            SortOrder prevOrder = GetColumnSortOrder(e.Column);
            fSortOrder = (prevOrder == SortOrder.Ascending) ? SortOrder.Descending : SortOrder.Ascending;
            fSortColumn = e.Column;

            SortContents(true);

            // we use Refresh() because only Invalidate() isn't update header's area
            Refresh();

            base.OnColumnClick(e);
        }*/

        /*protected override void OnDrawColumnHeader(DrawListViewColumnHeaderEventArgs e)
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
        }*/

        /*protected override void OnDrawItem(DrawListViewItemEventArgs e)
        {
            e.DrawDefault = true;
            base.OnDrawItem(e);
        }

        protected override void OnDrawSubItem(DrawListViewSubItemEventArgs e)
        {
            e.DrawDefault = true;
            base.OnDrawSubItem(e);
        }*/

        private void SortContents(bool restoreSelected)
        {
            if (fListMan != null) {
                object rec = (restoreSelected) ? GetSelectedData() : null;

                fListMan.SortContents(fSortColumn, fSortOrder == SortOrder.Ascending);
                ResetCache();

                if (restoreSelected) SelectItemByData(rec);
            } else {
                //Sort();
            }
        }

        #region Virtual mode with ListSource

        private static GKCore.Interfaces.IListItem CreateListItem(object itemValue, object data)
        {
            return new GKListItem(itemValue, data);
        }

        private GKListItem GetVirtualItem(int itemIndex)
        {
            GKListItem newItem;

            object rowData = fListMan.GetContentItem(itemIndex);
            if (rowData == null) {
                newItem = null;
            } else {
                newItem = fListMan.CreateListItem(rowData, CreateListItem) as GKListItem;
                fListMan.UpdateItem(newItem, rowData);
            }

            return newItem;
        }

        private void tempFillGrid()
        {
            int num = fListMan.FilteredCount;
            for (int i = 0; i < num; i++) {
                GKListItem newItem;
                object rowData = fListMan.GetContentItem(i);

                if (rowData == null) {
                    newItem = null;
                } else {
                    object[] itemData = fListMan.GetItemData(rowData);

                    newItem = new GKListItem(itemData);
                    newItem.Data = rowData;
                    fItems.Add(newItem);
                }
            }
        }

        /*protected override void OnCacheVirtualItems(CacheVirtualItemsEventArgs e)
        {
            // Only recreate the cache if we need to.
            if (fCache != null && e.StartIndex >= fCacheFirstItem && e.EndIndex <= fCacheFirstItem + fCache.Length) return;

            fCacheFirstItem = e.StartIndex;
            int length = e.EndIndex - e.StartIndex + 1;

            fCache = new GKListItem[length];
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
        }*/

        protected void ResetCache()
        {
            fCache = null;
        }

        /*protected override void OnColumnWidthChanged(ColumnWidthChangedEventArgs e)
        {
            if (fListMan != null && fUpdateCount == 0) {
                fListMan.ChangeColumnWidth(e.ColumnIndex, Columns[e.ColumnIndex].Width);
            }

            base.OnColumnWidthChanged(e);
        }*/

        public void UpdateContents(bool columnsChanged = false)
        {
            if (fListMan == null) return;

            try
            {
                if (fListMan.ColumnsHaveBeenChanged != columnsChanged && columnsChanged) {
                    fListMan.ColumnsHaveBeenChanged = columnsChanged;
                }

                object tempRec = GetSelectedData();

                BeginUpdate();
                try
                {
                    if (columnsChanged || Columns.Count == 0 || fListMan.ColumnsHaveBeenChanged) {
                        Columns.Clear();
                        fListMan.UpdateColumns(this);
                    }

                    fListMan.UpdateContents();
                    SortContents(false);

                    tempFillGrid();

                    /*VirtualListSize = fListMan.FilteredCount;

                    #if __MonoCS__
                    if (fListMan.FilteredCount != 0) {
                        TopItem = Items[0];
                    }
                    #endif*/

                    ResizeColumns();
                }
                finally
                {
                    EndUpdate();
                }

                if (tempRec != null) SelectItemByData(tempRec);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKListView.UpdateContents(): " + ex.Message);
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

        public void DeleteRecord(object data)
        {
            // crash protection: when you delete records from the diagrams,
            // between the actual deleting a record and updating the list
            // may take a few requests to update the list's items which does not already exist
            if (fListMan != null && fListMan.DeleteRecord(data)) {
                /*VirtualListSize = fListMan.FilteredCount;*/
            }
        }

        public void SelectItemByData(object rowData)
        {
            try {
                int idx = fListMan.IndexOfRecord(rowData);
                if (idx >= 0) {
                    ScrollToRow(idx);
                    SelectRow(idx);
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKListView.SelectItemByData(): " + ex.Message);
            }
        }

        public object GetSelectedData()
        {
            /*try {
                object result = null;

                if (!VirtualMode) {
                    GKListItem item = GetSelectedItem();
                    if (item != null) result = item.Data;
                } else {
                    if (SelectedIndices.Count > 0) {
                        int index = SelectedIndices[0];
                        result = fListMan.GetContentItem(index);
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.LogWrite("GKListView.GetSelectedData(): " + ex.Message);
                return null;
            }*/

            var item = GetSelectedItem();
            return (item != null) ? item.Data : null;
        }

        public void ClearItems()
        {
            fItems.Clear();
        }

        #endregion

        #region Public methods

        public void ClearColumns()
        {
            Columns.Clear();
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            //if (autoSize) width = -1;
            //Columns.Add(caption, width, HorizontalAlignment.Left);

            GridColumn column = new GridColumn();
            column.HeaderText = caption;
            column.DataCell = new TextBoxCell(Columns.Count);
            column.AutoSize = autoSize;
            column.Width = width;
            Columns.Add(column);
        }

        public void SetColumnCaption(int index, string caption)
        {
            Columns[index].HeaderText = caption;
        }

        public void ResizeColumn(int columnIndex)
        {
            try {
                /*if (columnIndex >= 0 && Items.Count > 0)
                {
                    AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

                    if (Columns[columnIndex].Width < 20)
                    {
                        AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.HeaderSize);
                    }
                }*/
            } catch (Exception ex) {
                Logger.LogWrite("GKListView.ResizeColumn(): " + ex.Message);
            }
        }

        public GKListItem AddItem(object rowData, params object[] columnValues)
        {
            var item = new GKListItem(columnValues);
            item.Data = rowData;
            fItems.Add(item);
            return item;
        }

        public GKListItem GetSelectedItem()
        {
            /*GKListItem result;

            if (SelectedItems.Count <= 0) {
                result = null;
            } else {
                result = (SelectedItems[0] as GKListItem);
            }

            return result;*/

            var item = SelectedItem as GKListItem;
            return item;
        }

        public void SelectItem(GKCore.Interfaces.IListItem item)
        {
            if (item == null) return;

            /*SelectedIndices.Clear();
            item.Selected = true;
            item.EnsureVisible();*/
        }

        public void SelectItem(int index)
        {
            /*if (index >= 0 && index < Items.Count) {
                ListViewItem item = Items[index];
                SelectItem(item);
            }*/
        }

        public void SelectItem(object data)
        {
            /*int num = Items.Count;
            for (int i = 0; i < num; i++) {
                var item = (GKListItem)Items[i];

                if (item.Data == data) {
                    SelectItem(item);
                    return;
                }
            }*/
        }

        #endregion
    }
}
