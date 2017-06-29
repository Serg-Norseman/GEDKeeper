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
using System.Collections.Generic;
using System.Collections.ObjectModel;
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /*
    /// <summary>
    /// 
    /// </summary>
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
        private Color fBackColor;
        private object fData;

        public Color BackColor
        {
            get { return fBackColor; }
            set { fBackColor = value; }
        }

        public object Data
        {
            get { return fData; }
            set { fData = value; }
        }

        public GKListItem(params object[] values) : base(values)
        {
            BackColor = Colors.Transparent;
        }

        public void AddSubItem(object itemValue)
        {
            
        }

        public void SetBackColor(IColor color)
        {
            BackColor = UIHelper.ConvertColor(color);
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
                    GKListItem item1 = (GKListItem)x;
                    GKListItem item2 = (GKListItem)y;

                    if (item1 is IComparable && item2 is IComparable) {
                        if (sortColumn == 0) {
                            IComparable eitem1 = (IComparable)x;
                            IComparable eitem2 = (IComparable)y;

                            result = eitem1.CompareTo(eitem2);
                        } else {
                            if (sortColumn < item1.Values.Length && sortColumn < item2.Values.Length)
                            {
                                IComparable sub1 = (IComparable)item1.Values[sortColumn];
                                IComparable sub2 = (IComparable)item2.Values[sortColumn];

                                result = sub1.CompareTo(sub2);
                            }
                        }
                    } else {
                        if (sortColumn < item1.Values.Length && sortColumn < item2.Values.Length)
                        {
                            result = agCompare(item1.Values[sortColumn].ToString(), item2.Values[sortColumn].ToString());
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
        private readonly ObservableCollection<GKListItem> fItems;

        private IListManager fListMan;
        private int fSortColumn;
        private SortOrder fSortOrder;
        private int fUpdateCount;


        public SortOrder Order
        {
            get { return fSortOrder; }
            set { fSortOrder = value; }
        }

        public int SortColumn
        {
            get { return fSortColumn; }
            set { fSortColumn = value; }
        }

        public int SelectedIndex
        {
            get; set;
        }

        public IList<GKListItem> Items
        {
            get { return fItems; }
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
                        fSortColumn = 0;
                        fSortOrder = SortOrder.Ascending;
                    } else {
                    }
                }
            }
        }


        public GKListView()
        {
            //SetStyle(ControlStyles.DoubleBuffer, true);
            //SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
            //SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            // Enable the OnNotifyMessage event so we get a chance to filter out
            // Windows messages before they get to the form's WndProc
            //SetStyle(ControlStyles.EnableNotifyMessage, true);
            //OwnerDraw = true;

            AllowColumnReordering = false;
            AllowMultipleSelection = false;
            fItems = new ObservableCollection<GKListItem>();
            DataStore = fItems;

            fListMan = null;
            fSortColumn = 0;
            fSortOrder = SortOrder.None;
            fColumnSorter = new LVColumnSorter(this);
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

        public void BeginUpdate()
        {
            if (fUpdateCount == 0)
            {
                #if !__MonoCS__
                //ListViewItemSorter = null;
                #endif
                //base.BeginUpdate();

                DataStore = null;
            }

            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;

            if (fUpdateCount == 0)
            {
                DataStore = fItems;

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

        protected override void OnColumnHeaderClick(GridColumnEventArgs e)
        {
            BeginUpdate();
            try {
                int columnIndex = this.Columns.IndexOf(e.Column);
                SortOrder prevOrder = GetColumnSortOrder(columnIndex);
                fSortOrder = (prevOrder == SortOrder.Ascending) ? SortOrder.Descending : SortOrder.Ascending;
                fSortColumn = columnIndex;

                SortContents(true);
                UpdateItems();
            } finally {
                EndUpdate();
            }

            base.OnColumnHeaderClick(e);
        }

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

        protected override void OnCellFormatting(GridCellFormatEventArgs e)
        {
            if (e.Row == this.SelectedIndex) return;

            var item = e.Item as GKListItem;
            if (item != null) {
                if (item.BackColor != Colors.Transparent) {
                    e.BackgroundColor = item.BackColor;
                    e.ForegroundColor = Colors.Black;
                }
            }

            base.OnCellFormatting(e);
        }

        private void SortContents(bool restoreSelected)
        {
            if (fListMan != null) {
                object rowData = (restoreSelected) ? GetSelectedData() : null;

                fListMan.SortContents(fSortColumn, fSortOrder == SortOrder.Ascending);

                if (restoreSelected) SelectItem(rowData);
            } else {
                //Sort();
            }
        }

        private void UpdateItems()
        {
            fItems.Clear();

            int num = fListMan.FilteredCount;
            for (int i = 0; i < num; i++) {
                object rowData = fListMan.GetContentItem(i);

                if (rowData != null) {
                    object[] itemData = fListMan.GetItemData(rowData);
                    GKListItem newItem = AddItem(rowData, itemData);
                    fListMan.UpdateItemProps(newItem, rowData);
                }
            }
        }

        #region Virtual mode with ListSource

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
                    UpdateItems();

                    ResizeColumns();
                }
                finally
                {
                    EndUpdate();
                }

                if (tempRec != null) SelectItem(tempRec);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKListView.UpdateContents(): " + ex.Message);
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

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            //if (autoSize) width = -1;
            //Columns.Add(caption, width, HorizontalAlignment.Left);

            GridColumn column = new GridColumn();
            column.HeaderText = caption;
            column.DataCell = new CheckBoxCell(Columns.Count);
            column.AutoSize = autoSize;
            column.Width = width;
            column.Editable = true;
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

        public void ResizeColumns()
        {
            if (fListMan == null) return;

            for (int i = 0; i < Columns.Count; i++) {
                if (fListMan.IsColumnAutosize(i)) {
                    ResizeColumn(i);
                }
            }
        }

        public void ClearItems()
        {
            fItems.Clear();
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
            var item = SelectedItem as GKListItem;
            return item;
        }

        public object GetSelectedData()
        {
            var item = GetSelectedItem();
            return (item != null) ? item.Data : null;
        }

        public void SelectItem(GKCore.Interfaces.IListItem item)
        {
            if (item != null) {
                int idx = fItems.IndexOf((GKListItem)item);
                SelectItem(idx);
            }
        }

        public void SelectItem(int index)
        {
            if (index >= 0 && index < fItems.Count) {
                ScrollToRow(index);
                SelectRow(index);
            }
        }

        public void SelectItem(object rowData)
        {
            try {
                if (fListMan != null) {
                    // "virtual" mode
                    int idx = fListMan.IndexOfRecord(rowData);
                    SelectItem(idx);
                } else {
                    int num = fItems.Count;
                    for (int i = 0; i < num; i++) {
                        var item = (GKListItem)fItems[i];

                        if (item.Data == rowData) {
                            SelectItem(i);
                            return;
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKListView.SelectItem(): " + ex.Message);
            }
        }

        #endregion
    }
}
