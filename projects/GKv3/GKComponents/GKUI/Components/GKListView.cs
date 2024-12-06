/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Runtime.InteropServices;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Platform.Handlers;
using BSDListItem = GKCore.Design.Controls.IListItem;
using BSDSortOrder = GKCore.Design.BSDTypes.SortOrder;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class ObservableExtList<T> : ExtObservableList<T>, IListViewItems where T : BSDListItem
    {
        BSDListItem IControlItems<BSDListItem>.this[int index]
        {
            get { return base[index]; }
        }

        public ObservableExtList()
        {
        }
    }


    /// <summary>
    ///
    /// </summary>
    public class GKListItem : GridItem, BSDListItem
    {
        private Color fBackColor;
        private Color fForeColor;

        public Color BackColor
        {
            get { return fBackColor; }
            set { fBackColor = value; }
        }

        public Color ForeColor
        {
            get { return fForeColor; }
            set { fForeColor = value; }
        }

        public bool Checked
        {
            get { return (bool)base.Values[0]; }
            set { base.Values[0] = value; }
        }

        public GKListItem(params object[] values) : base(values)
        {
            BackColor = Colors.Transparent;
        }

        public int CompareTo(object obj)
        {
            return 0;
        }

        public void SetBackColor(IColor color)
        {
            var colorHandler = color as ColorHandler;
            if (colorHandler != null) {
                BackColor = colorHandler.Handle;
            }
        }

        public void SetForeColor(IColor color)
        {
            var colorHandler = color as ColorHandler;
            if (colorHandler != null) {
                ForeColor = colorHandler.Handle;
            }
        }

        public void SetSubItem(int index, object value)
        {
            if (index >= 0 && index < base.Values.Length)
                base.Values[index] = value;
        }
    }


    public class ItemCheckEventArgs : EventArgs
    {
        public int Index { get; set; }
        public bool NewValue { get; set; }

        public ItemCheckEventArgs(int index, bool newValue)
        {
            Index = index;
            NewValue = newValue;
        }
    }


    public delegate void ItemCheckEventHandler(object sender, ItemCheckEventArgs e);


    /// <summary>
    ///
    /// </summary>
    public class GKListView : GridView, IListView
    {
        private readonly ObservableExtList<GKListItem> fItems;

        private bool fCheckedList;
        private bool fIsVirtual;
        private IListSource fListMan;
        private bool fSorting;
        private int fSortColumn;
        private BSDSortOrder fSortOrder;
        private int fUpdateCount;


        private IUpdatableCollection ContentList
        {
            get { return (IUpdatableCollection)this.DataStore; }
        }

        IListViewItems IListView.Items
        {
            get { return fItems; }
        }

        public IListSource ListMan
        {
            get {
                return fListMan;
            }
            set {
                if (fListMan != value) {
                    fListMan = value;

                    if (fListMan != null) {
                        fSorting = true;
                        fSortColumn = 0;
                        fSortOrder = BSDSortOrder.Ascending;

                        DataStore = fListMan.ContentList;
                        fIsVirtual = true;
                    } else {
                        DataStore = fItems;
                        fIsVirtual = false;
                    }
                }
            }
        }

        public int SelectedIndex
        {
            get {
                int index;
                if (!fIsVirtual) {
                    index = fItems.IndexOf(SelectedItem as GKListItem);
                } else {
                    index = fListMan.ContentList.IndexOf(SelectedItem as ContentItem);
                }
                return index;
            }
            set {
                SelectItem(value);
            }
        }

        public bool Sorting
        {
            get { return fSorting; }
            set { fSorting = value; }
        }

        public int SortColumn
        {
            get { return fSortColumn; }
            set { fSortColumn = value; }
        }

        public BSDSortOrder SortOrder
        {
            get { return fSortOrder; }
            set { fSortOrder = value; }
        }


        public event ItemCheckEventHandler ItemCheck;

        public event EventHandler ItemsUpdated;


        public GKListView()
        {
            fCheckedList = false;
            fIsVirtual = false;
            fItems = new ObservableExtList<GKListItem>();
            fSortColumn = 0;
            fSortOrder = BSDSortOrder.None;

            AllowColumnReordering = false;
            AllowMultipleSelection = false;

            // [Gtk] Selection of the last (or only) row does not work on left click; EtoForms issue #2443
            AllowEmptySelection = false;

            DataStore = fItems;

            fListMan = null;
        }

        public void Activate()
        {
            try {
                Focus();
            } catch {
                // why is an exception thrown here?
            }
        }

        public void BeginUpdate()
        {
            if (fUpdateCount == 0) {
                ContentList.BeginUpdate();
            }
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;
            if (fUpdateCount == 0) {
                ContentList.EndUpdate();
            }
        }

        protected BSDSortOrder GetColumnSortOrder(int columnIndex)
        {
            return (fSortColumn == columnIndex) ? fSortOrder : BSDSortOrder.None;
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            int prevColumn = fSortColumn;
            BSDSortOrder sortOrder;
            if (prevColumn == sortColumn && checkOrder) {
                var prevOrder = GetColumnSortOrder(sortColumn);
                sortOrder = (prevOrder == BSDSortOrder.Ascending) ? BSDSortOrder.Descending : BSDSortOrder.Ascending;
            } else {
                sortOrder = BSDSortOrder.Ascending;
            }

            Sort(sortColumn, sortOrder);
        }

        public void Sort(int sortColumn, BSDSortOrder sortOrder)
        {
            fSortColumn = sortColumn;
            fSortOrder = sortOrder;

            var rowData = GetSelectedData();

            BeginUpdate();
            try {
                SortContents();
            } finally {
                EndUpdate();
            }

            if (rowData != null) SelectItem(rowData);
        }

        protected override void OnColumnHeaderClick(GridColumnEventArgs e)
        {
            var columnIndex = Columns.IndexOf(e.Column);
            SetSortColumn(columnIndex);

            base.OnColumnHeaderClick(e);
        }

        /*protected override void OnSelectionChanged(EventArgs e)
        {
            base.OnSelectionChanged(e);

            // FIXME: [Wpf]GridView.ReloadData(...) is very slow, Eto 2.7.0 #2245
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) {
                base.ReloadData(base.SelectedRow);
            }
        }*/

        private int fRowFormatting = -1;
        private Color fRowBackColor = Colors.White;

        protected override void OnCellFormatting(GridCellFormatEventArgs e)
        {
            base.OnCellFormatting(e);

            if (!AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat)) {
                return;
            }

            // FIXME: doesn't work correctly because selection changes don't call this method (Eto <= 2.7.0)
            // This method only works with OnSelectionChanged -> ReloadData(SelectedRow)
            // In Gtk works correctly without this.
            // In Wpf without this selected row has invalid background.
            /*if (e.Row == base.SelectedRow) {
                e.BackgroundColor = SystemColors.Selection;
                e.ForegroundColor = SystemColors.SelectionText;
                return;
            }*/

            if (fIsVirtual) {
                var item = e.Item as ContentItem;
                if (item != null) {
                    if (fRowFormatting != e.Row) {
                        var backColor = fListMan.GetBackgroundColor(e.Row, item.Record);
                        fRowBackColor = (backColor != null) ? ((ColorHandler)backColor).Handle : Colors.White;
                        fRowFormatting = e.Row;
                    }
                    e.BackgroundColor = fRowBackColor;
                }
            } else {
                var item = e.Item as GKListItem;
                if (item != null) {
                    if (item.BackColor != Colors.Transparent) {
                        e.BackgroundColor = item.BackColor;
                    } else {
                        e.BackgroundColor = Colors.White;
                    }
                }
            }
            e.ForegroundColor = SystemColors.ControlText;
        }

        private int CompareItems(GKListItem item1, GKListItem item2)
        {
            int result = 0;

            if (fSortOrder != BSDSortOrder.None && fSortColumn >= 0) {
                if (fSortColumn < item1.Values.Length && fSortColumn < item2.Values.Length) {
                    IComparable val1 = item1.Values[fSortColumn] as IComparable;
                    IComparable val2 = item2.Values[fSortColumn] as IComparable;

                    if (val1 != null && val2 != null) {
                        bool isStr1 = val1 is string;
                        bool isStr2 = val2 is string;

                        if (isStr1 && isStr2) {
                            result = GKUtils.StrCompareEx((string)val1, (string)val2);
                        } else {
                            result = val1.CompareTo(val2);
                        }
                    }
                }

                if (fSortOrder == BSDSortOrder.Descending) {
                    result = -result;
                }
            }

            return result;
        }

        #region Virtual mode with ListSource

        protected override void OnColumnWidthChanged(GridColumnEventArgs e)
        {
            if (fListMan != null && fUpdateCount == 0) {
                fListMan.ChangeColumnWidth(e.Column.DisplayIndex, e.Column.Width);
            }

            base.OnColumnWidthChanged(e);
        }

        private void SortContents()
        {
            if (fSorting) {
                if (fListMan != null) {
                    fListMan.SortContents(fSortColumn, fSortOrder == BSDSortOrder.Ascending);
                } else {
                    SortHelper.MergeSort(fItems, CompareItems);
                }
            }
        }

        public void SortModelColumn(int columnId)
        {
            if (fListMan != null) {
                int sortColumn = fListMan.GetColumnIndex(columnId);
                if (sortColumn != -1) {
                    SetSortColumn(sortColumn, false);
                }
            }
        }

        private void DoItemsUpdated()
        {
            var eventHandler = ItemsUpdated;
            if (eventHandler != null) eventHandler(this, new EventArgs());
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            if (fListMan == null) return;

            try {
                object tempRec = GetSelectedData();
                BeginUpdate();
                try {
                    if (columnsChanged || Columns.Count == 0) {
                        fListMan.UpdateColumns(this);
                    }

                    fListMan.UpdateContents();
                    SortContents();

                    ResizeColumns();
                } finally {
                    EndUpdate();
                    if (tempRec != null) SelectItem(tempRec);

                    DoItemsUpdated();
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
            }
        }

        #endregion

        #region Public methods

        public void Clear()
        {
            Columns.Clear();
            fItems.Clear();
        }

        public void ClearColumns()
        {
            Columns.Clear();
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            int colIndex = Columns.Count;
            var cell = new TextBoxCell(colIndex);

            if (fIsVirtual) {
                cell.Binding = Binding.Property<ContentItem, string>(r => fListMan.GetColumnExternalValue(r, colIndex));
            }

            GridColumn column = new GridColumn();
            column.HeaderText = caption;
            column.DataCell = cell;
            column.AutoSize = autoSize;
            column.Width = width;
            column.Sortable = true;
            Columns.Add(column);
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            var cell = new CheckBoxCell(Columns.Count);

            GridColumn column = new GridColumn();
            column.HeaderText = caption;
            column.DataCell = cell;
            column.AutoSize = autoSize;
            column.Width = width;
            column.Editable = true;
            Columns.Add(column);

            fCheckedList = true;
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
            AddColumn(caption, width, autoSize);
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

        public void ClearItems()
        {
            fItems.Clear();
        }

        public BSDListItem AddItem(object rowData, params object[] columnValues)
        {
            return AddItem(rowData, false, columnValues);
        }

        public BSDListItem AddItem(object rowData, bool isChecked, params object[] columnValues)
        {
            object[] itemValues;
            if (fCheckedList) {
                int num = columnValues.Length;
                itemValues = new object[num + 1];
                itemValues[0] = isChecked;
                Array.Copy(columnValues, 0, itemValues, 1, num);
            } else {
                itemValues = columnValues;
            }

            var item = new GKListItem(itemValues);
            item.Tag = rowData;
            fItems.Add(item);
            return item;
        }

        public IList<object> GetSelectedItems()
        {
            try {
                var result = new List<object>();

                if (fListMan == null) {
                    foreach (GKListItem item in SelectedItems) {
                        result.Add(item.Tag);
                    }
                } else {
                    foreach (var index in SelectedRows) {
                        result.Add(fListMan.GetContentItem(index));
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.GetSelectedItems()", ex);
                return null;
            }
        }

        public object GetSelectedData()
        {
            if (SelectedRow < 0) {
                return null;
            }

            if (!fIsVirtual) {
                var item = SelectedItem as GKListItem;
                return (item != null) ? item.Tag : null;
            } else {
                var item = SelectedItem as ContentItem;
                return (item != null) ? item.Record : null;
            }
        }

        public void SelectItem(int index)
        {
            if (index == -1) {
                index = ContentList.Count - 1;
            }

            if (index >= 0 && index < ContentList.Count) {
                ScrollToRow(index);
                UnselectAll();
                SelectRow(index);
            }
        }

        public void SelectItem(object rowData)
        {
            try {
                if (fListMan != null) {
                    // "virtual" mode
                    int idx = fListMan.IndexOfItem(rowData);
                    SelectItem(idx);
                } else {
                    int num = fItems.Count;
                    for (int i = 0; i < num; i++) {
                        if (fItems[i].Tag == rowData) {
                            SelectItem(i);
                            return;
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.SelectItem()", ex);
            }
        }

        #endregion

        #region CheckedList

        protected override void OnCellEdited(GridViewCellEventArgs e)
        {
            if (fCheckedList) {
                if (e.Column == 0) {
                    DoItemCheck(e.Row, ((bool)((GKListItem)e.Item).Values[0]));
                }
            }
            base.OnCellEdited(e);
        }

        private void DoItemCheck(int index, bool newValue)
        {
            ItemCheckEventHandler handler = this.ItemCheck;
            if (handler != null)
                handler.Invoke(this, new ItemCheckEventArgs(index, newValue));
        }

        #endregion
    }
}
