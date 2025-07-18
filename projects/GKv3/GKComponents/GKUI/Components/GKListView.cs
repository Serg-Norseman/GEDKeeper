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
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKUI.Platform.Handlers;
using BSDSortOrder = GKCore.Design.BSDTypes.SortOrder;

namespace GKUI.Components
{
    /// <summary>
    ///
    /// </summary>
    public class GKListView : GKGridView, IListView
    {
        private bool fCheckBoxes;
        private IListSource fListMan;
        private int fSortColumn;
        private BSDSortOrder fSortOrder;


        public bool CheckBoxes
        {
            get { return fCheckBoxes; }
            set { fCheckBoxes = value; }
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
                        VirtualMode = true;
                        fSortColumn = 0;
                        fSortOrder = BSDSortOrder.Ascending;
                        DataStore = fListMan.ContentList;
                    } else {
                        VirtualMode = false;
                        DataStore = null;
                    }
                }
            }
        }

        public int SelectedIndex
        {
            get {
                return VirtualMode ? fListMan.ContentList.IndexOf(SelectedItem as ContentItem) : base.SelectedRow;
            }
            set {
                SelectItem(value);
            }
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


        public GKListView()
        {
            fCheckBoxes = false;
            VirtualMode = false;
            AllowColumnReordering = false;
            AllowMultipleSelection = false;
            // [Gtk] Selection of the last (or only) row does not work on left click; EtoForms issue #2443
            AllowEmptySelection = false;

            fSortColumn = 0;
            fSortOrder = BSDSortOrder.None;
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

            if (rec != null) SelectItem(rec);
        }

        public void SortModelColumn(int columnId)
        {
            if (fListMan == null) return;

            int sortColumn = fListMan.GetColumnIndex(columnId);
            if (sortColumn != -1) {
                SetSortColumn(sortColumn, false);
            }
        }

        protected override void OnColumnHeaderClick(GridColumnEventArgs e)
        {
            var columnIndex = Columns.IndexOf(e.Column);
            SetSortColumn(columnIndex);

            base.OnColumnHeaderClick(e);
        }

        private int fRowFormatting = -1;
        private Color fRowBackColor = Colors.White;

        protected override void OnRowFormatting(GridRowFormatEventArgs e)
        {
            base.OnRowFormatting(e);

            if (VirtualMode && HasGridCellFormat && e.Item is ContentItem item) {
                if (fRowFormatting != e.Row) {
                    var backColor = fListMan.GetBackgroundColor(e.Row, item.Record);
                    fRowBackColor = (backColor is ColorHandler colorHandler) ? colorHandler.Handle : this.BackgroundColor;
                    fRowFormatting = e.Row;
                }
                e.BackgroundColor = fRowBackColor;
            }
        }

        protected override void OnCellEdited(GridViewCellEventArgs e)
        {
            if (fCheckBoxes && e.Item is ContentItem contItem) {
                bool curValue = (bool)contItem.Values[e.Column];
                fListMan.SetColumnValue(e.Row, e.Column, !curValue);
            }
            base.OnCellEdited(e);
        }

        protected override void OnColumnWidthChanged(GridColumnEventArgs e)
        {
            if (fListMan != null) {
                fListMan.ChangeColumnWidth(e.Column.DisplayIndex, e.Column.Width);
            }

            base.OnColumnWidthChanged(e);
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            if (fListMan == null) return;

            try {
                object tempRec = GetSelectedData();
                fListMan.ContentList.BeginUpdate();
                try {
                    if (columnsChanged || Columns.Count == 0 || fListMan.ColumnsMap.Count == 0) {
                        fListMan.UpdateColumns(this);
                    }

                    fListMan.UpdateContents();
                    SortContents(false);

                    ResizeColumns();
                } finally {
                    fListMan.ContentList.EndUpdate();
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
            }
        }

        public void Clear()
        {
            Columns.Clear();
        }

        public void ClearColumns()
        {
            Columns.Clear();
        }

        public void AddColumn(string caption, int width, bool autoSize = false, BSDTypes.HorizontalAlignment textAlign = BSDTypes.HorizontalAlignment.Left)
        {
            int colIndex = Columns.Count;
            var cell = new TextBoxCell(colIndex);

            if (VirtualMode) {
                cell.Binding = Binding.Property<ContentItem, string>(r => (string)fListMan.GetColumnExternalValue(r, colIndex));
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
            int colIndex = Columns.Count;
            var cell = new CheckBoxCell(colIndex);

            if (VirtualMode) {
                cell.Binding = Binding.Property<ContentItem, bool?>(r => (bool?)fListMan.GetColumnExternalValue(r, colIndex));
            }

            GridColumn column = new GridColumn();
            column.HeaderText = caption;
            column.DataCell = cell;
            column.AutoSize = autoSize;
            column.Width = width;
            column.Editable = true;
            Columns.Add(column);

            fCheckBoxes = true;
        }

        public void SetColumnCaption(int index, string caption)
        {
            Columns[index].HeaderText = caption;
        }

        public void ResizeColumn(int columnIndex)
        {
            try {
                // not supported
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

                if (fListMan == null) {
                    result.AddRange(SelectedItems);
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
            if (SelectedRow < 0) return null;

            try {
                return (VirtualMode && SelectedItem is ContentItem item) ? item.Record : base.SelectedItem;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.GetSelectedData()", ex);
                return null;
            }
        }

        public void SelectItem(int index)
        {
            if (fListMan == null) return;

            if (index == -1) {
                index = fListMan.ContentList.Count - 1;
            }

            if (index >= 0 && index < fListMan.ContentList.Count) {
                ScrollToRow(index);
                UnselectAll();
                SelectRow(index);
            }
        }

        public void SelectItem(object rowData)
        {
            if (fListMan == null || rowData == null) return;

            try {
                // "virtual" mode
                int idx = fListMan.IndexOfItem(rowData);
                SelectItem(idx);
            } catch (Exception ex) {
                Logger.WriteError("GKListView.SelectItem()", ex);
            }
        }
    }
}
