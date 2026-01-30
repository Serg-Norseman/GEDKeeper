/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Data;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Lists;
using Terminal.Gui.Input;
using Terminal.Gui.Views;

namespace GKUI.Components
{
    public class GKListView : TableView, IListView
    {
        private readonly DataTable fDataTable;
        private IListSource fListMan;

        public IListSource ListMan
        {
            get {
                return fListMan;
            }
            set {
                if (fListMan != value) {
                    fListMan = value;
                    UpdateContents(true);
                }
            }
        }

        public int SelectedIndex
        {
            get {
                return /*(fListMan == null) ? -1 : fListMan.ContentList.IndexOf(SelectedItem as ContentItem)*/ -1;
            }
            set {
                SelectItem(value);
            }
        }

        public int SortColumn
        {
            get { return (fListMan != null) ? fListMan.SortColumn : 0; }
            set { if (fListMan != null) fListMan.SortColumn = value; }
        }

        public GKSortOrder SortOrder
        {
            get { return (fListMan != null) ? fListMan.SortOrder : GKSortOrder.None; }
            set { if (fListMan != null) fListMan.SortOrder = value; }
        }


        public GKListView()
        {
            Style.AlwaysShowHeaders = true;
            Style.ExpandLastColumn = true;
            FullRowSelect = true;
            CanFocus = true;
            VerticalScrollBar.Visible = true;

            // if user clicks the mouse in TableView
            this.Activating += (s, e) => {
                if (e.Context?.Binding is MouseBinding mouseBind) {
                    var mouse = mouseBind.MouseEvent;
                    this.ScreenToCell(mouse.Position!.Value, out int? clickedCol);
                    if (clickedCol.HasValue && mouse.Flags.HasFlag(MouseFlags.LeftButtonPressed)) {
                        this.SetSortColumn(clickedCol.Value);
                    }
                }
            };

            fDataTable = new DataTable();
            this.Table = new DataTableSource(fDataTable);
        }

        public void Activate()
        {
            SetFocus();
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            if (fListMan == null) return;

            object rec = GetSelectedData();

            fListMan.SetSortColumn(sortColumn, checkOrder);
            UpdateDataTable();

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

        public void UpdateContents(bool columnsChanged = false)
        {
            if (fListMan == null) return;

            try {
                object tempRec = GetSelectedData();
                fListMan.ContentList.BeginUpdate();
                try {
                    if (columnsChanged || fDataTable.Columns.Count == 0 || fListMan.ColumnsMap.Count == 0) {
                        fListMan.UpdateColumns(this);

                        fDataTable.Columns.Clear();
                        foreach (var col in fListMan.ColumnsMap) {
                            fDataTable.Columns.Add(col.Caption);
                        }
                    }

                    fListMan.UpdateContents();
                    fListMan.SortContents(false);

                    UpdateDataTable();

                    ResizeColumns();
                } finally {
                    fListMan.ContentList.EndUpdate();
                    if (tempRec != null) SelectItem(tempRec);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.UpdateContents()", ex);
            }
        }

        private void UpdateDataTable()
        {
            fDataTable.BeginLoadData();
            fDataTable.Rows.Clear();
            for (int i = 0; i < fListMan.ContentList.Count; i++) {
                var item = fListMan.GetContentItem(i);
                var row = fListMan.GetItemData(item);
                fDataTable.Rows.Add(row);
            }
            fDataTable.EndLoadData();
            this.Update();
        }

        public void DeleteRecord(object data)
        {
        }

        public void Clear()
        {
        }

        public void ClearColumns()
        {
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            AddColumn(caption, width, autoSize, GKHorizontalAlignment.Left);
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
        }

        public void AddColumn(string caption, int width, bool autoSize = false, GKHorizontalAlignment textAlign = GKHorizontalAlignment.Left)
        {
        }

        public void SetColumnCaption(int index, string caption)
        {
        }

        public void ResizeColumn(int columnIndex)
        {
        }

        public void ResizeColumns()
        {
            if (fListMan == null) return;

            /*for (int i = 0; i < Columns.Count; i++) {
                if (fListMan.IsColumnAutosize(i)) {
                    ResizeColumn(i);
                }
            }*/
        }

        public IList<object> GetSelectedItems()
        {
            return new List<object>();
        }

        public object GetSelectedData()
        {
            if (fListMan == null)
                return null;

            var contentList = fListMan.ContentList;
            int selectedRow = base.SelectedRow;
            return (selectedRow >= 0 && selectedRow < contentList.Count) ? contentList[selectedRow].Record : null;
        }

        public void SelectItem(int index)
        {
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
