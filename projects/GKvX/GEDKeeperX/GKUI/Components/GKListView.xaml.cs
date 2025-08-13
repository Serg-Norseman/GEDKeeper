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
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Lists;
using Xamarin.Forms;
using Xamarin.Forms.DataGrid;

namespace GKUI.Components
{
    internal class RowBgProvider : IColorProvider
    {
        private readonly GKListView fListView;

        public RowBgProvider(GKListView listView)
        {
            fListView = listView;
        }

        public Color GetColor(int rowIndex, object item)
        {
            return fListView.GetRowColor(rowIndex, item);
        }
    }


    /// <summary>
    ///
    /// </summary>
    public sealed partial class GKListView : DataGrid, IListView
    {
        private bool fCheckBoxes;
        private IListSource fListMan;

        public event EventHandler MouseDoubleClick;

        public bool AllowMultipleSelection { get; set; }

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return base.IsVisible; }
            set { base.IsVisible = value; }
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
                        ItemsSource = fListMan.ContentList;
                    } else {
                        ItemsSource = null;
                    }
                }
            }
        }

        public int SelectedIndex
        {
            get {
                return (fListMan == null) ? -1 : fListMan.ContentList.IndexOf(SelectedItem as ContentItem);
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
            InitializeComponent();
            IsSortable = true;
            RowsBackgroundColorPalette = new RowBgProvider(this);
            RowHeight = 26;

            fCheckBoxes = false;
            fListMan = null;
        }

        public void Activate()
        {
            Focus();
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            if (fListMan == null) return;

            object rec = GetSelectedData();

            fListMan.SetSortColumn(sortColumn, checkOrder);

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

        private int fRowFormatting = -1;
        private Color fRowBackColor = Color.White;

        internal Color GetRowColor(int rowIndex, object eItem)
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat)) {
                return Color.White;
            }

            if (fListMan != null) {
                var item = eItem as ContentItem;
                if (item != null && fRowFormatting != rowIndex) {
                    var backColor = fListMan.GetBackgroundColor(rowIndex, item.Record);
                    fRowBackColor = (backColor != null) ? ((ColorHandler)backColor).Handle : Color.White;
                    fRowFormatting = rowIndex;
                }
                return fRowBackColor;
            }

            return Color.White;
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
                    fListMan.SortContents(false);

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
        }

        public void Clear()
        {
            Columns.Clear();
        }

        public void ClearColumns()
        {
            Columns.Clear();
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            AddColumn(caption, width, autoSize, GKHorizontalAlignment.Left);
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            fCheckBoxes = true;
            AddColumn(caption, width, autoSize, GKHorizontalAlignment.Center);
        }

        public void AddColumn(string caption, int width, bool autoSize, GKHorizontalAlignment textAlign)
        {
            int index = Columns.Count;
            var binding = string.Format("Values[{0}]", index);

            LayoutOptions hca = LayoutOptions.Start;
            switch (textAlign) {
                case GKHorizontalAlignment.Left:
                    hca = LayoutOptions.Start;
                    break;
                case GKHorizontalAlignment.Right:
                    hca = LayoutOptions.End;
                    break;
                case GKHorizontalAlignment.Center:
                    hca = LayoutOptions.Center;
                    break;
            }

            Columns.Add(new DataGridColumn() {
                Title = caption,
                Width = width,
                PropertyName = binding,
                HorizontalContentAlignment = hca
            });
        }

        public void SetColumnCaption(int index, string caption)
        {
            Columns[index].Title = caption;
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

                /*if (!fIsVirtual) {
                    foreach (GKListItem item in SelectedItems) {
                        result.Add(item.Tag);
                    }
                } else {
                    foreach (var index in SelectedRows) {
                        result.Add(fListMan.GetContentItem(index));
                    }
                }*/

                return result;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.GetSelectedItems()", ex);
                return null;
            }
        }

        public object GetSelectedData()
        {
            try {
                return (fListMan != null && SelectedItem is ContentItem item) ? item.Record : null;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.GetSelectedData()", ex);
                return null;
            }
        }

        public void SelectItem(int index)
        {
            if (fListMan == null) return;

            if (index >= 0 && index < fListMan.ContentList.Count) {
                var item = fListMan.ContentList[index];
                SelectedItem = item;
                ScrollTo(item, ScrollToPosition.MakeVisible, false);
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
