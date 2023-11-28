/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using BSLib;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Types;
using Xamarin.Forms;
using Xamarin.Forms.DataGrid;
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
    public class GKListItem : BSDListItem
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
            get { return (bool)Values[0]; }
            set { Values[0] = value; }
        }

        public object Tag { get; set; }

        public object[] Values { get; set; }

        public GKListItem(params object[] values)
        {
            BackColor = Color.Transparent;
            Values = values;
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
            if (index >= 0 && index < Values.Length)
                Values[index] = value;
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
            get { return (IUpdatableCollection)this.ItemsSource; }
        }

        public event EventHandler MouseDoubleClick;

        public bool AllowMultipleSelection { get; set; }

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
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

                        ItemsSource = fListMan.ContentList;
                        fIsVirtual = true;
                    } else {
                        ItemsSource = fItems;
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


        public GKListView()
        {
            InitializeComponent();

            fCheckedList = false;
            fIsVirtual = false;
            fItems = new ObservableExtList<GKListItem>();
            fSortColumn = 0;
            fSortOrder = BSDSortOrder.None;

            IsSortable = true;
            ItemsSource = fItems;
            RowsBackgroundColorPalette = new RowBgProvider(this);
            RowHeight = 26;

            fListMan = null;
        }

        public void Activate()
        {
            Focus();
        }

        public void BeginUpdate()
        {
            if (fUpdateCount == 0) {
                IsRefreshing = true;
                ContentList.BeginUpdate();
            }
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;
            if (fUpdateCount == 0) {
                ContentList.EndUpdate();
                IsRefreshing = false;
            }
        }

        private BSDSortOrder GetColumnSortOrder(int columnIndex)
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

        private int fRowFormatting = -1;
        private Color fRowBackColor = Color.White;

        internal Color GetRowColor(int rowIndex, object eItem)
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat)) {
                return Color.White;
            }

            if (fIsVirtual) {
                var item = eItem as ContentItem;
                if (item != null && fRowFormatting != rowIndex) {
                    var backColor = fListMan.GetBackgroundColor(rowIndex, item.Record);
                    fRowBackColor = (backColor != null) ? ((ColorHandler)backColor).Handle : Color.White;
                    fRowFormatting = rowIndex;
                }
                return fRowBackColor;
            } else {
                var item = eItem as GKListItem;
                if (item != null && item.BackColor != Color.Transparent && item.BackColor != Color.Black) {
                    return item.BackColor;
                }
            }

            return Color.White;
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
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.UpdateContents()", ex);
            }
        }

        public void DeleteRecord(object data)
        {
        }

        #endregion

        #region Public methods

        public void Clear()
        {
            ClearItems();
            ClearColumns();
        }

        public void ClearColumns()
        {
            Columns.Clear();
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            AddColumn(caption, width, autoSize, BSDTypes.HorizontalAlignment.Left);
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            fCheckedList = true;
            AddColumn(caption, width, autoSize, BSDTypes.HorizontalAlignment.Center);
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
            int index = Columns.Count;
            var binding = string.Format("Values[{0}]", index);

            LayoutOptions hca = LayoutOptions.Start;
            switch (textAlign) {
                case BSDTypes.HorizontalAlignment.Left:
                    hca = LayoutOptions.Start;
                    break;
                case BSDTypes.HorizontalAlignment.Right:
                    hca = LayoutOptions.End;
                    break;
                case BSDTypes.HorizontalAlignment.Center:
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
                /*if (columnIndex >= 0 && Items.Count > 0) {
                    AutoResizeColumn(columnIndex, ColumnHeaderAutoResizeStyle.ColumnContent);

                    if (Columns[columnIndex].Width < 20) {
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
            object item = null;
            if (!fIsVirtual) {
                if (index >= 0 && index < fItems.Count)
                    item = fItems[index];
            } else {
                if (index >= 0 && index < fListMan.ContentList.Count)
                    item = fListMan.ContentList[index];
            }

            if (item != null) {
                SelectedItem = item;
                ScrollTo(item, ScrollToPosition.MakeVisible, false);
            }
        }

        public void SelectItem(object rowData)
        {
            try {
                if (!fIsVirtual) {
                    for (int i = 0, num = fItems.Count; i < num; i++) {
                        if (fItems[i].Tag == rowData) {
                            SelectItem(i);
                            return;
                        }
                    }
                } else {
                    // "virtual" mode
                    int idx = fListMan.IndexOfItem(rowData);
                    SelectItem(idx);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKListView.SelectItem()", ex);
            }
        }

        #endregion

        #region CheckedList


        #endregion
    }
}
