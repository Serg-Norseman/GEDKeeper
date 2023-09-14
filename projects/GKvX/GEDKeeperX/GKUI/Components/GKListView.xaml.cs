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
using System.Collections.ObjectModel;
using System.Linq.Expressions;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using Xamarin.Forms;
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
            get { return (BSDListItem)base[index]; }
        }

        public new BSDListItem this[int index]
        {
            get { return (BSDListItem)base[index]; }
        }

        public new int Count
        {
            get { return base.Count; }
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

        public object Data { get; set; }

        public object[] Values { get; set; }

        public GKListItem(params object[] values)
        {
            //BackColor = Colors.Transparent;
            Values = values;
        }

        public int CompareTo(object obj)
        {
            return 0;
        }

        public void AddSubItem(object itemValue)
        {
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

        public void SetFont(IFont font)
        {
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


    /// <summary>
    ///
    /// </summary>
    public sealed partial class GKListView : ListView, IListView
    {
        private readonly List<string> fColumns;
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

        public bool Enabled { get; set; }

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

        public GKListView()
        {
            InitializeComponent();

            fColumns = new List<string>();
            fCheckedList = false;
            fIsVirtual = false;
            fItems = new ObservableExtList<GKListItem>();
            fSortColumn = 0;
            fSortOrder = BSDSortOrder.None;

            ItemsSource = fItems;

            fListMan = null;
        }

        public void Activate()
        {
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

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
        }

        public void Sort(int sortColumn, BSDSortOrder sortOrder)
        {
        }

        public void SortModelColumn(int columnId)
        {
        }

        public void UpdateContents(bool columnsChanged = false)
        {
        }

        public void DeleteRecord(object data)
        {
        }

        public void Clear()
        {
            ClearItems();
            ClearColumns();
        }

        public void ClearColumns()
        {
            fColumns.Clear();
            ItemTemplate = null;
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            int index = fColumns.Count;
            var binding = string.Format("Values[{0}]", index);
            fColumns.Add(binding);

            //Expression<Func<GKListItem, object>> myLambda;
            //myLambda = item => Convert.ToString(item.Values[0]);
            /*
            //lbl.SetBinding<GKListItem>(Label.TextProperty, myLambda, BindingMode.OneWay); // vm => Convert.ToString(vm.Values[index])
            lbl.SetBinding(Label.TextProperty, "Values[0]"); //string.Format("Values[{0}]", index)); // string.Format("Values[{0}]", index)
            */

            ItemTemplate = new DataTemplate(() => {
                var rowView = new StackLayout() {
                    Orientation = StackOrientation.Horizontal
                };

                foreach (var col in fColumns) {
                    var lbl = new Label();
                    //lbl.SetBinding<GKListItem>(Label.TextProperty, col);
                    lbl.SetBinding(Label.TextProperty, col);

                    rowView.Children.Add(lbl);
                }

                return new ViewCell() {
                    View = rowView
                };
            });
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
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
            item.Data = rowData;
            fItems.Add(item);
            return item;
        }

        public object GetSelectedData()
        {
            if (!fIsVirtual) {
                var item = SelectedItem as GKListItem;
                return (item != null) ? item.Data : null;
            } else {
                var item = SelectedItem as ContentItem;
                return (item != null) ? item.Record : null;
            }
        }

        public void SelectItem(int index)
        {
        }

        public void SelectItem(object rowData)
        {
        }
    }
}
