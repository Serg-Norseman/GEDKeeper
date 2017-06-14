/*
 * Created by SharpDevelop.
 * User: Norseman
 * Date: 08.06.2017
 * Time: 17:40
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using Eto.Forms;
using GKCore.Interfaces;

namespace GKUI.Components
{
    public class GKListSubItem
    {
        
    }

    public class GKListItem : GridItem, GKCore.Interfaces.IListItem
    {
        public object Data
        {
            get; set;
        }

        public GKListItem(object dataValue)
        {
            
        }

        public GKListItem(object dataValue, object tag)
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
    public class GKListViewStub : GridView, IListView
    {
        private ObservableCollection<GridItem> fItems;

        public IListManager ListMan
        {
            get { return null; }
        }

        public int SortColumn
        {
            get; set;
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

        public GKListViewStub()
        {
            fItems = new ObservableCollection<GridItem>();
            DataStore = fItems;
        }

        public GKListItem GetSelectedItem()
        {
            return null;
        }

        public object GetSelectedData()
        {
            return null;
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            //var gv = new GridView();
            //gv.DataStore
        }

        public GKListItem AddItem(string text, object data)
        {
            return null;
        }

        public GKListItem AddItem(object itemValue, object data, GKListSubItem[] subitemsValues)
        {
            return null;
        }

        public GKListItem AddItem(object rowData, params object[] columnValues)
        {
            var item = new GKListItem(columnValues);
            fItems.Add(item);
            return item;
        }

        public void DeleteRecord(object data)
        {
        }

        public void BeginUpdate()
        {
        }

        public void EndUpdate()
        {
        }

        public void SelectItemByData(object data)
        {
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
            GridColumn column = new GridColumn();
            column.HeaderText = caption;
            column.DataCell = new TextBoxCell(Columns.Count);
            column.Width = width;
            column.AutoSize = autoSize;
            Columns.Add(column);
        }

        public void SetColumnCaption(int index, string caption)
        {
            Columns[index].HeaderText = caption;
        }

        public void ResizeColumn(int columnIndex)
        {
        }

        public void SelectItem(int index)
        {
        }

        public void SelectItem(object data)
        {
        }

        public void ClearColumns()
        {
        }

        public void ClearItems()
        {
        }
    }
}
