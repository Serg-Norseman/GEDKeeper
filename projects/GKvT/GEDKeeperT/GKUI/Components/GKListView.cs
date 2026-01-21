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
using Terminal.Gui;

namespace GKUI.Components
{
    public class GKListView : TableView, IListView
    {
        private IListSource fListMan;

        public IListSource ListMan
        {
            get {
                return fListMan;
            }
            set {
                fListMan = value;
                UpdateContents(true);
            }
        }

        public int SelectedIndex
        {
            get => throw new System.NotImplementedException();
            set => throw new System.NotImplementedException();
        }

        public int SortColumn
        {
            get => throw new System.NotImplementedException();
            set => throw new System.NotImplementedException();
        }

        public GKSortOrder SortOrder
        {
            get => throw new System.NotImplementedException();
            set => throw new System.NotImplementedException();
        }


        public GKListView()
        {
            Style.AlwaysShowHeaders = true;
            Style.ExpandLastColumn = true;
            FullRowSelect = true;
        }

        public void AddColumn(string caption, int width, bool autoSize = false, GKHorizontalAlignment textAlign = GKHorizontalAlignment.Left)
        {
        }

        public IList<object> GetSelectedItems()
        {
            return new List<object>();
        }

        public void ResizeColumns()
        {
        }

        public void SortModelColumn(int columnId)
        {
        }

        public void Activate()
        {
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize)
        {
        }

        public void Clear()
        {
        }

        public void ClearColumns()
        {
        }

        public void DeleteRecord(object data)
        {
        }

        public object GetSelectedData()
        {
            if (fListMan == null)
                return null;

            var contentList = fListMan.ContentList;
            int selectedRow = base.SelectedRow;
            return (selectedRow >= 0 && selectedRow < contentList.Count) ? contentList[selectedRow].Record : null;
        }

        public void ResizeColumn(int columnIndex)
        {
        }

        public void SelectItem(int index)
        {
        }

        public void SelectItem(object rowData)
        {
        }

        public void SetColumnCaption(int index, string caption)
        {
        }

        public void SetSortColumn(int sortColumn, bool checkOrder)
        {
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            if (fListMan == null) return;

            try {
                fListMan.ContentList.BeginUpdate();
                fListMan.UpdateContents();
                fListMan.SortContents(false);
                fListMan.ContentList.EndUpdate();

                var dt = new DataTable();
                fListMan.UpdateColumns(this);
                foreach (var col in fListMan.ColumnsMap) {
                    dt.Columns.Add(col.Caption);
                }
                for (int i = 0; i < fListMan.ContentList.Count; i++) {
                    var item = fListMan.GetContentItem(i);
                    var ex = fListMan.GetItemData(item);
                    dt.Rows.Add(ex);
                }
                this.Table = dt;
            } catch (Exception ex) {
                Logger.WriteError("GKListView.UpdateContents()", ex);
            }
        }
    }
}
