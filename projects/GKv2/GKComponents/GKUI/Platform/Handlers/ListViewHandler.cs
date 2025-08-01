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

using System.Collections.Generic;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    public sealed class ListViewHandler : BaseControlHandler<GKListView, ListViewHandler>, IListView
    {
        public ListViewHandler(GKListView control) : base(control)
        {
        }

        public IListSource ListMan
        {
            get { return Control.ListMan; }
            set { Control.ListMan = value; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public int SortColumn
        {
            get { return Control.SortColumn; }
            set { Control.SortColumn = value; }
        }

        public GKSortOrder SortOrder
        {
            get { return Control.SortOrder; }
            set { Control.SortOrder = value; }
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            Control.AddCheckedColumn(caption, width, autoSize);
        }

        public void AddColumn(string caption, int width, bool autoSize = false, GKHorizontalAlignment textAlign = GKHorizontalAlignment.Left)
        {
            Control.AddColumn(caption, width, autoSize, textAlign);
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void ClearColumns()
        {
            Control.ClearColumns();
        }

        public void DeleteRecord(object data)
        {
            Control.DeleteRecord(data);
        }

        public object GetSelectedData()
        {
            return Control.GetSelectedData();
        }

        public IList<object> GetSelectedItems()
        {
            return Control.GetSelectedItems();
        }

        public void ResizeColumn(int columnIndex)
        {
            Control.ResizeColumn(columnIndex);
        }

        public void ResizeColumns()
        {
            Control.ResizeColumns();
        }

        public void SelectItem(object rowData)
        {
            Control.SelectItem(rowData);
        }

        public void SelectItem(int index)
        {
            Control.SelectItem(index);
        }

        public void SetColumnCaption(int index, string caption)
        {
            Control.SetColumnCaption(index, caption);
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            Control.SetSortColumn(sortColumn, checkOrder);
        }

        public void SortModelColumn(int columnId)
        {
            Control.SortModelColumn(columnId);
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            Control.UpdateContents(columnsChanged);
        }
    }
}
