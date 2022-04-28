/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

using BSLib.Design;
using BSLib.Design.Handlers;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP.Controls;
using GKUI.Components;

namespace GKUI.Platform
{
    public sealed class DateBoxHandler : BaseControlHandler<GKDateBox, DateBoxHandler>, IDateBox
    {
        public DateBoxHandler(GKDateBox control) : base(control)
        {
        }

        public string NormalizeDate
        {
            get { return Control.NormalizeDate; }
            set { Control.NormalizeDate = value; }
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
        }

        public string SelectedText
        {
            get { return Control.SelectedText; }
            set { Control.SelectedText = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void Copy()
        {
            Control.Copy();
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }


    public sealed class LogChartHandler : BaseControlHandler<LogChart, LogChartHandler>, ILogChart
    {
        public LogChartHandler(LogChart control) : base(control)
        {
        }

        public void AddFragment(int val)
        {
            Control.AddFragment(val);
        }

        public void Clear()
        {
            Control.Clear();
        }
    }



    public sealed class DateControlHandler : BaseControlHandler<GKDateControl, DateControlHandler>, IDateControl
    {
        public GDMCustomDate Date
        {
            get { return Control.Date; }
            set { Control.Date = value; }
        }

        public DateControlHandler(GKDateControl control) : base(control)
        {
        }
    }


    public sealed class ListViewHandler : BaseControlHandler<GKListView, ListViewHandler>, IListView
    {
        public ListViewHandler(GKListView control) : base(control)
        {
        }

        public IListViewItems Items
        {
            get { return (IListViewItems)Control.Items; }
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

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            Control.AddColumn(caption, width, autoSize);
        }

        public void AddColumn(string caption, int width, bool autoSize)
        {
            Control.AddColumn(caption, width, autoSize);
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
            Control.AddColumn(caption, width, autoSize, textAlign);
        }

        public IListItem AddItem(object rowData, bool isChecked, params object[] columnValues)
        {
            return Control.AddItem(rowData, isChecked, columnValues);
        }

        public IListItem AddItem(object rowData, params object[] columnValues)
        {
            return Control.AddItem(rowData, columnValues);
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void ClearColumns()
        {
            Control.ClearColumns();
        }

        public void ClearItems()
        {
            Control.ClearItems();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public object GetSelectedData()
        {
            return Control.GetSelectedData();
        }

        public void SelectItem(object rowData)
        {
            Control.SelectItem(rowData);
        }

        public void SetColumnCaption(int index, string caption)
        {
            Control.SetColumnCaption(index, caption);
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            Control.SetSortColumn(sortColumn, checkOrder);
        }

        public void Sort(int sortColumn, BSDTypes.SortOrder sortOrder)
        {
            Control.Sort(sortColumn, sortOrder);
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            Control.UpdateContents(columnsChanged);
        }
    }
}
