/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GKCore.Interfaces;
using Terminal.Gui;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKListView : ListView, IListViewEx
    {
        IListSource IListViewEx.ListMan { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        IListViewItems IListView.Items => throw new System.NotImplementedException();

        int IListView.SelectedIndex { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }
        int IListView.SortColumn { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }
        bool IBaseControl.Enabled { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        void IBaseControl.Activate()
        {
            throw new System.NotImplementedException();
        }

        void IListView.AddCheckedColumn(string caption, int width, bool autoSize)
        {
            throw new System.NotImplementedException();
        }

        void IListView.AddColumn(string caption, int width, bool autoSize)
        {
            throw new System.NotImplementedException();
        }

        void IListView.AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
            throw new System.NotImplementedException();
        }

        IListItem IListView.AddItem(object rowData, bool isChecked, params object[] columnValues)
        {
            throw new System.NotImplementedException();
        }

        IListItem IListView.AddItem(object rowData, params object[] columnValues)
        {
            throw new System.NotImplementedException();
        }

        void IListView.BeginUpdate()
        {
            throw new System.NotImplementedException();
        }

        void IListView.Clear()
        {
            throw new System.NotImplementedException();
        }

        void IListView.ClearColumns()
        {
            throw new System.NotImplementedException();
        }

        void IListView.ClearItems()
        {
            throw new System.NotImplementedException();
        }

        void IListViewEx.DeleteRecord(object data)
        {
            throw new System.NotImplementedException();
        }

        void IListView.EndUpdate()
        {
            throw new System.NotImplementedException();
        }

        object IListView.GetSelectedData()
        {
            throw new System.NotImplementedException();
        }

        void IListViewEx.ResizeColumn(int columnIndex)
        {
            throw new System.NotImplementedException();
        }

        void IListViewEx.SelectItem(int index)
        {
            throw new System.NotImplementedException();
        }

        void IListView.SelectItem(object rowData)
        {
            throw new System.NotImplementedException();
        }

        void IListView.SetColumnCaption(int index, string caption)
        {
            throw new System.NotImplementedException();
        }

        void IListView.SetSortColumn(int sortColumn, bool checkOrder)
        {
            throw new System.NotImplementedException();
        }

        void IListView.Sort(int sortColumn, BSDTypes.SortOrder sortOrder)
        {
            throw new System.NotImplementedException();
        }

        void IListView.UpdateContents(bool columnsChanged)
        {
            throw new System.NotImplementedException();
        }
    }
}
