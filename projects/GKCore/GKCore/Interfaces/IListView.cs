/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

namespace GKCore.Interfaces
{
    public interface IListViewItems
    {
        IListItem this[int index] { get; }
        int Count { get; }
    }

    /// <summary>
    /// 
    /// </summary>
    public interface IListView
    {
        IListViewItems Items { get; }

        void AddColumn(string caption, int width, bool autoSize);
        IListItem AddItem(object rowData, params object[] columnValues);
        void BeginUpdate();
        void ClearItems();
        void EndUpdate();
        object GetSelectedData();
        void SetColumnCaption(int index, string caption);
    }
}
